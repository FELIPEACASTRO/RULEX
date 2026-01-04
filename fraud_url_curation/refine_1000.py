#!/usr/bin/env python3
"""
Refine an existing final_1000.csv by removing obvious off-topic URLs
and replacing them with fresh, validated, fraud-relevant URLs while
keeping the exact category totals (300/200/150/150/100/100) and 1000 lines.
"""

from __future__ import annotations

import asyncio
import csv
import json
import re
import sys
from collections import Counter, defaultdict
from dataclasses import dataclass
from typing import Iterable
from urllib.parse import parse_qsl, quote_plus, unquote, urlencode, urlsplit, urlunsplit

import aiohttp


USER_AGENT = (
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 "
    "(KHTML, like Gecko) Chrome/120 Safari/537.36"
)

TRACKING_PARAM_PREFIXES = ("utm_",)
TRACKING_PARAM_KEYS = {
    "gclid",
    "fbclid",
    "msclkid",
    "mc_cid",
    "mc_eid",
    "ref",
    "source",
    "spm",
    "igshid",
    "vero_id",
}

BLACKLIST_NETLOCS = {
    "orcid.org",
    "cid.org",
    "choicereviews.org",
    "validate.perfdrive.com",
}

BAD_URL_RE = re.compile(
    r"(?i)("
    r"leuk|myeloid|oncology|cancer|metabol|transcript|chip|atac|myeloma|"
    r"h1\\.co|plosone/article/figure|perfdrive|choicereviews|orcid\\.org|cid\\.org"
    r")"
)

FRAUD_TITLE_RE = re.compile(
    r"(?i)\\b("
    r"fraud|fraudulent|scam|chargeback|carding|account\\s*takeover|\\bATO\\b|"
    r"synthetic\\s+identity|mule\\s+account|money\\s+laundering|anti[-\\s]?money\\s+laundering|\\bKYC\\b|"
    r"transaction|payment|payments|credit\\s*card|debit\\s*card|e-?commerce|merchant|bank|banking|"
    r"financial\\s+crime|identity\\s+theft|phishing|sim\\s*swap|pix"
    r")\\b"
)

FUNDAMENTALS_TITLE_RE = re.compile(
    r"(?i)\\b("
    r"anomaly\\s+detection|outlier\\s+detection|concept\\s+drift|imbalanced|class\\s+imbalance|"
    r"explainable\\s+ai|xai|shap|lime|counterfactual|graph\\s+neural\\s+network|gnn|"
    r"domain\\s+adaptation|transfer\\s+learning|few-?shot|federated\\s+learning|drift"
    r")\\b"
)

EXCLUDE_TITLE_RE = re.compile(
    r"(?i)\\b("
    r"acute\\s+myeloid\\s+leukemia|leukemia|cancer|tumou?r|oncology|immunology|transcriptome|transcriptomic|gene|"
    r"metabolomics|single\\s*cell|myeloma|bioinformatics|marine\\s+science"
    r")\\b"
)


def normalize_url(url: str) -> str:
    url = url.strip().replace(" ", "%20")
    try:
        parts = urlsplit(url)
        if parts.netloc.endswith("duckduckgo.com") and parts.path.startswith("/l/"):
            q = dict(parse_qsl(parts.query, keep_blank_values=True))
            if "uddg" in q:
                url = unquote(q["uddg"])
    except Exception:
        pass

    parts = urlsplit(url)
    scheme = parts.scheme.lower() if parts.scheme else "https"
    netloc = parts.netloc.lower()
    path = parts.path
    query = parts.query
    fragment = ""

    if query:
        kept = []
        for k, v in parse_qsl(query, keep_blank_values=True):
            kl = k.lower()
            if any(kl.startswith(p) for p in TRACKING_PARAM_PREFIXES):
                continue
            if kl in TRACKING_PARAM_KEYS:
                continue
            kept.append((k, v))
        kept.sort(key=lambda kv: (kv[0], kv[1]))
        query = urlencode(kept, doseq=True)

    if path != "/" and path.endswith("/"):
        path = path[:-1]

    return urlunsplit((scheme, netloc, path, query, fragment))


def is_bad_url(url: str) -> bool:
    nu = normalize_url(url)
    netloc = urlsplit(nu).netloc.lower()
    if netloc in BLACKLIST_NETLOCS:
        return True
    return bool(BAD_URL_RE.search(nu))


def title_ok(title: str | None, category: str) -> bool:
    if not title:
        return False
    t = title.strip()
    if not t or EXCLUDE_TITLE_RE.search(t):
        return False
    if category == "FUNDAMENTALS":
        return bool(FUNDAMENTALS_TITLE_RE.search(t) or FRAUD_TITLE_RE.search(t))
    return bool(FRAUD_TITLE_RE.search(t))


@dataclass(frozen=True)
class Item:
    category: str
    url: str


async def fetch_json(session: aiohttp.ClientSession, url: str) -> dict:
    async with session.get(url, timeout=aiohttp.ClientTimeout(total=20)) as resp:
        resp.raise_for_status()
        return await resp.json()


async def validate_url(session: aiohttp.ClientSession, url: str) -> tuple[bool, str | None]:
    try:
        u = normalize_url(url)
        if is_bad_url(u):
            return False, None
        netloc = urlsplit(u).netloc.lower()

        # DOI: validate resolver only (avoid bot-block on publisher).
        if netloc in {"doi.org", "dx.doi.org"}:
            try:
                async with session.head(u, allow_redirects=False, timeout=aiohttp.ClientTimeout(total=4)) as resp:
                    if resp.status < 400:
                        return True, u
            except Exception:
                return False, None

        # HEAD then GET fallback
        try:
            async with session.head(u, allow_redirects=True, timeout=aiohttp.ClientTimeout(total=4)) as resp:
                if resp.status < 400:
                    return True, normalize_url(str(resp.url))
        except Exception:
            pass

        async with session.get(u, allow_redirects=True, timeout=aiohttp.ClientTimeout(total=6)) as resp:
            if resp.status >= 400:
                return False, None
            try:
                await resp.content.read(256)
            except Exception:
                pass
            return True, normalize_url(str(resp.url))
    except Exception:
        return False, None


async def openalex_fetch(session: aiohttp.ClientSession, query: str, *, filter_expr: str | None, needed: int, category: str) -> list[str]:
    urls: list[str] = []
    cursor = "*"
    per_page = 200
    while len(urls) < needed:
        params: dict[str, str | int] = {"search": query, "per-page": per_page, "cursor": cursor}
        if filter_expr:
            params["filter"] = filter_expr
        api = "https://api.openalex.org/works?" + urlencode(params)
        try:
            data = await fetch_json(session, api)
        except Exception:
            break
        results = data.get("results") or []
        if not results:
            break
        for w in results:
            title = w.get("display_name")
            if not title_ok(title, category):
                continue
            loc = (w.get("primary_location") or {}).get("landing_page_url")
            doi = w.get("doi")
            # For paper-like categories, prefer DOI (fast to validate, stable).
            if category in {"PAPER", "FUNDAMENTALS"}:
                if not doi:
                    continue
                chosen = doi
            else:
                chosen = loc or doi
                if not chosen:
                    continue
            nu = normalize_url(chosen)
            if is_bad_url(nu):
                continue
            urls.append(nu)
            if len(urls) >= needed:
                break
        cursor = (data.get("meta") or {}).get("next_cursor")
        if not cursor:
            break
        await asyncio.sleep(0.08)
    return urls


async def openml_search(session: aiohttp.ClientSession, query: str, limit: int = 200) -> list[str]:
    api = f"https://www.openml.org/api/v1/json/data/list?search={quote_plus(query)}&limit={limit}"
    try:
        data = await fetch_json(session, api)
    except Exception:
        return []
    datasets = (((data or {}).get("data") or {}).get("dataset")) or []
    out: list[str] = []
    for ds in datasets:
        did = ds.get("did")
        name = (ds.get("name") or "").lower()
        if did and ("fraud" in name or "credit" in name or "transaction" in name or "payment" in name):
            out.append(normalize_url(f"https://www.openml.org/d/{did}"))
    return out


async def hf_dataset_search(session: aiohttp.ClientSession, query: str, max_pages: int = 6) -> list[str]:
    urls: list[str] = []
    for page in range(max_pages):
        limit = 50
        offset = page * limit
        api = f"https://huggingface.co/api/datasets?search={quote_plus(query)}&limit={limit}&offset={offset}"
        try:
            async with session.get(api, timeout=aiohttp.ClientTimeout(total=20)) as resp:
                if resp.status >= 400:
                    break
                items = await resp.json()
        except Exception:
            break
        if not items:
            break
        for it in items:
            sid = it.get("id") or ""
            if not sid:
                continue
            s = sid.lower()
            if "fraud" in s or "transaction" in s or "payment" in s or "credit" in s or "aml" in s or "money" in s:
                urls.append(normalize_url(f"https://huggingface.co/datasets/{sid}"))
        await asyncio.sleep(0.05)
    return urls


async def add_validated(
    session: aiohttp.ClientSession,
    category: str,
    candidates: Iterable[str],
    need: int,
    seen: set[str],
    out: list[Item],
    sem: asyncio.Semaphore,
) -> int:
    added = 0

    async def handle(u: str) -> None:
        nonlocal added, need
        if need <= 0:
            return
        nu = normalize_url(u)
        if not nu or nu in seen or is_bad_url(nu):
            return
        async with sem:
            ok, final_u = await validate_url(session, nu)
        if not ok or not final_u or final_u in seen or is_bad_url(final_u):
            return
        if need <= 0:
            return
        seen.add(final_u)
        out.append(Item(category=category, url=final_u))
        need -= 1
        added += 1

    tasks: list[asyncio.Task[None]] = []
    for u in candidates:
        if need <= 0:
            break
        tasks.append(asyncio.create_task(handle(u)))
        if len(tasks) >= 120:
            await asyncio.gather(*tasks)
            tasks = []
    if tasks:
        await asyncio.gather(*tasks)
    return added


async def main() -> int:
    in_path = "/workspace/fraud_url_curation/final_1000.csv"
    rows = list(csv.DictReader(open(in_path, newline="", encoding="utf-8")))
    if len(rows) != 1000:
        print(f"Expected 1000 rows, got {len(rows)}", file=sys.stderr)
        return 2

    targets = {"PAPER": 300, "DATASET": 200, "WHITEPAPER": 150, "TECH": 150, "CASES": 100, "FUNDAMENTALS": 100}

    kept: list[Item] = []
    removed = Counter()
    seen: set[str] = set()

    for r in rows:
        cat = r["categoria"].strip()
        url = normalize_url(r["url"])
        if cat not in targets:
            continue
        if is_bad_url(url):
            removed[cat] += 1
            continue
        if url in seen:
            removed[cat] += 1
            continue
        seen.add(url)
        kept.append(Item(category=cat, url=url))

    kept_counts = Counter(i.category for i in kept)
    deficit = {c: max(0, targets[c] - kept_counts.get(c, 0)) for c in targets}
    total_deficit = sum(deficit.values())
    if total_deficit == 0 and len(kept) == 1000:
        return 0

    print(f"[refine] kept={len(kept)} removed={sum(removed.values())} deficit={deficit}", file=sys.stderr)

    timeout = aiohttp.ClientTimeout(total=15)
    headers = {"User-Agent": USER_AGENT, "Accept": "*/*", "Accept-Language": "en-US,en;q=0.9"}
    sem = asyncio.Semaphore(220)

    additions: list[Item] = []

    async with aiohttp.ClientSession(timeout=timeout, headers=headers) as session:
        # PAPER
        if deficit["PAPER"] > 0:
            paper_qs = [
                ("credit card fraud detection", "type:article"),
                ("payment fraud detection", "type:article"),
                ("transaction fraud detection", "type:article"),
                ("account takeover detection", "type:article"),
                ("money laundering detection", "type:article"),
                ("graph fraud detection", "type:article"),
                ("concept drift fraud detection", "type:article"),
                ("fraud detection", "type:preprint"),
            ]
            for q, fexpr in paper_qs:
                if deficit["PAPER"] <= 0:
                    break
                cand = await openalex_fetch(session, q, filter_expr=fexpr, needed=800, category="PAPER")
                got = await add_validated(session, "PAPER", cand, deficit["PAPER"], seen, additions, sem)
                deficit["PAPER"] -= got
                print(f"[refine] PAPER added {got} left {deficit['PAPER']}", file=sys.stderr)

        # DATASET
        if deficit["DATASET"] > 0:
            ds_qs = [
                ("credit card fraud dataset", "type:dataset"),
                ("payment fraud dataset", "type:dataset"),
                ("transaction fraud dataset", "type:dataset"),
                ("fraud dataset", "type:dataset"),
                ("money laundering dataset", "type:dataset"),
            ]
            for q, fexpr in ds_qs:
                if deficit["DATASET"] <= 0:
                    break
                cand = await openalex_fetch(session, q, filter_expr=fexpr, needed=900, category="DATASET")
                got = await add_validated(session, "DATASET", cand, deficit["DATASET"], seen, additions, sem)
                deficit["DATASET"] -= got
                print(f"[refine] DATASET(OpenAlex) added {got} left {deficit['DATASET']}", file=sys.stderr)

            if deficit["DATASET"] > 0:
                for q in ["fraud", "credit card fraud", "transaction fraud", "payment fraud"]:
                    if deficit["DATASET"] <= 0:
                        break
                    cand = await openml_search(session, q, limit=200)
                    got = await add_validated(session, "DATASET", cand, deficit["DATASET"], seen, additions, sem)
                    deficit["DATASET"] -= got
                    print(f"[refine] DATASET(OpenML) added {got} left {deficit['DATASET']}", file=sys.stderr)

            if deficit["DATASET"] > 0:
                for q in ["fraud", "credit card fraud", "transaction fraud", "payment fraud", "money laundering"]:
                    if deficit["DATASET"] <= 0:
                        break
                    cand = await hf_dataset_search(session, q, max_pages=6)
                    got = await add_validated(session, "DATASET", cand, deficit["DATASET"], seen, additions, sem)
                    deficit["DATASET"] -= got
                    print(f"[refine] DATASET(HF) added {got} left {deficit['DATASET']}", file=sys.stderr)

        # FUNDAMENTALS
        if deficit["FUNDAMENTALS"] > 0:
            fun_qs = [
                ("concept drift survey", "type:article"),
                ("imbalanced learning survey", "type:article"),
                ("anomaly detection survey", "type:article"),
                ("graph neural network tutorial", "type:article"),
                ("explainable AI survey", "type:article"),
                ("domain adaptation survey", "type:article"),
                ("transfer learning tutorial", "type:article"),
                ("few-shot learning survey", "type:article"),
                ("federated learning tutorial", "type:article"),
            ]
            for q, fexpr in fun_qs:
                if deficit["FUNDAMENTALS"] <= 0:
                    break
                cand = await openalex_fetch(session, q, filter_expr=fexpr, needed=1000, category="FUNDAMENTALS")
                got = await add_validated(session, "FUNDAMENTALS", cand, deficit["FUNDAMENTALS"], seen, additions, sem)
                deficit["FUNDAMENTALS"] -= got
                print(f"[refine] FUNDAMENTALS added {got} left {deficit['FUNDAMENTALS']}", file=sys.stderr)

    # Compose final list with exact targets
    all_items = kept + additions
    counts = Counter(i.category for i in all_items)
    if any(counts[c] < targets[c] for c in targets):
        print(f"[error] Still missing after refine counts={counts} deficit={deficit}", file=sys.stderr)
        return 3

    # Trim extras (if any) but keep targets exactly.
    by_cat: dict[str, list[Item]] = defaultdict(list)
    for it in all_items:
        by_cat[it.category].append(it)
    for c in by_cat:
        by_cat[c].sort(key=lambda x: x.url)

    final: list[Item] = []
    for c in ["PAPER", "DATASET", "WHITEPAPER", "TECH", "CASES", "FUNDAMENTALS"]:
        final.extend(by_cat[c][: targets[c]])

    if len(final) != 1000:
        print(f"[error] Final length {len(final)}", file=sys.stderr)
        return 4

    out_path = "/workspace/fraud_url_curation/final_1000_refined.csv"
    with open(out_path, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["categoria", "url"])
        for it in final:
            w.writerow([it.category, it.url])

    print(f"[ok] wrote {out_path}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(asyncio.run(main()))

