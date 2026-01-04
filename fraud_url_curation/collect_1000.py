#!/usr/bin/env python3
"""
Collect exactly 1000 unique, validated URLs about banking/payment fraud.

Output format (CSV):
categoria,url
... 1000 lines ...

Categories:
- PAPER, DATASET, WHITEPAPER, TECH, CASES, FUNDAMENTALS
"""

from __future__ import annotations

import asyncio
import csv
import json
import re
import subprocess
import sys
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


_FRAUD_INCLUDE_RE = re.compile(
    r"(?i)\\b("
    r"fraud|fraudulent|scam|chargeback|carding|account\\s*takeover|\\bATO\\b|"
    r"synthetic\\s+identity|mule\\s+account|money\\s+laundering|anti[-\\s]?money\\s+laundering|\\bKYC\\b|"
    r"transaction|payment|payments|credit\\s*card|debit\\s*card|e-?commerce|merchant|bank|banking|"
    r"financial\\s+crime|identity\\s+theft|phishing|sim\\s*swap|pix"
    r")\\b"
)

_FUNDAMENTALS_INCLUDE_RE = re.compile(
    r"(?i)\\b("
    r"anomaly\\s+detection|outlier\\s+detection|concept\\s+drift|imbalanced|class\\s+imbalance|"
    r"explainable\\s+ai|xai|shap|lime|counterfactual|graph\\s+neural\\s+network|gnn|"
    r"domain\\s+adaptation|transfer\\s+learning|few-?shot|federated\\s+learning|drift"
    r")\\b"
)

_EXCLUDE_RE = re.compile(
    r"(?i)\\b("
    r"acute\\s+myeloid\\s+leukemia|\\bAML\\b\\s*(?!\\s*(fraud|transaction|monitor|anti|money|launder))|"
    r"leukemia|cancer|tumou?r|oncology|immunology|transcriptome|transcriptomic|gene|genetic|"
    r"myeloid|metabolomics|single\\s*cell|chIP|ATAC|myeloma|bioinformatics|"
    r"agriculture|tourism|marine\\s+science"
    r")\\b"
)


def title_relevant(title: str | None, category: str) -> bool:
    if not title:
        return False
    t = title.strip()
    if not t:
        return False
    if _EXCLUDE_RE.search(t):
        return False
    if category == "FUNDAMENTALS":
        # Allow general ML fundamentals, but keep them strongly technical.
        return bool(_FUNDAMENTALS_INCLUDE_RE.search(t) or _FRAUD_INCLUDE_RE.search(t))
    return bool(_FRAUD_INCLUDE_RE.search(t))


def normalize_url(url: str) -> str:
    url = url.strip()
    url = url.replace(" ", "%20")

    # Decode common DDG wrapper: https://duckduckgo.com/l/?uddg=<encoded>
    try:
        parts = urlsplit(url)
        if parts.netloc.endswith("duckduckgo.com") and parts.path.startswith("/l/"):
            q = dict(parse_qsl(parts.query, keep_blank_values=True))
            if "uddg" in q:
                url = unquote(q["uddg"])
                parts = urlsplit(url)
    except Exception:
        pass

    parts = urlsplit(url)
    scheme = parts.scheme.lower() if parts.scheme else "https"
    netloc = parts.netloc.lower()

    # Some DDG results come as //example.com/...
    if not netloc and parts.path.startswith("//"):
        parts2 = urlsplit("https:" + parts.path)
        scheme, netloc, path, query = parts2.scheme, parts2.netloc.lower(), parts2.path, parts2.query
    else:
        path, query = parts.path, parts.query

    # Drop fragments
    fragment = ""

    # Remove tracking params
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

    # Remove trailing slash (except root)
    if path != "/" and path.endswith("/"):
        path = path[:-1]

    return urlunsplit((scheme, netloc, path, query, fragment))


def is_probably_spam(url: str) -> bool:
    u = url.lower()
    if any(x in u for x in ["pinterest.", "facebook.com/sharer", "twitter.com/intent", "t.me/share"]):
        return True
    if u.startswith("mailto:") or u.startswith("javascript:"):
        return True
    if urlsplit(u).netloc in BLACKLIST_NETLOCS:
        return True
    return False


@dataclass(frozen=True)
class Item:
    category: str
    url: str


async def fetch_json(session: aiohttp.ClientSession, url: str) -> dict:
    async with session.get(url, timeout=aiohttp.ClientTimeout(total=30)) as resp:
        resp.raise_for_status()
        return await resp.json()


async def fetch_text(session: aiohttp.ClientSession, url: str) -> str:
    async with session.get(url, timeout=aiohttp.ClientTimeout(total=30)) as resp:
        resp.raise_for_status()
        return await resp.text(errors="ignore")


async def validate_url(session: aiohttp.ClientSession, url: str) -> tuple[bool, str | None]:
    """
    Validate URL is reachable and return final URL after redirects (normalized).
    """
    # Some origins break on Range requests; keep validation conservative.
    try:
        netloc = urlsplit(url).netloc.lower()

        # DOI landing pages often redirect to publisher pages that may block bots.
        # Consider the DOI resolver itself as sufficient validation.
        if netloc in {"doi.org", "dx.doi.org"}:
            try:
                async with session.head(
                    url,
                    timeout=aiohttp.ClientTimeout(total=6),
                    allow_redirects=False,
                ) as resp:
                    if resp.status < 400:
                        return True, normalize_url(url)
            except Exception:
                pass

        # 1) Try HEAD first (cheap), but many sites disable it.
        try:
            async with session.head(
                url,
                timeout=aiohttp.ClientTimeout(total=6),
                allow_redirects=True,
            ) as resp:
                if resp.status < 400:
                    return True, normalize_url(str(resp.url))
        except Exception:
            pass

        # 2) Fallback to GET and read a small prefix.
        async with session.get(
            url,
            timeout=aiohttp.ClientTimeout(total=8),
            allow_redirects=True,
        ) as resp:
            if resp.status >= 400:
                return False, None
            try:
                await resp.content.read(1024)
            except Exception:
                pass
            return True, normalize_url(str(resp.url))
    except Exception:
        return False, None


async def openalex_works(
    session: aiohttp.ClientSession,
    query: str,
    needed: int,
    *,
    filter_expr: str | None = None,
    category_hint: str | None = None,
) -> list[str]:
    """
    Pull landing pages from OpenAlex works endpoint.
    """
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
            loc = (w.get("primary_location") or {}).get("landing_page_url")
            doi = w.get("doi")
            chosen = loc or doi
            if not chosen:
                continue
            if category_hint and not title_relevant(title, category_hint):
                continue
            nu = normalize_url(chosen)
            if is_probably_spam(nu):
                continue
            urls.append(nu)
            if len(urls) >= needed:
                break
        cursor = (data.get("meta") or {}).get("next_cursor")
        if not cursor:
            break
        await asyncio.sleep(0.12)
    return urls


async def hf_dataset_search(session: aiohttp.ClientSession, query: str, max_pages: int = 10) -> list[str]:
    """
    Hugging Face datasets search API.
    """
    urls: list[str] = []
    for page in range(max_pages):
        # Undocumented but stable: /api/datasets?search=...&limit=...&offset=...
        limit = 50
        offset = page * limit
        api = f"https://huggingface.co/api/datasets?search={quote_plus(query)}&limit={limit}&offset={offset}"
        try:
            async with session.get(api, timeout=aiohttp.ClientTimeout(total=30)) as resp:
                if resp.status >= 400:
                    break
                items = await resp.json()
        except Exception:
            break
        if not items:
            break
        for it in items:
            sid = it.get("id")
            if sid:
                urls.append(normalize_url(f"https://huggingface.co/datasets/{sid}"))
        await asyncio.sleep(0.12)
    return urls


async def openml_search(session: aiohttp.ClientSession, query: str, limit: int = 200) -> list[str]:
    """
    OpenML dataset search via list endpoint (server-side 'search' works in practice).
    """
    urls: list[str] = []
    # The OpenML API doesn't document "search" consistently, but it is supported on list endpoints.
    api = f"https://www.openml.org/api/v1/json/data/list?search={quote_plus(query)}&limit={limit}"
    try:
        data = await fetch_json(session, api)
    except Exception:
        return urls
    datasets = (((data or {}).get("data") or {}).get("dataset")) or []
    for ds in datasets:
        did = ds.get("did")
        if did:
            urls.append(normalize_url(f"https://www.openml.org/d/{did}"))
    return urls


def github_repo_search_via_gh(query: str, *, pages: int = 3, per_page: int = 100) -> list[str]:
    """
    Search GitHub repositories via authenticated `gh api`.
    Returns repository HTML URLs (useful as TECH content).
    """
    urls: list[str] = []
    for page in range(1, pages + 1):
        # NOTE: Using full URL because some environments return 404 on relative paths.
        q = quote_plus(query)
        url = (
            "https://api.github.com/search/repositories"
            f"?q={q}&sort=stars&order=desc&per_page={per_page}&page={page}"
        )
        cmd = ["gh", "api", url]
        try:
            p = subprocess.run(cmd, check=True, capture_output=True, text=True)
            data = json.loads(p.stdout)
        except Exception:
            break
        items = data.get("items") or []
        if not items:
            break
        for it in items:
            u = it.get("html_url")
            if u:
                urls.append(normalize_url(u))
    return urls


async def gather_and_add(
    session: aiohttp.ClientSession,
    category: str,
    candidates: Iterable[str],
    targets: dict[str, int],
    seen: set[str],
    out: list[Item],
    sem: asyncio.Semaphore,
) -> None:
    async def handle(raw_url: str) -> None:
        nonlocal category
        if targets[category] <= 0:
            return
        u0 = normalize_url(raw_url)
        if not u0 or is_probably_spam(u0):
            return
        if u0 in seen:
            return
        async with sem:
            ok, final_u = await validate_url(session, u0)
        if not ok or not final_u:
            return
        if is_probably_spam(final_u):
            return
        if final_u in seen:
            return
        if targets[category] <= 0:
            return
        seen.add(final_u)
        out.append(Item(category=category, url=final_u))
        targets[category] -= 1

    tasks: list[asyncio.Task[None]] = []
    for u in candidates:
        if targets[category] <= 0:
            break
        tasks.append(asyncio.create_task(handle(u)))
        if len(tasks) >= 250:
            await asyncio.gather(*tasks)
            tasks = []
    if tasks:
        await asyncio.gather(*tasks)


def print_progress(targets_left: dict[str, int], seen_count: int) -> None:
    left = " ".join(f"{k}:{v}" for k, v in targets_left.items())
    sys.stderr.write(f"[progress] seen={seen_count} left {left}\n")
    sys.stderr.flush()


async def main() -> int:
    targets: dict[str, int] = {
        "PAPER": 300,
        "DATASET": 200,
        "WHITEPAPER": 150,
        "TECH": 150,
        "CASES": 100,
        "FUNDAMENTALS": 100,
    }
    out: list[Item] = []
    seen: set[str] = set()

    timeout = aiohttp.ClientTimeout(total=20)
    headers = {"User-Agent": USER_AGENT, "Accept": "*/*", "Accept-Language": "en-US,en;q=0.9"}
    sem = asyncio.Semaphore(120)

    async with aiohttp.ClientSession(timeout=timeout, headers=headers) as session:
        # 1) Papers via OpenAlex (stable, fast)
        paper_queries = [
            "banking fraud detection",
            "payment fraud detection",
            "credit card fraud detection",
            "e-commerce fraud detection",
            "account takeover detection",
            "synthetic identity fraud detection",
            "mule account detection",
            "chargeback fraud detection",
            "AML transaction monitoring",
            "graph neural network fraud detection",
            "concept drift fraud detection",
            "federated learning fraud detection",
            "transfer learning fraud detection",
            "domain adaptation fraud detection",
            "behavioral biometrics fraud detection",
            "device fingerprinting fraud detection",
        ]
        paper_filters = [
            "type:article",
            "type:preprint",
        ]
        for q in paper_queries:
            if targets["PAPER"] <= 0:
                break
            for fexpr in paper_filters:
                if targets["PAPER"] <= 0:
                    break
                needed = min(400, targets["PAPER"] * 2)
                candidates = await openalex_works(session, q, needed=needed, filter_expr=fexpr, category_hint="PAPER")
                await gather_and_add(session, "PAPER", candidates, targets, seen, out, sem)
                print_progress(targets, len(seen))

        # 2) Datasets (OpenAlex datasets + OpenML + HuggingFace)
        dataset_queries_openml = [
            "fraud",
            "credit card fraud",
            "transaction fraud",
            "payment fraud",
            "bank fraud",
            "money laundering",
            "anti money laundering",
        ]
        dataset_queries_hf = [
            "fraud",
            "credit card fraud",
            "transaction fraud",
            "money laundering",
            "anti money laundering",
        ]

        dataset_queries_openalex = [
            "credit card fraud dataset",
            "payment fraud dataset",
            "transaction fraud dataset",
            "bank fraud dataset",
            "money laundering dataset",
            "anti money laundering dataset",
            "AML dataset",
        ]

        for q in dataset_queries_openalex:
            if targets["DATASET"] <= 0:
                break
            candidates = await openalex_works(
                session,
                q,
                needed=min(500, targets["DATASET"] * 3),
                filter_expr="type:dataset",
                category_hint="DATASET",
            )
            await gather_and_add(session, "DATASET", candidates, targets, seen, out, sem)
            print_progress(targets, len(seen))

        # OpenML
        for q in dataset_queries_openml:
            if targets["DATASET"] <= 0:
                break
            candidates = await openml_search(session, q, limit=200)
            await gather_and_add(session, "DATASET", candidates, targets, seen, out, sem)
            print_progress(targets, len(seen))

        # HuggingFace datasets
        for q in dataset_queries_hf:
            if targets["DATASET"] <= 0:
                break
            candidates = await hf_dataset_search(session, q, max_pages=6)
            await gather_and_add(session, "DATASET", candidates, targets, seen, out, sem)
            print_progress(targets, len(seen))

        # 3) Whitepapers
        whitepaper_queries = [
            "payment fraud report",
            "bank fraud report",
            "account takeover report",
            "chargeback report fraud",
            "AML typologies report",
            "money laundering typologies report",
            "payments fraud central bank report",
            "payment card fraud report",
            "financial crime report",
            "transaction monitoring report",
            "KYC AML report",
            "merchant fraud report",
            "e-commerce fraud report",
        ]
        for q in whitepaper_queries:
            if targets["WHITEPAPER"] <= 0:
                break
            candidates = await openalex_works(
                session,
                q,
                needed=min(500, targets["WHITEPAPER"] * 3),
                filter_expr="type:report",
                category_hint="WHITEPAPER",
            )
            await gather_and_add(session, "WHITEPAPER", candidates, targets, seen, out, sem)
            print_progress(targets, len(seen))

        # 4) Technical content
        tech_queries = [
            "fraud detection software",
            "fraud detection github",
            "transaction monitoring software",
            "aml transaction monitoring software",
            "graph fraud detection code",
            "real-time fraud detection system",
            "streaming fraud detection system",
            "fraud detection pipeline",
        ]
        for q in tech_queries:
            if targets["TECH"] <= 0:
                break
            candidates = await openalex_works(
                session,
                q,
                needed=min(500, targets["TECH"] * 3),
                filter_expr="type:software",
                category_hint="TECH",
            )
            await gather_and_add(session, "TECH", candidates, targets, seen, out, sem)
            print_progress(targets, len(seen))

        # TECH fallback: GitHub repositories (implementations, notebooks, pipelines, MLOps)
        if targets["TECH"] > 0:
            gh_queries = [
                '"fraud detection"',
                '"credit card fraud" detection',
                '"payment fraud" detection',
                '"transaction fraud" detection',
                '"ecommerce" fraud detection',
                '"account takeover" detection',
                '"synthetic identity" fraud',
                '"mule account" detection',
                '"chargeback" fraud',
                '"anti money laundering" transaction monitoring',
                'aml "transaction monitoring"',
                '"graph" fraud detection',
                '"graph neural network" fraud detection',
                '"concept drift" fraud detection',
                '"behavioral biometrics" fraud',
                '"device fingerprint" fraud',
                '"velocity rules" fraud',
                '"risk scoring" fraud',
                '"realtime" fraud detection streaming',
                '"pix" fraude',
            ]
            for gq in gh_queries:
                if targets["TECH"] <= 0:
                    break
                candidates = github_repo_search_via_gh(gq, pages=3, per_page=100)
                await gather_and_add(session, "TECH", candidates, targets, seen, out, sem)
                print_progress(targets, len(seen))

        # 5) Cases / threat intel
        cases_queries = [
            "payment fraud case study",
            "banking fraud case study",
            "account takeover case study",
            "chargeback fraud case study",
            "synthetic identity fraud case study",
            "mule account fraud case study",
            "carding fraud case study",
            "SIM swap fraud case study",
            "phishing banking fraud case study",
            "fraud advisory payment",
        ]
        for q in cases_queries:
            if targets["CASES"] <= 0:
                break
            candidates = await openalex_works(
                session,
                q,
                needed=min(500, targets["CASES"] * 4),
                filter_expr="type:report",
                category_hint="CASES",
            )
            await gather_and_add(session, "CASES", candidates, targets, seen, out, sem)
            print_progress(targets, len(seen))

        # 6) Fundamentals (high-quality reference pages)
        fundamentals_queries = [
            "concept drift survey",
            "imbalanced learning survey",
            "anomaly detection survey",
            "graph neural networks tutorial",
            "explainable AI survey",
            "SHAP explainability tutorial",
            "domain adaptation survey",
            "transfer learning tutorial",
            "few-shot learning survey",
            "federated learning tutorial",
        ]
        for q in fundamentals_queries:
            if targets["FUNDAMENTALS"] <= 0:
                break
            candidates = await openalex_works(session, q, needed=min(500, targets["FUNDAMENTALS"] * 4), category_hint="FUNDAMENTALS")
            await gather_and_add(session, "FUNDAMENTALS", candidates, targets, seen, out, sem)
            print_progress(targets, len(seen))

        # Final OpenAlex fallbacks if any category is still missing.
        fallbacks = [
            ("WHITEPAPER", "fraud report", "type:report"),
            ("TECH", "fraud detection github", "type:software"),
            ("CASES", "fraud case study", "type:report"),
            ("FUNDAMENTALS", "machine learning tutorial", None),
            ("DATASET", "dataset", "type:dataset"),
        ]
        for cat, q, fexpr in fallbacks:
            if targets.get(cat, 0) <= 0:
                continue
            candidates = await openalex_works(session, q, needed=2000, filter_expr=fexpr, category_hint=cat)
            await gather_and_add(session, cat, candidates, targets, seen, out, sem)
            print_progress(targets, len(seen))

    # Final checks
    remaining = sum(v for v in targets.values())
    if remaining != 0:
        sys.stderr.write(f"[error] Could not fill all targets, remaining={targets}\n")
        return 2

    if len(out) != 1000:
        sys.stderr.write(f"[error] Output size mismatch: {len(out)}\n")
        return 3

    # Deterministic-ish output order: by category then URL
    order = {"PAPER": 0, "DATASET": 1, "WHITEPAPER": 2, "TECH": 3, "CASES": 4, "FUNDAMENTALS": 5}
    out.sort(key=lambda it: (order.get(it.category, 99), it.url))

    out_path = "fraud_url_curation/final_1000.csv"
    with open(out_path, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["categoria", "url"])
        for it in out:
            w.writerow([it.category, it.url])

    sys.stderr.write(f"[ok] Wrote {out_path}\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(asyncio.run(main()))

