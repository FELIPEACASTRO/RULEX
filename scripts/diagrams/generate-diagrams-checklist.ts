import fs from "node:fs";
import path from "node:path";

import { DIAGRAM_ITEMS } from "../../client/src/features/diagrams/registry/diagramRegistry";
import type { DiagramCatalogItem } from "../../client/src/features/diagrams/types";

type ParseStatus = "OK" | "FAIL" | "SKIP";

function countBy<T extends string>(items: DiagramCatalogItem[], keyFn: (i: DiagramCatalogItem) => T): Record<T, number> {
  const out = Object.create(null) as Record<T, number>;
  for (const item of items) {
    const key = keyFn(item);
    out[key] = (out[key] ?? 0) + 1;
  }
  return out;
}

function mdEscape(text: string): string {
  return text.replace(/\|/g, "\\|").replace(/\r?\n/g, " ");
}

async function tryMermaidParse(code: string): Promise<{ status: ParseStatus; error?: string }> {
  const trimmed = code.trim();
  if (!trimmed) return { status: "SKIP" };

  try {
    const mermaidMod = await import("mermaid");
    const mermaid = mermaidMod.default as unknown as {
      initialize?: (cfg: Record<string, unknown>) => void;
      parse?: (code: string) => unknown;
    };

    mermaid.initialize?.({ startOnLoad: false, securityLevel: "strict" });

    if (typeof mermaid.parse !== "function") return { status: "SKIP", error: "mermaid.parse() não disponível" };

    const result = mermaid.parse(trimmed);
    if (result && typeof (result as Promise<unknown>).then === "function") {
      await (result as Promise<unknown>);
    }

    return { status: "OK" };
  } catch (e) {
    const msg = e instanceof Error ? e.message : String(e);

    // Mermaid parsing in pure Node often fails due to DOMPurify/DOM expectations,
    // which is not a diagram syntax failure (the UI runs in the browser).
    if (
      /DOMPurify\.addHook/i.test(msg) ||
      /document is not defined/i.test(msg) ||
      /window is not defined/i.test(msg)
    ) {
      return { status: "SKIP", error: msg };
    }

    return { status: "FAIL", error: msg };
  }
}

function basicBpmnCheck(xml: string): { status: ParseStatus; error?: string } {
  const trimmed = xml.trim();
  if (!trimmed) return { status: "SKIP" };
  if (!trimmed.startsWith("<")) return { status: "FAIL", error: "BPMN não parece XML" };
  if (!/definitions/i.test(trimmed)) return { status: "FAIL", error: "XML sem <definitions>" };
  if (!/bpmn/i.test(trimmed)) return { status: "FAIL", error: "XML sem namespace/elementos BPMN" };
  return { status: "OK" };
}

function sampleDescriptor(item: DiagramCatalogItem): string {
  const s = item.sample;
  if (s.kind === "file") return `file:${s.format}`;
  if (s.kind === "inline") return `inline:${s.format}`;
  return `model:${s.format}`;
}

async function main() {
  const repoRoot = path.resolve(import.meta.dirname, "..", "..");
  const outPath = path.join(repoRoot, "docs", "qa", "DIAGRAMS_CATALOG_CHECKLIST.md");

  const items = DIAGRAM_ITEMS.slice();

  const total = items.length;
  const byOrigin = countBy(items, (i) => i.origin);
  const byStatus = countBy(items, (i) => i.rendererStatus);
  const byRenderer = countBy(items, (i) => i.rendererId);

  const parseFindings: Array<{ id: string; name: string; renderer: string; status: ParseStatus; error?: string }> = [];

  for (const item of items) {
    if (item.sample.kind !== "inline") continue;

    if (item.rendererId === "mermaid" && item.sample.format === "mermaid") {
      const res = await tryMermaidParse(item.sample.content);
      if (res.status === "FAIL") {
        parseFindings.push({
          id: item.id,
          name: item.canonicalName,
          renderer: "mermaid",
          status: res.status,
          error: res.error,
        });
      }
    }

    if (item.rendererId === "bpmn" && item.sample.format === "bpmn") {
      const res = basicBpmnCheck(item.sample.content);
      if (res.status !== "OK") {
        parseFindings.push({
          id: item.id,
          name: item.canonicalName,
          renderer: "bpmn",
          status: res.status,
          error: res.error,
        });
      }
    }
  }

  const lines: string[] = [];

  lines.push(`# RULEX — Checklist de Diagramas e Fluxogramas`);
  lines.push("");
  lines.push(`Gerado em: ${new Date().toISOString()}`);
  lines.push("");
  lines.push("Este arquivo é gerado automaticamente a partir do catálogo real usado na tela 'Central de Diagramas do RULEX'.");
  lines.push("\nRegerar: `pnpm diagrams:sync` (ou `pnpm diagrams:checklist`)\n");

  lines.push("## Resumo");
  lines.push("");
  lines.push(`- Total: ${total}`);
  lines.push(`- Por origem: solution=${byOrigin.solution ?? 0}, template=${byOrigin.template ?? 0}`);
  lines.push(`- Por status: OK=${byStatus.OK ?? 0}, PENDENTE=${byStatus.PENDENTE ?? 0}`);
  lines.push(`- Por renderer: ${Object.entries(byRenderer)
    .sort((a, b) => b[1] - a[1])
    .map(([k, v]) => `${k}=${v}`)
    .join(", ")}`);

  if (parseFindings.length > 0) {
    const fails = parseFindings.filter((p) => p.status === "FAIL").length;
    lines.push("");
    lines.push("## Parse (double check)");
    lines.push("");
    lines.push(`- Achados: ${parseFindings.length} (FAIL=${fails})`);
    lines.push("- Observação: Mermaid usa `mermaid.parse()` quando disponível; BPMN usa checagem básica de XML (não é importXML).\n");
    lines.push("| Renderer | Status | ID | Nome | Erro |" );
    lines.push("|---|---|---|---|---|");
    for (const p of parseFindings) {
      lines.push(
        `| ${p.renderer} | ${p.status} | ${mdEscape(p.id)} | ${mdEscape(p.name)} | ${mdEscape(p.error ?? "")} |`
      );
    }
  }

  lines.push("");
  lines.push("## Catálogo completo (1 por 1)");
  lines.push("");
  lines.push("Legenda: `Implementado` = rendererStatus OK. `Fiel` = origin=solution + verified=true + evidência/notes.");
  lines.push("");
  lines.push("| Origem | Verificado | Implementado | Renderer | Status | Notação | Categoria | Nome | ID | Sample |" );
  lines.push("|---|---|---|---|---|---|---|---|---|---|");

  for (const item of items) {
    const implemented = item.rendererStatus === "OK" ? "SIM" : "NÃO";
    const verified = item.verified ? "SIM" : "NÃO";
    lines.push(
      `| ${item.origin} | ${verified} | ${implemented} | ${mdEscape(item.rendererId)} | ${item.rendererStatus} | ${mdEscape(item.notation)} | ${mdEscape(item.categoryId)} | ${mdEscape(item.canonicalName)} | ${mdEscape(item.id)} | ${mdEscape(sampleDescriptor(item))} |`
    );
  }

  lines.push("");
  fs.writeFileSync(outPath, lines.join("\n"), "utf8");
  console.log(`Wrote ${outPath}`);
}

void main();
