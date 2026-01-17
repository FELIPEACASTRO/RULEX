import fs from "node:fs";
import path from "node:path";

import { DIAGRAM_ITEMS } from "../../client/src/features/diagrams/registry/diagramRegistry";
import type { DiagramCatalogItem, DiagramFormat } from "../../client/src/features/diagrams/types";

function mdEscapeInline(text: string): string {
  return text.replace(/\r?\n/g, " ").trim();
}

function slugifyId(id: string): string {
  return (
    "d-" +
    id
      .trim()
      .toLowerCase()
      .replace(/[^a-z0-9]+/g, "-")
      .replace(/^-+|-+$/g, "")
  );
}

function areaForCategory(categoryId: DiagramCatalogItem["categoryId"]): string {
  switch (categoryId) {
    case "processos":
      return "Negócio";
    case "frontend":
      return "Frontend";
    case "api":
      return "Backend";
    case "uml_estrutural":
    case "uml_comportamental":
      return "Backend";
    case "arquitetura":
      return "Infraestrutura";
    case "infra":
      return "Infraestrutura";
    case "dados_postgres":
    case "dados_redis":
    case "dados_neo4j":
      return "Bancos de Dados";
    case "seguranca":
      return "Transversal";
    case "qualidade":
      return "Transversal";
    case "ddd":
      return "Backend";
    case "cs_classicos":
      return "Transversal";
    default: {
      const _exhaustive: never = categoryId;
      return _exhaustive;
    }
  }
}

function objectiveFor(item: DiagramCatalogItem): string {
  switch (item.notation) {
    case "C4":
      return "Dar visão de contexto/containers/componentes do sistema e suas dependências.";
    case "DFD":
      return "Evidenciar fluxo de dados, fronteiras e armazenamentos para análise de riscos e privacidade.";
    case "ER":
      return "Documentar o modelo de dados persistido (tabelas/entidades e relacionamentos quando evidenciados).";
    case "UML":
      return "Explicar estrutura e comportamento (classes/seq/estados) para implementação e manutenção.";
    case "BPMN":
      return "Explicar processo de negócio/operacional e handoffs.";
    case "DMN":
      return "Documentar decisão/score/roteamento como tabela de decisão (quando aplicável).";
    case "GRAPH":
      return "Representar redes e relações (ex.: grafos de fraude) para inspeção e comunicação.";
    case "MATRIX":
      return "Evidenciar cobertura/impacto (ex.: regras × campos, endpoints × papéis, etc.).";
    case "EPC":
      return "Representar processos orientados a eventos/funções (notação EPC).";
    case "ARCHIMATE":
      return "Representar camadas e dependências de arquitetura empresarial (quando aplicável).";
    case "TREE":
      return "Representar hierarquia (ex.: decomposição de domínio/risco/regra).";
    case "PROCESS":
    case "FLOWCHART":
      return "Representar fluxo de passos e pontos de decisão em alto nível.";
    case "OTHER":
    default:
      return "Documentar visualmente um aspecto do sistema conforme o catálogo oficial.";
  }
}

function whatItRepresentsFor(item: DiagramCatalogItem): string {
  if (item.origin === "solution" && item.verified) {
    return "Representa a solução RULEX conforme verificado (ver notas de verificação e evidências citadas).";
  }
  return "Representa um template didático do tipo de diagrama (não é garantia de fidelidade ao RULEX).";
}

function risksIfAbsentFor(item: DiagramCatalogItem): string {
  switch (item.notation) {
    case "C4":
      return "Perda de visão sistêmica, decisões de integração/infra sem alinhamento e maior risco operacional.";
    case "DFD":
      return "Risco de lacunas em privacidade/segurança (dados sensíveis), fronteiras mal definidas e auditoria frágil.";
    case "ER":
      return "Ambiguidade sobre persistência/relacionamentos, maior risco de inconsistências de schema e bugs de dados.";
    case "UML":
      return "Aumento do custo de manutenção e chance de regressões por falta de entendimento do comportamento/estrutura.";
    case "BPMN":
      return "Processo operacional ambíguo, falhas de governança e divergência entre áreas sobre o fluxo real.";
    case "DMN":
      return "Decisões de risco pouco auditáveis, dificuldade de explicar score e maior risco de divergência entre regra e implementação.";
    case "GRAPH":
      return "Menor capacidade de explicar relações/rede de fraude e dificuldade em validar hipóteses de grafos.";
    case "MATRIX":
      return "Dificulta rastreabilidade de cobertura (campos, regras, papéis) e aumenta risco de gaps não percebidos.";
    default:
      return "Diminui auditabilidade e entendimento compartilhado, aumentando risco de gaps e retrabalho.";
  }
}

function sampleToMarkdown(item: DiagramCatalogItem): string {
  const s = item.sample;
  if (s.kind === "inline") {
    const fmt: DiagramFormat = s.format;
    if (fmt === "mermaid") {
      return ["```mermaid", s.content.trimEnd(), "```"].join("\n");
    }

    if (fmt === "bpmn" || fmt === "dmn") {
      return ["```xml", s.content.trimEnd(), "```"].join("\n");
    }

    if (fmt === "dfd" || fmt === "epc" || fmt === "plantuml") {
      return ["```text", s.content.trimEnd(), "```"].join("\n");
    }

    return ["```text", s.content.trimEnd(), "```"].join("\n");
  }

  if (s.kind === "json") {
    return ["```json", JSON.stringify(s.data, null, 2), "```"].join("\n");
  }

  // kind === "file"
  return `Arquivo/URI (catálogo): ${s.uri}`;
}

function buildIndex(items: DiagramCatalogItem[]): string[] {
  const lines: string[] = [];
  lines.push("| Área | Origem | Verificado | Categoria | Notação | Nome | ID |" );
  lines.push("|---|---|---:|---|---|---|---|");
  for (const item of items) {
    const area = areaForCategory(item.categoryId);
    const anchor = slugifyId(item.id);
    const verified = item.verified ? "SIM" : "NÃO";
    lines.push(
      `| ${area} | ${item.origin} | ${verified} | ${item.categoryLabel} | ${item.notation} | [${mdEscapeInline(
        item.canonicalName,
      )}](#${anchor}) | ${mdEscapeInline(item.id)} |`,
    );
  }
  return lines;
}

async function main() {
  const repoRoot = path.resolve(import.meta.dirname, "..", "..");
  const outPath = path.join(repoRoot, "docs", "DIAGRAMAS.md");

  const items = DIAGRAM_ITEMS.slice().sort((a, b) => a.id.localeCompare(b.id));

  const total = items.length;
  const totalSolution = items.filter((i) => i.origin === "solution").length;
  const totalTemplates = items.filter((i) => i.origin === "template").length;

  const lines: string[] = [];

  lines.push("# DIAGRAMAS — Catálogo Oficial do RULEX");
  lines.push("");
  lines.push(`Gerado em: ${new Date().toISOString()}`);
  lines.push("");
  lines.push(
    "Este documento é o índice oficial e **auditável** dos diagramas/fluxogramas disponíveis no RULEX. Ele é gerado a partir do mesmo catálogo utilizado na UI (tela **Central de Diagramas**), evitando divergências.",
  );
  lines.push("");
  lines.push("## Regras de rigor (NÃO inventar / NÃO presumir)");
  lines.push("");
  lines.push("- Itens `origin=solution` e `verified=true` são tratados como **representação fiel** do RULEX, dentro do escopo descrito nas notas de verificação.");
  lines.push("- Itens `origin=template` são **modelos didáticos**: podem usar nomes genéricos e **não devem** ser interpretados como verdade do sistema.");
  lines.push("- Quando não há evidência suficiente no repositório para detalhar algo (ex.: processos AS-IS/TO-BE de negócio), o catálogo mantém o item como template e isso é explicitado.");

  lines.push("");
  lines.push("## Como acessar e regerar");
  lines.push("");
  lines.push("- UI (central): rota `/diagrams` no frontend.");
  lines.push("- Checklist QA (tabela completa): `docs/qa/DIAGRAMS_CATALOG_CHECKLIST.md`.");
  lines.push("- Regerar este documento e o checklist a partir do catálogo:");
  lines.push("  - `pnpm diagrams:doc` (gera `docs/DIAGRAMAS.md`)" );
  lines.push("  - `pnpm diagrams:sync` (sincroniza inventário → client + gera checklist)" );

  lines.push("");
  lines.push("## Resumo");
  lines.push("");
  lines.push(`- Total de itens no catálogo: ${total}`);
  lines.push(`- Por origem: solution=${totalSolution}, template=${totalTemplates}`);

  lines.push("");
  lines.push("## Índice (1 por 1)");
  lines.push("");
  lines.push("A tabela abaixo lista **todos** os diagramas/fluxogramas do catálogo e linka para o detalhamento individual (Objetivo/Quando usar/O que representa/Riscos)." );
  lines.push("");
  lines.push(...buildIndex(items));

  // Estrutura por áreas (conforme o pedido): Negócio, Usuário, Frontend, Backend, Bancos de Dados, Infra, Transversal.
  // Observação: a classificação é derivada do `categoryId` do catálogo.
  const byArea = new Map<string, DiagramCatalogItem[]>();
  for (const item of items) {
    const area = areaForCategory(item.categoryId);
    const list = byArea.get(area) ?? [];
    list.push(item);
    byArea.set(area, list);
  }

  lines.push("");
  lines.push("## Cobertura por área (Negócio / Usuário / Frontend / Backend / Bancos de Dados / Infra / Transversal)");
  lines.push("");
  lines.push(
    "Esta seção não adiciona novos fatos — apenas agrupa os itens já presentes no catálogo.",
  );

  const areaOrder = ["Negócio", "Usuário", "Frontend", "Backend", "Bancos de Dados", "Infraestrutura", "Transversal"];

  // Alguns repositórios podem não ter categoria explícita "Usuário"; neste caso, a área ficará vazia.
  for (const area of areaOrder) {
    const list = (byArea.get(area) ?? []).slice().sort((a, b) => a.id.localeCompare(b.id));
    lines.push("");
    lines.push(`### ${area}`);
    lines.push("");
    if (list.length === 0) {
      lines.push("Sem itens categorizados explicitamente nesta área no catálogo atual.");
      continue;
    }
    lines.push(`- Itens: ${list.length}`);
    for (const item of list) {
      const anchor = slugifyId(item.id);
      lines.push(`- [${mdEscapeInline(item.canonicalName)}](#${anchor}) (${mdEscapeInline(item.id)})`);
    }
  }

  lines.push("");
  lines.push("## Catálogo completo — detalhamento (1 por 1)");
  lines.push("");

  // Agrupar por categoria primeiro para facilitar leitura.
  const byCategory = new Map<string, DiagramCatalogItem[]>();
  for (const item of items) {
    const key = `${item.categoryLabel}||${item.categoryId}`;
    const list = byCategory.get(key) ?? [];
    list.push(item);
    byCategory.set(key, list);
  }

  const categoryKeys = Array.from(byCategory.keys()).sort((a, b) => a.localeCompare(b));

  for (const key of categoryKeys) {
    const [categoryLabel, categoryId] = key.split("||");
    const list = (byCategory.get(key) ?? []).slice().sort((a, b) => a.id.localeCompare(b.id));

    lines.push("");
    lines.push(`### Categoria: ${categoryLabel} (${categoryId})`);
    lines.push("");

    for (const item of list) {
      const anchor = slugifyId(item.id);
      const area = areaForCategory(item.categoryId);

      lines.push("");
      lines.push(`<a id="${anchor}"></a>`);
      lines.push(`#### ${mdEscapeInline(item.canonicalName)} — ${mdEscapeInline(item.id)}`);
      lines.push("");
      lines.push(`- Área: ${area}`);
      lines.push(`- Origem: ${item.origin} (verified=${item.verified ? "true" : "false"})`);
      lines.push(`- Implementado (rendererStatus): ${item.rendererStatus}`);
      lines.push(`- Renderer: ${item.rendererId}`);
      lines.push(`- Notação: ${item.notation}`);

      lines.push("");
      lines.push("**Objetivo**");
      lines.push(`- ${objectiveFor(item)}`);

      lines.push("");
      lines.push("**Quando usar**");
      lines.push(`- ${mdEscapeInline(item.descriptionWhenToUse)}`);

      lines.push("");
      lines.push("**O que representa**");
      lines.push(`- ${whatItRepresentsFor(item)}`);

      lines.push("");
      lines.push("**Riscos se ausente**");
      lines.push(`- ${risksIfAbsentFor(item)}`);

      lines.push("");
      lines.push("**Notas de verificação / evidência**");
      if (item.verificationNotes && item.verificationNotes.trim()) {
        lines.push(`- ${mdEscapeInline(item.verificationNotes)}`);
      } else {
        lines.push("- (sem notas adicionais no catálogo)");
      }

      lines.push("");
      lines.push("**Diagrama (sample do catálogo)**");
      lines.push("");
      lines.push(sampleToMarkdown(item));
    }
  }

  lines.push("");
  fs.writeFileSync(outPath, lines.join("\n"), "utf8");
  console.log(`Wrote ${outPath}`);
}

void main();
