#!/usr/bin/env node
/*
 * PASSADA 2 (Consistência cruzada) — Central de Diagramas “Solução RULEX”
 *
 * Cruza fontes reais do repositório:
 * - Inventário do backend (docs/DIAGRAMS_RULEX_INVENTORY.json)
 * - OpenAPI (openapi/rulex.yaml)
 * - Insomnia collection (Insomnia/*.json)
 * - E2E specs (e2e/*.spec.ts)
 * - Chamadas /api no frontend (client/src)
 *
 * Saídas:
 * - docs/DIAGRAMS_RULEX_PASSADA2.json
 * - docs/DIAGRAMS_RULEX_PASSADA2_REPORT.md
 */

import fs from "node:fs/promises";
import path from "node:path";

const REPO_ROOT = process.cwd();

function toPosix(p) {
  return p.split(path.sep).join("/");
}

function rel(p) {
  return toPosix(path.relative(REPO_ROOT, p));
}

async function readText(filePath) {
  try {
    return await fs.readFile(filePath, "utf8");
  } catch {
    return null;
  }
}

async function readJson(filePath) {
  const text = await readText(filePath);
  if (!text) return null;
  return JSON.parse(text);
}

async function pathExists(p) {
  try {
    await fs.access(p);
    return true;
  } catch {
    return false;
  }
}

function indexToLine(text, index) {
  let line = 1;
  for (let i = 0; i < index && i < text.length; i++) {
    if (text[i] === "\n") line++;
  }
  return line;
}

function getLine(text, lineNumber) {
  const lines = text.split(/\r?\n/);
  return lines[lineNumber - 1] ?? "";
}

function stripQuery(p) {
  const q = p.indexOf("?");
  return q >= 0 ? p.slice(0, q) : p;
}

function normalizePath(p) {
  if (!p) return null;
  const s = stripQuery(String(p)).trim();
  if (!s) return null;
  const withLeading = s.startsWith("/") ? s : `/${s}`;
  return withLeading.replace(/\/+$/g, "").replace(/\/+/g, "/");
}

function pathKey(p) {
  const norm = normalizePath(p);
  if (!norm) return null;
  const parts = norm.split("/").filter(Boolean);
  const keyed = parts.map((seg) => {
    if (/^\{[^}]+\}$/.test(seg)) return "{}";
    if (/^\{\{.*\}\}$/.test(seg)) return "{}";
    // Heurísticas para valores de exemplo (Insomnia/Frontend/E2E) que deveriam casar com {id}
    if (/^v\d+$/i.test(seg)) return seg; // versões (v1, v31, ...)
    if (/^\d+$/.test(seg)) return "{}";
    if (/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(seg)) return "{}";
    if (/^(true|false)$/i.test(seg)) return "{}";
    if (/^ext-\d+$/i.test(seg)) return "{}";
    if (/^[A-Z0-9_\-]{2,}$/.test(seg)) return "{}";
    if (/^[A-Za-z]+-\d+$/.test(seg)) return "{}";
    return seg;
  });
  return `/${keyed.join("/")}`;
}

function endpointKey(method, p) {
  const m = (method ?? "").toUpperCase();
  const pk = pathKey(p);
  if (!pk) return null;
  return `${m} ${pk}`.trim();
}

function extractContextPathFromApplicationYml(ymlText) {
  // Heurística: pega a primeira ocorrência de "context-path: /api" (ou similar)
  const re = /^\s*context-path\s*:\s*(\S+)\s*$/m;
  const m = re.exec(ymlText ?? "");
  if (!m) return null;
  return normalizePath(m[1]);
}

function joinPaths(prefix, p) {
  const a = normalizePath(prefix ?? "") ?? "";
  const b = normalizePath(p ?? "") ?? "";
  if (!a) return b || "/";
  if (!b || b === "/") return a;
  return normalizePath(`${a}/${b.replace(/^\//, "")}`);
}

function parseOpenApiYamlEndpoints(yamlText, fileRel) {
  const lines = yamlText.split(/\r?\n/);
  let inPaths = false;
  let currentPath = null;
  const endpoints = [];

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    if (!inPaths) {
      if (line.trim() === "paths:") inPaths = true;
      continue;
    }

    // Sai do bloco paths quando encontrar uma chave top-level
    if (/^[^\s#].*:\s*$/.test(line) && !/^paths\s*:\s*$/.test(line.trim())) {
      break;
    }

    const pathMatch = /^\s{2}(\/[^:]+)\s*:\s*$/.exec(line);
    if (pathMatch) {
      currentPath = pathMatch[1];
      continue;
    }

    const methodMatch = /^\s{4}(get|post|put|delete|patch)\s*:\s*$/.exec(line);
    if (methodMatch && currentPath) {
      const method = methodMatch[1].toUpperCase();
      endpoints.push({
        method,
        path: currentPath,
        evidence: {
          file: fileRel,
          line: i + 1,
          snippet: line.trim(),
        },
      });
    }
  }

  return endpoints;
}

function parseInsomniaRequests(insomniaJson, fileText, fileRel) {
  const resources = Array.isArray(insomniaJson?.resources) ? insomniaJson.resources : [];

  // Base path (tipicamente "/api") — se existir
  const envBase = resources.find((r) => r && r._type === "environment" && r._id === "env_rulex_base");
  const basePath = envBase?.data?.base_path ? normalizePath(envBase.data.base_path) : null;
  const baseUrlToken = "{{ _.base_url }}";

  const requests = [];

  for (const r of resources) {
    if (!r || r._type !== "request") continue;

    const method = (r.method ?? "(unspecified)").toUpperCase();
    const url = r.url ?? "";

    let extracted = null;

    if (typeof url === "string") {
      if (url.includes(baseUrlToken)) {
        const remainder = url.split(baseUrlToken)[1] ?? "";
        const remPath = normalizePath(remainder);
        extracted = basePath ? joinPaths(basePath, remPath) : remPath;
      } else {
        // tenta achar um trecho /api/... dentro da string
        const m = /(\/api\/[^"']+)/.exec(url);
        if (m) extracted = normalizePath(m[1]);
      }
    }

    if (!extracted) continue;

    const id = r._id ?? null;
    const idx = id && typeof fileText === "string" ? fileText.indexOf(id) : -1;
    const line = idx >= 0 ? indexToLine(fileText, idx) : null;

    requests.push({
      id,
      name: r.name ?? "(unnamed)",
      method,
      path: extracted,
      evidence: {
        file: fileRel,
        line: line ?? undefined,
      },
    });
  }

  return { basePath, requests };
}

async function walkFiles(rootDir, predicate) {
  const out = [];
  if (!(await pathExists(rootDir))) return out;

  const stack = [rootDir];
  while (stack.length) {
    const dir = stack.pop();
    const entries = await fs.readdir(dir, { withFileTypes: true });
    for (const entry of entries) {
      const fullPath = path.join(dir, entry.name);
      if (entry.isDirectory()) {
        // evita node_modules etc
        if (["node_modules", ".git", "dist", "build", "target"].includes(entry.name)) continue;
        stack.push(fullPath);
        continue;
      }
      if (entry.isFile()) {
        if (!predicate || predicate(fullPath)) out.push(fullPath);
      }
    }
  }
  return out;
}

function extractFrontendApiLiterals(text) {
  // Captura literal de string contendo /api/... (simples e suficiente p/ PASSADA 2)
  const out = [];
  const re = /(['"`])([^\n\r]*?\/api\/[^\n\r'"`]+)\1/g;
  let m;
  while ((m = re.exec(text))) {
    out.push(m[2]);
  }
  return out;
}

function parseE2eApiHits(text) {
  const out = [];
  const re = /(['"`])([^\n\r]*?\/api\/[^\n\r'"`]+)\1/g;
  let m;
  while ((m = re.exec(text))) {
    out.push(m[2]);
  }
  return out;
}

function diffKeys(aKeys, bKeys) {
  const a = new Set(aKeys);
  const b = new Set(bKeys);
  const onlyA = [];
  for (const k of a) if (!b.has(k)) onlyA.push(k);
  const onlyB = [];
  for (const k of b) if (!a.has(k)) onlyB.push(k);
  onlyA.sort();
  onlyB.sort();
  return { onlyA, onlyB };
}

async function main() {
  const docsDir = path.join(REPO_ROOT, "docs");
  await fs.mkdir(docsDir, { recursive: true });

  const inventoryPath = path.join(docsDir, "DIAGRAMS_RULEX_INVENTORY.json");
  const inventory = await readJson(inventoryPath);
  if (!inventory) {
    throw new Error("Inventário não encontrado. Rode primeiro: pnpm inventory:rulex");
  }

  const appYmlPath = path.join(REPO_ROOT, "backend", "src", "main", "resources", "application.yml");
  const appYml = (await readText(appYmlPath)) ?? "";
  const contextPath = extractContextPathFromApplicationYml(appYml) ?? "/api";

  // Backend endpoints (normalizados + prefixo /api)
  const backendEndpoints = [];
  for (const c of inventory?.backend?.controllers ?? []) {
    for (const e of c.endpoints ?? []) {
      const effPath = joinPaths(contextPath, e.path);
      backendEndpoints.push({
        method: e.httpMethod,
        path: effPath,
        rawPath: e.path,
        evidence: e.evidence,
      });
    }
  }

  // OpenAPI endpoints
  const openapiPath = path.join(REPO_ROOT, "openapi", "rulex.yaml");
  const openapiText = (await readText(openapiPath)) ?? "";
  const openapiEndpoints = parseOpenApiYamlEndpoints(openapiText, rel(openapiPath));

  // Insomnia endpoints
  const insomniaPath = path.join(REPO_ROOT, "Insomnia", "RULEX_Insomnia_Collection.json");
  const insomniaText = (await readText(insomniaPath)) ?? "";
  const insomniaJson = insomniaText ? JSON.parse(insomniaText) : null;
  const insomnia = insomniaJson ? parseInsomniaRequests(insomniaJson, insomniaText, rel(insomniaPath)) : { basePath: null, requests: [] };

  // E2E hits
  const e2eRoot = path.join(REPO_ROOT, "e2e");
  const e2eFiles = await walkFiles(e2eRoot, (p) => p.endsWith(".spec.ts"));
  const e2eHits = [];
  for (const f of e2eFiles) {
    const t = await readText(f);
    if (!t) continue;
    const hits = parseE2eApiHits(t);
    for (const h of hits) {
      const idx = t.indexOf(h);
      const line = idx >= 0 ? indexToLine(t, idx) : null;
      e2eHits.push({
        path: normalizePath(h),
        evidence: { file: rel(f), line: line ?? undefined },
      });
    }
  }

  // Frontend hits
  const frontendRoot = path.join(REPO_ROOT, "client", "src");
  const frontendFiles = await walkFiles(frontendRoot, (p) => /\.(ts|tsx)$/.test(p) && !/\.test\.(ts|tsx)$/.test(p));
  const frontendApiHits = [];
  for (const f of frontendFiles) {
    const t = await readText(f);
    if (!t) continue;
    const hits = extractFrontendApiLiterals(t);
    for (const h of hits) {
      const idx = t.indexOf(h);
      const line = idx >= 0 ? indexToLine(t, idx) : null;
      frontendApiHits.push({
        path: normalizePath(h),
        evidence: { file: rel(f), line: line ?? undefined },
      });
    }
  }

  const backendKeys = backendEndpoints
    .map((e) => endpointKey(e.method, e.path))
    .filter(Boolean);
  const openapiKeys = openapiEndpoints
    .map((e) => endpointKey(e.method, e.path))
    .filter(Boolean);
  const insomniaKeys = insomnia.requests
    .filter((r) => !String(r.path ?? "").startsWith("/api/actuator"))
    .map((r) => endpointKey(r.method, r.path))
    .filter(Boolean);

  const backendVsOpenapi = diffKeys(backendKeys, openapiKeys);
  const backendVsInsomnia = diffKeys(backendKeys, insomniaKeys);

  // methodless path-keys (para e2e/frontend)
  const backendPathKeys = backendEndpoints.map((e) => pathKey(e.path)).filter(Boolean);
  const e2ePathKeys = e2eHits.map((h) => pathKey(h.path)).filter(Boolean);
  const frontendPathKeys = frontendApiHits.map((h) => pathKey(h.path)).filter(Boolean);

  const backendVsE2e = diffKeys(backendPathKeys, e2ePathKeys);
  const backendVsFrontend = diffKeys(backendPathKeys, frontendPathKeys);

  const result = {
    generatedAt: new Date().toISOString(),
    contextPath,
    counts: {
      backendEndpoints: backendEndpoints.length,
      openapiEndpoints: openapiEndpoints.length,
      insomniaRequests: insomnia.requests.length,
      e2eApiHits: e2eHits.length,
      frontendApiHits: frontendApiHits.length,
    },
    comparisons: {
      backendVsOpenapi,
      backendVsInsomnia,
      backendVsE2e,
      backendVsFrontend,
    },
    sources: {
      inventory: rel(inventoryPath),
      openapi: rel(openapiPath),
      insomnia: rel(insomniaPath),
      e2eFiles: e2eFiles.map(rel).sort(),
    },
  };

  const outJsonPath = path.join(docsDir, "DIAGRAMS_RULEX_PASSADA2.json");
  await fs.writeFile(outJsonPath, `${JSON.stringify(result, null, 2)}\n`, "utf8");

  const md = [];
  md.push("# Central de Diagramas — Solução RULEX (PASSADA 2: Consistência)\n");
  md.push("Relatório gerado automaticamente a partir de cruzamento entre fontes do repositório.\n");
  md.push(`- Gerado em: ${result.generatedAt}`);
  md.push(`- Context path (backend): \`${result.contextPath}\``);
  md.push(`- JSON: \`docs/DIAGRAMS_RULEX_PASSADA2.json\`\n`);

  md.push("## Contagens\n");
  md.push(`- Backend endpoints (inventário + prefixo): ${result.counts.backendEndpoints}`);
  md.push(`- OpenAPI endpoints: ${result.counts.openapiEndpoints}`);
  md.push(`- Insomnia requests: ${result.counts.insomniaRequests}`);
  md.push(`- E2E hits contendo /api: ${result.counts.e2eApiHits}`);
  md.push(`- Frontend hits contendo /api (não-test): ${result.counts.frontendApiHits}\n`);

  md.push("## Gaps (Backend ↔ OpenAPI)\n");
  md.push(`- Backend não documentado no OpenAPI: ${backendVsOpenapi.onlyA.length}`);
  md.push(`- OpenAPI sem endpoint correspondente no Backend: ${backendVsOpenapi.onlyB.length}\n`);

  md.push("**Backend → faltando no OpenAPI (amostra)**");
  backendVsOpenapi.onlyA.slice(0, 50).forEach((k) => md.push(`- ${k}`));
  if (backendVsOpenapi.onlyA.length > 50) md.push(`- … +${backendVsOpenapi.onlyA.length - 50} outros`);
  md.push("");

  md.push("**OpenAPI → faltando no Backend (amostra)**");
  backendVsOpenapi.onlyB.slice(0, 50).forEach((k) => md.push(`- ${k}`));
  if (backendVsOpenapi.onlyB.length > 50) md.push(`- … +${backendVsOpenapi.onlyB.length - 50} outros`);
  md.push("");

  md.push("## Gaps (Backend ↔ Insomnia)\n");
  md.push(`- Backend não coberto na coleção Insomnia: ${backendVsInsomnia.onlyA.length}`);
  md.push(`- Insomnia sem endpoint correspondente no Backend: ${backendVsInsomnia.onlyB.length}\n`);

  md.push("**Backend → faltando no Insomnia (amostra)**");
  backendVsInsomnia.onlyA.slice(0, 50).forEach((k) => md.push(`- ${k}`));
  if (backendVsInsomnia.onlyA.length > 50) md.push(`- … +${backendVsInsomnia.onlyA.length - 50} outros`);
  md.push("");

  md.push("**Insomnia → faltando no Backend (amostra)**");
  backendVsInsomnia.onlyB.slice(0, 50).forEach((k) => md.push(`- ${k}`));
  if (backendVsInsomnia.onlyB.length > 50) md.push(`- … +${backendVsInsomnia.onlyB.length - 50} outros`);
  md.push("");

  md.push("## Sinais (methodless)\n");
  md.push(`- Backend paths sem match em E2E (apenas presença de \"/api\"): ${backendVsE2e.onlyA.length}`);
  md.push(`- Backend paths sem match em Frontend (apenas presença de \"/api\"): ${backendVsFrontend.onlyA.length}\n`);

  md.push("## Notas\n");
  md.push("- Comparações usam normalização por segmentos (variáveis {id} e {{ _.var }} viram {}).");
  md.push("- E2E/Frontend nesta etapa são heurísticos: apenas detectam literais contendo /api.");

  const outMdPath = path.join(docsDir, "DIAGRAMS_RULEX_PASSADA2_REPORT.md");
  await fs.writeFile(outMdPath, `${md.join("\n")}\n`, "utf8");

  console.log("✅ PASSADA 2 gerada:");
  console.log(`- ${rel(outJsonPath)}`);
  console.log(`- ${rel(outMdPath)}`);
  console.log("\nResumo:");
  console.log(`- Backend endpoints: ${result.counts.backendEndpoints}`);
  console.log(`- OpenAPI endpoints: ${result.counts.openapiEndpoints}`);
  console.log(`- Insomnia requests: ${result.counts.insomniaRequests}`);
  console.log(`- Backend→OpenAPI gaps: ${backendVsOpenapi.onlyA.length} missing, ${backendVsOpenapi.onlyB.length} extra`);
  console.log(`- Backend→Insomnia gaps: ${backendVsInsomnia.onlyA.length} missing, ${backendVsInsomnia.onlyB.length} extra`);
}

main().catch((err) => {
  console.error("❌ Falha na PASSADA 2:");
  console.error(err);
  process.exit(1);
});
