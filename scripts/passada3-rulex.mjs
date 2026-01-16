#!/usr/bin/env node
/*
 * PASSADA 3 (Validação executável) — Central de Diagramas “Solução RULEX”
 *
 * Objetivos:
 * - Executar (de forma reproduzível) os scripts PASSADA 1 e PASSADA 2
 * - Executar validações executáveis do repo (frontend e backend)
 * - Gerar relatório com evidências (exit codes + logs resumidos)
 *
 * Saídas:
 * - docs/DIAGRAMS_RULEX_PASSADA3.json
 * - docs/DIAGRAMS_RULEX_PASSADA3_REPORT.md
 */

import fs from "node:fs/promises";
import path from "node:path";
import { spawn } from "node:child_process";

const REPO_ROOT = process.cwd();

function toPosix(p) {
  return p.split(path.sep).join("/");
}

function rel(p) {
  return toPosix(path.relative(REPO_ROOT, p));
}

async function pathExists(p) {
  try {
    await fs.access(p);
    return true;
  } catch {
    return false;
  }
}

function truncate(s, max = 20000) {
  const text = String(s ?? "");
  if (text.length <= max) return text;
  return `${text.slice(0, max)}\n\n… (truncado; total=${text.length} chars)`;
}

async function runCommand(label, command, options = {}) {
  const startedAt = new Date().toISOString();
  const cwd = options.cwd ?? REPO_ROOT;

  return await new Promise((resolve) => {
    const child = spawn(command, {
      cwd,
      shell: true,
      env: process.env,
    });

    let stdout = "";
    let stderr = "";

    child.stdout.on("data", (d) => (stdout += d.toString()));
    child.stderr.on("data", (d) => (stderr += d.toString()));

    child.on("close", (code) => {
      resolve({
        label,
        command,
        cwd: rel(cwd) || ".",
        startedAt,
        finishedAt: new Date().toISOString(),
        exitCode: code ?? -1,
        ok: code === 0,
        stdout: truncate(stdout),
        stderr: truncate(stderr),
      });
    });

    child.on("error", (err) => {
      resolve({
        label,
        command,
        cwd: rel(cwd) || ".",
        startedAt,
        finishedAt: new Date().toISOString(),
        exitCode: -1,
        ok: false,
        stdout: "",
        stderr: `spawn error: ${err?.message ?? String(err)}`,
      });
    });
  });
}

async function cleanupBackendTarget() {
  const targetDir = path.join(REPO_ROOT, "backend", "target");
  const startedAt = new Date().toISOString();
  try {
    await fs.rm(targetDir, { recursive: true, force: true });
    return {
      label: "Backend: cleanup target",
      command: `fs.rm(${rel(targetDir) || "backend/target"})`,
      cwd: ".",
      startedAt,
      finishedAt: new Date().toISOString(),
      exitCode: 0,
      ok: true,
      stdout: "backend/target removido (force=true)",
      stderr: "",
    };
  } catch (err) {
    return {
      label: "Backend: cleanup target",
      command: `fs.rm(${rel(targetDir) || "backend/target"})`,
      cwd: ".",
      startedAt,
      finishedAt: new Date().toISOString(),
      exitCode: 1,
      ok: false,
      stdout: "",
      stderr: `Falha ao remover backend/target: ${err?.message ?? String(err)}`,
    };
  }
}

async function waitStep(label, ms) {
  const startedAt = new Date().toISOString();
  await new Promise((r) => setTimeout(r, ms));
  return {
    label,
    command: `sleep ${ms}ms`,
    cwd: ".",
    startedAt,
    finishedAt: new Date().toISOString(),
    exitCode: 0,
    ok: true,
    stdout: "",
    stderr: "",
  };
}

function mdEscape(s) {
  return String(s ?? "").replace(/\|/g, "\\|");
}

function mdCodeBlock(text) {
  const t = String(text ?? "");
  return `\n\n\`\`\`\n${t}\n\`\`\`\n`;
}

async function main() {
  const docsDir = path.join(REPO_ROOT, "docs");
  await fs.mkdir(docsDir, { recursive: true });

  // Preflight: sanity checks for expected files
  const preflight = {
    inventoryScript: await pathExists(path.join(REPO_ROOT, "scripts", "inventory-rulex.mjs")),
    consistencyScript: await pathExists(path.join(REPO_ROOT, "scripts", "consistency-rulex.mjs")),
    openapiSpec: await pathExists(path.join(REPO_ROOT, "openapi", "rulex.yaml")),
    insomnia: await pathExists(path.join(REPO_ROOT, "Insomnia", "RULEX_Insomnia_Collection.json")),
    backendPom: await pathExists(path.join(REPO_ROOT, "backend", "pom.xml")),
  };

  const steps = [];

  // Regenerar PASSADA 1/2 (garante que relatório reflete o estado atual do repo)
  steps.push(await runCommand("PASSADA 1: inventory", "pnpm inventory:rulex"));
  steps.push(await runCommand("PASSADA 2: consistency", "pnpm consistency:rulex"));

  // Frontend validation
  steps.push(await runCommand("Frontend: unit tests", "pnpm test"));
  steps.push(await runCommand("Frontend: build", "pnpm build"));

  // Backend validation
  // Obs: pode falhar se JDK/Maven não estiverem instalados; registramos como PENDENTE.
  steps.push(await cleanupBackendTarget());
  steps.push(await runCommand("Backend: mvn compile", "mvn -f backend/pom.xml -DskipTests compile"));
  steps.push(await waitStep("Backend: wait after compile", 2500));
  steps.push(await runCommand("Backend: mvn test", "mvn -f backend/pom.xml test"));

  const allOk = steps.every((s) => s.ok);

  const accessDeniedHints = steps
    .filter((s) => !s.ok)
    .some((s) => /AccessDeniedException/i.test(`${s.stdout}\n${s.stderr}`));

  const outJson = {
    generatedAt: new Date().toISOString(),
    preflight,
    overall: {
      ok: allOk,
      passed: steps.filter((s) => s.ok).length,
      failed: steps.filter((s) => !s.ok).length,
    },
    steps,
  };

  const outJsonPath = path.join(docsDir, "DIAGRAMS_RULEX_PASSADA3.json");
  await fs.writeFile(outJsonPath, `${JSON.stringify(outJson, null, 2)}\n`, "utf8");

  const md = [];
  md.push("# Central de Diagramas — Solução RULEX (PASSADA 3: Validação Executável)\n");
  md.push("Este relatório é gerado automaticamente e registra a execução real de comandos do repositório.\n");
  md.push(`- Gerado em: ${outJson.generatedAt}`);
  md.push(`- JSON: \`docs/DIAGRAMS_RULEX_PASSADA3.json\``);
  md.push(`- Status geral: ${allOk ? "OK" : "PENDENTE"}\n`);

  md.push("## Preflight\n");
  for (const [k, v] of Object.entries(preflight)) {
    md.push(`- ${k}: ${v ? "OK" : "FALTANDO"}`);
  }
  md.push("");

  md.push("## Execuções\n");
  md.push("| Etapa | OK | Exit | CWD | Comando |\n|---|---:|---:|---|---|");
  for (const s of steps) {
    md.push(
      `| ${mdEscape(s.label)} | ${s.ok ? "✅" : "❌"} | ${s.exitCode} | ${mdEscape(s.cwd)} | ${mdEscape(s.command)} |`,
    );
  }

  md.push("\n## Logs (resumo)\n");
  for (const s of steps) {
    md.push(`### ${s.ok ? "OK" : "FALHA"}: ${s.label}`);
    md.push(`- Comando: \`${s.command}\``);
    md.push(`- CWD: \`${s.cwd}\``);
    md.push(`- Exit: ${s.exitCode}`);
    if (s.stderr && s.stderr.trim()) {
      md.push("- STDERR:");
      md.push(mdCodeBlock(s.stderr));
    }
    if (s.stdout && s.stdout.trim()) {
      md.push("- STDOUT:");
      md.push(mdCodeBlock(s.stdout));
    }
  }

  md.push("\n## Interpretação\n");
  md.push("- Se todas as etapas estão OK, a PASSADA 3 está concluída.");
  md.push("- Se alguma etapa falhar (ex.: Maven/JDK ausente), a PASSADA 3 fica PENDENTE e o erro fica registrado acima.");

  if (accessDeniedHints) {
    md.push("\n## Diagnóstico (Windows / AccessDeniedException)\n");
    md.push(
      "Foi detectado erro de acesso negado durante compilação (ex.: leitura de .class em backend/target). Isso costuma ser causado por lock de arquivo (antivírus/indexação/IDE).",
    );
    md.push("\nSugestões práticas:");
    md.push("- Adicionar exclusão do Windows Defender/antivírus para a pasta do repo (principalmente backend/target)." );
    md.push("- Fechar processos que possam estar segurando arquivos do target (IDE/Java Language Server/terminal antigo) e reexecutar." );
    md.push("- Rodar novamente após reboot (locks fantasmas em Windows acontecem)." );
    md.push("- Tentar mover o workspace para um caminho curto (ex.: C:/work/RULEX) para reduzir atrito de IO." );
  }

  const outMdPath = path.join(docsDir, "DIAGRAMS_RULEX_PASSADA3_REPORT.md");
  await fs.writeFile(outMdPath, `${md.join("\n")}\n`, "utf8");

  console.log("✅ PASSADA 3 gerada:");
  console.log(`- ${rel(outJsonPath)}`);
  console.log(`- ${rel(outMdPath)}`);
  console.log(`\nStatus geral: ${allOk ? "OK" : "PENDENTE"}`);

  process.exit(allOk ? 0 : 2);
}

main().catch((err) => {
  console.error("❌ Falha na PASSADA 3:");
  console.error(err);
  process.exit(1);
});
