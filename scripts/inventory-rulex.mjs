#!/usr/bin/env node
/*
 * PASSADA 1 (Inventário) — Central de Diagramas “Solução RULEX”
 *
 * Objetivo: extrair inventário baseado em evidências do repositório (anti-alucinação).
 * Saídas:
 * - docs/DIAGRAMS_RULEX_INVENTORY.json
 * - docs/DIAGRAMS_RULEX_REPORT.md
 */

import fs from "node:fs/promises";
import path from "node:path";

const REPO_ROOT = process.cwd();

const IGNORE_DIRS = new Set([
	".git",
	"node_modules",
	"dist",
	"build",
	".turbo",
	".next",
	".vite",
	"target",
]);

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
				if (!IGNORE_DIRS.has(entry.name)) stack.push(fullPath);
				continue;
			}
			if (entry.isFile()) {
				if (!predicate || predicate(fullPath)) out.push(fullPath);
			}
		}
	}
	return out;
}

async function readText(filePath) {
	try {
		return await fs.readFile(filePath, "utf8");
	} catch {
		return null;
	}
}

function indexToLine(text, index) {
	// 1-based
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

function normalizeJoinPath(basePath, subPath) {
	const b = (basePath ?? "").trim();
	const s = (subPath ?? "").trim();
	const joined = `${b}/${s}`
		.replace(/\/+/g, "/")
		.replace(/\/+$/, "")
		.replace(/^\//, "/");
	return joined === "" ? "/" : joined;
}

function extractFirstStringLiteral(javaAnnotationArgs) {
	// Captura o primeiro "..." dentro dos parênteses.
	const m = /"([^"]*)"/.exec(javaAnnotationArgs ?? "");
	if (!m) return null;
	return m[1];
}

function extractNamedStringLiteral(javaAnnotationArgs, names) {
	for (const name of names) {
		const re = new RegExp(`${name}\\s*=\\s*\"([^\"]*)\"`);
		const m = re.exec(javaAnnotationArgs ?? "");
		if (m) return m[1];
	}
	return null;
}

function parseRequestMappingHttpMethod(javaAnnotationArgs) {
	const m = /(RequestMethod\.(GET|POST|PUT|DELETE|PATCH))/i.exec(javaAnnotationArgs ?? "");
	if (!m) return null;
	return m[2].toUpperCase();
}

function parseControllerFile(javaFilePath, text) {
	const hasController = /@RestController\b|@Controller\b/.test(text);
	if (!hasController) return null;

	const classNameMatch = /\bclass\s+(\w+)/.exec(text);
	const className = classNameMatch?.[1] ?? path.basename(javaFilePath);

	// tenta pegar o @RequestMapping mais próximo antes do class
	const classIndex = classNameMatch ? classNameMatch.index : 0;
	const beforeClass = text.slice(0, classIndex);
	let basePath = null;

	const rmRe = /@RequestMapping\s*\(([^)]*)\)/g;
	let rm;
	while ((rm = rmRe.exec(beforeClass))) {
		const args = rm[1] ?? "";
		basePath =
			extractNamedStringLiteral(args, ["value", "path"]) ??
			extractFirstStringLiteral(args) ??
			basePath;
	}

	const endpoints = [];
	const mappingRe = /@(GetMapping|PostMapping|PutMapping|DeleteMapping|PatchMapping|RequestMapping)\s*(\(([^)]*)\))?/g;

	let mm;
	while ((mm = mappingRe.exec(text))) {
		const annotation = mm[1];
		const fullArgs = mm[2] ?? "";
		const args = mm[3] ?? "";

		// Evita capturar o @RequestMapping da classe duas vezes (heurística simples)
		if (annotation === "RequestMapping" && mappingRe.lastIndex < classIndex) continue;

		const httpMethod =
			annotation === "GetMapping"
				? "GET"
				: annotation === "PostMapping"
					? "POST"
					: annotation === "PutMapping"
						? "PUT"
						: annotation === "DeleteMapping"
							? "DELETE"
							: annotation === "PatchMapping"
								? "PATCH"
								: parseRequestMappingHttpMethod(args);

		const subPath =
			extractNamedStringLiteral(args, ["value", "path"]) ??
			extractFirstStringLiteral(fullArgs) ??
			null;

		const endpointPath = normalizeJoinPath(basePath ?? "", subPath ?? "");

		const line = indexToLine(text, mm.index);
		endpoints.push({
			httpMethod: httpMethod ?? "(unspecified)",
			path: endpointPath,
			evidence: {
				file: rel(javaFilePath),
				line,
				snippet: getLine(text, line).trim(),
			},
		});
	}

	const controllerAnnotationIndex =
		text.indexOf("@RestController") >= 0
			? text.indexOf("@RestController")
			: Math.max(0, text.indexOf("@Controller"));

	return {
		className,
		basePath: basePath ?? "",
		evidence: {
			file: rel(javaFilePath),
			line: indexToLine(text, controllerAnnotationIndex),
		},
		endpoints,
	};
}

function extractAnnotatedClasses(text, annotation) {
	const hits = [];
	const re = new RegExp(`@${annotation}\\b`, "g");
	let m;
	while ((m = re.exec(text))) {
		const line = indexToLine(text, m.index);
		const after = text.slice(m.index);
		const classMatch = /\bclass\s+(\w+)/.exec(after);
		hits.push({
			className: classMatch?.[1] ?? "(unknown)",
			line,
			snippet: getLine(text, line).trim(),
		});
	}
	return hits;
}

function extractExtends(text, re, label) {
	const hits = [];
	const lines = text.split(/\r?\n/);
	for (let i = 0; i < lines.length; i++) {
		if (re.test(lines[i])) {
			const classMatch = /\binterface\s+(\w+)/.exec(lines[i]) ?? /\bclass\s+(\w+)/.exec(lines[i]);
			hits.push({
				name: classMatch?.[1] ?? label,
				line: i + 1,
				snippet: lines[i].trim(),
			});
		}
	}
	return hits;
}

function parseDockerComposeServices(yamlText) {
	const lines = yamlText.split(/\r?\n/);
	let inServices = false;
	const services = [];

	for (let i = 0; i < lines.length; i++) {
		const line = lines[i];

		if (!inServices) {
			if (/^services\s*:\s*$/.test(line.trim())) {
				inServices = true;
			}
			continue;
		}

		// Sai do bloco services quando encontra uma chave top-level (sem indent)
		if (/^[^\s#][^:]*:\s*$/.test(line) && !/^services\s*:\s*$/.test(line.trim())) {
			break;
		}

		const m = /^\s{2}([A-Za-z0-9_-]+)\s*:\s*$/.exec(line);
		if (m) {
			services.push({ name: m[1], line: i + 1, snippet: line.trim() });
		}
	}

	return services;
}

async function main() {
	const backendJavaRoot = path.join(REPO_ROOT, "backend", "src", "main", "java");
	const backendResRoot = path.join(REPO_ROOT, "backend", "src", "main", "resources");
	const flywayMigrationsRoot = path.join(backendResRoot, "db", "migration");
	const frontendPagesRoot = path.join(REPO_ROOT, "client", "src", "pages");
	const openapiRoot = path.join(REPO_ROOT, "openapi");

	const javaFiles = await walkFiles(backendJavaRoot, (p) => p.endsWith(".java"));

	const controllers = [];
	const services = [];
	const entities = [];
	const repositories = [];

	for (const filePath of javaFiles) {
		const text = await readText(filePath);
		if (!text) continue;

		const controller = parseControllerFile(filePath, text);
		if (controller) controllers.push(controller);

		for (const hit of extractAnnotatedClasses(text, "Service")) {
			services.push({
				className: hit.className,
				evidence: { file: rel(filePath), line: hit.line, snippet: hit.snippet },
			});
		}

		for (const hit of extractAnnotatedClasses(text, "Entity")) {
			entities.push({
				className: hit.className,
				evidence: { file: rel(filePath), line: hit.line, snippet: hit.snippet },
			});
		}

		// repositories por heurística
		for (const hit of extractExtends(text, /extends\s+JpaRepository\b|extends\s+CrudRepository\b/, "Repository")) {
			repositories.push({
				name: hit.name,
				evidence: { file: rel(filePath), line: hit.line, snippet: hit.snippet },
			});
		}
	}

	const migrationFiles = await walkFiles(flywayMigrationsRoot, (p) => /V\d+__.*\.sql$/i.test(path.basename(p)));
	migrationFiles.sort((a, b) => a.localeCompare(b));
	const flywayMigrations = migrationFiles.map((p) => {
		const base = path.basename(p);
		const m = /^V(\d+)__([^.]*)\.sql$/i.exec(base);
		return {
			version: m ? Number(m[1]) : null,
			name: m ? m[2] : base,
			file: rel(p),
		};
	});

	const backendConfigFiles = await walkFiles(backendResRoot, (p) => {
		const base = path.basename(p);
		return /application.*\.(yml|yaml|properties)$/i.test(base) || /prometheus-alerts\.yml$/i.test(base);
	});

	const pages = (await walkFiles(frontendPagesRoot, (p) => p.endsWith(".tsx")))
		.map((p) => ({ file: rel(p) }))
		.sort((a, b) => a.file.localeCompare(b.file));

	const openapiSpecs = (await walkFiles(openapiRoot, (p) => /\.(yml|yaml|json)$/i.test(p)))
		.map((p) => ({ file: rel(p) }))
		.sort((a, b) => a.file.localeCompare(b.file));

	const dockerComposePath = path.join(REPO_ROOT, "docker-compose.yml");
	const dockerComposeText = (await readText(dockerComposePath)) ?? "";
	const dockerComposeServices = dockerComposeText ? parseDockerComposeServices(dockerComposeText) : [];

	const dockerfiles = [];
	for (const name of ["Dockerfile", "Dockerfile.web", path.join("backend", "Dockerfile")]) {
		const full = path.join(REPO_ROOT, name);
		if (await pathExists(full)) dockerfiles.push({ file: rel(full) });
	}

	// Evidência de OpenAPI no backend (springdoc)
	const backendPomPath = path.join(REPO_ROOT, "backend", "pom.xml");
	const backendPom = (await readText(backendPomPath)) ?? "";
	const springdocIndex = backendPom.indexOf("springdoc-openapi");
	const springdocLine = springdocIndex >= 0 ? indexToLine(backendPom, springdocIndex) : null;

	const inventory = {
		generatedAt: new Date().toISOString(),
		repoRoot: path.basename(REPO_ROOT),
		backend: {
			controllers,
			services,
			entities,
			repositories,
			flywayMigrations,
			configFiles: backendConfigFiles.map((p) => rel(p)).sort(),
			openapi: {
				springdocDependency: springdocLine
					? {
							file: rel(backendPomPath),
							line: springdocLine,
						}
					: null,
			},
		},
		frontend: {
			pages,
		},
		infra: {
			dockerCompose: {
				file: rel(dockerComposePath),
				services: dockerComposeServices.map((s) => ({
					name: s.name,
					evidence: { file: rel(dockerComposePath), line: s.line, snippet: s.snippet },
				})),
			},
			dockerfiles,
		},
		openapi: {
			specs: openapiSpecs,
		},
	};

	const docsDir = path.join(REPO_ROOT, "docs");
	await fs.mkdir(docsDir, { recursive: true });

	const inventoryPath = path.join(docsDir, "DIAGRAMS_RULEX_INVENTORY.json");
	await fs.writeFile(inventoryPath, `${JSON.stringify(inventory, null, 2)}\n`, "utf8");

	const reportPath = path.join(docsDir, "DIAGRAMS_RULEX_REPORT.md");
	const controllerCount = controllers.length;
	const endpointCount = controllers.reduce((sum, c) => sum + c.endpoints.length, 0);

	const reportLines = [];
	reportLines.push("# Central de Diagramas — Solução RULEX (Relatório)\n");
	reportLines.push("Este relatório é gerado automaticamente a partir do repositório (PASSADA 1: inventário).\n");
	reportLines.push(`- Gerado em: ${inventory.generatedAt}`);
	reportLines.push(`- Fonte: \`${inventory.repoRoot}\``);
	reportLines.push(`- Inventário (JSON): \`docs/DIAGRAMS_RULEX_INVENTORY.json\`\n`);

	reportLines.push("## PASSADA 1 — Inventário (evidência do repositório)\n");

	reportLines.push("### Backend (Spring Boot)\n");
	reportLines.push(`- Controllers (@RestController/@Controller): ${controllerCount}`);
	reportLines.push(`- Endpoints (mapeamentos detectados): ${endpointCount}`);
	reportLines.push(`- Services (@Service): ${services.length}`);
	reportLines.push(`- Entities (@Entity): ${entities.length}`);
	reportLines.push(`- Repositories (extends JpaRepository/CrudRepository): ${repositories.length}`);
	reportLines.push(`- Flyway migrations: ${flywayMigrations.length}`);
	reportLines.push(`- Arquivos de config (resources): ${inventory.backend.configFiles.length}\n`);

	if (inventory.backend.openapi.springdocDependency) {
		reportLines.push("**OpenAPI (springdoc)**");
		reportLines.push(
			`- Dependência encontrada: ${inventory.backend.openapi.springdocDependency.file}#L${inventory.backend.openapi.springdocDependency.line}`,
		);
		reportLines.push("");
	}

	reportLines.push("**Controllers detectados (arquivo → basePath → endpoints)**");
	for (const c of controllers.sort((a, b) => a.evidence.file.localeCompare(b.evidence.file))) {
		reportLines.push(`- ${c.evidence.file}#L${c.evidence.line} — basePath=\`${c.basePath || "/"}\` — endpoints=${c.endpoints.length}`);
	}
	reportLines.push("");

	reportLines.push("### Banco de dados (Flyway)\n");
	reportLines.push("**Migrations (ordem lexical / versão)**");
	for (const m of flywayMigrations) {
		reportLines.push(`- ${m.file} (V${m.version ?? "?"}__${m.name})`);
	}
	reportLines.push("");

	reportLines.push("### Frontend (React)\n");
	reportLines.push(`- Pages (client/src/pages): ${pages.length}`);
	reportLines.push("**Pages detectadas**");
	for (const p of pages) reportLines.push(`- ${p.file}`);
	reportLines.push("");

	reportLines.push("### Infra/Execução\n");
	reportLines.push(`- docker-compose services: ${dockerComposeServices.length}`);
	reportLines.push("**docker-compose.yml → services**");
	for (const s of dockerComposeServices) {
		reportLines.push(`- ${s.name} (${rel(dockerComposePath)}#L${s.line})`);
	}
	reportLines.push("");

	reportLines.push("### OpenAPI Specs\n");
	reportLines.push(`- Specs em openapi/: ${openapiSpecs.length}`);
	for (const spec of openapiSpecs) reportLines.push(`- ${spec.file}`);
	reportLines.push("");

	reportLines.push("## Próximas passadas (não executadas nesta etapa)\n");
	reportLines.push("- PASSADA 2 (consistência cruzada): cruzar rotas frontend ↔ controllers ↔ OpenAPI ↔ Insomnia ↔ e2e.");
	reportLines.push("- PASSADA 3 (validação executável): rodar build/test e validar que os diagramas-instância gerados batem com a execução real.");
	reportLines.push("");

	reportLines.push("## Observações (anti-alucinação)\n");
	reportLines.push("- Este inventário é puramente estático (regex/heurísticas). Nenhum diagrama foi ‘inventado’ nesta fase.");
	reportLines.push("- Alguns endpoints podem ser contados duas vezes se houver anotações compostas; a PASSADA 2 normaliza.");

	await fs.writeFile(reportPath, `${reportLines.join("\n")}\n`, "utf8");

	console.log("✅ Inventário gerado:");
	console.log(`- ${rel(inventoryPath)}`);
	console.log(`- ${rel(reportPath)}`);
	console.log("\nResumo:");
	console.log(`- Controllers: ${controllerCount}`);
	console.log(`- Endpoints (heurístico): ${endpointCount}`);
	console.log(`- Services: ${services.length}`);
	console.log(`- Entities: ${entities.length}`);
	console.log(`- Repositories: ${repositories.length}`);
	console.log(`- Flyway migrations: ${flywayMigrations.length}`);
}

main().catch((err) => {
	console.error("❌ Falha ao gerar inventário:");
	console.error(err);
	process.exit(1);
});
