import fs from "node:fs";
import path from "node:path";

const repoRoot = path.resolve(process.cwd());
const sourcePath = path.join(repoRoot, "client/src/pages/Diagrams.tsx");
const outPath = path.join(
  repoRoot,
  "client/src/features/diagrams/registry/legacyCatalog.ts"
);

const src = fs.readFileSync(sourcePath, "utf8");

function fail(msg) {
  console.error(msg);
  process.exit(1);
}

// 1) Grab lucide-react import (needed for icon identifiers used in the catalog)
// Match specifically an import whose source is lucide-react.
const lucideImportMatch = src.match(/^import\s*\{[\s\S]*?\}\s*from\s*"lucide-react";?$/m);
if (!lucideImportMatch) fail("Could not find lucide-react import block in Diagrams.tsx");
const lucideImportBlock = lucideImportMatch[0];

// 2) Grab interfaces + DIAGRAM_CATEGORIES definition
// Be tolerant with CRLF and spacing.
const dataMarkerMatch = src.match(/\/\/\s*DADOS DOS DIAGRAMAS\s*\r?\n/);
if (!dataMarkerMatch || dataMarkerMatch.index == null) {
  fail("Could not find DADOS DOS DIAGRAMAS marker");
}
const dataStartIdx = dataMarkerMatch.index;

const categoriesDeclIdx = src.indexOf("const DIAGRAM_CATEGORIES", dataStartIdx);
if (categoriesDeclIdx === -1) fail("Could not find DIAGRAM_CATEGORIES declaration");

// find the end of the const array (the first occurrence of '];' after declaration)
const arrayEndIdx = src.indexOf("];", categoriesDeclIdx);
if (arrayEndIdx === -1) fail("Could not find end of DIAGRAM_CATEGORIES array");
const arrayEndInclusive = arrayEndIdx + "];".length;

const dataBlock = src.slice(dataStartIdx, arrayEndInclusive);

// Remove the marker line and keep everything after it
const markerLineEnd = dataBlock.search(/\r?\n/);
const dataBody = dataBlock.slice(markerLineEnd + 1).trimStart();

// Transform DIAGRAM_CATEGORIES -> LEGACY_DIAGRAM_CATEGORIES export
const transformed = dataBody.replace(
  /const\s+DIAGRAM_CATEGORIES\s*:\s*DiagramCategory\[\]\s*=\s*/, 
  "export const LEGACY_DIAGRAM_CATEGORIES: DiagramCategory[] = "
);

const out = `/**
 * legacyCatalog.ts
 *
 * Catálogo legado de tipos de diagramas do RULEX (fonte de verdade).
 * Extraído automaticamente de client/src/pages/Diagrams.tsx.
 */

import type React from "react";
${lucideImportBlock}

${transformed}
`;

fs.mkdirSync(path.dirname(outPath), { recursive: true });
fs.writeFileSync(outPath, out, "utf8");
console.log(`Wrote ${outPath}`);
