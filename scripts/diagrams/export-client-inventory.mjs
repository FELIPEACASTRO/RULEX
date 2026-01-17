import fs from "node:fs";
import path from "node:path";

const repoRoot = path.resolve(import.meta.dirname, "..", "..");
const sourcePath = path.join(repoRoot, "docs", "DIAGRAMS_RULEX_INVENTORY.json");
const outPath = path.join(
  repoRoot,
  "client",
  "src",
  "features",
  "diagrams",
  "generated",
  "rulexInventory.ts"
);

function main() {
  if (!fs.existsSync(sourcePath)) {
    console.error(`Inventory file not found: ${sourcePath}`);
    process.exitCode = 1;
    return;
  }

  const raw = fs.readFileSync(sourcePath, "utf8");

  // Validate JSON early (fail fast)
  let parsed;
  try {
    parsed = JSON.parse(raw);
  } catch (e) {
    console.error(`Invalid JSON in ${sourcePath}`);
    console.error(e);
    process.exitCode = 1;
    return;
  }

  const banner = [
    "/*",
    " * AUTO-GENERATED FILE.",
    " * Source: docs/DIAGRAMS_RULEX_INVENTORY.json",
    " * Do not edit manually. Run: pnpm diagrams:sync-inventory",
    " */",
    "",
  ].join("\n");

  // Keep it as a typed constant for easy import in Vite + node scripts.
  const content =
    banner +
    `export const RULEX_INVENTORY = ${JSON.stringify(parsed, null, 2)} as const;\n` +
    `export default RULEX_INVENTORY;\n`;

  fs.writeFileSync(outPath, content, "utf8");
  console.log(`Wrote ${outPath}`);
}

main();
