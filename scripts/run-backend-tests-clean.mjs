import { spawnSync } from "node:child_process";
import { existsSync } from "node:fs";
import { cp, mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";

const scriptDir = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(scriptDir, "..");
const backendPath = path.join(repoRoot, "backend");
const mavenArgs = process.argv.slice(2);
const args = mavenArgs.length > 0 ? mavenArgs : ["test"];
const mvnCommand =
  process.env.MAVEN_CMD || (process.platform === "win32" ? "mvn.cmd" : "mvn");

if (!existsSync(backendPath)) {
  console.error("Backend folder not found:", backendPath);
  process.exit(1);
}

const tempRoot = await mkdtemp(path.join(tmpdir(), "rulex-backend-test-"));
const tempBackendPath = path.join(tempRoot, "backend");

async function cleanup() {
  try {
    await rm(tempRoot, { recursive: true, force: true });
  } catch (error) {
    console.warn("Failed to clean temp directory:", error);
  }
}

try {
  console.log("Copying backend to temp directory:", tempRoot);
  await cp(backendPath, tempBackendPath, { recursive: true, force: true });

  const tempTargetPath = path.join(tempBackendPath, "target");
  if (existsSync(tempTargetPath)) {
    console.log("Removing copied target directory to force clean compile");
    await rm(tempTargetPath, { recursive: true, force: true });
  }

  console.log(`Running Maven: ${mvnCommand} -q ${args.join(" ")}`);
  const result = spawnSync(mvnCommand, ["-q", ...args], {
    cwd: tempBackendPath,
    stdio: "inherit",
    shell: process.platform === "win32",
    env: {
      ...process.env,
      SPRING_PROFILES_ACTIVE: "test",
    },
  });

  if (typeof result.status === "number") {
    process.exitCode = result.status;
  } else if (result.error) {
    console.error("Failed to run Maven:", result.error);
    process.exitCode = 1;
  }
} finally {
  await cleanup();
}
