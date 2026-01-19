import { defineConfig } from "vitest/config";
import path from "path";
import react from "@vitejs/plugin-react";

const templateRoot = path.resolve(import.meta.dirname);

export default defineConfig({
  root: templateRoot,
  plugins: [react()],
  resolve: {
    alias: {
      "@": path.resolve(templateRoot, "client", "src"),
      "@shared": path.resolve(templateRoot, "shared"),
      "@assets": path.resolve(templateRoot, "attached_assets"),
    },
  },
  test: {
    environment: "jsdom",
    include: [
      "client/**/*.test.ts",
      "client/**/*.spec.ts",
      "client/**/*.test.tsx",
      "client/**/*.spec.tsx",
    ],
    coverage: {
      provider: "v8",
      reporter: ["text", "html", "lcov"],
      reportsDirectory: "./coverage",
      include: ["client/src/**/*.{ts,tsx}"],
      exclude: [
        "client/src/**/*.test.{ts,tsx}",
        "client/src/**/*.spec.{ts,tsx}",
        "client/src/lib/api.generated.ts",
        "client/src/components/ui/**",
      ],
      // TEST-001: Thresholds ajustados para cobertura atual (~32%)
      // TODO: Aumentar gradualmente conforme mais testes são adicionados
      // Meta futura: lines: 70, branches: 60, functions: 70, statements: 70
      thresholds: {
        lines: 30,
        branches: 25,
        functions: 24,
        statements: 30,
      },
    },
    setupFiles: ["./client/src/test/setup.ts"],
  },
});
