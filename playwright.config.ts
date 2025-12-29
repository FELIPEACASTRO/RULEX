import { defineConfig } from "@playwright/test";

export default defineConfig({
  testDir: "e2e",
  timeout: 60_000,
  expect: { timeout: 15_000 },
  retries: process.env.CI ? 2 : 0,
  use: {
    baseURL: process.env.PLAYWRIGHT_BASE_URL || "http://localhost:5173",
    trace: "retain-on-failure",
  },
  webServer: process.env.CI
    ? undefined
    : {
        command: "docker compose up --build",
        url: "http://localhost:5173",
        reuseExistingServer: true,
        timeout: 120_000,
      },
});

