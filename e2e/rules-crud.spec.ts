import { test, expect, Page } from "@playwright/test";

/**
 * E2E Tests: Rules CRUD
 * Simplified version - basic tests only
 */

// Helper to login as admin
async function loginAsAdmin(page: Page) {
  await page.goto("/login");
  await page.getByLabel("Usuário").fill(process.env.E2E_USERNAME || "admin");
  await page.getByLabel("Senha").fill(process.env.E2E_PASSWORD || "rulex");
  await page.getByRole("button", { name: "Entrar" }).click();
  await expect(page.getByRole("button", { name: "Transações" })).toBeVisible({ timeout: 15000 });
}

// Helper to navigate to rules page
async function navigateToRules(page: Page) {
  await page.getByRole("button", { name: "Regras Complexas" }).click();
  await expect(page).toHaveURL(/rules/);
  await page.waitForLoadState("networkidle");
}

test.describe("Rules CRUD - Basic Tests", () => {
  test.beforeEach(async ({ page }) => {
    await loginAsAdmin(page);
    await navigateToRules(page);
  });

  test("rules page loads", async ({ page }) => {
    await expect(page).toHaveURL(/rules/);
  });
});
