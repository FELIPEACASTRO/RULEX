import { test, expect, Page } from "@playwright/test";

/**
 * E2E Tests: RBAC (Role-Based Access Control)
 * Simplified version - tests basic authentication
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
  await page.getByRole("button", { name: "Regras de Fraude" }).click();
  await expect(page).toHaveURL(/rules/);
  await page.waitForLoadState("networkidle");
}

test.describe("RBAC - Admin Role", () => {
  test.beforeEach(async ({ page }) => {
    await loginAsAdmin(page);
  });

  test("admin can view rules list", async ({ page }) => {
    await navigateToRules(page);
    // Wait for page to load
    await page.waitForLoadState("networkidle");
    // Check that we're on the rules page
    await expect(page).toHaveURL(/rules/);
  });

  test("admin can access audit page", async ({ page }) => {
    await page.getByRole("button", { name: /Auditoria|Audit/i }).click();
    await expect(page).toHaveURL(/audit/);
  });

  test("admin can access transactions page", async ({ page }) => {
    await page.getByRole("button", { name: "Transações" }).click();
    await expect(page).toHaveURL(/transactions/);
  });
});
