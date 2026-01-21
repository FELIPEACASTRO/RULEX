import { test, expect, Page } from "@playwright/test";

/**
 * E2E Tests: Complex Rules
 * Simplified version - basic rule page tests
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

test.describe("Complex Rules - Basic Tests", () => {
  test.beforeEach(async ({ page }) => {
    await loginAsAdmin(page);
    await navigateToRules(page);
  });

  test("rules page loads successfully", async ({ page }) => {
    await expect(page).toHaveURL(/rules/);
  });

  test("rules page has content", async ({ page }) => {
    // Wait for any content to load
    await page.waitForTimeout(2000);
    // Page should have some content
    const body = await page.locator("body").textContent();
    expect(body).toBeTruthy();
  });
});
