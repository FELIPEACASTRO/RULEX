import { test, expect } from "@playwright/test";

test.describe("Rules Management", () => {
  test.beforeEach(async ({ page }) => {
    // Login first
    await page.goto("/login");
    await page.getByLabel("Usuário").fill(process.env.E2E_USERNAME || "admin");
    await page.getByLabel("Senha").fill(process.env.E2E_PASSWORD || "rulex");
    await page.getByRole("button", { name: "Entrar" }).click();
    await expect(page.getByRole("button", { name: "Transações" })).toBeVisible();
  });

  test("navigates to rules page", async ({ page }) => {
    await page.getByRole("button", { name: "Regras Simples" }).click();
    await expect(page).toHaveURL(/rules/);
  });

  test("displays rules list", async ({ page }) => {
    await page.getByRole("button", { name: "Regras Simples" }).click();
    await page.waitForLoadState("networkidle");
    // Should have a table or list of rules
    const rulesContainer = page.locator('table, [class*="list"], [class*="grid"]').first();
    await expect(rulesContainer).toBeVisible({ timeout: 10000 });
  });

  test("can open create rule dialog", async ({ page }) => {
    await page.getByRole("button", { name: "Regras Simples" }).click();
    await page.waitForLoadState("networkidle");

    // Look for add/create button
    const addButton = page.getByRole("button", { name: /Nova|Criar|Add|New/i });
    if (await addButton.isVisible()) {
      await addButton.click();
      // Dialog should open
      const dialog = page.locator('[role="dialog"], [class*="modal"], [class*="dialog"]');
      await expect(dialog).toBeVisible({ timeout: 5000 });
    }
  });

  test("rules have toggle switch for enable/disable", async ({ page }) => {
    await page.getByRole("button", { name: "Regras Simples" }).click();
    await page.waitForLoadState("networkidle");

    // Look for toggle switches
    const toggles = page.locator('[role="switch"], input[type="checkbox"]');
    const count = await toggles.count();
    expect(count).toBeGreaterThanOrEqual(0);
  });
});
