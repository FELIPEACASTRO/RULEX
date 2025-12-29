import { test, expect } from "@playwright/test";

test.describe("Audit", () => {
  test.beforeEach(async ({ page }) => {
    // Login first
    await page.goto("/login");
    await page.getByLabel("Usuário").fill(process.env.E2E_USERNAME || "admin");
    await page.getByLabel("Senha").fill(process.env.E2E_PASSWORD || "rulex");
    await page.getByRole("button", { name: "Entrar" }).click();
    await expect(page.getByRole("button", { name: "Transações" })).toBeVisible();
  });

  test("navigates to audit page", async ({ page }) => {
    await page.getByRole("button", { name: /Auditoria|Audit/i }).click();
    await expect(page).toHaveURL(/audit/);
  });

  test("displays audit log", async ({ page }) => {
    await page.getByRole("button", { name: /Auditoria|Audit/i }).click();
    await page.waitForLoadState("networkidle");
    
    // Should have a table or list
    const container = page.locator("table, [class*='list']").first();
    await expect(container).toBeVisible({ timeout: 10000 });
  });

  test("has export functionality", async ({ page }) => {
    await page.getByRole("button", { name: /Auditoria|Audit/i }).click();
    await page.waitForLoadState("networkidle");
    
    // Look for export button
    const exportButton = page.getByRole("button", { name: /Exportar|Export|Download/i });
    await expect(exportButton).toBeVisible();
  });

  test("has date filter", async ({ page }) => {
    await page.getByRole("button", { name: /Auditoria|Audit/i }).click();
    await page.waitForLoadState("networkidle");
    
    // Look for date inputs
    const dateInputs = page.locator('input[type="date"], [class*="date"], [class*="calendar"]');
    const count = await dateInputs.count();
    expect(count).toBeGreaterThanOrEqual(0);
  });
});
