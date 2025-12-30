import { test, expect } from "@playwright/test";

test.describe("Dashboard", () => {
  test.beforeEach(async ({ page }) => {
    // Login first
    await page.goto("/login");
    await page.getByLabel("Usuário").fill(process.env.E2E_USERNAME || "admin");
    await page.getByLabel("Senha").fill(process.env.E2E_PASSWORD || "rulex");
    await page.getByRole("button", { name: "Entrar" }).click();
    await expect(page.getByRole("button", { name: "Transações" })).toBeVisible();
  });

  test("navigates to dashboard", async ({ page }) => {
    await page.locator('[data-sidebar="menu-button"]', { hasText: /Dashboard/i }).click();
    // Dashboard may be at root (/) or /dashboard
    await expect(page).toHaveURL(/\/$|\/dashboard/);
  });

  test("displays statistics cards", async ({ page }) => {
    await page.locator('[data-sidebar="menu-button"]', { hasText: /Dashboard/i }).click();
    // Wait for dashboard to load
    await page.waitForLoadState("networkidle");
    // Should have some statistics visible
    const cards = page.locator('[class*="card"]');
    await expect(cards.first()).toBeVisible();
  });

  test("displays charts", async ({ page }) => {
    await page.locator('[data-sidebar="menu-button"]', { hasText: /Dashboard/i }).click();
    await page.waitForLoadState("networkidle");
    // Charts should be rendered
    const chartContainer = page.locator('[class*="chart"], svg, canvas').first();
    await expect(chartContainer).toBeVisible({ timeout: 10000 });
  });
});
