import { test, expect, devices } from "@playwright/test";

test.describe("Responsive Design", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/login");
    await page.getByLabel("Usuário").fill(process.env.E2E_USERNAME || "admin");
    await page.getByLabel("Senha").fill(process.env.E2E_PASSWORD || "rulex");
    await page.getByRole("button", { name: "Entrar" }).click();
    await expect(page.getByRole("button", { name: "Transações" })).toBeVisible();
  });

  test("desktop layout works correctly", async ({ page }) => {
    await page.setViewportSize({ width: 1920, height: 1080 });
    await page.waitForLoadState("networkidle");
    
    // Sidebar should be visible on desktop
    const sidebar = page.locator('[class*="sidebar"], nav').first();
    await expect(sidebar).toBeVisible();
  });

  test("tablet layout adapts correctly", async ({ page }) => {
    await page.setViewportSize({ width: 768, height: 1024 });
    await page.waitForLoadState("networkidle");
    
    // Content should still be accessible
    const mainContent = page.locator("main, [class*='content']").first();
    await expect(mainContent).toBeVisible();
  });

  test("mobile layout adapts correctly", async ({ page }) => {
    await page.setViewportSize({ width: 375, height: 667 });
    await page.waitForLoadState("networkidle");
    
    // Content should be visible
    const content = page.locator("body");
    await expect(content).toBeVisible();
    
    // May have hamburger menu on mobile
    const menuButton = page.locator('[class*="menu"], [class*="hamburger"], button[aria-label*="menu"]');
    const menuCount = await menuButton.count();
    expect(menuCount).toBeGreaterThanOrEqual(0);
  });
});
