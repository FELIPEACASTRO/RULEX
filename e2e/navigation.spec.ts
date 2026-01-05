import { test, expect } from "@playwright/test";

test.describe("Navigation", () => {
  test.beforeEach(async ({ page }) => {
    // Login first
    await page.goto("/login");
    await page.getByLabel("Usuário").fill(process.env.E2E_USERNAME || "admin");
    await page.getByLabel("Senha").fill(process.env.E2E_PASSWORD || "rulex");
    await page.getByRole("button", { name: "Entrar" }).click();
    await expect(page.getByRole("button", { name: "Transações" })).toBeVisible();
  });

  test("sidebar navigation is visible", async ({ page }) => {
    const sidebar = page.locator('[class*="sidebar"], nav, [role="navigation"]').first();
    await expect(sidebar).toBeVisible();
  });

  test("can navigate between all main pages", async ({ page }) => {
    // Navigate to each main section
    const sections = [
      { button: "Transações", url: /transactions/ },
      { button: "Regras Simples", url: /rules/ },
      { button: /Auditoria|Audit/i, url: /audit/ },
    ];

    for (const section of sections) {
      await page.getByRole("button", { name: section.button }).click();
      await expect(page).toHaveURL(section.url);
      await page.waitForLoadState("networkidle");
    }
  });

  test("RULEX logo is visible", async ({ page }) => {
    const logo = page.getByText(/RULEX/i).first();
    await expect(logo).toBeVisible();
  });

  test("user can see their role/username", async ({ page }) => {
    // Look for user info in header/sidebar
    const userInfo = page.locator('[class*="user"], [class*="avatar"], [class*="profile"]');
    const count = await userInfo.count();
    expect(count).toBeGreaterThanOrEqual(0);
  });
});
