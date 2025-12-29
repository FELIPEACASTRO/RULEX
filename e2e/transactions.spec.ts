import { test, expect } from "@playwright/test";

test.describe("Transactions", () => {
  test.beforeEach(async ({ page }) => {
    // Login first
    await page.goto("/login");
    await page.getByLabel("Usuário").fill(process.env.E2E_USERNAME || "admin");
    await page.getByLabel("Senha").fill(process.env.E2E_PASSWORD || "rulex");
    await page.getByRole("button", { name: "Entrar" }).click();
    await expect(page.getByRole("button", { name: "Transações" })).toBeVisible();
  });

  test("navigates to transactions page", async ({ page }) => {
    await page.getByRole("button", { name: "Transações" }).click();
    await expect(page).toHaveURL(/transactions/);
  });

  test("displays transactions table", async ({ page }) => {
    await page.getByRole("button", { name: "Transações" }).click();
    await page.waitForLoadState("networkidle");
    
    // Should have a table
    const table = page.locator("table").first();
    await expect(table).toBeVisible({ timeout: 10000 });
  });

  test("has pagination controls", async ({ page }) => {
    await page.getByRole("button", { name: "Transações" }).click();
    await page.waitForLoadState("networkidle");
    
    // Look for pagination
    const pagination = page.locator('[class*="pagination"], [aria-label*="pagination"]');
    const paginationButtons = page.getByRole("button", { name: /próximo|anterior|next|prev|1|2/i });
    const hasPagination = await pagination.isVisible() || await paginationButtons.first().isVisible();
    expect(hasPagination).toBeTruthy();
  });

  test("can filter transactions", async ({ page }) => {
    await page.getByRole("button", { name: "Transações" }).click();
    await page.waitForLoadState("networkidle");
    
    // Look for filter inputs
    const filterInputs = page.locator('input[type="text"], input[type="search"], select');
    const count = await filterInputs.count();
    expect(count).toBeGreaterThan(0);
  });
});
