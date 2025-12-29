import { test, expect } from "@playwright/test";

test("login via Basic Auth and render app shell", async ({ page }) => {
  await page.goto("/login");

  await page.getByLabel("Usuário").fill(process.env.E2E_USERNAME || "admin");
  await page.getByLabel("Senha").fill(process.env.E2E_PASSWORD || "rulex");
  await page.getByRole("button", { name: "Entrar" }).click();

  await expect(page.getByRole("button", { name: "Transações" })).toBeVisible();
});

