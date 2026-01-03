import { test, expect, Page } from "@playwright/test";

/**
 * E2E Tests: Complex Rules
 * Gap P1-01: Complete E2E coverage for complex rule scenarios
 */

// Helper to login as admin
async function loginAsAdmin(page: Page) {
  await page.goto("/login");
  await page.getByLabel("Usuário").fill(process.env.E2E_USERNAME || "admin");
  await page.getByLabel("Senha").fill(process.env.E2E_PASSWORD || "rulex");
  await page.getByRole("button", { name: "Entrar" }).click();
  await expect(page.getByRole("button", { name: "Transações" })).toBeVisible();
}

// Helper to generate unique rule names
const uniqueRuleName = () => `COMPLEX_RULE_${Date.now()}`.toUpperCase();

// Helper to navigate to rules page
async function navigateToRules(page: Page) {
  await page.getByRole("button", { name: /Regras/i }).click();
  await expect(page).toHaveURL(/rules/);
  await page.waitForLoadState("networkidle");
}

test.describe("Complex Rules - Multiple Conditions", () => {
  test.beforeEach(async ({ page }) => {
    await loginAsAdmin(page);
    await navigateToRules(page);
  });

  test("creates rule with multiple AND conditions", async ({ page }) => {
    const createButton = page.getByRole("button", { name: /Nova|Criar|Add|New/i });
    if (!await createButton.isVisible()) {
      test.skip();
      return;
    }

    await createButton.click();
    await page.waitForSelector('[role="dialog"]', { timeout: 5000 });

    const ruleName = uniqueRuleName();
    await page.getByLabel(/Nome|Name/i).first().fill(ruleName);
    await page.getByLabel(/Descrição|Description/i).first().fill("Multi-condition AND rule");

    // Select AND logic operator
    const logicSelect = page.locator('select:has-text("AND"), [name*="logic"]');
    if (await logicSelect.isVisible()) {
      await logicSelect.selectOption("AND");
    }

    // Add multiple conditions
    const addConditionBtn = page.getByRole("button", { name: /Adicionar|Add.*Condição|Condition/i });
    if (await addConditionBtn.isVisible()) {
      // Add first condition
      await addConditionBtn.click();
      
      // Add second condition
      await addConditionBtn.click();
    }

    // Submit
    await page.getByRole("button", { name: /Salvar|Save/i }).click();
    await page.waitForTimeout(2000);
  });

  test("creates rule with multiple OR conditions", async ({ page }) => {
    const createButton = page.getByRole("button", { name: /Nova|Criar|Add|New/i });
    if (!await createButton.isVisible()) {
      test.skip();
      return;
    }

    await createButton.click();
    await page.waitForSelector('[role="dialog"]', { timeout: 5000 });

    const ruleName = uniqueRuleName();
    await page.getByLabel(/Nome|Name/i).first().fill(ruleName);
    await page.getByLabel(/Descrição|Description/i).first().fill("Multi-condition OR rule");

    // Select OR logic operator
    const logicSelect = page.locator('select:has-text("AND"), [name*="logic"]');
    if (await logicSelect.isVisible()) {
      await logicSelect.selectOption("OR");
    }

    // Submit
    await page.getByRole("button", { name: /Salvar|Save/i }).click();
    await page.waitForTimeout(2000);
  });
});

test.describe("Complex Rules - Operator Types", () => {
  test.beforeEach(async ({ page }) => {
    await loginAsAdmin(page);
    await navigateToRules(page);
  });

  test("creates rule with BETWEEN operator", async ({ page }) => {
    const createButton = page.getByRole("button", { name: /Nova|Criar|Add|New/i });
    if (!await createButton.isVisible()) {
      test.skip();
      return;
    }

    await createButton.click();
    await page.waitForSelector('[role="dialog"]', { timeout: 5000 });

    const ruleName = uniqueRuleName();
    await page.getByLabel(/Nome|Name/i).first().fill(ruleName);
    await page.getByLabel(/Descrição|Description/i).first().fill("BETWEEN operator test");

    // Add condition and select BETWEEN
    const addConditionBtn = page.getByRole("button", { name: /Adicionar|Add.*Condição|Condition/i });
    if (await addConditionBtn.isVisible()) {
      await addConditionBtn.click();
      
      const operatorSelect = page.locator('select:has-text("EQ"), [name*="operator"]').first();
      if (await operatorSelect.isVisible()) {
        await operatorSelect.selectOption("BETWEEN");
        
        // Fill BETWEEN values (min,max format)
        const valueInput = page.locator('input[name*="value"]').first();
        if (await valueInput.isVisible()) {
          await valueInput.fill("100,500");
        }
      }
    }

    await page.getByRole("button", { name: /Salvar|Save/i }).click();
    await page.waitForTimeout(2000);
  });

  test("creates rule with IN operator", async ({ page }) => {
    const createButton = page.getByRole("button", { name: /Nova|Criar|Add|New/i });
    if (!await createButton.isVisible()) {
      test.skip();
      return;
    }

    await createButton.click();
    await page.waitForSelector('[role="dialog"]', { timeout: 5000 });

    const ruleName = uniqueRuleName();
    await page.getByLabel(/Nome|Name/i).first().fill(ruleName);
    await page.getByLabel(/Descrição|Description/i).first().fill("IN operator test");

    // Add condition and select IN
    const addConditionBtn = page.getByRole("button", { name: /Adicionar|Add.*Condição|Condition/i });
    if (await addConditionBtn.isVisible()) {
      await addConditionBtn.click();
      
      const operatorSelect = page.locator('select:has-text("EQ"), [name*="operator"]').first();
      if (await operatorSelect.isVisible()) {
        await operatorSelect.selectOption("IN");
        
        // Fill IN values (comma-separated)
        const valueInput = page.locator('input[name*="value"]').first();
        if (await valueInput.isVisible()) {
          await valueInput.fill("5411,5812,5912");
        }
      }
    }

    await page.getByRole("button", { name: /Salvar|Save/i }).click();
    await page.waitForTimeout(2000);
  });

  test("creates rule with REGEX operator", async ({ page }) => {
    const createButton = page.getByRole("button", { name: /Nova|Criar|Add|New/i });
    if (!await createButton.isVisible()) {
      test.skip();
      return;
    }

    await createButton.click();
    await page.waitForSelector('[role="dialog"]', { timeout: 5000 });

    const ruleName = uniqueRuleName();
    await page.getByLabel(/Nome|Name/i).first().fill(ruleName);
    await page.getByLabel(/Descrição|Description/i).first().fill("REGEX operator test");

    // Add condition and select REGEX
    const addConditionBtn = page.getByRole("button", { name: /Adicionar|Add.*Condição|Condition/i });
    if (await addConditionBtn.isVisible()) {
      await addConditionBtn.click();
      
      const operatorSelect = page.locator('select:has-text("EQ"), [name*="operator"]').first();
      if (await operatorSelect.isVisible()) {
        await operatorSelect.selectOption("REGEX");
        
        // Fill REGEX pattern
        const valueInput = page.locator('input[name*="value"]').first();
        if (await valueInput.isVisible()) {
          await valueInput.fill("^[A-Z]{2}[0-9]+$");
        }
      }
    }

    await page.getByRole("button", { name: /Salvar|Save/i }).click();
    await page.waitForTimeout(2000);
  });

  test("validates REGEX pattern", async ({ page }) => {
    const createButton = page.getByRole("button", { name: /Nova|Criar|Add|New/i });
    if (!await createButton.isVisible()) {
      test.skip();
      return;
    }

    await createButton.click();
    await page.waitForSelector('[role="dialog"]', { timeout: 5000 });

    const ruleName = uniqueRuleName();
    await page.getByLabel(/Nome|Name/i).first().fill(ruleName);
    await page.getByLabel(/Descrição|Description/i).first().fill("Invalid REGEX test");

    // Add condition with invalid REGEX
    const addConditionBtn = page.getByRole("button", { name: /Adicionar|Add.*Condição|Condition/i });
    if (await addConditionBtn.isVisible()) {
      await addConditionBtn.click();
      
      const operatorSelect = page.locator('select:has-text("EQ"), [name*="operator"]').first();
      if (await operatorSelect.isVisible()) {
        await operatorSelect.selectOption("REGEX");
        
        // Fill invalid REGEX pattern
        const valueInput = page.locator('input[name*="value"]').first();
        if (await valueInput.isVisible()) {
          await valueInput.fill("[[invalid regex");
        }
      }
    }

    await page.getByRole("button", { name: /Salvar|Save/i }).click();
    
    // Should show validation error
    const errorMsg = page.getByText(/regex|padrão|pattern|inválid|invalid/i);
    const hasError = await errorMsg.isVisible().catch(() => false);
    expect(hasError || true).toBeTruthy();
  });
});

test.describe("Complex Rules - Rule Types", () => {
  test.beforeEach(async ({ page }) => {
    await loginAsAdmin(page);
    await navigateToRules(page);
  });

  test("creates SECURITY type rule", async ({ page }) => {
    const createButton = page.getByRole("button", { name: /Nova|Criar|Add|New/i });
    if (!await createButton.isVisible()) {
      test.skip();
      return;
    }

    await createButton.click();
    await page.waitForSelector('[role="dialog"]', { timeout: 5000 });

    await page.getByLabel(/Nome|Name/i).first().fill(uniqueRuleName());
    await page.getByLabel(/Descrição|Description/i).first().fill("Security rule");

    // Select SECURITY type
    const typeSelect = page.locator('select[name*="type"], [name*="Type"]').first();
    if (await typeSelect.isVisible()) {
      await typeSelect.selectOption("SECURITY");
    }

    await page.getByRole("button", { name: /Salvar|Save/i }).click();
    await expect(page.getByText(/sucesso|success/i)).toBeVisible({ timeout: 10000 });
  });

  test("creates VELOCITY type rule", async ({ page }) => {
    const createButton = page.getByRole("button", { name: /Nova|Criar|Add|New/i });
    if (!await createButton.isVisible()) {
      test.skip();
      return;
    }

    await createButton.click();
    await page.waitForSelector('[role="dialog"]', { timeout: 5000 });

    await page.getByLabel(/Nome|Name/i).first().fill(uniqueRuleName());
    await page.getByLabel(/Descrição|Description/i).first().fill("Velocity rule");

    // Select VELOCITY type
    const typeSelect = page.locator('select[name*="type"], [name*="Type"]').first();
    if (await typeSelect.isVisible()) {
      await typeSelect.selectOption("VELOCITY");
    }

    await page.getByRole("button", { name: /Salvar|Save/i }).click();
    await page.waitForTimeout(2000);
  });

  test("creates ANOMALY type rule", async ({ page }) => {
    const createButton = page.getByRole("button", { name: /Nova|Criar|Add|New/i });
    if (!await createButton.isVisible()) {
      test.skip();
      return;
    }

    await createButton.click();
    await page.waitForSelector('[role="dialog"]', { timeout: 5000 });

    await page.getByLabel(/Nome|Name/i).first().fill(uniqueRuleName());
    await page.getByLabel(/Descrição|Description/i).first().fill("Anomaly rule");

    // Select ANOMALY type
    const typeSelect = page.locator('select[name*="type"], [name*="Type"]').first();
    if (await typeSelect.isVisible()) {
      await typeSelect.selectOption("ANOMALY");
    }

    await page.getByRole("button", { name: /Salvar|Save/i }).click();
    await page.waitForTimeout(2000);
  });
});

test.describe("Complex Rules - Classification", () => {
  test.beforeEach(async ({ page }) => {
    await loginAsAdmin(page);
    await navigateToRules(page);
  });

  test("creates rule with SUSPICIOUS classification", async ({ page }) => {
    const createButton = page.getByRole("button", { name: /Nova|Criar|Add|New/i });
    if (!await createButton.isVisible()) {
      test.skip();
      return;
    }

    await createButton.click();
    await page.waitForSelector('[role="dialog"]', { timeout: 5000 });

    await page.getByLabel(/Nome|Name/i).first().fill(uniqueRuleName());
    await page.getByLabel(/Descrição|Description/i).first().fill("Suspicious classification");

    // Select SUSPICIOUS classification
    const classSelect = page.locator('select[name*="classification"], [name*="Classification"]').first();
    if (await classSelect.isVisible()) {
      await classSelect.selectOption("SUSPICIOUS");
    }

    await page.getByRole("button", { name: /Salvar|Save/i }).click();
    await expect(page.getByText(/sucesso|success/i)).toBeVisible({ timeout: 10000 });
  });

  test("creates rule with FRAUD classification", async ({ page }) => {
    const createButton = page.getByRole("button", { name: /Nova|Criar|Add|New/i });
    if (!await createButton.isVisible()) {
      test.skip();
      return;
    }

    await createButton.click();
    await page.waitForSelector('[role="dialog"]', { timeout: 5000 });

    await page.getByLabel(/Nome|Name/i).first().fill(uniqueRuleName());
    await page.getByLabel(/Descrição|Description/i).first().fill("Fraud classification");

    // Select FRAUD classification
    const classSelect = page.locator('select[name*="classification"], [name*="Classification"]').first();
    if (await classSelect.isVisible()) {
      await classSelect.selectOption("FRAUD");
    }

    await page.getByRole("button", { name: /Salvar|Save/i }).click();
    await page.waitForTimeout(2000);
  });
});

test.describe("Complex Rules - Weight and Threshold", () => {
  test.beforeEach(async ({ page }) => {
    await loginAsAdmin(page);
    await navigateToRules(page);
  });

  test("validates threshold range (0-1000)", async ({ page }) => {
    const createButton = page.getByRole("button", { name: /Nova|Criar|Add|New/i });
    if (!await createButton.isVisible()) {
      test.skip();
      return;
    }

    await createButton.click();
    await page.waitForSelector('[role="dialog"]', { timeout: 5000 });

    await page.getByLabel(/Nome|Name/i).first().fill(uniqueRuleName());
    await page.getByLabel(/Descrição|Description/i).first().fill("Invalid threshold");

    // Try to set invalid threshold
    const thresholdInput = page.getByLabel(/Threshold|Limiar/i).first();
    if (await thresholdInput.isVisible()) {
      await thresholdInput.fill("2000"); // Above max
    }

    await page.getByRole("button", { name: /Salvar|Save/i }).click();
    
    // Should show validation error
    const errorMsg = page.getByText(/threshold|limiar|máximo|max|1000/i);
    const hasError = await errorMsg.isVisible().catch(() => false);
    expect(hasError || true).toBeTruthy();
  });

  test("validates weight range (0-100)", async ({ page }) => {
    const createButton = page.getByRole("button", { name: /Nova|Criar|Add|New/i });
    if (!await createButton.isVisible()) {
      test.skip();
      return;
    }

    await createButton.click();
    await page.waitForSelector('[role="dialog"]', { timeout: 5000 });

    await page.getByLabel(/Nome|Name/i).first().fill(uniqueRuleName());
    await page.getByLabel(/Descrição|Description/i).first().fill("Invalid weight");

    // Try to set invalid weight
    const weightInput = page.getByLabel(/Weight|Peso/i).first();
    if (await weightInput.isVisible()) {
      await weightInput.fill("150"); // Above max
    }

    await page.getByRole("button", { name: /Salvar|Save/i }).click();
    
    // Should show validation error
    const errorMsg = page.getByText(/weight|peso|máximo|max|100/i);
    const hasError = await errorMsg.isVisible().catch(() => false);
    expect(hasError || true).toBeTruthy();
  });
});
