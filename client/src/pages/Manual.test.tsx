import "@testing-library/jest-dom/vitest";

import React from "react";

import { render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { describe, expect, it } from "vitest";

import Manual from "./Manual";
import { MANUAL_DATA } from "@/manual/manualData";

describe("Manual", () => {
  it("renderiza o titulo e a aba de regras complexas", () => {
    render(<Manual />);

    expect(screen.getByRole("heading", { name: /manual/i })).toBeInTheDocument();
    expect(screen.getByText(/regras complexas/i)).toBeInTheDocument();

    // Default tab: regras complexas
    expect(screen.getAllByText(/operadores de comparacao/i).length).toBeGreaterThan(0);
    expect(
      screen.getAllByText(
        String(MANUAL_DATA.generatedFrom.complexRuleBuilder.comparisonOperators.length)
      ).length
    ).toBeGreaterThan(0);
  });

  it("permite filtrar operadores de comparacao (regras complexas)", async () => {
    const user = userEvent.setup();
    render(<Manual />);

    const input = screen.getByPlaceholderText(/buscar por codigo, label ou descricao/i);
    await user.type(input, "REGEX");

    // Deve aparecer pelo menos REGEX no grid filtrado
    expect(screen.getAllByText("REGEX").length).toBeGreaterThan(0);
  });

  it("exibe a aba de regras (formulario) e lista os operadores", async () => {
    const user = userEvent.setup();
    render(<Manual />);

    await user.click(screen.getByRole("tab", { name: /regras \(formulario\)/i }));

    expect(screen.getByText(/lista carregada de/i)).toBeInTheDocument();
    expect(
      screen.getByText(String(MANUAL_DATA.generatedFrom.ruleFormDialog.operators.length))
    ).toBeInTheDocument();

    // Deve ter pelo menos um operador conhecido
    expect(screen.getAllByText("EQ").length).toBeGreaterThan(0);
  });
});
