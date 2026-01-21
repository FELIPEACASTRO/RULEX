import "@testing-library/jest-dom/vitest";

import React from "react";

import { render, screen, waitFor, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { describe, expect, it, vi } from "vitest";

import Manual from "./Manual";
import { MANUAL_STATS, OPERATORS, FIELD_LABELS } from "@/manual/manualData";

describe("Manual", () => {
  it("renderiza o titulo e estatisticas principais", () => {
    render(<Manual />);

    // Titulo principal
    expect(screen.getByRole("heading", { name: /manual do rulex/i })).toBeInTheDocument();

    // Estatisticas na descricao - texto pode aparecer em múltiplos lugares
    expect(screen.getAllByText(/operadores/i).length).toBeGreaterThan(0);
    expect(screen.getAllByText(/campos/i).length).toBeGreaterThan(0);
  });

  it("exibe a tab Visao Geral por padrao com estatisticas", () => {
    render(<Manual />);

    // Verifica que a tab Visao Geral está ativa (tabs existem)
    expect(screen.getByRole("tab", { name: /visão geral/i })).toBeInTheDocument();
    
    // Cards de estatisticas existem - usando getAllByText pois aparecem na tab e nos cards
    expect(screen.getAllByText(/operadores/i).length).toBeGreaterThan(0);
    expect(screen.getAllByText(/campos/i).length).toBeGreaterThan(0);
    expect(screen.getAllByText(/categorias/i).length).toBeGreaterThan(0);
    expect(screen.getAllByText(/templates/i).length).toBeGreaterThan(0);
  });

  it("navega para tab Operadores e exibe catalogo", async () => {
    const user = userEvent.setup();
    render(<Manual />);

    // Clicar na tab Operadores
    const operadoresTab = screen.getByRole("tab", { name: /operadores/i });
    await user.click(operadoresTab);

    // Deve mostrar o catalogo de operadores
    expect(screen.getByText(/catálogo de operadores/i)).toBeInTheDocument();
    // O número de operadores aparece em múltiplos lugares, então verificamos que existe
    expect(screen.getAllByText(String(OPERATORS.length)).length).toBeGreaterThan(0);
  });

  it(
    "navega para tab Payload e exibe dicionario de campos",
    async () => {
    const user = userEvent.setup();
    render(<Manual />);

    // Clicar na tab Payload
    const payloadTab = screen.getByRole("tab", { name: /payload/i });
    await user.click(payloadTab);

    // Deve mostrar o dicionario de campos
    expect(screen.getByText(/dicionário de campos do payload/i)).toBeInTheDocument();
    // Colunas enriquecidas
    expect(await screen.findByRole('columnheader', { name: /descrição/i })).toBeInTheDocument();
    expect(screen.getByRole('columnheader', { name: /valores esperados/i })).toBeInTheDocument();
    expect(screen.getByRole('columnheader', { name: /^exemplo$/i })).toBeInTheDocument();

    // Buscar e garantir que um campo conhecido aparece
    const fieldSearch = screen.getByPlaceholderText(/buscar por nome do campo ou label/i);
    await user.type(fieldSearch, "externalTransactionId");
    expect(screen.getByText(/externalTransactionId/i)).toBeInTheDocument();
    // O número de campos aparece em múltiplos lugares
    expect(screen.getAllByText(String(Object.keys(FIELD_LABELS).length)).length).toBeGreaterThan(0);
    },
    10000
  );

  it("navega para tab Exemplos e exibe templates", async () => {
    const user = userEvent.setup();
    render(<Manual />);

    // Clicar na tab Exemplos
    const exemplosTab = screen.getByRole("tab", { name: /exemplos/i });
    await user.click(exemplosTab);

    // Deve mostrar a galeria de templates
    expect(screen.getByText(/templates de regras/i)).toBeInTheDocument();
    expect(screen.getAllByText(String(MANUAL_STATS.totalTemplates)).length).toBeGreaterThan(0);

    // E também a biblioteca de regras (agora dentro de Exemplos)
    expect(screen.getByText(/biblioteca de regras de exemplo/i)).toBeInTheDocument();
  });

  it("navega para tab FAQ e exibe perguntas", async () => {
    const user = userEvent.setup();
    render(<Manual />);

    // Clicar na tab FAQ
    const faqTab = screen.getByRole("tab", { name: /faq/i });
    await user.click(faqTab);

    // Deve mostrar perguntas frequentes
    expect(screen.getByText(/perguntas frequentes/i)).toBeInTheDocument();
    expect(screen.getByText(/como criar uma regra simples/i)).toBeInTheDocument();
  });

  it("navega para tab Glossario e exibe termos", async () => {
    const user = userEvent.setup();
    render(<Manual />);

    // Clicar na tab Glossario
    const glossarioTab = screen.getByRole("tab", { name: /glossário/i });
    await user.click(glossarioTab);

    // Deve mostrar glossario
    expect(screen.getByText(/glossário de termos/i)).toBeInTheDocument();
    // Payload aparece na tab e no glossário, então usamos getAllByText
    expect(screen.getAllByText("Payload").length).toBeGreaterThan(0);
    expect(screen.getByText("MCC")).toBeInTheDocument();
  });

  it("busca global encontra operadores", async () => {
    const user = userEvent.setup();
    render(<Manual />);

    // Digitar na busca global
    const searchInput = screen.getByPlaceholderText(
      /buscar operadores, campos, templates, ações, funções, endpoints, exemplos/i
    );
    await user.clear(searchInput);
    await user.type(searchInput, "CONTAINS");

    // Deve mostrar resultados (pode haver múltiplos, então usamos getAllByText)
    const results = await screen.findAllByText("Operador");
    expect(results.length).toBeGreaterThan(0);
  });

  it("tem exatamente 17 abas principais (sem aba extra)", () => {
    render(<Manual />);
    const tablist = screen.getByRole("tablist");
    const tabs = within(tablist).getAllByRole("tab");
    expect(tabs).toHaveLength(17);

    // Confirma que a antiga aba top-level “Biblioteca” não existe
    expect(within(tablist).queryByRole("tab", { name: /biblioteca/i })).not.toBeInTheDocument();
  });

  it("busca global encontra ações, funções, endpoints e exemplos", async () => {
    const user = userEvent.setup();
    render(<Manual />);

    const searchInput = screen.getByPlaceholderText(
      /buscar operadores, campos, templates, ações, funções, endpoints, exemplos/i
    );

    await user.clear(searchInput);
    await user.type(searchInput, "SET_DECISION");
    expect(screen.getAllByText("Ação").length).toBeGreaterThan(0);

    await user.clear(searchInput);
    await user.type(searchInput, "ROUND");
    expect(screen.getAllByText("Função").length).toBeGreaterThan(0);

    await user.clear(searchInput);
    await user.type(searchInput, "metrics/timeline");
    expect(screen.getAllByText("Endpoint").length).toBeGreaterThan(0);

    await user.clear(searchInput);
    await user.type(searchInput, "S01");
    expect(screen.getAllByText("Exemplo").length).toBeGreaterThan(0);
  });

  it("inclui as tabs obrigatórias: Infra/Runbook e Regras Complexas", () => {
    render(<Manual />);
    expect(screen.getByRole("tab", { name: /infra\/runbook/i })).toBeInTheDocument();
    expect(screen.getByRole("tab", { name: /regras complexas/i })).toBeInTheDocument();
  });

  it(
    "busca global navega para Operadores e destaca o item por ~2s",
    async () => {
      const user = userEvent.setup();
    render(<Manual />);

    const searchInput = screen.getByPlaceholderText(
      /buscar operadores, campos, templates, ações, funções, endpoints, exemplos/i
    );
    await user.type(searchInput, "EQ");

    const eqValue = await screen.findByText(/^EQ$/);
    const result = eqValue.closest('[role="button"]');
    expect(result).toBeTruthy();
    await user.click(result as HTMLElement);

    await screen.findByText(/catálogo de operadores/i);

    await waitFor(
      () => {
        const row = document.getElementById("manual-operator-EQ");
        expect(row).toBeTruthy();
        expect(row?.className ?? "").toContain("ring-2");
      },
      { timeout: 2000 }
    );

    await new Promise((resolve) => setTimeout(resolve, 2300));

    const row = document.getElementById("manual-operator-EQ");
    expect(row).toBeTruthy();
    expect(row?.className ?? "").not.toContain("ring-2");
    },
    10000
  );

  it(
    "busca global navega para API e destaca endpoint por ~2s",
    async () => {
      const user = userEvent.setup();
      render(<Manual />);

      const searchInput = screen.getByPlaceholderText(
        /buscar operadores, campos, templates, ações, funções, endpoints, exemplos/i
      );

      await user.type(searchInput, "metrics/timeline");

      const valueEl = await screen.findByText(/^GET\s+\/api\/metrics\/timeline$/i);
      const result = valueEl.closest('[role="button"]');
      expect(result).toBeTruthy();
      await user.click(result as HTMLElement);

      await screen.findByText(/catálogo de api rest/i);

      const path = "/api/metrics/timeline";
      const slug = path
        .toLowerCase()
        .replace(/[^a-z0-9]+/g, "-")
        .replace(/^-+|-+$/g, "");
      const anchorId = `manual-endpoint-GET-${slug}`;

      await waitFor(
        () => {
          const row = document.getElementById(anchorId);
          expect(row).toBeTruthy();
          expect(row?.className ?? "").toContain("outline-2");
        },
        { timeout: 2000 }
      );

      await new Promise((resolve) => setTimeout(resolve, 2300));

      const row = document.getElementById(anchorId);
      expect(row).toBeTruthy();
      expect(row?.className ?? "").not.toContain("outline-2");
    },
    10000
  );

  it(
    "busca global navega para Exemplos e destaca regra por ~2s",
    async () => {
      const user = userEvent.setup();
      render(<Manual />);

      const searchInput = screen.getByPlaceholderText(
        /buscar operadores, campos, templates, ações, funções, endpoints, exemplos/i
      );

      await user.type(searchInput, "S01");

      const valueEl = await screen.findByText(/^S01$/);
      const result = valueEl.closest('[role="button"]');
      expect(result).toBeTruthy();
      await user.click(result as HTMLElement);

      await screen.findByText(/biblioteca de regras de exemplo/i);

      const anchorId = "manual-example-S01";

      await waitFor(
        () => {
          const card = document.getElementById(anchorId);
          expect(card).toBeTruthy();
          expect(card?.className ?? "").toContain("outline-2");
        },
        { timeout: 2000 }
      );

      await new Promise((resolve) => setTimeout(resolve, 2300));

      const card = document.getElementById(anchorId);
      expect(card).toBeTruthy();
      expect(card?.className ?? "").not.toContain("outline-2");
    },
    10000
  );
});

