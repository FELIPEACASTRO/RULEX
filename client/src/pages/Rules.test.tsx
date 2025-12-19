import '@testing-library/jest-dom/vitest';

import React from 'react';

import { cleanup, render, screen, waitFor, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import Rules from './Rules';

function mockFetchSequence(handlers: Array<(input: RequestInfo, init?: RequestInit) => any>) {
  const fn = vi.fn(async (input: RequestInfo, init?: RequestInit) => {
    const handler = handlers.shift();
    if (!handler) throw new Error('Unexpected fetch call: ' + JSON.stringify({ input, init }));
    return handler(input, init);
  });
  vi.stubGlobal('fetch', fn);
  return fn;
}

function okJson(body: unknown) {
  return {
    ok: true,
    status: 200,
    async json() {
      return body;
    },
  } as any;
}

describe('Rules popup (Rules.tsx)', () => {
  beforeEach(() => {
    vi.unstubAllGlobals();
    vi.restoreAllMocks();
  });

  afterEach(() => {
    cleanup();
  });

  it('creates a rule via popup and posts all required fields', async () => {
    const user = userEvent.setup();
    const fetchMock = mockFetchSequence([
      // initial list
      (input) => {
        expect(String(input)).toContain('/api/rules?page=0&size=100');
        return okJson({ content: [] });
      },
      // create
      async (input, init) => {
        expect(String(input)).toBe('/api/rules');
        expect(init?.method).toBe('POST');
        const body = JSON.parse(String(init?.body ?? '{}'));
        expect(body).toMatchObject({
          ruleName: 'LOW_AUTHENTICATION_SCORE',
          description: 'Score baixo de autenticação',
          ruleType: 'SECURITY',
          threshold: 50,
          weight: 25,
          enabled: true,
          classification: 'SUSPICIOUS',
        });
        return okJson({ id: 1, ...body, version: 1 });
      },
      // refresh list after save
      (input) => {
        expect(String(input)).toContain('/api/rules?page=0&size=100');
        return okJson({ content: [] });
      },
    ]);

    render(<Rules />);

    await screen.findByText('Regras Configuradas');

    await user.click(screen.getByRole('button', { name: 'Nova Regra' }));
    const dialog = await screen.findByRole('dialog');
    expect(within(dialog).getByRole('heading', { name: 'Nova Regra' })).toBeInTheDocument();

    await user.type(
      screen.getByPlaceholderText('Ex: LOW_AUTHENTICATION_SCORE'),
      'LOW_AUTHENTICATION_SCORE',
    );
    await user.type(screen.getByPlaceholderText('Descrição da regra'), 'Score baixo de autenticação');

    // threshold + weight
    const thresholdInput = screen.getByLabelText('Threshold');
    await user.clear(thresholdInput);
    await user.type(thresholdInput, '50');

    const weightInput = screen.getByLabelText('Peso (0-100)');
    await user.clear(weightInput);
    await user.type(weightInput, '25');

    await user.click(screen.getByRole('button', { name: 'Criar' }));

    await waitFor(() => {
      expect(fetchMock).toHaveBeenCalled();
    });

    // dialog should close on success
    await waitFor(() => {
      expect(screen.queryByRole('dialog')).not.toBeInTheDocument();
    });
  });

  it('edits a rule via popup; ruleName is read-only; uses PUT', async () => {
    const user = userEvent.setup();
    const seededRule = {
      id: 10,
      ruleName: 'LOW_EXTERNAL_SCORE',
      description: 'Score externo baixo',
      ruleType: 'SECURITY',
      threshold: 10,
      weight: 10,
      enabled: true,
      classification: 'FRAUD',
      version: 1,
    };

    const fetchMock = mockFetchSequence([
      // initial list
      () => okJson({ content: [seededRule] }),
      // update
      async (input, init) => {
        expect(String(input)).toBe('/api/rules/10');
        expect(init?.method).toBe('PUT');
        const body = JSON.parse(String(init?.body ?? '{}'));
        expect(body.ruleName).toBe('LOW_EXTERNAL_SCORE');
        expect(body.description).toBe('Score externo muito baixo');
        return okJson({ ...seededRule, ...body, version: 2 });
      },
      // refresh list after save
      () => okJson({ content: [seededRule] }),
    ]);

    render(<Rules />);

    await screen.findByText('LOW_EXTERNAL_SCORE');

    await user.click(screen.getByTitle('Editar'));
    await screen.findByText('Editar Regra');

    const ruleNameInput = screen.getByPlaceholderText('Ex: LOW_AUTHENTICATION_SCORE') as HTMLInputElement;
    expect(ruleNameInput).toBeDisabled();
    expect(ruleNameInput.value).toBe('LOW_EXTERNAL_SCORE');

    const descInput = screen.getByPlaceholderText('Descrição da regra');
    await user.clear(descInput);
    await user.type(descInput, 'Score externo muito baixo');

    await user.click(screen.getByRole('button', { name: 'Atualizar' }));

    await waitFor(() => {
      expect(fetchMock).toHaveBeenCalled();
    });
  });

  it('toggles a rule enabled/disabled via PATCH', async () => {
    const user = userEvent.setup();
    const seededRule = {
      id: 1,
      ruleName: 'INVALID_CAVV',
      description: 'CAVV inválido',
      ruleType: 'SECURITY',
      threshold: 0,
      weight: 40,
      enabled: true,
      classification: 'FRAUD',
      version: 1,
    };

    const fetchMock = mockFetchSequence([
      // initial list
      () => okJson({ content: [seededRule] }),
      // toggle
      (input, init) => {
        expect(String(input)).toBe('/api/rules/1/toggle');
        expect(init?.method).toBe('PATCH');
        return okJson({ ...seededRule, enabled: false, version: 2 });
      },
      // refresh list
      () => okJson({ content: [{ ...seededRule, enabled: false, version: 2 }] }),
    ]);

    render(<Rules />);

    await screen.findByText('INVALID_CAVV');

    await user.click(screen.getByTitle('Desativar'));

    await waitFor(() => {
      expect(fetchMock).toHaveBeenCalled();
    });
  });

  it('snapshot: popup visual regression (modal content)', async () => {
    const user = userEvent.setup();
    mockFetchSequence([
      () => okJson({ content: [] }),
    ]);

    render(<Rules />);
    await screen.findByRole('heading', { name: 'Configuração de Regras' });

    await user.click(screen.getByRole('button', { name: 'Nova Regra' }));

    const dialog = await screen.findByRole('dialog');
    expect(dialog).toMatchSnapshot();
  });
});
