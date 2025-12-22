import '@testing-library/jest-dom/vitest';

import React from 'react';

import { cleanup, render, screen, waitFor, within } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import userEvent from '@testing-library/user-event';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import Rules from './Rules';

function renderWithQueryClient(ui: React.ReactElement) {
  const queryClient = new QueryClient({
    defaultOptions: {
      queries: {
        retry: false,
      },
    },
  });

  return render(<QueryClientProvider client={queryClient}>{ui}</QueryClientProvider>);
}

function mockFetchSequence(handlers: Array<(input: RequestInfo, init?: RequestInit) => any>) {
  const fn = vi.fn(async (input: RequestInfo, init?: RequestInit) => {
    const handler = handlers.shift();
    if (!handler) throw new Error('Unexpected fetch call: ' + JSON.stringify({ input, init }));
    return handler(input, init);
  });
  vi.stubGlobal('fetch', fn);
  return fn;
}

type FetchCall = { url: string; method: string; bodyText: string };

function mockRulesApi(initialContent: any[] = []) {
  let rules = [...initialContent];
  const calls: FetchCall[] = [];

  const fn = vi.fn(async (input: RequestInfo, init?: RequestInit) => {
    const url = reqUrl(input);
    const method = (reqMethod(input, init) ?? 'GET').toUpperCase();
    const bodyText = await reqBody(input, init);
    calls.push({ url, method, bodyText });

    // GET list
    if (url.includes('/api/rules') && method === 'GET') {
      return okJson(rules);
    }

    // Create
    if (url.includes('/api/rules') && method === 'POST') {
      const body = JSON.parse(bodyText || '{}');
      const maxId = rules.reduce((acc: number, r: any) => Math.max(acc, Number(r?.id ?? 0)), 0);
      const id = body.id ?? (maxId + 1);
      const created = { id, ...body, version: body.version ?? 1 };
      rules = [...rules, created];
      return okJson(created);
    }

    // Update
    if (url.includes('/api/rules/') && method === 'PUT') {
      const body = JSON.parse(bodyText || '{}');
      const idStr = url.split('/api/rules/')[1]?.split(/[?#/]/)[0];
      const id = Number(idStr);
      rules = rules.map((r: any) => (r.id === id ? { ...r, ...body, version: (r.version ?? 1) + 1 } : r));
      const updated = rules.find((r: any) => r.id === id);
      return okJson(updated ?? { id, ...body, version: 2 });
    }

    // Toggle
    if (url.includes('/toggle') && method === 'PATCH') {
      const idStr = url.split('/api/rules/')[1]?.split('/toggle')[0];
      const id = Number(idStr);
      const body = JSON.parse(bodyText || '{}');
      const enabled = typeof body.enabled === 'boolean' ? body.enabled : undefined;
      rules = rules.map((r: any) =>
        r.id === id
          ? { ...r, enabled: enabled ?? !r.enabled, version: (r.version ?? 1) + 1 }
          : r,
      );
      const toggled = rules.find((r: any) => r.id === id);
      return okJson(toggled ?? { id, enabled: false, version: 2 });
    }

    throw new Error('Unexpected fetch: ' + JSON.stringify({ url, method }));
  });

  vi.stubGlobal('fetch', fn);
  return { fetchMock: fn, calls, getRules: () => rules };
}

function okJson(body: unknown) {
  return {
    ok: true,
    status: 200,
    headers: new Headers({ 'content-type': 'application/json' }),
    async json() {
      return body;
    },
    async text() {
      return JSON.stringify(body);
    },
  } as any;
}

function reqUrl(input: RequestInfo) {
  if (typeof input === 'string') return input;
  if (input instanceof Request) return input.url;
  return String(input);
}

function reqMethod(input: RequestInfo, init?: RequestInit) {
  return init?.method ?? (input instanceof Request ? input.method : undefined);
}

async function reqBody(input: RequestInfo, init?: RequestInit) {
  if (init?.body != null) return String(init.body);
  if (input instanceof Request) return await input.text();
  return '';
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
    const api = mockRulesApi([]);

    renderWithQueryClient(<Rules />);

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

    await waitFor(() => expect(api.fetchMock).toHaveBeenCalled());

    const post = api.calls.find((c) => c.method === 'POST' && c.url.includes('/api/rules'));
    expect(post).toBeTruthy();
    const postedBody = JSON.parse(post?.bodyText || '{}');
    expect(postedBody).toMatchObject({
      ruleName: 'LOW_AUTHENTICATION_SCORE',
      description: 'Score baixo de autenticação',
      ruleType: 'SECURITY',
      threshold: 50,
      weight: 25,
      enabled: true,
      classification: 'SUSPICIOUS',
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

    const api = mockRulesApi([seededRule]);

    renderWithQueryClient(<Rules />);

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

    await waitFor(() => expect(api.fetchMock).toHaveBeenCalled());
    const put = api.calls.find((c) => c.method === 'PUT' && c.url.includes('/api/rules/10'));
    expect(put).toBeTruthy();
    const putBody = JSON.parse(put?.bodyText || '{}');
    expect(putBody.ruleName).toBe('LOW_EXTERNAL_SCORE');
    expect(putBody.description).toBe('Score externo muito baixo');
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

    const api = mockRulesApi([seededRule]);

    renderWithQueryClient(<Rules />);

    await screen.findByText('INVALID_CAVV');

    await user.click(screen.getByTitle('Desativar'));

    await waitFor(() => expect(api.fetchMock).toHaveBeenCalled());
    const patch = api.calls.find((c) => c.method === 'PATCH' && c.url.includes('/api/rules/1/toggle'));
    expect(patch).toBeTruthy();
  });

  it('snapshot: popup visual regression (modal content)', async () => {
    const user = userEvent.setup();
    mockRulesApi([]);

    renderWithQueryClient(<Rules />);
    await screen.findByRole('heading', { name: 'Configuração de Regras' });

    await user.click(screen.getByRole('button', { name: 'Nova Regra' }));

    const dialog = await screen.findByRole('dialog');
    expect(dialog).toMatchSnapshot();
  });
});
