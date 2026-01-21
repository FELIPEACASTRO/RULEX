import "@testing-library/jest-dom/vitest";

import React from "react";

import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { cleanup, render, screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { afterEach, describe, expect, it, vi } from "vitest";

import RuleApprovals from "./RuleApprovals";

function renderWithQueryClient(ui: React.ReactElement) {
  const queryClient = new QueryClient({
    defaultOptions: { queries: { retry: false } },
  });

  return render(<QueryClientProvider client={queryClient}>{ui}</QueryClientProvider>);
}

function okJson(body: unknown) {
  return {
    ok: true,
    status: 200,
    headers: new Headers({ "content-type": "application/json" }),
    async json() {
      return body;
    },
    async text() {
      return JSON.stringify(body);
    },
  } as any;
}

function reqUrl(input: RequestInfo) {
  if (typeof input === "string") return input;
  if (input instanceof Request) return input.url;
  return String(input);
}

function reqMethod(input: RequestInfo, init?: RequestInit) {
  if (init?.method) return init.method;
  if (input instanceof Request) return input.method;
  return "GET";
}

afterEach(() => {
  cleanup();
  vi.unstubAllGlobals();
});

describe("RuleApprovals", () => {
  it("renders pending approvals and approves one", async () => {
    const user = userEvent.setup();
    const approvals = [
      {
        id: 10,
        ruleId: 99,
        ruleName: "HIGH_AMOUNT_RULE",
        actionType: "CREATE",
        requestedBy: "analyst",
        requestedAt: new Date().toISOString(),
        status: "PENDING",
      },
    ];

    const fetchMock = vi.fn(async (input: RequestInfo, init?: RequestInit) => {
      const url = reqUrl(input);
      const method = reqMethod(input, init).toUpperCase();

      if (url.includes("/api/rules/approvals/pending") && method === "GET") {
        return okJson(approvals);
      }

      if (url.includes("/api/rules/approvals/10/approve") && method === "POST") {
        return okJson({ approval: { ...approvals[0], status: "APPROVED" }, success: true, message: "ok" });
      }

      throw new Error(`Unexpected fetch: ${method} ${url}`);
    });

    vi.stubGlobal("fetch", fetchMock);

    renderWithQueryClient(<RuleApprovals />);

    await screen.findByText("HIGH_AMOUNT_RULE");

    await user.click(screen.getByRole("button", { name: "Aprovar" }));

    const approveButtons = await screen.findAllByRole("button", { name: "Aprovar" });
    await user.click(approveButtons[approveButtons.length - 1]);

    await waitFor(() => {
      expect(fetchMock).toHaveBeenCalled();
    });
  });
});
