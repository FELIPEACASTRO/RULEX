import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { render, screen } from "@testing-library/react";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import Transactions from "./Transactions";

// Mock wouter
vi.mock("wouter", () => ({
  useLocation: () => ["/transactions", vi.fn()],
  Link: ({ children, href }: { children: React.ReactNode; href: string }) => (
    <a href={href}>{children}</a>
  ),
}));

const mockFetch = vi.fn();

const createTestQueryClient = () =>
  new QueryClient({
    defaultOptions: {
      queries: {
        retry: false,
      },
    },
  });

const renderWithProviders = (ui: React.ReactElement) => {
  const queryClient = createTestQueryClient();
  return render(
    <QueryClientProvider client={queryClient}>{ui}</QueryClientProvider>
  );
};

describe("Transactions", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => ({ content: [], totalElements: 0 }),
    });
    vi.stubGlobal("fetch", mockFetch);
  });

  afterEach(() => {
    vi.unstubAllGlobals();
  });

  it("renders without crashing", () => {
    const { container } = renderWithProviders(<Transactions />);
    expect(container).toBeTruthy();
  });

  it("renders transactions content", () => {
    renderWithProviders(<Transactions />);
    // Should render something related to transactions
    const content = document.body.textContent;
    expect(content?.toLowerCase()).toMatch(/transaç|transaction/i);
  });

  it("has proper structure", () => {
    const { container } = renderWithProviders(<Transactions />);
    const elements = container.querySelectorAll("div");
    expect(elements.length).toBeGreaterThan(0);
  });
});
