import { describe, it, expect, vi, beforeEach } from "vitest";
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
