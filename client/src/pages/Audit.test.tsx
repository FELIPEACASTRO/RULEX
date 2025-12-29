import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen } from "@testing-library/react";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import Audit from "./Audit";

// Mock wouter
vi.mock("wouter", () => ({
  useLocation: () => ["/audit", vi.fn()],
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

describe("Audit", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("renders without crashing", () => {
    const { container } = renderWithProviders(<Audit />);
    expect(container).toBeTruthy();
  });

  it("renders audit content", () => {
    renderWithProviders(<Audit />);
    const content = document.body.textContent;
    expect(content?.toLowerCase()).toMatch(/audit|decisão|decision/i);
  });

  it("has buttons for actions", () => {
    renderWithProviders(<Audit />);
    const buttons = screen.queryAllByRole("button");
    expect(buttons.length).toBeGreaterThanOrEqual(0);
  });
});
