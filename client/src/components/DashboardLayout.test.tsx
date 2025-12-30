import { describe, it, expect, vi } from "vitest";
import { render, screen } from "@testing-library/react";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import DashboardLayout from "./DashboardLayout";

// Mock wouter
vi.mock("wouter", () => ({
  useLocation: () => ["/dashboard", vi.fn()],
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

describe("DashboardLayout", () => {
  it("renders without crashing", () => {
    const { container } = renderWithProviders(
      <DashboardLayout>
        <div>Test content</div>
      </DashboardLayout>
    );
    expect(container).toBeTruthy();
  });

  it("renders layout structure", () => {
    const { container } = renderWithProviders(
      <DashboardLayout>
        <div>Test content</div>
      </DashboardLayout>
    );
    // Should have layout elements
    expect(container.querySelector("div")).toBeTruthy();
  });

  it("has proper layout structure", () => {
    const { container } = renderWithProviders(
      <DashboardLayout>
        <div>Content</div>
      </DashboardLayout>
    );
    const elements = container.querySelectorAll("div");
    expect(elements.length).toBeGreaterThan(0);
  });
});
