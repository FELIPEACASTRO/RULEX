import { describe, it, expect, vi } from "vitest";
import { render } from "@testing-library/react";
import Home from "./Home";

// Mock wouter
vi.mock("wouter", () => ({
  useLocation: () => ["/", vi.fn()],
  Link: ({ children, href }: { children: React.ReactNode; href: string }) => (
    <a href={href}>{children}</a>
  ),
  Redirect: ({ to }: { to: string }) => <div data-testid="redirect" data-to={to} />,
}));

describe("Home", () => {
  it("renders without crashing", () => {
    const { container } = render(<Home />);
    expect(container).toBeTruthy();
  });

  it("has content or redirects", () => {
    const { container } = render(<Home />);
    // Either has content or redirect element
    const hasContent = container.textContent && container.textContent.length > 0;
    const hasRedirect = container.querySelector('[data-testid="redirect"]');
    expect(hasContent || hasRedirect).toBeTruthy();
  });
});
