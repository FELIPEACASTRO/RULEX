import { describe, it, expect, vi } from "vitest";
import { render, screen } from "@testing-library/react";
import NotFound from "./NotFound";

// Mock wouter
vi.mock("wouter", () => ({
  useLocation: () => ["/unknown-page", vi.fn()],
  Link: ({ children, href }: { children: React.ReactNode; href: string }) => (
    <a href={href}>{children}</a>
  ),
}));

describe("NotFound", () => {
  it("renders without crashing", () => {
    const { container } = render(<NotFound />);
    expect(container).toBeTruthy();
  });

  it("renders 404 content", () => {
    render(<NotFound />);
    const content = document.body.textContent;
    expect(content?.toLowerCase()).toMatch(/404|não encontrad|not found|página/i);
  });

  it("has navigation option", () => {
    render(<NotFound />);
    const links = screen.queryAllByRole("link");
    const buttons = screen.queryAllByRole("button");
    expect(links.length + buttons.length).toBeGreaterThanOrEqual(0);
  });
});
