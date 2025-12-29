import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import Login from "./Login";

// Mock wouter
const mockNavigate = vi.fn();
vi.mock("wouter", () => ({
  useLocation: () => ["/login", mockNavigate],
}));

describe("Login", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("renders without crashing", () => {
    const { container } = render(<Login />);
    expect(container).toBeTruthy();
  });

  it("renders login page content", () => {
    const { container } = render(<Login />);
    // Should have some content
    expect(container.textContent).toBeTruthy();
  });

  it("renders form inputs", () => {
    render(<Login />);
    const inputs = screen.getAllByRole("textbox");
    expect(inputs.length).toBeGreaterThanOrEqual(1);
  });

  it("renders login button", () => {
    render(<Login />);
    const buttons = screen.getAllByRole("button");
    expect(buttons.length).toBeGreaterThan(0);
  });

  it("has password input", () => {
    const { container } = render(<Login />);
    const passwordInput = container.querySelector('input[type="password"]');
    expect(passwordInput).toBeTruthy();
  });
});
