import { describe, it, expect } from "vitest";
import { render, screen } from "@testing-library/react";
import Operators from "./Operators";

describe("Operators", () => {
  it("renders the operators title", () => {
    render(<Operators />);
    expect(screen.getByText(/Operadores/i)).toBeTruthy();
  });

  it("renders the operators list", () => {
    render(<Operators />);
    const content = document.body.textContent ?? "";
    expect(content).toMatch(/EQ/);
  });
});
