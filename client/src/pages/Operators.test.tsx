import { describe, it, expect } from "vitest";
import { render, screen } from "@testing-library/react";
import Operators from "./Operators";

describe("Operators", () => {
  it("renders the operators title", () => {
    render(<Operators />);
    // Usa getAllByText porque pode haver múltiplos elementos com "Operadores"
    const elements = screen.getAllByText(/Operadores/i);
    expect(elements.length).toBeGreaterThan(0);
  });

  it("renders the operators list", () => {
    render(<Operators />);
    const content = document.body.textContent ?? "";
    expect(content).toMatch(/EQ/);
  });
});
