import { describe, expect, it, vi } from "vitest";
import { render, screen } from "@testing-library/react";

// Avoid loading the real mermaid runtime in jsdom
vi.mock("mermaid", () => ({
  default: {
    initialize: vi.fn(),
    render: vi.fn().mockResolvedValue({ svg: "<svg></svg>" }),
  },
}));

import DiagramsHub from "../view/DiagramsHub";

describe("DiagramsHub", () => {
  it("renders the hub shell", () => {
    render(<DiagramsHub />);
    expect(
      screen.getByText("Central de Diagramas do RULEX")
    ).toBeInTheDocument();
  });

  it("renders list and item count", () => {
    render(<DiagramsHub />);
    expect(screen.getByText(/itens/i)).toBeInTheDocument();
    expect(screen.getByLabelText("Buscar diagramas")).toBeInTheDocument();
  });
});
