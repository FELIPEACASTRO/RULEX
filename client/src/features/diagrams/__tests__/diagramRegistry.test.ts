import { describe, expect, it } from "vitest";

import { DIAGRAM_ITEMS, getLegacyDiagramCount } from "../registry/diagramRegistry";

describe("Diagram registry", () => {
  it("covers the full legacy catalog (anti-gap)", () => {
    expect(DIAGRAM_ITEMS.length).toBe(getLegacyDiagramCount());
  });

  it("has unique ids", () => {
    const ids = DIAGRAM_ITEMS.map((d) => d.id);
    expect(new Set(ids).size).toBe(ids.length);
  });

  it("has required metadata for each item", () => {
    for (const item of DIAGRAM_ITEMS) {
      expect(item.canonicalName).toBeTruthy();
      expect(item.categoryId).toBeTruthy();
      expect(item.categoryLabel).toBeTruthy();
      expect(item.formatsSupported.length).toBeGreaterThan(0);
      expect(item.sample).toBeTruthy();
    }
  });

  it("does not use the old generic placeholder sample", () => {
    const oldPlaceholder = /\n\s*A\[[^\]]+\]\s*-->\s*B\[Exemplo\]\s*$/m;
    for (const item of DIAGRAM_ITEMS) {
      if (item.sample.kind === "inline") {
        expect(item.sample.content).not.toMatch(oldPlaceholder);
      }
    }
  });

  it("uses sample formats coherent with renderer", () => {
    for (const item of DIAGRAM_ITEMS) {
      if (item.rendererId === "bpmn") {
        expect(item.sample.kind).toBe("inline");
        expect(item.sample.format).toBe("bpmn");
      }
      if (item.rendererId === "dfd" || item.rendererId === "matrix" || item.rendererId === "graph") {
        expect(item.sample.kind).toBe("json");
        expect(item.sample.format).toBe("json");
      }
    }
  });
});
