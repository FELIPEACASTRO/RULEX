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
});
