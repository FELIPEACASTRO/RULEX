import * as React from "react";
import type { RendererId } from "../types";

export interface RendererDefinition {
  id: RendererId;
  label: string;
  supportsZoom: boolean;
  Component: React.LazyExoticComponent<React.ComponentType<any>>;
}

const MermaidRenderer = React.lazy(() => import("../renderers/MermaidRenderer"));
const BpmnRenderer = React.lazy(() => import("../renderers/BpmnRenderer"));
const DmnRenderer = React.lazy(() => import("../renderers/DmnRenderer"));
const DfdRenderer = React.lazy(() => import("../renderers/DfdRenderer"));
const MatrixRenderer = React.lazy(() => import("../renderers/MatrixRenderer"));
const ImageRenderer = React.lazy(() => import("../renderers/ImageRenderer"));
const PdfRenderer = React.lazy(() => import("../renderers/PdfRenderer"));
const GraphRenderer = React.lazy(() => import("../renderers/GraphRenderer"));
const PlantUmlRenderer = React.lazy(() => import("../renderers/PlantUmlRenderer"));
const FallbackRenderer = React.lazy(() => import("../renderers/FallbackRenderer"));

export const RENDERER_REGISTRY: Record<RendererId, RendererDefinition> = {
  mermaid: {
    id: "mermaid",
    label: "Mermaid",
    supportsZoom: true,
    Component: MermaidRenderer,
  },
  bpmn: {
    id: "bpmn",
    label: "BPMN",
    supportsZoom: false,
    Component: BpmnRenderer,
  },
  dmn: {
    id: "dmn",
    label: "DMN",
    supportsZoom: false,
    Component: DmnRenderer,
  },
  dfd: {
    id: "dfd",
    label: "DFD",
    supportsZoom: true,
    Component: DfdRenderer,
  },
  matrix: {
    id: "matrix",
    label: "Matrix",
    supportsZoom: false,
    Component: MatrixRenderer,
  },
  image: {
    id: "image",
    label: "Imagem",
    supportsZoom: true,
    Component: ImageRenderer,
  },
  pdf: {
    id: "pdf",
    label: "PDF",
    supportsZoom: false,
    Component: PdfRenderer,
  },
  graph: {
    id: "graph",
    label: "Grafo",
    supportsZoom: true,
    Component: GraphRenderer,
  },
  plantuml: {
    id: "plantuml",
    label: "PlantUML",
    supportsZoom: false,
    Component: PlantUmlRenderer,
  },
  fallback: {
    id: "fallback",
    label: "Fallback",
    supportsZoom: false,
    Component: FallbackRenderer,
  },
};

export function getRenderer(rendererId: RendererId): RendererDefinition {
  return RENDERER_REGISTRY[rendererId] ?? RENDERER_REGISTRY.fallback;
}
