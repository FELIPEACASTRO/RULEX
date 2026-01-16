import type { DiagramCategory, DiagramCategoryId } from "../types";

export const DIAGRAM_CATEGORIES: Record<DiagramCategoryId, DiagramCategory> = {
  processos: {
    id: "processos",
    label: "Processos",
    description: "Fluxos operacionais e jornadas ponta-a-ponta.",
  },
  uml_estrutural: {
    id: "uml_estrutural",
    label: "UML Estrutural",
    description: "Estrutura do sistema: classes, componentes, pacotes.",
  },
  uml_comportamental: {
    id: "uml_comportamental",
    label: "UML Comportamental",
    description: "Comportamento: sequência, estados, atividades, casos de uso.",
  },
  arquitetura: {
    id: "arquitetura",
    label: "Arquitetura",
    description: "Visões arquiteturais (C4, deployment, infraestrutura).",
  },
  ddd: {
    id: "ddd",
    label: "DDD",
    description: "Domínio: bounded contexts, agregados e contexto.",
  },
  api: {
    id: "api",
    label: "API",
    description: "Fluxos e contratos de integrações e endpoints.",
  },
  dados: {
    id: "dados",
    label: "Dados",
    description: "Modelagem e estruturas de dados (ER, tabelas, schemas).",
  },
  frontend: {
    id: "frontend",
    label: "Frontend",
    description: "Arquitetura e fluxos do cliente web.",
  },
  infra: {
    id: "infra",
    label: "Infra",
    description: "Infraestrutura, ambientes e runtime.",
  },
  seguranca: {
    id: "seguranca",
    label: "Segurança",
    description: "Ameaças, controles e fluxos de autenticação/autorização.",
  },
};

export function getDiagramCategory(categoryId: DiagramCategoryId): DiagramCategory {
  return DIAGRAM_CATEGORIES[categoryId];
}
