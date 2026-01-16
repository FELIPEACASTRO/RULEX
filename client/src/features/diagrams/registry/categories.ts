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
  dados_postgres: {
    id: "dados_postgres",
    label: "Dados (Postgres)",
    description: "Modelagem relacional, schemas, índices e replicação.",
  },
  dados_redis: {
    id: "dados_redis",
    label: "Dados (Redis)",
    description: "Cache, keyspaces, replicação e estratégias in-memory.",
  },
  dados_neo4j: {
    id: "dados_neo4j",
    label: "Dados (Neo4j)",
    description: "Grafos de propriedades, traversals e ontologias.",
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
  qualidade: {
    id: "qualidade",
    label: "Qualidade",
    description: "Testes, confiabilidade e engenharia de qualidade.",
  },
  cs_classicos: {
    id: "cs_classicos",
    label: "CS Clássicos",
    description: "Modelos formais e diagramas de ciência da computação.",
  },
};

export function getDiagramCategory(categoryId: DiagramCategoryId): DiagramCategory {
  return DIAGRAM_CATEGORIES[categoryId];
}
