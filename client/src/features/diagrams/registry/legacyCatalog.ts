/**
 * legacyCatalog.ts
 *
 * Catálogo legado de tipos de diagramas do RULEX (fonte de verdade).
 * Extraído automaticamente de client/src/pages/Diagrams.tsx.
 */

import type React from "react";
import {
  GitBranch,
  Workflow,
  Database,
  Server,
  Layout,
  Shield,
  Network,
  Box,
  Layers,
  FileJson,
  Search,
  ChevronRight,
  ArrowRight,
  ArrowDown,
  CheckCircle2,
  CircleDot,
  Square,
  Diamond,
  Hexagon,
  Triangle,
  Circle,
  Activity,
  Zap,
  Lock,
  Eye,
  BarChart3,
  Code2,
  Cpu,
  Cloud,
  Container,
  Settings2,
  Users,
  FileText,
  FolderTree,
  BookOpen,
} from "lucide-react";

// ============================================================

export interface DiagramType {
  name: string;
  nameEn: string;
  description: string;
  useCase: string;
  tools?: string[];
  example?: string;
}

export interface DiagramCategory {
  id: string;
  title: string;
  icon: React.ElementType;
  color: string;
  description: string;
  subcategories?: {
    name: string;
    diagrams: DiagramType[];
  }[];
  diagrams?: DiagramType[];
}

export const LEGACY_DIAGRAM_CATEGORIES: DiagramCategory[] = [
  {
    id: "process",
    title: "Fluxogramas e Processos",
    icon: Workflow,
    color: "blue",
    description: "Diagramas para modelar fluxos de trabalho, processos de negócio e tomada de decisão.",
    diagrams: [
      {
        name: "Fluxograma",
        nameEn: "Flowchart",
        description: "Representação gráfica de um processo usando símbolos padronizados (início, fim, decisão, processo).",
        useCase: "Documentar o fluxo de análise de transação no RULEX.",
        tools: ["draw.io", "Lucidchart", "Mermaid"],
        example: `┌─────────┐
│  Início │
└────┬────┘
     ▼
┌─────────────┐
│ Recebe Txn  │
└─────┬───────┘
      ▼
  ◇ Valor > 10k?
 /           \\
Sim          Não
 │            │
 ▼            ▼
┌────────┐  ┌────────┐
│ Review │  │ Aprovar│
└────────┘  └────────┘`,
      },
      {
        name: "Fluxograma Funcional",
        nameEn: "Cross-Functional Flowchart",
        description: "Fluxograma que mostra a responsabilidade de cada área/departamento no processo.",
        useCase: "Mapear handoffs entre Prevenção, Compliance e Operações.",
        tools: ["Visio", "Lucidchart"],
      },
      {
        name: "Diagrama com Raias",
        nameEn: "Swimlane Diagram",
        description: "Organiza atividades em raias horizontais ou verticais por ator/sistema.",
        useCase: "Visualizar quem faz o quê no fluxo de detecção de fraude.",
        tools: ["draw.io", "Miro", "Figma"],
        example: `┌─────────────┬─────────────┬─────────────┐
│   Frontend  │   Backend   │   Database  │
├─────────────┼─────────────┼─────────────┤
│ [Submete]   │             │             │
│     │       │             │             │
│     └──────►│ [Valida]    │             │
│             │     │       │             │
│             │     └──────►│ [Persiste]  │
│             │             │     │       │
│             │◄────────────┘     │       │
│◄────────────┘                   │       │
└─────────────┴─────────────┴─────────────┘`,
      },
      {
        name: "Fluxo de Trabalho",
        nameEn: "Workflow Diagram",
        description: "Sequência de tarefas e decisões que compõem um processo de trabalho.",
        useCase: "Automatizar aprovações de regras no RULEX.",
        tools: ["Miro", "Monday", "Jira"],
      },
      {
        name: "BPMN - Processo",
        nameEn: "Business Process Model and Notation",
        description: "Notação padrão ISO para modelagem de processos de negócio com eventos, gateways e atividades.",
        useCase: "Documentar o processo completo de análise de fraude conforme padrões de mercado.",
        tools: ["Camunda Modeler", "Bizagi", "draw.io"],
      },
      {
        name: "BPMN - Colaboração",
        nameEn: "BPMN Collaboration Diagram",
        description: "Mostra a interação entre múltiplos participantes/pools em um processo.",
        useCase: "Mapear integração entre RULEX, gateway de pagamento e banco emissor.",
        tools: ["Camunda Modeler", "Bizagi"],
      },
      {
        name: "BPMN - Coreografia",
        nameEn: "BPMN Choreography Diagram",
        description: "Foca na troca de mensagens entre participantes sem mostrar processos internos.",
        useCase: "Documentar protocolo de comunicação com sistemas externos.",
        tools: ["Camunda Modeler"],
      },
      {
        name: "Diagrama de Estados do Processo",
        nameEn: "Process State Diagram",
        description: "Estados pelos quais uma entidade passa durante o processo.",
        useCase: "Ciclo de vida de uma transação: PENDING → ANALYZING → APPROVED/BLOCKED.",
        tools: ["PlantUML", "Mermaid"],
        example: `[*] --> Pending
Pending --> Analyzing : receive
Analyzing --> Approved : score < 50
Analyzing --> Suspicious : score 50-80
Analyzing --> Blocked : score > 80
Approved --> [*]
Suspicious --> Review
Review --> Approved
Review --> Blocked
Blocked --> [*]`,
      },
      {
        name: "Árvore de Decisão",
        nameEn: "Decision Tree",
        description: "Estrutura hierárquica de decisões e suas consequências.",
        useCase: "Lógica de classificação de risco baseada em múltiplos critérios.",
        tools: ["draw.io", "Miro"],
        example: `              ┌─────────────┐
              │ Valor > 5k? │
              └──────┬──────┘
            Sim/     \\Não
           ┌────┐    ┌────┐
           │País│    │Hora│
           │BR? │    │noite│
           └─┬──┘    └──┬─┘
          Não│Sim    Sim│Não
           ▼  ▼       ▼  ▼
         [H] [M]    [M] [L]`,
      },
      {
        name: "Tabela de Decisão",
        nameEn: "Decision Table",
        description: "Matriz que mapeia combinações de condições para ações/resultados.",
        useCase: "Documentar regras de negócio complexas do RULEX de forma tabular.",
        tools: ["Excel", "Confluence", "DMN"],
      },
      {
        name: "SIPOC",
        nameEn: "Supplier–Input–Process–Output–Customer",
        description: "Visão de alto nível: fornecedores, entradas, processo, saídas e clientes.",
        useCase: "Definir escopo do motor de regras para stakeholders não-técnicos.",
        tools: ["Miro", "Lucidchart"],
      },
      {
        name: "Mapa de Fluxo de Valor",
        nameEn: "Value Stream Map (VSM)",
        description: "Visualiza fluxo de valor e identifica desperdícios no processo.",
        useCase: "Otimizar tempo de resposta do endpoint /analyze.",
        tools: ["Lucidchart", "Miro"],
      },
      {
        name: "Jornada do Usuário",
        nameEn: "User Journey Map",
        description: "Experiência do usuário através de touchpoints e emoções.",
        useCase: "Mapear experiência do analista de fraude usando o RULEX.",
        tools: ["Figma", "Miro", "UXPressia"],
      },
      {
        name: "Service Blueprint",
        nameEn: "Service Blueprint",
        description: "Jornada do usuário + processos de backstage e infraestrutura.",
        useCase: "Visão completa: UI → API → Serviços → DB → Logs.",
        tools: ["Miro", "Figma"],
      },
    ],
  },
  {
    id: "uml",
    title: "UML",
    icon: Box,
    color: "purple",
    description: "Diagramas UML para modelagem orientada a objetos (estruturais e comportamentais).",
    subcategories: [
      {
        name: "Estruturais",
        diagrams: [
          {
            name: "Diagrama de Classes",
            nameEn: "Class Diagram",
            description: "Classes, atributos, métodos e relacionamentos (herança, composição, agregação).",
            useCase: "Modelar entidades do RULEX: Rule, Condition, Transaction, Decision.",
            tools: ["PlantUML", "Enterprise Architect", "IntelliJ"],
            example: `┌─────────────────────┐
│      <<entity>>     │
│        Rule         │
├─────────────────────┤
│ - id: Long          │
│ - name: String      │
│ - enabled: boolean  │
│ - conditions: List  │
├─────────────────────┤
│ + evaluate(txn)     │
│ + getScore(): int   │
└─────────────────────┘
          △
          │
┌─────────────────────┐
│    ComplexRule      │
├─────────────────────┤
│ - expression: String│
└─────────────────────┘`,
          },
          {
            name: "Diagrama de Objetos",
            nameEn: "Object Diagram",
            description: "Instâncias de classes em um momento específico.",
            useCase: "Snapshot de uma regra e suas condições em runtime.",
            tools: ["PlantUML", "Enterprise Architect"],
          },
          {
            name: "Diagrama de Componentes",
            nameEn: "Component Diagram",
            description: "Componentes do sistema e suas dependências.",
            useCase: "Visualizar módulos do backend: Controller, Service, Repository.",
            tools: ["PlantUML", "draw.io"],
            example: `┌─────────────────────────────────────┐
│           <<component>>             │
│          RuleService                │
│  ┌───────────┐  ┌────────────────┐  │
│  │Evaluation │  │  Persistence   │  │
│  │  Engine   │  │    Layer       │  │
│  └─────┬─────┘  └───────┬────────┘  │
│        │                │           │
└────────┼────────────────┼───────────┘
         │                │
         ▼                ▼
   [Repository]     [Database]`,
          },
          {
            name: "Diagrama de Pacotes",
            nameEn: "Package Diagram",
            description: "Organização de classes/componentes em pacotes.",
            useCase: "Estrutura do backend: com.rulex.controller, .service, .entity.",
            tools: ["PlantUML", "IntelliJ"],
          },
          {
            name: "Diagrama de Implantação",
            nameEn: "Deployment Diagram",
            description: "Distribuição física de artefatos em nós (servidores, containers).",
            useCase: "Deploy do RULEX: Frontend (CDN), Backend (K8s), DB (RDS).",
            tools: ["PlantUML", "draw.io"],
          },
          {
            name: "Diagrama de Estrutura Composta",
            nameEn: "Composite Structure Diagram",
            description: "Estrutura interna de um classificador e colaborações.",
            useCase: "Detalhamento interno do ExpressionEvaluator.",
            tools: ["Enterprise Architect"],
          },
          {
            name: "Diagrama de Perfil",
            nameEn: "Profile Diagram",
            description: "Extensões e estereótipos customizados para UML.",
            useCase: "Definir estereótipos para domínio de fraude.",
            tools: ["Enterprise Architect"],
          },
        ],
      },
      {
        name: "Comportamentais / Interação",
        diagrams: [
          {
            name: "Diagrama de Casos de Uso",
            nameEn: "Use Case Diagram",
            description: "Atores e casos de uso do sistema.",
            useCase: "Funcionalidades do RULEX: Criar Regra, Analisar Transação, Gerar Relatório.",
            tools: ["PlantUML", "draw.io"],
            example: `     ┌─────────────────────────────┐
     │         RULEX              │
     │  ┌─────────────────────┐   │
     │  │   Criar Regra       │   │
     │  └─────────────────────┘   │
     │  ┌─────────────────────┐   │
○────┼──│  Analisar Transação │   │
Admin│  └─────────────────────┘   │
     │  ┌─────────────────────┐   │
     │  │   Ver Dashboard     │───┼──○
     │  └─────────────────────┘   │ Analyst
     └─────────────────────────────┘`,
          },
          {
            name: "Diagrama de Sequência",
            nameEn: "Sequence Diagram",
            description: "Interação entre objetos ao longo do tempo.",
            useCase: "Fluxo de análise: Controller → Service → Repository → DB.",
            tools: ["PlantUML", "Mermaid", "SequenceDiagram.org"],
            example: `┌──────┐     ┌─────────┐     ┌──────┐     ┌────┐
│Client│     │Controller│     │Service│     │ DB │
└──┬───┘     └────┬────┘     └───┬───┘     └─┬──┘
   │   POST /analyze    │           │          │
   │──────────────────►│           │          │
   │              │    │ evaluate()│          │
   │              │    │──────────►│          │
   │              │    │           │ loadRules│
   │              │    │           │─────────►│
   │              │    │           │◄─────────│
   │              │    │◄──────────│          │
   │◄──────────────────│           │          │
   │    Decision       │           │          │`,
          },
          {
            name: "Diagrama de Comunicação",
            nameEn: "Communication Diagram",
            description: "Interação entre objetos com foco em estrutura.",
            useCase: "Mensagens entre componentes do motor de regras.",
            tools: ["PlantUML", "Enterprise Architect"],
          },
          {
            name: "Diagrama de Atividade",
            nameEn: "Activity Diagram",
            description: "Fluxo de controle de atividades (similar a fluxograma, mas UML).",
            useCase: "Algoritmo de avaliação de regras com fork/join para paralelismo.",
            tools: ["PlantUML", "draw.io"],
          },
          {
            name: "Diagrama de Máquina de Estados",
            nameEn: "State Machine Diagram",
            description: "Estados e transições de uma entidade.",
            useCase: "Ciclo de vida de uma Rule: DRAFT → ACTIVE → DEPRECATED.",
            tools: ["PlantUML", "Mermaid"],
          },
          {
            name: "Diagrama de Tempo",
            nameEn: "Timing Diagram",
            description: "Mudança de estado ao longo do tempo (timeline).",
            useCase: "SLA de resposta do endpoint /analyze ao longo de 24h.",
            tools: ["Enterprise Architect"],
          },
          {
            name: "Diagrama de Visão Geral de Interação",
            nameEn: "Interaction Overview Diagram",
            description: "Combina diagramas de atividade e sequência.",
            useCase: "Fluxo completo de uma transação mostrando alternativas.",
            tools: ["Enterprise Architect"],
          },
        ],
      },
    ],
  },
  {
    id: "architecture",
    title: "Arquitetura de Software",
    icon: Layers,
    color: "green",
    description: "Diagramas para documentar a arquitetura do sistema (C4, camadas, microserviços).",
    diagrams: [
      {
        name: "C4 - Contexto",
        nameEn: "C4 System Context Diagram",
        description: "Nível 1: Sistema no contexto de usuários e sistemas externos.",
        useCase: "RULEX e suas integrações: Usuários, Gateway, Banco Emissor.",
        tools: ["Structurizr", "PlantUML C4", "draw.io"],
        example: `                    ┌─────────┐
                    │ Analyst │
                    └────┬────┘
                         │
                         ▼
┌──────────┐       ┌──────────┐       ┌──────────┐
│ Payment  │◄─────►│  RULEX   │◄─────►│  Core    │
│ Gateway  │       │  System  │       │ Banking  │
└──────────┘       └──────────┘       └──────────┘
                         │
                         ▼
                    ┌──────────┐
                    │ External │
                    │  Bureau  │
                    └──────────┘`,
      },
      {
        name: "C4 - Contêineres",
        nameEn: "C4 Container Diagram",
        description: "Nível 2: Aplicações e datastores que compõem o sistema.",
        useCase: "Frontend React, Backend Spring Boot, PostgreSQL, Redis.",
        tools: ["Structurizr", "PlantUML C4"],
        example: `┌─────────────────────────────────────────────┐
│                  RULEX System               │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐   │
│  │   SPA    │  │   API    │  │   DB     │   │
│  │  React   │─►│  Spring  │─►│ Postgres │   │
│  │  :5173   │  │  :8080   │  │  :5432   │   │
│  └──────────┘  └──────────┘  └──────────┘   │
│                      │                      │
│                      ▼                      │
│               ┌──────────┐                  │
│               │   Redis  │                  │
│               │  :6379   │                  │
│               └──────────┘                  │
└─────────────────────────────────────────────┘`,
      },
      {
        name: "C4 - Componentes",
        nameEn: "C4 Component Diagram",
        description: "Nível 3: Componentes dentro de um contêiner.",
        useCase: "Controllers, Services, Repositories do backend.",
        tools: ["Structurizr", "PlantUML C4"],
      },
      {
        name: "C4 - Código",
        nameEn: "C4 Code Diagram",
        description: "Nível 4: Classes/código (opcional, usa UML).",
        useCase: "Detalhe do ExpressionEvaluator e AST.",
        tools: ["PlantUML", "IDE diagrams"],
      },
      {
        name: "Arquitetura em Camadas",
        nameEn: "Layered Architecture Diagram",
        description: "Organização em camadas: Presentation, Business, Data.",
        useCase: "Estrutura do backend RULEX seguindo Clean Architecture.",
        tools: ["draw.io", "Lucidchart"],
        example: `┌─────────────────────────────────┐
│        Presentation Layer       │
│   (Controllers, DTOs, Mappers)  │
├─────────────────────────────────┤
│         Business Layer          │
│  (Services, Domain, Validators) │
├─────────────────────────────────┤
│       Infrastructure Layer      │
│ (Repositories, External APIs)   │
├─────────────────────────────────┤
│          Data Layer             │
│    (Entities, Migrations)       │
└─────────────────────────────────┘`,
      },
      {
        name: "Arquitetura Hexagonal",
        nameEn: "Hexagonal / Ports & Adapters",
        description: "Domínio no centro, ports (interfaces) e adapters (implementações).",
        useCase: "Isolar lógica de negócio de frameworks e I/O.",
        tools: ["draw.io", "PlantUML"],
      },
      {
        name: "Diagrama de Microserviços",
        nameEn: "Microservices Architecture Diagram",
        description: "Serviços independentes e suas comunicações.",
        useCase: "Se RULEX evoluir para microserviços: Rules, Transactions, Audit.",
        tools: ["draw.io", "Lucidchart"],
      },
      {
        name: "Topologia de Sistemas",
        nameEn: "System Topology Diagram",
        description: "Visão física/lógica da distribuição dos sistemas.",
        useCase: "Deployment multi-região com failover.",
        tools: ["draw.io", "Cloudcraft"],
      },
      {
        name: "Cliente-Servidor",
        nameEn: "Client-Server Diagram",
        description: "Interação básica entre cliente e servidor.",
        useCase: "Browser ↔ API ↔ Database.",
        tools: ["draw.io"],
      },
      {
        name: "Landscape de Integração",
        nameEn: "Integration Landscape Diagram",
        description: "Visão geral de todas as integrações do ecossistema.",
        useCase: "RULEX + todos os sistemas que consome ou expõe APIs.",
        tools: ["draw.io", "Lucidchart"],
      },
      {
        name: "ArchiMate",
        nameEn: "ArchiMate Diagram",
        description: "Notação enterprise: camadas de negócio, aplicação e tecnologia.",
        useCase: "Documentação corporativa alinhando TI e negócio.",
        tools: ["Archi", "BiZZdesign"],
      },
    ],
  },
  {
    id: "ddd",
    title: "DDD (Domain-Driven Design)",
    icon: Hexagon,
    color: "orange",
    description: "Diagramas para modelagem de domínio usando padrões DDD.",
    diagrams: [
      {
        name: "Mapa de Contextos",
        nameEn: "Context Map",
        description: "Relações entre Bounded Contexts (upstream/downstream, ACL, partnership).",
        useCase: "Contextos do RULEX: Rules, Transactions, Users, Audit, Reporting.",
        tools: ["Context Mapper", "draw.io"],
        example: `┌───────────────┐     ┌───────────────┐
│    Rules      │     │ Transactions  │
│   Context     │◄───►│   Context     │
│               │ ACL │               │
└───────────────┘     └───────────────┘
        │                     │
        │ U/D                  │ U/D
        ▼                     ▼
┌───────────────┐     ┌───────────────┐
│    Users      │     │    Audit      │
│   Context     │     │   Context     │
└───────────────┘     └───────────────┘`,
      },
      {
        name: "Bounded Contexts",
        nameEn: "Bounded Context Diagram",
        description: "Limites claros de cada contexto e sua linguagem ubíqua.",
        useCase: "Definir fronteiras: o que é 'Rule' no contexto de configuração vs. avaliação.",
        tools: ["draw.io", "Miro"],
      },
      {
        name: "Modelo de Domínio",
        nameEn: "Domain Model Diagram",
        description: "Entidades, Value Objects e seus relacionamentos no domínio.",
        useCase: "Modelo do domínio de Regras: Rule, Condition, Expression, Action.",
        tools: ["PlantUML", "draw.io"],
      },
      {
        name: "Event Storming",
        nameEn: "Event Storming",
        description: "Workshop colaborativo: eventos de domínio em post-its (laranja, azul, amarelo).",
        useCase: "Descobrir eventos: TransactionReceived, RuleEvaluated, FraudDetected.",
        tools: ["Miro", "Mural", "FigJam"],
        example: `┌─────────────┐  ┌─────────────┐  ┌─────────────┐
│  Transaction│  │    Rule     │  │   Fraud     │
│  Received   │─►│  Evaluated  │─►│  Detected   │
│   (orange)  │  │   (orange)  │  │   (orange)  │
└─────────────┘  └─────────────┘  └─────────────┘
      │                │                │
      ▼                ▼                ▼
┌─────────────┐  ┌─────────────┐  ┌─────────────┐
│   Analyst   │  │  Rule Svc   │  │Alert Service│
│   (yellow)  │  │   (blue)    │  │   (blue)    │
└─────────────┘  └─────────────┘  └─────────────┘`,
      },
      {
        name: "Diagrama de Agregados",
        nameEn: "Aggregate Diagram",
        description: "Aggregate roots e suas entidades/value objects internos.",
        useCase: "Rule como aggregate root contendo Conditions.",
        tools: ["PlantUML", "draw.io"],
      },
      {
        name: "Entidades e Value Objects",
        nameEn: "Entity/Value Object Diagram",
        description: "Distinção entre entidades (identidade) e VOs (imutáveis, sem identidade).",
        useCase: "Transaction (entity) vs. Money, Address (value objects).",
        tools: ["PlantUML"],
      },
      {
        name: "Serviços de Domínio",
        nameEn: "Domain Services Diagram",
        description: "Operações que não pertencem a nenhuma entidade específica.",
        useCase: "FraudScoringService, RuleEvaluationService.",
        tools: ["PlantUML", "draw.io"],
      },
    ],
  },
  {
    id: "api",
    title: "API e Integração",
    icon: FileJson,
    color: "cyan",
    description: "Diagramas para documentar APIs REST, eventos e padrões de integração.",
    diagrams: [
      {
        name: "Contrato de API",
        nameEn: "API Contract Diagram",
        description: "Visualização do OpenAPI/Swagger: endpoints, métodos, schemas.",
        useCase: "Documentar a API do RULEX: /api/rules, /api/transactions, etc.",
        tools: ["Swagger UI", "Redoc", "Stoplight"],
      },
      {
        name: "Mapa de Endpoints",
        nameEn: "Endpoint Map",
        description: "Visão geral de todas as rotas agrupadas por recurso.",
        useCase: "Catálogo visual dos 18 endpoints do RULEX.",
        tools: ["draw.io", "Notion"],
        example: `/api
├── /rules
│   ├── GET    (list)
│   ├── POST   (create)
│   └── /{id}
│       ├── GET    (read)
│       ├── PUT    (update)
│       ├── DELETE (delete)
│       └── /toggle (PATCH)
├── /transactions
│   ├── GET    (list)
│   └── /analyze (POST)
├── /metrics
│   └── /timeline (GET)
└── /auth
    └── /login (POST)`,
      },
      {
        name: "Sequência de API",
        nameEn: "API Request/Response Sequence",
        description: "Fluxo de requisições e respostas entre cliente e servidor.",
        useCase: "Detalhar autenticação + chamada + resposta + retry.",
        tools: ["Mermaid", "PlantUML"],
      },
      {
        name: "Arquitetura Event-Driven",
        nameEn: "Event-Driven Architecture Diagram",
        description: "Produtores, consumidores e brokers de eventos.",
        useCase: "Se RULEX usar Kafka: FraudDetected → Notification Service.",
        tools: ["draw.io", "Confluent"],
      },
      {
        name: "Diagrama de Tópicos/Filas",
        nameEn: "Topic/Queue Diagram",
        description: "Estrutura de tópicos/filas e seus consumidores.",
        useCase: "Tópicos Kafka: transactions, fraud-alerts, audit-events.",
        tools: ["draw.io", "Lucidchart"],
      },
      {
        name: "Pub/Sub",
        nameEn: "Publish-Subscribe Diagram",
        description: "Padrão de publicação e assinatura de mensagens.",
        useCase: "Notificações em tempo real via WebSocket ou SSE.",
        tools: ["draw.io"],
      },
      {
        name: "Diagrama de Saga",
        nameEn: "Saga Pattern Diagram",
        description: "Orquestração ou coreografia de transações distribuídas.",
        useCase: "Rollback de aprovação se notificação falhar.",
        tools: ["draw.io", "PlantUML"],
      },
      {
        name: "Integração Assíncrona",
        nameEn: "Async Integration Diagram",
        description: "Fluxos assíncronos com callbacks, polling ou webhooks.",
        useCase: "Integração com bureau de crédito via webhook.",
        tools: ["draw.io"],
      },
      {
        name: "Circuit Breaker / Resiliência",
        nameEn: "Resilience Pattern Diagram",
        description: "Padrões de resiliência: circuit breaker, retry, bulkhead.",
        useCase: "Proteger RULEX de falhas em serviços externos.",
        tools: ["draw.io"],
        example: `┌─────────┐     ┌──────────────┐     ┌─────────┐
│ Service │────►│Circuit Breaker│────►│ External│
│    A    │◄────│    (Closed)   │◄────│ Service │
└─────────┘     └──────────────┘     └─────────┘
                     │
              Failures > threshold
                     │
                     ▼
                ┌──────────────┐
                │Circuit Breaker│
                │    (Open)    │
                │  → Fallback  │
                └──────────────┘`,
      },
    ],
  },
  {
    id: "data",
    title: "Dados e Bancos",
    icon: Database,
    color: "violet",
    description: "Diagramas para modelagem de dados (PostgreSQL, Redis, Neo4j).",
    subcategories: [
      {
        name: "PostgreSQL (Relacional)",
        diagrams: [
          {
            name: "DER / ERD",
            nameEn: "Entity-Relationship Diagram",
            description: "Entidades, atributos e relacionamentos do banco relacional.",
            useCase: "Schema do RULEX: rules, conditions, transactions, decisions.",
            tools: ["dbdiagram.io", "DBeaver", "pgModeler"],
            example: `┌─────────────┐       ┌─────────────┐
│    rules    │       │ conditions  │
├─────────────┤       ├─────────────┤
│ id (PK)     │       │ id (PK)     │
│ name        │───┐   │ rule_id (FK)│───┘
│ enabled     │   │   │ field       │
│ threshold   │   │   │ operator    │
└─────────────┘   │   │ value       │
                  │   └─────────────┘
                  │
                  │   ┌─────────────┐
                  │   │transactions │
                  │   ├─────────────┤
                  └──►│ id (PK)     │
                      │ amount      │
                      │ decision    │
                      └─────────────┘`,
          },
          {
            name: "Modelo Conceitual",
            nameEn: "Conceptual Data Model",
            description: "Entidades e relacionamentos sem detalhes técnicos.",
            useCase: "Discussão com stakeholders sobre o domínio.",
            tools: ["draw.io", "Lucidchart"],
          },
          {
            name: "Modelo Lógico",
            nameEn: "Logical Data Model",
            description: "Atributos, tipos e cardinalidades sem físico.",
            useCase: "Design independente de SGBD.",
            tools: ["ERwin", "PowerDesigner"],
          },
          {
            name: "Modelo Físico",
            nameEn: "Physical Data Model",
            description: "Schema completo com tipos, índices, constraints.",
            useCase: "Script de criação do banco RULEX.",
            tools: ["DBeaver", "pgModeler"],
          },
          {
            name: "Diagrama de Esquema",
            nameEn: "Schema Diagram",
            description: "Tabelas, colunas e FKs do banco.",
            useCase: "Documentação técnica do schema rulex.",
            tools: ["DBeaver", "DataGrip"],
          },
          {
            name: "Diagrama de Normalização",
            nameEn: "Normalization Diagram",
            description: "Etapas de normalização (1NF → 3NF → BCNF).",
            useCase: "Justificar decisões de design do schema.",
            tools: ["draw.io"],
          },
          {
            name: "Diagrama de Índices",
            nameEn: "Index Diagram",
            description: "Índices e sua cobertura de queries.",
            useCase: "Otimização de queries do RULEX.",
            tools: ["pgAdmin", "explain.depesz.com"],
          },
          {
            name: "Diagrama de Particionamento",
            nameEn: "Partitioning Diagram",
            description: "Estratégia de particionamento de tabelas grandes.",
            useCase: "Particionar transactions por data.",
            tools: ["pgModeler", "draw.io"],
          },
          {
            name: "Diagrama de Replicação",
            nameEn: "Replication Topology Diagram",
            description: "Primary/replica setup para HA.",
            useCase: "PostgreSQL com streaming replication.",
            tools: ["draw.io", "Cloudcraft"],
          },
          {
            name: "Diagrama de Sharding",
            nameEn: "Sharding Diagram",
            description: "Distribuição de dados entre shards.",
            useCase: "Se escalar horizontalmente por tenant/região.",
            tools: ["draw.io"],
          },
          {
            name: "Linhagem de Dados",
            nameEn: "Data Lineage Diagram",
            description: "Origem e transformações dos dados.",
            useCase: "Rastrear dados de transação da origem à decisão.",
            tools: ["DataHub", "draw.io"],
          },
          {
            name: "DFD - Fluxo de Dados",
            nameEn: "Data Flow Diagram",
            description: "Fluxo de dados entre processos e datastores.",
            useCase: "Dados fluindo: API → Service → DB → Cache.",
            tools: ["draw.io", "Lucidchart"],
          },
        ],
      },
      {
        name: "Redis (Cache/In-Memory)",
        diagrams: [
          {
            name: "Estratégia de Cache",
            nameEn: "Caching Strategy Diagram",
            description: "Padrões: Cache-Aside, Read-Through, Write-Through, Write-Behind.",
            useCase: "Cache de regras ativas para performance.",
            tools: ["draw.io"],
            example: `┌─────────────────────────────────────────────┐
│           Cache-Aside Pattern               │
│  ┌────────┐    miss     ┌────────┐          │
│  │  App   │───────────►│   DB   │          │
│  │        │◄───────────│        │          │
│  │        │    data     └────────┘          │
│  │        │                                  │
│  │        │───────────►┌────────┐           │
│  │        │   write    │  Redis │           │
│  │        │◄───────────│  Cache │           │
│  └────────┘    hit     └────────┘           │
└─────────────────────────────────────────────┘`,
          },
          {
            name: "Diagrama de Keyspace",
            nameEn: "Keyspace Diagram",
            description: "Organização de chaves: prefixos, namespaces.",
            useCase: "rulex:rules:{id}, rulex:txn:{id}:score.",
            tools: ["draw.io", "RedisInsight"],
          },
          {
            name: "TTL e Evicção",
            nameEn: "TTL & Eviction Flow Diagram",
            description: "Políticas de expiração e evicção de cache.",
            useCase: "TTL de 5min para regras, LRU para scores.",
            tools: ["draw.io"],
          },
          {
            name: "Consistência de Cache",
            nameEn: "Cache Consistency Diagram",
            description: "Estratégias para manter cache consistente com DB.",
            useCase: "Invalidação ao atualizar regra.",
            tools: ["draw.io"],
          },
          {
            name: "Replicação Redis",
            nameEn: "Redis Replication Diagram",
            description: "Master-replica para HA.",
            useCase: "Redis Sentinel setup.",
            tools: ["draw.io", "RedisInsight"],
          },
          {
            name: "Cluster/Slots",
            nameEn: "Redis Cluster Topology Diagram",
            description: "Hash slots distribuídos entre nós.",
            useCase: "Redis Cluster para escala horizontal.",
            tools: ["draw.io"],
          },
          {
            name: "Pub/Sub Redis",
            nameEn: "Redis Pub/Sub Topology",
            description: "Canais de publicação/assinatura.",
            useCase: "Notificações em tempo real de fraude.",
            tools: ["draw.io"],
          },
          {
            name: "Streams e Consumer Groups",
            nameEn: "Redis Streams Diagram",
            description: "Log de eventos com consumer groups.",
            useCase: "Event sourcing leve com Redis Streams.",
            tools: ["draw.io", "RedisInsight"],
          },
        ],
      },
      {
        name: "Neo4j (Grafo)",
        diagrams: [
          {
            name: "Modelo de Grafo",
            nameEn: "Graph Data Model Diagram",
            description: "Nós, relacionamentos e propriedades.",
            useCase: "Grafo de relacionamentos para detectar fraude em rede.",
            tools: ["Neo4j Browser", "Arrows.app"],
            example: `(User:A)──[:TRANSFERRED]──►(User:B)
    │                           │
    │                           │
[:OWNS]                     [:OWNS]
    │                           │
    ▼                           ▼
(Device:1)                  (Device:2)
    │                           │
    └───────[:SAME_IP]──────────┘`,
          },
          {
            name: "Schema de Grafo",
            nameEn: "Graph Schema Diagram",
            description: "Labels, tipos de relacionamento, propriedades.",
            useCase: "Definir schema para análise de rede de fraude.",
            tools: ["Arrows.app", "Neo4j Browser"],
          },
          {
            name: "Padrões de Consulta",
            nameEn: "Graph Pattern Diagram",
            description: "Match patterns visuais para Cypher.",
            useCase: "Encontrar ciclos de transferência suspeitos.",
            tools: ["Neo4j Browser"],
          },
          {
            name: "Diagrama de Travessia",
            nameEn: "Traversal Diagram",
            description: "Caminho de navegação no grafo.",
            useCase: "Caminho mais curto entre dois usuários suspeitos.",
            tools: ["Neo4j Browser"],
          },
          {
            name: "Diagrama de Subgrafos",
            nameEn: "Subgraph Diagram",
            description: "Subconjunto relevante do grafo.",
            useCase: "Cluster de contas relacionadas.",
            tools: ["Neo4j Bloom", "Arrows.app"],
          },
          {
            name: "Mapa de Relacionamentos",
            nameEn: "Relationship Map",
            description: "Todos os tipos de relacionamento e sua cardinalidade.",
            useCase: "Documentar modelo de grafo do RULEX.",
            tools: ["Arrows.app"],
          },
          {
            name: "Projeção de Grafo",
            nameEn: "Graph Projection Diagram",
            description: "Grafo projetado para algoritmos GDS.",
            useCase: "Projeção para PageRank de contas suspeitas.",
            tools: ["Neo4j Browser"],
          },
        ],
      },
    ],
  },
  {
    id: "frontend",
    title: "Frontend React e UX",
    icon: Layout,
    color: "pink",
    description: "Diagramas para arquitetura de frontend, estado e UX.",
    diagrams: [
      {
        name: "Árvore de Componentes",
        nameEn: "Component Tree Diagram",
        description: "Hierarquia de componentes React.",
        useCase: "Estrutura do RULEX: App → Layout → Pages → Components.",
        tools: ["React DevTools", "draw.io"],
        example: `<App>
├── <ThemeProvider>
│   └── <DashboardLayout>
│       ├── <Sidebar>
│       │   └── <NavItems/>
│       └── <Main>
│           └── <Routes>
│               ├── <Dashboard/>
│               ├── <Rules/>
│               │   ├── <RuleTable/>
│               │   └── <RuleFormDialog/>
│               └── <Manual/>
│                   ├── <GlobalSearch/>
│                   └── <Tabs>...`,
      },
      {
        name: "Hierarquia de Views",
        nameEn: "View Hierarchy Diagram",
        description: "Páginas e suas sub-views/seções.",
        useCase: "Estrutura de páginas: Dashboard, Rules, Transactions, Manual.",
        tools: ["Figma", "draw.io"],
      },
      {
        name: "Diagrama de Estado UI",
        nameEn: "UI State Diagram",
        description: "Estados de um componente: loading, error, success, empty.",
        useCase: "Estados da RuleTable: loading → loaded | error.",
        tools: ["draw.io", "Stately.ai"],
      },
      {
        name: "Fluxo de Estado",
        nameEn: "State Flow Diagram",
        description: "Fluxo de dados através de Context/Redux/Zustand.",
        useCase: "AuthContext → useAuth() → Components.",
        tools: ["draw.io"],
        example: `┌─────────────────────────────────────────┐
│              React Context              │
│  ┌─────────┐                            │
│  │ AuthCtx │                            │
│  └────┬────┘                            │
│       │ useAuth()                       │
│       ▼                                 │
│  ┌─────────┐    ┌─────────┐             │
│  │  Login  │    │ Layout  │             │
│  │  Page   │    │  (user) │             │
│  └─────────┘    └─────────┘             │
└─────────────────────────────────────────┘`,
      },
      {
        name: "Diagrama de Navegação",
        nameEn: "Navigation / Routing Diagram",
        description: "Rotas e transições entre páginas.",
        useCase: "Mapa de navegação do RULEX.",
        tools: ["draw.io", "Overflow"],
      },
      {
        name: "Wireframe",
        nameEn: "Wireframe",
        description: "Esboço de baixa fidelidade da interface.",
        useCase: "Prototipar nova feature antes de implementar.",
        tools: ["Figma", "Balsamiq", "Excalidraw"],
      },
      {
        name: "Mockup",
        nameEn: "Mockup",
        description: "Design de alta fidelidade da interface.",
        useCase: "Design final para aprovação de stakeholders.",
        tools: ["Figma", "Sketch", "Adobe XD"],
      },
      {
        name: "Protótipo",
        nameEn: "Prototype",
        description: "Mockup interativo com transições.",
        useCase: "Teste de usabilidade antes do desenvolvimento.",
        tools: ["Figma", "Framer", "InVision"],
      },
      {
        name: "Mapa do Site / IA",
        nameEn: "Information Architecture / Sitemap",
        description: "Estrutura de informação e navegação.",
        useCase: "Organização de conteúdo do RULEX.",
        tools: ["Miro", "Octopus.do"],
      },
      {
        name: "Diagrama de Interação",
        nameEn: "Interaction Diagram",
        description: "Cliques, efeitos e chamadas resultantes.",
        useCase: "Fluxo: Click 'Criar Regra' → Modal → Submit → API → Toast.",
        tools: ["Figma", "draw.io"],
      },
      {
        name: "Sequência UI↔API",
        nameEn: "Frontend-Backend Sequence",
        description: "Interação entre componente React e backend.",
        useCase: "RuleFormDialog → POST /api/rules → onSuccess → refetch.",
        tools: ["Mermaid", "PlantUML"],
      },
    ],
  },
  {
    id: "infra",
    title: "Infra e DevOps",
    icon: Cloud,
    color: "slate",
    description: "Diagramas para infraestrutura, cloud, CI/CD e observabilidade.",
    diagrams: [
      {
        name: "Diagrama de Cloud",
        nameEn: "Cloud Architecture Diagram",
        description: "Recursos de cloud: VPC, subnets, compute, storage.",
        useCase: "AWS/GCP/Azure setup para o RULEX.",
        tools: ["Cloudcraft", "draw.io AWS icons", "Diagrams (Python)"],
        example: `┌────────────────────────────────────────────────┐
│                    AWS VPC                     │
│  ┌──────────────────┐  ┌──────────────────┐   │
│  │  Public Subnet   │  │  Private Subnet  │   │
│  │  ┌────────────┐  │  │  ┌────────────┐  │   │
│  │  │    ALB     │  │  │  │   ECS/K8s  │  │   │
│  │  │  (HTTPS)   │──┼──┼─►│  Backend   │  │   │
│  │  └────────────┘  │  │  └────────────┘  │   │
│  │                  │  │        │         │   │
│  │  ┌────────────┐  │  │        ▼         │   │
│  │  │  CloudFront│  │  │  ┌────────────┐  │   │
│  │  │  (Frontend)│  │  │  │    RDS     │  │   │
│  │  └────────────┘  │  │  │ PostgreSQL │  │   │
│  └──────────────────┘  │  └────────────┘  │   │
│                        └──────────────────┘   │
└────────────────────────────────────────────────┘`,
      },
      {
        name: "Diagrama de Rede",
        nameEn: "Network Diagram",
        description: "Topologia de rede: VPCs, peering, gateways.",
        useCase: "Conectividade entre ambientes dev/staging/prod.",
        tools: ["draw.io", "Lucidchart"],
      },
      {
        name: "Segurança de Rede",
        nameEn: "Security Groups / Firewall Diagram",
        description: "Regras de ingress/egress por security group.",
        useCase: "Documentar regras de firewall do RULEX.",
        tools: ["draw.io", "Cloudcraft"],
      },
      {
        name: "Arquitetura Kubernetes",
        nameEn: "K8s Architecture Diagram",
        description: "Pods, Services, Ingress, ConfigMaps, Secrets.",
        useCase: "Deploy do RULEX em Kubernetes.",
        tools: ["draw.io", "Lens"],
        example: `┌─────────────────────────────────────┐
│           Kubernetes Cluster        │
│  ┌──────────────────────────────┐   │
│  │         Namespace: rulex     │   │
│  │  ┌─────────┐  ┌─────────┐    │   │
│  │  │Deployment│  │Deployment│   │   │
│  │  │ backend │  │ frontend│    │   │
│  │  │ (3 pods)│  │ (2 pods)│    │   │
│  │  └────┬────┘  └────┬────┘    │   │
│  │       │            │          │   │
│  │       ▼            ▼          │   │
│  │  ┌─────────┐  ┌─────────┐    │   │
│  │  │ Service │  │ Service │    │   │
│  │  │ClusterIP│  │ClusterIP│    │   │
│  │  └────┬────┘  └────┬────┘    │   │
│  │       └─────┬──────┘          │   │
│  │             ▼                 │   │
│  │        ┌─────────┐            │   │
│  │        │ Ingress │            │   │
│  │        └─────────┘            │   │
│  └──────────────────────────────┘   │
└─────────────────────────────────────┘`,
      },
      {
        name: "Diagrama de Contêineres",
        nameEn: "Containerization Diagram",
        description: "Imagens Docker, registries, orquestração.",
        useCase: "Build e deploy de containers RULEX.",
        tools: ["draw.io"],
      },
      {
        name: "Pipeline CI/CD",
        nameEn: "CI/CD Pipeline Diagram",
        description: "Etapas: build, test, scan, deploy, rollback.",
        useCase: "GitHub Actions workflow do RULEX.",
        tools: ["draw.io", "GitLab CI viz"],
        example: `┌──────┐    ┌──────┐    ┌──────┐    ┌──────┐    ┌──────┐
│ Push │───►│Build │───►│ Test │───►│ Scan │───►│Deploy│
│      │    │      │    │      │    │      │    │      │
└──────┘    └──────┘    └──────┘    └──────┘    └──────┘
                            │                       │
                            ▼                       ▼
                       ┌──────┐                ┌──────┐
                       │Notify│                │Rollbk│
                       │ Fail │                │      │
                       └──────┘                └──────┘`,
      },
      {
        name: "Diagrama de Release",
        nameEn: "Release Flow Diagram",
        description: "Fluxo de versionamento e releases.",
        useCase: "Git flow: feature → develop → release → main.",
        tools: ["draw.io", "GitKraken"],
      },
      {
        name: "Blue/Green Deployment",
        nameEn: "Blue-Green Deployment Diagram",
        description: "Dois ambientes idênticos para zero-downtime.",
        useCase: "Deploy sem downtime do RULEX.",
        tools: ["draw.io"],
      },
      {
        name: "Canary Deployment",
        nameEn: "Canary Deployment Diagram",
        description: "Rollout gradual para subset de tráfego.",
        useCase: "Testar nova versão com 5% do tráfego.",
        tools: ["draw.io"],
      },
      {
        name: "Observabilidade",
        nameEn: "Observability Architecture Diagram",
        description: "Logs, métricas, traces e alertas.",
        useCase: "Stack: Prometheus + Grafana + Loki + Tempo.",
        tools: ["draw.io", "Grafana"],
        example: `┌─────────────────────────────────────────────┐
│              Observability Stack            │
│                                             │
│  ┌─────────┐  ┌─────────┐  ┌─────────┐     │
│  │  Logs   │  │ Metrics │  │ Traces  │     │
│  │  Loki   │  │Prometheu│  │  Tempo  │     │
│  └────┬────┘  └────┬────┘  └────┬────┘     │
│       │            │            │           │
│       └────────────┼────────────┘           │
│                    ▼                        │
│              ┌──────────┐                   │
│              │ Grafana  │                   │
│              │Dashboard │                   │
│              └──────────┘                   │
└─────────────────────────────────────────────┘`,
      },
    ],
  },
  {
    id: "security",
    title: "Segurança e Risco",
    icon: Shield,
    color: "red",
    description: "Diagramas para threat modeling, autenticação e análise de risco.",
    diagrams: [
      {
        name: "DFD com Trust Boundaries",
        nameEn: "Threat-Model DFD",
        description: "Data Flow Diagram com limites de confiança marcados.",
        useCase: "Base para threat modeling do RULEX.",
        tools: ["Microsoft Threat Modeling Tool", "draw.io"],
        example: `┌─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ┐
  Trust Boundary: Internet
│ ┌────────────┐                        │
  │   Browser  │
│ │  (Client)  │                        │
  └─────┬──────┘
│       │ HTTPS                         │
─ ─ ─ ─ ┼ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─
        │   Trust Boundary: DMZ
┌ ─ ─ ─ ┼ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ┐
        ▼
│ ┌────────────┐     ┌────────────┐    │
  │    API     │────►│    DB      │
│ │  Gateway   │     │ PostgreSQL │    │
  └────────────┘     └────────────┘
└ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ┘`,
      },
      {
        name: "Diagrama STRIDE",
        nameEn: "STRIDE Threat Model",
        description: "Ameaças mapeadas por categoria STRIDE em cada componente.",
        useCase: "Identificar Spoofing, Tampering, Repudiation, Info Disclosure, DoS, Elevation.",
        tools: ["Microsoft TMT", "OWASP Threat Dragon"],
      },
      {
        name: "Árvore de Ataque",
        nameEn: "Attack Tree",
        description: "Hierarquia de caminhos que um atacante pode seguir.",
        useCase: "Mapear vetores de ataque ao endpoint /analyze.",
        tools: ["draw.io", "ADTool"],
        example: `              [Comprometer RULEX]
                     │
         ┌───────────┼───────────┐
         │           │           │
    [SQL Inj]   [Auth Bypass] [DoS]
         │           │           │
    ┌────┴────┐  ┌───┴───┐   ┌──┴──┐
 [Input] [ORM] [JWT] [Session] [Rate]
                               [Limit]`,
      },
      {
        name: "Kill Chain",
        nameEn: "Kill Chain Diagram",
        description: "Fases de um ataque: recon → weaponize → deliver → exploit → persist.",
        useCase: "Entender como um atacante poderia comprometer o sistema.",
        tools: ["draw.io"],
      },
      {
        name: "Fluxo de Autenticação",
        nameEn: "Auth Flow Diagram",
        description: "Sequência de autenticação e autorização.",
        useCase: "Fluxo de login do RULEX.",
        tools: ["draw.io", "Mermaid"],
        example: `┌────────┐     ┌────────┐     ┌────────┐
│ Client │     │  API   │     │  Auth  │
│        │     │        │     │ Server │
└───┬────┘     └───┬────┘     └───┬────┘
    │   Login      │              │
    │─────────────►│              │
    │              │  Validate    │
    │              │─────────────►│
    │              │◄─────────────│
    │              │    Token     │
    │◄─────────────│              │
    │   JWT Token  │              │
    │              │              │
    │ Request+JWT  │              │
    │─────────────►│              │
    │              │ Verify JWT   │
    │◄─────────────│              │
    │   Response   │              │`,
      },
      {
        name: "OAuth2 Flow",
        nameEn: "OAuth2 Flow Diagram",
        description: "Authorization Code, Implicit, Client Credentials flows.",
        useCase: "Integração com IdP externo.",
        tools: ["draw.io"],
      },
      {
        name: "OIDC Flow",
        nameEn: "OpenID Connect Flow Diagram",
        description: "OAuth2 + ID Token para autenticação.",
        useCase: "SSO com Azure AD ou Okta.",
        tools: ["draw.io"],
      },
      {
        name: "Matriz de Risco",
        nameEn: "Risk Matrix",
        description: "Probabilidade vs. Impacto para priorizar riscos.",
        useCase: "Priorizar mitigações de segurança.",
        tools: ["Excel", "draw.io"],
      },
      {
        name: "Diagrama RBAC",
        nameEn: "RBAC Diagram",
        description: "Roles, permissões e usuários.",
        useCase: "Modelo de autorização do RULEX: Admin, Analyst, Viewer.",
        tools: ["draw.io"],
        example: `┌─────────────────────────────────────────┐
│               RBAC Model                │
│                                         │
│  ┌─────────┐         ┌─────────┐        │
│  │  Admin  │         │ Analyst │        │
│  └────┬────┘         └────┬────┘        │
│       │                   │             │
│       ▼                   ▼             │
│  ┌─────────────────────────────────┐    │
│  │         Permissions             │    │
│  ├─────────────────────────────────┤    │
│  │ rules:create    ✓        ✗      │    │
│  │ rules:read      ✓        ✓      │    │
│  │ rules:update    ✓        ✗      │    │
│  │ rules:delete    ✓        ✗      │    │
│  │ txn:analyze     ✓        ✓      │    │
│  │ audit:read      ✓        ✓      │    │
│  └─────────────────────────────────┘    │
└─────────────────────────────────────────┘`,
      },
    ],
  },
];
