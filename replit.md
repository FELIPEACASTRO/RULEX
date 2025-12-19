# RULEX - Fraud Detection Rules Engine

## Overview

RULEX is a credit card transaction fraud detection platform implementing a configurable hard rules engine. The system evaluates transactions in real-time, classifying them as APPROVED, SUSPICIOUS, or FRAUD based on deterministic rules without machine learning.

**Core Purpose**: Banking-grade anti-fraud system with audit trails, rule versioning, and compliance-ready architecture.

**Tech Stack**:
- Backend: Java 21 + Spring Boot 3.2.1 + PostgreSQL (primary API)
- Frontend: React 19 + TypeScript + Tailwind CSS + shadcn/ui
- Node Server: Express + tRPC (authentication/supplementary APIs)
- Database: PostgreSQL (Java) + MySQL/TiDB (Node/Drizzle)

## User Preferences

Preferred communication style: Simple, everyday language.

## System Architecture

### Multi-Service Design
The application runs as multiple services orchestrated via Docker Compose:
- **web**: Vite/React frontend
- **backend**: Java Spring Boot (core fraud detection engine)
- **postgres**: PostgreSQL database for Java backend

The Node/tRPC server exists in the codebase but is not deployed in the Docker Compose configuration. The frontend proxies `/api` requests to the Java backend.

### Backend Architecture (Java)

**Clean Architecture with Hexagonal Patterns**:
- `controller/` - REST API endpoints (delivery layer)
- `homolog/usecase/` - Business logic use cases (framework-independent)
- `homolog/port/` - Abstractions/interfaces for external dependencies
- `homolog/adapter/` - Spring implementations of ports
- `homolog/application/` - Application services (transaction boundaries)
- `entity/` - JPA entities
- `dto/` - Data transfer objects
- `repository/` - JPA repositories

**Rule Engine**:
- Deterministic rules only (no ML)
- Rules stored in database with versioning
- Each rule has: name, conditions (JSON), weight, classification, category
- Transaction analysis triggers matching rules and computes final classification

**Key Endpoints** (under `/api`):
- `POST /transactions/analyze` - Idempotent transaction analysis
- `GET /transactions` - List with filters
- `GET/POST/PUT/DELETE /rules` - Rule CRUD
- `GET /audit` - Audit trail
- `GET /metrics` - System metrics
- Homologation endpoints under `/homolog/*`

### Frontend Architecture (React)

**Component Structure**:
- `pages/` - Route components (Dashboard, Transactions, Rules, Audit, Simulator)
- `components/` - Reusable UI components
- `components/ui/` - shadcn/ui primitives
- `lib/` - Utilities and API clients
- `contexts/` - React context providers

**State Management**: TanStack Query for server state, React context for UI state.

**Routing**: Wouter for client-side routing.

### Database Design

**PostgreSQL (Java Backend)**:
- `transactions` - Transaction records with analysis results
- `decisions` - Rule engine decisions
- `rule_configurations` - Active rules with conditions
- `audit_logs` - Compliance audit trail
- Flyway migrations in `backend/src/main/resources/db/migration/`

**MySQL/TiDB (Node Server via Drizzle)**:
- `users` - OAuth user accounts
- `rules` - Rule configurations (separate from Java)
- `transactionAudits` - Audit records
- `ruleHistory` - Rule change history

### Build and Quality Enforcement

**Java Backend**:
- Java 21 required (Maven Enforcer)
- Strict compilation with `-Werror`
- Spotless code formatting (google-java-format)
- ArchUnit tests for architecture compliance

**Node/Frontend**:
- TypeScript strict mode
- Vitest for testing
- OpenAPI spec generation for type-safe clients

## External Dependencies

### Databases
- **PostgreSQL**: Primary database for Java backend (transactions, rules, audit)
- **MySQL/TiDB**: Used by Node server for user auth and supplementary data

### Authentication
- Manus OAuth integration via Node server
- Session cookies for authenticated requests
- Role-based access (user/admin)

### APIs and Services
- AWS S3 (optional): File storage via `@aws-sdk/client-s3`
- Forge API: Storage proxy for file uploads

### Key Libraries
- **Java**: Spring Boot, Spring Data JPA, Flyway, Jackson
- **Node**: tRPC, Drizzle ORM, Zod validation
- **Frontend**: React Query, Axios, Recharts, Lucide icons

### Configuration
- Environment variables for database URLs, API keys, OAuth settings
- `VITE_API_PROXY_TARGET` controls API proxy target in development
- `DATABASE_URL` for Drizzle/Node database connection