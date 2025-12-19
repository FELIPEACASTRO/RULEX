# RULEX - Fraud Detection Rules Engine

## Overview

RULEX is a credit card transaction fraud detection platform implementing a configurable hard rules engine. The system evaluates transactions in real-time, classifying them as APPROVED, SUSPICIOUS, or FRAUD based on deterministic rules without machine learning.

**Core Purpose**: Banking-grade anti-fraud system with audit trails, rule versioning, and compliance-ready architecture.

**Tech Stack**:
- Backend: Java 21 + Spring Boot 3.2.1 + PostgreSQL
- Frontend: React 19 + TypeScript + Tailwind CSS + shadcn/ui
- Database: PostgreSQL

## User Preferences

Preferred communication style: Simple, everyday language.

## System Architecture

### Design
- **Frontend**: Static React SPA served by Vite (development) or static hosting (production)
- **Backend**: Java Spring Boot REST API (core fraud detection engine)
- **Database**: PostgreSQL for transactions, rules, and audit logs

The frontend proxies `/api` requests to the Java backend at `http://localhost:8080` during development.

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

### Database Design (PostgreSQL)

- `transactions` - Transaction records with analysis results
- `decisions` - Rule engine decisions
- `rule_configurations` - Active rules with conditions
- `audit_logs` - Compliance audit trail
- Flyway migrations in `backend/src/main/resources/db/migration/`

### Build and Quality Enforcement

**Java Backend**:
- Java 21 required (Maven Enforcer)
- Strict compilation with `-Werror`
- Spotless code formatting (google-java-format)
- ArchUnit tests for architecture compliance

**Frontend**:
- TypeScript strict mode
- Vitest for testing
- OpenAPI spec generation for type-safe clients

## Scripts

- `npm run dev` - Start Vite dev server on port 5000
- `npm run build` - Build frontend for production
- `npm run test` - Run Vitest tests

## Deployment

- **Frontend**: Static deployment (dist/public)
- **Backend**: Java Spring Boot (separate deployment)
- API proxy configured via `VITE_API_PROXY_TARGET` environment variable
