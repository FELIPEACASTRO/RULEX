# RULEX - Antifraud Rules Engine

## Overview

RULEX is a banking antifraud hard rules engine for credit transaction analysis. The system evaluates transactions against configurable rules to classify them as APPROVED, SUSPICIOUS, or FRAUD. It features a Java Spring Boot backend for rule processing and a React/Vite frontend for management and monitoring.

**Core Purpose**: Real-time credit transaction fraud detection using deterministic rule evaluation with full audit trail.

## User Preferences

Preferred communication style: Simple, everyday language.

## System Architecture

### Backend (Java Spring Boot)
- **Location**: `backend/` directory
- **Runtime**: Java 21 with virtual threads enabled
- **Framework**: Spring Boot with REST controllers
- **Build**: Maven with strict compilation (`-Werror`, `-Xlint:all`)
- **Architecture Pattern**: Hexagonal/Clean Architecture with ports and adapters
  - Use cases focus on business flow/rules
  - Adapters handle external integrations
  - Ports define small, specific interfaces for dependency inversion

### Frontend (React/Vite)
- **Location**: `client/` directory
- **Framework**: React 19 with TypeScript
- **Styling**: Tailwind CSS with shadcn/ui components
- **State Management**: TanStack Query for server state
- **Routing**: Wouter (lightweight router)
- **Build Tool**: Vite with proxy to backend at `/api`

### API Communication
- Frontend proxies `/api` requests to Java backend (default: `http://localhost:8080`)
- OpenAPI specification at `openapi/rulex.yaml` with TypeScript type generation
- REST endpoints for transactions, rules, audit, and metrics

### Key Design Decisions
1. **SOLID Principles**: Strict adherence with small interfaces, dependency injection via ports
2. **Idempotent Transaction Analysis**: Same transaction ID returns cached result
3. **Audit Trail**: All actions logged with actor, timestamp, and result
4. **Brazilian Portuguese**: Error messages and logs in PT-BR
5. **No Sensitive Data Leakage**: Error responses sanitized before client return

### Testing Strategy
- **Integration Tests**: Testcontainers with Postgres for full flow validation
- **Unit Tests**: Stubs/mocks for use cases without Spring context
- **Frontend Tests**: Vitest with React Testing Library

## External Dependencies

### Database
- **Primary**: PostgreSQL (via Docker Compose or Testcontainers)
- **Drizzle Config**: Present but configured for MySQL dialect (may need adjustment for Postgres)
- **Connection**: `DATABASE_URL` environment variable required

### Authentication
- JWT-based authentication with `JWT_SECRET` required in production
- Optional OAuth integration via `OAUTH_SERVER_URL` and `VITE_OAUTH_PORTAL_URL`
- Mock auth available for development when OAuth not configured

### Required Environment Variables
- `DATABASE_URL`: Database connection string
- `JWT_SECRET`: Required in production for authentication
- `VITE_API_PROXY_TARGET`: Backend URL for frontend proxy (default: `http://localhost:8080`)
- `VITE_JAVA_API_URL`: Java API base URL for client-side calls

### Development Setup
- Docker Compose for local environment (Postgres, backend, frontend)
- Backend: `mvn -q clean test` in `backend/` directory
- Frontend: `pnpm install && pnpm dev`
- Web UI: http://localhost:5173 (proxies to backend)
- Backend API: http://localhost:8080