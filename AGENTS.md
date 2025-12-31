# AGENTS.md - RULEX Repository Agent Guide

## Overview
RULEX is a fraud detection rules engine with a React frontend and Spring Boot backend.

## Repository Structure
```
RULEX/
├── client/           # React 19 + Vite frontend
├── backend/          # Spring Boot 3.x backend
├── docs/             # Documentation
│   └── qa/           # QA artifacts and audit logs
├── e2e/              # Playwright E2E tests
├── openapi/          # OpenAPI specifications
└── scripts/          # Utility scripts
```

## Commands

### Setup
```bash
# Install frontend dependencies
cd ~/repos/RULEX && pnpm install --frozen-lockfile

# Install backend dependencies
cd ~/repos/RULEX && mvn -q -f backend/pom.xml dependency:resolve
```

### Development
```bash
# Start full stack (Docker)
cd ~/repos/RULEX && docker compose up -d --build

# Start frontend only (dev mode)
cd ~/repos/RULEX && pnpm dev

# Start backend only
cd ~/repos/RULEX/backend && mvn spring-boot:run
```

### Testing
```bash
# Frontend tests (Vitest)
cd ~/repos/RULEX && pnpm test --run

# Backend tests (JUnit)
cd ~/repos/RULEX && mvn -f backend/pom.xml test

# E2E tests (Playwright)
cd ~/repos/RULEX && pnpm exec playwright test
```

### Linting
```bash
# Frontend lint
cd ~/repos/RULEX && pnpm check

# Backend lint (Spotless)
cd ~/repos/RULEX && mvn -f backend/pom.xml spotless:check

# Fix backend formatting
cd ~/repos/RULEX && mvn -f backend/pom.xml spotless:apply
```

### Build
```bash
# Frontend build
cd ~/repos/RULEX && pnpm build

# Backend build
cd ~/repos/RULEX && mvn -f backend/pom.xml package -DskipTests
```

## Agent Rules

### Git Hygiene
1. **ALWAYS** keep git status clean before finishing work
2. **NEVER** leave uncommitted changes that affect functionality
3. **ALWAYS** run tests before committing
4. Commit messages follow conventional commits format

### Code Changes
1. **NEVER** change payload/API contracts without explicit approval
2. **ALWAYS** update tests when changing functionality
3. **ALWAYS** update documentation when changing behavior
4. Use existing patterns - don't invent new architectures

### Testing Requirements
1. Frontend changes require Vitest tests
2. Backend changes require JUnit tests
3. API changes require integration tests
4. Critical flows require E2E tests

### Documentation
1. Update DEVIN_PROGRESS.md after completing tasks
2. Update DEVIN_EVIDENCE_LOG.md with test outputs
3. Update GAPS_REGISTER.md when closing gaps

## Key Files

### Frontend
- `client/src/lib/javaApi.ts` - API client
- `client/src/components/RuleFormDialog/` - Rule popup
- `client/src/components/ComplexRuleBuilder/` - Advanced rule builder
- `client/src/pages/Rules.tsx` - Rules page

### Backend
- `backend/src/main/java/com/rulex/controller/` - REST controllers
- `backend/src/main/java/com/rulex/service/` - Business logic
- `backend/src/main/java/com/rulex/entity/` - JPA entities
- `backend/src/main/resources/db/migration/` - Flyway migrations

### Configuration
- `docker-compose.yml` - Docker stack
- `backend/src/main/resources/application.properties` - Backend config
- `vite.config.ts` - Frontend config

## Environment Variables
```bash
# Database
POSTGRES_HOST=localhost
POSTGRES_PORT=5432
POSTGRES_DB=rulex
POSTGRES_USER=rulex
POSTGRES_PASSWORD=rulex

# Backend
SPRING_PROFILES_ACTIVE=dev
SERVER_PORT=8080

# Frontend
VITE_API_URL=http://localhost:8080
```

## Credentials (Dev)
- Admin: `admin` / `admin123`
- Analyst: `analyst` / `analyst123`

## Common Issues

### Flyway Migration Fails
```bash
# Check migration status
cd ~/repos/RULEX/backend && mvn flyway:info

# Repair if needed
cd ~/repos/RULEX/backend && mvn flyway:repair
```

### Port Already in Use
```bash
# Kill process on port 8080
lsof -ti:8080 | xargs kill -9

# Kill process on port 5173
lsof -ti:5173 | xargs kill -9
```

### Docker Issues
```bash
# Full reset
cd ~/repos/RULEX && docker compose down -v && docker compose up -d --build
```

## Last Updated
2024-12-31T22:10:00Z
