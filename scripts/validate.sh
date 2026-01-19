#!/bin/bash
# RULEX - Script de Validação Completa
# Uso: ./scripts/validate.sh
# Exit code: 0 = sucesso, 1 = falha P0/P1

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "=============================================="
echo "  RULEX - VALIDAÇÃO COMPLETA"
echo "  $(date)"
echo "=============================================="

FAILED=0

# Função para reportar resultado
report() {
    local name=$1
    local status=$2
    if [ "$status" -eq 0 ]; then
        echo -e "${GREEN}✓ PASS${NC}: $name"
    else
        echo -e "${RED}✗ FAIL${NC}: $name"
        FAILED=1
    fi
}

# 1. Backend Tests
echo ""
echo "=== 1. BACKEND TESTS ==="
cd backend
export JAVA_HOME=${JAVA_HOME:-/usr/lib/jvm/java-21-openjdk-amd64}
mvn -q test -Pcoverage > /tmp/backend-test.log 2>&1
BACKEND_STATUS=$?
report "Backend Tests (JUnit)" $BACKEND_STATUS
if [ $BACKEND_STATUS -ne 0 ]; then
    tail -50 /tmp/backend-test.log
fi
cd ..

# 2. Frontend Tests
echo ""
echo "=== 2. FRONTEND TESTS ==="
pnpm test > /tmp/frontend-test.log 2>&1
FRONTEND_STATUS=$?
report "Frontend Tests (Vitest)" $FRONTEND_STATUS
if [ $FRONTEND_STATUS -ne 0 ]; then
    cat /tmp/frontend-test.log
fi

# 3. TypeCheck
echo ""
echo "=== 3. TYPECHECK ==="
pnpm check > /tmp/typecheck.log 2>&1
TYPECHECK_STATUS=$?
report "TypeCheck (tsc)" $TYPECHECK_STATUS

# 4. Build
echo ""
echo "=== 4. BUILD ==="
pnpm build > /tmp/build.log 2>&1
BUILD_STATUS=$?
report "Build (Vite)" $BUILD_STATUS

# 5. Gitleaks
echo ""
echo "=== 5. SECURITY - GITLEAKS ==="
if command -v gitleaks &> /dev/null; then
    gitleaks detect --source . --no-git --exit-code 1 > /tmp/gitleaks.log 2>&1
    GITLEAKS_STATUS=$?
    report "Gitleaks (secret scan)" $GITLEAKS_STATUS
else
    echo -e "${YELLOW}⚠ SKIP${NC}: Gitleaks not installed"
fi

# 6. Trivy
echo ""
echo "=== 6. SECURITY - TRIVY ==="
if command -v trivy &> /dev/null; then
    trivy fs --severity HIGH,CRITICAL --exit-code 1 . > /tmp/trivy.log 2>&1
    TRIVY_STATUS=$?
    report "Trivy (SCA)" $TRIVY_STATUS
else
    echo -e "${YELLOW}⚠ SKIP${NC}: Trivy not installed"
fi

# 7. E2E Tests (requires Docker)
echo ""
echo "=== 7. E2E TESTS ==="
if command -v docker &> /dev/null && docker info > /dev/null 2>&1; then
    echo "Starting Docker Compose stack..."
    cp .env.example .env 2>/dev/null || true
    docker compose up -d --build > /tmp/docker-up.log 2>&1
    
    echo "Waiting for backend health..."
    HEALTH_URL="http://localhost:8080/api/actuator/health"
    TIMEOUT=120
    START=$(date +%s)
    while true; do
        if curl -fsS "$HEALTH_URL" > /dev/null 2>&1; then
            echo "Backend is ready"
            break
        fi
        NOW=$(date +%s)
        if [ $((NOW - START)) -ge $TIMEOUT ]; then
            echo "Timeout waiting for backend"
            docker compose logs backend postgres
            docker compose down -v
            rm -f .env
            report "E2E Tests (Playwright)" 1
            break
        fi
        sleep 2
    done
    
    if curl -fsS "$HEALTH_URL" > /dev/null 2>&1; then
        export PLAYWRIGHT_BASE_URL=http://localhost:5173
        # GAP-B FIX: Derivar credenciais E2E do .env para consistência
        export E2E_USERNAME="${RULEX_ADMIN_USERNAME:-admin}"
        export E2E_PASSWORD="${RULEX_ADMIN_PASSWORD:-rulex}"
        echo "E2E usando credenciais: $E2E_USERNAME / [MASKED]"
        pnpm exec playwright test > /tmp/e2e.log 2>&1
        E2E_STATUS=$?
        report "E2E Tests (Playwright)" $E2E_STATUS
        if [ $E2E_STATUS -ne 0 ]; then
            cat /tmp/e2e.log
        fi
    fi
    
    echo "Shutting down Docker Compose..."
    docker compose down -v > /dev/null 2>&1
    rm -f .env
else
    echo -e "${YELLOW}⚠ SKIP${NC}: Docker not available"
fi

# Summary
echo ""
echo "=============================================="
echo "  SUMMARY"
echo "=============================================="
if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}✓ ALL CHECKS PASSED${NC}"
    exit 0
else
    echo -e "${RED}✗ SOME CHECKS FAILED${NC}"
    exit 1
fi
