#!/bin/bash
# =============================================================================
# GAP 0 Readiness Check Script
# Validates environment, dependencies, and operator coverage for production
# =============================================================================

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "=========================================="
echo "  GAP 0 Readiness Check - RULEX"
echo "=========================================="
echo ""

# Load .env file if specified
if [ -n "$ENV_FILE" ] && [ -f "$ENV_FILE" ]; then
    echo -e "${YELLOW}Loading environment from $ENV_FILE${NC}"
    export $(grep -v '^#' "$ENV_FILE" | xargs)
fi

ERRORS=0
WARNINGS=0

# Function to check command exists
check_command() {
    if command -v "$1" &> /dev/null; then
        echo -e "${GREEN}✓${NC} $1 is installed"
        return 0
    else
        echo -e "${RED}✗${NC} $1 is NOT installed"
        return 1
    fi
}

# Function to check environment variable
check_env_var() {
    local var_name=$1
    local required=$2
    if [ -n "${!var_name}" ]; then
        echo -e "${GREEN}✓${NC} $var_name is set"
        return 0
    else
        if [ "$required" = "required" ]; then
            echo -e "${RED}✗${NC} $var_name is NOT set (required)"
            return 1
        else
            echo -e "${YELLOW}⚠${NC} $var_name is NOT set (optional)"
            return 0
        fi
    fi
}

echo "1. Checking required tools..."
echo "-------------------------------------------"
check_command "docker" || ((ERRORS++))
check_command "docker-compose" || check_command "docker" || ((ERRORS++))
check_command "java" || ((ERRORS++))
check_command "mvn" || ((ERRORS++))
check_command "node" || ((ERRORS++))
check_command "pnpm" || ((ERRORS++))
echo ""

echo "2. Checking Docker availability..."
echo "-------------------------------------------"
if docker info &> /dev/null; then
    echo -e "${GREEN}✓${NC} Docker daemon is running"
else
    echo -e "${RED}✗${NC} Docker daemon is NOT running"
    ((ERRORS++))
fi
echo ""

echo "3. Checking environment variables..."
echo "-------------------------------------------"
check_env_var "DATABASE_URL" "optional" || ((WARNINGS++))
check_env_var "REDIS_URL" "optional" || ((WARNINGS++))
check_env_var "NEO4J_URI" "optional" || ((WARNINGS++))
check_env_var "JWT_SECRET" "optional" || ((WARNINGS++))
echo ""

echo "4. Checking StubOperatorEvaluator..."
echo "-------------------------------------------"
STUB_FILE="backend/src/main/java/com/rulex/service/complex/evaluator/StubOperatorEvaluator.java"
if [ -f "$STUB_FILE" ]; then
    # Check if PLANNED_OPERATORS set is empty (contains only comments or whitespace)
    STUB_CONTENT=$(grep -A 5 "PLANNED_OPERATORS = Set.of" "$STUB_FILE" | grep -v "//" | grep "ConditionOperator\." || true)
    if [ -z "$STUB_CONTENT" ]; then
        echo -e "${GREEN}✓${NC} StubOperatorEvaluator is EMPTY - all operators implemented!"
    else
        STUB_COUNT=$(echo "$STUB_CONTENT" | wc -l)
        echo -e "${RED}✗${NC} StubOperatorEvaluator has $STUB_COUNT STUB operators"
        ((ERRORS++))
    fi
else
    echo -e "${YELLOW}⚠${NC} StubOperatorEvaluator.java not found"
    ((WARNINGS++))
fi
echo ""

echo "5. Checking operator evaluators..."
echo "-------------------------------------------"
EVALUATOR_DIR="backend/src/main/java/com/rulex/service/complex/evaluator"
if [ -d "$EVALUATOR_DIR" ]; then
    EVALUATOR_COUNT=$(ls -1 "$EVALUATOR_DIR"/*.java 2>/dev/null | wc -l)
    echo -e "${GREEN}✓${NC} Found $EVALUATOR_COUNT operator evaluator files"

    # List evaluators
    for f in "$EVALUATOR_DIR"/*Evaluator.java; do
        if [ -f "$f" ]; then
            name=$(basename "$f" .java)
            op_count=$(grep -c "ConditionOperator\." "$f" 2>/dev/null || echo "0")
            echo "   - $name: ~$op_count operator references"
        fi
    done
else
    echo -e "${RED}✗${NC} Evaluator directory not found"
    ((ERRORS++))
fi
echo ""

echo "6. Checking database migrations..."
echo "-------------------------------------------"
MIGRATION_DIR="backend/src/main/resources/db/migration"
if [ -d "$MIGRATION_DIR" ]; then
    MIGRATION_COUNT=$(ls -1 "$MIGRATION_DIR"/V*.sql 2>/dev/null | wc -l)
    echo -e "${GREEN}✓${NC} Found $MIGRATION_COUNT Flyway migrations"

    # Check for latest migration
    LATEST=$(ls -1 "$MIGRATION_DIR"/V*.sql 2>/dev/null | sort -V | tail -1)
    if [ -n "$LATEST" ]; then
        echo "   Latest: $(basename "$LATEST")"
    fi
else
    echo -e "${RED}✗${NC} Migration directory not found"
    ((ERRORS++))
fi
echo ""

echo "7. Checking frontend build..."
echo "-------------------------------------------"
if [ -f "package.json" ]; then
    echo -e "${GREEN}✓${NC} package.json found"
    if [ -d "node_modules" ]; then
        echo -e "${GREEN}✓${NC} node_modules exists"
    else
        echo -e "${YELLOW}⚠${NC} node_modules not found - run 'pnpm install'"
        ((WARNINGS++))
    fi
else
    echo -e "${RED}✗${NC} package.json not found"
    ((ERRORS++))
fi
echo ""

echo "8. Checking backend build..."
echo "-------------------------------------------"
if [ -f "backend/pom.xml" ]; then
    echo -e "${GREEN}✓${NC} backend/pom.xml found"
    if [ -d "backend/target" ]; then
        echo -e "${GREEN}✓${NC} backend/target exists (compiled)"
    else
        echo -e "${YELLOW}⚠${NC} backend/target not found - run 'mvn compile'"
        ((WARNINGS++))
    fi
else
    echo -e "${RED}✗${NC} backend/pom.xml not found"
    ((ERRORS++))
fi
echo ""

echo "=========================================="
echo "  SUMMARY"
echo "=========================================="
if [ $ERRORS -eq 0 ] && [ $WARNINGS -eq 0 ]; then
    echo -e "${GREEN}✓ ALL CHECKS PASSED - Ready for GAP 0!${NC}"
    exit 0
elif [ $ERRORS -eq 0 ]; then
    echo -e "${YELLOW}⚠ $WARNINGS warnings found - Review before proceeding${NC}"
    exit 0
else
    echo -e "${RED}✗ $ERRORS errors and $WARNINGS warnings found${NC}"
    echo -e "${RED}  Fix errors before proceeding to production${NC}"
    exit 1
fi
