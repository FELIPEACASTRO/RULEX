#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

function header() {
  printf "\n=== %s ===\n" "$1"
}

header "Environment checks"
REQUIRED_ENV=(POSTGRES_USER POSTGRES_PASSWORD NEO4J_PASSWORD RULEX_ADMIN_USERNAME RULEX_ADMIN_PASSWORD RULEX_ANALYST_USERNAME RULEX_ANALYST_PASSWORD VITE_API_BASIC_AUTH)
missing=0
for var in "${REQUIRED_ENV[@]}"; do
  if [[ -z "${!var:-}" ]]; then
    echo "MISSING: ${var}"
    missing=$((missing+1))
  else
    echo "OK: ${var}"
  fi
done

header "Docker availability"
if command -v docker >/dev/null 2>&1; then
  docker --version
else
  echo "docker not available"
fi

header "Operator coverage audit"
"$ROOT_DIR/scripts/validate-operator-coverage.sh"

header "Completed"
if [[ $missing -gt 0 ]]; then
  echo "Missing required env vars: $missing"
  exit 2
fi
