#!/bin/bash
# GAP-E: Script para exportar OpenAPI spec do backend
# Uso: ./scripts/export-openapi.sh
# Requer: backend rodando em localhost:8080

set -e

OPENAPI_URL="http://localhost:8080/api/v3/api-docs"
OUTPUT_FILE="openapi/rulex-generated.yaml"

echo "🔍 Verificando se backend está rodando..."
if ! curl -fsS "http://localhost:8080/api/actuator/health" > /dev/null 2>&1; then
    echo "❌ Backend não está rodando em localhost:8080"
    echo "   Execute: docker compose up -d"
    exit 1
fi

echo "📥 Exportando OpenAPI spec de $OPENAPI_URL..."
curl -fsS "$OPENAPI_URL.yaml" -o "$OUTPUT_FILE"

if [ -f "$OUTPUT_FILE" ]; then
    echo "✅ OpenAPI exportado para $OUTPUT_FILE"
    echo "   Endpoints documentados: $(grep -c '^\s\+/' "$OUTPUT_FILE" || echo 0)"
else
    echo "❌ Falha ao exportar OpenAPI"
    exit 1
fi

echo ""
echo "📝 Para regenerar o client TypeScript:"
echo "   pnpm exec openapi-typescript openapi/rulex-generated.yaml -o client/src/lib/api.generated.ts"
