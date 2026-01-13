# RULEX Performance Tests

## Requisitos

- [k6](https://k6.io/docs/getting-started/installation/) instalado
- Backend RULEX rodando em `http://localhost:8080`
- Postgres e Redis disponíveis (via docker-compose)

## Instalação do k6

```bash
# Ubuntu/Debian
sudo gpg -k
sudo gpg --no-default-keyring --keyring /usr/share/keyrings/k6-archive-keyring.gpg --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys C5AD17C747E3415A3642D57D77C6C491D6AC1D69
echo "deb [signed-by=/usr/share/keyrings/k6-archive-keyring.gpg] https://dl.k6.io/deb stable main" | sudo tee /etc/apt/sources.list.d/k6.list
sudo apt-get update
sudo apt-get install k6

# macOS
brew install k6

# Docker
docker run --rm -i grafana/k6 run - <perf/load-test.js
```

## Executar Teste de Carga

### 1. Subir a infraestrutura

```bash
cd ~/repos/RULEX
docker compose up -d
```

### 2. Executar teste básico

```bash
k6 run perf/load-test.js
```

### 3. Executar com parâmetros customizados

```bash
# Teste rápido (smoke test)
k6 run --vus 10 --duration 30s perf/load-test.js

# Teste de carga (target: 1000 TPS)
k6 run perf/load-test.js

# Teste com URL customizada
k6 run -e BASE_URL=http://localhost:8080 perf/load-test.js

# Teste com autenticação customizada
k6 run -e AUTH_HEADER="Basic YWRtaW46cnVsZXg=" perf/load-test.js
```

## SLO (Service Level Objectives)

| Métrica | Target | Descrição |
|---------|--------|-----------|
| TPS | >= 1000 | Transações por segundo |
| p95 Latência | <= 200ms | 95% das requisições |
| p99 Latência | <= 500ms | 99% das requisições |
| Taxa de Erro | < 1% | Requisições com erro |

## Interpretando Resultados

```
✓ http_req_duration..............: avg=45ms  min=10ms  med=40ms  max=500ms  p(90)=80ms  p(95)=120ms
✓ errors.........................: 0.50%  ✓ 50       ✗ 9950
✓ evaluate_latency...............: avg=42ms  min=8ms   med=38ms  max=480ms  p(90)=75ms  p(95)=115ms
```

- **avg**: Latência média
- **p(95)**: 95% das requisições abaixo deste valor
- **p(99)**: 99% das requisições abaixo deste valor
- **errors**: Taxa de erro (deve ser < 1%)

## Troubleshooting

### Backend não responde
```bash
# Verificar logs
docker compose logs backend

# Verificar health
curl http://localhost:8080/actuator/health
```

### Latência alta
1. Verificar conexões com Postgres/Redis
2. Verificar número de regras ativas
3. Verificar logs de GC do Java

### Taxa de erro alta
1. Verificar autenticação
2. Verificar payload (campos obrigatórios)
3. Verificar logs do backend
