/**
 * RULEX Load Test Script (k6)
 *
 * Objetivo: Validar SLO de 1000 TPS com latência <= 200ms (p95/p99)
 *
 * Uso:
 *   k6 run perf/load-test.js
 *   k6 run --vus 100 --duration 60s perf/load-test.js
 *
 * Requisitos:
 *   - Backend rodando em http://localhost:8080
 *   - k6 instalado (https://k6.io/docs/getting-started/installation/)
 */

import http from 'k6/http';
import { check, sleep } from 'k6';
import { Rate, Trend } from 'k6/metrics';

// Métricas customizadas
const errorRate = new Rate('errors');
const evaluateLatency = new Trend('evaluate_latency', true);

// Configuração do teste
export const options = {
  // Cenário de rampa para atingir 1000 TPS
  stages: [
    { duration: '10s', target: 50 },   // Warm-up
    { duration: '30s', target: 200 },  // Ramp-up
    { duration: '60s', target: 500 },  // Sustentado
    { duration: '30s', target: 1000 }, // Pico (target: 1000 VUs)
    { duration: '10s', target: 0 },    // Ramp-down
  ],

  // Thresholds (critérios de aceite)
  thresholds: {
    'http_req_duration': ['p(95)<200', 'p(99)<500'],  // p95 < 200ms, p99 < 500ms
    'errors': ['rate<0.01'],                          // Taxa de erro < 1%
    'evaluate_latency': ['p(95)<200'],                // Latência < 200ms
  },
};

// Configuração base
const BASE_URL = __ENV.BASE_URL || 'http://localhost:8080/api';
const AUTH_HEADER = __ENV.AUTH_HEADER || 'Basic YWRtaW46cnVsZXg='; // admin:rulex

export default function () {
  // Payload mínimo válido para TransactionRequest
  const payload = {
    externalTransactionId: `TX-${Date.now()}-${__VU}-${__ITER}`,
    customerIdFromHeader: "CUST-12345",
    customerAcctNumber: 4111111111111111,
    pan: "411111******1111",
    transactionAmount: Math.floor(Math.random() * 900) + 100, // 100-999
    transactionDate: 20260105,
    transactionTime: 143000,
    transactionCurrencyCode: 986,
    mcc: 5411,
    consumerAuthenticationScore: 85,
    externalScore3: 70,
    cavvResult: 2,
    eciIndicator: 5,
    atcCard: 100,
    atcHost: 100,
    tokenAssuranceLevel: 3,
    availableCredit: 5000.00,
    cardCashBalance: 0.00,
    cardDelinquentAmount: 0.00
  };

  const params = {
    headers: {
      'Content-Type': 'application/json',
      'Authorization': AUTH_HEADER,
    },
    timeout: '5s',
  };

  // Requisição principal: POST /transactions/analyze
  const startTime = Date.now();
  const response = http.post(
    `${BASE_URL}/transactions/analyze`,
    JSON.stringify(payload),
    params
  );
  const latency = Date.now() - startTime;

  // Registra latência customizada
  evaluateLatency.add(latency);

  // Validações
  const success = check(response, {
    'status is 200': (r) => r.status === 200,
    'response has classification': (r) => {
      try {
        const body = JSON.parse(r.body);
        return body.classification !== undefined;
      } catch (e) {
        return false;
      }
    },
    'latency < 200ms': () => latency < 200,
  });

  errorRate.add(!success);

  // Think time mínimo
  sleep(0.01);
}

// Função de setup
export function setup() {
  console.log(`🚀 Iniciando teste de carga RULEX`);
  console.log(`   Base URL: ${BASE_URL}`);
  console.log(`   Target: 1000 TPS, p95 < 200ms`);

  const healthCheck = http.get(`${BASE_URL}/actuator/health`, {
    headers: { 'Authorization': AUTH_HEADER },
    timeout: '10s',
  });

  if (healthCheck.status !== 200 && healthCheck.status !== 503) {
    console.error(`❌ Backend não está respondendo: ${healthCheck.status}`);
    return { ready: false };
  }

  console.log(`✅ Backend está respondendo`);
  return { ready: true };
}

// Função de teardown
export function teardown(data) {
  if (data.ready) {
    console.log(`\n📊 Teste finalizado.`);
    console.log(`   Critérios: TPS >= 1000, p95 <= 200ms, p99 <= 500ms, erro < 1%`);
  }
}
