/**
 * 🔥 RuleX - Testes de Performance com k6
 * 
 * Execução:
 *   k6 run tests/performance/load-test.js
 *   k6 run --vus 100 --duration 5m tests/performance/load-test.js
 */

import http from 'k6/http';
import { check, sleep, group } from 'k6';
import { Rate, Trend, Counter } from 'k6/metrics';

// ========== CONFIGURAÇÃO ==========
const BASE_URL = __ENV.BASE_URL || 'http://localhost:5000';

// Métricas customizadas
const errorRate = new Rate('errors');
const evaluateLatency = new Trend('evaluate_latency');
const rulesLatency = new Trend('rules_latency');
const transactionsLatency = new Trend('transactions_latency');
const successfulEvaluations = new Counter('successful_evaluations');

// Configuração de carga
export const options = {
  stages: [
    { duration: '30s', target: 10 },   // Ramp-up
    { duration: '1m', target: 50 },    // Carga média
    { duration: '2m', target: 100 },   // Carga alta
    { duration: '1m', target: 200 },   // Pico
    { duration: '30s', target: 0 },    // Ramp-down
  ],
  thresholds: {
    'http_req_duration': ['p(95)<500', 'p(99)<1000'],
    'errors': ['rate<0.01'],
    'evaluate_latency': ['p(95)<200'],
    'rules_latency': ['p(95)<300'],
  },
};

// ========== DADOS DE TESTE ==========
function generateTransaction() {
  const mccs = ['5411', '5812', '7995', '5999', '4111'];
  const countries = ['BR', 'US', 'NG', 'GB', 'DE'];
  const amounts = [100, 500, 1000, 5000, 10000, 50000];
  
  return {
    transactionId: `TXN_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`,
    amount: amounts[Math.floor(Math.random() * amounts.length)],
    currency: 'BRL',
    cardNumber: '4111111111111111',
    merchantId: `MERCHANT_${Math.floor(Math.random() * 1000)}`,
    merchantName: 'Test Merchant',
    merchantCity: 'São Paulo',
    merchantCountryCode: countries[Math.floor(Math.random() * countries.length)],
    mcc: mccs[Math.floor(Math.random() * mccs.length)],
    customerId: `CUST_${Math.floor(Math.random() * 10000)}`,
    deviceId: `DEVICE_${Math.floor(Math.random() * 1000)}`,
    ipAddress: `192.168.${Math.floor(Math.random() * 255)}.${Math.floor(Math.random() * 255)}`,
    timestamp: new Date().toISOString(),
  };
}

// ========== CENÁRIOS DE TESTE ==========
export default function() {
  
  group('🔥 Avaliação de Transações', function() {
    const transaction = generateTransaction();
    const payload = JSON.stringify(transaction);
    
    const params = {
      headers: {
        'Content-Type': 'application/json',
      },
    };
    
    const startTime = Date.now();
    const response = http.post(`${BASE_URL}/api/evaluate`, payload, params);
    const duration = Date.now() - startTime;
    
    evaluateLatency.add(duration);
    
    const success = check(response, {
      'status is 200': (r) => r.status === 200,
      'response has decision': (r) => {
        try {
          const body = JSON.parse(r.body);
          return body.decision !== undefined;
        } catch {
          return false;
        }
      },
      'latency < 500ms': () => duration < 500,
    });
    
    if (success) {
      successfulEvaluations.add(1);
    }
    errorRate.add(!success);
  });
  
  group('📋 Listagem de Regras', function() {
    const startTime = Date.now();
    const response = http.get(`${BASE_URL}/api/rules`);
    const duration = Date.now() - startTime;
    
    rulesLatency.add(duration);
    
    const success = check(response, {
      'status is 200': (r) => r.status === 200,
      'response is array': (r) => {
        try {
          const body = JSON.parse(r.body);
          return Array.isArray(body) || body.content !== undefined;
        } catch {
          return false;
        }
      },
    });
    
    errorRate.add(!success);
  });
  
  group('📊 Listagem de Transações', function() {
    const startTime = Date.now();
    const response = http.get(`${BASE_URL}/api/transactions?page=0&size=20`);
    const duration = Date.now() - startTime;
    
    transactionsLatency.add(duration);
    
    const success = check(response, {
      'status is 200': (r) => r.status === 200,
    });
    
    errorRate.add(!success);
  });
  
  group('📈 Métricas', function() {
    const response = http.get(`${BASE_URL}/api/metrics`);
    
    check(response, {
      'status is 200': (r) => r.status === 200,
    });
  });
  
  sleep(Math.random() * 2 + 1); // 1-3 segundos entre iterações
}

// ========== RELATÓRIO FINAL ==========
export function handleSummary(data) {
  return {
    'tests/performance/results/summary.json': JSON.stringify(data, null, 2),
    'stdout': textSummary(data, { indent: '  ', enableColors: true }),
  };
}

function textSummary(data, options) {
  const { metrics } = data;
  
  let summary = `
╔══════════════════════════════════════════════════════════════════════════════╗
║  🔥 RuleX - Relatório de Performance                                        ║
╚══════════════════════════════════════════════════════════════════════════════╝

📊 MÉTRICAS PRINCIPAIS
═══════════════════════════════════════════════════════════════════════════════

  HTTP Requests:
    Total: ${metrics.http_reqs?.values?.count || 0}
    Rate: ${(metrics.http_reqs?.values?.rate || 0).toFixed(2)}/s

  Latência (http_req_duration):
    Média: ${(metrics.http_req_duration?.values?.avg || 0).toFixed(2)}ms
    P95: ${(metrics.http_req_duration?.values?.['p(95)'] || 0).toFixed(2)}ms
    P99: ${(metrics.http_req_duration?.values?.['p(99)'] || 0).toFixed(2)}ms
    Max: ${(metrics.http_req_duration?.values?.max || 0).toFixed(2)}ms

  Avaliação de Transações:
    Latência P95: ${(metrics.evaluate_latency?.values?.['p(95)'] || 0).toFixed(2)}ms
    Sucessos: ${metrics.successful_evaluations?.values?.count || 0}

  Taxa de Erros: ${((metrics.errors?.values?.rate || 0) * 100).toFixed(2)}%

═══════════════════════════════════════════════════════════════════════════════
`;
  
  return summary;
}
