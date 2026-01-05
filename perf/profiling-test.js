// Script k6 para profiling e análise de bottlenecks
// Executa apenas 1 VU com 10 iterações para capturar logs SQL detalhados
// 
// Uso: k6 run --vus 1 --iterations 10 perf/profiling-test.js

import http from 'k6/http';
import { check, sleep } from 'k6';
import { Counter, Trend } from 'k6/metrics';

// Métricas customizadas
const evaluateLatency = new Trend('evaluate_latency', true);
const errors = new Counter('errors');

// Configuração
const BASE_URL = __ENV.BASE_URL || 'http://localhost:8080/api';
const USERNAME = __ENV.USERNAME || 'admin';
const PASSWORD = __ENV.PASSWORD || 'rulex';

// Payload de transação para análise
const transactionPayload = JSON.stringify({
  externalId: `prof-${Date.now()}-${__VU}-${__ITER}`,
  amount: 1500.00,
  currency: 'BRL',
  merchantId: 'MERCH-PROF-001',
  merchantName: 'Profiling Test Merchant',
  merchantCategory: '5411',
  cardBin: '123456',
  cardLast4: '7890',
  customerId: `CUST-PROF-${__VU}`,
  timestamp: new Date().toISOString(),
  channel: 'WEB',
  deviceFingerprint: `dev-prof-${__VU}`,
  ipAddress: '192.168.1.100',
  location: {
    country: 'BR',
    city: 'São Paulo',
    latitude: -23.5505,
    longitude: -46.6333
  }
});

// Setup: verificar se backend está respondendo
export function setup() {
  console.log('🔍 Iniciando teste de profiling');
  console.log(`   Base URL: ${BASE_URL}`);
  console.log(`   VUs: 1, Iterations: 10`);
  
  const healthRes = http.get(`${BASE_URL}/../actuator/health`);
  if (healthRes.status !== 200) {
    throw new Error(`Backend não está healthy: ${healthRes.status}`);
  }
  console.log('✅ Backend está respondendo');
}

// Cenário principal
export default function () {
  const params = {
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Basic ${__ENV.AUTH || btoa(`${USERNAME}:${PASSWORD}`)}`,
    },
    timeout: '10s', // Timeout maior para profiling
  };

  const startTime = Date.now();
  
  const response = http.post(
    `${BASE_URL}/transactions/analyze`,
    transactionPayload,
    params
  );
  
  const endTime = Date.now();
  const latency = endTime - startTime;
  
  // Registrar latência
  evaluateLatency.add(latency);
  
  // Checks
  const checksOk = check(response, {
    'status is 200': (r) => r.status === 200,
    'response has classification': (r) => {
      try {
        const body = JSON.parse(r.body);
        return body.classification !== undefined;
      } catch (e) {
        return false;
      }
    },
  });
  
  if (!checksOk) {
    errors.add(1);
    console.warn(`⚠️  Request failed: ${response.status} - ${response.body?.substring(0, 200)}`);
  } else {
    console.log(`✓ Iteration ${__ITER + 1}: ${latency}ms`);
  }
  
  // Parse response para análise
  if (response.status === 200) {
    try {
      const data = JSON.parse(response.body);
      console.log(`   Classification: ${data.classification}`);
      console.log(`   Score: ${data.score}`);
      console.log(`   Rules triggered: ${data.triggeredRules?.length || 0}`);
    } catch (e) {
      console.error('Failed to parse response:', e);
    }
  }
  
  // Pequeno delay entre iterações para não sobrecarregar
  sleep(0.5);
}

// Teardown
export function teardown(data) {
  console.log('\n📊 Profiling concluído.');
  console.log('   Analise os logs SQL gerados pelo backend.');
}

// Thresholds (mais relaxados para profiling)
export const options = {
  thresholds: {
    'http_req_duration': ['p(95)<5000'], // 5s
    'http_req_failed': ['rate<0.1'],     // 10%
    'errors': ['rate<0.1'],              // 10%
  },
};
