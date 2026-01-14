/**
 * 🔥 RuleX - Teste de Stress
 * 
 * Objetivo: Encontrar o ponto de ruptura do sistema
 * 
 * Execução:
 *   k6 run tests/performance/stress-test.js
 */

import http from 'k6/http';
import { check, sleep } from 'k6';
import { Rate, Trend } from 'k6/metrics';

const BASE_URL = __ENV.BASE_URL || 'http://localhost:5000';

const errorRate = new Rate('errors');
const responseTime = new Trend('response_time');

// Configuração de stress - aumenta até quebrar
export const options = {
  stages: [
    { duration: '2m', target: 100 },   // Aquecimento
    { duration: '5m', target: 100 },   // Carga estável
    { duration: '2m', target: 200 },   // Aumento
    { duration: '5m', target: 200 },   // Carga alta
    { duration: '2m', target: 300 },   // Stress
    { duration: '5m', target: 300 },   // Stress sustentado
    { duration: '2m', target: 400 },   // Ponto de ruptura
    { duration: '5m', target: 400 },   // Verificar estabilidade
    { duration: '10m', target: 0 },    // Recuperação
  ],
  thresholds: {
    'http_req_duration': ['p(99)<2000'],
    'errors': ['rate<0.1'],
  },
};

function generateTransaction() {
  return {
    transactionId: `STRESS_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`,
    amount: Math.floor(Math.random() * 100000),
    currency: 'BRL',
    cardNumber: '4111111111111111',
    merchantId: `MERCHANT_${Math.floor(Math.random() * 1000)}`,
    merchantName: 'Stress Test Merchant',
    merchantCity: 'São Paulo',
    merchantCountryCode: ['BR', 'US', 'NG'][Math.floor(Math.random() * 3)],
    mcc: ['5411', '7995', '5812'][Math.floor(Math.random() * 3)],
    customerId: `CUST_${Math.floor(Math.random() * 10000)}`,
    deviceId: `DEVICE_${Math.floor(Math.random() * 1000)}`,
    ipAddress: `10.0.${Math.floor(Math.random() * 255)}.${Math.floor(Math.random() * 255)}`,
    timestamp: new Date().toISOString(),
  };
}

export default function() {
  const transaction = generateTransaction();
  
  const params = {
    headers: { 'Content-Type': 'application/json' },
    timeout: '30s',
  };
  
  const startTime = Date.now();
  const response = http.post(
    `${BASE_URL}/api/evaluate`,
    JSON.stringify(transaction),
    params
  );
  const duration = Date.now() - startTime;
  
  responseTime.add(duration);
  
  const success = check(response, {
    'status is 200': (r) => r.status === 200,
    'response time < 2s': () => duration < 2000,
  });
  
  errorRate.add(!success);
  
  sleep(0.1); // Mínimo delay entre requests
}
