/**
 * RULEX - Serviço de API para comunicação com Backend Java
 * 
 * Este módulo fornece funções para comunicação com a API REST Java
 * que processa transações de crédito e gerencia regras de fraude.
 * 
 * @author Manus AI
 * @version 1.0
 */

import { getAccessToken } from "@/_core/auth/tokens";

// ========================================
// CONFIGURAÇÃO
// ========================================

// URL base da API Java - configurável via variável de ambiente
const JAVA_API_BASE_URL = import.meta.env.VITE_JAVA_API_URL || 'http://localhost:8080';

// ========================================
// TIPOS E INTERFACES
// ========================================

// Requisição de Transação (103 parâmetros do payload)
export interface TransactionRequest {
  externalTransactionId?: string;
  transactionAmount?: number;
  transactionDate?: string;
  transactionTime?: string;
  mcc?: string;
  merchantCountryCode?: string;
  merchantId?: string;
  merchantName?: string;
  merchantPostalCode?: string;
  customerIdFromHeader?: string;
  customerPresent?: string;
  consumerAuthenticationScore?: number;
  externalScore3?: number;
  cvv2Response?: string;
  cvv2EntryLimitExceeded?: boolean;
  pinEntryLimitExceeded?: boolean;
  cryptogramValid?: boolean;
  cavvResult?: string;
  eciIndicator?: number;
  posSecurity?: number;
  posOffPremises?: number;
  posEntryMode?: string;
  cardAipStatic?: number;
  cardAipDynamic?: number;
  terminalVerificationResults?: string;
  cardExpireDate?: string;
  cardCaptured?: boolean;
  recurringTransaction?: boolean;
  pan?: string;
  panSequenceNumber?: string;
  acquirerId?: string;
  acquirerCountryCode?: string;
  gmtOffset?: string;
  transactionCurrencyCode?: string;
  billingCurrencyCode?: string;
  conversionRate?: number;
  authorizationIdResponse?: string;
  responseCode?: string;
  additionalResponseData?: string;
  // Campos customizados (custom1-20)
  custom1?: string;
  custom2?: string;
  custom3?: string;
  custom4?: string;
  custom5?: string;
  custom6?: string;
  custom7?: string;
  custom8?: string;
  custom9?: string;
  custom10?: string;
  custom11?: string;
  custom12?: string;
  custom13?: string;
  custom14?: string;
  custom15?: string;
  custom16?: string;
  custom17?: string;
  custom18?: string;
  custom19?: string;
  custom20?: string;
}

// Resposta de Análise de Transação
export interface TransactionResponse {
  transactionId: string;
  classification: 'APPROVED' | 'SUSPICIOUS' | 'FRAUD';
  totalScore: number;
  triggeredRules: string[];
  ruleDetails: RuleDetail[];
  reason: string;
  processedAt: string;
}

export interface RuleDetail {
  ruleName: string;
  ruleDescription: string;
  score: number;
  threshold: number;
  triggered: boolean;
}

// Transação Processada (para listagem)
export interface ProcessedTransaction {
  id: number;
  externalTransactionId: string;
  transactionAmount: number;
  transactionDate: string;
  merchantId: string;
  merchantName: string;
  customerId: string;
  classification: 'APPROVED' | 'SUSPICIOUS' | 'FRAUD';
  totalScore: number;
  triggeredRulesCount: number;
  processedAt: string;
}

// Filtros de Transação
export interface TransactionFilters {
  startDate?: string;
  endDate?: string;
  classification?: 'APPROVED' | 'SUSPICIOUS' | 'FRAUD';
  merchantId?: string;
  customerId?: string;
  minAmount?: number;
  maxAmount?: number;
  page?: number;
  size?: number;
}

// Resposta Paginada
export interface PaginatedResponse<T> {
  content: T[];
  totalElements: number;
  totalPages: number;
  page: number;
  size: number;
}

// Métricas do Dashboard
export interface DashboardMetrics {
  totalTransactions: number;
  approvedCount: number;
  suspiciousCount: number;
  fraudCount: number;
  approvalRate: number;
  suspiciousRate: number;
  fraudRate: number;
  averageScore: number;
  totalAmount: number;
  periodComparison: {
    transactionsChange: number;
    fraudRateChange: number;
  };
  topMerchants: MerchantMetric[];
  mccDistribution: MCCMetric[];
  hourlyDistribution: HourlyMetric[];
}

export interface MerchantMetric {
  merchantId: string;
  merchantName: string;
  transactionCount: number;
  fraudCount: number;
  fraudRate: number;
}

export interface MCCMetric {
  mcc: string;
  description: string;
  count: number;
  percentage: number;
}

export interface HourlyMetric {
  hour: number;
  count: number;
  fraudCount: number;
}

// Configuração de Regra
export interface RuleConfiguration {
  id?: number;
  name: string;
  description: string;
  category: string;
  field: string;
  operator: string;
  threshold: string;
  weight: number;
  classification: 'APPROVED' | 'SUSPICIOUS' | 'FRAUD';
  enabled: boolean;
  version: number;
  conditions?: RuleCondition[];
  createdAt?: string;
  updatedAt?: string;
}

export interface RuleCondition {
  field: string;
  operator: string;
  value: string;
  logicOperator?: 'AND' | 'OR';
}

// Log de Auditoria
export interface AuditLog {
  id: number;
  transactionId: string;
  action: string;
  classification: string;
  totalScore: number;
  rulesApplied: string[];
  userId: string;
  timestamp: string;
  details: string;
}

// ========================================
// FUNÇÕES DE API
// ========================================

/**
 * Função auxiliar para fazer requisições HTTP
 */
async function apiRequest<T>(
  endpoint: string,
  options: RequestInit = {}
): Promise<T> {
  const url = `${JAVA_API_BASE_URL}${endpoint}`;
  
  const token = getAccessToken();
  const defaultHeaders: HeadersInit = {
    'Content-Type': 'application/json',
    'Accept': 'application/json',
    ...(token ? { Authorization: `Bearer ${token}` } : {}),
  };

  const response = await fetch(url, {
    ...options,
    headers: {
      ...defaultHeaders,
      ...options.headers,
    },
  });

  if (!response.ok) {
    const text = await response.text().catch(() => "");
    const errorMessage = text || `API Error: ${response.status} ${response.statusText}`;
    throw new Error(errorMessage);
  }

  // Algumas rotas de export retornam blob; para JSON convertemos normalmente
  const contentType = response.headers.get('content-type');
  if (contentType && contentType.includes('application/json')) {
    return response.json();
  }
  // @ts-expect-error permitir retorno de blob/texte conforme T
  return response as unknown as T;
}

// ========================================
// TRANSAÇÕES
// ========================================

/**
 * Analisar uma transação em tempo real
 */
export async function analyzeTransaction(
  request: TransactionRequest
): Promise<TransactionResponse> {
  return apiRequest<TransactionResponse>('/api/transactions/analyze', {
    method: 'POST',
    body: JSON.stringify(request),
  });
}

/**
 * Analisar transação com regras avançadas (50+ regras)
 */
export async function analyzeTransactionAdvanced(
  request: TransactionRequest
): Promise<TransactionResponse> {
  return apiRequest<TransactionResponse>('/api/transactions/analyze-advanced', {
    method: 'POST',
    body: JSON.stringify(request),
  });
}

/**
 * Listar transações processadas com filtros
 */
export async function listTransactions(
  filters: TransactionFilters = {}
): Promise<PaginatedResponse<ProcessedTransaction>> {
  const params = new URLSearchParams();
  
  if (filters.startDate) params.append('startDate', filters.startDate);
  if (filters.endDate) params.append('endDate', filters.endDate);
  if (filters.classification) params.append('classification', filters.classification);
  if (filters.merchantId) params.append('merchantId', filters.merchantId);
  if (filters.customerId) params.append('customerId', filters.customerId);
  if (filters.minAmount) params.append('minAmount', filters.minAmount.toString());
  if (filters.maxAmount) params.append('maxAmount', filters.maxAmount.toString());
  if (filters.page !== undefined) params.append('page', filters.page.toString());
  if (filters.size !== undefined) params.append('size', filters.size.toString());

  const queryString = params.toString();
  const endpoint = queryString ? `/api/transactions?${queryString}` : '/api/transactions';
  
  return apiRequest<PaginatedResponse<ProcessedTransaction>>(endpoint);
}

/**
 * Obter detalhes de uma transação específica
 */
export async function getTransactionDetails(
  transactionId: string
): Promise<ProcessedTransaction & { ruleDetails: RuleDetail[] }> {
  return apiRequest(`/api/transactions/${transactionId}`);
}

// ========================================
// MÉTRICAS
// ========================================

/**
 * Obter métricas do dashboard
 */
export async function getDashboardMetrics(
  period: '1h' | '24h' | '7d' | '30d' = '24h'
): Promise<DashboardMetrics> {
  return apiRequest<DashboardMetrics>(`/api/metrics?period=${period}`);
}

/**
 * Obter métricas em tempo real (polling)
 */
export async function getRealTimeMetrics(): Promise<{
  transactionsPerMinute: number;
  fraudDetectedLast5Min: number;
  averageResponseTime: number;
}> {
  return apiRequest('/api/metrics/realtime');
}

// ========================================
// REGRAS
// ========================================

/**
 * Listar todas as regras configuradas
 */
export async function listRules(): Promise<RuleConfiguration[]> {
  return apiRequest<RuleConfiguration[]>('/api/rules');
}

/**
 * Obter detalhes de uma regra específica
 */
export async function getRuleDetails(ruleId: number): Promise<RuleConfiguration> {
  return apiRequest<RuleConfiguration>(`/api/rules/${ruleId}`);
}

/**
 * Criar uma nova regra
 */
export async function createRule(
  rule: Omit<RuleConfiguration, 'id' | 'version' | 'createdAt' | 'updatedAt'>
): Promise<RuleConfiguration> {
  return apiRequest<RuleConfiguration>('/api/rules', {
    method: 'POST',
    body: JSON.stringify(rule),
  });
}

/**
 * Atualizar uma regra existente
 */
export async function updateRule(
  ruleId: number,
  rule: Partial<RuleConfiguration>
): Promise<RuleConfiguration> {
  return apiRequest<RuleConfiguration>(`/api/rules/${ruleId}`, {
    method: 'PUT',
    body: JSON.stringify(rule),
  });
}

/**
 * Deletar uma regra
 */
export async function deleteRule(ruleId: number): Promise<void> {
  return apiRequest(`/api/rules/${ruleId}`, {
    method: 'DELETE',
  });
}

/**
 * Ativar/Desativar uma regra
 */
export async function toggleRuleStatus(
  ruleId: number,
  enabled: boolean
): Promise<RuleConfiguration> {
  return apiRequest<RuleConfiguration>(`/api/rules/${ruleId}/toggle`, {
    method: 'PATCH',
    body: JSON.stringify({ enabled }),
  });
}

// ========================================
// AUDITORIA
// ========================================

/**
 * Listar logs de auditoria
 */
export async function listAuditLogs(
  filters: {
    startDate?: string;
    endDate?: string;
    action?: string;
    transactionId?: string;
    page?: number;
    size?: number;
  } = {}
): Promise<PaginatedResponse<AuditLog>> {
  const params = new URLSearchParams();
  
  if (filters.startDate) params.append('startDate', filters.startDate);
  if (filters.endDate) params.append('endDate', filters.endDate);
  if (filters.action) params.append('action', filters.action);
  if (filters.transactionId) params.append('transactionId', filters.transactionId);
  if (filters.page !== undefined) params.append('page', filters.page.toString());
  if (filters.size !== undefined) params.append('size', filters.size.toString());

  const queryString = params.toString();
  const endpoint = queryString ? `/api/audit?${queryString}` : '/api/audit';
  
  return apiRequest<PaginatedResponse<AuditLog>>(endpoint);
}

/**
 * Exportar logs de auditoria
 */
export async function exportAuditLogs(
  format: 'csv' | 'json' | 'pdf',
  filters: {
    startDate?: string;
    endDate?: string;
  } = {}
): Promise<Blob> {
  const params = new URLSearchParams();
  params.append('format', format);
  if (filters.startDate) params.append('startDate', filters.startDate);
  if (filters.endDate) params.append('endDate', filters.endDate);

  const response = await fetch(
    `${JAVA_API_BASE_URL}/api/audit/export?${params.toString()}`,
    {
      headers: {
        'Accept': format === 'json' ? 'application/json' : 
                  format === 'csv' ? 'text/csv' : 'application/pdf',
      },
    }
  );

  if (!response.ok) {
    throw new Error(`Export failed: ${response.status}`);
  }

  return response.blob();
}

// ========================================
// HEALTH CHECK
// ========================================

/**
 * Verificar status da API Java
 */
export async function checkApiHealth(): Promise<{
  status: 'UP' | 'DOWN';
  database: 'UP' | 'DOWN';
  responseTime: number;
}> {
  const startTime = Date.now();
  
  try {
    const response = await fetch(`${JAVA_API_BASE_URL}/api/health`, {
      method: 'GET',
      headers: { 'Accept': 'application/json' },
    });
    
    const responseTime = Date.now() - startTime;
    
    if (response.ok) {
      const data = await response.json();
      return {
        status: 'UP',
        database: data.database || 'UP',
        responseTime,
      };
    }
    
    return {
      status: 'DOWN',
      database: 'DOWN',
      responseTime,
    };
  } catch {
    return {
      status: 'DOWN',
      database: 'DOWN',
      responseTime: Date.now() - startTime,
    };
  }
}

// ========================================
// EXPORTAÇÕES
// ========================================

export const javaApi = {
  // Transações
  analyzeTransaction,
  analyzeTransactionAdvanced,
  listTransactions,
  getTransactionDetails,
  
  // Métricas
  getDashboardMetrics,
  getRealTimeMetrics,
  
  // Regras
  listRules,
  getRuleDetails,
  createRule,
  updateRule,
  deleteRule,
  toggleRuleStatus,
  
  // Auditoria
  listAuditLogs,
  exportAuditLogs,
  
  // Health
  checkApiHealth,
};

export default javaApi;
