/**
 * RULEX - API client (frontend) para Backend Java (Spring Boot).
 *
 * Objetivo: manter este módulo alinhado com o contrato real exposto em `/api/*`.
 * (server.servlet.context-path = /api)
 */

import { getAccessToken, getBasicAuthRaw } from "@/_core/auth/tokens";
import type { ConditionOperatorType } from "./operatorTypes";

// ========================================
// CONFIG
// ========================================

// Preferir same-origin (Vite proxy em dev). Pode ser sobrescrito por env.
const JAVA_API_BASE_URL: string = import.meta.env.VITE_JAVA_API_URL || "";

// Opcional: Basic Auth (Spring Security HTTP Basic).
// Formato: "usuario:senha" (ex.: "admin:rulex").
const BASIC_AUTH_RAW = import.meta.env.VITE_API_BASIC_AUTH as string | undefined;

// ========================================
// TYPES
// ========================================

/**
 * Transaction request DTO.
 * Required fields are marked as non-optional per OpenAPI spec.
 * Optional fields use the '?' modifier.
 */
export interface TransactionRequest {
  // === REQUIRED FIELDS (per OpenAPI AnalyzeTransactionRequest) ===
  externalTransactionId: string;
  customerIdFromHeader: string;
  customerAcctNumber: number;
  pan: string;
  transactionAmount: number;
  transactionDate: number; // YYYYMMDD
  transactionTime: number; // HHMMSS
  transactionCurrencyCode: number;
  mcc: number;
  consumerAuthenticationScore: number;
  externalScore3: number;
  cavvResult: number;
  eciIndicator: number;
  atcCard: number;
  atcHost: number;
  tokenAssuranceLevel: number;
  availableCredit: number;
  cardCashBalance: number;
  cardDelinquentAmount: number;

  // === OPTIONAL FIELDS ===
  // extras usados por regras/filtros
  merchantId?: string;
  merchantName?: string;
  merchantCountryCode?: string;
  merchantCity?: string;
  merchantState?: string;
  merchantPostalCode?: string;
  posEntryMode?: string;
  customerPresent?: string;
  gmtOffset?: string;

  // campos opcionais de segurança/terminal (subset)
  cryptogramValid?: string;
  cvv2Response?: string;
  cvv2Present?: string;
  pinVerifyCode?: string;
  cvvVerifyCode?: string;
  posOffPremises?: number;
  posCardCapture?: number;
  posSecurity?: number;
  cvvPinTryLimitExceeded?: number;
  cvrofflinePinVerificationPerformed?: number;
  cvrofflinePinVerificationFailed?: number;
  cardExpireDate?: number;
  cardMediaType?: string;
  transactionCurrencyConversionRate?: number;
}

/**
 * Partial transaction request for forms and simulators.
 * All fields are optional to allow incremental form filling.
 */
export type TransactionRequestPartial = Partial<TransactionRequest>;

export interface TriggeredRule {
  name: string;
  weight: number;
  contribution: number;
  detail?: string | null;
}

export interface TransactionResponse {
  transactionId: string;
  id?: number | null;
  customerIdFromHeader?: string | null;
  merchantId?: string | null;
  merchantName?: string | null;
  transactionAmount?: number | null;
  transactionDate?: number | null;
  transactionTime?: number | null;
  classification: "APPROVED" | "SUSPICIOUS" | "FRAUD" | "UNKNOWN";
  riskScore: number;
  triggeredRules: TriggeredRule[];
  reason: string;
  rulesetVersion: string;
  processingTimeMs: number;
  timestamp: string;
  success: boolean;
}

export type ProcessedTransaction = TransactionResponse;

export interface TransactionFilters {
  customerId?: string;
  merchantId?: string;
  classification?: "APPROVED" | "SUSPICIOUS" | "FRAUD";
  mcc?: number;
  minAmount?: number;
  maxAmount?: number;
  startDate?: string; // ISO_DATE_TIME
  endDate?: string; // ISO_DATE_TIME
  page?: number;
  size?: number;
}

export interface PaginatedResponse<T> {
  content: T[];
  totalElements: number;
  totalPages: number;
  number: number;
  size: number;
}

export interface DashboardMetrics {
  totalTransactions: number;
  approvedTransactions: number;
  suspiciousTransactions: number;
  fraudTransactions: number;
  approvalRate: number;
  suspiciousRate: number;
  fraudRate: number;
  totalVolume?: number;
  averageTransactionAmount?: number;
  highestTransactionAmount?: number;
  lowestTransactionAmount?: number;
  period?: string;
  timestamp?: string;
  mccDistribution?: Record<string, number>;
  merchantDistribution?: Record<string, number>;
}

export interface RuleCondition {
  field: string;
  operator: ConditionOperatorType;
  value: string;
}

export interface RuleConfiguration {
  id: number;
  ruleName: string;
  description?: string | null;
  ruleType: "SECURITY" | "CONTEXT" | "VELOCITY" | "ANOMALY";
  threshold: number;
  weight: number;
  enabled: boolean;
  classification: "APPROVED" | "SUSPICIOUS" | "FRAUD";
  parameters?: string | null;
  conditions: RuleCondition[];
  logicOperator: "AND" | "OR";
  version: number;
}

export interface RuleSimulationConditionResult {
  field: string;
  operator: string;
  expectedValue: string;
  actualValue: string;
  met: boolean;
}

export interface RuleSimulationResult {
  ruleName: string;
  triggered: boolean;
  classification?: string | null;
  weight?: number | null;
  reason: string;
  logicOperator?: string | null;
  processingTimeMs: number;
  conditionResults: RuleSimulationConditionResult[];
}

export interface RuleBacktestResult {
  ruleId: number;
  ruleName: string;
  startDate: string;
  endDate: string;
  totalEvaluated: number;
  totalTriggered: number;
  triggerRate: number;
  wouldApprove: number;
  wouldSuspect: number;
  wouldBlock: number;
  totalAmountAffected: number;
  sampleResults: RuleSimulationResult[];
}

export interface AuditLog {
  id: number;
  transactionId: number | null;
  actionType: string;
  description: string;
  details?: string | null;
  performedBy?: string | null;
  result: string;
  errorMessage?: string | null;
  sourceIp?: string | null;
  createdAt: string;
}

export interface FieldDictionaryItem {
  workflow: string | null;
  recordType: string | null;
  portfolio: string | null;
  jsonPath: string;
  type: string;
  domain: unknown | null;
  sentinelValues: unknown | null;
  allowedOperators: string[];
  allowedFunctions: string[];
  requirednessByContext: unknown | null;
  securityConstraints: unknown | null;
  normalizationAllowed: boolean;
}

// ========================================
// HTTP helper
// ========================================

async function apiRequest<T>(endpoint: string, options: RequestInit = {}): Promise<T> {
  const url = `${JAVA_API_BASE_URL}${endpoint}`;

  const token = getAccessToken();
  const basicAuthRaw = BASIC_AUTH_RAW || getBasicAuthRaw() || undefined;
  const basicAuthHeader =
    !token && basicAuthRaw ? `Basic ${btoa(basicAuthRaw)}` : undefined;

  const headers: HeadersInit = {
    Accept: "application/json",
    ...(options.body ? { "Content-Type": "application/json" } : {}),
    ...(token ? { Authorization: `Bearer ${token}` } : {}),
    ...(!token && basicAuthHeader ? { Authorization: basicAuthHeader } : {}),
    ...options.headers,
  };

  const response = await fetch(url, { ...options, headers });

  if (!response.ok) {
    const text = await response.text().catch(() => "");
    throw new Error(text || `API Error: ${response.status} ${response.statusText}`);
  }

  const contentType = response.headers.get("content-type") || "";
  if (contentType.includes("application/json")) {
    return response.json();
  }

  return (await response.blob()) as unknown as T;
}

// ========================================
// TRANSACTIONS
// ========================================

export async function analyzeTransaction(request: TransactionRequest): Promise<TransactionResponse> {
  return apiRequest<TransactionResponse>("/api/transactions/analyze", {
    method: "POST",
    body: JSON.stringify(request),
  });
}

export async function analyzeTransactionAdvanced(
  request: TransactionRequest
): Promise<TransactionResponse> {
  return apiRequest<TransactionResponse>("/api/transactions/analyze-advanced", {
    method: "POST",
    body: JSON.stringify(request),
  });
}

export async function listTransactions(
  filters: TransactionFilters = {}
): Promise<PaginatedResponse<ProcessedTransaction>> {
  const params = new URLSearchParams();
  if (filters.customerId) params.append("customerId", filters.customerId);
  if (filters.merchantId) params.append("merchantId", filters.merchantId);
  if (filters.classification) params.append("classification", filters.classification);
  if (filters.mcc !== undefined) params.append("mcc", String(filters.mcc));
  if (filters.minAmount !== undefined) params.append("minAmount", String(filters.minAmount));
  if (filters.maxAmount !== undefined) params.append("maxAmount", String(filters.maxAmount));
  if (filters.startDate) params.append("startDate", filters.startDate);
  if (filters.endDate) params.append("endDate", filters.endDate);
  if (filters.page !== undefined) params.append("page", String(filters.page));
  if (filters.size !== undefined) params.append("size", String(filters.size));

  const qs = params.toString();
  return apiRequest<PaginatedResponse<ProcessedTransaction>>(
    qs ? `/api/transactions?${qs}` : "/api/transactions"
  );
}

export async function exportTransactions(
  format: "csv" | "json" = "csv",
  filters: TransactionFilters = {},
  limit = 10000
): Promise<Blob | TransactionResponse[]> {
  const params = new URLSearchParams();
  params.append("format", format);
  params.append("limit", String(limit));
  if (filters.customerId) params.append("customerId", filters.customerId);
  if (filters.merchantId) params.append("merchantId", filters.merchantId);
  if (filters.classification) params.append("classification", filters.classification);
  if (filters.mcc !== undefined) params.append("mcc", String(filters.mcc));
  if (filters.minAmount !== undefined) params.append("minAmount", String(filters.minAmount));
  if (filters.maxAmount !== undefined) params.append("maxAmount", String(filters.maxAmount));
  if (filters.startDate) params.append("startDate", filters.startDate);
  if (filters.endDate) params.append("endDate", filters.endDate);

  const url = `${JAVA_API_BASE_URL}/api/transactions/export?${params.toString()}`;
  const token = getAccessToken();
  const basicAuthHeader =
    !token && BASIC_AUTH_RAW ? `Basic ${btoa(BASIC_AUTH_RAW)}` : undefined;

  const response = await fetch(url, {
    headers: {
      Accept: format === "json" ? "application/json" : "text/csv",
      ...(token ? { Authorization: `Bearer ${token}` } : {}),
      ...(!token && basicAuthHeader ? { Authorization: basicAuthHeader } : {}),
    },
  });
  if (!response.ok) {
    const text = await response.text().catch(() => "");
    throw new Error(text || `Export failed: ${response.status}`);
  }
  if (format === "json") {
    return response.json();
  }
  return response.blob();
}

export async function getTransactionDetails(transactionId: string): Promise<TransactionResponse> {
  return apiRequest<TransactionResponse>(`/api/transactions/${transactionId}`);
}

export async function getTransactionById(id: number): Promise<TransactionResponse> {
  return apiRequest<TransactionResponse>(`/api/transactions/${id}`);
}

export async function getTransactionByExternalId(externalId: string): Promise<TransactionResponse> {
  return apiRequest<TransactionResponse>(`/api/transactions/external/${encodeURIComponent(externalId)}`);
}

// ========================================
// METRICS
// ========================================

export async function getDashboardMetrics(
  period: "1h" | "24h" | "7d" | "30d" = "24h"
): Promise<DashboardMetrics> {
  return apiRequest<DashboardMetrics>(`/api/metrics?period=${encodeURIComponent(period)}`);
}

export interface TimelineBucket {
  bucket: string;
  total: number;
  fraud: number;
  approved?: number;
  suspicious?: number;
}

export interface MetricsTimeline {
  granularity: string;
  buckets: TimelineBucket[];
}

export async function getMetricsTimeline(
  granularity: "hour" | "day" = "hour"
): Promise<MetricsTimeline> {
  return apiRequest<MetricsTimeline>(`/api/metrics/timeline?granularity=${encodeURIComponent(granularity)}`);
}

export interface MccMetrics {
  total: number;
  approved: number;
  suspicious: number;
  fraud: number;
  fraudRate: number;
}

export async function getMetricsByMcc(
  period: "1h" | "24h" | "7d" | "30d" = "24h"
): Promise<Record<string, MccMetrics>> {
  return apiRequest<Record<string, MccMetrics>>(`/api/metrics/mcc?period=${encodeURIComponent(period)}`);
}

export interface MerchantMetrics {
  merchantId: string;
  merchantName: string;
  total: number;
  fraud: number;
  fraudRate: number;
}

export async function getMetricsByMerchant(
  period: "1h" | "24h" | "7d" | "30d" = "24h"
): Promise<Record<string, MerchantMetrics>> {
  return apiRequest<Record<string, MerchantMetrics>>(`/api/metrics/merchant?period=${encodeURIComponent(period)}`);
}

// ========================================
// RULES
// ========================================

export async function listRules(): Promise<RuleConfiguration[]> {
  // Buscar todas as regras com paginação grande para garantir que todas sejam retornadas
  const response = await apiRequest<any>("/api/rules?page=0&size=1000");
  if (Array.isArray(response)) return response;
  if (response && Array.isArray(response.content)) return response.content;
  return [];
}

export async function getRuleDetails(ruleId: number): Promise<RuleConfiguration> {
  return apiRequest<RuleConfiguration>(`/api/rules/${ruleId}`);
}

export async function createRule(
  rule: Omit<RuleConfiguration, "id" | "version">
): Promise<RuleConfiguration> {
  return apiRequest<RuleConfiguration>("/api/rules", {
    method: "POST",
    body: JSON.stringify(rule),
  });
}

export async function updateRule(
  ruleId: number,
  rule: Partial<RuleConfiguration>
): Promise<RuleConfiguration> {
  return apiRequest<RuleConfiguration>(`/api/rules/${ruleId}`, {
    method: "PUT",
    body: JSON.stringify(rule),
  });
}

export async function deleteRule(ruleId: number): Promise<void> {
  await apiRequest<void>(`/api/rules/${ruleId}`, { method: "DELETE" });
}

export async function toggleRuleStatus(
  ruleId: number,
  enabled: boolean
): Promise<RuleConfiguration> {
  return apiRequest<RuleConfiguration>(`/api/rules/${ruleId}/toggle`, {
    method: "PATCH",
    body: JSON.stringify({ enabled }),
  });
}

export async function simulateRule(
  rule: RuleConfiguration,
  testPayload: TransactionRequest
): Promise<RuleSimulationResult> {
  return apiRequest<RuleSimulationResult>("/api/rules/simulation/test", {
    method: "POST",
    body: JSON.stringify({ rule, testPayload }),
  });
}

export async function backtestRule(
  ruleId: number,
  startDate: string,
  endDate: string,
  sampleSize: number
): Promise<RuleBacktestResult> {
  const params = new URLSearchParams({
    startDate,
    endDate,
    sampleSize: String(sampleSize),
  });
  return apiRequest<RuleBacktestResult>(`/api/rules/simulation/backtest/${ruleId}?${params.toString()}`, {
    method: "POST",
  });
}

// ========================================
// AUDIT
// ========================================

export async function listAuditLogs(
  filters: {
    actionType?: string;
    result?: string;
    startDate?: string;
    endDate?: string;
    page?: number;
    size?: number;
  } = {}
): Promise<PaginatedResponse<AuditLog>> {
  const params = new URLSearchParams();
  if (filters.actionType) params.append("actionType", filters.actionType);
  if (filters.result) params.append("result", filters.result);
  if (filters.startDate) params.append("startDate", filters.startDate);
  if (filters.endDate) params.append("endDate", filters.endDate);
  if (filters.page !== undefined) params.append("page", String(filters.page));
  if (filters.size !== undefined) params.append("size", String(filters.size));
  const qs = params.toString();
  return apiRequest<PaginatedResponse<AuditLog>>(qs ? `/api/audit?${qs}` : "/api/audit");
}

export async function exportAuditLogs(
  format: "csv" | "json" = "csv",
  filters: {
    actionType?: string;
    result?: string;
    startDate?: string;
    endDate?: string;
    limit?: number;
  } = {}
): Promise<Blob | AuditLog[]> {
  const params = new URLSearchParams();
  params.append("format", format);
  if (filters.actionType) params.append("actionType", filters.actionType);
  if (filters.result) params.append("result", filters.result);
  if (filters.startDate) params.append("startDate", filters.startDate);
  if (filters.endDate) params.append("endDate", filters.endDate);
  params.append("limit", String(filters.limit ?? 10000));

  const url = `${JAVA_API_BASE_URL}/api/audit/export?${params.toString()}`;
  const token = getAccessToken();
  const basicAuthHeader =
    !token && BASIC_AUTH_RAW ? `Basic ${btoa(BASIC_AUTH_RAW)}` : undefined;

  const response = await fetch(url, {
    headers: {
      Accept: format === "json" ? "application/json" : "text/csv",
      ...(token ? { Authorization: `Bearer ${token}` } : {}),
      ...(!token && basicAuthHeader ? { Authorization: basicAuthHeader } : {}),
    },
  });
  if (!response.ok) {
    const text = await response.text().catch(() => "");
    throw new Error(text || `Export failed: ${response.status}`);
  }
  if (format === "json") {
    return response.json();
  }
  return response.blob();
}

// ========================================
// FIELD DICTIONARY (v3.1)
// ========================================

export async function listFieldDictionary(params?: {
  workflow?: string;
  recordType?: string;
  portfolio?: string;
}): Promise<FieldDictionaryItem[]> {
  const qs = new URLSearchParams();
  if (params?.workflow) qs.append("workflow", params.workflow);
  if (params?.recordType) qs.append("recordType", params.recordType);
  if (params?.portfolio) qs.append("portfolio", params.portfolio);
  const url = qs.toString() ? `/api/field-dictionary?${qs}` : "/api/field-dictionary";
  const response = await apiRequest<any>(url);
  return Array.isArray(response) ? (response as FieldDictionaryItem[]) : [];
}

// ========================================
// HEALTH
// ========================================

export async function checkApiHealth(): Promise<{
  status: "UP" | "DOWN";
  responseTime: number;
}> {
  const start = Date.now();
  try {
    const response = await fetch(`${JAVA_API_BASE_URL}/actuator/health`, {
      method: "GET",
      headers: { Accept: "application/json" },
    });
    const responseTime = Date.now() - start;
    return { status: response.ok ? "UP" : "DOWN", responseTime };
  } catch {
    return { status: "DOWN", responseTime: Date.now() - start };
  }
}

// ========================================
// COMPLEX RULES API
// ========================================

export interface ComplexRuleCondition {
  id?: string;
  fieldName: string;
  fieldPath?: string;
  operator: string;
  valueType: string;
  valueSingle?: string;
  valueArray?: string[];
  valueMin?: string;
  valueMax?: string;
  valueFieldRef?: string;
  valueExpression?: string;
  caseSensitive?: boolean;
  negate?: boolean;
  enabled?: boolean;
}

export interface ComplexRuleConditionGroup {
  id?: string;
  logicOperator: 'AND' | 'OR' | 'NOT' | 'XOR' | 'NAND' | 'NOR';
  name?: string;
  description?: string;
  position?: number;
  enabled?: boolean;
  conditions: ComplexRuleCondition[];
  children: ComplexRuleConditionGroup[];
}

export interface ComplexRuleDTO {
  id?: string;
  key: string;
  title: string;
  description?: string;
  version?: number;
  status: 'DRAFT' | 'PUBLISHED' | 'ARCHIVED' | 'TESTING';
  priority: number;
  severity: number;
  decision: 'APROVADO' | 'SUSPEITA_DE_FRAUDE' | 'FRAUDE';
  reasonTemplate?: string;
  enabled: boolean;
  rootConditionGroup: ComplexRuleConditionGroup;
  expressions?: Array<{
    id?: string;
    name: string;
    expression: string;
    resultType: string;
    description?: string;
  }>;
  contextVariables?: Array<{
    id?: string;
    name: string;
    source: string;
    path?: string;
    defaultValue?: string;
    description?: string;
  }>;
  actions?: Array<{
    id?: string;
    type: string;
    config: Record<string, unknown>;
    enabled?: boolean;
  }>;
  tags?: string[];
  fieldsUsed?: string[];
  createdBy?: string;
  createdAt?: string;
  updatedAt?: string;
}

/**
 * Lista todas as regras complexas
 */
export async function listComplexRules(): Promise<ComplexRuleDTO[]> {
  return apiRequest<ComplexRuleDTO[]>('/api/complex-rules');
}

/**
 * Busca uma regra complexa por ID
 */
export async function getComplexRule(id: string): Promise<ComplexRuleDTO> {
  return apiRequest<ComplexRuleDTO>(`/api/complex-rules/${id}`);
}

/**
 * Busca uma regra complexa por chave
 */
export async function getComplexRuleByKey(key: string): Promise<ComplexRuleDTO> {
  return apiRequest<ComplexRuleDTO>(`/api/complex-rules/key/${key}`);
}

/**
 * Cria uma nova regra complexa
 */
export async function createComplexRule(rule: Omit<ComplexRuleDTO, 'id' | 'version' | 'createdAt' | 'updatedAt'>): Promise<ComplexRuleDTO> {
  return apiRequest<ComplexRuleDTO>('/api/complex-rules', {
    method: 'POST',
    body: JSON.stringify(rule),
  });
}

/**
 * Atualiza uma regra complexa existente
 */
export async function updateComplexRule(id: string, rule: Partial<ComplexRuleDTO>): Promise<ComplexRuleDTO> {
  return apiRequest<ComplexRuleDTO>(`/api/complex-rules/${id}`, {
    method: 'PUT',
    body: JSON.stringify(rule),
  });
}

/**
 * Deleta uma regra complexa
 */
export async function deleteComplexRule(id: string): Promise<void> {
  await apiRequest<void>(`/api/complex-rules/${id}`, {
    method: 'DELETE',
  });
}

/**
 * Alterna o status de uma regra complexa
 */
export async function toggleComplexRuleStatus(id: string, enabled: boolean): Promise<ComplexRuleDTO> {
  return apiRequest<ComplexRuleDTO>(`/api/complex-rules/${id}/toggle?enabled=${enabled}`, {
    method: 'PATCH',
  });
}

/**
 * Duplica uma regra complexa
 */
export async function duplicateComplexRule(id: string, newKey: string): Promise<ComplexRuleDTO> {
  return apiRequest<ComplexRuleDTO>(`/api/complex-rules/${id}/duplicate?newKey=${encodeURIComponent(newKey)}`, {
    method: 'POST',
  });
}

/**
 * Valida uma regra complexa sem salvar
 */
export async function validateComplexRule(rule: Partial<ComplexRuleDTO>): Promise<{ valid: boolean; errors: string[] }> {
  return apiRequest<{ valid: boolean; errors: string[] }>('/api/complex-rules/validate', {
    method: 'POST',
    body: JSON.stringify(rule),
  });
}

export const javaApi = {
  analyzeTransaction,
  analyzeTransactionAdvanced,
  listTransactions,
  exportTransactions,
  getTransactionDetails,
  getTransactionById,
  getTransactionByExternalId,
  getDashboardMetrics,
  getMetricsTimeline,
  getMetricsByMcc,
  getMetricsByMerchant,
  listRules,
  getRuleDetails,
  createRule,
  updateRule,
  deleteRule,
  toggleRuleStatus,
  listAuditLogs,
  exportAuditLogs,
  listFieldDictionary,
  checkApiHealth,
  // Complex Rules
  listComplexRules,
  getComplexRule,
  getComplexRuleByKey,
  createComplexRule,
  updateComplexRule,
  deleteComplexRule,
  toggleComplexRuleStatus,
  duplicateComplexRule,
  validateComplexRule,
};

export default javaApi;
