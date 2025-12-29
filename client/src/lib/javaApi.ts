/**
 * RULEX - API client (frontend) para Backend Java (Spring Boot).
 *
 * Objetivo: manter este módulo alinhado com o contrato real exposto em `/api/*`.
 * (server.servlet.context-path = /api)
 */

import { getAccessToken, getBasicAuthRaw } from "@/_core/auth/tokens";

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

export interface TransactionRequest {
  externalTransactionId?: string;
  customerIdFromHeader?: string;
  customerAcctNumber?: number;
  pan?: string;
  transactionAmount?: number;
  transactionDate?: number; // YYYYMMDD
  transactionTime?: number; // HHMMSS
  transactionCurrencyCode?: number;
  mcc?: number;
  consumerAuthenticationScore?: number;
  externalScore3?: number;
  cavvResult?: number;
  eciIndicator?: number;
  atcCard?: number;
  atcHost?: number;
  tokenAssuranceLevel?: number;
  availableCredit?: number;
  cardCashBalance?: number;
  cardDelinquentAmount?: number;

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
  operator:
    | "=="
    | "!="
    | ">"
    | "<"
    | ">="
    | "<="
    | "IN"
    | "NOT_IN"
    | "CONTAINS"
    | "NOT_CONTAINS";
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
  conditions: RuleCondition[];
  logicOperator: "AND" | "OR";
  version: number;
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

// ========================================
// RULES
// ========================================

export async function listRules(): Promise<RuleConfiguration[]> {
  const response = await apiRequest<any>("/api/rules");
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

export const javaApi = {
  analyzeTransaction,
  analyzeTransactionAdvanced,
  listTransactions,
  exportTransactions,
  getTransactionDetails,
  getTransactionById,
  getTransactionByExternalId,
  getDashboardMetrics,
  listRules,
  getRuleDetails,
  createRule,
  updateRule,
  deleteRule,
  toggleRuleStatus,
  listAuditLogs,
  exportAuditLogs,
  checkApiHealth,
};

export default javaApi;
