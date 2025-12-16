import { int, mysqlEnum, mysqlTable, text, timestamp, varchar, boolean, json, decimal } from "drizzle-orm/mysql-core";

/**
 * Core user table backing auth flow.
 * Extend this file with additional tables as your product grows.
 * Columns use camelCase to match both database fields and generated types.
 */
export const users = mysqlTable("users", {
  /**
   * Surrogate primary key. Auto-incremented numeric value managed by the database.
   * Use this for relations between tables.
   */
  id: int("id").autoincrement().primaryKey(),
  /** Manus OAuth identifier (openId) returned from the OAuth callback. Unique per user. */
  openId: varchar("openId", { length: 64 }).notNull().unique(),
  name: text("name"),
  email: varchar("email", { length: 320 }),
  loginMethod: varchar("loginMethod", { length: 64 }),
  role: mysqlEnum("role", ["user", "admin"]).default("user").notNull(),
  createdAt: timestamp("createdAt").defaultNow().notNull(),
  updatedAt: timestamp("updatedAt").defaultNow().onUpdateNow().notNull(),
  lastSignedIn: timestamp("lastSignedIn").defaultNow().notNull(),
});

export type User = typeof users.$inferSelect;
export type InsertUser = typeof users.$inferInsert;

/**
 * Tabela de Regras Duras para detecção de fraude
 * Cada regra define condições, thresholds e classificação
 */
export const rules = mysqlTable("rules", {
  id: int("id").autoincrement().primaryKey(),
  /** Nome único da regra */
  name: varchar("name", { length: 100 }).notNull().unique(),
  /** Descrição detalhada da regra */
  description: text("description"),
  /** Categoria da regra */
  category: mysqlEnum("category", [
    "VALUE",
    "TEMPORAL", 
    "GEOGRAPHIC",
    "MCC",
    "AUTHENTICATION",
    "CVV_PIN",
    "TERMINAL",
    "EMV",
    "CARD",
    "CONTEXT",
    "COMBINED",
    "BRAZIL_SPECIFIC"
  ]).notNull(),
  /** Classificação quando a regra é acionada */
  classification: mysqlEnum("classification", ["APPROVED", "SUSPICIOUS", "FRAUD"]).notNull(),
  /** Peso da regra (0-100) */
  weight: int("weight").notNull().default(50),
  /** Condições da regra em formato JSON */
  conditions: json("conditions").notNull(),
  /** Operador lógico entre condições */
  logicOperator: mysqlEnum("logicOperator", ["AND", "OR"]).notNull().default("AND"),
  /** Se a regra está ativa */
  isActive: boolean("isActive").notNull().default(true),
  /** Versão da regra para auditoria */
  version: int("version").notNull().default(1),
  /** Fonte/referência da regra */
  source: varchar("source", { length: 255 }),
  createdAt: timestamp("createdAt").defaultNow().notNull(),
  updatedAt: timestamp("updatedAt").defaultNow().onUpdateNow().notNull(),
  createdBy: varchar("createdBy", { length: 64 }),
});

export type Rule = typeof rules.$inferSelect;
export type InsertRule = typeof rules.$inferInsert;

/**
 * Tabela de Auditoria de Transações
 * Registra todas as decisões tomadas pelo motor de regras
 */
export const transactionAudits = mysqlTable("transaction_audits", {
  id: int("id").autoincrement().primaryKey(),
  /** ID externo da transação */
  externalTransactionId: varchar("externalTransactionId", { length: 100 }).notNull(),
  /** ID do cliente */
  customerId: varchar("customerId", { length: 100 }),
  /** ID do merchant */
  merchantId: varchar("merchantId", { length: 100 }),
  /** Valor da transação em centavos */
  transactionAmount: int("transactionAmount"),
  /** Classificação final */
  classification: mysqlEnum("classification", ["APPROVED", "SUSPICIOUS", "FRAUD"]).notNull(),
  /** Score total calculado */
  totalScore: int("totalScore").notNull().default(0),
  /** Regras acionadas (JSON array de IDs) */
  triggeredRules: json("triggeredRules"),
  /** Detalhes das regras acionadas (JSON) */
  ruleDetails: json("ruleDetails"),
  /** Motivo da decisão */
  reason: text("reason"),
  /** Payload original da requisição (JSON) */
  originalPayload: json("originalPayload"),
  /** Tempo de processamento em ms */
  processingTimeMs: int("processingTimeMs"),
  createdAt: timestamp("createdAt").defaultNow().notNull(),
});

export type TransactionAudit = typeof transactionAudits.$inferSelect;
export type InsertTransactionAudit = typeof transactionAudits.$inferInsert;

/**
 * Tabela de Histórico de Alterações de Regras
 * Para auditoria e compliance
 */
export const ruleHistory = mysqlTable("rule_history", {
  id: int("id").autoincrement().primaryKey(),
  /** ID da regra alterada */
  ruleId: int("ruleId").notNull(),
  /** Tipo de ação */
  action: mysqlEnum("action", ["CREATE", "UPDATE", "DELETE", "ACTIVATE", "DEACTIVATE"]).notNull(),
  /** Estado anterior (JSON) */
  previousState: json("previousState"),
  /** Estado novo (JSON) */
  newState: json("newState"),
  /** Usuário que fez a alteração */
  changedBy: varchar("changedBy", { length: 64 }),
  /** Motivo da alteração */
  changeReason: text("changeReason"),
  createdAt: timestamp("createdAt").defaultNow().notNull(),
});

export type RuleHistory = typeof ruleHistory.$inferSelect;
export type InsertRuleHistory = typeof ruleHistory.$inferInsert;
