import { eq, desc, and, sql } from "drizzle-orm";
import { drizzle } from "drizzle-orm/mysql2";
import { InsertUser, users, rules, InsertRule, Rule, transactionAudits, InsertTransactionAudit, ruleHistory, InsertRuleHistory } from "../drizzle/schema";
import { ENV } from './_core/env';

let _db: ReturnType<typeof drizzle> | null = null;

// Lazily create the drizzle instance so local tooling can run without a DB.
export async function getDb() {
  if (!_db && process.env.DATABASE_URL) {
    try {
      _db = drizzle(process.env.DATABASE_URL);
    } catch (error) {
      console.warn("[Database] Failed to connect:", error);
      _db = null;
    }
  }
  return _db;
}

export async function upsertUser(user: InsertUser): Promise<void> {
  if (!user.openId) {
    throw new Error("User openId is required for upsert");
  }

  const db = await getDb();
  if (!db) {
    console.warn("[Database] Cannot upsert user: database not available");
    return;
  }

  try {
    const values: InsertUser = {
      openId: user.openId,
    };
    const updateSet: Record<string, unknown> = {};

    const textFields = ["name", "email", "loginMethod"] as const;
    type TextField = (typeof textFields)[number];

    const assignNullable = (field: TextField) => {
      const value = user[field];
      if (value === undefined) return;
      const normalized = value ?? null;
      values[field] = normalized;
      updateSet[field] = normalized;
    };

    textFields.forEach(assignNullable);

    if (user.lastSignedIn !== undefined) {
      values.lastSignedIn = user.lastSignedIn;
      updateSet.lastSignedIn = user.lastSignedIn;
    }
    if (user.role !== undefined) {
      values.role = user.role;
      updateSet.role = user.role;
    } else if (user.openId === ENV.ownerOpenId) {
      values.role = 'admin';
      updateSet.role = 'admin';
    }

    if (!values.lastSignedIn) {
      values.lastSignedIn = new Date();
    }

    if (Object.keys(updateSet).length === 0) {
      updateSet.lastSignedIn = new Date();
    }

    await db.insert(users).values(values).onDuplicateKeyUpdate({
      set: updateSet,
    });
  } catch (error) {
    console.error("[Database] Failed to upsert user:", error);
    throw error;
  }
}

export async function getUserByOpenId(openId: string) {
  const db = await getDb();
  if (!db) {
    console.warn("[Database] Cannot get user: database not available");
    return undefined;
  }

  const result = await db.select().from(users).where(eq(users.openId, openId)).limit(1);

  return result.length > 0 ? result[0] : undefined;
}

// ==================== RULES QUERIES ====================

export async function getAllRules(): Promise<Rule[]> {
  const db = await getDb();
  if (!db) {
    console.warn("[Database] Cannot get rules: database not available");
    return [];
  }

  const result = await db.select().from(rules).orderBy(desc(rules.createdAt));
  return result;
}

export async function getActiveRules(): Promise<Rule[]> {
  const db = await getDb();
  if (!db) {
    console.warn("[Database] Cannot get rules: database not available");
    return [];
  }

  const result = await db.select().from(rules).where(eq(rules.isActive, true)).orderBy(desc(rules.weight));
  return result;
}

export async function getRuleById(id: number): Promise<Rule | undefined> {
  const db = await getDb();
  if (!db) {
    console.warn("[Database] Cannot get rule: database not available");
    return undefined;
  }

  const result = await db.select().from(rules).where(eq(rules.id, id)).limit(1);
  return result.length > 0 ? result[0] : undefined;
}

export async function createRule(rule: InsertRule): Promise<Rule> {
  const db = await getDb();
  if (!db) {
    throw new Error("Database not available");
  }

  const result = await db.insert(rules).values(rule);
  const insertId = result[0].insertId;
  
  const newRule = await getRuleById(insertId);
  if (!newRule) {
    throw new Error("Failed to create rule");
  }
  
  return newRule;
}

export async function updateRule(id: number, updates: Partial<InsertRule>): Promise<Rule | undefined> {
  const db = await getDb();
  if (!db) {
    throw new Error("Database not available");
  }

  await db.update(rules).set({
    ...updates,
    version: sql`${rules.version} + 1`,
  }).where(eq(rules.id, id));

  return getRuleById(id);
}

export async function deleteRule(id: number): Promise<boolean> {
  const db = await getDb();
  if (!db) {
    throw new Error("Database not available");
  }

  const result = await db.delete(rules).where(eq(rules.id, id));
  return true;
}

export async function toggleRuleActive(id: number, isActive: boolean): Promise<Rule | undefined> {
  return updateRule(id, { isActive });
}

// ==================== TRANSACTION AUDIT QUERIES ====================

export async function createTransactionAudit(audit: InsertTransactionAudit): Promise<void> {
  const db = await getDb();
  if (!db) {
    console.warn("[Database] Cannot create audit: database not available");
    return;
  }

  await db.insert(transactionAudits).values(audit);
}

export async function getTransactionAudits(limit: number = 100): Promise<typeof transactionAudits.$inferSelect[]> {
  const db = await getDb();
  if (!db) {
    console.warn("[Database] Cannot get audits: database not available");
    return [];
  }

  const result = await db.select().from(transactionAudits).orderBy(desc(transactionAudits.createdAt)).limit(limit);
  return result;
}

// ==================== RULE HISTORY QUERIES ====================

export async function createRuleHistory(history: InsertRuleHistory): Promise<void> {
  const db = await getDb();
  if (!db) {
    console.warn("[Database] Cannot create history: database not available");
    return;
  }

  await db.insert(ruleHistory).values(history);
}

export async function getRuleHistory(ruleId: number): Promise<typeof ruleHistory.$inferSelect[]> {
  const db = await getDb();
  if (!db) {
    console.warn("[Database] Cannot get history: database not available");
    return [];
  }

  const result = await db.select().from(ruleHistory).where(eq(ruleHistory.ruleId, ruleId)).orderBy(desc(ruleHistory.createdAt));
  return result;
}

// ==================== METRICS QUERIES ====================

export async function getMetrics() {
  const db = await getDb();
  if (!db) {
    return {
      totalTransactions: 0,
      approvedCount: 0,
      suspiciousCount: 0,
      fraudCount: 0,
      approvalRate: 0,
      suspiciousRate: 0,
      fraudRate: 0,
    };
  }

  const result = await db.select({
    classification: transactionAudits.classification,
    count: sql<number>`count(*)`,
  }).from(transactionAudits).groupBy(transactionAudits.classification);

  let approvedCount = 0;
  let suspiciousCount = 0;
  let fraudCount = 0;

  result.forEach(row => {
    if (row.classification === "APPROVED") approvedCount = Number(row.count);
    if (row.classification === "SUSPICIOUS") suspiciousCount = Number(row.count);
    if (row.classification === "FRAUD") fraudCount = Number(row.count);
  });

  const totalTransactions = approvedCount + suspiciousCount + fraudCount;

  return {
    totalTransactions,
    approvedCount,
    suspiciousCount,
    fraudCount,
    approvalRate: totalTransactions > 0 ? (approvedCount / totalTransactions) * 100 : 0,
    suspiciousRate: totalTransactions > 0 ? (suspiciousCount / totalTransactions) * 100 : 0,
    fraudRate: totalTransactions > 0 ? (fraudCount / totalTransactions) * 100 : 0,
  };
}
