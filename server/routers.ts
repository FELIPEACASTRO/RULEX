import { COOKIE_NAME } from "@shared/const";
import { getSessionCookieOptions } from "./_core/cookies";
import { systemRouter } from "./_core/systemRouter";
import { publicProcedure, protectedProcedure, router } from "./_core/trpc";
import { z } from "zod";
import { 
  getAllRules, 
  getActiveRules, 
  getRuleById, 
  createRule, 
  updateRule, 
  deleteRule, 
  toggleRuleActive,
  createRuleHistory,
  getRuleHistory,
  getTransactionAudits,
  createTransactionAudit,
  getMetrics
} from "./db";

// Schema de validação para condições de regra
const conditionSchema = z.object({
  field: z.string(),
  operator: z.enum(["==", "!=", ">", "<", ">=", "<=", "IN", "NOT_IN", "CONTAINS", "NOT_CONTAINS"]),
  value: z.union([z.string(), z.number(), z.boolean(), z.array(z.string()), z.array(z.number())]),
});

// Schema de validação para criação de regra
const createRuleSchema = z.object({
  name: z.string().min(1).max(100),
  description: z.string().optional(),
  category: z.enum([
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
  ]),
  classification: z.enum(["APPROVED", "SUSPICIOUS", "FRAUD"]),
  weight: z.number().min(0).max(100).default(50),
  conditions: z.array(conditionSchema).min(1),
  logicOperator: z.enum(["AND", "OR"]).default("AND"),
  isActive: z.boolean().default(true),
  source: z.string().optional(),
});

// Schema de validação para atualização de regra
const updateRuleSchema = createRuleSchema.partial();

export const appRouter = router({
  system: systemRouter,
  
  auth: router({
    me: publicProcedure.query(opts => opts.ctx.user),
    logout: publicProcedure.mutation(({ ctx }) => {
      const cookieOptions = getSessionCookieOptions(ctx.req);
      ctx.res.clearCookie(COOKIE_NAME, { ...cookieOptions, maxAge: -1 });
      return { success: true } as const;
    }),
  }),

  // ==================== RULES ROUTER ====================
  rules: router({
    // Listar todas as regras
    list: publicProcedure.query(async () => {
      return getAllRules();
    }),

    // Listar apenas regras ativas
    listActive: publicProcedure.query(async () => {
      return getActiveRules();
    }),

    // Obter regra por ID
    getById: publicProcedure
      .input(z.object({ id: z.number() }))
      .query(async ({ input }) => {
        return getRuleById(input.id);
      }),

    // Criar nova regra
    create: protectedProcedure
      .input(createRuleSchema)
      .mutation(async ({ input, ctx }) => {
        const newRule = await createRule({
          ...input,
          conditions: input.conditions,
          createdBy: ctx.user.openId,
        });

        // Registrar no histórico
        await createRuleHistory({
          ruleId: newRule.id,
          action: "CREATE",
          newState: newRule,
          changedBy: ctx.user.openId,
        });

        return newRule;
      }),

    // Atualizar regra existente
    update: protectedProcedure
      .input(z.object({
        id: z.number(),
        data: updateRuleSchema,
      }))
      .mutation(async ({ input, ctx }) => {
        const previousRule = await getRuleById(input.id);
        
        const updatedRule = await updateRule(input.id, {
          ...input.data,
          conditions: input.data.conditions,
        });

        // Registrar no histórico
        if (updatedRule) {
          await createRuleHistory({
            ruleId: input.id,
            action: "UPDATE",
            previousState: previousRule,
            newState: updatedRule,
            changedBy: ctx.user.openId,
          });
        }

        return updatedRule;
      }),

    // Deletar regra
    delete: protectedProcedure
      .input(z.object({ id: z.number() }))
      .mutation(async ({ input, ctx }) => {
        const previousRule = await getRuleById(input.id);
        
        await deleteRule(input.id);

        // Registrar no histórico
        if (previousRule) {
          await createRuleHistory({
            ruleId: input.id,
            action: "DELETE",
            previousState: previousRule,
            changedBy: ctx.user.openId,
          });
        }

        return { success: true };
      }),

    // Ativar/desativar regra
    toggle: protectedProcedure
      .input(z.object({ 
        id: z.number(),
        isActive: z.boolean(),
      }))
      .mutation(async ({ input, ctx }) => {
        const previousRule = await getRuleById(input.id);
        
        const updatedRule = await toggleRuleActive(input.id, input.isActive);

        // Registrar no histórico
        if (updatedRule) {
          await createRuleHistory({
            ruleId: input.id,
            action: input.isActive ? "ACTIVATE" : "DEACTIVATE",
            previousState: previousRule,
            newState: updatedRule,
            changedBy: ctx.user.openId,
          });
        }

        return updatedRule;
      }),

    // Obter histórico de alterações de uma regra
    history: publicProcedure
      .input(z.object({ ruleId: z.number() }))
      .query(async ({ input }) => {
        return getRuleHistory(input.ruleId);
      }),
  }),

  // ==================== AUDIT ROUTER ====================
  audit: router({
    // Listar auditorias de transações
    list: publicProcedure
      .input(z.object({ limit: z.number().default(100) }).optional())
      .query(async ({ input }) => {
        return getTransactionAudits(input?.limit ?? 100);
      }),
  }),

  // ==================== METRICS ROUTER ====================
  metrics: router({
    // Obter métricas do dashboard
    get: publicProcedure.query(async () => {
      return getMetrics();
    }),
  }),
});

export type AppRouter = typeof appRouter;
