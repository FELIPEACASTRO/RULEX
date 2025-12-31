/**
 * RuleFormDialog - Componente unificado para criação e edição de regras
 * 
 * Features:
 * - Validação com Zod
 * - Tipos do backend (RuleConfiguration)
 * - Campos dinâmicos via fieldDictionary API
 * - Loading states
 * - Acessibilidade completa (focus trap, keyboard navigation)
 * - Suporte a criação e edição
 * 
 * @version 2.0.0
 */

// TODO: Implementar RuleFormDialog componente
// export { RuleFormDialog } from './RuleFormDialog';
export { useRuleForm } from './useRuleForm';
export { 
  ruleFormSchema, 
  validateValueByOperator,
  getPlaceholderForOperator,
  MAX_CONDITIONS,
  type RuleFormData 
} from './schema';
export type { RuleFormDialogProps } from './types';
