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
 * - Preview JSON antes de salvar
 * - Suporte a todos os 52 operadores
 * 
 * @version 2.0.0
 */

export { RuleFormDialog, default } from './RuleFormDialog';
export { useRuleForm } from './useRuleForm';
export { 
  ruleFormSchema, 
  validateValueByOperator,
  getPlaceholderForOperator,
  MAX_CONDITIONS,
  type RuleFormData 
} from './schema';
export type { RuleFormDialogProps } from './types';
