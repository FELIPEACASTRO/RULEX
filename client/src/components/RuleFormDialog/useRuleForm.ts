/**
 * Hook customizado para gerenciar o formulário de regras
 */

import { useCallback, useEffect, useMemo } from 'react';
import { useForm, useFieldArray } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { toast } from 'sonner';

import {
  createRule,
  updateRule,
  listFieldDictionary,
  type RuleConfiguration,
  type RuleCondition,
  type FieldDictionaryItem,
} from '@/lib/javaApi';

import {
  ruleFormSchema,
  defaultRuleFormValues,
  defaultConditionValues,
  type RuleFormData,
  type ConditionFormData,
} from './schema';

import {
  FALLBACK_FIELDS,
  OPERATORS_BY_TYPE,
  OPERATORS,
  type ConditionFieldOption,
  type ConditionOperator,
} from './types';

// ============================================
// HELPER PARA CONVERTER CONDIÇÕES
// ============================================

function mapRuleConditionsToFormData(conditions: RuleConfiguration['conditions']): ConditionFormData[] {
  return conditions.map(c => ({
    field: c.field,
    operator: c.operator as ConditionFormData['operator'],
    value: c.value,
  }));
}

function mapFormDataToRuleConditions(conditions: ConditionFormData[]): RuleCondition[] {
  return conditions.map(c => ({
    field: c.field,
    operator: c.operator as RuleCondition['operator'],
    value: c.value,
  }));
}

// ============================================
// TIPOS DO HOOK
// ============================================

interface UseRuleFormOptions {
  rule?: RuleConfiguration | null;
  onSuccess?: (rule: RuleConfiguration) => void;
  onError?: (error: Error) => void;
}

interface UseRuleFormReturn {
  // Form
  form: ReturnType<typeof useForm<RuleFormData>>;
  conditionsFieldArray: ReturnType<typeof useFieldArray<RuleFormData, 'conditions'>>;

  // Estado
  isLoading: boolean;
  isSaving: boolean;
  isEditing: boolean;

  // Campos disponíveis
  availableFields: ConditionFieldOption[];
  isLoadingFields: boolean;

  // Ações
  handleSubmit: () => void;
  handleReset: () => void;
  addCondition: () => void;
  removeCondition: (index: number) => void;

  // Helpers
  getOperatorsForField: (fieldName: string) => typeof OPERATORS;
  getFieldInfo: (fieldName: string) => ConditionFieldOption | undefined;
}

// ============================================
// HOOK PRINCIPAL
// ============================================

export function useRuleForm({
  rule,
  onSuccess,
  onError,
}: UseRuleFormOptions = {}): UseRuleFormReturn {
  const queryClient = useQueryClient();
  const isEditing = !!rule;

  // ============================================
  // BUSCAR CAMPOS DO DICIONÁRIO
  // ============================================

  const {
    data: fieldDictionary,
    isLoading: isLoadingFields,
  } = useQuery({
    queryKey: ['fieldDictionary'],
    queryFn: () => listFieldDictionary({
      workflow: 'BRZLCREDIT',
      recordType: 'CRTRAN25',
      portfolio: '*',
    }),
    staleTime: 5 * 60 * 1000, // 5 minutos
    retry: 1,
  });

  // Converter dicionário da API para opções de campo
  const availableFields = useMemo<ConditionFieldOption[]>(() => {
    if (!fieldDictionary || fieldDictionary.length === 0) {
      return FALLBACK_FIELDS;
    }

    return fieldDictionary.map((item: FieldDictionaryItem) => {
      // Extrair nome do campo do jsonPath ($.fieldName -> fieldName)
      const fieldName = item.jsonPath?.startsWith('$.')
        ? item.jsonPath.slice(2)
        : item.jsonPath || '';

      // Mapear tipo
      const typeMap: Record<string, 'string' | 'number' | 'boolean' | 'date'> = {
        string: 'string',
        number: 'number',
        integer: 'number',
        boolean: 'boolean',
        date: 'date',
      };

      return {
        value: fieldName,
        label: fieldName, // Idealmente viria um label amigável da API
        type: typeMap[item.type?.toLowerCase() || 'string'] || 'string',
        allowedOperators: item.allowedOperators as ConditionOperator[] | undefined,
      };
    }).filter(f => f.value); // Remover campos vazios
  }, [fieldDictionary]);

  // ============================================
  // CONFIGURAR FORMULÁRIO
  // ============================================

  const form = useForm<RuleFormData>({
    resolver: zodResolver(ruleFormSchema),
    defaultValues: rule
      ? {
          ruleName: rule.ruleName,
          description: rule.description || '',
          ruleType: rule.ruleType,
          classification: rule.classification,
          threshold: rule.threshold,
          weight: rule.weight,
          enabled: rule.enabled,
          parameters: rule.parameters || '',
          conditions: mapRuleConditionsToFormData(rule.conditions),
          logicOperator: rule.logicOperator,
        }
      : defaultRuleFormValues,
    mode: 'onChange',
  });

  // Field array para condições
  const conditionsFieldArray = useFieldArray({
    control: form.control,
    name: 'conditions',
  });

  // Resetar form quando a regra mudar
  useEffect(() => {
    if (rule) {
      form.reset({
        ruleName: rule.ruleName,
        description: rule.description || '',
        ruleType: rule.ruleType,
        classification: rule.classification,
        threshold: rule.threshold,
        weight: rule.weight,
        enabled: rule.enabled,
        parameters: rule.parameters || '',
        conditions: mapRuleConditionsToFormData(rule.conditions),
        logicOperator: rule.logicOperator,
      });
    } else {
      form.reset(defaultRuleFormValues);
    }
  }, [rule, form]);

  // ============================================
  // MUTATIONS
  // ============================================

  const createMutation = useMutation({
    mutationFn: (data: Omit<RuleConfiguration, 'id' | 'version'>) => createRule(data),
    onSuccess: (result) => {
      toast.success('Regra criada com sucesso!');
      queryClient.invalidateQueries({ queryKey: ['rules'] });
      onSuccess?.(result);
    },
    onError: (error: Error) => {
      toast.error(`Erro ao criar regra: ${error.message}`);
      onError?.(error);
    },
  });

  const updateMutation = useMutation({
    mutationFn: ({ id, data }: { id: number; data: Partial<RuleConfiguration> }) =>
      updateRule(id, data),
    onSuccess: (result) => {
      toast.success('Regra atualizada com sucesso!');
      queryClient.invalidateQueries({ queryKey: ['rules'] });
      onSuccess?.(result);
    },
    onError: (error: Error) => {
      toast.error(`Erro ao atualizar regra: ${error.message}`);
      onError?.(error);
    },
  });

  const isSaving = createMutation.isPending || updateMutation.isPending;

  // ============================================
  // HANDLERS
  // ============================================

  const handleSubmit = useCallback(() => {
    form.handleSubmit((data) => {
      const payload: Omit<RuleConfiguration, 'id' | 'version'> = {
        ruleName: data.ruleName,
        description: data.description || null,
        ruleType: data.ruleType,
        classification: data.classification,
        threshold: data.threshold,
        weight: data.weight,
        enabled: data.enabled,
        parameters: data.parameters?.trim() || null,
        conditions: mapFormDataToRuleConditions(data.conditions),
        logicOperator: data.logicOperator,
      };

      if (isEditing && rule) {
        updateMutation.mutate({ id: rule.id, data: payload });
      } else {
        createMutation.mutate(payload);
      }
    })();
  }, [form, isEditing, rule, createMutation, updateMutation]);

  const handleReset = useCallback(() => {
    form.reset(rule ? {
      ruleName: rule.ruleName,
      description: rule.description || '',
      ruleType: rule.ruleType,
      classification: rule.classification,
      threshold: rule.threshold,
      weight: rule.weight,
      enabled: rule.enabled,
      parameters: rule.parameters || '',
      conditions: mapRuleConditionsToFormData(rule.conditions),
      logicOperator: rule.logicOperator,
    } : defaultRuleFormValues);
  }, [form, rule]);

  const addCondition = useCallback(() => {
    conditionsFieldArray.append(defaultConditionValues);
  }, [conditionsFieldArray]);

  const removeCondition = useCallback((index: number) => {
    conditionsFieldArray.remove(index);
  }, [conditionsFieldArray]);

  // ============================================
  // HELPERS
  // ============================================

  const getOperatorsForField = useCallback((fieldName: string) => {
    const field = availableFields.find(f => f.value === fieldName);

    // Se o campo tem operadores específicos da API, usar esses
    if (field?.allowedOperators && field.allowedOperators.length > 0) {
      return OPERATORS.filter(op => field.allowedOperators!.includes(op.value));
    }

    // Senão, usar operadores baseados no tipo
    const fieldType = field?.type || 'string';
    const allowedOps = OPERATORS_BY_TYPE[fieldType] || OPERATORS_BY_TYPE.string;
    return OPERATORS.filter(op => allowedOps.includes(op.value));
  }, [availableFields]);

  const getFieldInfo = useCallback((fieldName: string) => {
    return availableFields.find(f => f.value === fieldName);
  }, [availableFields]);

  // ============================================
  // RETURN
  // ============================================

  return {
    form,
    conditionsFieldArray,
    isLoading: isLoadingFields,
    isSaving,
    isEditing,
    availableFields,
    isLoadingFields,
    handleSubmit,
    handleReset,
    addCondition,
    removeCondition,
    getOperatorsForField,
    getFieldInfo,
  };
}
