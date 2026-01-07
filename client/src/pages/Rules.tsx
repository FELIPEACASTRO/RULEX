import { useCallback, useEffect, useMemo, useState } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Textarea } from '@/components/ui/textarea';
import { Badge } from '@/components/ui/badge';
import { Plus, Edit2, Trash2, ToggleRight, Loader2, AlertTriangle, Filter, Layers, Search } from 'lucide-react';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
  DialogTrigger,
} from '@/components/ui/dialog';
import {
  AlertDialog,
  AlertDialogAction,
  AlertDialogCancel,
  AlertDialogContent,
  AlertDialogDescription,
  AlertDialogFooter,
  AlertDialogHeader,
  AlertDialogTitle,
} from '@/components/ui/alert-dialog';
import {
  validateValueByOperator,
  getPlaceholderForOperator,
  MAX_CONDITIONS,
} from '@/components/RuleFormDialog/schema';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  FieldDictionaryItem,
  RuleConfiguration,
  ComplexRuleDTO,
  createRule,
  deleteRule,
  listFieldDictionary,
  listRules,
  listComplexRules,
  toggleRuleStatus,
  toggleComplexRuleStatus,
  updateRule,
  deleteComplexRule,
} from '@/lib/javaApi';
import { toast } from 'sonner';

/**
 * Página de configuração dinâmica de regras.
 */
export default function Rules() {
  const queryClient = useQueryClient();
  const { data, isLoading, isError, error } = useQuery({
    queryKey: ['rules'],
    queryFn: () => listRules(),
    retry: 1,
  });

  const fieldDictionaryQuery = useQuery({
    queryKey: ['fieldDictionary'],
    // Defaults used by the backend seeder for a catalog-driven FE.
    queryFn: () => listFieldDictionary({ workflow: 'BRZLCREDIT', recordType: 'CRTRAN25', portfolio: '*' }),
    retry: 1,
  });

  // Query para regras complexas
  const { data: complexRulesData, isLoading: isLoadingComplex } = useQuery({
    queryKey: ['complexRules'],
    queryFn: () => listComplexRules(),
    retry: 1,
  });

  // Estado para filtro de tipo de regra
  const [ruleTypeFilter, setRuleTypeFilter] = useState<'all' | 'simple' | 'complex'>('all');
  const [searchTerm, setSearchTerm] = useState('');

  const simpleRules = useMemo(() => data ?? [], [data]);
  const complexRules = useMemo(() => complexRulesData ?? [], [complexRulesData]);

  // Interface unificada para exibição
  type UnifiedRule = {
    id: string | number;
    name: string;
    description: string;
    type: 'simple' | 'complex';
    ruleType?: string;
    classification?: string;
    decision?: string;
    enabled: boolean;
    weight?: number;
    priority?: number;
    threshold?: number;
    severity?: number;
    conditionsCount: number;
    original: RuleConfiguration | ComplexRuleDTO;
  };

  // Combinar regras simples e complexas em uma lista unificada
  const unifiedRules = useMemo((): UnifiedRule[] => {
    const simple: UnifiedRule[] = simpleRules.map((r: RuleConfiguration) => ({
      id: r.id,
      name: r.ruleName,
      description: r.description ?? '',
      type: 'simple' as const,
      ruleType: r.ruleType,
      classification: r.classification,
      enabled: r.enabled,
      weight: r.weight,
      threshold: r.threshold,
      conditionsCount: r.conditions?.length ?? 0,
      original: r,
    }));

    const complex: UnifiedRule[] = complexRules.map((r: ComplexRuleDTO) => ({
      id: r.id ?? r.key,
      name: r.title || r.key,
      description: r.description ?? '',
      type: 'complex' as const,
      decision: r.decision,
      enabled: r.enabled,
      priority: r.priority,
      severity: r.severity,
      conditionsCount: r.rootConditionGroup?.conditions?.length ?? 0,
      original: r,
    }));

    return [...simple, ...complex];
  }, [simpleRules, complexRules]);

  // Filtrar regras baseado no filtro de tipo e busca
  const filteredRules = useMemo(() => {
    return unifiedRules.filter((rule) => {
      // Filtro por tipo
      if (ruleTypeFilter !== 'all' && rule.type !== ruleTypeFilter) {
        return false;
      }
      // Filtro por busca
      if (searchTerm) {
        const search = searchTerm.toLowerCase();
        return (
          rule.name.toLowerCase().includes(search) ||
          rule.description.toLowerCase().includes(search)
        );
      }
      return true;
    });
  }, [unifiedRules, ruleTypeFilter, searchTerm]);

  // Manter compatibilidade com código existente
  const rules = simpleRules;
  const [editingRule, setEditingRule] = useState<RuleConfiguration | null>(null);
  const [showDialog, setShowDialog] = useState(false);
  const [formData, setFormData] = useState({
    ruleName: '',
    description: '',
    ruleType: 'SECURITY' as RuleConfiguration['ruleType'],
    threshold: 0,
    weight: 0,
    enabled: true,
    classification: 'SUSPICIOUS' as RuleConfiguration['classification'],
    parameters: '' as string,
    logicOperator: 'AND' as RuleConfiguration['logicOperator'],
    conditions: [] as RuleConfiguration['conditions'],
  });

  // P0-04: Estado para unsaved changes warning
  const [isDirty, setIsDirty] = useState(false);
  const [showUnsavedWarning, setShowUnsavedWarning] = useState(false);
  const [pendingClose, setPendingClose] = useState(false);

  // P1-09: Estado para confirmação de delete
  const [deleteConfirmId, setDeleteConfirmId] = useState<number | null>(null);

  // P1-02: Estado para erros de validação
  const [validationErrors, setValidationErrors] = useState<Record<string, string>>({});

  const invalidateRules = () => {
    queryClient.invalidateQueries({ queryKey: ['rules'] });
    queryClient.invalidateQueries({ queryKey: ['complexRules'] });
  };

  // P0-04: Marcar como dirty quando formData muda
  const updateFormData = useCallback((updates: Partial<typeof formData>) => {
    setFormData(prev => ({ ...prev, ...updates }));
    setIsDirty(true);
  }, []);

  // P0-04: Handler para fechar dialog com verificação de unsaved changes
  const handleDialogClose = useCallback((open: boolean) => {
    if (!open && isDirty) {
      setShowUnsavedWarning(true);
      setPendingClose(true);
      return;
    }
    setShowDialog(open);
    if (!open) {
      setIsDirty(false);
      setValidationErrors({});
    }
  }, [isDirty]);

  // P0-04: Confirmar descarte de alterações
  const confirmDiscard = useCallback(() => {
    setShowUnsavedWarning(false);
    setPendingClose(false);
    setShowDialog(false);
    setIsDirty(false);
    setValidationErrors({});
    setEditingRule(null);
  }, []);

  // P0-04: Cancelar descarte
  const cancelDiscard = useCallback(() => {
    setShowUnsavedWarning(false);
    setPendingClose(false);
  }, []);

  const saveRule = useMutation({
    mutationFn: async () => {
      const payload: Omit<RuleConfiguration, 'id' | 'version'> & { version?: number } = {
        ruleName: formData.ruleName,
        description: formData.description,
        ruleType: formData.ruleType,
        threshold: formData.threshold,
        weight: formData.weight,
        enabled: formData.enabled,
        classification: formData.classification,
        parameters: formData.parameters?.trim() ? formData.parameters : null,
        // Backend exige: conditions + logicOperator.
        conditions: formData.conditions ?? [],
        logicOperator: formData.logicOperator ?? 'AND',
      };

      if (editingRule) {
        // P0-05: Enviar version para optimistic locking
        payload.version = editingRule.version;
        return updateRule(editingRule.id, payload);
      }
      return createRule(payload);
    },
    onSuccess: () => {
      toast.success(editingRule ? 'Regra atualizada com sucesso!' : 'Regra criada com sucesso!');
      setShowDialog(false);
      setEditingRule(null);
      setIsDirty(false);
      setValidationErrors({});
      setFormData({
        ruleName: '',
        description: '',
        ruleType: 'SECURITY',
        threshold: 0,
        weight: 0,
        enabled: true,
        classification: 'SUSPICIOUS',
        parameters: '',
        logicOperator: 'AND',
        conditions: [],
      });
      invalidateRules();
    },
    onError: (error: Error) => {
      // P0-05: Tratar conflito de versão
      if (error.message.includes('409') || error.message.toLowerCase().includes('conflict')) {
        toast.error('Esta regra foi modificada por outro usuário. Recarregue a página e tente novamente.');
        invalidateRules();
        return;
      }
      // P1-07: Mensagens de erro amigáveis
      if (error.message.includes('400')) {
        toast.error('Dados inválidos. Verifique os campos e tente novamente.');
      } else if (error.message.includes('401') || error.message.includes('403')) {
        toast.error('Você não tem permissão para realizar esta ação.');
      } else if (error.message.includes('500')) {
        toast.error('Erro interno do servidor. Tente novamente mais tarde.');
      } else {
        toast.error(`Erro ao salvar regra: ${error.message}`);
      }
    },
  });

  const deleteMutation = useMutation({
    mutationFn: (id: number) => deleteRule(id),
    onSuccess: () => {
      toast.success('Regra deletada');
      invalidateRules();
    },
    onError: () => toast.error('Não foi possível deletar a regra'),
  });

  const toggleMutation = useMutation({
    mutationFn: (id: number) => {
      const current = rules.find((r: RuleConfiguration) => r.id === id);
      return toggleRuleStatus(id, !(current?.enabled ?? false));
    },
    onSuccess: () => invalidateRules(),
    onError: () => toast.error('Falha ao alternar regra'),
  });

  // Mutations para regras complexas
  const deleteComplexMutation = useMutation({
    mutationFn: (id: string) => deleteComplexRule(id),
    onSuccess: () => {
      toast.success('Regra complexa deletada');
      invalidateRules();
    },
    onError: () => toast.error('Não foi possível deletar a regra complexa'),
  });

  const toggleComplexMutation = useMutation({
    mutationFn: (id: string) => {
      const current = complexRules.find((r: ComplexRuleDTO) => r.id === id);
      return toggleComplexRuleStatus(id, !(current?.enabled ?? false));
    },
    onSuccess: () => invalidateRules(),
    onError: () => toast.error('Falha ao alternar regra complexa'),
  });

  const handleEdit = (rule: RuleConfiguration) => {
    setEditingRule(rule);
    setFormData({
      ruleName: rule.ruleName,
      description: rule.description ?? '',
      ruleType: rule.ruleType,
      threshold: rule.threshold,
      weight: rule.weight,
      enabled: rule.enabled,
      classification: rule.classification,
      parameters: rule.parameters ?? '',
      logicOperator: rule.logicOperator ?? 'AND',
      conditions: rule.conditions ?? [],
    });
    setShowDialog(true);
  };

  // P1-01/P1-02: Validação antes de salvar
  const validateForm = useCallback((): boolean => {
    const errors: Record<string, string> = {};

    // Validar ruleName
    if (!formData.ruleName.trim()) {
      errors.ruleName = 'Nome da regra é obrigatório';
    } else if (formData.ruleName.length < 3) {
      errors.ruleName = 'Nome deve ter pelo menos 3 caracteres';
    } else if (!/^[A-Z][A-Z0-9_]*$/.test(formData.ruleName)) {
      errors.ruleName = 'Nome deve começar com letra maiúscula e conter apenas letras maiúsculas, números e underscores';
    }

    // Validar threshold
    if (formData.threshold < 0 || formData.threshold > 1000) {
      errors.threshold = 'Threshold deve ser entre 0 e 1000';
    }

    // Validar weight
    if (formData.weight < 0 || formData.weight > 100) {
      errors.weight = 'Peso deve ser entre 0 e 100';
    }

    // Validar condições
    formData.conditions.forEach((c, idx) => {
      if (!c.field.trim()) {
        errors[`condition_${idx}_field`] = 'Campo é obrigatório';
      }
      const valueError = validateValueByOperator(c.operator, c.value);
      if (valueError) {
        errors[`condition_${idx}_value`] = valueError;
      }
    });

    // P1-03: Limite de condições
    if (formData.conditions.length > MAX_CONDITIONS) {
      errors.conditions = `Máximo de ${MAX_CONDITIONS} condições permitidas`;
    }

    setValidationErrors(errors);
    return Object.keys(errors).length === 0;
  }, [formData]);

  const handleSave = async () => {
    if (!validateForm()) {
      // Mostrar mensagem mais específica sobre os erros
      const errorKeys = Object.keys(validationErrors);
      const errorMessages = Object.values(validationErrors);
      if (errorMessages.length === 1) {
        toast.error(errorMessages[0]);
      } else if (errorMessages.length > 1) {
        toast.error(`Corrija ${errorMessages.length} erros no formulário: ${errorMessages.slice(0, 2).join(', ')}${errorMessages.length > 2 ? '...' : ''}`);
      } else {
        toast.error('Corrija os erros no formulário antes de salvar');
      }
      return;
    }
    saveRule.mutate();
  };

  // P1-09: Usar AlertDialog em vez de confirm()
  const handleDeleteClick = (id: number) => {
    setDeleteConfirmId(id);
  };

  const confirmDelete = () => {
    if (deleteConfirmId !== null) {
      deleteMutation.mutate(deleteConfirmId);
      setDeleteConfirmId(null);
    }
  };

  const cancelDelete = () => {
    setDeleteConfirmId(null);
  };

  const handleToggle = async (id: number) => {
    toggleMutation.mutate(id);
  };

  const getRuleTypeColor = (type: string) => {
    switch (type) {
      case 'SECURITY':
        return 'bg-blue-100 text-blue-800';
      case 'CONTEXT':
        return 'bg-purple-100 text-purple-800';
      case 'VELOCITY':
        return 'bg-orange-100 text-orange-800';
      case 'ANOMALY':
        return 'bg-pink-100 text-pink-800';
      default:
        return 'bg-gray-100 text-gray-800';
    }
  };

  const getClassificationColor = (classification: string) => {
    switch (classification) {
      case 'APPROVED':
        return 'bg-green-100 text-green-800';
      case 'SUSPICIOUS':
        return 'bg-amber-100 text-amber-800';
      case 'FRAUD':
        return 'bg-red-100 text-red-800';
      default:
        return 'bg-gray-100 text-gray-800';
    }
  };

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex flex-col gap-4">
        <div className="flex justify-between items-center">
          <div>
            <h1 className="text-3xl font-bold text-foreground">Configuração de Regras</h1>
            <p className="text-muted-foreground mt-1">
              Gerenciar regras de detecção de fraude ({filteredRules.length} de {unifiedRules.length} regras)
            </p>
          </div>
          <Dialog open={showDialog} onOpenChange={handleDialogClose}>
          <DialogTrigger asChild>
            <Button
              onClick={() => {
                setEditingRule(null);
                setIsDirty(false);
                setValidationErrors({});
                setFormData({
                  ruleName: '',
                  description: '',
                  ruleType: 'SECURITY',
                  threshold: 0,
                  weight: 0,
                  enabled: true,
                  classification: 'SUSPICIOUS',
                  parameters: '',
                  logicOperator: 'AND',
                  conditions: [],
                });
              }}
            >
              <Plus className="h-4 w-4 mr-2" />
              Nova Regra
            </Button>
          </DialogTrigger>
          <DialogContent className="sm:max-w-2xl">
            <DialogHeader>
              <DialogTitle>{editingRule ? 'Editar Regra' : 'Nova Regra'}</DialogTitle>
              <DialogDescription>
                Configure os parâmetros da regra de detecção de fraude
              </DialogDescription>
            </DialogHeader>
            <div className="space-y-4">
              <div>
                <label htmlFor="ruleName" className="block text-sm font-medium text-foreground mb-2">
                  Nome da Regra <span className="text-red-500">*</span>
                </label>
                <Input
                  id="ruleName"
                  value={formData.ruleName}
                  onChange={(e) => updateFormData({ ruleName: e.target.value.toUpperCase() })}
                  placeholder="Ex: HIGH_AMOUNT_RULE"
                  disabled={!!editingRule}
                  aria-invalid={!!validationErrors.ruleName}
                  aria-describedby={validationErrors.ruleName ? 'ruleName-error' : undefined}
                  className={validationErrors.ruleName ? 'border-red-500 focus:ring-red-500' : ''}
                />
                {validationErrors.ruleName && (
                  <p id="ruleName-error" className="mt-1 text-xs text-red-500" role="alert">
                    {validationErrors.ruleName}
                  </p>
                )}
                <p className="mt-1 text-xs text-muted-foreground">
                  Use UPPER_SNAKE_CASE (ex: HIGH_AMOUNT_RULE)
                </p>
              </div>
              <div>
                <label htmlFor="description" className="block text-sm font-medium text-foreground mb-2">
                  Descrição
                </label>
                <Input
                  id="description"
                  value={formData.description}
                  onChange={(e) => updateFormData({ description: e.target.value })}
                  placeholder="Descrição da regra"
                />
              </div>

              <div>
                <label htmlFor="parameters" className="block text-sm font-medium text-foreground mb-2">
                  Parâmetros (JSON) — opcional
                </label>
                <Textarea
                  id="parameters"
                  value={formData.parameters}
                  onChange={(e) => setFormData({ ...formData, parameters: e.target.value })}
                  placeholder={
                    formData.ruleType === 'VELOCITY'
                      ? `Ex (velocity/state):\n{\n  "velocity": {\n    "metric": "COUNT",\n    "dimension": "CUSTOMER",\n    "windowSeconds": 3600,\n    "operator": "GT",\n    "threshold": 3\n  }\n}`
                      : 'JSON livre (ex.: configs avançadas futuras)'
                  }
                  rows={formData.ruleType === 'VELOCITY' ? 8 : 4}
                />
                {formData.ruleType === 'VELOCITY' ? (
                  <p className="mt-1 text-xs text-muted-foreground">
                    Para regras de velocidade, use <code>metric</code> = <code>COUNT</code> ou{' '}
                    <code>SUM_AMOUNT</code>, <code>dimension</code> = <code>CUSTOMER</code> /{' '}
                    <code>MERCHANT</code> / <code>GLOBAL</code>.
                  </p>
                ) : null}
              </div>
              <div className="grid grid-cols-2 gap-4">
                <div>
                  <label htmlFor="ruleType" className="block text-sm font-medium text-foreground mb-2">
                    Tipo de Regra
                  </label>
                  <select
                    id="ruleType"
                    value={formData.ruleType}
                    onChange={(e) =>
                      setFormData({
                        ...formData,
                        ruleType: e.target.value as RuleConfiguration['ruleType'],
                      })
                    }
                    className="w-full px-3 py-2 border border-input rounded-lg bg-background text-foreground"
                  >
                    <option value="SECURITY">Segurança</option>
                    <option value="CONTEXT">Contexto</option>
                    <option value="VELOCITY">Velocidade</option>
                    <option value="ANOMALY">Anomalia</option>
                  </select>
                </div>
                <div>
                  <label htmlFor="classification" className="block text-sm font-medium text-foreground mb-2">
                    Classificação
                  </label>
                  <select
                    id="classification"
                    value={formData.classification}
                    onChange={(e) =>
                      setFormData({
                        ...formData,
                        classification: e.target.value as RuleConfiguration['classification'],
                      })
                    }
                    className="w-full px-3 py-2 border border-input rounded-lg bg-background text-foreground"
                  >
                    <option value="APPROVED">Aprovada</option>
                    <option value="SUSPICIOUS">Suspeita</option>
                    <option value="FRAUD">Fraude</option>
                  </select>
                </div>
              </div>
              <div className="grid grid-cols-2 gap-4">
                <div>
                  <label htmlFor="threshold" className="block text-sm font-medium text-foreground mb-2">
                    Threshold
                  </label>
                  <Input
                    id="threshold"
                    type="number"
                    value={formData.threshold}
                    onChange={(e) =>
                      setFormData({
                        ...formData,
                        threshold: e.target.value === '' ? 0 : parseInt(e.target.value, 10),
                      })
                    }
                    placeholder="0"
                    aria-invalid={!!validationErrors.threshold}
                    className={validationErrors.threshold ? 'border-red-500 focus:ring-red-500' : ''}
                  />
                  {validationErrors.threshold && (
                    <p className="mt-1 text-xs text-red-500" role="alert">
                      {validationErrors.threshold}
                    </p>
                  )}
                </div>
                <div>
                  <label htmlFor="weight" className="block text-sm font-medium text-foreground mb-2">
                    Peso (0-100)
                  </label>
                  <Input
                    id="weight"
                    type="number"
                    min="0"
                    max="100"
                    value={formData.weight}
                    onChange={(e) =>
                      setFormData({
                        ...formData,
                        weight: e.target.value === '' ? 0 : parseInt(e.target.value, 10),
                      })
                    }
                    placeholder="0"
                    aria-invalid={!!validationErrors.weight}
                    className={validationErrors.weight ? 'border-red-500 focus:ring-red-500' : ''}
                  />
                  {validationErrors.weight && (
                    <p className="mt-1 text-xs text-red-500" role="alert">
                      {validationErrors.weight}
                    </p>
                  )}
                </div>
              </div>

              {/* Condições / "subregras" (engine genérico) */}
              <div className="rounded-lg border border-border p-4">
                <div className="flex flex-col gap-1 sm:flex-row sm:items-center sm:justify-between">
                  <div>
                    <p className="text-sm font-medium text-foreground">Condições (subregras)</p>
                    <p className="text-xs text-muted-foreground">
                      Para regras genéricas, adicione condições. Para regras legadas por nome, pode deixar vazio.
                    </p>
                  </div>
                  <div className="flex items-center gap-2">
                    <label htmlFor="logicOperator" className="text-xs font-medium text-muted-foreground">
                      Operador
                    </label>
                    <select
                      id="logicOperator"
                      value={formData.logicOperator}
                      onChange={(e) =>
                        setFormData({
                          ...formData,
                          logicOperator: e.target.value as RuleConfiguration['logicOperator'],
                        })
                      }
                      className="px-3 py-2 border border-input rounded-lg bg-background text-foreground"
                    >
                      <option value="AND">AND</option>
                      <option value="OR">OR</option>
                    </select>
                  </div>
                </div>

                <div className="mt-3 space-y-3">
                  {formData.conditions.length === 0 ? (
                    <p className="text-sm text-muted-foreground">Nenhuma condição adicionada.</p>
                  ) : null}

                  {formData.conditions.map((c, idx) => {
                    const available = (fieldDictionaryQuery.data ?? []) as FieldDictionaryItem[];
                    const fieldOptions = available
                      .map((f) => (f.jsonPath?.startsWith('$.') ? f.jsonPath.slice(2) : f.jsonPath))
                      .filter(Boolean);
                    const currentField = available.find(
                      (f) => (f.jsonPath?.startsWith('$.') ? f.jsonPath.slice(2) : f.jsonPath) === c.field,
                    );
                    const typeForField = currentField?.type ?? 'unknown';

                    const baseOps: RuleConfiguration['conditions'][number]['operator'][] = [
                      'EQ',
                      'NE',
                      'GT',
                      'LT',
                      'GTE',
                      'LTE',
                      'IN',
                      'NOT_IN',
                      'BETWEEN',
                      'NOT_BETWEEN',
                      'CONTAINS',
                      'NOT_CONTAINS',
                      'STARTS_WITH',
                      'ENDS_WITH',
                      'MATCHES_REGEX',
                      'IS_NULL',
                      'IS_NOT_NULL',
                      'IS_TRUE',
                      'IS_FALSE',
                    ];
                    const opsFromCatalog =
                      currentField?.allowedOperators && currentField.allowedOperators.length > 0
                        ? (currentField.allowedOperators as RuleConfiguration['conditions'][number]['operator'][])
                        : null;
                    const opsFallback =
                      typeForField === 'number'
                        ? baseOps.filter(
                            (o) =>
                              ![
                                'CONTAINS',
                                'NOT_CONTAINS',
                                'STARTS_WITH',
                                'ENDS_WITH',
                                'MATCHES_REGEX',
                              ].includes(o),
                          )
                        : typeForField === 'boolean'
                          ? baseOps.filter((o) => ['IS_TRUE', 'IS_FALSE', 'IS_NULL', 'IS_NOT_NULL'].includes(o))
                          : baseOps;
                    const ops = opsFromCatalog ?? opsFallback;
                    const isUnary = ['IS_NULL', 'IS_NOT_NULL', 'IS_TRUE', 'IS_FALSE'].includes(c.operator);
                    const operatorLabel = (op: string) => {
                      const map: Record<string, string> = {
                        EQ: '== (EQ)',
                        NE: '!= (NE)',
                        GT: '> (GT)',
                        LT: '< (LT)',
                        GTE: '>= (GTE)',
                        LTE: '<= (LTE)',
                        IN: 'IN',
                        NOT_IN: 'NOT IN',
                        BETWEEN: 'BETWEEN',
                        NOT_BETWEEN: 'NOT BETWEEN',
                        CONTAINS: 'CONTAINS',
                        NOT_CONTAINS: 'NOT CONTAINS',
                        STARTS_WITH: 'STARTS WITH',
                        ENDS_WITH: 'ENDS WITH',
                        MATCHES_REGEX: 'MATCHES REGEX',
                        IS_NULL: 'IS NULL',
                        IS_NOT_NULL: 'IS NOT NULL',
                        IS_TRUE: 'IS TRUE',
                        IS_FALSE: 'IS FALSE',
                        '==': '==',
                        '!=': '!=',
                        '>': '>',
                        '<': '<',
                        '>=': '>=',
                        '<=': '<=',
                      };
                      return map[op] ?? op;
                    };

                    return (
                      <div key={`${idx}-${c.field}`} className="grid grid-cols-1 gap-2 sm:grid-cols-12 sm:items-end">
                        <div className="sm:col-span-5">
                          <label className="block text-xs font-medium text-muted-foreground mb-1">
                            Campo
                          </label>
                          {/* Combobox simples: permite qualquer campo, mas sugere os do catálogo */}
                          <Input
                            value={c.field}
                            onChange={(e) => {
                              const next = [...formData.conditions];
                              next[idx] = { ...next[idx], field: e.target.value };
                              setFormData({ ...formData, conditions: next });
                            }}
                            placeholder="Ex: consumerAuthenticationScore"
                            list="rule-condition-fields"
                            aria-invalid={!!validationErrors[`condition_${idx}_field`]}
                            className={validationErrors[`condition_${idx}_field`] ? 'border-red-500 focus:ring-red-500' : ''}
                          />
                          {validationErrors[`condition_${idx}_field`] && (
                            <p className="mt-1 text-xs text-red-500" role="alert">
                              {validationErrors[`condition_${idx}_field`]}
                            </p>
                          )}
                          <datalist id="rule-condition-fields">
                            {fieldOptions.map((f) => (
                              <option key={f} value={f} />
                            ))}
                          </datalist>
                          {fieldDictionaryQuery.isError ? (
                            <p className="mt-1 text-xs text-muted-foreground">
                              Catálogo de campos indisponível; você ainda pode digitar o nome do campo.
                            </p>
                          ) : null}
                        </div>
                        <div className="sm:col-span-3">
                          <label className="block text-xs font-medium text-muted-foreground mb-1">
                            Operador
                          </label>
                          <select
                            value={c.operator}
                            onChange={(e) => {
                              const next = [...formData.conditions];
                              next[idx] = {
                                ...next[idx],
                                operator: e.target.value as RuleConfiguration['conditions'][number]['operator'],
                                // Operadores unários não usam value.
                                value: ['IS_NULL', 'IS_NOT_NULL', 'IS_TRUE', 'IS_FALSE'].includes(e.target.value)
                                  ? ''
                                  : next[idx].value,
                              };
                              setFormData({ ...formData, conditions: next });
                            }}
                            className="w-full px-3 py-2 border border-input rounded-lg bg-background text-foreground"
                          >
                            {ops.map((op) => (
                              <option key={op} value={op}>
                                {operatorLabel(op)}
                              </option>
                            ))}
                          </select>
                        </div>
                        <div className="sm:col-span-3">
                          <label className="block text-xs font-medium text-muted-foreground mb-1">
                            Valor
                          </label>
                          {isUnary ? (
                            <Input value="" disabled placeholder="(não aplicável)" />
                          ) : (
                            <Input
                              value={c.value}
                              onChange={(e) => {
                                const next = [...formData.conditions];
                                next[idx] = { ...next[idx], value: e.target.value };
                                setFormData({ ...formData, conditions: next });
                              }}
                              placeholder={
                                c.operator === 'IN' || c.operator === 'NOT_IN'
                                  ? "Ex: [1,2,3] ou ['RU','CN']"
                                  : c.operator === 'BETWEEN' || c.operator === 'NOT_BETWEEN'
                                    ? 'Ex: 10,20 (ou 10..20)'
                                    : 'Ex: 10'
                              }
                              aria-invalid={!!validationErrors[`condition_${idx}_value`]}
                              className={validationErrors[`condition_${idx}_value`] ? 'border-red-500 focus:ring-red-500' : ''}
                            />
                          )}
                          {validationErrors[`condition_${idx}_value`] && (
                            <p className="mt-1 text-xs text-red-500" role="alert">
                              {validationErrors[`condition_${idx}_value`]}
                            </p>
                          )}
                        </div>
                        <div className="sm:col-span-1 flex sm:justify-end">
                          <Button
                            type="button"
                            variant="ghost"
                            onClick={() => {
                              const next = formData.conditions.filter((_, i) => i !== idx);
                              setFormData({ ...formData, conditions: next });
                            }}
                            title="Remover condição"
                          >
                            Remover
                          </Button>
                        </div>
                      </div>
                    );
                  })}

                  <Button
                    type="button"
                    variant="outline"
                    onClick={() =>
                      setFormData({
                        ...formData,
                        conditions: [
                          ...formData.conditions,
                          { field: '', operator: 'EQ', value: '' } as RuleConfiguration['conditions'][number],
                        ],
                      })
                    }
                  >
                    Adicionar condição
                  </Button>
                </div>
              </div>

              <div className="flex items-center gap-2">
                <input
                  type="checkbox"
                  id="enabled"
                  checked={formData.enabled}
                  onChange={(e) => setFormData({ ...formData, enabled: e.target.checked })}
                  className="w-4 h-4 rounded border-input"
                />
                <label htmlFor="enabled" className="text-sm font-medium text-foreground">
                  Habilitada
                </label>
              </div>
              <div className="flex justify-end gap-2 pt-4">
                <Button variant="outline" onClick={() => handleDialogClose(false)} disabled={saveRule.isPending}>
                  Cancelar
                </Button>
                <Button onClick={handleSave} disabled={saveRule.isPending}>
                  {saveRule.isPending && <Loader2 className="h-4 w-4 mr-2 animate-spin" />}
                  {editingRule ? 'Atualizar' : 'Criar'}
                </Button>
              </div>
            </div>
          </DialogContent>
        </Dialog>
        </div>

        {/* Filtros */}
        <div className="flex flex-wrap gap-4 items-center">
          <div className="relative flex-1 min-w-[200px] max-w-md">
            <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-muted-foreground" />
            <Input
              placeholder="Buscar regras..."
              value={searchTerm}
              onChange={(e) => setSearchTerm(e.target.value)}
              className="pl-10"
            />
          </div>
          <Select value={ruleTypeFilter} onValueChange={(v) => setRuleTypeFilter(v as 'all' | 'simple' | 'complex')}>
            <SelectTrigger className="w-[180px]">
              <Filter className="h-4 w-4 mr-2" />
              <SelectValue placeholder="Filtrar por tipo" />
            </SelectTrigger>
            <SelectContent>
              <SelectItem value="all">Todas ({unifiedRules.length})</SelectItem>
              <SelectItem value="simple">Simples ({simpleRules.length})</SelectItem>
              <SelectItem value="complex">Complexas ({complexRules.length})</SelectItem>
            </SelectContent>
          </Select>
        </div>
      </div>

      {/* Tabela de Regras */}
      <Card>
        <CardHeader>
          <CardTitle>Regras Configuradas</CardTitle>
          <CardDescription>
            Mostrando {filteredRules.length} de {unifiedRules.length} regras
            {ruleTypeFilter !== 'all' && ` (filtro: ${ruleTypeFilter === 'simple' ? 'Simples' : 'Complexas'})`}
          </CardDescription>
        </CardHeader>
        <CardContent>
          {isError && (
            <div className="mb-4 rounded-lg border border-red-200 bg-red-50 p-3 text-red-800" role="alert">
              Erro ao carregar regras: {error instanceof Error ? error.message : 'erro inesperado'}
            </div>
          )}
          {(isLoading || isLoadingComplex) ? (
            <div className="flex items-center justify-center h-64">
              <div className="text-center">
                <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-primary mx-auto mb-2"></div>
                <p className="text-muted-foreground">Carregando regras...</p>
              </div>
            </div>
          ) : filteredRules.length === 0 ? (
            <div className="flex items-center justify-center h-64">
              <p className="text-muted-foreground">
                {searchTerm || ruleTypeFilter !== 'all'
                  ? 'Nenhuma regra encontrada com os filtros aplicados'
                  : 'Nenhuma regra configurada'}
              </p>
            </div>
          ) : (
            <div className="overflow-x-auto">
              <table className="w-full">
                <thead className="border-b border-border">
                  <tr>
                    <th className="text-left py-3 px-4 font-semibold text-sm text-foreground">Nome da Regra</th>
                    <th className="text-center py-3 px-4 font-semibold text-sm text-foreground">Categoria</th>
                    <th className="text-left py-3 px-4 font-semibold text-sm text-foreground">Tipo/Decisão</th>
                    <th className="text-center py-3 px-4 font-semibold text-sm text-foreground">Peso/Prioridade</th>
                    <th className="text-left py-3 px-4 font-semibold text-sm text-foreground">Classificação</th>
                    <th className="text-center py-3 px-4 font-semibold text-sm text-foreground">Status</th>
                    <th className="text-center py-3 px-4 font-semibold text-sm text-foreground">Ações</th>
                  </tr>
                </thead>
                <tbody>
                  {filteredRules.map((rule) => (
                    <tr key={`${rule.type}-${rule.id}`} className="border-b border-border hover:bg-muted/50 transition-colors">
                      <td className="py-3 px-4">
                        <div className="flex flex-col">
                          <span className="text-sm font-medium text-foreground">{rule.name}</span>
                          {rule.description && (
                            <span className="text-xs text-muted-foreground truncate max-w-[300px]" title={rule.description}>
                              {rule.description}
                            </span>
                          )}
                        </div>
                      </td>
                      <td className="py-3 px-4 text-sm text-center">
                        <Badge
                          variant="outline"
                          className={rule.type === 'complex'
                            ? 'bg-indigo-50 text-indigo-700 border-indigo-200'
                            : 'bg-slate-50 text-slate-700 border-slate-200'}
                        >
                          <Layers className="h-3 w-3 mr-1" />
                          {rule.type === 'complex' ? 'Complexa' : 'Simples'}
                        </Badge>
                      </td>
                      <td className="py-3 px-4 text-sm">
                        {rule.type === 'simple' ? (
                          <Badge className={getRuleTypeColor(rule.ruleType ?? '')}>
                            {rule.ruleType}
                          </Badge>
                        ) : (
                          <Badge className="bg-emerald-100 text-emerald-800">
                            {rule.decision}
                          </Badge>
                        )}
                      </td>
                      <td className="py-3 px-4 text-sm text-center text-foreground">
                        {rule.type === 'simple' ? `${rule.weight}%` : `P${rule.priority}`}
                      </td>
                      <td className="py-3 px-4 text-sm">
                        {rule.type === 'simple' ? (
                          <Badge className={getClassificationColor(rule.classification ?? '')}>
                            {rule.classification}
                          </Badge>
                        ) : (
                          <span className="text-xs text-muted-foreground">
                            Severidade: {rule.severity}
                          </span>
                        )}
                      </td>
                      <td className="py-3 px-4 text-sm text-center">
                        <Badge variant={rule.enabled ? 'default' : 'secondary'}>
                          {rule.enabled ? 'Ativa' : 'Inativa'}
                        </Badge>
                      </td>
                      <td className="py-3 px-4 text-center space-x-2">
                        <Button
                          variant="ghost"
                          size="sm"
                          onClick={() => {
                            if (rule.type === 'simple') {
                              handleToggle(rule.id as number);
                            } else {
                              toggleComplexMutation.mutate(rule.id as string);
                            }
                          }}
                          title={rule.enabled ? 'Desativar' : 'Ativar'}
                        >
                          <ToggleRight className="h-4 w-4" />
                        </Button>
                        {rule.type === 'simple' && (
                          <Button
                            variant="ghost"
                            size="sm"
                            onClick={() => handleEdit(rule.original as RuleConfiguration)}
                            title="Editar"
                          >
                            <Edit2 className="h-4 w-4" />
                          </Button>
                        )}
                        <Button
                          variant="ghost"
                          size="sm"
                          onClick={() => {
                            if (rule.type === 'simple') {
                              handleDeleteClick(rule.id as number);
                            } else {
                              deleteComplexMutation.mutate(rule.id as string);
                            }
                          }}
                          title="Deletar"
                          className="text-red-600 hover:text-red-700"
                        >
                          <Trash2 className="h-4 w-4" />
                        </Button>
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}
        </CardContent>
      </Card>

      {/* P1-09: AlertDialog para confirmação de delete */}
      <AlertDialog open={deleteConfirmId !== null} onOpenChange={(open) => !open && cancelDelete()}>
        <AlertDialogContent>
          <AlertDialogHeader>
            <AlertDialogTitle>Confirmar exclusão</AlertDialogTitle>
            <AlertDialogDescription>
              Tem certeza que deseja deletar esta regra? Esta ação não pode ser desfeita.
            </AlertDialogDescription>
          </AlertDialogHeader>
          <AlertDialogFooter>
            <AlertDialogCancel onClick={cancelDelete}>Cancelar</AlertDialogCancel>
            <AlertDialogAction onClick={confirmDelete} className="bg-red-600 hover:bg-red-700">
              Deletar
            </AlertDialogAction>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>

      {/* P0-04: AlertDialog para unsaved changes warning */}
      <AlertDialog open={showUnsavedWarning} onOpenChange={(open) => !open && cancelDiscard()}>
        <AlertDialogContent>
          <AlertDialogHeader>
            <AlertDialogTitle className="flex items-center gap-2">
              <AlertTriangle className="h-5 w-5 text-amber-500" />
              Alterações não salvas
            </AlertDialogTitle>
            <AlertDialogDescription>
              Você tem alterações não salvas. Deseja descartá-las?
            </AlertDialogDescription>
          </AlertDialogHeader>
          <AlertDialogFooter>
            <AlertDialogCancel onClick={cancelDiscard}>Continuar editando</AlertDialogCancel>
            <AlertDialogAction onClick={confirmDiscard} className="bg-amber-600 hover:bg-amber-700">
              Descartar alterações
            </AlertDialogAction>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>
    </div>
  );
}
