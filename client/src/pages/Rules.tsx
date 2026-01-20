import { useCallback, useEffect, useMemo, useState } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Textarea } from '@/components/ui/textarea';
import { Badge } from '@/components/ui/badge';
import { Plus, Edit2, Trash2, ToggleRight, Loader2, AlertTriangle, Filter, Layers, Search, Play, BarChart2, ArrowRight } from 'lucide-react';
import { useLocation } from 'wouter';
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
import { Switch } from '@/components/ui/switch';
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
  RuleSimulationResult,
  RuleBacktestResult,
  TransactionRequest,
  createRule,
  deleteRule,
  listFieldDictionary,
  listRules,
  listComplexRules,
  requestCreateApproval,
  requestUpdateApproval,
  requestDeleteApproval,
  simulateRule,
  backtestRule,
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
  const [, setLocation] = useLocation();
  const DEFAULT_SIMULATION_PAYLOAD = `{
  "externalTransactionId": "TXN-SIM-001",
  "customerIdFromHeader": "CUST-123",
  "customerAcctNumber": 1234567890,
  "pan": "4111111111111111",
  "merchantId": "MERCH-TEST-001",
  "merchantCountryCode": "BR",
  "transactionAmount": 15000,
  "transactionDate": 20241216,
  "transactionTime": 143000,
  "transactionCurrencyCode": 986,
  "mcc": 5411,
  "posEntryMode": "010",
  "consumerAuthenticationScore": 500,
  "externalScore3": 85,
  "cavvResult": 1,
  "eciIndicator": 5,
  "atcCard": 10,
  "atcHost": 10,
  "tokenAssuranceLevel": 2,
  "availableCredit": 100000,
  "cardCashBalance": 50000,
  "cardDelinquentAmount": 0
}`;

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
    complexId?: string;
    complexKey?: string;
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
      complexId: r.id,
      complexKey: r.key,
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
  const [requiresApproval, setRequiresApproval] = useState(false);

  // P1-09: Estado para confirmação de delete
  const [deleteConfirmId, setDeleteConfirmId] = useState<number | null>(null);

  // P1-02: Estado para erros de validação
  const [validationErrors, setValidationErrors] = useState<Record<string, string>>({});

  // P0: Simulação e backtest
  const [isSimulationOpen, setIsSimulationOpen] = useState(false);
  const [simulationRule, setSimulationRule] = useState<RuleConfiguration | null>(null);
  const [simulationPayload, setSimulationPayload] = useState(DEFAULT_SIMULATION_PAYLOAD);
  const [simulationResult, setSimulationResult] = useState<RuleSimulationResult | null>(null);
  const [simulationError, setSimulationError] = useState<string | null>(null);
  const [isSimulating, setIsSimulating] = useState(false);

  const [isBacktestOpen, setIsBacktestOpen] = useState(false);
  const [backtestRule, setBacktestRule] = useState<RuleConfiguration | null>(null);
  const [backtestStart, setBacktestStart] = useState(() => {
    const date = new Date();
    date.setDate(date.getDate() - 7);
    return date.toISOString().slice(0, 16);
  });
  const [backtestEnd, setBacktestEnd] = useState(() => new Date().toISOString().slice(0, 16));
  const [backtestSampleSize, setBacktestSampleSize] = useState(1000);
  const [backtestResult, setBacktestResult] = useState<RuleBacktestResult | null>(null);
  const [backtestError, setBacktestError] = useState<string | null>(null);
  const [isBacktesting, setIsBacktesting] = useState(false);

  const normalizeDateTime = (value: string) => (value.length === 16 ? `${value}:00` : value);

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
      setRequiresApproval(false);
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
    setRequiresApproval(false);
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
        payload.version = editingRule.version;
        if (requiresApproval) {
          return requestUpdateApproval(editingRule.id, payload as RuleConfiguration);
        }
        return updateRule(editingRule.id, payload);
      }
      if (requiresApproval) {
        return requestCreateApproval(payload as RuleConfiguration);
      }
      return createRule(payload);
    },
    onSuccess: () => {
      toast.success(
        requiresApproval
          ? 'Solicitação enviada para aprovação'
          : editingRule
            ? 'Regra atualizada com sucesso!'
            : 'Regra criada com sucesso!'
      );
      setShowDialog(false);
      setEditingRule(null);
      setIsDirty(false);
      setValidationErrors({});
      setRequiresApproval(false);
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

  const deleteApprovalMutation = useMutation({
    mutationFn: (id: number) => requestDeleteApproval(id),
    onSuccess: () => {
      toast.success('Solicitação de exclusão enviada para aprovação');
      invalidateRules();
      setDeleteConfirmId(null);
    },
    onError: (error: Error) => toast.error(`Não foi possível solicitar aprovação: ${error.message}`),
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
    setRequiresApproval(false);
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

  const openSimulation = (rule: RuleConfiguration) => {
    setSimulationRule(rule);
    setSimulationPayload(DEFAULT_SIMULATION_PAYLOAD);
    setSimulationResult(null);
    setSimulationError(null);
    setIsSimulationOpen(true);
  };

  const runSimulation = async () => {
    if (!simulationRule) return;
    setIsSimulating(true);
    setSimulationError(null);
    setSimulationResult(null);
    try {
      const parsedPayload = JSON.parse(simulationPayload) as TransactionRequest;
      const result = await simulateRule(simulationRule, parsedPayload);
      setSimulationResult(result);
      toast.success('Simulação concluída!');
    } catch (error) {
      const message = error instanceof Error ? error.message : 'Erro desconhecido';
      setSimulationError(message);
      toast.error(`Erro na simulação: ${message}`);
    } finally {
      setIsSimulating(false);
    }
  };

  const openBacktest = (rule: RuleConfiguration) => {
    setBacktestRule(rule);
    setBacktestResult(null);
    setBacktestError(null);
    setIsBacktestOpen(true);
  };

  const runBacktest = async () => {
    if (!backtestRule) return;
    setIsBacktesting(true);
    setBacktestError(null);
    setBacktestResult(null);
    try {
      const result = await backtestRule(
        backtestRule.id,
        normalizeDateTime(backtestStart),
        normalizeDateTime(backtestEnd),
        backtestSampleSize,
      );
      setBacktestResult(result);
      toast.success('Backtest concluído!');
    } catch (error) {
      const message = error instanceof Error ? error.message : 'Erro desconhecido';
      setBacktestError(message);
      toast.error(`Erro no backtest: ${message}`);
    } finally {
      setIsBacktesting(false);
    }
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

  const getComplexStatusColor = (status?: ComplexRuleDTO['status']) => {
    switch (status) {
      case 'PUBLISHED':
        return 'bg-green-100 text-green-800';
      case 'DRAFT':
        return 'bg-gray-100 text-gray-800';
      case 'DEPRECATED':
        return 'bg-orange-100 text-orange-800';
      case 'TESTING':
        return 'bg-yellow-100 text-yellow-800';
      case 'ARCHIVED':
        return 'bg-red-100 text-red-800';
      default:
        return 'bg-gray-100 text-gray-800';
    }
  };

  const getDecisionLabel = (decision?: ComplexRuleDTO['decision']) => {
    switch (decision) {
      case 'APROVADO':
        return 'Aprovado';
      case 'SUSPEITA_DE_FRAUDE':
        return 'Suspeita de Fraude';
      case 'FRAUDE':
        return 'Fraude';
      default:
        return decision ?? '-';
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
                setRequiresApproval(false);
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
              <div className="flex items-center justify-between rounded-lg border border-border p-3">
                <div>
                  <p className="text-sm font-medium text-foreground">Governança</p>
                  <p className="text-xs text-muted-foreground">
                    Envie esta regra para aprovação antes de publicar.
                  </p>
                </div>
                <div className="flex items-center gap-2">
                  <Switch
                    id="requiresApproval"
                    checked={requiresApproval}
                    onCheckedChange={setRequiresApproval}
                  />
                </div>
              </div>
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
                  onChange={(e) => updateFormData({ parameters: e.target.value })}
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
                      updateFormData({
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
                      updateFormData({
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
                      updateFormData({
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
                      updateFormData({
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
                        updateFormData({
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
                              updateFormData({ conditions: next });
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
                              updateFormData({ conditions: next });
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
                                updateFormData({ conditions: next });
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
                              updateFormData({ conditions: next });
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
                      updateFormData({
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
                  onChange={(e) => updateFormData({ enabled: e.target.checked })}
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
        <Card className="border border-dashed">
          <CardContent className="flex flex-col gap-3 py-4 sm:flex-row sm:items-center sm:justify-between">
            <div>
              <p className="text-sm font-medium text-foreground">Precisa de regras avançadas?</p>
              <p className="text-xs text-muted-foreground">
                Use o construtor de regras complexas com grupos, condições aninhadas e templates.
              </p>
            </div>
            <Button onClick={() => setLocation('/rules')} variant="secondary">
              Ir para Regras Complexas
              <ArrowRight className="ml-2 h-4 w-4" />
            </Button>
          </CardContent>
        </Card>
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
                  {filteredRules.map((rule) => {
                    const complexId = rule.type === 'complex' ? (rule.original as ComplexRuleDTO).id : undefined;
                    const complexKey = rule.type === 'complex' ? (rule.original as ComplexRuleDTO).key : undefined;
                    const complexActionDisabled = rule.type === 'complex' && !complexId;
                    return (
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
                            {getDecisionLabel(rule.decision)}
                          </Badge>
                        )}
                      </td>
                      <td className="py-3 px-4 text-sm text-center text-foreground">
                        {rule.type === 'simple'
                          ? `${rule.weight}%`
                          : rule.priority !== undefined
                            ? `P${rule.priority}`
                            : '-'}
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
                        {rule.type === 'simple' ? (
                          <Badge variant={rule.enabled ? 'default' : 'secondary'}>
                            {rule.enabled ? 'Ativa' : 'Inativa'}
                          </Badge>
                        ) : (
                          <div className="flex flex-col items-center gap-1">
                            <Badge className={getComplexStatusColor((rule.original as ComplexRuleDTO).status)}>
                              {(rule.original as ComplexRuleDTO).status}
                            </Badge>
                            {!rule.enabled && (
                              <span className="text-xs text-muted-foreground">Inativa</span>
                            )}
                          </div>
                        )}
                      </td>
                      <td className="py-3 px-4 text-center space-x-2">
                        <Button
                          variant="ghost"
                          size="sm"
                          onClick={() => {
                            if (rule.type === 'simple') {
                              handleToggle(rule.id as number);
                            } else if (complexId) {
                              toggleComplexMutation.mutate(complexId);
                            } else {
                              toast.error(`Regra complexa sem ID válido (${complexKey ?? 'chave indisponível'}).`);
                            }
                          }}
                          title={rule.enabled ? 'Desativar' : 'Ativar'}
                          disabled={complexActionDisabled}
                        >
                          <ToggleRight className="h-4 w-4" />
                        </Button>
                        {rule.type === 'simple' ? (
                          <>
                            <Button
                              variant="ghost"
                              size="sm"
                              onClick={() => openSimulation(rule.original as RuleConfiguration)}
                              title="Simular regra"
                            >
                              <Play className="h-4 w-4" />
                            </Button>
                            <Button
                              variant="ghost"
                              size="sm"
                              onClick={() => openBacktest(rule.original as RuleConfiguration)}
                              title="Backtest"
                            >
                              <BarChart2 className="h-4 w-4" />
                            </Button>
                          </>
                        ) : (
                          <>
                            <Button
                              variant="ghost"
                              size="sm"
                              onClick={() => toast.info('Simulação para regras complexas ainda não está disponível.')}
                              title="Simulação indisponível para regras complexas"
                            >
                              <Play className="h-4 w-4" />
                            </Button>
                            <Button
                              variant="ghost"
                              size="sm"
                              onClick={() => toast.info('Backtest para regras complexas ainda não está disponível.')}
                              title="Backtest indisponível para regras complexas"
                            >
                              <BarChart2 className="h-4 w-4" />
                            </Button>
                          </>
                        )}
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
                            } else if (complexId) {
                              deleteComplexMutation.mutate(complexId);
                            } else {
                              toast.error(`Regra complexa sem ID válido (${complexKey ?? 'chave indisponível'}).`);
                            }
                          }}
                          title="Deletar"
                          className="text-red-600 hover:text-red-700"
                          disabled={complexActionDisabled}
                        >
                          <Trash2 className="h-4 w-4" />
                        </Button>
                      </td>
                    </tr>
                  );
                  })}
                </tbody>
              </table>
            </div>
          )}
        </CardContent>
      </Card>

      {/* P0: Simulação de regra (simple rules) */}
      <Dialog open={isSimulationOpen} onOpenChange={(open) => {
        setIsSimulationOpen(open);
        if (!open) {
          setSimulationResult(null);
          setSimulationError(null);
        }
      }}>
        <DialogContent className="sm:max-w-3xl">
          <DialogHeader>
            <DialogTitle>Simular regra</DialogTitle>
            <DialogDescription>
              Execute a regra selecionada contra um payload de teste.
            </DialogDescription>
          </DialogHeader>

          <div className="space-y-4">
            <div className="rounded-lg border border-border p-3 text-sm text-muted-foreground">
              Regra: <span className="font-medium text-foreground">{simulationRule?.ruleName ?? '-'}</span>
            </div>

            <div>
              <label className="block text-sm font-medium text-foreground mb-2">
                Payload de teste (JSON)
              </label>
              <Textarea
                value={simulationPayload}
                onChange={(e) => setSimulationPayload(e.target.value)}
                rows={10}
                className="font-mono text-xs"
              />
              <p className="mt-1 text-xs text-muted-foreground">
                Campos obrigatórios devem estar presentes para validação do backend.
              </p>
            </div>

            <div className="flex items-center justify-end gap-2">
              <Button
                onClick={runSimulation}
                disabled={isSimulating || !simulationPayload.trim() || !simulationRule}
              >
                {isSimulating ? (
                  <>
                    <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                    Simulando...
                  </>
                ) : (
                  <>
                    <Play className="mr-2 h-4 w-4" />
                    Simular
                  </>
                )}
              </Button>
            </div>

            {simulationError && (
              <div className="rounded-lg border border-red-200 bg-red-50 p-3 text-sm text-red-700" role="alert">
                {simulationError}
              </div>
            )}

            {simulationResult && (
              <div className="space-y-3 rounded-lg border border-border bg-muted/40 p-4">
                <div className="flex items-center justify-between">
                  <div className="text-sm font-medium text-foreground">
                    Resultado da simulação
                  </div>
                  <Badge variant={simulationResult.triggered ? 'destructive' : 'secondary'}>
                    {simulationResult.triggered ? 'Disparou' : 'Não disparou'}
                  </Badge>
                </div>
                <div className="grid grid-cols-2 gap-4 text-sm">
                  <div>
                    <div className="text-xs text-muted-foreground">Classificação</div>
                    <div className="font-medium">{simulationResult.classification ?? '—'}</div>
                  </div>
                  <div>
                    <div className="text-xs text-muted-foreground">Peso</div>
                    <div className="font-medium">{simulationResult.weight ?? 0}</div>
                  </div>
                  <div>
                    <div className="text-xs text-muted-foreground">Operador lógico</div>
                    <div className="font-medium">{simulationResult.logicOperator ?? 'AND'}</div>
                  </div>
                  <div>
                    <div className="text-xs text-muted-foreground">Tempo (ms)</div>
                    <div className="font-medium">{simulationResult.processingTimeMs}</div>
                  </div>
                </div>
                <div className="text-sm">
                  <div className="text-xs text-muted-foreground">Motivo</div>
                  <div className="font-medium">{simulationResult.reason}</div>
                </div>

                {simulationResult.conditionResults?.length ? (
                  <div className="mt-2">
                    <div className="text-xs text-muted-foreground mb-2">Condições avaliadas</div>
                    <div className="overflow-x-auto">
                      <table className="w-full text-xs">
                        <thead>
                          <tr className="border-b">
                            <th className="text-left py-1">Campo</th>
                            <th className="text-left py-1">Operador</th>
                            <th className="text-left py-1">Esperado</th>
                            <th className="text-left py-1">Atual</th>
                            <th className="text-left py-1">Resultado</th>
                          </tr>
                        </thead>
                        <tbody>
                          {simulationResult.conditionResults.map((condition, index) => (
                            <tr key={`${condition.field}-${index}`} className="border-b">
                              <td className="py-1 pr-2">{condition.field}</td>
                              <td className="py-1 pr-2">{condition.operator}</td>
                              <td className="py-1 pr-2">{condition.expectedValue}</td>
                              <td className="py-1 pr-2">{condition.actualValue}</td>
                              <td className="py-1">
                                <Badge variant={condition.met ? 'default' : 'secondary'}>
                                  {condition.met ? 'OK' : 'NOK'}
                                </Badge>
                              </td>
                            </tr>
                          ))}
                        </tbody>
                      </table>
                    </div>
                  </div>
                ) : null}
              </div>
            )}
          </div>
        </DialogContent>
      </Dialog>

      {/* P0: Backtest de regra (simple rules) */}
      <Dialog open={isBacktestOpen} onOpenChange={(open) => {
        setIsBacktestOpen(open);
        if (!open) {
          setBacktestResult(null);
          setBacktestError(null);
        }
      }}>
        <DialogContent className="sm:max-w-3xl">
          <DialogHeader>
            <DialogTitle>Backtest de regra</DialogTitle>
            <DialogDescription>
              Executa a regra contra transações históricas para estimar impacto.
            </DialogDescription>
          </DialogHeader>

          <div className="space-y-4">
            <div className="rounded-lg border border-border p-3 text-sm text-muted-foreground">
              Regra: <span className="font-medium text-foreground">{backtestRule?.ruleName ?? '-'}</span>
            </div>

            <div className="grid grid-cols-1 gap-4 sm:grid-cols-3">
              <div>
                <label className="block text-sm font-medium text-foreground mb-2">Início</label>
                <Input
                  type="datetime-local"
                  value={backtestStart}
                  onChange={(e) => setBacktestStart(e.target.value)}
                />
              </div>
              <div>
                <label className="block text-sm font-medium text-foreground mb-2">Fim</label>
                <Input
                  type="datetime-local"
                  value={backtestEnd}
                  onChange={(e) => setBacktestEnd(e.target.value)}
                />
              </div>
              <div>
                <label className="block text-sm font-medium text-foreground mb-2">Amostra</label>
                <Input
                  type="number"
                  min="10"
                  value={backtestSampleSize}
                  onChange={(e) => setBacktestSampleSize(Number(e.target.value))}
                />
              </div>
            </div>

            <div className="flex items-center justify-end gap-2">
              <Button
                onClick={runBacktest}
                disabled={isBacktesting || !backtestRule}
              >
                {isBacktesting ? (
                  <>
                    <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                    Executando...
                  </>
                ) : (
                  <>
                    <BarChart2 className="mr-2 h-4 w-4" />
                    Executar backtest
                  </>
                )}
              </Button>
            </div>

            {backtestError && (
              <div className="rounded-lg border border-red-200 bg-red-50 p-3 text-sm text-red-700" role="alert">
                {backtestError}
              </div>
            )}

            {backtestResult && (
              <div className="space-y-3 rounded-lg border border-border bg-muted/40 p-4">
                <div className="grid grid-cols-2 gap-4 text-sm">
                  <div>
                    <div className="text-xs text-muted-foreground">Avaliações</div>
                    <div className="font-medium">{backtestResult.totalEvaluated}</div>
                  </div>
                  <div>
                    <div className="text-xs text-muted-foreground">Disparos</div>
                    <div className="font-medium">{backtestResult.totalTriggered}</div>
                  </div>
                  <div>
                    <div className="text-xs text-muted-foreground">Taxa de disparo</div>
                    <div className="font-medium">{backtestResult.triggerRate.toFixed(2)}%</div>
                  </div>
                  <div>
                    <div className="text-xs text-muted-foreground">Valor impactado</div>
                    <div className="font-medium">{backtestResult.totalAmountAffected}</div>
                  </div>
                </div>

                <div className="grid grid-cols-3 gap-4 text-sm">
                  <div>
                    <div className="text-xs text-muted-foreground">Aprovaria</div>
                    <div className="font-medium">{backtestResult.wouldApprove}</div>
                  </div>
                  <div>
                    <div className="text-xs text-muted-foreground">Suspeita</div>
                    <div className="font-medium">{backtestResult.wouldSuspect}</div>
                  </div>
                  <div>
                    <div className="text-xs text-muted-foreground">Bloquearia</div>
                    <div className="font-medium">{backtestResult.wouldBlock}</div>
                  </div>
                </div>

                {backtestResult.sampleResults?.length ? (
                  <div className="mt-2">
                    <div className="text-xs text-muted-foreground mb-2">Amostra (até 10)</div>
                    <div className="space-y-2">
                      {backtestResult.sampleResults.map((sample, index) => (
                        <div key={`${sample.ruleName}-${index}`} className="text-xs flex items-center justify-between border-b pb-1">
                          <span>{sample.reason}</span>
                          <Badge variant={sample.triggered ? 'destructive' : 'secondary'}>
                            {sample.triggered ? 'Disparou' : 'Não disparou'}
                          </Badge>
                        </div>
                      ))}
                    </div>
                  </div>
                ) : null}
              </div>
            )}
          </div>
        </DialogContent>
      </Dialog>

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
            <AlertDialogAction
              onClick={() => {
                if (deleteConfirmId !== null) {
                  deleteApprovalMutation.mutate(deleteConfirmId);
                }
              }}
              className="bg-amber-600 hover:bg-amber-700"
            >
              Solicitar aprovação
            </AlertDialogAction>
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
