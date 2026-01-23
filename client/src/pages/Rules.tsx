import { useCallback, useState } from 'react';
import { AlertTriangle } from 'lucide-react';
import { useLocation } from 'wouter';

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
import { MAX_CONDITIONS, validateValueByOperator } from '@/components/RuleFormDialog/schema';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  ComplexRuleDTO,
  FieldDictionaryItem,
  RuleBacktestResult,
  RuleConfiguration,
  RuleSimulationResult,
  TransactionRequest,
  backtestRule,
  createRule,
  deleteComplexRule,
  deleteRule,
  listComplexRules,
  listFieldDictionary,
  listRules,
  requestCreateApproval,
  requestDeleteApproval,
  requestUpdateApproval,
  simulateRule,
  toggleComplexRuleStatus,
  toggleRuleStatus,
  updateRule,
} from '@/lib/javaApi';
import { DEFAULT_SIMULATION_PAYLOAD } from '@/pages/rules/constants';
import { RulesBacktestDialog } from '@/pages/rules/RulesBacktestDialog';
import { RulesFilters } from '@/pages/rules/RulesFilters';
import { RulesFormDialog } from '@/pages/rules/RulesFormDialog';
import { RulesSimulationDialog } from '@/pages/rules/RulesSimulationDialog';
import { RulesTable } from '@/pages/rules/RulesTable';
import { useUnifiedRules } from '@/pages/rules/useUnifiedRules';
import { toast } from 'sonner';

/**
 * Página de configuração dinâmica de regras.
 */
export default function Rules() {
  const [, setLocation] = useLocation();

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

  const simpleRules = data ?? [];
  const complexRules = complexRulesData ?? [];

  const { unifiedRules, filteredRules } = useUnifiedRules({
    simpleRules,
    complexRules,
    ruleTypeFilter,
    searchTerm,
  });

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
  const [selectedBacktestRule, setSelectedBacktestRule] = useState<RuleConfiguration | null>(null);
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
    setShowDialog(false);
    setIsDirty(false);
    setValidationErrors({});
    setEditingRule(null);
    setRequiresApproval(false);
  }, []);

  // P0-04: Cancelar descarte
  const cancelDiscard = useCallback(() => {
    setShowUnsavedWarning(false);
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

  const openBacktest = (rule: RuleConfiguration) => {
    setSelectedBacktestRule(rule);
    setBacktestResult(null);
    setBacktestError(null);
    setIsBacktestOpen(true);
  };

  const resetFormForNewRule = useCallback(() => {
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
  }, []);

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

  const runBacktest = async () => {
    if (!selectedBacktestRule) return;
    setIsBacktesting(true);
    setBacktestError(null);
    setBacktestResult(null);
    try {
      const result = await backtestRule(
        selectedBacktestRule.id,
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

  const handleDeleteComplex = (id: string | undefined, key?: string) => {
    if (!id) {
      toast.error(`Regra complexa sem ID válido (${key ?? 'chave indisponível'}).`);
      return;
    }
    deleteComplexMutation.mutate(id);
  };

  const handleToggle = async (id: number) => {
    toggleMutation.mutate(id);
  };

  const handleToggleComplex = (id: string | undefined, key?: string) => {
    if (!id) {
      toast.error(`Regra complexa sem ID válido (${key ?? 'chave indisponível'}).`);
      return;
    }
    toggleComplexMutation.mutate(id);
  };

  const handleComplexSimulationUnavailable = () => {
    toast.info('Simulação para regras complexas ainda não está disponível.');
  };

  const handleComplexBacktestUnavailable = () => {
    toast.info('Backtest para regras complexas ainda não está disponível.');
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
          <RulesFormDialog
            open={showDialog}
            onOpenChange={handleDialogClose}
            editingRule={editingRule}
            requiresApproval={requiresApproval}
            onRequiresApprovalChange={setRequiresApproval}
            formData={formData}
            validationErrors={validationErrors}
            fieldDictionary={(fieldDictionaryQuery.data ?? []) as FieldDictionaryItem[]}
            fieldDictionaryError={fieldDictionaryQuery.isError}
            onUpdateFormData={updateFormData}
            onResetForm={resetFormForNewRule}
            onClose={() => handleDialogClose(false)}
            onSave={handleSave}
            isSaving={saveRule.isPending}
          />
        </div>

        {/* Filtros */}
        <RulesFilters
          searchTerm={searchTerm}
          onSearchTermChange={setSearchTerm}
          ruleTypeFilter={ruleTypeFilter}
          onRuleTypeFilterChange={setRuleTypeFilter}
          totalCount={unifiedRules.length}
          simpleCount={simpleRules.length}
          complexCount={complexRules.length}
          onGoToComplexRules={() => setLocation('/rules')}
        />
      </div>

      {/* Tabela de Regras */}
      <RulesTable
        filteredRules={filteredRules}
        totalRules={unifiedRules.length}
        ruleTypeFilter={ruleTypeFilter}
        searchTerm={searchTerm}
        isLoading={isLoading}
        isLoadingComplex={isLoadingComplex}
        isError={isError}
        error={error}
        onToggleSimple={handleToggle}
        onToggleComplex={handleToggleComplex}
        onOpenSimulation={openSimulation}
        onOpenBacktest={openBacktest}
        onSimulationUnavailable={handleComplexSimulationUnavailable}
        onBacktestUnavailable={handleComplexBacktestUnavailable}
        onEditRule={handleEdit}
        onDeleteSimple={handleDeleteClick}
        onDeleteComplex={handleDeleteComplex}
        getDecisionLabel={getDecisionLabel}
        getRuleTypeColor={getRuleTypeColor}
      />

      {/* P0: Simulação de regra (simple rules) */}
      <RulesSimulationDialog
        open={isSimulationOpen}
        onOpenChange={(open) => {
          setIsSimulationOpen(open);
          if (!open) {
            setSimulationResult(null);
            setSimulationError(null);
          }
        }}
        simulationRule={simulationRule}
        simulationPayload={simulationPayload}
        onPayloadChange={setSimulationPayload}
        onRunSimulation={runSimulation}
        isSimulating={isSimulating}
        simulationError={simulationError}
        simulationResult={simulationResult}
      />

      {/* P0: Backtest de regra (simple rules) */}
      <RulesBacktestDialog
        open={isBacktestOpen}
        onOpenChange={(open) => {
          setIsBacktestOpen(open);
          if (!open) {
            setBacktestResult(null);
            setBacktestError(null);
          }
        }}
        selectedBacktestRule={selectedBacktestRule}
        backtestStart={backtestStart}
        backtestEnd={backtestEnd}
        backtestSampleSize={backtestSampleSize}
        onBacktestStartChange={setBacktestStart}
        onBacktestEndChange={setBacktestEnd}
        onBacktestSampleSizeChange={setBacktestSampleSize}
        onRunBacktest={runBacktest}
        isBacktesting={isBacktesting}
        backtestError={backtestError}
        backtestResult={backtestResult}
      />

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
}import { useCallback, useState } from 'react';
import { AlertTriangle } from 'lucide-react';
import { useLocation } from 'wouter';
import { Button } from '@/components/ui/button';
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
import { DEFAULT_SIMULATION_PAYLOAD } from '@/pages/rules/constants';
import { RulesBacktestDialog } from '@/pages/rules/RulesBacktestDialog';
import { RulesFilters } from '@/pages/rules/RulesFilters';
import { RulesFormDialog } from '@/pages/rules/RulesFormDialog';
import { RulesSimulationDialog } from '@/pages/rules/RulesSimulationDialog';
import { RulesTable } from '@/pages/rules/RulesTable';
import { useUnifiedRules } from '@/pages/rules/useUnifiedRules';
import { toast } from 'sonner';

/**
 * Página de configuração dinâmica de regras.
 */
export default function Rules() {
  const [, setLocation] = useLocation();

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

  const simpleRules = data ?? [];
  const complexRules = complexRulesData ?? [];

  const { unifiedRules, filteredRules } = useUnifiedRules({
    simpleRules,
    complexRules,
    ruleTypeFilter,
    searchTerm,
  });

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
  const [selectedBacktestRule, setSelectedBacktestRule] = useState<RuleConfiguration | null>(null);
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

  const resetFormForNewRule = useCallback(() => {
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
  }, []);

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
          <RulesFormDialog
            open={showDialog}
            onOpenChange={handleDialogClose}
            editingRule={editingRule}
            requiresApproval={requiresApproval}
            onRequiresApprovalChange={setRequiresApproval}
            formData={formData}
            validationErrors={validationErrors}
            fieldDictionary={(fieldDictionaryQuery.data ?? []) as FieldDictionaryItem[]}
            fieldDictionaryError={fieldDictionaryQuery.isError}
            onUpdateFormData={updateFormData}
            onResetForm={resetFormForNewRule}
            onClose={() => handleDialogClose(false)}
            onSave={handleSave}
            isSaving={saveRule.isPending}
          />
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
        <RulesFilters
          searchTerm={searchTerm}
          onSearchTermChange={setSearchTerm}
          ruleTypeFilter={ruleTypeFilter}
          onRuleTypeFilterChange={setRuleTypeFilter}
          totalCount={unifiedRules.length}
          simpleCount={simpleRules.length}
          complexCount={complexRules.length}
          onGoToComplexRules={() => setLocation('/rules')}
        />
      </div>

      {/* Tabela de Regras */}
      <RulesTable
        filteredRules={filteredRules}
        totalRules={unifiedRules.length}
        ruleTypeFilter={ruleTypeFilter}
        searchTerm={searchTerm}
        isLoading={isLoading}
        isLoadingComplex={isLoadingComplex}
        isError={isError}
        error={error}
        onToggleSimple={handleToggle}
        onToggleComplex={handleToggleComplex}
        onOpenSimulation={openSimulation}
        onOpenBacktest={openBacktest}
        onSimulationUnavailable={handleComplexSimulationUnavailable}
        onBacktestUnavailable={handleComplexBacktestUnavailable}
        onEditRule={handleEdit}
        onDeleteSimple={handleDeleteClick}
        onDeleteComplex={handleDeleteComplex}
        getDecisionLabel={getDecisionLabel}
        getRuleTypeColor={getRuleTypeColor}
      />

      {/* P0: Simulação de regra (simple rules) */}
      <RulesSimulationDialog
        open={isSimulationOpen}
        onOpenChange={(open) => {
          setIsSimulationOpen(open);
          if (!open) {
            setSimulationResult(null);
            setSimulationError(null);
          }
        }}
        simulationRule={simulationRule}
        simulationPayload={simulationPayload}
        onPayloadChange={setSimulationPayload}
        onRunSimulation={runSimulation}
        isSimulating={isSimulating}
        simulationError={simulationError}
        simulationResult={simulationResult}
      />

      {/* P0: Backtest de regra (simple rules) */}
      <RulesBacktestDialog
        open={isBacktestOpen}
        onOpenChange={(open) => {
          setIsBacktestOpen(open);
          if (!open) {
            setBacktestResult(null);
            setBacktestError(null);
          }
        }}
        selectedBacktestRule={selectedBacktestRule}
        backtestStart={backtestStart}
        backtestEnd={backtestEnd}
        backtestSampleSize={backtestSampleSize}
        onBacktestStartChange={setBacktestStart}
        onBacktestEndChange={setBacktestEnd}
        onBacktestSampleSizeChange={setBacktestSampleSize}
        onRunBacktest={runBacktest}
        isBacktesting={isBacktesting}
        backtestError={backtestError}
        backtestResult={backtestResult}
      />

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
