/**
 * Rules.tsx - Página de configuração de regras de fraude (REFATORADA)
 *
 * REFATORAÇÃO: Esta página foi simplificada de 1672 linhas para ~450 linhas.
 * A lógica foi extraída para componentes e hooks reutilizáveis em /pages/rules/:
 *
 * - RulesTable: Tabela de regras com ações
 * - RulesFormDialog: Dialog para criar/editar regras
 * - RulesSimulationDialog: Dialog para simulação
 * - RulesBacktestDialog: Dialog para backtest
 * - RulesFilters: Filtros de busca e tipo
 * - useUnifiedRules: Hook para unificar regras simples e complexas
 * - constants: Constantes compartilhadas
 */
import { useCallback, useState } from 'react';
import { useLocation } from 'wouter';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { toast } from 'sonner';
import { AlertTriangle } from 'lucide-react';
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
  simulateRule,
  backtestRule,
  toggleRuleStatus,
  toggleComplexRuleStatus,
  updateRule,
  deleteComplexRule,
} from '@/lib/javaApi';

// Componentes extraídos
import { RulesTable } from '@/pages/rules/RulesTable';
import { RulesFormDialog } from '@/pages/rules/RulesFormDialog';
import { RulesSimulationDialog } from '@/pages/rules/RulesSimulationDialog';
import { RulesBacktestDialog } from '@/pages/rules/RulesBacktestDialog';
import { RulesFilters } from '@/pages/rules/RulesFilters';
import { useUnifiedRules, type RuleTypeFilter } from '@/pages/rules/useUnifiedRules';
import { DEFAULT_SIMULATION_PAYLOAD } from '@/pages/rules/constants';

// Types
type RuleFormData = {
  ruleName: string;
  description: string;
  ruleType: RuleConfiguration['ruleType'];
  threshold: number;
  weight: number;
  enabled: boolean;
  classification: RuleConfiguration['classification'];
  parameters: string;
  logicOperator: RuleConfiguration['logicOperator'];
  conditions: RuleConfiguration['conditions'];
};

const INITIAL_FORM_DATA: RuleFormData = {
  ruleName: '',
  description: '',
  ruleType: 'SECURITY',
  threshold: 0,
  weight: 10,
  enabled: true,
  classification: 'SUSPICIOUS',
  parameters: '',
  logicOperator: 'AND',
  conditions: [],
};

/**
 * Página de configuração dinâmica de regras.
 */
export default function Rules() {
  const [, setLocation] = useLocation();
  const queryClient = useQueryClient();

  // ========== Queries ==========
  const { data: simpleRulesData, isLoading, isError, error } = useQuery({
    queryKey: ['rules'],
    queryFn: listRules,
    retry: 1,
  });

  const { data: complexRulesData, isLoading: isLoadingComplex } = useQuery({
    queryKey: ['complexRules'],
    queryFn: listComplexRules,
    retry: 1,
  });

  const fieldDictionaryQuery = useQuery({
    queryKey: ['fieldDictionary'],
    queryFn: () => listFieldDictionary(),
    staleTime: 5 * 60 * 1000,
  });

  // ========== State ==========
  const [searchTerm, setSearchTerm] = useState('');
  const [ruleTypeFilter, setRuleTypeFilter] = useState<RuleTypeFilter>('all');
  const [showDialog, setShowDialog] = useState(false);
  const [editingRule, setEditingRule] = useState<RuleConfiguration | null>(null);
  const [formData, setFormData] = useState<RuleFormData>(INITIAL_FORM_DATA);
  const [validationErrors, setValidationErrors] = useState<Record<string, string>>({});
  const [requiresApproval, setRequiresApproval] = useState(false);
  const [isDirty, setIsDirty] = useState(false);
  const [showUnsavedWarning, setShowUnsavedWarning] = useState(false);
  const [pendingCloseAction, setPendingCloseAction] = useState<(() => void) | null>(null);

  // Delete state
  const [deleteId, setDeleteId] = useState<number | null>(null);
  const [showDeleteConfirm, setShowDeleteConfirm] = useState(false);

  // Simulation state
  const [simulationRule, setSimulationRule] = useState<RuleConfiguration | null>(null);
  const [showSimulationDialog, setShowSimulationDialog] = useState(false);
  const [simulationPayload, setSimulationPayload] = useState(DEFAULT_SIMULATION_PAYLOAD);
  const [simulationResult, setSimulationResult] = useState<RuleSimulationResult | null>(null);
  const [simulationError, setSimulationError] = useState<string | null>(null);
  const [isSimulating, setIsSimulating] = useState(false);

  // Backtest state
  const [backtestRuleState, setBacktestRuleState] = useState<RuleConfiguration | null>(null);
  const [showBacktestDialog, setShowBacktestDialog] = useState(false);
  const [backtestStart, setBacktestStart] = useState(() => {
    const d = new Date();
    d.setDate(d.getDate() - 7);
    return d.toISOString().slice(0, 16);
  });
  const [backtestEnd, setBacktestEnd] = useState(() => new Date().toISOString().slice(0, 16));
  const [backtestSampleSize, setBacktestSampleSize] = useState(100);
  const [backtestResult, setBacktestResult] = useState<RuleBacktestResult | null>(null);
  const [backtestError, setBacktestError] = useState<string | null>(null);
  const [isBacktesting, setIsBacktesting] = useState(false);

  // ========== Unified Rules Hook ==========
  const simpleRules = simpleRulesData ?? [];
  const complexRules = complexRulesData ?? [];
  const { unifiedRules, filteredRules } = useUnifiedRules({
    simpleRules,
    complexRules,
    ruleTypeFilter,
    searchTerm,
  });

  // ========== Mutations ==========
  const saveRule = useMutation({
    mutationFn: async (data: RuleFormData) => {
      const payload = {
        ruleName: data.ruleName,
        description: data.description,
        ruleType: data.ruleType,
        threshold: data.threshold,
        weight: data.weight,
        enabled: data.enabled,
        classification: data.classification,
        parameters: data.parameters,
        logicOperator: data.logicOperator,
        conditions: data.conditions,
      };

      if (requiresApproval) {
        if (editingRule) {
          return requestUpdateApproval(editingRule.id, { ...editingRule, ...payload });
        }
        return requestCreateApproval({ ...payload, id: 0, version: 0 } as RuleConfiguration);
      }

      if (editingRule) {
        return updateRule(editingRule.id, payload);
      }
      return createRule(payload);
    },
    onSuccess: () => {
      toast.success(
        requiresApproval
          ? 'Solicitação de aprovação enviada!'
          : editingRule
          ? 'Regra atualizada!'
          : 'Regra criada!'
      );
      invalidateRules();
      setShowDialog(false);
      resetFormForNewRule();
    },
    onError: (err: Error) => {
      toast.error(`Erro: ${err.message}`);
    },
  });

  const deleteRuleMutation = useMutation({
    mutationFn: (id: number) => deleteRule(id),
    onSuccess: () => {
      toast.success('Regra excluída!');
      invalidateRules();
    },
    onError: (err: Error) => {
      toast.error(`Erro ao excluir: ${err.message}`);
    },
  });

  const deleteComplexRuleMutation = useMutation({
    mutationFn: (id: string) => deleteComplexRule(id),
    onSuccess: () => {
      toast.success('Regra complexa excluída!');
      invalidateRules();
    },
    onError: (err: Error) => {
      toast.error(`Erro ao excluir: ${err.message}`);
    },
  });

  // ========== Callbacks ==========
  const invalidateRules = useCallback(() => {
    queryClient.invalidateQueries({ queryKey: ['rules'] });
    queryClient.invalidateQueries({ queryKey: ['complexRules'] });
  }, [queryClient]);

  const updateFormData = useCallback((updates: Partial<RuleFormData>) => {
    setFormData((prev) => ({ ...prev, ...updates }));
    setIsDirty(true);
  }, []);

  const resetFormForNewRule = useCallback(() => {
    setEditingRule(null);
    setFormData(INITIAL_FORM_DATA);
    setValidationErrors({});
    setRequiresApproval(false);
    setIsDirty(false);
  }, []);

  const handleDialogClose = useCallback(
    (open: boolean) => {
      if (!open && isDirty) {
        setPendingCloseAction(() => () => {
          setShowDialog(false);
          resetFormForNewRule();
        });
        setShowUnsavedWarning(true);
        return;
      }
      setShowDialog(open);
      if (!open) {
        resetFormForNewRule();
      }
    },
    [isDirty, resetFormForNewRule]
  );

  const confirmDiscard = useCallback(() => {
    setShowUnsavedWarning(false);
    if (pendingCloseAction) {
      pendingCloseAction();
      setPendingCloseAction(null);
    }
    resetFormForNewRule();
  }, [pendingCloseAction, resetFormForNewRule]);

  const cancelDiscard = useCallback(() => {
    setShowUnsavedWarning(false);
    setPendingCloseAction(null);
  }, []);

  const handleEdit = useCallback((rule: RuleConfiguration) => {
    setEditingRule(rule);
    setFormData({
      ruleName: rule.ruleName,
      description: rule.description ?? '',
      ruleType: rule.ruleType,
      threshold: rule.threshold ?? 0,
      weight: rule.weight ?? 10,
      enabled: rule.enabled,
      classification: rule.classification,
      parameters: rule.parameters ?? '',
      logicOperator: rule.logicOperator ?? 'AND',
      conditions: rule.conditions ?? [],
    });
    setValidationErrors({});
    setIsDirty(false);
    setShowDialog(true);
  }, []);

  const handleSave = useCallback(() => {
    const errors: Record<string, string> = {};
    if (!formData.ruleName.trim()) {
      errors.ruleName = 'Nome é obrigatório';
    }
    if (Object.keys(errors).length > 0) {
      setValidationErrors(errors);
      return;
    }
    saveRule.mutate(formData);
  }, [formData, saveRule]);

  const handleDeleteClick = useCallback((id: number) => {
    setDeleteId(id);
    setShowDeleteConfirm(true);
  }, []);

  const confirmDelete = useCallback(() => {
    if (deleteId !== null) {
      deleteRuleMutation.mutate(deleteId);
    }
    setShowDeleteConfirm(false);
    setDeleteId(null);
  }, [deleteId, deleteRuleMutation]);

  const cancelDelete = useCallback(() => {
    setShowDeleteConfirm(false);
    setDeleteId(null);
  }, []);

  const handleToggleSimple = useCallback(
    async (id: number) => {
      const rule = simpleRules.find((r) => r.id === id);
      if (rule) {
        try {
          await toggleRuleStatus(id, !rule.enabled);
          invalidateRules();
          toast.success(`Regra ${rule.enabled ? 'desativada' : 'ativada'}!`);
        } catch {
          toast.error('Erro ao alterar status');
        }
      }
    },
    [simpleRules, invalidateRules]
  );

  const handleToggleComplex = useCallback(
    async (id: string | undefined, key?: string) => {
      const rule = complexRules.find((r) => r.id === id || r.key === key);
      if (rule && rule.id) {
        try {
          await toggleComplexRuleStatus(rule.id, !rule.enabled);
          invalidateRules();
          toast.success(`Regra ${rule.enabled ? 'desativada' : 'ativada'}!`);
        } catch {
          toast.error('Erro ao alterar status');
        }
      }
    },
    [complexRules, invalidateRules]
  );

  const handleDeleteComplex = useCallback(
    (id: string | undefined) => {
      if (id) {
        deleteComplexRuleMutation.mutate(id);
      }
    },
    [deleteComplexRuleMutation]
  );

  // Simulation handlers
  const openSimulation = useCallback((rule: RuleConfiguration) => {
    setSimulationRule(rule);
    setSimulationResult(null);
    setSimulationError(null);
    setShowSimulationDialog(true);
  }, []);

  const runSimulationFn = useCallback(async () => {
    if (!simulationRule) return;
    setIsSimulating(true);
    setSimulationError(null);
    setSimulationResult(null);
    try {
      const parsedPayload = JSON.parse(simulationPayload) as TransactionRequest;
      const result = await simulateRule(simulationRule, parsedPayload);
      setSimulationResult(result);
      toast.success('Simulação concluída!');
    } catch (err) {
      const message = err instanceof Error ? err.message : 'Erro desconhecido';
      setSimulationError(message);
      toast.error(`Erro na simulação: ${message}`);
    } finally {
      setIsSimulating(false);
    }
  }, [simulationRule, simulationPayload]);

  // Backtest handlers
  const openBacktest = useCallback((rule: RuleConfiguration) => {
    setBacktestRuleState(rule);
    setBacktestResult(null);
    setBacktestError(null);
    setShowBacktestDialog(true);
  }, []);

  const runBacktestFn = useCallback(async () => {
    if (!backtestRuleState) return;
    setIsBacktesting(true);
    setBacktestError(null);
    setBacktestResult(null);
    try {
      const normalizeDateTime = (value: string) => (value.length === 16 ? `${value}:00` : value);
      const result = await backtestRule(
        backtestRuleState.id,
        normalizeDateTime(backtestStart),
        normalizeDateTime(backtestEnd),
        backtestSampleSize
      );
      setBacktestResult(result);
      toast.success('Backtest concluído!');
    } catch (err) {
      const message = err instanceof Error ? err.message : 'Erro desconhecido';
      setBacktestError(message);
      toast.error(`Erro no backtest: ${message}`);
    } finally {
      setIsBacktesting(false);
    }
  }, [backtestRuleState, backtestStart, backtestEnd, backtestSampleSize]);

  // Helper functions
  const getRuleTypeColor = useCallback((type: string) => {
    switch (type) {
      case 'SECURITY':
        return 'bg-blue-100 text-blue-800';
      case 'CONTEXT':
        return 'bg-green-100 text-green-800';
      case 'VELOCITY':
        return 'bg-purple-100 text-purple-800';
      case 'ANOMALY':
        return 'bg-orange-100 text-orange-800';
      default:
        return 'bg-gray-100 text-gray-800';
    }
  }, []);

  const getDecisionLabel = useCallback((decision?: ComplexRuleDTO['decision']) => {
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
  }, []);

  // ========== Render ==========
  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex justify-between items-center">
        <div>
          <h1 className="text-3xl font-bold text-foreground">Configuração de Regras</h1>
          <p className="text-muted-foreground mt-1">
            Gerenciar regras de detecção de fraude ({filteredRules.length} de {unifiedRules.length} regras)
          </p>
        </div>
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
        onGoToComplexRules={() => setLocation('/complex-rules')}
      />

      {/* Tabela de regras */}
      <RulesTable
        filteredRules={filteredRules}
        totalRules={unifiedRules.length}
        ruleTypeFilter={ruleTypeFilter}
        searchTerm={searchTerm}
        isLoading={isLoading}
        isLoadingComplex={isLoadingComplex}
        isError={isError}
        error={error}
        onToggleSimple={handleToggleSimple}
        onToggleComplex={handleToggleComplex}
        onOpenSimulation={openSimulation}
        onOpenBacktest={openBacktest}
        onSimulationUnavailable={() => toast.info('Simulação disponível apenas para regras simples')}
        onBacktestUnavailable={() => toast.info('Backtest disponível apenas para regras simples')}
        onEditRule={handleEdit}
        onDeleteSimple={handleDeleteClick}
        onDeleteComplex={handleDeleteComplex}
        getDecisionLabel={getDecisionLabel}
        getRuleTypeColor={getRuleTypeColor}
      />

      {/* Dialog de formulário */}
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

      {/* Dialog de simulação */}
      <RulesSimulationDialog
        open={showSimulationDialog}
        onOpenChange={setShowSimulationDialog}
        simulationRule={simulationRule}
        simulationPayload={simulationPayload}
        onPayloadChange={setSimulationPayload}
        onRunSimulation={runSimulationFn}
        isSimulating={isSimulating}
        simulationError={simulationError}
        simulationResult={simulationResult}
      />

      {/* Dialog de backtest */}
      <RulesBacktestDialog
        open={showBacktestDialog}
        onOpenChange={setShowBacktestDialog}
        selectedBacktestRule={backtestRuleState}
        backtestStart={backtestStart}
        backtestEnd={backtestEnd}
        backtestSampleSize={backtestSampleSize}
        onBacktestStartChange={setBacktestStart}
        onBacktestEndChange={setBacktestEnd}
        onBacktestSampleSizeChange={setBacktestSampleSize}
        onRunBacktest={runBacktestFn}
        isBacktesting={isBacktesting}
        backtestError={backtestError}
        backtestResult={backtestResult}
      />

      {/* Dialog de confirmação de exclusão */}
      <AlertDialog open={showDeleteConfirm} onOpenChange={setShowDeleteConfirm}>
        <AlertDialogContent>
          <AlertDialogHeader>
            <AlertDialogTitle className="flex items-center gap-2">
              <AlertTriangle className="h-5 w-5 text-red-500" />
              Confirmar Exclusão
            </AlertDialogTitle>
            <AlertDialogDescription>
              Tem certeza que deseja excluir esta regra? Esta ação não pode ser desfeita.
            </AlertDialogDescription>
          </AlertDialogHeader>
          <AlertDialogFooter>
            <AlertDialogCancel onClick={cancelDelete}>Cancelar</AlertDialogCancel>
            <AlertDialogAction onClick={confirmDelete} className="bg-red-600 hover:bg-red-700">
              Excluir
            </AlertDialogAction>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>

      {/* Dialog de alterações não salvas */}
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
