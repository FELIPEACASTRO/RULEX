import { useEffect, useMemo, useState } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Badge } from '@/components/ui/badge';
import { Plus, Edit2, Trash2, ToggleRight } from 'lucide-react';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
  DialogTrigger,
} from '@/components/ui/dialog';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  FieldDictionaryItem,
  RuleConfiguration,
  createRule,
  deleteRule,
  listFieldDictionary,
  listRules,
  toggleRuleStatus,
  updateRule,
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

  const rules = useMemo(() => data ?? [], [data]);
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
    logicOperator: 'AND' as RuleConfiguration['logicOperator'],
    conditions: [] as RuleConfiguration['conditions'],
  });

  const invalidateRules = () => queryClient.invalidateQueries({ queryKey: ['rules'] });

  const saveRule = useMutation({
    mutationFn: async () => {
      const payload: Omit<RuleConfiguration, 'id' | 'version'> = {
        ruleName: formData.ruleName,
        description: formData.description,
        ruleType: formData.ruleType,
        threshold: formData.threshold,
        weight: formData.weight,
        enabled: formData.enabled,
        classification: formData.classification,
        // Backend exige: conditions + logicOperator.
        // - Para regras "genéricas": preencha conditions + logicOperator.
        // - Para regras "legadas por nome": conditions pode ficar vazio e o engine cai no switch(ruleName).
        conditions: formData.conditions ?? [],
        logicOperator: formData.logicOperator ?? 'AND',
      };

      if (editingRule) {
        return updateRule(editingRule.id, payload);
      }
      return createRule(payload);
    },
    onSuccess: () => {
      toast.success('Regra salva com sucesso');
      setShowDialog(false);
      setEditingRule(null);
      setFormData({
        ruleName: '',
        description: '',
        ruleType: 'SECURITY',
        threshold: 0,
        weight: 0,
        enabled: true,
        classification: 'SUSPICIOUS',
        logicOperator: 'AND',
        conditions: [],
      });
      invalidateRules();
    },
    onError: () => toast.error('Falha ao salvar regra'),
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
      logicOperator: rule.logicOperator ?? 'AND',
      conditions: rule.conditions ?? [],
    });
    setShowDialog(true);
  };

  const handleSave = async () => {
    saveRule.mutate();
  };

  const handleDelete = async (id: number) => {
    if (!confirm('Tem certeza que deseja deletar esta regra?')) return;

    deleteMutation.mutate(id);
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
      <div className="flex justify-between items-center">
        <div>
          <h1 className="text-3xl font-bold text-foreground">Configuração de Regras</h1>
          <p className="text-muted-foreground mt-1">Gerenciar regras de detecção de fraude</p>
        </div>
        <Dialog open={showDialog} onOpenChange={setShowDialog}>
          <DialogTrigger asChild>
            <Button
              onClick={() => {
                setEditingRule(null);
                setFormData({
                  ruleName: '',
                  description: '',
                  ruleType: 'SECURITY',
                  threshold: 0,
                  weight: 0,
                  enabled: true,
                  classification: 'SUSPICIOUS',
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
                  Nome da Regra
                </label>
                <Input
                  id="ruleName"
                  value={formData.ruleName}
                  onChange={(e) => setFormData({ ...formData, ruleName: e.target.value })}
                  placeholder="Ex: RULE_GENERIC_001 (ou LOW_AUTHENTICATION_SCORE para regra legada)"
                  disabled={!!editingRule}
                />
              </div>
              <div>
                <label htmlFor="description" className="block text-sm font-medium text-foreground mb-2">
                  Descrição
                </label>
                <Input
                  id="description"
                  value={formData.description}
                  onChange={(e) => setFormData({ ...formData, description: e.target.value })}
                  placeholder="Descrição da regra"
                />
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
                  />
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
                  />
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
                          />
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
                            />
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
                <Button variant="outline" onClick={() => setShowDialog(false)}>
                  Cancelar
                </Button>
                <Button onClick={handleSave}>
                  {editingRule ? 'Atualizar' : 'Criar'}
                </Button>
              </div>
            </div>
          </DialogContent>
        </Dialog>
      </div>

      {/* Tabela de Regras */}
      <Card>
        <CardHeader>
          <CardTitle>Regras Configuradas</CardTitle>
          <CardDescription>Total: {rules.length} regras</CardDescription>
        </CardHeader>
        <CardContent>
          {isError && (
            <div className="mb-4 rounded-lg border border-red-200 bg-red-50 p-3 text-red-800" role="alert">
              Erro ao carregar regras: {error instanceof Error ? error.message : 'erro inesperado'}
            </div>
          )}
          {isLoading ? (
            <div className="flex items-center justify-center h-64">
              <div className="text-center">
                <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-primary mx-auto mb-2"></div>
                <p className="text-muted-foreground">Carregando regras...</p>
              </div>
            </div>
          ) : rules.length === 0 ? (
            <div className="flex items-center justify-center h-64">
              <p className="text-muted-foreground">Nenhuma regra configurada</p>
            </div>
          ) : (
            <div className="overflow-x-auto">
              <table className="w-full">
                <thead className="border-b border-border">
                  <tr>
                    <th className="text-left py-3 px-4 font-semibold text-sm text-foreground">Nome da Regra</th>
                    <th className="text-left py-3 px-4 font-semibold text-sm text-foreground">Tipo</th>
                    <th className="text-center py-3 px-4 font-semibold text-sm text-foreground">Threshold</th>
                    <th className="text-center py-3 px-4 font-semibold text-sm text-foreground">Peso</th>
                    <th className="text-left py-3 px-4 font-semibold text-sm text-foreground">Classificação</th>
                    <th className="text-center py-3 px-4 font-semibold text-sm text-foreground">Status</th>
                    <th className="text-center py-3 px-4 font-semibold text-sm text-foreground">Ações</th>
                  </tr>
                </thead>
                <tbody>
                  {rules.map((rule) => (
                    <tr key={rule.id} className="border-b border-border hover:bg-muted/50 transition-colors">
                      <td className="py-3 px-4 text-sm font-medium text-foreground">{rule.ruleName}</td>
                      <td className="py-3 px-4 text-sm">
                        <Badge className={getRuleTypeColor(rule.ruleType)}>
                          {rule.ruleType}
                        </Badge>
                      </td>
                      <td className="py-3 px-4 text-sm text-center text-foreground">{rule.threshold}</td>
                      <td className="py-3 px-4 text-sm text-center text-foreground">{rule.weight}%</td>
                      <td className="py-3 px-4 text-sm">
                        <Badge className={getClassificationColor(rule.classification)}>
                          {rule.classification}
                        </Badge>
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
                          onClick={() => handleToggle(rule.id)}
                          title={rule.enabled ? 'Desativar' : 'Ativar'}
                        >
                          <ToggleRight className="h-4 w-4" />
                        </Button>
                        <Button
                          variant="ghost"
                          size="sm"
                          onClick={() => handleEdit(rule)}
                          title="Editar"
                        >
                          <Edit2 className="h-4 w-4" />
                        </Button>
                        <Button
                          variant="ghost"
                          size="sm"
                          onClick={() => handleDelete(rule.id)}
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
    </div>
  );
}
