import { BarChart2, Edit2, Layers, Play, ToggleRight, Trash2 } from 'lucide-react';

import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { ComplexRuleDTO, RuleConfiguration } from '@/lib/javaApi';
import type { RuleTypeFilter, UnifiedRule } from '@/pages/rules/useUnifiedRules';

export type RulesTableProps = {
  filteredRules: UnifiedRule[];
  totalRules: number;
  ruleTypeFilter: RuleTypeFilter;
  searchTerm: string;
  isLoading: boolean;
  isLoadingComplex: boolean;
  isError: boolean;
  error: unknown;
  onToggleSimple: (id: number) => void;
  onToggleComplex: (id: string | undefined, key?: string) => void;
  onOpenSimulation: (rule: RuleConfiguration) => void;
  onOpenBacktest: (rule: RuleConfiguration) => void;
  onSimulationUnavailable: () => void;
  onBacktestUnavailable: () => void;
  onEditRule: (rule: RuleConfiguration) => void;
  onDeleteSimple: (id: number) => void;
  onDeleteComplex: (id: string | undefined, key?: string) => void;
  getDecisionLabel: (decision?: ComplexRuleDTO['decision']) => string;
  getRuleTypeColor: (ruleType: string) => string;
};

export function RulesTable({
  filteredRules,
  totalRules,
  ruleTypeFilter,
  searchTerm,
  isLoading,
  isLoadingComplex,
  isError,
  error,
  onToggleSimple,
  onToggleComplex,
  onOpenSimulation,
  onOpenBacktest,
  onSimulationUnavailable,
  onBacktestUnavailable,
  onEditRule,
  onDeleteSimple,
  onDeleteComplex,
  getDecisionLabel,
  getRuleTypeColor,
}: RulesTableProps) {
  return (
    <Card>
      <CardHeader>
        <CardTitle>Regras Configuradas</CardTitle>
        <CardDescription>
          Mostrando {filteredRules.length} de {totalRules} regras
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
                          <Badge variant="outline" className={rule.classification === 'HARD'
                            ? 'bg-red-50 text-red-700 border-red-200'
                            : rule.classification === 'SOFT'
                              ? 'bg-yellow-50 text-yellow-700 border-yellow-200'
                              : 'bg-blue-50 text-blue-700 border-blue-200'}>
                            {rule.classification}
                          </Badge>
                        ) : (
                          <Badge variant="outline" className="bg-slate-50 text-slate-700 border-slate-200">
                            {rule.severity ?? '—'}
                          </Badge>
                        )}
                      </td>
                      <td className="py-3 px-4 text-center text-sm">
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
                              onToggleSimple(rule.id as number);
                            } else {
                              onToggleComplex(complexId, complexKey);
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
                              onClick={() => onOpenSimulation(rule.original as RuleConfiguration)}
                              title="Simular regra"
                            >
                              <Play className="h-4 w-4" />
                            </Button>
                            <Button
                              variant="ghost"
                              size="sm"
                              onClick={() => onOpenBacktest(rule.original as RuleConfiguration)}
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
                              onClick={onSimulationUnavailable}
                              title="Simulação indisponível para regras complexas"
                            >
                              <Play className="h-4 w-4" />
                            </Button>
                            <Button
                              variant="ghost"
                              size="sm"
                              onClick={onBacktestUnavailable}
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
                            onClick={() => onEditRule(rule.original as RuleConfiguration)}
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
                              onDeleteSimple(rule.id as number);
                            } else {
                              onDeleteComplex(complexId, complexKey);
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
  );
}
