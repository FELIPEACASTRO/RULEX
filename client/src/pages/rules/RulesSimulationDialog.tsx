import { Play, Loader2 } from 'lucide-react';

import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { Dialog, DialogContent, DialogDescription, DialogHeader, DialogTitle } from '@/components/ui/dialog';
import { Textarea } from '@/components/ui/textarea';
import { RuleConfiguration, RuleSimulationResult } from '@/lib/javaApi';

type RulesSimulationDialogProps = {
  open: boolean;
  onOpenChange: (open: boolean) => void;
  simulationRule: RuleConfiguration | null;
  simulationPayload: string;
  onPayloadChange: (value: string) => void;
  onRunSimulation: () => void;
  isSimulating: boolean;
  simulationError: string | null;
  simulationResult: RuleSimulationResult | null;
};

export function RulesSimulationDialog({
  open,
  onOpenChange,
  simulationRule,
  simulationPayload,
  onPayloadChange,
  onRunSimulation,
  isSimulating,
  simulationError,
  simulationResult,
}: RulesSimulationDialogProps) {
  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
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
              onChange={(e) => onPayloadChange(e.target.value)}
              rows={10}
              className="font-mono text-xs"
            />
            <p className="mt-1 text-xs text-muted-foreground">
              Campos obrigatórios devem estar presentes para validação do backend.
            </p>
          </div>

          <div className="flex items-center justify-end gap-2">
            <Button
              onClick={onRunSimulation}
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
  );
}
