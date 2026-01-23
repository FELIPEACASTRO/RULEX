import { BarChart2, Loader2 } from 'lucide-react';

import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { Dialog, DialogContent, DialogDescription, DialogHeader, DialogTitle } from '@/components/ui/dialog';
import { Input } from '@/components/ui/input';
import { RuleBacktestResult, RuleConfiguration } from '@/lib/javaApi';

type RulesBacktestDialogProps = {
  open: boolean;
  onOpenChange: (open: boolean) => void;
  selectedBacktestRule: RuleConfiguration | null;
  backtestStart: string;
  backtestEnd: string;
  backtestSampleSize: number;
  onBacktestStartChange: (value: string) => void;
  onBacktestEndChange: (value: string) => void;
  onBacktestSampleSizeChange: (value: number) => void;
  onRunBacktest: () => void;
  isBacktesting: boolean;
  backtestError: string | null;
  backtestResult: RuleBacktestResult | null;
};

export function RulesBacktestDialog({
  open,
  onOpenChange,
  selectedBacktestRule,
  backtestStart,
  backtestEnd,
  backtestSampleSize,
  onBacktestStartChange,
  onBacktestEndChange,
  onBacktestSampleSizeChange,
  onRunBacktest,
  isBacktesting,
  backtestError,
  backtestResult,
}: RulesBacktestDialogProps) {
  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogContent className="sm:max-w-3xl">
        <DialogHeader>
          <DialogTitle>Backtest de regra</DialogTitle>
          <DialogDescription>
            Executa a regra contra transações históricas para estimar impacto.
          </DialogDescription>
        </DialogHeader>

        <div className="space-y-4">
          <div className="rounded-lg border border-border p-3 text-sm text-muted-foreground">
            Regra: <span className="font-medium text-foreground">{selectedBacktestRule?.ruleName ?? '-'}</span>
          </div>

          <div className="grid grid-cols-1 gap-4 sm:grid-cols-3">
            <div>
              <label className="block text-sm font-medium text-foreground mb-2">Início</label>
              <Input
                type="datetime-local"
                value={backtestStart}
                onChange={(e) => onBacktestStartChange(e.target.value)}
              />
            </div>
            <div>
              <label className="block text-sm font-medium text-foreground mb-2">Fim</label>
              <Input
                type="datetime-local"
                value={backtestEnd}
                onChange={(e) => onBacktestEndChange(e.target.value)}
              />
            </div>
            <div>
              <label className="block text-sm font-medium text-foreground mb-2">Amostra</label>
              <Input
                type="number"
                min="10"
                value={backtestSampleSize}
                onChange={(e) => onBacktestSampleSizeChange(Number(e.target.value))}
              />
            </div>
          </div>

          <div className="flex items-center justify-end gap-2">
            <Button
              onClick={onRunBacktest}
              disabled={isBacktesting || !selectedBacktestRule}
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
  );
}
