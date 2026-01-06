/**
 * RuleSimulator - Simulador de regra integrado
 *
 * Permite testar a regra em tempo real com um payload JSON
 * sem necessidade de salvar ou navegar para outra página.
 *
 * @version 1.0.0
 */

import { useState } from 'react';
import { Card, CardContent, CardHeader, CardTitle, CardDescription } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Textarea } from '@/components/ui/textarea';
import { Badge } from '@/components/ui/badge';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { Play, AlertTriangle, CheckCircle2, XCircle, Loader2, Copy } from 'lucide-react';
import { toast } from 'sonner';
import api from '@/lib/api';
import type { ComplexRule } from './types';

interface RuleSimulatorProps {
  rule: ComplexRule;
}

interface SimulationResult {
  matched: boolean;
  decision: string;
  score: number;
  reason: string;
  executionTimeMs: number;
}

const EXAMPLE_PAYLOAD = `{
  "transactionAmount": 5000.00,
  "merchantCountryCode": "US",
  "cardCountryCode": "BR",
  "mcc": "5999",
  "transactionType": "PURCHASE",
  "cardPresent": false,
  "cvv2Response": "M",
  "avsResponse": "N"
}`;

export function RuleSimulator({ rule }: RuleSimulatorProps) {
  const [payload, setPayload] = useState(EXAMPLE_PAYLOAD);
  const [result, setResult] = useState<SimulationResult | null>(null);
  const [isSimulating, setIsSimulating] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const handleSimulate = async () => {
    setIsSimulating(true);
    setError(null);
    setResult(null);

    try {
      // Validate JSON
      const parsedPayload = JSON.parse(payload);

      // Call real API endpoint for evaluation
      const startTime = performance.now();
      const response = await api.post('/evaluate', {
        payload: parsedPayload
      });
      const endTime = performance.now();
      const executionTimeMs = Math.round(endTime - startTime);

      // Transform API response to SimulationResult
      const apiResult = response.data;
      const result: SimulationResult = {
        matched: apiResult.decision !== 'APPROVE',
        decision: apiResult.decision || 'UNKNOWN',
        score: apiResult.totalScore || 0,
        reason: apiResult.ruleHits?.map((hit: any) => hit.ruleName).join(', ') || 'Nenhuma regra acionada',
        executionTimeMs,
      };

      setResult(result);
      toast.success('Simulação concluída!');
    } catch (err) {
      const errorMessage = err instanceof Error ? err.message : 'Erro desconhecido';
      setError(errorMessage);
      toast.error(`Erro na simulação: ${errorMessage}`);
    } finally {
      setIsSimulating(false);
    }
  };

  const handleLoadExample = () => {
    setPayload(EXAMPLE_PAYLOAD);
    toast.info('Payload de exemplo carregado');
  };

  const handleCopyResult = () => {
    if (result) {
      navigator.clipboard.writeText(JSON.stringify(result, null, 2));
      toast.success('Resultado copiado!');
    }
  };

  return (
    <Card className="border-dashed">
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <Play className="h-5 w-5" />
          Simulador de Regra
        </CardTitle>
        <CardDescription>
          Teste a regra em tempo real com um payload JSON
        </CardDescription>
      </CardHeader>
      <CardContent className="space-y-4">
        {/* Payload Input */}
        <div className="space-y-2">
          <div className="flex items-center justify-between">
            <label className="text-sm font-medium">Payload da Transação (JSON)</label>
            <Button
              variant="ghost"
              size="sm"
              onClick={handleLoadExample}
            >
              Carregar Exemplo
            </Button>
          </div>
          <Textarea
            value={payload}
            onChange={(e) => setPayload(e.target.value)}
            placeholder="Cole o JSON da transação aqui..."
            className="font-mono text-sm min-h-[200px]"
          />
        </div>

        {/* Simulate Button */}
        <Button
          onClick={handleSimulate}
          disabled={isSimulating || !payload.trim()}
          className="w-full"
        >
          {isSimulating ? (
            <>
              <Loader2 className="mr-2 h-4 w-4 animate-spin" />
              Simulando...
            </>
          ) : (
            <>
              <Play className="mr-2 h-4 w-4" />
              Simular Regra
            </>
          )}
        </Button>

        {/* Error */}
        {error && (
          <Alert variant="destructive">
            <AlertTriangle className="h-4 w-4" />
            <AlertDescription>{error}</AlertDescription>
          </Alert>
        )}

        {/* Result */}
        {result && (
          <Card className="bg-muted/50">
            <CardHeader>
              <div className="flex items-center justify-between">
                <CardTitle className="text-base flex items-center gap-2">
                  {result.matched ? (
                    <>
                      <CheckCircle2 className="h-5 w-5 text-green-500" />
                      Regra Acionada
                    </>
                  ) : (
                    <>
                      <XCircle className="h-5 w-5 text-gray-500" />
                      Regra Não Acionada
                    </>
                  )}
                </CardTitle>
                <Button
                  variant="ghost"
                  size="sm"
                  onClick={handleCopyResult}
                >
                  <Copy className="h-4 w-4" />
                </Button>
              </div>
            </CardHeader>
            <CardContent className="space-y-3">
              <div className="grid grid-cols-2 gap-4">
                <div>
                  <p className="text-sm text-muted-foreground">Decisão</p>
                  <Badge variant={result.decision === 'FRAUD' ? 'destructive' : 'secondary'}>
                    {result.decision}
                  </Badge>
                </div>
                <div>
                  <p className="text-sm text-muted-foreground">Score</p>
                  <p className="text-lg font-semibold">{result.score}</p>
                </div>
                <div>
                  <p className="text-sm text-muted-foreground">Tempo de Execução</p>
                  <p className="text-sm font-mono">{result.executionTimeMs}ms</p>
                </div>
                <div>
                  <p className="text-sm text-muted-foreground">Razão</p>
                  <p className="text-sm">{result.reason}</p>
                </div>
              </div>
            </CardContent>
          </Card>
        )}
      </CardContent>
    </Card>
  );
}
