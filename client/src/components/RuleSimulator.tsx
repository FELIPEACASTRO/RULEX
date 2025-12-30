import React, { useState } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Textarea } from '@/components/ui/textarea';
import { 
  Play, 
  CheckCircle, 
  XCircle, 
  AlertTriangle, 
  Clock,
  ChevronDown,
  ChevronUp,
  Copy,
  RefreshCw
} from 'lucide-react';
import { RuleConfiguration } from '@/lib/javaApi';
import { toast } from 'sonner';

interface ConditionResult {
  field: string;
  operator: string;
  expectedValue: string;
  actualValue: string;
  met: boolean;
}

interface SimulationResult {
  ruleName: string;
  triggered: boolean;
  classification: string | null;
  weight: number;
  reason: string;
  conditionResults: ConditionResult[];
  logicOperator: string;
  processingTimeMs: number;
}

interface RuleSimulatorProps {
  rule: Partial<RuleConfiguration>;
  onClose?: () => void;
}

// Payload de exemplo para testes
const SAMPLE_PAYLOADS = {
  normal: {
    externalTransactionId: 'TXN-TEST-001',
    customerIdFromHeader: 'CUST-12345',
    transactionAmount: 150000, // R$ 1.500,00 em centavos
    transactionDate: 20241230,
    transactionTime: 143000,
    mcc: 5411, // Supermercado
    merchantCountryCode: '076', // Brasil
    consumerAuthenticationScore: 850,
    externalScore3: 780,
    cavvResult: 0,
    eciIndicator: 5,
    cryptogramValid: 'V',
    cvv2Response: 'M',
    customerPresent: 'Y',
  },
  suspicious: {
    externalTransactionId: 'TXN-TEST-002',
    customerIdFromHeader: 'CUST-67890',
    transactionAmount: 750000, // R$ 7.500,00
    transactionDate: 20241230,
    transactionTime: 30000, // 03:00 da manhã
    mcc: 7995, // Jogos de azar
    merchantCountryCode: 'RU', // Rússia
    consumerAuthenticationScore: 250,
    externalScore3: 180,
    cavvResult: 2,
    eciIndicator: 0,
    cryptogramValid: 'N',
    cvv2Response: 'N',
    customerPresent: 'N',
  },
  fraud: {
    externalTransactionId: 'TXN-TEST-003',
    customerIdFromHeader: 'CUST-99999',
    transactionAmount: 1500000, // R$ 15.000,00
    transactionDate: 20241230,
    transactionTime: 40000, // 04:00 da manhã
    mcc: 6051, // Cripto
    merchantCountryCode: 'NG', // Nigéria
    consumerAuthenticationScore: 50,
    externalScore3: 30,
    cavvResult: 9,
    eciIndicator: 0,
    cryptogramValid: 'N',
    cvv2Response: 'N',
    customerPresent: 'N',
    cvvPinTryLimitExceeded: 1,
  },
};

/**
 * Componente para simular e testar regras antes de ativá-las.
 */
export function RuleSimulator({ rule, onClose }: RuleSimulatorProps) {
  const [payload, setPayload] = useState(JSON.stringify(SAMPLE_PAYLOADS.normal, null, 2));
  const [result, setResult] = useState<SimulationResult | null>(null);
  const [isLoading, setIsLoading] = useState(false);
  const [showDetails, setShowDetails] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const handleSimulate = async () => {
    setIsLoading(true);
    setError(null);
    setResult(null);

    try {
      // Validar JSON
      let testPayload;
      try {
        testPayload = JSON.parse(payload);
      } catch (e) {
        throw new Error('JSON inválido. Verifique a sintaxe do payload.');
      }

      // Chamar API de simulação
      const response = await fetch('/api/rules/simulation/test', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          rule: rule,
          testPayload: testPayload,
        }),
      });

      if (!response.ok) {
        const errorText = await response.text();
        throw new Error(errorText || 'Erro ao simular regra');
      }

      const simulationResult: SimulationResult = await response.json();
      setResult(simulationResult);

      if (simulationResult.triggered) {
        toast.success('Regra seria disparada!');
      } else {
        toast.info('Regra não seria disparada');
      }
    } catch (err) {
      const message = err instanceof Error ? err.message : 'Erro desconhecido';
      setError(message);
      toast.error(message);
    } finally {
      setIsLoading(false);
    }
  };

  const loadSamplePayload = (type: 'normal' | 'suspicious' | 'fraud') => {
    setPayload(JSON.stringify(SAMPLE_PAYLOADS[type], null, 2));
    setResult(null);
    setError(null);
  };

  const copyPayload = () => {
    navigator.clipboard.writeText(payload);
    toast.success('Payload copiado!');
  };

  const formatValue = (value: string) => {
    if (value === 'null' || value === 'undefined') {
      return <span className="text-muted-foreground italic">null</span>;
    }
    return value;
  };

  return (
    <Card className="w-full">
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <Play className="h-5 w-5" />
          Simulador de Regra
        </CardTitle>
        <CardDescription>
          Teste a regra <span className="font-mono font-semibold">{rule.ruleName || 'Nova Regra'}</span> contra um payload de transação
        </CardDescription>
      </CardHeader>

      <CardContent className="space-y-4">
        {/* Botões de Payload de Exemplo */}
        <div className="flex flex-wrap gap-2">
          <span className="text-sm text-muted-foreground self-center">Carregar exemplo:</span>
          <Button
            variant="outline"
            size="sm"
            onClick={() => loadSamplePayload('normal')}
            className="text-green-600 border-green-200 hover:bg-green-50"
          >
            <CheckCircle className="h-3 w-3 mr-1" />
            Normal
          </Button>
          <Button
            variant="outline"
            size="sm"
            onClick={() => loadSamplePayload('suspicious')}
            className="text-amber-600 border-amber-200 hover:bg-amber-50"
          >
            <AlertTriangle className="h-3 w-3 mr-1" />
            Suspeito
          </Button>
          <Button
            variant="outline"
            size="sm"
            onClick={() => loadSamplePayload('fraud')}
            className="text-red-600 border-red-200 hover:bg-red-50"
          >
            <XCircle className="h-3 w-3 mr-1" />
            Fraude
          </Button>
          <Button
            variant="ghost"
            size="sm"
            onClick={copyPayload}
            className="ml-auto"
          >
            <Copy className="h-3 w-3 mr-1" />
            Copiar
          </Button>
        </div>

        {/* Editor de Payload */}
        <div>
          <label className="block text-sm font-medium text-foreground mb-2">
            Payload de Teste (JSON)
          </label>
          <Textarea
            value={payload}
            onChange={(e) => setPayload(e.target.value)}
            rows={12}
            className="font-mono text-sm"
            placeholder='{"transactionAmount": 100000, ...}'
          />
        </div>

        {/* Botão de Simulação */}
        <div className="flex gap-2">
          <Button
            onClick={handleSimulate}
            disabled={isLoading || !payload.trim()}
            className="flex-1"
          >
            {isLoading ? (
              <>
                <RefreshCw className="h-4 w-4 mr-2 animate-spin" />
                Simulando...
              </>
            ) : (
              <>
                <Play className="h-4 w-4 mr-2" />
                Simular Regra
              </>
            )}
          </Button>
          {onClose && (
            <Button variant="outline" onClick={onClose}>
              Fechar
            </Button>
          )}
        </div>

        {/* Erro */}
        {error && (
          <div className="rounded-lg border border-red-200 bg-red-50 p-4">
            <p className="text-sm text-red-700">{error}</p>
          </div>
        )}

        {/* Resultado */}
        {result && (
          <div className="space-y-4">
            {/* Resultado Principal */}
            <div
              className={`rounded-lg border p-4 ${
                result.triggered
                  ? result.classification === 'FRAUD'
                    ? 'border-red-200 bg-red-50'
                    : result.classification === 'SUSPICIOUS'
                    ? 'border-amber-200 bg-amber-50'
                    : 'border-green-200 bg-green-50'
                  : 'border-gray-200 bg-gray-50'
              }`}
            >
              <div className="flex items-center justify-between">
                <div className="flex items-center gap-3">
                  {result.triggered ? (
                    result.classification === 'FRAUD' ? (
                      <XCircle className="h-6 w-6 text-red-600" />
                    ) : result.classification === 'SUSPICIOUS' ? (
                      <AlertTriangle className="h-6 w-6 text-amber-600" />
                    ) : (
                      <CheckCircle className="h-6 w-6 text-green-600" />
                    )
                  ) : (
                    <CheckCircle className="h-6 w-6 text-gray-400" />
                  )}
                  <div>
                    <p className="font-semibold text-foreground">
                      {result.triggered ? 'Regra Disparada' : 'Regra Não Disparada'}
                    </p>
                    <p className="text-sm text-muted-foreground">{result.reason}</p>
                  </div>
                </div>
                <div className="flex items-center gap-2">
                  {result.triggered && result.classification && (
                    <Badge
                      className={
                        result.classification === 'FRAUD'
                          ? 'bg-red-100 text-red-800'
                          : result.classification === 'SUSPICIOUS'
                          ? 'bg-amber-100 text-amber-800'
                          : 'bg-green-100 text-green-800'
                      }
                    >
                      {result.classification}
                    </Badge>
                  )}
                  {result.triggered && (
                    <Badge variant="outline">Peso: {result.weight}%</Badge>
                  )}
                  <Badge variant="secondary" className="flex items-center gap-1">
                    <Clock className="h-3 w-3" />
                    {result.processingTimeMs}ms
                  </Badge>
                </div>
              </div>
            </div>

            {/* Detalhes das Condições */}
            <div className="rounded-lg border border-border">
              <button
                onClick={() => setShowDetails(!showDetails)}
                className="w-full flex items-center justify-between p-3 hover:bg-muted/50 transition-colors"
              >
                <span className="font-medium text-sm">
                  Detalhes das Condições ({result.conditionResults.length})
                </span>
                {showDetails ? (
                  <ChevronUp className="h-4 w-4" />
                ) : (
                  <ChevronDown className="h-4 w-4" />
                )}
              </button>

              {showDetails && (
                <div className="border-t border-border">
                  <div className="p-3 bg-muted/30 text-sm">
                    <span className="text-muted-foreground">Operador Lógico:</span>{' '}
                    <Badge variant="outline">{result.logicOperator}</Badge>
                    <span className="text-muted-foreground ml-2">
                      ({result.logicOperator === 'AND' ? 'Todas devem ser verdadeiras' : 'Pelo menos uma deve ser verdadeira'})
                    </span>
                  </div>

                  <div className="divide-y divide-border">
                    {result.conditionResults.map((condition, index) => (
                      <div
                        key={index}
                        className={`p-3 flex items-center justify-between ${
                          condition.met ? 'bg-green-50/50' : 'bg-red-50/50'
                        }`}
                      >
                        <div className="flex-1">
                          <div className="flex items-center gap-2">
                            {condition.met ? (
                              <CheckCircle className="h-4 w-4 text-green-600" />
                            ) : (
                              <XCircle className="h-4 w-4 text-red-600" />
                            )}
                            <span className="font-mono text-sm">
                              {condition.field} {condition.operator} {condition.expectedValue}
                            </span>
                          </div>
                          <div className="ml-6 text-xs text-muted-foreground mt-1">
                            Valor atual: <span className="font-mono">{formatValue(condition.actualValue)}</span>
                          </div>
                        </div>
                        <Badge variant={condition.met ? 'default' : 'destructive'} className="ml-2">
                          {condition.met ? 'Passou' : 'Falhou'}
                        </Badge>
                      </div>
                    ))}
                  </div>
                </div>
              )}
            </div>
          </div>
        )}
      </CardContent>
    </Card>
  );
}

export default RuleSimulator;
