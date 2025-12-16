import React, { useState } from 'react';
import {
  Card, CardContent, CardDescription, CardHeader, CardTitle
} from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import {
  Plus, Trash2, Eye, Save, X, ChevronDown, Info
} from 'lucide-react';

/**
 * Componente RuleBuilder
 * Permite criar regras com múltiplas condições e operadores lógicos
 * Acessibilidade: WCAG 2.1 AA
 */

interface Condition {
  id: string;
  field: string;
  operator: string;
  value: string;
  logicOperator?: 'AND' | 'OR'; // Operador antes desta condição
}

interface Rule {
  id: string;
  name: string;
  description: string;
  classification: 'APPROVED' | 'SUSPICIOUS' | 'FRAUD';
  weight: number;
  enabled: boolean;
  conditions: Condition[];
}

const AVAILABLE_FIELDS = [
  // Identificação
  { category: 'Identificação', name: 'customerIdFromHeader', label: 'ID do Cliente', type: 'text' },
  { category: 'Identificação', name: 'merchantId', label: 'ID do Merchant', type: 'text' },
  { category: 'Identificação', name: 'pan', label: 'PAN (Cartão)', type: 'text' },
  { category: 'Identificação', name: 'externalTransactionId', label: 'ID Externo', type: 'text' },
  
  // Valores/Datas
  { category: 'Valores/Datas', name: 'transactionAmount', label: 'Valor da Transação', type: 'number' },
  { category: 'Valores/Datas', name: 'transactionDate', label: 'Data da Transação', type: 'date' },
  { category: 'Valores/Datas', name: 'transactionTime', label: 'Hora da Transação', type: 'time' },
  { category: 'Valores/Datas', name: 'cardExpireDate', label: 'Data de Expiração', type: 'date' },
  
  // Localização
  { category: 'Localização', name: 'merchantCountryCode', label: 'País do Merchant', type: 'text' },
  { category: 'Localização', name: 'gmtOffset', label: 'GMT Offset', type: 'text' },
  { category: 'Localização', name: 'merchantPostalCode', label: 'CEP do Merchant', type: 'text' },
  
  // Segurança
  { category: 'Segurança', name: 'consumerAuthenticationScore', label: 'Score de Autenticação', type: 'number' },
  { category: 'Segurança', name: 'externalScore3', label: 'Score Externo 3', type: 'number' },
  { category: 'Segurança', name: 'cryptogramValid', label: 'Criptograma Válido', type: 'select' },
  { category: 'Segurança', name: 'cvv2Response', label: 'Resposta CVV2', type: 'select' },
  { category: 'Segurança', name: 'cavvResult', label: 'Resultado CAVV', type: 'number' },
  
  // Categoria
  { category: 'Categoria', name: 'mcc', label: 'MCC (Merchant Category)', type: 'number' },
  { category: 'Categoria', name: 'transactionType', label: 'Tipo de Transação', type: 'text' },
  { category: 'Categoria', name: 'eciIndicator', label: 'ECI Indicator', type: 'number' },
];

const OPERATORS = {
  number: ['>', '<', '>=', '<=', '==', '!=', 'IN', 'NOT_IN'],
  text: ['==', '!=', 'IN', 'NOT_IN', 'CONTAINS', 'NOT_CONTAINS'],
  date: ['>', '<', '>=', '<=', '==', '!='],
  time: ['>', '<', '>=', '<=', '==', '!='],
  select: ['==', '!='],
};

export default function RuleBuilder({
  onSave,
  onCancel,
}: {
  onSave: (rule: Rule) => void;
  onCancel: () => void;
}) {
  const [rule, setRule] = useState<Rule>({
    id: Date.now().toString(),
    name: '',
    description: '',
    classification: 'SUSPICIOUS',
    weight: 50,
    enabled: true,
    conditions: [
      {
        id: '1',
        field: '',
        operator: '',
        value: '',
      },
    ],
  });

  const [showPreview, setShowPreview] = useState(false);

  // Adicionar nova condição
  const addCondition = () => {
    const newCondition: Condition = {
      id: Date.now().toString(),
      field: '',
      operator: '',
      value: '',
      logicOperator: rule.conditions.length > 0 ? 'AND' : undefined,
    };
    setRule({
      ...rule,
      conditions: [...rule.conditions, newCondition],
    });
  };

  // Remover condição
  const removeCondition = (id: string) => {
    if (rule.conditions.length === 1) {
      alert('Uma regra deve ter pelo menos uma condição');
      return;
    }
    setRule({
      ...rule,
      conditions: rule.conditions.filter(c => c.id !== id),
    });
  };

  // Atualizar condição
  const updateCondition = (id: string, updates: Partial<Condition>) => {
    setRule({
      ...rule,
      conditions: rule.conditions.map(c =>
        c.id === id ? { ...c, ...updates } : c
      ),
    });
  };

  // Obter tipo de campo
  const getFieldType = (fieldName: string) => {
    const field = AVAILABLE_FIELDS.find(f => f.name === fieldName);
    return field?.type || 'text';
  };

  // Validar regra
  const isValid = () => {
    if (!rule.name.trim()) return false;
    if (rule.conditions.some(c => !c.field || !c.operator || !c.value)) return false;
    return true;
  };

  // Gerar preview legível
  const generatePreview = () => {
    return rule.conditions
      .map((cond, idx) => {
        const field = AVAILABLE_FIELDS.find(f => f.name === cond.field);
        const logic = idx > 0 ? ` ${cond.logicOperator} ` : '';
        return `${logic}${field?.label || cond.field} ${cond.operator} ${cond.value}`;
      })
      .join('');
  };

  return (
    <div className="fixed inset-0 bg-black/50 flex items-center justify-center p-4 z-50">
      <Card className="w-full max-w-2xl max-h-[90vh] overflow-y-auto border-0 shadow-2xl">
        <CardHeader className="sticky top-0 bg-white border-b flex items-center justify-between">
          <div>
            <CardTitle className="text-2xl">Criar Nova Regra</CardTitle>
            <CardDescription>
              Defina as condições para a regra de fraude
            </CardDescription>
          </div>
          <button
            onClick={onCancel}
            className="p-2 hover:bg-gray-100 rounded-lg transition-colors"
            aria-label="Fechar"
          >
            <X className="w-5 h-5" />
          </button>
        </CardHeader>

        <CardContent className="p-6 space-y-6">
          {/* Informações Básicas */}
          <div className="space-y-4">
            <h3 className="font-semibold text-lg text-gray-900">Informações Básicas</h3>
            
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Nome da Regra *
              </label>
              <input
                type="text"
                value={rule.name}
                onChange={(e) => setRule({ ...rule, name: e.target.value })}
                placeholder="Ex: HIGH_AMOUNT_LOW_SCORE"
                className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500"
                aria-label="Nome da regra"
              />
            </div>

            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Descrição
              </label>
              <textarea
                value={rule.description}
                onChange={(e) => setRule({ ...rule, description: e.target.value })}
                placeholder="Descreva o objetivo desta regra..."
                className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500"
                rows={2}
                aria-label="Descrição da regra"
              />
            </div>

            <div className="grid grid-cols-2 gap-4">
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Classificação *
                </label>
                <select
                  value={rule.classification}
                  onChange={(e) => setRule({ ...rule, classification: e.target.value as any })}
                  className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500"
                  aria-label="Classificação"
                >
                  <option value="APPROVED">✅ Aprovada</option>
                  <option value="SUSPICIOUS">⚠️ Suspeita</option>
                  <option value="FRAUD">🚫 Fraude</option>
                </select>
              </div>

              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Peso (0-100)
                </label>
                <input
                  type="number"
                  min="0"
                  max="100"
                  value={rule.weight}
                  onChange={(e) => setRule({ ...rule, weight: parseInt(e.target.value) })}
                  className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500"
                  aria-label="Peso da regra"
                />
              </div>
            </div>

            <div className="flex items-center gap-2">
              <input
                type="checkbox"
                id="enabled"
                checked={rule.enabled}
                onChange={(e) => setRule({ ...rule, enabled: e.target.checked })}
                className="rounded"
                aria-label="Ativar regra"
              />
              <label htmlFor="enabled" className="text-sm font-medium text-gray-700">
                Regra ativa
              </label>
            </div>
          </div>

          {/* Condições */}
          <div className="space-y-4 border-t pt-6">
            <div className="flex items-center justify-between">
              <h3 className="font-semibold text-lg text-gray-900">Condições</h3>
              <Button
                onClick={addCondition}
                size="sm"
                className="flex items-center gap-2"
                aria-label="Adicionar condição"
              >
                <Plus className="w-4 h-4" />
                Adicionar Condição
              </Button>
            </div>

            <div className="space-y-4">
              {rule.conditions.map((condition, idx) => {
                const fieldType = getFieldType(condition.field);
                const availableOperators = OPERATORS[fieldType as keyof typeof OPERATORS] || OPERATORS.text;

                return (
                  <div key={condition.id} className="p-4 bg-gray-50 rounded-lg border border-gray-200">
                    {/* Operador Lógico */}
                    {idx > 0 && (
                      <div className="mb-4 flex gap-2">
                        <select
                          value={condition.logicOperator || 'AND'}
                          onChange={(e) =>
                            updateCondition(condition.id, {
                              logicOperator: e.target.value as 'AND' | 'OR',
                            })
                          }
                          className="px-3 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 font-semibold"
                          aria-label="Operador lógico"
                        >
                          <option value="AND">AND</option>
                          <option value="OR">OR</option>
                        </select>
                        <span className="text-xs text-gray-600 self-center">
                          (Operador lógico entre condições)
                        </span>
                      </div>
                    )}

                    {/* Campo */}
                    <div className="grid grid-cols-3 gap-3 mb-3">
                      <div>
                        <label className="block text-xs font-medium text-gray-700 mb-1">
                          Campo *
                        </label>
                        <select
                          value={condition.field}
                          onChange={(e) =>
                            updateCondition(condition.id, {
                              field: e.target.value,
                              operator: '', // Reset operator
                              value: '', // Reset value
                            })
                          }
                          className="w-full px-3 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 text-sm"
                          aria-label="Campo da condição"
                        >
                          <option value="">Selecione um campo...</option>
                          {AVAILABLE_FIELDS.map(field => (
                            <option key={field.name} value={field.name}>
                              [{field.category}] {field.label}
                            </option>
                          ))}
                        </select>
                      </div>

                      {/* Operador */}
                      <div>
                        <label className="block text-xs font-medium text-gray-700 mb-1">
                          Operador *
                        </label>
                        <select
                          value={condition.operator}
                          onChange={(e) =>
                            updateCondition(condition.id, { operator: e.target.value })
                          }
                          disabled={!condition.field}
                          className="w-full px-3 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 text-sm disabled:bg-gray-100"
                          aria-label="Operador"
                        >
                          <option value="">Selecione...</option>
                          {availableOperators.map(op => (
                            <option key={op} value={op}>
                              {op}
                            </option>
                          ))}
                        </select>
                      </div>

                      {/* Valor */}
                      <div>
                        <label className="block text-xs font-medium text-gray-700 mb-1">
                          Valor *
                        </label>
                        <input
                          type={fieldType === 'number' ? 'number' : 'text'}
                          value={condition.value}
                          onChange={(e) =>
                            updateCondition(condition.id, { value: e.target.value })
                          }
                          placeholder="Digite o valor..."
                          disabled={!condition.field}
                          className="w-full px-3 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 text-sm disabled:bg-gray-100"
                          aria-label="Valor"
                        />
                      </div>
                    </div>

                    {/* Botão Remover */}
                    {rule.conditions.length > 1 && (
                      <button
                        onClick={() => removeCondition(condition.id)}
                        className="flex items-center gap-2 text-red-600 hover:text-red-700 text-sm font-medium"
                        aria-label="Remover condição"
                      >
                        <Trash2 className="w-4 h-4" />
                        Remover
                      </button>
                    )}
                  </div>
                );
              })}
            </div>
          </div>

          {/* Preview */}
          <div className="border-t pt-6">
            <button
              onClick={() => setShowPreview(!showPreview)}
              className="flex items-center gap-2 text-blue-600 hover:text-blue-700 font-medium mb-3"
              aria-label="Mostrar preview"
            >
              <Eye className="w-4 h-4" />
              {showPreview ? 'Ocultar' : 'Mostrar'} Preview
            </button>

            {showPreview && (
              <div className="p-4 bg-blue-50 border border-blue-200 rounded-lg">
                <p className="text-sm text-gray-700">
                  <strong>Regra:</strong> {rule.name || '(sem nome)'}
                </p>
                <p className="text-sm text-gray-700 mt-2">
                  <strong>Condições:</strong> {generatePreview() || '(nenhuma condição)'}
                </p>
                <p className="text-sm text-gray-700 mt-2">
                  <strong>Classificação:</strong> {rule.classification} | <strong>Peso:</strong> {rule.weight}
                </p>
              </div>
            )}
          </div>

          {/* Botões de Ação */}
          <div className="border-t pt-6 flex gap-3 justify-end">
            <Button
              variant="outline"
              onClick={onCancel}
              aria-label="Cancelar"
            >
              Cancelar
            </Button>
            <Button
              onClick={() => {
                if (isValid()) {
                  onSave(rule);
                } else {
                  alert('Preencha todos os campos obrigatórios');
                }
              }}
              disabled={!isValid()}
              className="flex items-center gap-2"
              aria-label="Salvar regra"
            >
              <Save className="w-4 h-4" />
              Salvar Regra
            </Button>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}
