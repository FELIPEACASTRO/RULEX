import { useEffect, useState } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Badge } from '@/components/ui/badge';
import { Plus, Edit2, Trash2, ToggleRight, Copy, HelpCircle } from 'lucide-react';
import RuleBuilder from '@/components/RuleBuilder';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
  DialogTrigger,
} from '@/components/ui/dialog';
import {
  Tooltip,
  TooltipContent,
  TooltipTrigger,
} from '@/components/ui/tooltip';

// Definição de todos os campos disponíveis no payload
const PAYLOAD_FIELDS = {
  IDENTIFICATION: {
    label: 'Identificação',
    fields: [
      { name: 'externalTransactionId', label: 'ID da Transação', type: 'string', description: 'ID único da transação' },
      { name: 'customerIdFromHeader', label: 'ID do Cliente', type: 'string', description: 'ID do cliente no header' },
      { name: 'customerAcctNumber', label: 'Número da Conta', type: 'number', description: 'Número da conta do cliente' },
      { name: 'pan', label: 'PAN (Cartão)', type: 'string', description: 'Número do cartão tokenizado' },
      { name: 'merchantId', label: 'ID do Merchant', type: 'string', description: 'ID do estabelecimento' },
      { name: 'merchantName', label: 'Nome do Merchant', type: 'string', description: 'Nome do estabelecimento' },
    ]
  },
  VALUES_AND_DATES: {
    label: 'Valores e Datas',
    fields: [
      { name: 'transactionAmount', label: 'Valor da Transação', type: 'number', description: 'Valor em reais' },
      { name: 'transactionDate', label: 'Data da Transação', type: 'number', description: 'Formato YYYYMMDD' },
      { name: 'transactionTime', label: 'Hora da Transação', type: 'number', description: 'Formato HHMMSS' },
      { name: 'transactionCurrencyCode', label: 'Código da Moeda', type: 'number', description: '986 = BRL' },
      { name: 'availableCredit', label: 'Crédito Disponível', type: 'number', description: 'Limite disponível' },
      { name: 'cardCashBalance', label: 'Saldo em Dinheiro', type: 'number', description: 'Saldo do cartão' },
      { name: 'cardDelinquentAmount', label: 'Valor em Atraso', type: 'number', description: 'Montante em atraso' },
    ]
  },
  LOCATION: {
    label: 'Localização',
    fields: [
      { name: 'merchantCountryCode', label: 'País do Merchant', type: 'string', description: 'Código do país (ex: 076=Brasil)' },
      { name: 'merchantCity', label: 'Cidade do Merchant', type: 'string', description: 'Cidade do estabelecimento' },
      { name: 'merchantState', label: 'Estado do Merchant', type: 'string', description: 'Estado do estabelecimento' },
      { name: 'merchantPostalCode', label: 'CEP do Merchant', type: 'string', description: 'CEP do estabelecimento' },
    ]
  },
  SECURITY: {
    label: 'Segurança e Autenticação',
    fields: [
      { name: 'consumerAuthenticationScore', label: 'Score de Autenticação', type: 'number', description: 'Score 0-999' },
      { name: 'externalScore3', label: 'Score Externo', type: 'number', description: 'Score externo 0-999' },
      { name: 'cavvResult', label: 'Resultado CAVV', type: 'number', description: '3D Secure (0=válido)' },
      { name: 'cryptogramValid', label: 'Criptograma Válido', type: 'string', description: 'V=Válido, N=Inválido' },
      { name: 'cvv2Response', label: 'Resposta CVV2', type: 'string', description: 'M=Match, N=No Match' },
      { name: 'cvv2Present', label: 'CVV2 Presente', type: 'string', description: 'Y/N' },
      { name: 'pinVerifyCode', label: 'Código Verificação PIN', type: 'string', description: 'Resultado verificação PIN' },
      { name: 'cvvVerifyCode', label: 'Código Verificação CVV', type: 'string', description: 'Resultado verificação CVV' },
      { name: 'eciIndicator', label: 'Indicador ECI', type: 'number', description: 'E-commerce indicator' },
      { name: 'atcCard', label: 'ATC Cartão', type: 'number', description: 'Application Transaction Counter' },
      { name: 'atcHost', label: 'ATC Host', type: 'number', description: 'Application Transaction Counter Host' },
      { name: 'tokenAssuranceLevel', label: 'Nível de Segurança Token', type: 'number', description: 'Nível de segurança' },
      { name: 'tokenizationIndicator', label: 'Indicador Tokenização', type: 'string', description: 'Indicador de token' },
    ]
  },
  CATEGORY: {
    label: 'Categoria e Tipo',
    fields: [
      { name: 'mcc', label: 'MCC (Merchant Category Code)', type: 'number', description: 'Código da categoria' },
      { name: 'posEntryMode', label: 'Modo de Entrada', type: 'string', description: 'E=E-commerce, C=Chip, etc' },
      { name: 'customerPresent', label: 'Cliente Presente', type: 'string', description: 'Y=Sim, N=Não' },
      { name: 'workflow', label: 'Tipo de Workflow', type: 'string', description: 'Tipo de workflow' },
      { name: 'recordType', label: 'Tipo de Registro', type: 'string', description: 'Tipo do registro' },
    ]
  }
};

// Operadores disponíveis por tipo de campo
const OPERATORS = {
  number: [
    { value: '>', label: 'Maior que (>)' },
    { value: '<', label: 'Menor que (<)' },
    { value: '>=', label: 'Maior ou igual (>=)' },
    { value: '<=', label: 'Menor ou igual (<=)' },
    { value: '==', label: 'Igual (==)' },
    { value: '!=', label: 'Diferente (!=)' },
    { value: 'IN', label: 'Em lista (IN)' },
    { value: 'NOT_IN', label: 'Não em lista (NOT_IN)' },
  ],
  string: [
    { value: '==', label: 'Igual (==)' },
    { value: '!=', label: 'Diferente (!=)' },
    { value: 'IN', label: 'Em lista (IN)' },
    { value: 'NOT_IN', label: 'Não em lista (NOT_IN)' },
    { value: 'CONTAINS', label: 'Contém (CONTAINS)' },
    { value: 'NOT_CONTAINS', label: 'Não contém (NOT_CONTAINS)' },
  ]
};

// Templates de regras pré-configuradas
const RULE_TEMPLATES = [
  {
    name: 'LOW_AUTHENTICATION_SCORE',
    description: 'Bloqueia transações com score de autenticação baixo',
    field: 'consumerAuthenticationScore',
    operator: '<',
    value: '50',
    weight: 25,
    classification: 'SUSPICIOUS'
  },
  {
    name: 'LOW_EXTERNAL_SCORE',
    description: 'Bloqueia transações com score externo baixo',
    field: 'externalScore3',
    operator: '<',
    value: '50',
    weight: 25,
    classification: 'SUSPICIOUS'
  },
  {
    name: 'INVALID_CAVV',
    description: 'Bloqueia transações com CAVV inválido (3D Secure)',
    field: 'cavvResult',
    operator: '!=',
    value: '0',
    weight: 40,
    classification: 'FRAUD'
  },
  {
    name: 'HIGH_TRANSACTION_AMOUNT',
    description: 'Bloqueia transações com valor acima do threshold',
    field: 'transactionAmount',
    operator: '>',
    value: '5000',
    weight: 20,
    classification: 'SUSPICIOUS'
  },
  {
    name: 'HIGH_RISK_MCC',
    description: 'Bloqueia transações de MCCs de alto risco',
    field: 'mcc',
    operator: 'IN',
    value: '7995,6211,6051,7273,7994',
    weight: 25,
    classification: 'SUSPICIOUS'
  },
  {
    name: 'INTERNATIONAL_TRANSACTION',
    description: 'Bloqueia transações internacionais',
    field: 'merchantCountryCode',
    operator: '!=',
    value: '076',
    weight: 15,
    classification: 'SUSPICIOUS'
  },
];

interface Rule {
  id: number;
  ruleName: string;
  description: string;
  conditions: Array<{
    field: string;
    operator: string;
    value: string;
    logicOperator?: 'AND' | 'OR' | undefined;
  }> | any[];
  weight: number;
  enabled: boolean;
  classification: string;
  version: number;
}

/**
 * Página avançada de configuração de regras com suporte a qualquer campo do payload.
 */
export default function RulesAdvanced() {
  const [rules, setRules] = useState<Rule[]>([]);
  const [loading, setLoading] = useState(true);
  const [editingRule, setEditingRule] = useState<Rule | null>(null);
  const [showDialog, setShowDialog] = useState(false);
  const [showTemplates, setShowTemplates] = useState(false);
  const [showRuleBuilder, setShowRuleBuilder] = useState(false);
  const [formData, setFormData] = useState({
    ruleName: '',
    description: '',
    conditions: [{ field: '', operator: '', value: '', logicOperator: 'AND' as 'AND' | 'OR' }],
    weight: 50,
    enabled: true,
    classification: 'SUSPICIOUS',
  });

  useEffect(() => {
    fetchRules();
  }, []);

  const fetchRules = async () => {
    setLoading(true);
    try {
      const response = await fetch('/api/trpc/rules.list');
      const data = await response.json();
      // tRPC retorna { result: { data: [...] } }
      const rulesData = data?.result?.data || [];
      // Mapear para o formato esperado pelo componente
      const mappedRules = rulesData.map((r: any) => ({
        id: r.id,
        ruleName: r.name,
        description: r.description,
        conditions: r.conditions || [],
        weight: r.weight,
        enabled: r.isActive,
        classification: r.classification,
        version: r.version,
        category: r.category,
        source: r.source,
      }));
      setRules(mappedRules);
    } catch (error) {
      console.error('Erro ao buscar regras:', error);
      setRules([]);
    } finally {
      setLoading(false);
    }
  };

  const handleEdit = (rule: Rule) => {
    setEditingRule(rule);
    setFormData({
      ruleName: rule.ruleName,
      description: rule.description,
      conditions: (rule.conditions as any[]).map(c => ({
        field: c.field,
        operator: c.operator,
        value: c.value,
        logicOperator: (c.logicOperator || 'AND') as 'AND' | 'OR'
      })),
      weight: rule.weight,
      enabled: rule.enabled,
      classification: rule.classification,
    });
    setShowDialog(true);
  };

  const handleAddCondition = () => {
    setFormData(prev => ({
      ...prev,
      conditions: [...prev.conditions, { field: '', operator: '', value: '', logicOperator: 'AND' as 'AND' | 'OR' }]
    }));
  };

  const handleRemoveCondition = (index: number) => {
    setFormData(prev => ({
      ...prev,
      conditions: prev.conditions.filter((_, i) => i !== index)
    }));
  };

  const handleConditionChange = (index: number, key: string, value: string) => {
    setFormData(prev => {
      const newConditions = [...prev.conditions];
      newConditions[index] = { ...newConditions[index], [key]: value };
      return { ...prev, conditions: newConditions };
    });
  };

  const handleApplyTemplate = (template: typeof RULE_TEMPLATES[0]) => {
    setFormData({
      ruleName: template.name,
      description: template.description,
      conditions: [{ field: template.field, operator: template.operator, value: template.value, logicOperator: 'AND' as 'AND' | 'OR' }],
      weight: template.weight,
      enabled: true,
      classification: template.classification,
    });
    setShowTemplates(false);
    setShowDialog(true);
  };

  const handleSave = async () => {
    if (!formData.ruleName || formData.conditions.some(c => !c.field || !c.operator || !c.value)) {
      alert('Preencha todos os campos obrigatórios');
      return;
    }

    try {
      const url = editingRule ? `/api/rules/${editingRule.id}` : '/api/rules';
      const method = editingRule ? 'PUT' : 'POST';

      const response = await fetch(url, {
        method,
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(formData),
      });

      if (response.ok) {
        fetchRules();
        setShowDialog(false);
        setEditingRule(null);
        setFormData({
          ruleName: '',
          description: '',
          conditions: [{ field: '', operator: '', value: '', logicOperator: 'AND' as 'AND' | 'OR' }],
          weight: 50,
          enabled: true,
          classification: 'SUSPICIOUS',
        });
      }
    } catch (error) {
      console.error('Erro ao salvar regra:', error);
    }
  };

  const handleDelete = async (id: number) => {
    if (!confirm('Tem certeza que deseja deletar esta regra?')) return;

    try {
      const response = await fetch(`/api/rules/${id}`, { method: 'DELETE' });
      if (response.ok) {
        fetchRules();
      }
    } catch (error) {
      console.error('Erro ao deletar regra:', error);
    }
  };

  const handleToggle = async (id: number) => {
    try {
      const response = await fetch(`/api/rules/${id}/toggle`, { method: 'PATCH' });
      if (response.ok) {
        fetchRules();
      }
    } catch (error) {
      console.error('Erro ao alternar regra:', error);
    }
  };

  const getFieldLabel = (fieldName: string) => {
    for (const category of Object.values(PAYLOAD_FIELDS)) {
      const field = category.fields.find(f => f.name === fieldName);
      if (field) return field.label;
    }
    return fieldName;
  };

  const getFieldType = (fieldName: string) => {
    for (const category of Object.values(PAYLOAD_FIELDS)) {
      const field = category.fields.find(f => f.name === fieldName);
      if (field) return field.type;
    }
    return 'string';
  };

  const renderConditionPreview = (condition: any) => {
    const fieldLabel = getFieldLabel(condition.field);
    return `${fieldLabel} ${condition.operator} ${condition.value}`;
  };

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex justify-between items-center">
        <div>
          <h1 className="text-3xl font-bold text-foreground">Configuração de Regras</h1>
          <p className="text-muted-foreground mt-1">Motor de regras genérico e flexível baseado em qualquer campo do payload</p>
        </div>
        <div className="flex gap-2">
          <Dialog open={showTemplates} onOpenChange={setShowTemplates}>
            <DialogTrigger asChild>
              <Button variant="outline">
                <Copy className="h-4 w-4 mr-2" />
                Templates
              </Button>
            </DialogTrigger>
            <DialogContent className="max-w-2xl">
              <DialogHeader>
                <DialogTitle>Templates de Regras Pré-configuradas</DialogTitle>
                <DialogDescription>
                  Selecione um template para usar como base ou customize conforme necessário
                </DialogDescription>
              </DialogHeader>
              <div className="grid grid-cols-1 gap-4">
                {RULE_TEMPLATES.map((template) => (
                  <Card key={template.name} className="cursor-pointer hover:bg-muted/50 transition-colors">
                    <CardContent className="pt-6">
                      <div className="flex justify-between items-start">
                        <div className="flex-1">
                          <h3 className="font-semibold text-foreground">{template.name}</h3>
                          <p className="text-sm text-muted-foreground mt-1">{template.description}</p>
                          <div className="mt-3 flex gap-2 flex-wrap">
                            <Badge variant="outline">{getFieldLabel(template.field)}</Badge>
                            <Badge variant="outline">{template.operator}</Badge>
                            <Badge variant="outline">{template.value}</Badge>
                            <Badge className="bg-blue-100 text-blue-800">Peso: {template.weight}%</Badge>
                            <Badge className={
                              template.classification === 'FRAUD' ? 'bg-red-100 text-red-800' :
                              template.classification === 'SUSPICIOUS' ? 'bg-amber-100 text-amber-800' :
                              'bg-green-100 text-green-800'
                            }>
                              {template.classification}
                            </Badge>
                          </div>
                        </div>
                        <Button
                          onClick={() => handleApplyTemplate(template)}
                          className="ml-4"
                        >
                          Usar
                        </Button>
                      </div>
                    </CardContent>
                  </Card>
                ))}
              </div>
            </DialogContent>
          </Dialog>

          <Dialog open={showDialog} onOpenChange={setShowDialog}>
            <DialogTrigger asChild>
              <Button
                onClick={() => {
                  setEditingRule(null);
                  setFormData({
                    ruleName: '',
                    description: '',
                    conditions: [{ field: '', operator: '', value: '', logicOperator: 'AND' as 'AND' | 'OR' }],
                    weight: 50,
                    enabled: true,
                    classification: 'SUSPICIOUS',
                  });
                }}
              >
                <Plus className="h-4 w-4 mr-2" />
                Nova Regra
              </Button>
            </DialogTrigger>
            <DialogContent className="max-w-3xl max-h-[90vh] overflow-y-auto">
              <DialogHeader>
                <DialogTitle>{editingRule ? 'Editar Regra' : 'Nova Regra'}</DialogTitle>
                <DialogDescription>
                  Configure uma regra selecionando campos, operadores e valores do payload
                </DialogDescription>
              </DialogHeader>

              <div className="space-y-6">
                {/* Informações Básicas */}
                <div className="space-y-4">
                  <h3 className="font-semibold text-foreground">Informações Básicas</h3>
                  <div>
                    <label className="block text-sm font-medium text-foreground mb-2">
                      Nome da Regra *
                      <Tooltip>
                        <TooltipTrigger asChild>
                          <HelpCircle className="h-4 w-4 inline ml-2 cursor-help" />
                        </TooltipTrigger>
                        <TooltipContent>
                          Identificador único da regra (ex: LOW_AUTHENTICATION_SCORE)
                        </TooltipContent>
                      </Tooltip>
                    </label>
                    <Input
                      value={formData.ruleName}
                      onChange={(e) => setFormData({ ...formData, ruleName: e.target.value })}
                      placeholder="Ex: LOW_AUTHENTICATION_SCORE"
                      disabled={!!editingRule}
                    />
                  </div>
                  <div>
                    <label className="block text-sm font-medium text-foreground mb-2">
                      Descrição *
                      <Tooltip>
                        <TooltipTrigger asChild>
                          <HelpCircle className="h-4 w-4 inline ml-2 cursor-help" />
                        </TooltipTrigger>
                        <TooltipContent>
                          Descrição clara do que a regra faz
                        </TooltipContent>
                      </Tooltip>
                    </label>
                    <Input
                      value={formData.description}
                      onChange={(e) => setFormData({ ...formData, description: e.target.value })}
                      placeholder="Descrição da regra"
                    />
                  </div>
                </div>

                {/* Condições */}
                <div className="space-y-4">
                  <div className="flex justify-between items-center">
                    <h3 className="font-semibold text-foreground">Condições *</h3>
                    <Button
                      variant="outline"
                      size="sm"
                      onClick={handleAddCondition}
                    >
                      <Plus className="h-4 w-4 mr-1" />
                      Adicionar Condição
                    </Button>
                  </div>

                  {formData.conditions.map((condition, index) => (
                    <Card key={index} className="p-4">
                      <div className="space-y-4">
                        {index > 0 && (
                          <div>
                            <label className="block text-sm font-medium text-foreground mb-2">Operador Lógico</label>
                            <select
                              value={condition.logicOperator || 'AND'}
                              onChange={(e) => handleConditionChange(index, 'logicOperator', e.target.value)}
                              className="w-full px-3 py-2 border border-input rounded-lg bg-background text-foreground"
                            >
                              <option value="AND">E (AND) - Todas as condições devem ser verdadeiras</option>
                              <option value="OR">OU (OR) - Pelo menos uma condição deve ser verdadeira</option>
                            </select>
                          </div>
                        )}

                        <div>
                          <label className="block text-sm font-medium text-foreground mb-2">
                            Campo do Payload *
                            <Tooltip>
                              <TooltipTrigger asChild>
                                <HelpCircle className="h-4 w-4 inline ml-2 cursor-help" />
                              </TooltipTrigger>
                              <TooltipContent>
                                Selecione qualquer campo disponível no JSON de transação
                              </TooltipContent>
                            </Tooltip>
                          </label>
                          <select
                            value={condition.field}
                            onChange={(e) => handleConditionChange(index, 'field', e.target.value)}
                            className="w-full px-3 py-2 border border-input rounded-lg bg-background text-foreground"
                          >
                            <option value="">Selecione um campo...</option>
                            {Object.entries(PAYLOAD_FIELDS).map(([_, category]) => (
                              <optgroup key={category.label} label={category.label}>
                                {category.fields.map(field => (
                                  <option key={field.name} value={field.name}>
                                    {field.label} ({field.type})
                                  </option>
                                ))}
                              </optgroup>
                            ))}
                          </select>
                          {condition.field && (
                            <p className="text-xs text-muted-foreground mt-1">
                              {PAYLOAD_FIELDS[Object.keys(PAYLOAD_FIELDS).find(key => 
                                PAYLOAD_FIELDS[key as keyof typeof PAYLOAD_FIELDS].fields.some(f => f.name === condition.field)
                              ) as keyof typeof PAYLOAD_FIELDS]?.fields.find(f => f.name === condition.field)?.description}
                            </p>
                          )}
                        </div>

                        <div>
                          <label className="block text-sm font-medium text-foreground mb-2">
                            Operador *
                            <Tooltip>
                              <TooltipTrigger asChild>
                                <HelpCircle className="h-4 w-4 inline ml-2 cursor-help" />
                              </TooltipTrigger>
                              <TooltipContent>
                                Selecione o operador de comparação
                              </TooltipContent>
                            </Tooltip>
                          </label>
                          <select
                            value={condition.operator}
                            onChange={(e) => handleConditionChange(index, 'operator', e.target.value)}
                            className="w-full px-3 py-2 border border-input rounded-lg bg-background text-foreground"
                          >
                            <option value="">Selecione um operador...</option>
                            {condition.field && OPERATORS[getFieldType(condition.field) as keyof typeof OPERATORS]?.map(op => (
                              <option key={op.value} value={op.value}>{op.label}</option>
                            ))}
                          </select>
                        </div>

                        <div>
                          <label className="block text-sm font-medium text-foreground mb-2">
                            Valor *
                            <Tooltip>
                              <TooltipTrigger asChild>
                                <HelpCircle className="h-4 w-4 inline ml-2 cursor-help" />
                              </TooltipTrigger>
                              <TooltipContent>
                                Para IN/NOT_IN, use valores separados por vírgula (ex: 7995,6211,6051)
                              </TooltipContent>
                            </Tooltip>
                          </label>
                          <Input
                            value={condition.value}
                            onChange={(e) => handleConditionChange(index, 'value', e.target.value)}
                            placeholder={condition.operator === 'IN' || condition.operator === 'NOT_IN' ? 'valor1,valor2,valor3' : 'Digite o valor'}
                            type={getFieldType(condition.field) === 'number' ? 'number' : 'text'}
                          />
                        </div>

                        {formData.conditions.length > 1 && (
                          <Button
                            variant="destructive"
                            size="sm"
                            onClick={() => handleRemoveCondition(index)}
                            className="w-full"
                          >
                            Remover Condição
                          </Button>
                        )}
                      </div>
                    </Card>
                  ))}

                  {/* Preview das Condições */}
                  <Card className="bg-muted/50 p-4">
                    <h4 className="font-semibold text-sm text-foreground mb-2">Preview da Regra</h4>
                    <div className="space-y-2 text-sm text-muted-foreground font-mono">
                  {formData.conditions.map((condition, index) => (
                    <div key={index}>
                      {index > 0 && <span className="text-amber-600 font-semibold">{(condition as any).logicOperator} </span>}
                      <span>{renderConditionPreview(condition)}</span>
                    </div>
                  ))}
                    </div>
                  </Card>
                </div>

                {/* Configuração de Impacto */}
                <div className="space-y-4">
                  <h3 className="font-semibold text-foreground">Configuração de Impacto</h3>
                  <div className="grid grid-cols-2 gap-4">
                    <div>
                      <label className="block text-sm font-medium text-foreground mb-2">
                        Peso (0-100) *
                        <Tooltip>
                          <TooltipTrigger asChild>
                            <HelpCircle className="h-4 w-4 inline ml-2 cursor-help" />
                          </TooltipTrigger>
                          <TooltipContent>
                            Influência desta regra no score final (0-100)
                          </TooltipContent>
                        </Tooltip>
                      </label>
                      <Input
                        type="number"
                        min="0"
                        max="100"
                        value={formData.weight}
                        onChange={(e) => setFormData({ ...formData, weight: parseInt(e.target.value) })}
                      />
                    </div>
                    <div>
                      <label className="block text-sm font-medium text-foreground mb-2">
                        Classificação *
                        <Tooltip>
                          <TooltipTrigger asChild>
                            <HelpCircle className="h-4 w-4 inline ml-2 cursor-help" />
                          </TooltipTrigger>
                          <TooltipContent>
                            Classificação que esta regra pode gerar
                          </TooltipContent>
                        </Tooltip>
                      </label>
                      <select
                        value={formData.classification}
                        onChange={(e) => setFormData({ ...formData, classification: e.target.value })}
                        className="w-full px-3 py-2 border border-input rounded-lg bg-background text-foreground"
                      >
                        <option value="APPROVED">Aprovada</option>
                        <option value="SUSPICIOUS">Suspeita</option>
                        <option value="FRAUD">Fraude</option>
                      </select>
                    </div>
                  </div>
                </div>

                {/* Status */}
                <div className="flex items-center gap-2">
                  <input
                    type="checkbox"
                    id="enabled"
                    checked={formData.enabled}
                    onChange={(e) => setFormData({ ...formData, enabled: e.target.checked })}
                    className="w-4 h-4 rounded border-input"
                  />
                  <label htmlFor="enabled" className="text-sm font-medium text-foreground">
                    Regra Habilitada
                  </label>
                </div>

                {/* Botões de Ação */}
                <div className="flex justify-end gap-2 pt-4 border-t border-border">
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
      </div>

      {/* Tabela de Regras */}
      <Card>
        <CardHeader>
          <CardTitle>Regras Configuradas</CardTitle>
          <CardDescription>Total: {rules.length} regras</CardDescription>
        </CardHeader>
        <CardContent>
          {loading ? (
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
            <div className="space-y-4">
              {rules.map((rule) => (
                <Card key={rule.id} className="p-4">
                  <div className="flex justify-between items-start">
                    <div className="flex-1">
                      <div className="flex items-center gap-2 mb-2">
                        <h3 className="font-semibold text-foreground">{rule.ruleName}</h3>
                        <Badge variant={rule.enabled ? 'default' : 'secondary'}>
                          {rule.enabled ? 'Ativa' : 'Inativa'}
                        </Badge>
                        <Badge className={
                          rule.classification === 'FRAUD' ? 'bg-red-100 text-red-800' :
                          rule.classification === 'SUSPICIOUS' ? 'bg-amber-100 text-amber-800' :
                          'bg-green-100 text-green-800'
                        }>
                          {rule.classification}
                        </Badge>
                      </div>
                      <p className="text-sm text-muted-foreground mb-3">{rule.description}</p>
                      <div className="space-y-1">
                        {rule.conditions?.map((condition, idx) => (
                          <div key={idx} className="text-sm font-mono text-muted-foreground">
                            {idx > 0 && <span className="text-amber-600">{(condition as any).logicOperator} </span>}
                            {renderConditionPreview(condition)}
                          </div>
                        ))}
                      </div>
                      <div className="mt-3 flex gap-4 text-sm text-muted-foreground">
                        <span>Peso: <strong>{rule.weight}%</strong></span>
                        <span>Versão: <strong>{rule.version}</strong></span>
                      </div>
                    </div>
                    <div className="flex gap-2">
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
                    </div>
                  </div>
                </Card>
              ))}
            </div>
          )}
        </CardContent>
      </Card>

      {/* Documentação */}
      <Card>
        <CardHeader>
          <CardTitle>📚 Guia de Uso</CardTitle>
        </CardHeader>
        <CardContent className="space-y-4 text-sm text-muted-foreground">
          <div>
            <h4 className="font-semibold text-foreground mb-2">Como criar uma regra:</h4>
            <ol className="list-decimal list-inside space-y-1">
              <li>Clique em "Nova Regra" ou use um template</li>
              <li>Dê um nome único e descritivo à regra</li>
              <li>Selecione o campo do payload que deseja avaliar</li>
              <li>Escolha o operador de comparação apropriado</li>
              <li>Defina o valor de comparação</li>
              <li>Adicione mais condições se necessário (AND/OR)</li>
              <li>Configure o peso (influência no score) e classificação</li>
              <li>Salve a regra</li>
            </ol>
          </div>
          <div>
            <h4 className="font-semibold text-foreground mb-2">Operadores disponíveis:</h4>
            <ul className="space-y-1">
              <li><strong>Números:</strong> &gt;, &lt;, &gt;=, &lt;=, ==, !=, IN, NOT_IN</li>
              <li><strong>Texto:</strong> ==, !=, IN, NOT_IN, CONTAINS, NOT_CONTAINS</li>
            </ul>
          </div>
          <div>
            <h4 className="font-semibold text-foreground mb-2">Exemplo:</h4>
            <p className="font-mono bg-muted p-2 rounded">
              consumerAuthenticationScore &lt; 50 AND transactionAmount &gt; 5000
            </p>
          </div>
        </CardContent>
      </Card>

      {/* Rule Builder Modal */}
      {showRuleBuilder && (
        <RuleBuilder
          onSave={(newRule) => {
            // Converter para o formato esperado
            const ruleToSave = {
              ruleName: newRule.name,
              description: newRule.description,
              conditions: newRule.conditions,
              weight: newRule.weight,
              enabled: newRule.enabled,
              classification: newRule.classification,
            };
            handleSave();
            setShowRuleBuilder(false);
          }}
          onCancel={() => setShowRuleBuilder(false)}
        />
      )}
    </div>
  );
}
