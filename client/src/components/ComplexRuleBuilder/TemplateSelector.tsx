/**
 * TemplateSelector - Seletor de templates de regras pré-definidas
 * Facilita a criação de regras comuns
 */

import { memo } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { ScrollArea } from '@/components/ui/scroll-area';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
  DialogTrigger,
} from '@/components/ui/dialog';
import {
  Sparkles,
  DollarSign,
  Globe,
  Clock,
  AlertTriangle,
  Shield,
  Zap,
  Users,
} from 'lucide-react';
import { ComplexRule, createEmptyRule, createEmptyGroup, createEmptyCondition } from './types';

interface Template {
  id: string;
  name: string;
  description: string;
  icon: React.ReactNode;
  category: string;
  rule: ComplexRule;
}

const TEMPLATES: Template[] = [
  {
    id: 'high-amount',
    name: 'Transação de Alto Valor',
    description: 'Detecta transações acima de um valor específico',
    icon: <DollarSign className="h-5 w-5" />,
    category: 'Valor',
    rule: {
      ...createEmptyRule(),
      key: 'HIGH_AMOUNT_TRANSACTION',
      title: 'Transação de Alto Valor',
      description: 'Detecta transações com valor acima do limite configurado',
      decision: 'SUSPEITA_DE_FRAUDE',
      priority: 80,
      severity: 60,
      rootConditionGroup: {
        ...createEmptyGroup('AND'),
        conditions: [
          {
            ...createEmptyCondition(),
            fieldName: 'transactionAmount',
            operator: 'GT',
            valueType: 'NUMBER',
            valueSingle: '10000',
          },
        ],
      },
    },
  },
  {
    id: 'foreign-country',
    name: 'País Estrangeiro',
    description: 'Detecta transações em países diferentes do cadastro',
    icon: <Globe className="h-5 w-5" />,
    category: 'Geolocalização',
    rule: {
      ...createEmptyRule(),
      key: 'FOREIGN_COUNTRY_TRANSACTION',
      title: 'Transação em País Estrangeiro',
      description: 'Detecta transações realizadas em países diferentes do país de origem do cartão',
      decision: 'SUSPEITA_DE_FRAUDE',
      priority: 70,
      severity: 50,
      rootConditionGroup: {
        ...createEmptyGroup('AND'),
        conditions: [
          {
            ...createEmptyCondition(),
            fieldName: 'merchantCountryCode',
            operator: 'NEQ',
            valueType: 'STRING',
            valueSingle: '076',
          },
        ],
      },
    },
  },
  {
    id: 'night-transaction',
    name: 'Transação Noturna',
    description: 'Detecta transações em horário incomum (madrugada)',
    icon: <Clock className="h-5 w-5" />,
    category: 'Horário',
    rule: {
      ...createEmptyRule(),
      key: 'NIGHT_TRANSACTION',
      title: 'Transação em Horário Noturno',
      description: 'Detecta transações realizadas entre 00:00 e 06:00',
      decision: 'SUSPEITA_DE_FRAUDE',
      priority: 50,
      severity: 40,
      rootConditionGroup: {
        ...createEmptyGroup('AND'),
        conditions: [
          {
            ...createEmptyCondition(),
            fieldName: 'transactionTime',
            operator: 'BETWEEN',
            valueType: 'NUMBER',
            valueMin: '0',
            valueMax: '60000',
          },
        ],
      },
    },
  },
  {
    id: 'high-risk-mcc',
    name: 'MCC de Alto Risco',
    description: 'Detecta transações em categorias de alto risco (jogos, cripto, etc)',
    icon: <AlertTriangle className="h-5 w-5" />,
    category: 'Categoria',
    rule: {
      ...createEmptyRule(),
      key: 'HIGH_RISK_MCC',
      title: 'MCC de Alto Risco',
      description: 'Detecta transações em MCCs considerados de alto risco para fraude',
      decision: 'SUSPEITA_DE_FRAUDE',
      priority: 90,
      severity: 70,
      rootConditionGroup: {
        ...createEmptyGroup('OR'),
        conditions: [
          {
            ...createEmptyCondition(),
            fieldName: 'mcc',
            operator: 'IN',
            valueType: 'NUMBER',
            valueSingle: '7995,6211,6051,5967,5966',
          },
        ],
      },
    },
  },
  {
    id: 'low-auth-score',
    name: 'Score de Autenticação Baixo',
    description: 'Detecta transações com score de autenticação abaixo do limite',
    icon: <Shield className="h-5 w-5" />,
    category: 'Autenticação',
    rule: {
      ...createEmptyRule(),
      key: 'LOW_AUTH_SCORE',
      title: 'Score de Autenticação Baixo',
      description: 'Detecta transações onde o score de autenticação está abaixo do limite seguro',
      decision: 'SUSPEITA_DE_FRAUDE',
      priority: 85,
      severity: 65,
      rootConditionGroup: {
        ...createEmptyGroup('AND'),
        conditions: [
          {
            ...createEmptyCondition(),
            fieldName: 'consumerAuthenticationScore',
            operator: 'LT',
            valueType: 'NUMBER',
            valueSingle: '50',
          },
        ],
      },
    },
  },
  {
    id: 'complex-high-amount-foreign',
    name: 'Alto Valor + País Estrangeiro',
    description: 'Combina alto valor com transação em país estrangeiro',
    icon: <Zap className="h-5 w-5" />,
    category: 'Combinada',
    rule: {
      ...createEmptyRule(),
      key: 'HIGH_AMOUNT_FOREIGN_COUNTRY',
      title: 'Alto Valor em País Estrangeiro',
      description: 'Detecta transações de alto valor realizadas em países estrangeiros',
      decision: 'FRAUDE',
      priority: 95,
      severity: 85,
      rootConditionGroup: {
        ...createEmptyGroup('AND'),
        conditions: [
          {
            ...createEmptyCondition(),
            fieldName: 'transactionAmount',
            operator: 'GT',
            valueType: 'NUMBER',
            valueSingle: '5000',
          },
          {
            ...createEmptyCondition(),
            fieldName: 'merchantCountryCode',
            operator: 'NEQ',
            valueType: 'STRING',
            valueSingle: '076',
          },
        ],
      },
    },
  },
  {
    id: 'complex-nested',
    name: 'Regra com Grupos Aninhados',
    description: 'Exemplo de regra complexa com (A AND B) OR (C AND D)',
    icon: <Users className="h-5 w-5" />,
    category: 'Avançada',
    rule: {
      ...createEmptyRule(),
      key: 'COMPLEX_NESTED_RULE',
      title: 'Regra Complexa com Grupos',
      description: 'Exemplo: (Alto valor E baixo score) OU (País estrangeiro E horário noturno)',
      decision: 'FRAUDE',
      priority: 100,
      severity: 90,
      rootConditionGroup: {
        ...createEmptyGroup('OR'),
        conditions: [],
        children: [
          {
            ...createEmptyGroup('AND'),
            name: 'Grupo 1: Alto valor + Baixo score',
            conditions: [
              {
                ...createEmptyCondition(),
                fieldName: 'transactionAmount',
                operator: 'GT',
                valueType: 'NUMBER',
                valueSingle: '10000',
              },
              {
                ...createEmptyCondition(),
                fieldName: 'consumerAuthenticationScore',
                operator: 'LT',
                valueType: 'NUMBER',
                valueSingle: '30',
              },
            ],
            children: [],
          },
          {
            ...createEmptyGroup('AND'),
            name: 'Grupo 2: País estrangeiro + Horário noturno',
            conditions: [
              {
                ...createEmptyCondition(),
                fieldName: 'merchantCountryCode',
                operator: 'NEQ',
                valueType: 'STRING',
                valueSingle: '076',
              },
              {
                ...createEmptyCondition(),
                fieldName: 'transactionTime',
                operator: 'BETWEEN',
                valueType: 'NUMBER',
                valueMin: '0',
                valueMax: '60000',
              },
            ],
            children: [],
          },
        ],
      },
    },
  },
];

interface TemplateSelectorProps {
  onSelect: (rule: ComplexRule) => void;
}

export const TemplateSelector = memo(function TemplateSelector({
  onSelect,
}: TemplateSelectorProps) {
  const categories = [...new Set(TEMPLATES.map(t => t.category))];

  return (
    <Dialog>
      <DialogTrigger asChild>
        <Button variant="outline" className="gap-2">
          <Sparkles className="h-4 w-4" />
          Usar Template
        </Button>
      </DialogTrigger>
      <DialogContent className="max-w-3xl max-h-[80vh]">
        <DialogHeader>
          <DialogTitle className="flex items-center gap-2">
            <Sparkles className="h-5 w-5" />
            Templates de Regras
          </DialogTitle>
          <DialogDescription>
            Selecione um template para começar rapidamente. Você pode personalizar depois.
          </DialogDescription>
        </DialogHeader>

        <ScrollArea className="h-[500px] pr-4">
          <div className="space-y-6">
            {categories.map((category) => (
              <div key={category}>
                <h3 className="text-sm font-semibold text-muted-foreground mb-3">
                  {category}
                </h3>
                <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
                  {TEMPLATES.filter(t => t.category === category).map((template) => (
                    <Card
                      key={template.id}
                      className="cursor-pointer hover:border-primary transition-colors"
                      onClick={() => {
                        // Deep clone the template rule
                        const clonedRule = JSON.parse(JSON.stringify(template.rule));
                        // Regenerate all IDs
                        const regenerateIds = (obj: Record<string, unknown>): void => {
                          if (obj.id) obj.id = crypto.randomUUID();
                          Object.values(obj).forEach(value => {
                            if (Array.isArray(value)) {
                              value.forEach(item => {
                                if (typeof item === 'object' && item !== null) {
                                  regenerateIds(item as Record<string, unknown>);
                                }
                              });
                            } else if (typeof value === 'object' && value !== null) {
                              regenerateIds(value as Record<string, unknown>);
                            }
                          });
                        };
                        regenerateIds(clonedRule);
                        onSelect(clonedRule);
                      }}
                    >
                      <CardHeader className="pb-2">
                        <CardTitle className="text-base flex items-center gap-2">
                          {template.icon}
                          {template.name}
                        </CardTitle>
                        <CardDescription className="text-xs">
                          {template.description}
                        </CardDescription>
                      </CardHeader>
                      <CardContent className="pt-0">
                        <div className="flex gap-2">
                          <Badge variant="outline" className="text-xs">
                            Prioridade: {template.rule.priority}
                          </Badge>
                          <Badge
                            className={`text-xs ${
                              template.rule.decision === 'FRAUDE'
                                ? 'bg-red-500'
                                : template.rule.decision === 'SUSPEITA_DE_FRAUDE'
                                ? 'bg-yellow-500'
                                : 'bg-green-500'
                            } text-white`}
                          >
                            {template.rule.decision.replace('_', ' ')}
                          </Badge>
                        </div>
                      </CardContent>
                    </Card>
                  ))}
                </div>
              </div>
            ))}
          </div>
        </ScrollArea>
      </DialogContent>
    </Dialog>
  );
});
