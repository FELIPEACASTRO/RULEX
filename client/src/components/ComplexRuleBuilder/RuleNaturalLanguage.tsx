/**
 * RuleNaturalLanguage - Tradução de regras para linguagem natural
 *
 * Converte a estrutura JSON da regra em texto legível e compreensível
 * para analistas não-técnicos.
 *
 * @version 1.0.0
 */

import { Card, CardContent, CardHeader, CardTitle, CardDescription } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { MessageSquare, Copy } from 'lucide-react';
import { Button } from '@/components/ui/button';
import { toast } from 'sonner';
import { getFieldLabel } from '@/lib/fieldLabels';
import type { ComplexRule, ConditionGroup, Condition } from './types';

interface RuleNaturalLanguageProps {
  rule: ComplexRule;
}

const OPERATOR_LABELS: Record<string, string> = {
  EQ: 'é igual a',
  NE: 'é diferente de',
  GT: 'é maior que',
  GTE: 'é maior ou igual a',
  LT: 'é menor que',
  LTE: 'é menor ou igual a',
  IN: 'está em',
  NOT_IN: 'não está em',
  CONTAINS: 'contém',
  NOT_CONTAINS: 'não contém',
  STARTS_WITH: 'começa com',
  ENDS_WITH: 'termina com',
  MATCHES_REGEX: 'corresponde ao padrão',
  BETWEEN: 'está entre',
  IS_NULL: 'é nulo',
  IS_NOT_NULL: 'não é nulo',
};

const LOGIC_LABELS: Record<string, string> = {
  AND: 'E',
  OR: 'OU',
  NOT: 'NÃO',
  XOR: 'OU EXCLUSIVO',
  NAND: 'NÃO E',
  NOR: 'NEM',
};

// FIELD_LABELS agora vem de @/lib/fieldLabels.ts com todos os 102 campos

function translateCondition(condition: Condition): string {
  const fieldLabel = getFieldLabel(condition.fieldName);
  const operatorLabel = OPERATOR_LABELS[condition.operator] || condition.operator;
  const value = condition.valueSingle || condition.valueArray?.join(', ') || '';

  return `**${fieldLabel}** ${operatorLabel} \`${value}\``;
}

function translateGroup(group: ConditionGroup, level: number = 0): string[] {
  const lines: string[] = [];
  const indent = '  '.repeat(level);
  const logicLabel = LOGIC_LABELS[group.logicOperator] || group.logicOperator;

  // Add group header
  if (level > 0) {
    lines.push(`${indent}**Grupo ${logicLabel}:**`);
  }

  // Add conditions
  const conditions = group.conditions || [];
  conditions.forEach((condition, index) => {
    const prefix = index === 0 ? 'SE' : logicLabel;
    lines.push(`${indent}${prefix} ${translateCondition(condition)}`);
  });

  // Add child groups
  const children = group.children || [];
  children.forEach((child, index) => {
    if (conditions.length > 0 || index > 0) {
      lines.push(`${indent}${logicLabel}`);
    }
    lines.push(...translateGroup(child, level + 1));
  });

  return lines;
}

export function RuleNaturalLanguage({ rule }: RuleNaturalLanguageProps) {
  const translatedLines = translateGroup(rule.rootConditionGroup);
  const translatedText = translatedLines.join('\n');

  const handleCopy = () => {
    navigator.clipboard.writeText(translatedText);
    toast.success('Texto copiado!');
  };

  return (
    <Card className="border-primary/20">
      <CardHeader>
        <div className="flex items-center justify-between">
          <div>
            <CardTitle className="flex items-center gap-2">
              <MessageSquare className="h-5 w-5" />
              Linguagem Natural
            </CardTitle>
            <CardDescription>
              Tradução legível da lógica da regra
            </CardDescription>
          </div>
          <Button
            variant="ghost"
            size="sm"
            onClick={handleCopy}
          >
            <Copy className="h-4 w-4" />
          </Button>
        </div>
      </CardHeader>
      <CardContent>
        <div className="bg-muted/30 rounded-lg p-4 space-y-2 font-mono text-sm">
          <div className="mb-4">
            <p className="text-xs text-muted-foreground mb-1">REGRA:</p>
            <p className="font-semibold">{rule.title || rule.key}</p>
          </div>

          <div className="mb-4">
            <p className="text-xs text-muted-foreground mb-1">DECISÃO:</p>
            <Badge variant={rule.decision === 'FRAUDE' ? 'destructive' : 'secondary'}>
              {rule.decision || 'SUSPEITA_DE_FRAUDE'}
            </Badge>
            <span className="ml-2 text-muted-foreground">
              (Severidade: {rule.severity || 0})
            </span>
          </div>

          <div>
            <p className="text-xs text-muted-foreground mb-2">LÓGICA:</p>
            <div className="space-y-1 whitespace-pre-wrap">
              {translatedLines.map((line, index) => (
                <div key={index} className="leading-relaxed">
                  {line}
                </div>
              ))}
            </div>
          </div>

          <div className="mt-4 pt-4 border-t">
            <p className="text-xs text-muted-foreground mb-1">ENTÃO:</p>
            <p className="text-sm">
              Classificar como <Badge variant="outline">{rule.decision || 'SUSPEITA_DE_FRAUDE'}</Badge> com severidade <strong>{rule.severity || 0}</strong>.
            </p>
          </div>
        </div>
      </CardContent>
    </Card>
  );
}
