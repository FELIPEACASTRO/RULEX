/**
 * RulePreview - Preview visual da regra em linguagem natural
 * Mostra a regra de forma legível e fácil de entender
 */

import { memo, useMemo } from 'react';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { ScrollArea } from '@/components/ui/scroll-area';
import { Eye, Code, FileText } from 'lucide-react';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import {
  ComplexRule,
  ConditionGroup,
  Condition,
  LOGIC_OPERATORS,
  COMPARISON_OPERATORS,
  DECISION_TYPES,
  RULE_STATUSES,
} from './types';

interface RulePreviewProps {
  rule: ComplexRule;
}

export const RulePreview = memo(function RulePreview({ rule }: RulePreviewProps) {
  // Generate natural language description
  const naturalLanguage = useMemo(() => {
    return generateNaturalLanguage(rule.rootConditionGroup);
  }, [rule.rootConditionGroup]);

  // Generate pseudo-code
  const pseudoCode = useMemo(() => {
    return generatePseudoCode(rule.rootConditionGroup, 0);
  }, [rule.rootConditionGroup]);

  // Generate JSON preview
  const jsonPreview = useMemo(() => {
    return JSON.stringify(rule, null, 2);
  }, [rule]);

  const statusInfo = RULE_STATUSES.find(s => s.value === rule.status);
  const decisionInfo = DECISION_TYPES.find(d => d.value === rule.decision);

  return (
    <Card className="h-full">
      <CardHeader className="pb-3">
        <div className="flex items-center justify-between">
          <CardTitle className="text-lg flex items-center gap-2">
            <Eye className="h-5 w-5" />
            Preview da Regra
          </CardTitle>
          <div className="flex gap-2">
            {statusInfo && (
              <Badge className={`${statusInfo.color} text-white`}>
                {statusInfo.label}
              </Badge>
            )}
            {decisionInfo && (
              <Badge className={`${decisionInfo.color} text-white`}>
                {decisionInfo.label}
              </Badge>
            )}
          </div>
        </div>
      </CardHeader>
      <CardContent>
        <Tabs defaultValue="natural" className="w-full">
          <TabsList className="grid w-full grid-cols-3">
            <TabsTrigger value="natural" className="text-xs">
              <FileText className="h-3 w-3 mr-1" />
              Natural
            </TabsTrigger>
            <TabsTrigger value="pseudo" className="text-xs">
              <Code className="h-3 w-3 mr-1" />
              Pseudo-código
            </TabsTrigger>
            <TabsTrigger value="json" className="text-xs">
              <Code className="h-3 w-3 mr-1" />
              JSON
            </TabsTrigger>
          </TabsList>

          <TabsContent value="natural" className="mt-4">
            <ScrollArea className="h-[300px] rounded-md border p-4">
              <div className="space-y-4">
                {/* Rule Info */}
                <div className="pb-3 border-b">
                  <h3 className="font-semibold text-lg">{rule.title || 'Sem título'}</h3>
                  {rule.description && (
                    <p className="text-sm text-muted-foreground mt-1">{rule.description}</p>
                  )}
                  <div className="flex gap-4 mt-2 text-sm">
                    <span>Prioridade: <strong>{rule.priority}</strong></span>
                    <span>Severidade: <strong>{rule.severity}</strong></span>
                  </div>
                </div>

                {/* Conditions */}
                <div>
                  <h4 className="font-medium mb-2">Condições:</h4>
                  <div className="pl-4 border-l-2 border-primary/30">
                    {naturalLanguage || (
                      <span className="text-muted-foreground italic">
                        Nenhuma condição definida
                      </span>
                    )}
                  </div>
                </div>

                {/* Decision */}
                <div className="pt-3 border-t">
                  <span className="text-sm">
                    <strong>Então:</strong> Classificar como{' '}
                    <Badge className={`${decisionInfo?.color || 'bg-gray-500'} text-white`}>
                      {decisionInfo?.label || rule.decision}
                    </Badge>
                  </span>
                  {rule.reasonTemplate && (
                    <p className="text-sm text-muted-foreground mt-1">
                      Motivo: "{rule.reasonTemplate}"
                    </p>
                  )}
                </div>
              </div>
            </ScrollArea>
          </TabsContent>

          <TabsContent value="pseudo" className="mt-4">
            <ScrollArea className="h-[300px] rounded-md border bg-muted/50">
              <pre className="p-4 text-sm font-mono whitespace-pre-wrap">
                {`REGRA: ${rule.key || 'SEM_CHAVE'}\n`}
                {`TÍTULO: ${rule.title || 'Sem título'}\n`}
                {`STATUS: ${rule.status}\n`}
                {`PRIORIDADE: ${rule.priority}\n`}
                {`SEVERIDADE: ${rule.severity}\n\n`}
                {`SE (\n${pseudoCode || '  // Nenhuma condição'}\n)\n`}
                {`ENTÃO:\n`}
                {`  DECISÃO = ${rule.decision}\n`}
                {rule.reasonTemplate ? `  MOTIVO = "${rule.reasonTemplate}"\n` : ''}
                {`FIM`}
              </pre>
            </ScrollArea>
          </TabsContent>

          <TabsContent value="json" className="mt-4">
            <ScrollArea className="h-[300px] rounded-md border bg-muted/50">
              <pre className="p-4 text-xs font-mono whitespace-pre-wrap">
                {jsonPreview}
              </pre>
            </ScrollArea>
          </TabsContent>
        </Tabs>
      </CardContent>
    </Card>
  );
});

// Helper: Generate natural language from condition group
function generateNaturalLanguage(group: ConditionGroup, depth: number = 0): React.ReactNode {
  const logicOp = LOGIC_OPERATORS.find(op => op.value === group.logicOperator);
  const connector = logicOp?.label || group.logicOperator;

  const items: React.ReactNode[] = [];

  // Add conditions
  group.conditions.forEach((condition, index) => {
    if (condition.enabled === false) return;
    
    const conditionText = formatCondition(condition);
    items.push(
      <div key={`cond-${condition.id}`} className="py-1">
        {condition.negate && <span className="text-red-500 font-medium">NÃO </span>}
        <span className="text-foreground">{conditionText}</span>
      </div>
    );
  });

  // Add child groups
  group.children.forEach((child, index) => {
    if (child.enabled === false) return;
    
    items.push(
      <div key={`group-${child.id}`} className="py-1 pl-4 border-l-2 border-muted-foreground/30 my-2">
        <span className="text-muted-foreground text-xs uppercase mb-1 block">
          Sub-grupo ({LOGIC_OPERATORS.find(op => op.value === child.logicOperator)?.label}):
        </span>
        {generateNaturalLanguage(child, depth + 1)}
      </div>
    );
  });

  if (items.length === 0) {
    return null;
  }

  return (
    <div className="space-y-1">
      {items.map((item, index) => (
        <div key={index}>
          {index > 0 && (
            <Badge variant="outline" className="text-xs mb-1">
              {connector}
            </Badge>
          )}
          {item}
        </div>
      ))}
    </div>
  );
}

// Helper: Format a single condition
function formatCondition(condition: Condition): string {
  const opInfo = COMPARISON_OPERATORS.find(op => op.value === condition.operator);
  const opLabel = opInfo?.label || condition.operator;

  let valueStr = '';
  
  if (opInfo?.requiresFieldRef && condition.valueFieldRef) {
    valueStr = `[${condition.valueFieldRef}]`;
  } else if (opInfo?.requiresSecondValue) {
    valueStr = `${condition.valueMin || '?'} e ${condition.valueMax || '?'}`;
  } else if (condition.valueSingle) {
    valueStr = condition.valueSingle;
  } else if (condition.valueArray?.length) {
    valueStr = `[${condition.valueArray.join(', ')}]`;
  }

  if (!opInfo?.requiresValue && !opInfo?.requiresFieldRef) {
    return `${condition.fieldName} ${opLabel}`;
  }

  return `${condition.fieldName} ${opLabel} ${valueStr}`;
}

// Helper: Generate pseudo-code from condition group
function generatePseudoCode(group: ConditionGroup, indent: number): string {
  const spaces = '  '.repeat(indent + 1);
  const logicOp = group.logicOperator;

  const lines: string[] = [];

  // Add conditions
  group.conditions.forEach((condition, index) => {
    if (condition.enabled === false) {
      lines.push(`${spaces}// DESABILITADO: ${formatConditionCode(condition)}`);
      return;
    }
    
    const prefix = index > 0 || group.children.length > 0 ? `${logicOp} ` : '';
    const negate = condition.negate ? 'NOT ' : '';
    lines.push(`${spaces}${prefix}${negate}${formatConditionCode(condition)}`);
  });

  // Add child groups
  group.children.forEach((child, index) => {
    if (child.enabled === false) {
      lines.push(`${spaces}// DESABILITADO: grupo ${child.logicOperator}`);
      return;
    }
    
    const prefix = group.conditions.length > 0 || index > 0 ? `${logicOp} ` : '';
    lines.push(`${spaces}${prefix}(`);
    lines.push(generatePseudoCode(child, indent + 1));
    lines.push(`${spaces})`);
  });

  return lines.join('\n');
}

// Helper: Format condition as code
function formatConditionCode(condition: Condition): string {
  const opInfo = COMPARISON_OPERATORS.find(op => op.value === condition.operator);
  
  let valueStr = '';
  
  if (opInfo?.requiresFieldRef && condition.valueFieldRef) {
    valueStr = condition.valueFieldRef;
  } else if (opInfo?.requiresSecondValue) {
    valueStr = `${condition.valueMin || '?'}, ${condition.valueMax || '?'}`;
  } else if (condition.valueSingle) {
    // Quote strings
    if (condition.valueType === 'STRING') {
      valueStr = `"${condition.valueSingle}"`;
    } else {
      valueStr = condition.valueSingle;
    }
  } else if (condition.valueArray?.length) {
    valueStr = `[${condition.valueArray.join(', ')}]`;
  }

  if (!opInfo?.requiresValue && !opInfo?.requiresFieldRef) {
    return `${condition.fieldName} ${condition.operator}`;
  }

  return `${condition.fieldName} ${condition.operator} ${valueStr}`;
}
