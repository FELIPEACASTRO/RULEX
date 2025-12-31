/**
 * ConditionCard - Card visual para uma condição individual
 * Design intuitivo com campos lado a lado
 */

import { memo, useCallback } from 'react';
import { Trash2, GripVertical, ToggleLeft, ToggleRight } from 'lucide-react';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Badge } from '@/components/ui/badge';
import {
  Select,
  SelectContent,
  SelectGroup,
  SelectItem,
  SelectLabel,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select';
import {
  Tooltip,
  TooltipContent,
  TooltipProvider,
  TooltipTrigger,
} from '@/components/ui/tooltip';
import {
  Condition,
  ComparisonOperator,
  ValueType,
  COMPARISON_OPERATORS,
  VALUE_TYPES,
  getOperatorInfo,
  getOperatorsForType,
} from './types';

interface ConditionCardProps {
  condition: Condition;
  onChange: (condition: Condition) => void;
  onDelete: () => void;
  fieldOptions?: string[];
  isDragging?: boolean;
}

export const ConditionCard = memo(function ConditionCard({
  condition,
  onChange,
  onDelete,
  fieldOptions = [],
  isDragging = false,
}: ConditionCardProps) {
  const operatorInfo = getOperatorInfo(condition.operator);
  const availableOperators = getOperatorsForType(condition.valueType);

  const handleFieldChange = useCallback((value: string) => {
    onChange({ ...condition, fieldName: value });
  }, [condition, onChange]);

  const handleOperatorChange = useCallback((value: string) => {
    const newOperator = value as ComparisonOperator;
    const newOpInfo = getOperatorInfo(newOperator);
    
    // Reset values when changing operator type
    const updates: Partial<Condition> = { operator: newOperator };
    
    if (!newOpInfo?.requiresValue) {
      updates.valueSingle = '';
      updates.valueArray = undefined;
      updates.valueMin = undefined;
      updates.valueMax = undefined;
    }
    
    if (newOpInfo?.requiresFieldRef) {
      updates.valueFieldRef = updates.valueFieldRef || '';
    }
    
    onChange({ ...condition, ...updates });
  }, [condition, onChange]);

  const handleValueTypeChange = useCallback((value: string) => {
    const newType = value as ValueType;
    const newOperators = getOperatorsForType(newType);
    
    // If current operator is not valid for new type, reset to first valid
    const currentOpValid = newOperators.some(op => op.value === condition.operator);
    
    onChange({
      ...condition,
      valueType: newType,
      operator: currentOpValid ? condition.operator : (newOperators[0]?.value || 'EQ'),
      valueSingle: '',
      valueArray: undefined,
      valueMin: undefined,
      valueMax: undefined,
    });
  }, [condition, onChange]);

  const handleValueChange = useCallback((value: string) => {
    onChange({ ...condition, valueSingle: value });
  }, [condition, onChange]);

  const handleValueMinChange = useCallback((value: string) => {
    onChange({ ...condition, valueMin: value });
  }, [condition, onChange]);

  const handleValueMaxChange = useCallback((value: string) => {
    onChange({ ...condition, valueMax: value });
  }, [condition, onChange]);

  const handleFieldRefChange = useCallback((value: string) => {
    onChange({ ...condition, valueFieldRef: value });
  }, [condition, onChange]);

  const handleToggleEnabled = useCallback(() => {
    onChange({ ...condition, enabled: !condition.enabled });
  }, [condition, onChange]);

  const handleToggleNegate = useCallback(() => {
    onChange({ ...condition, negate: !condition.negate });
  }, [condition, onChange]);

  // Group operators by category for better UX
  const operatorsByCategory = availableOperators.reduce((acc, op) => {
    if (!acc[op.category]) acc[op.category] = [];
    acc[op.category].push(op);
    return acc;
  }, {} as Record<string, typeof availableOperators>);

  const categoryLabels: Record<string, string> = {
    basic: 'Comparação',
    list: 'Listas',
    range: 'Intervalo',
    string: 'Texto',
    null: 'Nulo',
    boolean: 'Booleano',
    field: 'Entre Campos',
    date: 'Data/Hora',
    array: 'Arrays',
    math: 'Matemático',
  };

  return (
    <div
      className={`
        relative flex items-center gap-2 p-3 rounded-lg border transition-all
        ${condition.enabled === false ? 'opacity-50 bg-muted' : 'bg-card'}
        ${condition.negate ? 'border-red-300 bg-red-50 dark:bg-red-950/20' : 'border-border'}
        ${isDragging ? 'shadow-lg ring-2 ring-primary' : 'hover:shadow-md'}
      `}
    >
      {/* Drag Handle */}
      <div className="cursor-grab active:cursor-grabbing text-muted-foreground hover:text-foreground">
        <GripVertical className="h-4 w-4" />
      </div>

      {/* Negate Badge */}
      {condition.negate && (
        <Badge variant="destructive" className="text-xs">
          NÃO
        </Badge>
      )}

      {/* Field Name */}
      <div className="flex-1 min-w-[150px]">
        <Input
          value={condition.fieldName}
          onChange={(e) => handleFieldChange(e.target.value)}
          placeholder="Campo..."
          list="condition-fields"
          className="h-9 text-sm"
        />
        {fieldOptions.length > 0 && (
          <datalist id="condition-fields">
            {fieldOptions.map((f) => (
              <option key={f} value={f} />
            ))}
          </datalist>
        )}
      </div>

      {/* Value Type */}
      <Select value={condition.valueType} onValueChange={handleValueTypeChange}>
        <SelectTrigger className="w-[120px] h-9 text-sm">
          <SelectValue />
        </SelectTrigger>
        <SelectContent>
          {VALUE_TYPES.map((type) => (
            <SelectItem key={type.value} value={type.value}>
              {type.label}
            </SelectItem>
          ))}
        </SelectContent>
      </Select>

      {/* Operator */}
      <Select value={condition.operator} onValueChange={handleOperatorChange}>
        <SelectTrigger className="w-[140px] h-9 text-sm">
          <SelectValue />
        </SelectTrigger>
        <SelectContent>
          {Object.entries(operatorsByCategory).map(([category, ops]) => (
            <SelectGroup key={category}>
              <SelectLabel>{categoryLabels[category] || category}</SelectLabel>
              {ops.map((op) => (
                <SelectItem key={op.value} value={op.value}>
                  <span className="font-mono mr-2">{op.label}</span>
                  <span className="text-muted-foreground text-xs">{op.description}</span>
                </SelectItem>
              ))}
            </SelectGroup>
          ))}
        </SelectContent>
      </Select>

      {/* Value Input(s) */}
      {operatorInfo?.requiresValue && !operatorInfo?.requiresFieldRef && (
        <>
          {operatorInfo.requiresSecondValue ? (
            // Range inputs (BETWEEN, etc.)
            <div className="flex items-center gap-1">
              <Input
                value={condition.valueMin || ''}
                onChange={(e) => handleValueMinChange(e.target.value)}
                placeholder="Min"
                className="w-[80px] h-9 text-sm"
              />
              <span className="text-muted-foreground text-sm">e</span>
              <Input
                value={condition.valueMax || ''}
                onChange={(e) => handleValueMaxChange(e.target.value)}
                placeholder="Max"
                className="w-[80px] h-9 text-sm"
              />
            </div>
          ) : (
            // Single value input
            <Input
              value={condition.valueSingle || ''}
              onChange={(e) => handleValueChange(e.target.value)}
              placeholder={
                condition.operator === 'IN' || condition.operator === 'NOT_IN'
                  ? 'valor1, valor2, ...'
                  : 'Valor...'
              }
              className="w-[150px] h-9 text-sm"
            />
          )}
        </>
      )}

      {/* Field Reference Input */}
      {operatorInfo?.requiresFieldRef && (
        <Input
          value={condition.valueFieldRef || ''}
          onChange={(e) => handleFieldRefChange(e.target.value)}
          placeholder="Outro campo..."
          list="condition-fields"
          className="w-[150px] h-9 text-sm"
        />
      )}

      {/* No value needed indicator */}
      {!operatorInfo?.requiresValue && !operatorInfo?.requiresFieldRef && (
        <span className="text-muted-foreground text-sm italic w-[150px]">
          (sem valor)
        </span>
      )}

      {/* Actions */}
      <div className="flex items-center gap-1">
        <TooltipProvider>
          <Tooltip>
            <TooltipTrigger asChild>
              <Button
                variant="ghost"
                size="icon"
                className="h-8 w-8"
                onClick={handleToggleNegate}
              >
                <span className={`text-xs font-bold ${condition.negate ? 'text-red-500' : 'text-muted-foreground'}`}>
                  ¬
                </span>
              </Button>
            </TooltipTrigger>
            <TooltipContent>
              {condition.negate ? 'Remover negação' : 'Negar condição'}
            </TooltipContent>
          </Tooltip>
        </TooltipProvider>

        <TooltipProvider>
          <Tooltip>
            <TooltipTrigger asChild>
              <Button
                variant="ghost"
                size="icon"
                className="h-8 w-8"
                onClick={handleToggleEnabled}
              >
                {condition.enabled !== false ? (
                  <ToggleRight className="h-4 w-4 text-green-500" />
                ) : (
                  <ToggleLeft className="h-4 w-4 text-muted-foreground" />
                )}
              </Button>
            </TooltipTrigger>
            <TooltipContent>
              {condition.enabled !== false ? 'Desabilitar' : 'Habilitar'}
            </TooltipContent>
          </Tooltip>
        </TooltipProvider>

        <TooltipProvider>
          <Tooltip>
            <TooltipTrigger asChild>
              <Button
                variant="ghost"
                size="icon"
                className="h-8 w-8 text-destructive hover:text-destructive"
                onClick={onDelete}
              >
                <Trash2 className="h-4 w-4" />
              </Button>
            </TooltipTrigger>
            <TooltipContent>Remover condição</TooltipContent>
          </Tooltip>
        </TooltipProvider>
      </div>
    </div>
  );
});
