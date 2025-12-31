/**
 * ConditionGroupCard - Card visual para um grupo de condições
 * Suporta aninhamento recursivo com visual intuitivo
 */

import { memo, useCallback, useState } from 'react';
import {
  Plus,
  FolderPlus,
  Trash2,
  ChevronDown,
  ChevronRight,
  GripVertical,
  Copy,
} from 'lucide-react';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import {
  Select,
  SelectContent,
  SelectItem,
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
  Collapsible,
  CollapsibleContent,
  CollapsibleTrigger,
} from '@/components/ui/collapsible';
import { ConditionCard } from './ConditionCard';
import {
  ConditionGroup,
  Condition,
  LogicOperator,
  LOGIC_OPERATORS,
  createEmptyCondition,
  createEmptyGroup,
} from './types';

interface ConditionGroupCardProps {
  group: ConditionGroup;
  onChange: (group: ConditionGroup) => void;
  onDelete?: () => void;
  onDuplicate?: () => void;
  depth?: number;
  isRoot?: boolean;
  fieldOptions?: string[];
}

const DEPTH_COLORS = [
  'border-l-blue-500',
  'border-l-green-500',
  'border-l-purple-500',
  'border-l-orange-500',
  'border-l-pink-500',
  'border-l-cyan-500',
];

const DEPTH_BG_COLORS = [
  'bg-blue-50 dark:bg-blue-950/20',
  'bg-green-50 dark:bg-green-950/20',
  'bg-purple-50 dark:bg-purple-950/20',
  'bg-orange-50 dark:bg-orange-950/20',
  'bg-pink-50 dark:bg-pink-950/20',
  'bg-cyan-50 dark:bg-cyan-950/20',
];

export const ConditionGroupCard = memo(function ConditionGroupCard({
  group,
  onChange,
  onDelete,
  onDuplicate,
  depth = 0,
  isRoot = false,
  fieldOptions = [],
}: ConditionGroupCardProps) {
  const [isExpanded, setIsExpanded] = useState(true);

  const colorIndex = depth % DEPTH_COLORS.length;
  const borderColor = DEPTH_COLORS[colorIndex];
  const bgColor = DEPTH_BG_COLORS[colorIndex];

  const logicOperatorInfo = LOGIC_OPERATORS.find(op => op.value === group.logicOperator);

  // Handlers
  const handleLogicOperatorChange = useCallback((value: string) => {
    onChange({ ...group, logicOperator: value as LogicOperator });
  }, [group, onChange]);

  const handleAddCondition = useCallback(() => {
    onChange({
      ...group,
      conditions: [...group.conditions, createEmptyCondition()],
    });
  }, [group, onChange]);

  const handleAddGroup = useCallback(() => {
    onChange({
      ...group,
      children: [...group.children, createEmptyGroup('AND')],
    });
  }, [group, onChange]);

  const handleConditionChange = useCallback((index: number, condition: Condition) => {
    const newConditions = [...group.conditions];
    newConditions[index] = condition;
    onChange({ ...group, conditions: newConditions });
  }, [group, onChange]);

  const handleConditionDelete = useCallback((index: number) => {
    const newConditions = group.conditions.filter((_, i) => i !== index);
    onChange({ ...group, conditions: newConditions });
  }, [group, onChange]);

  const handleChildGroupChange = useCallback((index: number, childGroup: ConditionGroup) => {
    const newChildren = [...group.children];
    newChildren[index] = childGroup;
    onChange({ ...group, children: newChildren });
  }, [group, onChange]);

  const handleChildGroupDelete = useCallback((index: number) => {
    const newChildren = group.children.filter((_, i) => i !== index);
    onChange({ ...group, children: newChildren });
  }, [group, onChange]);

  const handleChildGroupDuplicate = useCallback((index: number) => {
    const childToDuplicate = group.children[index];
    const duplicated: ConditionGroup = {
      ...JSON.parse(JSON.stringify(childToDuplicate)),
      id: crypto.randomUUID(),
    };
    // Regenerate IDs for nested items
    const regenerateIds = (g: ConditionGroup): ConditionGroup => ({
      ...g,
      id: crypto.randomUUID(),
      conditions: g.conditions.map(c => ({ ...c, id: crypto.randomUUID() })),
      children: g.children.map(regenerateIds),
    });
    const newChildren = [...group.children];
    newChildren.splice(index + 1, 0, regenerateIds(duplicated));
    onChange({ ...group, children: newChildren });
  }, [group, onChange]);

  const totalItems = group.conditions.length + group.children.length;

  return (
    <div
      className={`
        relative rounded-lg border-l-4 ${borderColor} ${bgColor}
        ${isRoot ? '' : 'ml-4'}
        transition-all
      `}
    >
      {/* Header */}
      <div className="flex items-center gap-2 p-3 border-b border-border/50">
        {/* Drag Handle (não para root) */}
        {!isRoot && (
          <div className="cursor-grab active:cursor-grabbing text-muted-foreground hover:text-foreground">
            <GripVertical className="h-4 w-4" />
          </div>
        )}

        {/* Collapse Toggle */}
        <Collapsible open={isExpanded} onOpenChange={setIsExpanded}>
          <CollapsibleTrigger asChild>
            <Button variant="ghost" size="icon" className="h-6 w-6">
              {isExpanded ? (
                <ChevronDown className="h-4 w-4" />
              ) : (
                <ChevronRight className="h-4 w-4" />
              )}
            </Button>
          </CollapsibleTrigger>
        </Collapsible>

        {/* Logic Operator Selector */}
        <Select value={group.logicOperator} onValueChange={handleLogicOperatorChange}>
          <SelectTrigger className="w-[100px] h-8">
            <SelectValue />
          </SelectTrigger>
          <SelectContent>
            {LOGIC_OPERATORS.map((op) => (
              <SelectItem key={op.value} value={op.value}>
                <div className="flex items-center gap-2">
                  <Badge className={`${op.color} text-white text-xs`}>
                    {op.label}
                  </Badge>
                </div>
              </SelectItem>
            ))}
          </SelectContent>
        </Select>

        {/* Description */}
        <span className="text-sm text-muted-foreground flex-1">
          {logicOperatorInfo?.description}
        </span>

        {/* Item Count */}
        <Badge variant="secondary" className="text-xs">
          {totalItems} {totalItems === 1 ? 'item' : 'itens'}
        </Badge>

        {/* Actions */}
        <div className="flex items-center gap-1">
          <TooltipProvider>
            <Tooltip>
              <TooltipTrigger asChild>
                <Button
                  variant="ghost"
                  size="icon"
                  className="h-8 w-8"
                  onClick={handleAddCondition}
                >
                  <Plus className="h-4 w-4" />
                </Button>
              </TooltipTrigger>
              <TooltipContent>Adicionar condição</TooltipContent>
            </Tooltip>
          </TooltipProvider>

          <TooltipProvider>
            <Tooltip>
              <TooltipTrigger asChild>
                <Button
                  variant="ghost"
                  size="icon"
                  className="h-8 w-8"
                  onClick={handleAddGroup}
                  disabled={depth >= 9} // Max 10 levels
                >
                  <FolderPlus className="h-4 w-4" />
                </Button>
              </TooltipTrigger>
              <TooltipContent>
                {depth >= 9 ? 'Máximo de 10 níveis' : 'Adicionar sub-grupo'}
              </TooltipContent>
            </Tooltip>
          </TooltipProvider>

          {!isRoot && onDuplicate && (
            <TooltipProvider>
              <Tooltip>
                <TooltipTrigger asChild>
                  <Button
                    variant="ghost"
                    size="icon"
                    className="h-8 w-8"
                    onClick={onDuplicate}
                  >
                    <Copy className="h-4 w-4" />
                  </Button>
                </TooltipTrigger>
                <TooltipContent>Duplicar grupo</TooltipContent>
              </Tooltip>
            </TooltipProvider>
          )}

          {!isRoot && onDelete && (
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
                <TooltipContent>Remover grupo</TooltipContent>
              </Tooltip>
            </TooltipProvider>
          )}
        </div>
      </div>

      {/* Content */}
      <Collapsible open={isExpanded}>
        <CollapsibleContent>
          <div className="p-3 space-y-2">
            {/* Empty State */}
            {totalItems === 0 && (
              <div className="text-center py-8 text-muted-foreground">
                <p className="text-sm">Grupo vazio</p>
                <p className="text-xs mt-1">
                  Adicione condições ou sub-grupos usando os botões acima
                </p>
                <div className="flex justify-center gap-2 mt-4">
                  <Button variant="outline" size="sm" onClick={handleAddCondition}>
                    <Plus className="h-4 w-4 mr-1" />
                    Condição
                  </Button>
                  <Button variant="outline" size="sm" onClick={handleAddGroup} disabled={depth >= 9}>
                    <FolderPlus className="h-4 w-4 mr-1" />
                    Sub-grupo
                  </Button>
                </div>
              </div>
            )}

            {/* Conditions */}
            {group.conditions.map((condition, index) => (
              <div key={condition.id} className="relative">
                {/* Logic Operator Connector */}
                {(index > 0 || group.children.length > 0) && (
                  <div className="absolute -top-1 left-4 flex items-center">
                    <Badge
                      className={`${logicOperatorInfo?.color || 'bg-gray-500'} text-white text-[10px] px-1.5 py-0`}
                    >
                      {logicOperatorInfo?.label}
                    </Badge>
                  </div>
                )}
                <ConditionCard
                  condition={condition}
                  onChange={(c) => handleConditionChange(index, c)}
                  onDelete={() => handleConditionDelete(index)}
                  fieldOptions={fieldOptions}
                />
              </div>
            ))}

            {/* Child Groups */}
            {group.children.map((childGroup, index) => (
              <div key={childGroup.id} className="relative">
                {/* Logic Operator Connector */}
                {(group.conditions.length > 0 || index > 0) && (
                  <div className="absolute -top-1 left-4 flex items-center z-10">
                    <Badge
                      className={`${logicOperatorInfo?.color || 'bg-gray-500'} text-white text-[10px] px-1.5 py-0`}
                    >
                      {logicOperatorInfo?.label}
                    </Badge>
                  </div>
                )}
                <ConditionGroupCard
                  group={childGroup}
                  onChange={(g) => handleChildGroupChange(index, g)}
                  onDelete={() => handleChildGroupDelete(index)}
                  onDuplicate={() => handleChildGroupDuplicate(index)}
                  depth={depth + 1}
                  fieldOptions={fieldOptions}
                />
              </div>
            ))}
          </div>
        </CollapsibleContent>
      </Collapsible>
    </div>
  );
});
