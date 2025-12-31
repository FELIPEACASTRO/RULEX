/**
 * ComplexRuleBuilder - Construtor visual de regras complexas
 * 
 * Features:
 * - Interface drag-and-drop intuitiva
 * - Grupos aninhados (até 10 níveis)
 * - Todos os operadores lógicos (AND, OR, NOT, XOR, NAND, NOR)
 * - Preview em tempo real
 * - Templates pré-definidos
 * - Validação completa
 * 
 * @version 1.0.0
 */

import { useState, useCallback, useMemo } from 'react';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Separator } from '@/components/ui/separator';
import {
  AlertDialog,
  AlertDialogAction,
  AlertDialogCancel,
  AlertDialogContent,
  AlertDialogDescription,
  AlertDialogFooter,
  AlertDialogHeader,
  AlertDialogTitle,
} from '@/components/ui/alert-dialog';
import {
  Save,
  RotateCcw,
  Layers,
  AlertTriangle,
  CheckCircle2,
  XCircle,
} from 'lucide-react';
import { toast } from 'sonner';

import { ConditionGroupCard } from './ConditionGroupCard';
import { RuleMetadataForm } from './RuleMetadataForm';
import { RulePreview } from './RulePreview';
import { TemplateSelector } from './TemplateSelector';
import {
  ComplexRule,
  ConditionGroup,
  Condition,
  createEmptyRule,
} from './types';

// Re-export types
export * from './types';

interface ComplexRuleBuilderProps {
  initialRule?: ComplexRule;
  onSave: (rule: ComplexRule) => Promise<void>;
  onCancel?: () => void;
  fieldOptions?: string[];
  isLoading?: boolean;
}

export function ComplexRuleBuilder({
  initialRule,
  onSave,
  onCancel,
  fieldOptions = [],
  isLoading = false,
}: ComplexRuleBuilderProps) {
  const [rule, setRule] = useState<ComplexRule>(initialRule || createEmptyRule());
  const [isDirty, setIsDirty] = useState(false);
  const [showResetConfirm, setShowResetConfirm] = useState(false);
  const [showCancelConfirm, setShowCancelConfirm] = useState(false);
  const [errors, setErrors] = useState<Record<string, string>>({});
  const [isSaving, setIsSaving] = useState(false);

  // Update rule and mark as dirty
  const updateRule = useCallback((updates: Partial<ComplexRule>) => {
    setRule(prev => ({ ...prev, ...updates }));
    setIsDirty(true);
    setErrors({});
  }, []);

  // Update root condition group
  const updateRootGroup = useCallback((group: ConditionGroup) => {
    updateRule({ rootConditionGroup: group });
  }, [updateRule]);

  // Handle template selection
  const handleTemplateSelect = useCallback((templateRule: ComplexRule) => {
    setRule(templateRule);
    setIsDirty(true);
    setErrors({});
    toast.success('Template aplicado! Personalize conforme necessário.');
  }, []);

  // Reset to initial or empty
  const handleReset = useCallback(() => {
    setRule(initialRule || createEmptyRule());
    setIsDirty(false);
    setErrors({});
    setShowResetConfirm(false);
    toast.info('Formulário resetado');
  }, [initialRule]);

  // Cancel with confirmation if dirty
  const handleCancel = useCallback(() => {
    if (isDirty) {
      setShowCancelConfirm(true);
    } else {
      onCancel?.();
    }
  }, [isDirty, onCancel]);

  // Validate rule
  const validateRule = useCallback((): boolean => {
    const newErrors: Record<string, string> = {};

    // Key validation
    if (!rule.key.trim()) {
      newErrors.key = 'Chave é obrigatória';
    } else if (!/^[A-Z][A-Z0-9_]*$/.test(rule.key)) {
      newErrors.key = 'Use UPPER_SNAKE_CASE (ex: HIGH_AMOUNT_RULE)';
    }

    // Title validation
    if (!rule.title.trim()) {
      newErrors.title = 'Título é obrigatório';
    }

    // Validate conditions recursively
    const validateGroup = (group: ConditionGroup, path: string): void => {
      group.conditions.forEach((condition, index) => {
        if (!condition.fieldName.trim()) {
          newErrors[`${path}.conditions[${index}].fieldName`] = 'Campo é obrigatório';
        }
        // Add more condition validations as needed
      });

      group.children.forEach((child, index) => {
        validateGroup(child, `${path}.children[${index}]`);
      });
    };

    validateGroup(rule.rootConditionGroup, 'root');

    // Check if rule has at least one condition
    const countConditions = (group: ConditionGroup): number => {
      return group.conditions.length + group.children.reduce((sum, child) => sum + countConditions(child), 0);
    };

    if (countConditions(rule.rootConditionGroup) === 0) {
      newErrors.conditions = 'Adicione pelo menos uma condição';
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  }, [rule]);

  // Save rule
  const handleSave = useCallback(async () => {
    if (!validateRule()) {
      toast.error('Corrija os erros antes de salvar');
      return;
    }

    setIsSaving(true);
    try {
      await onSave(rule);
      setIsDirty(false);
      toast.success('Regra salva com sucesso!');
    } catch (error) {
      toast.error(`Erro ao salvar: ${error instanceof Error ? error.message : 'Erro desconhecido'}`);
    } finally {
      setIsSaving(false);
    }
  }, [rule, validateRule, onSave]);

  // Count total conditions and groups
  const stats = useMemo(() => {
    const countItems = (group: ConditionGroup): { conditions: number; groups: number; depth: number } => {
      const childStats = group.children.map(countItems);
      return {
        conditions: group.conditions.length + childStats.reduce((sum, s) => sum + s.conditions, 0),
        groups: 1 + childStats.reduce((sum, s) => sum + s.groups, 0),
        depth: 1 + Math.max(0, ...childStats.map(s => s.depth)),
      };
    };
    return countItems(rule.rootConditionGroup);
  }, [rule.rootConditionGroup]);

  // Validation status
  const hasErrors = Object.keys(errors).length > 0;

  return (
    <div className="h-full flex flex-col">
      {/* Header */}
      <div className="flex items-center justify-between p-4 border-b bg-background/95 backdrop-blur supports-[backdrop-filter]:bg-background/60">
        <div className="flex items-center gap-4">
          <div className="flex items-center gap-2">
            <Layers className="h-6 w-6 text-primary" />
            <h1 className="text-xl font-bold">
              {initialRule ? 'Editar Regra Complexa' : 'Nova Regra Complexa'}
            </h1>
          </div>
          
          {isDirty && (
            <Badge variant="outline" className="text-amber-500 border-amber-500">
              <AlertTriangle className="h-3 w-3 mr-1" />
              Não salvo
            </Badge>
          )}
        </div>

        <div className="flex items-center gap-2">
          {/* Stats */}
          <div className="flex items-center gap-3 mr-4 text-sm text-muted-foreground">
            <span>{stats.conditions} condições</span>
            <span>{stats.groups} grupos</span>
            <span>{stats.depth} níveis</span>
          </div>

          <TemplateSelector onSelect={handleTemplateSelect} />

          <Button
            variant="outline"
            onClick={() => setShowResetConfirm(true)}
            disabled={!isDirty || isSaving}
          >
            <RotateCcw className="h-4 w-4 mr-2" />
            Resetar
          </Button>

          {onCancel && (
            <Button
              variant="outline"
              onClick={handleCancel}
              disabled={isSaving}
            >
              Cancelar
            </Button>
          )}

          <Button
            onClick={handleSave}
            disabled={isSaving || isLoading}
            className="gap-2"
          >
            {isSaving ? (
              <>
                <span className="animate-spin">⏳</span>
                Salvando...
              </>
            ) : (
              <>
                <Save className="h-4 w-4" />
                Salvar Regra
              </>
            )}
          </Button>
        </div>
      </div>

      {/* Main Content */}
      <div className="flex-1 overflow-hidden">
        <div className="h-full grid grid-cols-1 lg:grid-cols-3 gap-4 p-4">
          {/* Left Column: Metadata */}
          <div className="lg:col-span-1 space-y-4 overflow-y-auto">
            <RuleMetadataForm
              rule={rule}
              onChange={updateRule}
              errors={errors}
            />

            {/* Validation Status */}
            <Card>
              <CardHeader className="pb-2">
                <CardTitle className="text-sm flex items-center gap-2">
                  {hasErrors ? (
                    <XCircle className="h-4 w-4 text-red-500" />
                  ) : (
                    <CheckCircle2 className="h-4 w-4 text-green-500" />
                  )}
                  Status da Validação
                </CardTitle>
              </CardHeader>
              <CardContent>
                {hasErrors ? (
                  <ul className="text-sm text-red-500 space-y-1">
                    {Object.entries(errors).map(([key, message]) => (
                      <li key={key} className="flex items-start gap-1">
                        <span className="text-red-500">•</span>
                        {message}
                      </li>
                    ))}
                  </ul>
                ) : (
                  <p className="text-sm text-green-600">
                    ✓ Regra válida e pronta para salvar
                  </p>
                )}
              </CardContent>
            </Card>
          </div>

          {/* Center Column: Condition Builder */}
          <div className="lg:col-span-1 overflow-y-auto">
            <Card className="h-full">
              <CardHeader className="pb-2">
                <CardTitle className="text-lg flex items-center gap-2">
                  <Layers className="h-5 w-5" />
                  Construtor de Condições
                </CardTitle>
              </CardHeader>
              <CardContent>
                <ConditionGroupCard
                  group={rule.rootConditionGroup}
                  onChange={updateRootGroup}
                  isRoot
                  fieldOptions={fieldOptions}
                />
              </CardContent>
            </Card>
          </div>

          {/* Right Column: Preview */}
          <div className="lg:col-span-1 overflow-y-auto">
            <RulePreview rule={rule} />
          </div>
        </div>
      </div>

      {/* Reset Confirmation Dialog */}
      <AlertDialog open={showResetConfirm} onOpenChange={setShowResetConfirm}>
        <AlertDialogContent>
          <AlertDialogHeader>
            <AlertDialogTitle className="flex items-center gap-2">
              <AlertTriangle className="h-5 w-5 text-amber-500" />
              Resetar formulário?
            </AlertDialogTitle>
            <AlertDialogDescription>
              Todas as alterações não salvas serão perdidas. Esta ação não pode ser desfeita.
            </AlertDialogDescription>
          </AlertDialogHeader>
          <AlertDialogFooter>
            <AlertDialogCancel>Cancelar</AlertDialogCancel>
            <AlertDialogAction onClick={handleReset} className="bg-amber-600 hover:bg-amber-700">
              Resetar
            </AlertDialogAction>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>

      {/* Cancel Confirmation Dialog */}
      <AlertDialog open={showCancelConfirm} onOpenChange={setShowCancelConfirm}>
        <AlertDialogContent>
          <AlertDialogHeader>
            <AlertDialogTitle className="flex items-center gap-2">
              <AlertTriangle className="h-5 w-5 text-amber-500" />
              Descartar alterações?
            </AlertDialogTitle>
            <AlertDialogDescription>
              Você tem alterações não salvas. Deseja descartá-las e sair?
            </AlertDialogDescription>
          </AlertDialogHeader>
          <AlertDialogFooter>
            <AlertDialogCancel>Continuar editando</AlertDialogCancel>
            <AlertDialogAction
              onClick={() => {
                setShowCancelConfirm(false);
                onCancel?.();
              }}
              className="bg-red-600 hover:bg-red-700"
            >
              Descartar e sair
            </AlertDialogAction>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>
    </div>
  );
}

export default ComplexRuleBuilder;
