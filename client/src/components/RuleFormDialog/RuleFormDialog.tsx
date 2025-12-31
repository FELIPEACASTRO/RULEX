/**
 * RuleFormDialog - Componente de diálogo para criação e edição de regras
 * 
 * Features:
 * - Validação com Zod
 * - Suporte a todos os 52 operadores
 * - Campos dinâmicos via fieldDictionary API
 * - Loading states
 * - Acessibilidade completa (focus trap, keyboard navigation)
 * - Preview JSON antes de salvar
 * 
 * @version 2.0.0
 */

import { useCallback, useState } from 'react';
import { Controller } from 'react-hook-form';
import { Plus, Trash2, Loader2, Eye, EyeOff, AlertCircle } from 'lucide-react';

import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from '@/components/ui/dialog';
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
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Textarea } from '@/components/ui/textarea';
import { Label } from '@/components/ui/label';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select';
import { Switch } from '@/components/ui/switch';
import { Badge } from '@/components/ui/badge';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { ScrollArea } from '@/components/ui/scroll-area';
import { Alert, AlertDescription } from '@/components/ui/alert';

import { useRuleForm } from './useRuleForm';
import { 
  RULE_TYPES, 
  CLASSIFICATIONS, 
  LOGIC_OPERATORS,
  UNARY_OPERATORS,
  type RuleFormDialogProps 
} from './types';
import { 
  getPlaceholderForOperator, 
  validateValueByOperator,
  MAX_CONDITIONS 
} from './schema';

export function RuleFormDialog({
  open,
  onOpenChange,
  rule,
  onSuccess,
  onError,
  mode = rule ? 'edit' : 'create',
  title,
  description,
}: RuleFormDialogProps) {
  const [showUnsavedWarning, setShowUnsavedWarning] = useState(false);
  const [showJsonPreview, setShowJsonPreview] = useState(false);

  const {
    form,
    conditionsFieldArray,
    isLoading,
    isSaving,
    isEditing,
    availableFields,
    handleSubmit,
    handleReset,
    addCondition,
    removeCondition,
    getOperatorsForField,
    getFieldInfo,
  } = useRuleForm({
    rule,
    onSuccess: (result) => {
      onSuccess?.(result);
      onOpenChange(false);
    },
    onError,
  });

  const { fields: conditions, } = conditionsFieldArray;
  const { formState: { errors, isDirty } } = form;

  // Handler para fechar com verificação de alterações não salvas
  const handleClose = useCallback((newOpen: boolean) => {
    if (!newOpen && isDirty) {
      setShowUnsavedWarning(true);
      return;
    }
    onOpenChange(newOpen);
  }, [isDirty, onOpenChange]);

  // Confirmar descarte de alterações
  const confirmDiscard = useCallback(() => {
    setShowUnsavedWarning(false);
    handleReset();
    onOpenChange(false);
  }, [handleReset, onOpenChange]);

  // Gerar JSON preview
  const getJsonPreview = useCallback(() => {
    const values = form.getValues();
    return JSON.stringify({
      ruleName: values.ruleName,
      description: values.description || null,
      ruleType: values.ruleType,
      classification: values.classification,
      threshold: values.threshold,
      weight: values.weight,
      enabled: values.enabled,
      logicOperator: values.logicOperator,
      conditions: values.conditions,
      parameters: values.parameters?.trim() || null,
    }, null, 2);
  }, [form]);

  const dialogTitle = title || (isEditing ? 'Editar Regra' : 'Nova Regra');
  const dialogDescription = description || 'Configure os parâmetros da regra de detecção de fraude';

  return (
    <>
      <Dialog open={open} onOpenChange={handleClose}>
        <DialogContent className="sm:max-w-3xl max-h-[90vh] flex flex-col">
          <DialogHeader>
            <DialogTitle>{dialogTitle}</DialogTitle>
            <DialogDescription>{dialogDescription}</DialogDescription>
          </DialogHeader>

          <Tabs defaultValue="basic" className="flex-1 overflow-hidden flex flex-col">
            <TabsList className="grid w-full grid-cols-3">
              <TabsTrigger value="basic">Básico</TabsTrigger>
              <TabsTrigger value="conditions">
                Condições
                {conditions.length > 0 && (
                  <Badge variant="secondary" className="ml-2">
                    {conditions.length}
                  </Badge>
                )}
              </TabsTrigger>
              <TabsTrigger value="advanced">Avançado</TabsTrigger>
            </TabsList>

            <ScrollArea className="flex-1 pr-4">
              {/* Tab: Básico */}
              <TabsContent value="basic" className="space-y-4 mt-4">
                {/* Nome da Regra */}
                <div className="space-y-2">
                  <Label htmlFor="ruleName">
                    Nome da Regra <span className="text-destructive">*</span>
                  </Label>
                  <Controller
                    name="ruleName"
                    control={form.control}
                    render={({ field }) => (
                      <Input
                        {...field}
                        id="ruleName"
                        placeholder="Ex: HIGH_AMOUNT_RULE"
                        disabled={isEditing}
                        onChange={(e) => field.onChange(e.target.value.toUpperCase())}
                        aria-invalid={!!errors.ruleName}
                        aria-describedby={errors.ruleName ? 'ruleName-error' : undefined}
                        className={errors.ruleName ? 'border-destructive' : ''}
                      />
                    )}
                  />
                  {errors.ruleName && (
                    <p id="ruleName-error" className="text-xs text-destructive" role="alert">
                      {errors.ruleName.message}
                    </p>
                  )}
                  <p className="text-xs text-muted-foreground">
                    Use UPPER_SNAKE_CASE (ex: HIGH_AMOUNT_RULE)
                  </p>
                </div>

                {/* Descrição */}
                <div className="space-y-2">
                  <Label htmlFor="description">Descrição</Label>
                  <Controller
                    name="description"
                    control={form.control}
                    render={({ field }) => (
                      <Textarea
                        {...field}
                        id="description"
                        placeholder="Descrição da regra..."
                        value={field.value || ''}
                        rows={2}
                      />
                    )}
                  />
                </div>

                {/* Tipo e Classificação */}
                <div className="grid grid-cols-2 gap-4">
                  <div className="space-y-2">
                    <Label htmlFor="ruleType">Tipo de Regra</Label>
                    <Controller
                      name="ruleType"
                      control={form.control}
                      render={({ field }) => (
                        <Select value={field.value} onValueChange={field.onChange}>
                          <SelectTrigger id="ruleType">
                            <SelectValue placeholder="Selecione..." />
                          </SelectTrigger>
                          <SelectContent>
                            {RULE_TYPES.map((type) => (
                              <SelectItem key={type.value} value={type.value}>
                                {type.label}
                              </SelectItem>
                            ))}
                          </SelectContent>
                        </Select>
                      )}
                    />
                  </div>

                  <div className="space-y-2">
                    <Label htmlFor="classification">Classificação</Label>
                    <Controller
                      name="classification"
                      control={form.control}
                      render={({ field }) => (
                        <Select value={field.value} onValueChange={field.onChange}>
                          <SelectTrigger id="classification">
                            <SelectValue placeholder="Selecione..." />
                          </SelectTrigger>
                          <SelectContent>
                            {CLASSIFICATIONS.map((cls) => (
                              <SelectItem key={cls.value} value={cls.value}>
                                <span className="flex items-center gap-2">
                                  <span>{cls.icon}</span>
                                  <span>{cls.label}</span>
                                </span>
                              </SelectItem>
                            ))}
                          </SelectContent>
                        </Select>
                      )}
                    />
                  </div>
                </div>

                {/* Threshold e Weight */}
                <div className="grid grid-cols-2 gap-4">
                  <div className="space-y-2">
                    <Label htmlFor="threshold">Threshold</Label>
                    <Controller
                      name="threshold"
                      control={form.control}
                      render={({ field }) => (
                        <Input
                          {...field}
                          id="threshold"
                          type="number"
                          min={0}
                          max={1000}
                          onChange={(e) => field.onChange(parseInt(e.target.value) || 0)}
                          aria-invalid={!!errors.threshold}
                          className={errors.threshold ? 'border-destructive' : ''}
                        />
                      )}
                    />
                    {errors.threshold && (
                      <p className="text-xs text-destructive">{errors.threshold.message}</p>
                    )}
                  </div>

                  <div className="space-y-2">
                    <Label htmlFor="weight">Peso (0-100)</Label>
                    <Controller
                      name="weight"
                      control={form.control}
                      render={({ field }) => (
                        <Input
                          {...field}
                          id="weight"
                          type="number"
                          min={0}
                          max={100}
                          onChange={(e) => field.onChange(parseInt(e.target.value) || 0)}
                          aria-invalid={!!errors.weight}
                          className={errors.weight ? 'border-destructive' : ''}
                        />
                      )}
                    />
                    {errors.weight && (
                      <p className="text-xs text-destructive">{errors.weight.message}</p>
                    )}
                  </div>
                </div>

                {/* Enabled */}
                <div className="flex items-center justify-between rounded-lg border p-4">
                  <div className="space-y-0.5">
                    <Label htmlFor="enabled">Regra Ativa</Label>
                    <p className="text-xs text-muted-foreground">
                      Regras desativadas não serão avaliadas
                    </p>
                  </div>
                  <Controller
                    name="enabled"
                    control={form.control}
                    render={({ field }) => (
                      <Switch
                        id="enabled"
                        checked={field.value}
                        onCheckedChange={field.onChange}
                      />
                    )}
                  />
                </div>
              </TabsContent>

              {/* Tab: Condições */}
              <TabsContent value="conditions" className="space-y-4 mt-4">
                {/* Operador Lógico */}
                <div className="flex items-center justify-between">
                  <div>
                    <Label>Operador Lógico</Label>
                    <p className="text-xs text-muted-foreground">
                      Como as condições serão combinadas
                    </p>
                  </div>
                  <Controller
                    name="logicOperator"
                    control={form.control}
                    render={({ field }) => (
                      <Select value={field.value} onValueChange={field.onChange}>
                        <SelectTrigger className="w-32">
                          <SelectValue />
                        </SelectTrigger>
                        <SelectContent>
                          {LOGIC_OPERATORS.map((op) => (
                            <SelectItem key={op.value} value={op.value}>
                              {op.label}
                            </SelectItem>
                          ))}
                        </SelectContent>
                      </Select>
                    )}
                  />
                </div>

                {/* Lista de Condições */}
                <div className="space-y-3">
                  {conditions.length === 0 ? (
                    <Alert>
                      <AlertCircle className="h-4 w-4" />
                      <AlertDescription>
                        Nenhuma condição adicionada. Clique em "Adicionar Condição" para começar.
                      </AlertDescription>
                    </Alert>
                  ) : (
                    conditions.map((condition, index) => {
                      const fieldInfo = getFieldInfo(form.watch(`conditions.${index}.field`));
                      const currentOperator = form.watch(`conditions.${index}.operator`);
                      const isUnary = UNARY_OPERATORS.includes(currentOperator as any);
                      const operators = getOperatorsForField(form.watch(`conditions.${index}.field`));
                      const valueError = validateValueByOperator(
                        currentOperator,
                        form.watch(`conditions.${index}.value`),
                        fieldInfo?.type
                      );

                      return (
                        <div
                          key={condition.id}
                          className="grid grid-cols-12 gap-2 items-start p-3 rounded-lg border bg-muted/30"
                        >
                          {/* Campo */}
                          <div className="col-span-4 space-y-1">
                            <Label className="text-xs">Campo</Label>
                            <Controller
                              name={`conditions.${index}.field`}
                              control={form.control}
                              render={({ field }) => (
                                <Input
                                  {...field}
                                  placeholder="Ex: transactionAmount"
                                  list={`fields-${index}`}
                                  className="h-9"
                                />
                              )}
                            />
                            <datalist id={`fields-${index}`}>
                              {availableFields.map((f) => (
                                <option key={f.value} value={f.value} />
                              ))}
                            </datalist>
                            {errors.conditions?.[index]?.field && (
                              <p className="text-xs text-destructive">
                                {errors.conditions[index]?.field?.message}
                              </p>
                            )}
                          </div>

                          {/* Operador */}
                          <div className="col-span-3 space-y-1">
                            <Label className="text-xs">Operador</Label>
                            <Controller
                              name={`conditions.${index}.operator`}
                              control={form.control}
                              render={({ field }) => (
                                <Select value={field.value} onValueChange={field.onChange}>
                                  <SelectTrigger className="h-9">
                                    <SelectValue />
                                  </SelectTrigger>
                                  <SelectContent>
                                    {operators.map((op) => (
                                      <SelectItem key={op.value} value={op.value}>
                                        {op.label}
                                      </SelectItem>
                                    ))}
                                  </SelectContent>
                                </Select>
                              )}
                            />
                          </div>

                          {/* Valor */}
                          <div className="col-span-4 space-y-1">
                            <Label className="text-xs">Valor</Label>
                            {isUnary ? (
                              <Input
                                value=""
                                disabled
                                placeholder="(não aplicável)"
                                className="h-9"
                              />
                            ) : (
                              <Controller
                                name={`conditions.${index}.value`}
                                control={form.control}
                                render={({ field }) => (
                                  <Input
                                    {...field}
                                    placeholder={getPlaceholderForOperator(currentOperator)}
                                    className={`h-9 ${valueError ? 'border-destructive' : ''}`}
                                  />
                                )}
                              />
                            )}
                            {valueError && (
                              <p className="text-xs text-destructive">{valueError}</p>
                            )}
                          </div>

                          {/* Remover */}
                          <div className="col-span-1 flex items-end justify-center pb-1">
                            <Button
                              type="button"
                              variant="ghost"
                              size="icon"
                              className="h-9 w-9 text-destructive hover:text-destructive"
                              onClick={() => removeCondition(index)}
                              aria-label={`Remover condição ${index + 1}`}
                            >
                              <Trash2 className="h-4 w-4" />
                            </Button>
                          </div>
                        </div>
                      );
                    })
                  )}
                </div>

                {/* Adicionar Condição */}
                <Button
                  type="button"
                  variant="outline"
                  onClick={addCondition}
                  disabled={conditions.length >= MAX_CONDITIONS}
                  className="w-full"
                >
                  <Plus className="h-4 w-4 mr-2" />
                  Adicionar Condição
                  {conditions.length > 0 && (
                    <span className="ml-2 text-muted-foreground">
                      ({conditions.length}/{MAX_CONDITIONS})
                    </span>
                  )}
                </Button>
              </TabsContent>

              {/* Tab: Avançado */}
              <TabsContent value="advanced" className="space-y-4 mt-4">
                {/* Parâmetros JSON */}
                <div className="space-y-2">
                  <Label htmlFor="parameters">Parâmetros (JSON)</Label>
                  <Controller
                    name="parameters"
                    control={form.control}
                    render={({ field }) => (
                      <Textarea
                        {...field}
                        id="parameters"
                        placeholder='{"velocity": {"metric": "COUNT", ...}}'
                        value={field.value || ''}
                        rows={6}
                        className="font-mono text-sm"
                      />
                    )}
                  />
                  <p className="text-xs text-muted-foreground">
                    JSON livre para configurações avançadas (ex: velocity, state)
                  </p>
                </div>

                {/* Preview JSON */}
                <div className="space-y-2">
                  <div className="flex items-center justify-between">
                    <Label>Preview da Regra</Label>
                    <Button
                      type="button"
                      variant="ghost"
                      size="sm"
                      onClick={() => setShowJsonPreview(!showJsonPreview)}
                    >
                      {showJsonPreview ? (
                        <>
                          <EyeOff className="h-4 w-4 mr-2" />
                          Ocultar
                        </>
                      ) : (
                        <>
                          <Eye className="h-4 w-4 mr-2" />
                          Mostrar JSON
                        </>
                      )}
                    </Button>
                  </div>
                  {showJsonPreview && (
                    <pre className="p-4 rounded-lg bg-muted text-xs font-mono overflow-auto max-h-64">
                      {getJsonPreview()}
                    </pre>
                  )}
                </div>
              </TabsContent>
            </ScrollArea>
          </Tabs>

          <DialogFooter className="gap-2 sm:gap-0">
            <Button
              type="button"
              variant="outline"
              onClick={() => handleClose(false)}
              disabled={isSaving}
            >
              Cancelar
            </Button>
            <Button
              type="button"
              onClick={handleSubmit}
              disabled={isSaving || isLoading}
            >
              {isSaving ? (
                <>
                  <Loader2 className="h-4 w-4 mr-2 animate-spin" />
                  Salvando...
                </>
              ) : (
                isEditing ? 'Salvar Alterações' : 'Criar Regra'
              )}
            </Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>

      {/* Dialog de confirmação para alterações não salvas */}
      <AlertDialog open={showUnsavedWarning} onOpenChange={setShowUnsavedWarning}>
        <AlertDialogContent>
          <AlertDialogHeader>
            <AlertDialogTitle>Alterações não salvas</AlertDialogTitle>
            <AlertDialogDescription>
              Você tem alterações não salvas. Deseja descartá-las?
            </AlertDialogDescription>
          </AlertDialogHeader>
          <AlertDialogFooter>
            <AlertDialogCancel>Continuar Editando</AlertDialogCancel>
            <AlertDialogAction onClick={confirmDiscard}>
              Descartar Alterações
            </AlertDialogAction>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>
    </>
  );
}

export default RuleFormDialog;
