import { Loader2, Plus } from 'lucide-react';

import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Textarea } from '@/components/ui/textarea';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
  DialogTrigger,
} from '@/components/ui/dialog';
import { Switch } from '@/components/ui/switch';
import { FieldDictionaryItem, RuleConfiguration } from '@/lib/javaApi';

type RuleFormData = {
  ruleName: string;
  description: string;
  ruleType: RuleConfiguration['ruleType'];
  threshold: number;
  weight: number;
  enabled: boolean;
  classification: RuleConfiguration['classification'];
  parameters: string;
  logicOperator: RuleConfiguration['logicOperator'];
  conditions: RuleConfiguration['conditions'];
};

type RulesFormDialogProps = {
  open: boolean;
  onOpenChange: (open: boolean) => void;
  editingRule: RuleConfiguration | null;
  requiresApproval: boolean;
  onRequiresApprovalChange: (checked: boolean) => void;
  formData: RuleFormData;
  validationErrors: Record<string, string>;
  fieldDictionary: FieldDictionaryItem[];
  fieldDictionaryError: boolean;
  onUpdateFormData: (updates: Partial<RuleFormData>) => void;
  onResetForm: () => void;
  onClose: () => void;
  onSave: () => void;
  isSaving: boolean;
};

export function RulesFormDialog({
  open,
  onOpenChange,
  editingRule,
  requiresApproval,
  onRequiresApprovalChange,
  formData,
  validationErrors,
  fieldDictionary,
  fieldDictionaryError,
  onUpdateFormData,
  onResetForm,
  onClose,
  onSave,
  isSaving,
}: RulesFormDialogProps) {
  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogTrigger asChild>
        <Button onClick={onResetForm}>
          <Plus className="h-4 w-4 mr-2" />
          Nova Regra
        </Button>
      </DialogTrigger>
      <DialogContent className="sm:max-w-2xl">
        <DialogHeader>
          <DialogTitle>{editingRule ? 'Editar Regra' : 'Nova Regra'}</DialogTitle>
          <DialogDescription>
            Configure os parâmetros da regra de detecção de fraude
          </DialogDescription>
        </DialogHeader>
        <div className="space-y-4">
          <div className="flex items-center justify-between rounded-lg border border-border p-3">
            <div>
              <p className="text-sm font-medium text-foreground">Governança</p>
              <p className="text-xs text-muted-foreground">
                Envie esta regra para aprovação antes de publicar.
              </p>
            </div>
            <div className="flex items-center gap-2">
              <Switch
                id="requiresApproval"
                checked={requiresApproval}
                onCheckedChange={onRequiresApprovalChange}
              />
              <label htmlFor="requiresApproval" className="text-sm text-muted-foreground">
                Requer aprovação
              </label>
            </div>
          </div>
          <div>
            <label htmlFor="ruleName" className="block text-sm font-medium text-foreground mb-2">
              Nome da Regra
            </label>
            <Input
              id="ruleName"
              value={formData.ruleName}
              onChange={(e) => onUpdateFormData({ ruleName: e.target.value })}
              placeholder="EX: HIGH_AMOUNT_RULE"
              aria-invalid={!!validationErrors.ruleName}
              aria-describedby={validationErrors.ruleName ? 'ruleName-error' : undefined}
              className={validationErrors.ruleName ? 'border-red-500 focus:ring-red-500' : ''}
            />
            {validationErrors.ruleName && (
              <p id="ruleName-error" className="mt-1 text-xs text-red-500" role="alert">
                {validationErrors.ruleName}
              </p>
            )}
            <p className="mt-1 text-xs text-muted-foreground">
              Use UPPER_SNAKE_CASE (ex: HIGH_AMOUNT_RULE)
            </p>
          </div>
          <div>
            <label htmlFor="description" className="block text-sm font-medium text-foreground mb-2">
              Descrição
            </label>
            <Input
              id="description"
              value={formData.description}
              onChange={(e) => onUpdateFormData({ description: e.target.value })}
              placeholder="Descrição da regra"
            />
          </div>
          <div>
            <label htmlFor="parameters" className="block text-sm font-medium text-foreground mb-2">
              Parâmetros (JSON) — opcional
            </label>
            <Textarea
              id="parameters"
              value={formData.parameters}
              onChange={(e) => onUpdateFormData({ parameters: e.target.value })}
              placeholder={
                formData.ruleType === 'VELOCITY'
                  ? `Ex (velocity/state):\n{\n  "velocity": {\n    "metric": "COUNT",\n    "dimension": "CUSTOMER",\n    "windowSeconds": 3600,\n    "operator": "GT",\n    "threshold": 3\n  }\n}`
                  : 'JSON livre (ex.: configs avançadas futuras)'
              }
              rows={formData.ruleType === 'VELOCITY' ? 8 : 4}
            />
            {formData.ruleType === 'VELOCITY' ? (
              <p className="mt-1 text-xs text-muted-foreground">
                Para regras de velocidade, use <code>metric</code> = <code>COUNT</code> ou{' '}
                <code>SUM_AMOUNT</code>, <code>dimension</code> = <code>CUSTOMER</code> /{' '}
                <code>MERCHANT</code> / <code>GLOBAL</code>.
              </p>
            ) : null}
          </div>
          <div className="grid grid-cols-2 gap-4">
            <div>
              <label htmlFor="ruleType" className="block text-sm font-medium text-foreground mb-2">
                Tipo de Regra
              </label>
              <select
                id="ruleType"
                value={formData.ruleType}
                onChange={(e) =>
                  onUpdateFormData({
                    ruleType: e.target.value as RuleConfiguration['ruleType'],
                  })
                }
                className="w-full px-3 py-2 border border-input rounded-lg bg-background text-foreground"
              >
                <option value="SECURITY">Segurança</option>
                <option value="CONTEXT">Contexto</option>
                <option value="VELOCITY">Velocidade</option>
                <option value="ANOMALY">Anomalia</option>
              </select>
            </div>
            <div>
              <label htmlFor="classification" className="block text-sm font-medium text-foreground mb-2">
                Classificação
              </label>
              <select
                id="classification"
                value={formData.classification}
                onChange={(e) =>
                  onUpdateFormData({
                    classification: e.target.value as RuleConfiguration['classification'],
                  })
                }
                className="w-full px-3 py-2 border border-input rounded-lg bg-background text-foreground"
              >
                <option value="APPROVED">Aprovada</option>
                <option value="SUSPICIOUS">Suspeita</option>
                <option value="FRAUD">Fraude</option>
              </select>
            </div>
          </div>
          <div className="grid grid-cols-2 gap-4">
            <div>
              <label htmlFor="threshold" className="block text-sm font-medium text-foreground mb-2">
                Threshold
              </label>
              <Input
                id="threshold"
                type="number"
                value={formData.threshold}
                onChange={(e) =>
                  onUpdateFormData({
                    threshold: e.target.value === '' ? 0 : parseInt(e.target.value, 10),
                  })
                }
                placeholder="0"
                aria-invalid={!!validationErrors.threshold}
                className={validationErrors.threshold ? 'border-red-500 focus:ring-red-500' : ''}
              />
              {validationErrors.threshold && (
                <p className="mt-1 text-xs text-red-500" role="alert">
                  {validationErrors.threshold}
                </p>
              )}
            </div>
            <div>
              <label htmlFor="weight" className="block text-sm font-medium text-foreground mb-2">
                Peso (0-100)
              </label>
              <Input
                id="weight"
                type="number"
                min="0"
                max="100"
                value={formData.weight}
                onChange={(e) =>
                  onUpdateFormData({
                    weight: e.target.value === '' ? 0 : parseInt(e.target.value, 10),
                  })
                }
                placeholder="0"
                aria-invalid={!!validationErrors.weight}
                className={validationErrors.weight ? 'border-red-500 focus:ring-red-500' : ''}
              />
              {validationErrors.weight && (
                <p className="mt-1 text-xs text-red-500" role="alert">
                  {validationErrors.weight}
                </p>
              )}
            </div>
          </div>
          {/* Condições / "subregras" (engine genérico) */}
          <div className="rounded-lg border border-border p-4">
            <div className="flex flex-col gap-1 sm:flex-row sm:items-center sm:justify-between">
              <div>
                <p className="text-sm font-medium text-foreground">Condições (subregras)</p>
                <p className="text-xs text-muted-foreground">
                  Para regras genéricas, adicione condições. Para regras legadas por nome, pode deixar vazio.
                </p>
              </div>
              <div className="flex items-center gap-2">
                <label htmlFor="logicOperator" className="text-xs font-medium text-muted-foreground">
                  Operador
                </label>
                <select
                  id="logicOperator"
                  value={formData.logicOperator}
                  onChange={(e) =>
                    onUpdateFormData({
                      logicOperator: e.target.value as RuleConfiguration['logicOperator'],
                    })
                  }
                  className="px-3 py-2 border border-input rounded-lg bg-background text-foreground"
                >
                  <option value="AND">AND</option>
                  <option value="OR">OR</option>
                </select>
              </div>
            </div>

            <div className="mt-3 space-y-3">
              {formData.conditions.length === 0 ? (
                <p className="text-sm text-muted-foreground">Nenhuma condição adicionada.</p>
              ) : null}

              {formData.conditions.map((c, idx) => {
                const fieldOptions = fieldDictionary
                  .map((f) => (f.jsonPath?.startsWith('$.') ? f.jsonPath.slice(2) : f.jsonPath))
                  .filter(Boolean);
                const currentField = fieldDictionary.find(
                  (f) => (f.jsonPath?.startsWith('$.') ? f.jsonPath.slice(2) : f.jsonPath) === c.field,
                );
                const typeForField = currentField?.type ?? 'unknown';

                const baseOps: RuleConfiguration['conditions'][number]['operator'][] = [
                  'EQ',
                  'NE',
                  'GT',
                  'LT',
                  'GTE',
                  'LTE',
                  'IN',
                  'NOT_IN',
                  'BETWEEN',
                  'NOT_BETWEEN',
                  'CONTAINS',
                  'NOT_CONTAINS',
                  'STARTS_WITH',
                  'ENDS_WITH',
                  'MATCHES_REGEX',
                  'IS_NULL',
                  'IS_NOT_NULL',
                  'IS_TRUE',
                  'IS_FALSE',
                ];
                const opsFromCatalog =
                  currentField?.allowedOperators && currentField.allowedOperators.length > 0
                    ? (currentField.allowedOperators as RuleConfiguration['conditions'][number]['operator'][])
                    : null;
                const opsFallback =
                  typeForField === 'number'
                    ? baseOps.filter(
                        (o) =>
                          ![
                            'CONTAINS',
                            'NOT_CONTAINS',
                            'STARTS_WITH',
                            'ENDS_WITH',
                            'MATCHES_REGEX',
                          ].includes(o),
                      )
                    : typeForField === 'boolean'
                      ? baseOps.filter((o) => ['IS_TRUE', 'IS_FALSE', 'IS_NULL', 'IS_NOT_NULL'].includes(o))
                      : baseOps;
                const ops = opsFromCatalog ?? opsFallback;
                const isUnary = ['IS_NULL', 'IS_NOT_NULL', 'IS_TRUE', 'IS_FALSE'].includes(c.operator);
                const operatorLabel = (op: string) => {
                  const map: Record<string, string> = {
                    EQ: '== (EQ)',
                    NE: '!= (NE)',
                    GT: '> (GT)',
                    LT: '< (LT)',
                    GTE: '>= (GTE)',
                    LTE: '<= (LTE)',
                    IN: 'IN',
                    NOT_IN: 'NOT IN',
                    BETWEEN: 'BETWEEN',
                    NOT_BETWEEN: 'NOT BETWEEN',
                    CONTAINS: 'CONTAINS',
                    NOT_CONTAINS: 'NOT CONTAINS',
                    STARTS_WITH: 'STARTS WITH',
                    ENDS_WITH: 'ENDS WITH',
                    MATCHES_REGEX: 'MATCHES REGEX',
                    IS_NULL: 'IS NULL',
                    IS_NOT_NULL: 'IS NOT NULL',
                    IS_TRUE: 'IS TRUE',
                    IS_FALSE: 'IS FALSE',
                    '==': '==',
                    '!=': '!=',
                    '>': '>',
                    '<': '<',
                    '>=': '>=',
                    '<=': '<=',
                  };
                  return map[op] ?? op;
                };

                return (
                  <div key={`${idx}-${c.field}`} className="grid grid-cols-1 gap-2 sm:grid-cols-12 sm:items-end">
                    <div className="sm:col-span-5">
                      <label className="block text-xs font-medium text-muted-foreground mb-1">
                        Campo
                      </label>
                      {/* Combobox simples: permite qualquer campo, mas sugere os do catálogo */}
                      <Input
                        value={c.field}
                        onChange={(e) => {
                          const next = [...formData.conditions];
                          next[idx] = { ...next[idx], field: e.target.value };
                          onUpdateFormData({ conditions: next });
                        }}
                        placeholder="Ex: consumerAuthenticationScore"
                        list="rule-condition-fields"
                        aria-invalid={!!validationErrors[`condition_${idx}_field`]}
                        className={validationErrors[`condition_${idx}_field`] ? 'border-red-500 focus:ring-red-500' : ''}
                      />
                      {validationErrors[`condition_${idx}_field`] && (
                        <p className="mt-1 text-xs text-red-500" role="alert">
                          {validationErrors[`condition_${idx}_field`]}
                        </p>
                      )}
                      <datalist id="rule-condition-fields">
                        {fieldOptions.map((f) => (
                          <option key={f} value={f} />
                        ))}
                      </datalist>
                      {fieldDictionaryError ? (
                        <p className="mt-1 text-xs text-muted-foreground">
                          Catálogo de campos indisponível; você ainda pode digitar o nome do campo.
                        </p>
                      ) : null}
                    </div>
                    <div className="sm:col-span-3">
                      <label className="block text-xs font-medium text-muted-foreground mb-1">
                        Operador
                      </label>
                      <select
                        value={c.operator}
                        onChange={(e) => {
                          const next = [...formData.conditions];
                          next[idx] = {
                            ...next[idx],
                            operator: e.target.value as RuleConfiguration['conditions'][number]['operator'],
                            // Operadores unários não usam value.
                            value: ['IS_NULL', 'IS_NOT_NULL', 'IS_TRUE', 'IS_FALSE'].includes(e.target.value)
                              ? ''
                              : next[idx].value,
                          };
                          onUpdateFormData({ conditions: next });
                        }}
                        className="w-full px-3 py-2 border border-input rounded-lg bg-background text-foreground"
                      >
                        {ops.map((op) => (
                          <option key={op} value={op}>
                            {operatorLabel(op)}
                          </option>
                        ))}
                      </select>
                    </div>
                    <div className="sm:col-span-3">
                      <label className="block text-xs font-medium text-muted-foreground mb-1">
                        Valor
                      </label>
                      {isUnary ? (
                        <Input value="" disabled placeholder="(não aplicável)" />
                      ) : (
                        <Input
                          value={c.value}
                          onChange={(e) => {
                            const next = [...formData.conditions];
                            next[idx] = { ...next[idx], value: e.target.value };
                            onUpdateFormData({ conditions: next });
                          }}
                          placeholder={
                            c.operator === 'IN' || c.operator === 'NOT_IN'
                              ? "Ex: [1,2,3] ou ['RU','CN']"
                              : c.operator === 'BETWEEN' || c.operator === 'NOT_BETWEEN'
                                ? 'Ex: 10,20 (ou 10..20)'
                                : 'Ex: 10'
                          }
                          aria-invalid={!!validationErrors[`condition_${idx}_value`]}
                          className={validationErrors[`condition_${idx}_value`] ? 'border-red-500 focus:ring-red-500' : ''}
                        />
                      )}
                      {validationErrors[`condition_${idx}_value`] && (
                        <p className="mt-1 text-xs text-red-500" role="alert">
                          {validationErrors[`condition_${idx}_value`]}
                        </p>
                      )}
                    </div>
                    <div className="sm:col-span-1 flex sm:justify-end">
                      <Button
                        type="button"
                        variant="ghost"
                        onClick={() => {
                          const next = formData.conditions.filter((_, i) => i !== idx);
                          onUpdateFormData({ conditions: next });
                        }}
                        title="Remover condição"
                      >
                        Remover
                      </Button>
                    </div>
                  </div>
                );
              })}

              <Button
                type="button"
                variant="outline"
                onClick={() =>
                  onUpdateFormData({
                    conditions: [
                      ...formData.conditions,
                      { field: '', operator: 'EQ', value: '' } as RuleConfiguration['conditions'][number],
                    ],
                  })
                }
              >
                Adicionar condição
              </Button>
            </div>
          </div>

          <div className="flex items-center gap-2">
            <input
              type="checkbox"
              id="enabled"
              checked={formData.enabled}
              onChange={(e) => onUpdateFormData({ enabled: e.target.checked })}
              className="w-4 h-4 rounded border-input"
            />
            <label htmlFor="enabled" className="text-sm font-medium text-foreground">
              Habilitada
            </label>
          </div>
          <div className="flex justify-end gap-2 pt-4">
            <Button variant="outline" onClick={onClose} disabled={isSaving}>
              Cancelar
            </Button>
            <Button onClick={onSave} disabled={isSaving}>
              {isSaving && <Loader2 className="mr-2 h-4 w-4 animate-spin" />}
              {editingRule ? 'Atualizar' : 'Criar'}
            </Button>
          </div>
        </div>
      </DialogContent>
    </Dialog>
  );
}
