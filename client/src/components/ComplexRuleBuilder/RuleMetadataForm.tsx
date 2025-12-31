/**
 * RuleMetadataForm - Formulário para metadados da regra
 * Informações básicas como nome, descrição, prioridade, etc.
 */

import { memo, useCallback } from 'react';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Input } from '@/components/ui/input';
import { Textarea } from '@/components/ui/textarea';
import { Label } from '@/components/ui/label';
import { Slider } from '@/components/ui/slider';
import { Switch } from '@/components/ui/switch';
import { Badge } from '@/components/ui/badge';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select';
import { Settings, Tag, X } from 'lucide-react';
import {
  ComplexRule,
  RuleStatus,
  DecisionType,
  RULE_STATUSES,
  DECISION_TYPES,
} from './types';

interface RuleMetadataFormProps {
  rule: ComplexRule;
  onChange: (rule: ComplexRule) => void;
  errors?: Record<string, string>;
}

export const RuleMetadataForm = memo(function RuleMetadataForm({
  rule,
  onChange,
  errors = {},
}: RuleMetadataFormProps) {
  const handleChange = useCallback(<K extends keyof ComplexRule>(
    field: K,
    value: ComplexRule[K]
  ) => {
    onChange({ ...rule, [field]: value });
  }, [rule, onChange]);

  const handleAddTag = useCallback((tag: string) => {
    if (tag && !rule.tags?.includes(tag)) {
      handleChange('tags', [...(rule.tags || []), tag]);
    }
  }, [rule.tags, handleChange]);

  const handleRemoveTag = useCallback((tag: string) => {
    handleChange('tags', (rule.tags || []).filter(t => t !== tag));
  }, [rule.tags, handleChange]);

  return (
    <Card>
      <CardHeader className="pb-3">
        <CardTitle className="text-lg flex items-center gap-2">
          <Settings className="h-5 w-5" />
          Informações da Regra
        </CardTitle>
      </CardHeader>
      <CardContent className="space-y-4">
        {/* Key */}
        <div className="space-y-2">
          <Label htmlFor="rule-key">
            Chave da Regra <span className="text-red-500">*</span>
          </Label>
          <Input
            id="rule-key"
            value={rule.key}
            onChange={(e) => handleChange('key', e.target.value.toUpperCase().replace(/[^A-Z0-9_]/g, '_'))}
            placeholder="EX: HIGH_AMOUNT_FOREIGN_COUNTRY"
            className={errors.key ? 'border-red-500' : ''}
          />
          {errors.key && (
            <p className="text-xs text-red-500">{errors.key}</p>
          )}
          <p className="text-xs text-muted-foreground">
            Identificador único. Use UPPER_SNAKE_CASE.
          </p>
        </div>

        {/* Title */}
        <div className="space-y-2">
          <Label htmlFor="rule-title">
            Título <span className="text-red-500">*</span>
          </Label>
          <Input
            id="rule-title"
            value={rule.title}
            onChange={(e) => handleChange('title', e.target.value)}
            placeholder="Ex: Transação de alto valor em país estrangeiro"
            className={errors.title ? 'border-red-500' : ''}
          />
          {errors.title && (
            <p className="text-xs text-red-500">{errors.title}</p>
          )}
        </div>

        {/* Description */}
        <div className="space-y-2">
          <Label htmlFor="rule-description">Descrição</Label>
          <Textarea
            id="rule-description"
            value={rule.description || ''}
            onChange={(e) => handleChange('description', e.target.value)}
            placeholder="Descreva o objetivo desta regra..."
            rows={3}
          />
        </div>

        {/* Status & Decision */}
        <div className="grid grid-cols-2 gap-4">
          <div className="space-y-2">
            <Label>Status</Label>
            <Select
              value={rule.status}
              onValueChange={(v) => handleChange('status', v as RuleStatus)}
            >
              <SelectTrigger>
                <SelectValue />
              </SelectTrigger>
              <SelectContent>
                {RULE_STATUSES.map((status) => (
                  <SelectItem key={status.value} value={status.value}>
                    <div className="flex items-center gap-2">
                      <Badge className={`${status.color} text-white text-xs`}>
                        {status.label}
                      </Badge>
                    </div>
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>

          <div className="space-y-2">
            <Label>Decisão</Label>
            <Select
              value={rule.decision}
              onValueChange={(v) => handleChange('decision', v as DecisionType)}
            >
              <SelectTrigger>
                <SelectValue />
              </SelectTrigger>
              <SelectContent>
                {DECISION_TYPES.map((decision) => (
                  <SelectItem key={decision.value} value={decision.value}>
                    <div className="flex items-center gap-2">
                      <Badge className={`${decision.color} text-white text-xs`}>
                        {decision.label}
                      </Badge>
                    </div>
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>
        </div>

        {/* Priority */}
        <div className="space-y-2">
          <div className="flex justify-between">
            <Label>Prioridade</Label>
            <span className="text-sm text-muted-foreground">{rule.priority}</span>
          </div>
          <Slider
            value={[rule.priority]}
            onValueChange={([v]) => handleChange('priority', v)}
            min={0}
            max={1000}
            step={10}
            className="w-full"
          />
          <div className="flex justify-between text-xs text-muted-foreground">
            <span>Baixa (0)</span>
            <span>Alta (1000)</span>
          </div>
        </div>

        {/* Severity */}
        <div className="space-y-2">
          <div className="flex justify-between">
            <Label>Severidade</Label>
            <span className="text-sm text-muted-foreground">{rule.severity}</span>
          </div>
          <Slider
            value={[rule.severity]}
            onValueChange={([v]) => handleChange('severity', v)}
            min={0}
            max={100}
            step={5}
            className="w-full"
          />
          <div className="flex justify-between text-xs text-muted-foreground">
            <span>Baixa (0)</span>
            <span>Crítica (100)</span>
          </div>
        </div>

        {/* Reason Template */}
        <div className="space-y-2">
          <Label htmlFor="rule-reason">Template de Motivo</Label>
          <Input
            id="rule-reason"
            value={rule.reasonTemplate || ''}
            onChange={(e) => handleChange('reasonTemplate', e.target.value)}
            placeholder="Ex: Transação de {amount} em {country} detectada"
          />
          <p className="text-xs text-muted-foreground">
            Use {'{campo}'} para variáveis dinâmicas.
          </p>
        </div>

        {/* Enabled */}
        <div className="flex items-center justify-between">
          <div className="space-y-0.5">
            <Label>Regra Ativa</Label>
            <p className="text-xs text-muted-foreground">
              Regras desativadas não são executadas
            </p>
          </div>
          <Switch
            checked={rule.enabled}
            onCheckedChange={(v) => handleChange('enabled', v)}
          />
        </div>

        {/* Tags */}
        <div className="space-y-2">
          <Label className="flex items-center gap-2">
            <Tag className="h-4 w-4" />
            Tags
          </Label>
          <div className="flex flex-wrap gap-2">
            {(rule.tags || []).map((tag) => (
              <Badge key={tag} variant="secondary" className="flex items-center gap-1">
                {tag}
                <button
                  type="button"
                  onClick={() => handleRemoveTag(tag)}
                  className="ml-1 hover:text-destructive"
                >
                  <X className="h-3 w-3" />
                </button>
              </Badge>
            ))}
            <Input
              placeholder="Nova tag..."
              className="w-[120px] h-6 text-xs"
              onKeyDown={(e) => {
                if (e.key === 'Enter') {
                  e.preventDefault();
                  handleAddTag((e.target as HTMLInputElement).value.trim());
                  (e.target as HTMLInputElement).value = '';
                }
              }}
            />
          </div>
        </div>
      </CardContent>
    </Card>
  );
});
