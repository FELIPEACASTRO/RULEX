import React, { useState } from 'react';
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
import { Input } from '@/components/ui/input';
import { Badge } from '@/components/ui/badge';
import { AlertTriangle, Trash2, ToggleRight } from 'lucide-react';
import { Button } from '@/components/ui/button';

interface RuleInfo {
  id: number;
  ruleName: string;
  description?: string;
  enabled: boolean;
  classification: string;
  weight: number;
  // Métricas opcionais
  triggerCount?: number;
  amountBlocked?: number;
  lastTriggered?: string;
  createdAt?: string;
}

interface DeleteRuleDialogProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
  rule: RuleInfo | null;
  onConfirmDelete: () => void;
  onDeactivateInstead?: () => void;
  isLoading?: boolean;
}

/**
 * Dialog de confirmação de exclusão de regra com informações de impacto.
 * Implementa UX segura com confirmação por digitação do nome.
 */
export function DeleteRuleDialog({
  open,
  onOpenChange,
  rule,
  onConfirmDelete,
  onDeactivateInstead,
  isLoading = false,
}: DeleteRuleDialogProps) {
  const [confirmText, setConfirmText] = useState('');
  const [understood, setUnderstood] = useState(false);

  const isConfirmValid = confirmText === rule?.ruleName;

  const handleClose = () => {
    setConfirmText('');
    setUnderstood(false);
    onOpenChange(false);
  };

  const handleDelete = () => {
    if (isConfirmValid && understood) {
      onConfirmDelete();
      handleClose();
    }
  };

  const handleDeactivate = () => {
    if (onDeactivateInstead) {
      onDeactivateInstead();
      handleClose();
    }
  };

  if (!rule) return null;

  const formatCurrency = (value?: number) => {
    if (value === undefined || value === null) return 'N/A';
    return new Intl.NumberFormat('pt-BR', {
      style: 'currency',
      currency: 'BRL',
    }).format(value);
  };

  const formatDate = (dateStr?: string) => {
    if (!dateStr) return 'N/A';
    return new Date(dateStr).toLocaleDateString('pt-BR', {
      day: '2-digit',
      month: '2-digit',
      year: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
    });
  };

  return (
    <AlertDialog open={open} onOpenChange={handleClose}>
      <AlertDialogContent className="max-w-lg">
        <AlertDialogHeader>
          <AlertDialogTitle className="flex items-center gap-2 text-red-600">
            <AlertTriangle className="h-5 w-5" />
            Excluir Regra Permanentemente?
          </AlertDialogTitle>
          <AlertDialogDescription asChild>
            <div className="space-y-4">
              {/* Informações da Regra */}
              <div className="rounded-lg border border-border bg-muted/50 p-4">
                <div className="flex items-start justify-between">
                  <div>
                    <p className="font-semibold text-foreground">{rule.ruleName}</p>
                    {rule.description && (
                      <p className="text-sm text-muted-foreground mt-1">{rule.description}</p>
                    )}
                  </div>
                  <Badge
                    variant={rule.enabled ? 'default' : 'secondary'}
                    className={rule.enabled ? 'bg-green-100 text-green-800' : ''}
                  >
                    {rule.enabled ? 'Ativa' : 'Inativa'}
                  </Badge>
                </div>

                <div className="mt-3 grid grid-cols-2 gap-2 text-sm">
                  <div>
                    <span className="text-muted-foreground">Classificação:</span>{' '}
                    <Badge
                      variant="outline"
                      className={
                        rule.classification === 'FRAUD'
                          ? 'border-red-200 text-red-700'
                          : rule.classification === 'SUSPICIOUS'
                          ? 'border-amber-200 text-amber-700'
                          : 'border-green-200 text-green-700'
                      }
                    >
                      {rule.classification}
                    </Badge>
                  </div>
                  <div>
                    <span className="text-muted-foreground">Peso:</span>{' '}
                    <span className="font-medium">{rule.weight}%</span>
                  </div>
                </div>
              </div>

              {/* Métricas de Impacto */}
              {(rule.triggerCount !== undefined || rule.amountBlocked !== undefined) && (
                <div className="rounded-lg border border-amber-200 bg-amber-50 p-4">
                  <p className="font-medium text-amber-800 mb-2">⚠️ Impacto da Exclusão</p>
                  <div className="grid grid-cols-2 gap-2 text-sm text-amber-700">
                    {rule.triggerCount !== undefined && (
                      <div>
                        <span className="text-amber-600">Disparos (30 dias):</span>{' '}
                        <span className="font-semibold">{rule.triggerCount.toLocaleString('pt-BR')}</span>
                      </div>
                    )}
                    {rule.amountBlocked !== undefined && (
                      <div>
                        <span className="text-amber-600">Valor bloqueado:</span>{' '}
                        <span className="font-semibold">{formatCurrency(rule.amountBlocked)}</span>
                      </div>
                    )}
                    {rule.lastTriggered && (
                      <div className="col-span-2">
                        <span className="text-amber-600">Último disparo:</span>{' '}
                        <span className="font-semibold">{formatDate(rule.lastTriggered)}</span>
                      </div>
                    )}
                  </div>
                </div>
              )}

              {/* Aviso */}
              <div className="rounded-lg border border-red-200 bg-red-50 p-4">
                <p className="text-sm text-red-700">
                  <strong>Esta ação é irreversível.</strong> A regra será permanentemente removida
                  do sistema e não poderá ser recuperada. Todo o histórico de configurações será
                  mantido para auditoria.
                </p>
              </div>

              {/* Checkbox de Confirmação */}
              <div className="flex items-start gap-2">
                <input
                  type="checkbox"
                  id="understood"
                  checked={understood}
                  onChange={(e) => setUnderstood(e.target.checked)}
                  className="mt-1 h-4 w-4 rounded border-gray-300"
                />
                <label htmlFor="understood" className="text-sm text-muted-foreground">
                  Entendo que esta ação é irreversível e que a regra será permanentemente excluída.
                </label>
              </div>

              {/* Campo de Confirmação */}
              <div>
                <label className="block text-sm font-medium text-foreground mb-2">
                  Digite <span className="font-mono bg-muted px-1 rounded">{rule.ruleName}</span> para confirmar:
                </label>
                <Input
                  value={confirmText}
                  onChange={(e) => setConfirmText(e.target.value)}
                  placeholder="Digite o nome da regra"
                  className={
                    confirmText && !isConfirmValid
                      ? 'border-red-300 focus:border-red-500'
                      : isConfirmValid
                      ? 'border-green-300 focus:border-green-500'
                      : ''
                  }
                />
                {confirmText && !isConfirmValid && (
                  <p className="text-xs text-red-500 mt-1">O nome não confere</p>
                )}
              </div>
            </div>
          </AlertDialogDescription>
        </AlertDialogHeader>

        <AlertDialogFooter className="flex-col sm:flex-row gap-2">
          <AlertDialogCancel onClick={handleClose} disabled={isLoading}>
            Cancelar
          </AlertDialogCancel>

          {rule.enabled && onDeactivateInstead && (
            <Button
              variant="outline"
              onClick={handleDeactivate}
              disabled={isLoading}
              className="border-amber-300 text-amber-700 hover:bg-amber-50"
            >
              <ToggleRight className="h-4 w-4 mr-2" />
              Desativar ao Invés
            </Button>
          )}

          <AlertDialogAction
            onClick={handleDelete}
            disabled={!isConfirmValid || !understood || isLoading}
            className="bg-red-600 hover:bg-red-700 focus:ring-red-600"
          >
            <Trash2 className="h-4 w-4 mr-2" />
            {isLoading ? 'Excluindo...' : 'Excluir Permanentemente'}
          </AlertDialogAction>
        </AlertDialogFooter>
      </AlertDialogContent>
    </AlertDialog>
  );
}

export default DeleteRuleDialog;
