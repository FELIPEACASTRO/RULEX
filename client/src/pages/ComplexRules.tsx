/**
 * ComplexRules - Página para gerenciamento de regras complexas
 * 
 * Features:
 * - Listagem de regras complexas
 * - Criação/edição com ComplexRuleBuilder
 * - Filtros e busca
 * - Ações em lote
 */

import { useState, useMemo, useCallback } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Badge } from '@/components/ui/badge';
import { Separator } from '@/components/ui/separator';
import {
  Dialog,
  DialogContent,
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
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from '@/components/ui/dropdown-menu';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select';
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from '@/components/ui/table';
import {
  Plus,
  Search,
  Filter,
  MoreHorizontal,
  Edit2,
  Trash2,
  Copy,
  ToggleLeft,
  ToggleRight,
  Layers,
  AlertTriangle,
  Loader2,
  RefreshCw,
  Eye,
} from 'lucide-react';
import { toast } from 'sonner';

import { ComplexRuleBuilder } from '@/components/ComplexRuleBuilder';
import type { ComplexRule } from '@/components/ComplexRuleBuilder';
import {
  listComplexRules,
  createComplexRule,
  updateComplexRule,
  deleteComplexRule,
  toggleComplexRuleStatus,
  duplicateComplexRule,
  listFieldDictionary,
  ComplexRuleDTO,
} from '@/lib/javaApi';

type ViewMode = 'list' | 'create' | 'edit' | 'view';

export default function ComplexRules() {
  const queryClient = useQueryClient();
  
  // State
  const [viewMode, setViewMode] = useState<ViewMode>('list');
  const [selectedRule, setSelectedRule] = useState<ComplexRuleDTO | null>(null);
  const [searchTerm, setSearchTerm] = useState('');
  const [statusFilter, setStatusFilter] = useState<string>('all');
  const [decisionFilter, setDecisionFilter] = useState<string>('all');
  const [deleteConfirmId, setDeleteConfirmId] = useState<string | null>(null);
  const [duplicateDialogRule, setDuplicateDialogRule] = useState<ComplexRuleDTO | null>(null);
  const [duplicateNewKey, setDuplicateNewKey] = useState('');

  // Queries
  const rulesQuery = useQuery({
    queryKey: ['complex-rules'],
    queryFn: listComplexRules,
    retry: 1,
  });

  const fieldDictionaryQuery = useQuery({
    queryKey: ['fieldDictionary'],
    queryFn: () => listFieldDictionary({ workflow: 'BRZLCREDIT', recordType: 'CRTRAN25', portfolio: '*' }),
    retry: 1,
  });

  const fieldOptions = useMemo(() => {
    return (fieldDictionaryQuery.data ?? [])
      .map((f) => (f.jsonPath?.startsWith('$.') ? f.jsonPath.slice(2) : f.jsonPath))
      .filter(Boolean) as string[];
  }, [fieldDictionaryQuery.data]);

  // Mutations
  const createMutation = useMutation({
    mutationFn: createComplexRule,
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['complex-rules'] });
      setViewMode('list');
      toast.success('Regra criada com sucesso!');
    },
    onError: (error: Error) => {
      toast.error(`Erro ao criar regra: ${error.message}`);
    },
  });

  const updateMutation = useMutation({
    mutationFn: ({ id, rule }: { id: string; rule: Partial<ComplexRuleDTO> }) =>
      updateComplexRule(id, rule),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['complex-rules'] });
      setViewMode('list');
      setSelectedRule(null);
      toast.success('Regra atualizada com sucesso!');
    },
    onError: (error: Error) => {
      toast.error(`Erro ao atualizar regra: ${error.message}`);
    },
  });

  const deleteMutation = useMutation({
    mutationFn: deleteComplexRule,
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['complex-rules'] });
      toast.success('Regra deletada com sucesso!');
    },
    onError: (error: Error) => {
      toast.error(`Erro ao deletar regra: ${error.message}`);
    },
  });

  const toggleMutation = useMutation({
    mutationFn: ({ id, enabled }: { id: string; enabled: boolean }) =>
      toggleComplexRuleStatus(id, enabled),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['complex-rules'] });
    },
    onError: (error: Error) => {
      toast.error(`Erro ao alternar status: ${error.message}`);
    },
  });

  const duplicateMutation = useMutation({
    mutationFn: ({ id, newKey }: { id: string; newKey: string }) =>
      duplicateComplexRule(id, newKey),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['complex-rules'] });
      setDuplicateDialogRule(null);
      setDuplicateNewKey('');
      toast.success('Regra duplicada com sucesso!');
    },
    onError: (error: Error) => {
      toast.error(`Erro ao duplicar regra: ${error.message}`);
    },
  });

  // Filtered rules
  const filteredRules = useMemo(() => {
    let rules = rulesQuery.data ?? [];

    if (searchTerm) {
      const term = searchTerm.toLowerCase();
      rules = rules.filter(
        (r) =>
          r.key.toLowerCase().includes(term) ||
          r.title.toLowerCase().includes(term) ||
          r.description?.toLowerCase().includes(term)
      );
    }

    if (statusFilter !== 'all') {
      rules = rules.filter((r) => r.status === statusFilter);
    }

    if (decisionFilter !== 'all') {
      rules = rules.filter((r) => r.decision === decisionFilter);
    }

    return rules;
  }, [rulesQuery.data, searchTerm, statusFilter, decisionFilter]);

  // Handlers
  const handleCreate = useCallback(() => {
    setSelectedRule(null);
    setViewMode('create');
  }, []);

  const handleEdit = useCallback((rule: ComplexRuleDTO) => {
    setSelectedRule(rule);
    setViewMode('edit');
  }, []);

  const handleView = useCallback((rule: ComplexRuleDTO) => {
    setSelectedRule(rule);
    setViewMode('view');
  }, []);

  const handleSave = useCallback(async (rule: ComplexRule) => {
    if (viewMode === 'edit' && selectedRule?.id) {
      await updateMutation.mutateAsync({ id: selectedRule.id, rule: rule as ComplexRuleDTO });
    } else {
      await createMutation.mutateAsync(rule as ComplexRuleDTO);
    }
  }, [viewMode, selectedRule, updateMutation, createMutation]);

  const handleCancel = useCallback(() => {
    setViewMode('list');
    setSelectedRule(null);
  }, []);

  const handleDelete = useCallback((id: string) => {
    setDeleteConfirmId(id);
  }, []);

  const confirmDelete = useCallback(() => {
    if (deleteConfirmId) {
      deleteMutation.mutate(deleteConfirmId);
      setDeleteConfirmId(null);
    }
  }, [deleteConfirmId, deleteMutation]);

  const handleToggle = useCallback((id: string, currentEnabled: boolean) => {
    toggleMutation.mutate({ id, enabled: !currentEnabled });
  }, [toggleMutation]);

  const handleDuplicate = useCallback((rule: ComplexRuleDTO) => {
    setDuplicateDialogRule(rule);
    setDuplicateNewKey(`${rule.key}_COPY`);
  }, []);

  const confirmDuplicate = useCallback(() => {
    if (duplicateDialogRule?.id && duplicateNewKey) {
      duplicateMutation.mutate({ id: duplicateDialogRule.id, newKey: duplicateNewKey });
    }
  }, [duplicateDialogRule, duplicateNewKey, duplicateMutation]);

  // Status badge colors
  const getStatusColor = (status: string) => {
    switch (status) {
      case 'PUBLISHED':
        return 'bg-green-500';
      case 'DRAFT':
        return 'bg-gray-500';
      case 'TESTING':
        return 'bg-yellow-500';
      case 'ARCHIVED':
        return 'bg-red-500';
      default:
        return 'bg-gray-500';
    }
  };

  const getDecisionColor = (decision: string) => {
    switch (decision) {
      case 'APROVADO':
        return 'bg-green-500';
      case 'SUSPEITA_DE_FRAUDE':
        return 'bg-yellow-500';
      case 'FRAUDE':
        return 'bg-red-500';
      default:
        return 'bg-gray-500';
    }
  };

  // Render builder view
  if (viewMode === 'create' || viewMode === 'edit') {
    return (
      <div className="h-[calc(100vh-4rem)]">
        <ComplexRuleBuilder
          initialRule={selectedRule as ComplexRule | undefined}
          onSave={handleSave}
          onCancel={handleCancel}
          fieldOptions={fieldOptions}
          isLoading={createMutation.isPending || updateMutation.isPending}
        />
      </div>
    );
  }

  // Render view mode (read-only)
  if (viewMode === 'view' && selectedRule) {
    return (
      <div className="h-[calc(100vh-4rem)] p-4">
        <div className="flex items-center justify-between mb-4">
          <h1 className="text-2xl font-bold flex items-center gap-2">
            <Eye className="h-6 w-6" />
            Visualizar Regra: {selectedRule.title}
          </h1>
          <div className="flex gap-2">
            <Button variant="outline" onClick={handleCancel}>
              Voltar
            </Button>
            <Button onClick={() => handleEdit(selectedRule)}>
              <Edit2 className="h-4 w-4 mr-2" />
              Editar
            </Button>
          </div>
        </div>
        <ComplexRuleBuilder
          initialRule={selectedRule as ComplexRule}
          onSave={async () => {}}
          onCancel={handleCancel}
          fieldOptions={fieldOptions}
        />
      </div>
    );
  }

  // Render list view
  return (
    <div className="space-y-6 p-6">
      {/* Header */}
      <div className="flex justify-between items-center">
        <div>
          <h1 className="text-3xl font-bold text-foreground flex items-center gap-2">
            <Layers className="h-8 w-8" />
            Regras Complexas
          </h1>
          <p className="text-muted-foreground mt-1">
            Crie e gerencie regras avançadas com grupos aninhados e operadores lógicos
          </p>
        </div>
        <Button onClick={handleCreate} className="gap-2">
          <Plus className="h-4 w-4" />
          Nova Regra Complexa
        </Button>
      </div>

      {/* Filters */}
      <Card>
        <CardContent className="pt-6">
          <div className="flex flex-wrap gap-4">
            <div className="flex-1 min-w-[200px]">
              <div className="relative">
                <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-muted-foreground" />
                <Input
                  placeholder="Buscar por chave, título ou descrição..."
                  value={searchTerm}
                  onChange={(e) => setSearchTerm(e.target.value)}
                  className="pl-10"
                />
              </div>
            </div>

            <Select value={statusFilter} onValueChange={setStatusFilter}>
              <SelectTrigger className="w-[150px]">
                <Filter className="h-4 w-4 mr-2" />
                <SelectValue placeholder="Status" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="all">Todos Status</SelectItem>
                <SelectItem value="DRAFT">Rascunho</SelectItem>
                <SelectItem value="PUBLISHED">Publicada</SelectItem>
                <SelectItem value="TESTING">Em Teste</SelectItem>
                <SelectItem value="ARCHIVED">Arquivada</SelectItem>
              </SelectContent>
            </Select>

            <Select value={decisionFilter} onValueChange={setDecisionFilter}>
              <SelectTrigger className="w-[180px]">
                <Filter className="h-4 w-4 mr-2" />
                <SelectValue placeholder="Decisão" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="all">Todas Decisões</SelectItem>
                <SelectItem value="APROVADO">Aprovado</SelectItem>
                <SelectItem value="SUSPEITA_DE_FRAUDE">Suspeita</SelectItem>
                <SelectItem value="FRAUDE">Fraude</SelectItem>
              </SelectContent>
            </Select>

            <Button
              variant="outline"
              onClick={() => rulesQuery.refetch()}
              disabled={rulesQuery.isFetching}
            >
              <RefreshCw className={`h-4 w-4 mr-2 ${rulesQuery.isFetching ? 'animate-spin' : ''}`} />
              Atualizar
            </Button>
          </div>
        </CardContent>
      </Card>

      {/* Rules Table */}
      <Card>
        <CardHeader>
          <CardTitle>
            Regras ({filteredRules.length})
          </CardTitle>
          <CardDescription>
            Lista de todas as regras complexas configuradas
          </CardDescription>
        </CardHeader>
        <CardContent>
          {rulesQuery.isLoading ? (
            <div className="flex items-center justify-center py-12">
              <Loader2 className="h-8 w-8 animate-spin text-muted-foreground" />
            </div>
          ) : rulesQuery.isError ? (
            <div className="flex flex-col items-center justify-center py-12 text-center">
              <AlertTriangle className="h-12 w-12 text-amber-500 mb-4" />
              <p className="text-lg font-medium">Erro ao carregar regras</p>
              <p className="text-sm text-muted-foreground mb-4">
                {rulesQuery.error instanceof Error ? rulesQuery.error.message : 'Erro desconhecido'}
              </p>
              <Button variant="outline" onClick={() => rulesQuery.refetch()}>
                Tentar novamente
              </Button>
            </div>
          ) : filteredRules.length === 0 ? (
            <div className="flex flex-col items-center justify-center py-12 text-center">
              <Layers className="h-12 w-12 text-muted-foreground mb-4" />
              <p className="text-lg font-medium">Nenhuma regra encontrada</p>
              <p className="text-sm text-muted-foreground mb-4">
                {searchTerm || statusFilter !== 'all' || decisionFilter !== 'all'
                  ? 'Tente ajustar os filtros'
                  : 'Crie sua primeira regra complexa'}
              </p>
              {!searchTerm && statusFilter === 'all' && decisionFilter === 'all' && (
                <Button onClick={handleCreate}>
                  <Plus className="h-4 w-4 mr-2" />
                  Criar Regra
                </Button>
              )}
            </div>
          ) : (
            <Table>
              <TableHeader>
                <TableRow>
                  <TableHead>Chave</TableHead>
                  <TableHead>Título</TableHead>
                  <TableHead>Status</TableHead>
                  <TableHead>Decisão</TableHead>
                  <TableHead className="text-center">Prioridade</TableHead>
                  <TableHead className="text-center">Severidade</TableHead>
                  <TableHead className="text-center">Ativa</TableHead>
                  <TableHead className="text-right">Ações</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {filteredRules.map((rule) => (
                  <TableRow key={rule.id}>
                    <TableCell className="font-mono text-sm">
                      {rule.key}
                    </TableCell>
                    <TableCell>
                      <div>
                        <p className="font-medium">{rule.title}</p>
                        {rule.description && (
                          <p className="text-xs text-muted-foreground truncate max-w-[300px]">
                            {rule.description}
                          </p>
                        )}
                      </div>
                    </TableCell>
                    <TableCell>
                      <Badge className={`${getStatusColor(rule.status)} text-white`}>
                        {rule.status}
                      </Badge>
                    </TableCell>
                    <TableCell>
                      <Badge className={`${getDecisionColor(rule.decision)} text-white`}>
                        {rule.decision.replace('_', ' ')}
                      </Badge>
                    </TableCell>
                    <TableCell className="text-center">{rule.priority}</TableCell>
                    <TableCell className="text-center">{rule.severity}</TableCell>
                    <TableCell className="text-center">
                      <Button
                        variant="ghost"
                        size="icon"
                        onClick={() => rule.id && handleToggle(rule.id, rule.enabled)}
                        disabled={toggleMutation.isPending}
                      >
                        {rule.enabled ? (
                          <ToggleRight className="h-5 w-5 text-green-500" />
                        ) : (
                          <ToggleLeft className="h-5 w-5 text-muted-foreground" />
                        )}
                      </Button>
                    </TableCell>
                    <TableCell className="text-right">
                      <DropdownMenu>
                        <DropdownMenuTrigger asChild>
                          <Button variant="ghost" size="icon">
                            <MoreHorizontal className="h-4 w-4" />
                          </Button>
                        </DropdownMenuTrigger>
                        <DropdownMenuContent align="end">
                          <DropdownMenuItem onClick={() => handleView(rule)}>
                            <Eye className="h-4 w-4 mr-2" />
                            Visualizar
                          </DropdownMenuItem>
                          <DropdownMenuItem onClick={() => handleEdit(rule)}>
                            <Edit2 className="h-4 w-4 mr-2" />
                            Editar
                          </DropdownMenuItem>
                          <DropdownMenuItem onClick={() => handleDuplicate(rule)}>
                            <Copy className="h-4 w-4 mr-2" />
                            Duplicar
                          </DropdownMenuItem>
                          <DropdownMenuSeparator />
                          <DropdownMenuItem
                            onClick={() => rule.id && handleDelete(rule.id)}
                            className="text-red-600"
                          >
                            <Trash2 className="h-4 w-4 mr-2" />
                            Deletar
                          </DropdownMenuItem>
                        </DropdownMenuContent>
                      </DropdownMenu>
                    </TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          )}
        </CardContent>
      </Card>

      {/* Delete Confirmation Dialog */}
      <AlertDialog open={!!deleteConfirmId} onOpenChange={() => setDeleteConfirmId(null)}>
        <AlertDialogContent>
          <AlertDialogHeader>
            <AlertDialogTitle className="flex items-center gap-2">
              <AlertTriangle className="h-5 w-5 text-red-500" />
              Confirmar exclusão
            </AlertDialogTitle>
            <AlertDialogDescription>
              Tem certeza que deseja deletar esta regra? Esta ação não pode ser desfeita.
            </AlertDialogDescription>
          </AlertDialogHeader>
          <AlertDialogFooter>
            <AlertDialogCancel>Cancelar</AlertDialogCancel>
            <AlertDialogAction onClick={confirmDelete} className="bg-red-600 hover:bg-red-700">
              Deletar
            </AlertDialogAction>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>

      {/* Duplicate Dialog */}
      <Dialog open={!!duplicateDialogRule} onOpenChange={() => setDuplicateDialogRule(null)}>
        <DialogContent>
          <div className="space-y-4">
            <h2 className="text-lg font-semibold flex items-center gap-2">
              <Copy className="h-5 w-5" />
              Duplicar Regra
            </h2>
            <p className="text-sm text-muted-foreground">
              Duplicando: <strong>{duplicateDialogRule?.title}</strong>
            </p>
            <div className="space-y-2">
              <label className="text-sm font-medium">Nova Chave</label>
              <Input
                value={duplicateNewKey}
                onChange={(e) => setDuplicateNewKey(e.target.value.toUpperCase().replace(/[^A-Z0-9_]/g, '_'))}
                placeholder="NOVA_CHAVE_DA_REGRA"
              />
              <p className="text-xs text-muted-foreground">
                Use UPPER_SNAKE_CASE
              </p>
            </div>
            <div className="flex justify-end gap-2">
              <Button variant="outline" onClick={() => setDuplicateDialogRule(null)}>
                Cancelar
              </Button>
              <Button
                onClick={confirmDuplicate}
                disabled={!duplicateNewKey || duplicateMutation.isPending}
              >
                {duplicateMutation.isPending ? (
                  <>
                    <Loader2 className="h-4 w-4 mr-2 animate-spin" />
                    Duplicando...
                  </>
                ) : (
                  <>
                    <Copy className="h-4 w-4 mr-2" />
                    Duplicar
                  </>
                )}
              </Button>
            </div>
          </div>
        </DialogContent>
      </Dialog>
    </div>
  );
}
