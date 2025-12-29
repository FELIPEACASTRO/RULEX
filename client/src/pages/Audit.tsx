import { useEffect, useMemo, useState } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Input } from '@/components/ui/input';
import { Button } from '@/components/ui/button';
import { ChevronLeft, ChevronRight, Download } from 'lucide-react';
import { keepPreviousData, useQuery } from '@tanstack/react-query';
import { AuditLog, exportAuditLogs, listAuditLogs } from '@/lib/javaApi';
import { toast } from 'sonner';

/**
 * Página de auditoria com histórico de todas as ações.
 */
export default function Audit() {
  const [page, setPage] = useState(0);
  const [size, setSize] = useState(20);
  const [exportFormat, setExportFormat] = useState<'csv' | 'json'>('csv');
  const [startDate, setStartDate] = useState<string>(''); // datetime-local
  const [endDate, setEndDate] = useState<string>(''); // datetime-local
  const [filters, setFilters] = useState({
    actionType: '',
    result: '',
  });

  const { data, isLoading, isError, error } = useQuery({
    queryKey: ['auditLogs', { page, size, filters, startDate, endDate }],
    queryFn: () =>
      listAuditLogs({
        page,
        size,
        actionType: filters.actionType || undefined,
        result: filters.result || undefined,
        startDate: startDate ? new Date(startDate).toISOString() : undefined,
        endDate: endDate ? new Date(endDate).toISOString() : undefined,
      }),
    placeholderData: keepPreviousData,
  });

  useEffect(() => {
    if (isError) {
      toast.error('Falha ao buscar logs de auditoria');
    }
  }, [isError]);

  const handleFilterChange = (key: string, value: string) => {
    setFilters(prev => ({ ...prev, [key]: value }));
    setPage(0);
  };

  const handleClear = () => {
    setFilters({ actionType: '', result: '' });
    setStartDate('');
    setEndDate('');
    setPage(0);
  };

  const handleExport = async () => {
    try {
      const startIso = startDate ? new Date(startDate).toISOString() : undefined;
      const endIso = endDate ? new Date(endDate).toISOString() : undefined;
      const exported = await exportAuditLogs(exportFormat, {
        actionType: filters.actionType || undefined,
        result: filters.result || undefined,
        startDate: startIso,
        endDate: endIso,
        limit: 10000,
      });

      const blob =
        exportFormat === 'json'
          ? new Blob([JSON.stringify(exported, null, 2)], { type: 'application/json' })
          : (exported as Blob);

      const url = window.URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = exportFormat === 'json' ? 'rulex-audit.json' : 'rulex-audit.csv';
      document.body.appendChild(a);
      a.click();
      a.remove();
      window.URL.revokeObjectURL(url);
      toast.success('Export gerado');
    } catch (e) {
      toast.error(`Falha ao exportar: ${e instanceof Error ? e.message : 'erro inesperado'}`);
    }
  };

  const getActionTypeColor = (actionType: string) => {
    switch (actionType) {
      case 'TRANSACTION_PROCESSED':
        return 'bg-blue-100 text-blue-800';
      case 'RULE_CREATED':
        return 'bg-green-100 text-green-800';
      case 'RULE_UPDATED':
        return 'bg-amber-100 text-amber-800';
      case 'RULE_DELETED':
        return 'bg-red-100 text-red-800';
      case 'CONFIG_CHANGED':
        return 'bg-purple-100 text-purple-800';
      default:
        return 'bg-gray-100 text-gray-800';
    }
  };

  const getResultColor = (result: string) => {
    switch (result) {
      case 'SUCCESS':
        return 'bg-green-100 text-green-800';
      case 'FAILURE':
        return 'bg-red-100 text-red-800';
      default:
        return 'bg-gray-100 text-gray-800';
    }
  };

  const getActionTypeLabel = (actionType: string) => {
    switch (actionType) {
      case 'TRANSACTION_PROCESSED':
        return 'Transação Processada';
      case 'RULE_CREATED':
        return 'Regra Criada';
      case 'RULE_UPDATED':
        return 'Regra Atualizada';
      case 'RULE_DELETED':
        return 'Regra Deletada';
      case 'CONFIG_CHANGED':
        return 'Configuração Alterada';
      default:
        return actionType;
    }
  };

  const logs: AuditLog[] = useMemo(() => data?.content ?? [], [data]);
  const totalElements = data?.totalElements ?? logs.length;
  const totalPages = Math.ceil(totalElements / size);

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex items-start justify-between gap-4">
        <div>
          <h1 className="text-3xl font-bold text-foreground">Auditoria</h1>
          <p className="text-muted-foreground mt-1">Histórico de todas as ações do sistema</p>
        </div>
        <div className="flex items-center gap-2">
          <select
            value={exportFormat}
            onChange={(e) => setExportFormat(e.target.value as 'csv' | 'json')}
            className="px-3 py-2 border border-input rounded-lg bg-background text-foreground"
            aria-label="Formato de export"
          >
            <option value="csv">CSV</option>
            <option value="json">JSON</option>
          </select>
          <Button variant="outline" onClick={handleExport} className="flex items-center gap-2">
            <Download className="h-4 w-4" />
            Exportar
          </Button>
        </div>
      </div>

      {/* Filtros */}
      <Card>
        <CardHeader>
          <CardTitle className="text-lg">Filtros</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
            <div>
              <label className="block text-sm font-medium text-foreground mb-2">Tipo de Ação</label>
              <select
                value={filters.actionType}
                onChange={(e) => handleFilterChange('actionType', e.target.value)}
                className="w-full px-3 py-2 border border-input rounded-lg bg-background text-foreground"
              >
                <option value="">Todos os tipos</option>
                <option value="TRANSACTION_PROCESSED">Transação Processada</option>
                <option value="RULE_CREATED">Regra Criada</option>
                <option value="RULE_UPDATED">Regra Atualizada</option>
                <option value="RULE_DELETED">Regra Deletada</option>
                <option value="CONFIG_CHANGED">Configuração Alterada</option>
              </select>
            </div>
            <div>
              <label className="block text-sm font-medium text-foreground mb-2">Resultado</label>
              <select
                value={filters.result}
                onChange={(e) => handleFilterChange('result', e.target.value)}
                className="w-full px-3 py-2 border border-input rounded-lg bg-background text-foreground"
              >
                <option value="">Todos os resultados</option>
                <option value="SUCCESS">Sucesso</option>
                <option value="FAILURE">Falha</option>
              </select>
            </div>
            <div>
              <label className="block text-sm font-medium text-foreground mb-2">Início</label>
              <input
                type="datetime-local"
                value={startDate}
                onChange={(e) => {
                  setStartDate(e.target.value);
                  setPage(0);
                }}
                className="w-full px-3 py-2 border border-input rounded-lg bg-background text-foreground"
              />
            </div>
            <div>
              <label className="block text-sm font-medium text-foreground mb-2">Fim</label>
              <input
                type="datetime-local"
                value={endDate}
                onChange={(e) => {
                  setEndDate(e.target.value);
                  setPage(0);
                }}
                className="w-full px-3 py-2 border border-input rounded-lg bg-background text-foreground"
              />
            </div>
          </div>
          <div className="mt-4 flex justify-end">
            <Button variant="outline" onClick={handleClear}>
              Limpar filtros
            </Button>
          </div>
        </CardContent>
      </Card>

      {/* Tabela de Logs */}
      <Card>
        <CardHeader>
          <CardTitle>Logs de Auditoria</CardTitle>
          <CardDescription>Total: {totalElements.toLocaleString()} registros</CardDescription>
        </CardHeader>
        <CardContent>
          {isError && (
            <div className="mb-4 rounded-lg border border-red-200 bg-red-50 p-3 text-red-800" role="alert">
              Erro ao carregar logs: {error instanceof Error ? error.message : 'erro inesperado'}
            </div>
          )}
          {isLoading ? (
            <div className="flex items-center justify-center h-64">
              <div className="text-center">
                <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-primary mx-auto mb-2"></div>
                <p className="text-muted-foreground">Carregando logs...</p>
              </div>
            </div>
          ) : logs.length === 0 ? (
            <div className="flex items-center justify-center h-64">
              <p className="text-muted-foreground">Nenhum log encontrado</p>
            </div>
          ) : (
            <div className="overflow-x-auto">
              <table className="w-full">
                <thead className="border-b border-border">
                  <tr>
                    <th className="text-left py-3 px-4 font-semibold text-sm text-foreground">Tipo de Ação</th>
                    <th className="text-left py-3 px-4 font-semibold text-sm text-foreground">Descrição</th>
                    <th className="text-left py-3 px-4 font-semibold text-sm text-foreground">Realizado Por</th>
                    <th className="text-center py-3 px-4 font-semibold text-sm text-foreground">Resultado</th>
                    <th className="text-left py-3 px-4 font-semibold text-sm text-foreground">Data/Hora</th>
                  </tr>
                </thead>
                <tbody>
                  {logs.map((log) => (
                    <tr key={log.id} className="border-b border-border hover:bg-muted/50 transition-colors">
                      <td className="py-3 px-4 text-sm">
                        <Badge className={getActionTypeColor(log.actionType)}>
                          {getActionTypeLabel(log.actionType)}
                        </Badge>
                      </td>
                      <td className="py-3 px-4 text-sm text-foreground">{log.description}</td>
                      <td className="py-3 px-4 text-sm text-muted-foreground">{log.performedBy || '-'}</td>
                      <td className="py-3 px-4 text-sm text-center">
                        <Badge className={getResultColor(log.result)}>
                          {log.result === 'SUCCESS' ? 'Sucesso' : 'Falha'}
                        </Badge>
                      </td>
                      <td className="py-3 px-4 text-sm text-muted-foreground">
                        {new Date(log.createdAt).toLocaleString('pt-BR')}
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}

          {/* Paginação */}
          {totalPages > 1 && (
            <div className="flex items-center justify-between mt-6 pt-4 border-t border-border">
              <div className="text-sm text-muted-foreground">
                Página {page + 1} de {totalPages}
              </div>
              <div className="flex gap-2">
                <Button
                  variant="outline"
                  size="sm"
                  onClick={() => setPage(Math.max(0, page - 1))}
                  disabled={page === 0}
                >
                  <ChevronLeft className="h-4 w-4 mr-1" />
                  Anterior
                </Button>
                <Button
                  variant="outline"
                  size="sm"
                  onClick={() => setPage(Math.min(totalPages - 1, page + 1))}
                  disabled={page >= totalPages - 1}
                >
                  Próxima
                  <ChevronRight className="h-4 w-4 ml-1" />
                </Button>
              </div>
            </div>
          )}
        </CardContent>
      </Card>
    </div>
  );
}
