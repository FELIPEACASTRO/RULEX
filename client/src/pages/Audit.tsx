import { useEffect, useMemo, useState } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Input } from '@/components/ui/input';
import { Button } from '@/components/ui/button';
import { ChevronLeft, ChevronRight } from 'lucide-react';
import { useQuery } from '@tanstack/react-query';
import { listAuditExecutionsV31, PaginatedResponse, RuleExecutionLog } from '@/lib/javaApi';
import { toast } from 'sonner';

/**
 * Página de auditoria com histórico de todas as ações.
 */
export default function Audit() {
  const [page, setPage] = useState(0);
  const [size, setSize] = useState(20);
  const [filters, setFilters] = useState({
    externalTransactionId: '',
  });

  const { data, isLoading, isError, error } = useQuery<PaginatedResponse<RuleExecutionLog>>({
    queryKey: ['auditLogs', { page, size, filters }],
    queryFn: () =>
      listAuditExecutionsV31({
        page,
        size,
        externalTransactionId: filters.externalTransactionId || undefined,
      }),
    placeholderData: (prev) => prev,
    retry: 1,
  });

  useEffect(() => {
    if (isError) {
      toast.error('Falha ao buscar execuções (v3.1)');
    }
  }, [isError]);

  const logs = useMemo(() => data?.content ?? [], [data]);
  const totalElements = data?.totalElements ?? 0;

  const handleFilterChange = (key: string, value: string) => {
    setFilters(prev => ({ ...prev, [key]: value }));
    setPage(0);
  };

  const getDecisionColor = (decision: string) => {
    switch ((decision || '').toUpperCase()) {
      case 'FRAUDE':
      case 'FRAUD':
        return 'bg-red-100 text-red-800';
      case 'SUSPEITA_DE_FRAUDE':
      case 'SUSPICIOUS':
        return 'bg-amber-100 text-amber-800';
      case 'APROVADA':
      case 'APPROVED':
        return 'bg-green-100 text-green-800';
      default:
        return 'bg-gray-100 text-gray-800';
    }
  };

  const totalPages = Math.ceil(totalElements / size);

  return (
    <div className="space-y-6">
      {/* Header */}
      <div>
        <h1 className="text-3xl font-bold text-foreground">Auditoria</h1>
        <p className="text-muted-foreground mt-1">Execuções do motor v3.1 (append-only)</p>
      </div>

      {/* Filtros */}
      <Card>
        <CardHeader>
          <CardTitle className="text-lg">Filtros</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <div>
              <label className="block text-sm font-medium text-foreground mb-2">External Transaction ID</label>
              <Input
                value={filters.externalTransactionId}
                onChange={(e) => handleFilterChange('externalTransactionId', e.target.value)}
                placeholder="Ex: TXN-2024-001"
              />
            </div>
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
                    <th className="text-left py-3 px-4 font-semibold text-sm text-foreground">Decisão</th>
                    <th className="text-left py-3 px-4 font-semibold text-sm text-foreground">Evento</th>
                    <th className="text-left py-3 px-4 font-semibold text-sm text-foreground">External Tx</th>
                    <th className="text-center py-3 px-4 font-semibold text-sm text-foreground">Risk Score</th>
                    <th className="text-left py-3 px-4 font-semibold text-sm text-foreground">Data/Hora</th>
                  </tr>
                </thead>
                <tbody>
                  {logs.map((log) => (
                    <tr key={log.id} className="border-b border-border hover:bg-muted/50 transition-colors">
                      <td className="py-3 px-4 text-sm">
                        <Badge className={getDecisionColor(log.decision)}>{log.decision}</Badge>
                      </td>
                      <td className="py-3 px-4 text-sm text-muted-foreground">{log.eventType}</td>
                      <td className="py-3 px-4 text-sm text-foreground">{log.externalTransactionId || '-'}</td>
                      <td className="py-3 px-4 text-sm text-center text-foreground">{log.riskScore}</td>
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
