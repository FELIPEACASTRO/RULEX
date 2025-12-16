import { useEffect, useState } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Badge } from '@/components/ui/badge';
import { Search, ChevronLeft, ChevronRight, Eye } from 'lucide-react';

interface Transaction {
  id: number;
  externalTransactionId: string;
  customerIdFromHeader: string;
  merchantId: string;
  transactionAmount: number;
  classification: string;
  riskScore: number;
  timestamp: string;
}

/**
 * Página de listagem de transações com filtros e paginação.
 */
export default function Transactions() {
  const [transactions, setTransactions] = useState<Transaction[]>([]);
  const [loading, setLoading] = useState(true);
  const [page, setPage] = useState(0);
  const [size, setSize] = useState(20);
  const [totalElements, setTotalElements] = useState(0);
  const [filters, setFilters] = useState({
    customerId: '',
    merchantId: '',
    minAmount: '',
    maxAmount: '',
  });

  useEffect(() => {
    fetchTransactions();
  }, [page, size, filters]);

  const fetchTransactions = async () => {
    setLoading(true);
    try {
      const params = new URLSearchParams({
        page: page.toString(),
        size: size.toString(),
      });

      if (filters.customerId) params.append('customerId', filters.customerId);
      if (filters.merchantId) params.append('merchantId', filters.merchantId);
      if (filters.minAmount) params.append('minAmount', filters.minAmount);
      if (filters.maxAmount) params.append('maxAmount', filters.maxAmount);

      const response = await fetch(`/api/transactions?${params}`);
      const data = await response.json();
      setTransactions(data.content || []);
      setTotalElements(data.totalElements || 0);
    } catch (error) {
      console.error('Erro ao buscar transações:', error);
    } finally {
      setLoading(false);
    }
  };

  const handleFilterChange = (key: string, value: string) => {
    setFilters(prev => ({ ...prev, [key]: value }));
    setPage(0);
  };

  const getClassificationColor = (classification: string) => {
    switch (classification) {
      case 'APPROVED':
        return 'bg-green-100 text-green-800';
      case 'SUSPICIOUS':
        return 'bg-amber-100 text-amber-800';
      case 'FRAUD':
        return 'bg-red-100 text-red-800';
      default:
        return 'bg-gray-100 text-gray-800';
    }
  };

  const getClassificationLabel = (classification: string) => {
    switch (classification) {
      case 'APPROVED':
        return 'Aprovada';
      case 'SUSPICIOUS':
        return 'Suspeita';
      case 'FRAUD':
        return 'Fraude';
      default:
        return classification;
    }
  };

  const totalPages = Math.ceil(totalElements / size);

  return (
    <div className="space-y-6">
      {/* Header */}
      <div>
        <h1 className="text-3xl font-bold text-foreground">Transações</h1>
        <p className="text-muted-foreground mt-1">Histórico de transações processadas</p>
      </div>

      {/* Filtros */}
      <Card>
        <CardHeader>
          <CardTitle className="text-lg">Filtros</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
            <div>
              <label className="block text-sm font-medium text-foreground mb-2">ID do Cliente</label>
              <Input
                placeholder="Filtrar por cliente..."
                value={filters.customerId}
                onChange={(e) => handleFilterChange('customerId', e.target.value)}
                className="w-full"
              />
            </div>
            <div>
              <label className="block text-sm font-medium text-foreground mb-2">ID do Merchant</label>
              <Input
                placeholder="Filtrar por merchant..."
                value={filters.merchantId}
                onChange={(e) => handleFilterChange('merchantId', e.target.value)}
                className="w-full"
              />
            </div>
            <div>
              <label className="block text-sm font-medium text-foreground mb-2">Valor Mínimo</label>
              <Input
                type="number"
                placeholder="Valor mínimo..."
                value={filters.minAmount}
                onChange={(e) => handleFilterChange('minAmount', e.target.value)}
                className="w-full"
              />
            </div>
            <div>
              <label className="block text-sm font-medium text-foreground mb-2">Valor Máximo</label>
              <Input
                type="number"
                placeholder="Valor máximo..."
                value={filters.maxAmount}
                onChange={(e) => handleFilterChange('maxAmount', e.target.value)}
                className="w-full"
              />
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Tabela de Transações */}
      <Card>
        <CardHeader>
          <CardTitle>Transações Processadas</CardTitle>
          <CardDescription>Total: {totalElements.toLocaleString()} transações</CardDescription>
        </CardHeader>
        <CardContent>
          {loading ? (
            <div className="flex items-center justify-center h-64">
              <div className="text-center">
                <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-primary mx-auto mb-2"></div>
                <p className="text-muted-foreground">Carregando transações...</p>
              </div>
            </div>
          ) : transactions.length === 0 ? (
            <div className="flex items-center justify-center h-64">
              <p className="text-muted-foreground">Nenhuma transação encontrada</p>
            </div>
          ) : (
            <div className="overflow-x-auto">
              <table className="w-full">
                <thead className="border-b border-border">
                  <tr>
                    <th className="text-left py-3 px-4 font-semibold text-sm text-foreground">ID da Transação</th>
                    <th className="text-left py-3 px-4 font-semibold text-sm text-foreground">Cliente</th>
                    <th className="text-left py-3 px-4 font-semibold text-sm text-foreground">Merchant</th>
                    <th className="text-right py-3 px-4 font-semibold text-sm text-foreground">Valor</th>
                    <th className="text-left py-3 px-4 font-semibold text-sm text-foreground">Classificação</th>
                    <th className="text-center py-3 px-4 font-semibold text-sm text-foreground">Score de Risco</th>
                    <th className="text-left py-3 px-4 font-semibold text-sm text-foreground">Data/Hora</th>
                    <th className="text-center py-3 px-4 font-semibold text-sm text-foreground">Ações</th>
                  </tr>
                </thead>
                <tbody>
                  {transactions.map((txn) => (
                    <tr key={txn.id} className="border-b border-border hover:bg-muted/50 transition-colors">
                      <td className="py-3 px-4 text-sm font-mono text-foreground">{txn.externalTransactionId}</td>
                      <td className="py-3 px-4 text-sm text-foreground">{txn.customerIdFromHeader}</td>
                      <td className="py-3 px-4 text-sm text-foreground">{txn.merchantId || '-'}</td>
                      <td className="py-3 px-4 text-sm text-right font-semibold text-foreground">
                        R$ {txn.transactionAmount?.toLocaleString('pt-BR', { minimumFractionDigits: 2 })}
                      </td>
                      <td className="py-3 px-4 text-sm">
                        <Badge className={`${getClassificationColor(txn.classification)}`}>
                          {getClassificationLabel(txn.classification)}
                        </Badge>
                      </td>
                      <td className="py-3 px-4 text-sm text-center font-semibold text-foreground">
                        <span className={`inline-block px-2 py-1 rounded ${
                          txn.riskScore < 30 ? 'bg-green-100 text-green-800' :
                          txn.riskScore < 70 ? 'bg-amber-100 text-amber-800' :
                          'bg-red-100 text-red-800'
                        }`}>
                          {txn.riskScore}
                        </span>
                      </td>
                      <td className="py-3 px-4 text-sm text-muted-foreground">
                        {new Date(txn.timestamp).toLocaleString('pt-BR')}
                      </td>
                      <td className="py-3 px-4 text-center">
                        <Button
                          variant="ghost"
                          size="sm"
                          onClick={() => window.location.href = `/transactions/${txn.id}`}
                          title="Ver detalhes"
                        >
                          <Eye className="h-4 w-4" />
                        </Button>
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
