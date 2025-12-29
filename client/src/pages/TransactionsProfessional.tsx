import React, { useEffect, useMemo, useState } from 'react';
import {
  Card, CardContent, CardDescription, CardHeader, CardTitle
} from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import {
  Search, Filter, Download, Eye, CheckCircle, AlertCircle, XCircle,
  ChevronLeft, ChevronRight, Calendar, DollarSign
} from 'lucide-react';
import { keepPreviousData, useQuery } from '@tanstack/react-query';
import { exportTransactions, listTransactions } from '@/lib/javaApi';
import { toast } from 'sonner';

/**
 * Página de Transações Profissional
 * Design: Seguro, Tecnológico, Profissional
 * Acessibilidade: WCAG 2.1 AA
 */
export default function TransactionsProfessional() {
  const [searchTerm, setSearchTerm] = useState('');
  const [filterStatus, setFilterStatus] = useState('all');
  const [currentPage, setCurrentPage] = useState(1);
  const itemsPerPage = 10;
  const { data, isLoading, isError, error, isFetching } = useQuery({
    queryKey: ['transactions', { searchTerm, filterStatus, currentPage }],
    queryFn: () =>
      listTransactions({
        customerId: searchTerm || undefined,
        merchantId: searchTerm || undefined,
        classification:
          filterStatus === "all"
            ? undefined
            : (filterStatus as "APPROVED" | "SUSPICIOUS" | "FRAUD"),
        page: currentPage - 1,
        size: itemsPerPage,
      }),
    retry: 1,
    placeholderData: keepPreviousData,
  });

  const handleExport = async () => {
    try {
      const blob = (await exportTransactions(
        "csv",
        {
          customerId: searchTerm || undefined,
          merchantId: searchTerm || undefined,
          classification:
            filterStatus === "all"
              ? undefined
              : (filterStatus as "APPROVED" | "SUSPICIOUS" | "FRAUD"),
        },
        10000
      )) as Blob;

      const url = window.URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = url;
      a.download = "rulex-transactions.csv";
      document.body.appendChild(a);
      a.click();
      a.remove();
      window.URL.revokeObjectURL(url);
      toast.success("Export gerado");
    } catch (e) {
      toast.error(`Falha ao exportar: ${e instanceof Error ? e.message : "erro inesperado"}`);
    }
  };

  useEffect(() => {
    if (isError) {
      toast.error('Falha ao carregar transações');
    }
  }, [isError]);

  const paginatedTransactions = data?.content ?? [];
  const totalPages = useMemo(() => data?.totalPages ?? 1, [data]);
  const totalElements = data?.totalElements ?? paginatedTransactions.length;
  const startIndex = (currentPage - 1) * itemsPerPage;

  // Função para obter cor do status
  const getStatusColor = (status: string) => {
    switch (status) {
      case 'APPROVED':
        return 'bg-green-50 text-green-800 border-green-200';
      case 'SUSPICIOUS':
        return 'bg-amber-50 text-amber-800 border-amber-200';
      case 'FRAUD':
        return 'bg-red-50 text-red-800 border-red-200';
      default:
        return 'bg-gray-50 text-gray-800 border-gray-200';
    }
  };

  // Função para obter ícone do status
  const getStatusIcon = (status: string) => {
    switch (status) {
      case 'APPROVED':
        return <CheckCircle className="w-4 h-4 text-green-600" />;
      case 'SUSPICIOUS':
        return <AlertCircle className="w-4 h-4 text-amber-600" />;
      case 'FRAUD':
        return <XCircle className="w-4 h-4 text-red-600" />;
      default:
        return null;
    }
  };

  return (
    <div className="min-h-screen bg-gradient-to-br from-gray-50 to-gray-100 p-6 md:p-8">
      {/* Header */}
      <div className="mb-8">
        <h1 className="text-3xl md:text-4xl font-bold text-gray-900 mb-2">
          Transações
        </h1>
        <p className="text-gray-600">
          Visualize e analise todas as transações processadas pelo sistema
        </p>
      </div>

      {/* Filters and Search */}
      <Card className="border-0 shadow-lg mb-8">
        <CardHeader className="pb-4">
          <CardTitle className="text-lg">Filtros e Busca</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            {/* Search */}
            <div className="relative">
              <Search className="absolute left-3 top-3 w-5 h-5 text-gray-400" />
              <input
                type="text"
                placeholder="Buscar por ID, Cliente ou Merchant..."
                value={searchTerm}
                onChange={(e) => {
                  setSearchTerm(e.target.value);
                  setCurrentPage(1);
                }}
                className="w-full pl-10 pr-4 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                aria-label="Buscar transações"
              />
            </div>

            {/* Status Filter */}
            <div>
              <select
                value={filterStatus}
                onChange={(e) => {
                  setFilterStatus(e.target.value);
                  setCurrentPage(1);
                }}
                className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                aria-label="Filtrar por status"
              >
                <option value="all">Todos os Status</option>
                <option value="APPROVED">Aprovadas</option>
                <option value="SUSPICIOUS">Suspeitas</option>
                <option value="FRAUD">Fraudes</option>
              </select>
            </div>

            {/* Action Buttons */}
            <div className="flex gap-2">
              <Button
                variant="outline"
                className="flex-1 flex items-center justify-center gap-2"
                aria-label="Filtros avançados"
              >
                <Filter className="w-4 h-4" />
                Filtros
              </Button>
              <Button
                variant="outline"
                className="flex-1 flex items-center justify-center gap-2"
                aria-label="Exportar transações"
                onClick={handleExport}
              >
                <Download className="w-4 h-4" />
                Exportar
              </Button>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Transactions Table */}
      <Card className="border-0 shadow-lg">
        <CardHeader className="pb-4">
          <div className="flex items-center justify-between">
            <div>
              <CardTitle className="text-lg">
                Transações ({totalElements})
              </CardTitle>
              <CardDescription>
                Mostrando {startIndex + 1} a {Math.min(startIndex + itemsPerPage, totalElements)} de {totalElements}
              </CardDescription>
            </div>
          </div>
        </CardHeader>
        <CardContent>
          {isError && (
            <div className="mb-4 rounded-lg border border-red-200 bg-red-50 p-3 text-red-800" role="alert">
              Erro ao carregar transações: {error instanceof Error ? error.message : 'erro inesperado'}
            </div>
          )}
          {(isLoading || isFetching) && (
            <p className="mb-4 text-sm text-muted-foreground" role="status" aria-live="polite">
              Carregando transações...
            </p>
          )}
          {!isLoading && !isError && paginatedTransactions.length === 0 && (
            <p className="mb-4 text-sm text-muted-foreground">Nenhuma transação encontrada para os filtros atuais.</p>
          )}
          {/* Desktop Table */}
          <div className="hidden md:block overflow-x-auto">
            <table className="w-full text-sm">
              <thead>
                <tr className="border-b border-gray-200 bg-gray-50">
                  <th className="px-6 py-3 text-left font-semibold text-gray-700">ID Transação</th>
                  <th className="px-6 py-3 text-left font-semibold text-gray-700">Cliente</th>
                  <th className="px-6 py-3 text-left font-semibold text-gray-700">Merchant</th>
                  <th className="px-6 py-3 text-right font-semibold text-gray-700">Valor</th>
                  <th className="px-6 py-3 text-right font-semibold text-gray-700">Score</th>
                  <th className="px-6 py-3 text-left font-semibold text-gray-700">Status</th>
                  <th className="px-6 py-3 text-left font-semibold text-gray-700">Data/Hora</th>
                  <th className="px-6 py-3 text-center font-semibold text-gray-700">Ação</th>
                </tr>
              </thead>
              <tbody>
                {paginatedTransactions.map((tx) => (
                  <tr
                    key={tx.transactionId}
                    className="border-b border-gray-200 hover:bg-gray-50 transition-colors"
                    role="row"
                  >
                    <td className="px-6 py-4 font-mono text-blue-600">{tx.transactionId}</td>
                    <td className="px-6 py-4 text-gray-700">{tx.customerIdFromHeader || "-"}</td>
                    <td className="px-6 py-4 text-gray-700">{tx.merchantName || tx.merchantId || "-"}</td>
                    <td className="px-6 py-4 text-right font-semibold text-gray-900">
                      {typeof tx.transactionAmount === "number" ? `R$ ${tx.transactionAmount.toFixed(2)}` : "-"}
                    </td>
                    <td className="px-6 py-4 text-right font-semibold text-gray-900">
                      {tx.riskScore}
                    </td>
                    <td className="px-6 py-4">
                      <div className="flex items-center gap-2">
                        {getStatusIcon(tx.classification)}
                        <Badge
                          variant="outline"
                          className={`${getStatusColor(tx.classification)}`}
                        >
                          {tx.classification === 'APPROVED'
                            ? 'Aprovada'
                            : tx.classification === 'SUSPICIOUS'
                            ? 'Suspeita'
                            : 'Fraude'}
                        </Badge>
                      </div>
                    </td>
                    <td className="px-6 py-4 text-gray-600 text-xs">{tx.timestamp}</td>
                    <td className="px-6 py-4 text-center">
                      <button
                        className="p-2 hover:bg-blue-100 rounded-lg transition-colors"
                        aria-label={`Visualizar detalhes da transação ${tx.transactionId}`}
                      >
                        <Eye className="w-4 h-4 text-blue-600" />
                      </button>
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>

          {/* Mobile Cards */}
          <div className="md:hidden space-y-4">
            {paginatedTransactions.map((tx) => (
              <div
                key={tx.transactionId}
                className="p-4 border border-gray-200 rounded-lg hover:shadow-md transition-shadow"
              >
                <div className="flex items-start justify-between mb-3">
                  <div>
                    <p className="font-mono text-blue-600 font-semibold">{tx.transactionId}</p>
                    <p className="text-xs text-gray-600 mt-1">{tx.timestamp}</p>
                  </div>
                  <div className="flex items-center gap-2">
                    {getStatusIcon(tx.classification)}
                    <Badge
                      variant="outline"
                      className={`${getStatusColor(tx.classification)}`}
                    >
                      {tx.classification === 'APPROVED'
                        ? 'Aprovada'
                        : tx.classification === 'SUSPICIOUS'
                        ? 'Suspeita'
                        : 'Fraude'}
                    </Badge>
                  </div>
                </div>
                <div className="grid grid-cols-2 gap-2 text-sm mb-3">
                  <div>
                    <p className="text-gray-600">Cliente</p>
                    <p className="font-medium text-gray-900">{tx.customerIdFromHeader || "-"}</p>
                  </div>
                  <div>
                    <p className="text-gray-600">Merchant</p>
                    <p className="font-medium text-gray-900">{tx.merchantName || tx.merchantId || "-"}</p>
                  </div>
                </div>
                <div className="flex items-center justify-between">
                  <div className="flex items-center gap-2">
                    <DollarSign className="w-4 h-4 text-gray-600" />
                    <span className="font-semibold text-gray-900">
                      {typeof tx.transactionAmount === "number"
                        ? `R$ ${tx.transactionAmount.toFixed(2)}`
                        : `Score: ${tx.riskScore}`}
                    </span>
                  </div>
                  <button
                    className="p-2 hover:bg-blue-100 rounded-lg transition-colors"
                    aria-label={`Visualizar detalhes da transação ${tx.transactionId}`}
                  >
                    <Eye className="w-4 h-4 text-blue-600" />
                  </button>
                </div>
              </div>
            ))}
          </div>

          {/* Pagination */}
          {totalPages > 1 && (
            <div className="flex items-center justify-between mt-6 pt-6 border-t border-gray-200">
              <p className="text-sm text-gray-600">
                Página {currentPage} de {totalPages}
              </p>
              <div className="flex gap-2">
                <button
                  onClick={() => setCurrentPage(Math.max(1, currentPage - 1))}
                  disabled={currentPage === 1}
                  className="p-2 border border-gray-300 rounded-lg hover:bg-gray-100 disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
                  aria-label="Página anterior"
                >
                  <ChevronLeft className="w-4 h-4" />
                </button>
                <button
                  onClick={() => setCurrentPage(Math.min(totalPages, currentPage + 1))}
                  disabled={currentPage === totalPages}
                  className="p-2 border border-gray-300 rounded-lg hover:bg-gray-100 disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
                  aria-label="Próxima página"
                >
                  <ChevronRight className="w-4 h-4" />
                </button>
              </div>
            </div>
          )}
        </CardContent>
      </Card>
    </div>
  );
}
