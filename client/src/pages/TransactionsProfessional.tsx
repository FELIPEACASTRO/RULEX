import React, { useState } from 'react';
import {
  Card, CardContent, CardDescription, CardHeader, CardTitle
} from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import {
  Search, Filter, Download, Eye, CheckCircle, AlertCircle, XCircle,
  ChevronLeft, ChevronRight, Calendar, DollarSign
} from 'lucide-react';

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

  // Dados simulados
  const transactions = [
    {
      id: 1,
      externalId: 'TRX-2025-001',
      customerId: 'CUST-12345',
      merchantId: 'MER-98765',
      amount: 1250.00,
      currency: 'BRL',
      status: 'APPROVED',
      timestamp: '2025-12-16 14:32:15',
      mcc: 5411,
      rules: ['SCORE_CHECK', 'VELOCITY_OK']
    },
    {
      id: 2,
      externalId: 'TRX-2025-002',
      customerId: 'CUST-54321',
      merchantId: 'MER-11111',
      amount: 5800.00,
      currency: 'BRL',
      status: 'SUSPICIOUS',
      timestamp: '2025-12-16 14:28:42',
      mcc: 6211,
      rules: ['UNUSUAL_LOCATION', 'HIGH_AMOUNT']
    },
    {
      id: 3,
      externalId: 'TRX-2025-003',
      customerId: 'CUST-99999',
      merchantId: 'MER-22222',
      amount: 3200.00,
      currency: 'BRL',
      status: 'FRAUD',
      timestamp: '2025-12-16 14:15:08',
      mcc: 7995,
      rules: ['DUPLICATE_TRANSACTION', 'VELOCITY_SPIKE']
    },
    {
      id: 4,
      externalId: 'TRX-2025-004',
      customerId: 'CUST-77777',
      merchantId: 'MER-33333',
      amount: 450.50,
      currency: 'BRL',
      status: 'APPROVED',
      timestamp: '2025-12-16 14:05:33',
      mcc: 5812,
      rules: ['SCORE_CHECK', 'VELOCITY_OK']
    },
    {
      id: 5,
      externalId: 'TRX-2025-005',
      customerId: 'CUST-55555',
      merchantId: 'MER-44444',
      amount: 12000.00,
      currency: 'BRL',
      status: 'SUSPICIOUS',
      timestamp: '2025-12-16 13:52:19',
      mcc: 4829,
      rules: ['VERY_HIGH_AMOUNT', 'NEW_MERCHANT']
    },
    {
      id: 6,
      externalId: 'TRX-2025-006',
      customerId: 'CUST-33333',
      merchantId: 'MER-55555',
      amount: 890.00,
      currency: 'BRL',
      status: 'APPROVED',
      timestamp: '2025-12-16 13:40:05',
      mcc: 5411,
      rules: ['SCORE_CHECK', 'VELOCITY_OK']
    },
    {
      id: 7,
      externalId: 'TRX-2025-007',
      customerId: 'CUST-88888',
      merchantId: 'MER-66666',
      amount: 2100.00,
      currency: 'BRL',
      status: 'FRAUD',
      timestamp: '2025-12-16 13:28:42',
      mcc: 6051,
      rules: ['EXPIRED_CARD', 'INVALID_CVV']
    },
    {
      id: 8,
      externalId: 'TRX-2025-008',
      customerId: 'CUST-11111',
      merchantId: 'MER-77777',
      amount: 650.00,
      currency: 'BRL',
      status: 'APPROVED',
      timestamp: '2025-12-16 13:15:27',
      mcc: 5814,
      rules: ['SCORE_CHECK', 'VELOCITY_OK']
    }
  ];

  // Filtrar transações
  const filteredTransactions = transactions.filter(tx => {
    const matchesSearch = tx.externalId.toLowerCase().includes(searchTerm.toLowerCase()) ||
                         tx.customerId.toLowerCase().includes(searchTerm.toLowerCase()) ||
                         tx.merchantId.toLowerCase().includes(searchTerm.toLowerCase());
    const matchesStatus = filterStatus === 'all' || tx.status === filterStatus;
    return matchesSearch && matchesStatus;
  });

  // Paginação
  const totalPages = Math.ceil(filteredTransactions.length / itemsPerPage);
  const startIndex = (currentPage - 1) * itemsPerPage;
  const paginatedTransactions = filteredTransactions.slice(startIndex, startIndex + itemsPerPage);

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
                Transações ({filteredTransactions.length})
              </CardTitle>
              <CardDescription>
                Mostrando {startIndex + 1} a {Math.min(startIndex + itemsPerPage, filteredTransactions.length)} de {filteredTransactions.length}
              </CardDescription>
            </div>
          </div>
        </CardHeader>
        <CardContent>
          {/* Desktop Table */}
          <div className="hidden md:block overflow-x-auto">
            <table className="w-full text-sm">
              <thead>
                <tr className="border-b border-gray-200 bg-gray-50">
                  <th className="px-6 py-3 text-left font-semibold text-gray-700">ID Transação</th>
                  <th className="px-6 py-3 text-left font-semibold text-gray-700">Cliente</th>
                  <th className="px-6 py-3 text-left font-semibold text-gray-700">Merchant</th>
                  <th className="px-6 py-3 text-right font-semibold text-gray-700">Valor</th>
                  <th className="px-6 py-3 text-left font-semibold text-gray-700">Status</th>
                  <th className="px-6 py-3 text-left font-semibold text-gray-700">Data/Hora</th>
                  <th className="px-6 py-3 text-center font-semibold text-gray-700">Ação</th>
                </tr>
              </thead>
              <tbody>
                {paginatedTransactions.map((tx) => (
                  <tr
                    key={tx.id}
                    className="border-b border-gray-200 hover:bg-gray-50 transition-colors"
                    role="row"
                  >
                    <td className="px-6 py-4 font-mono text-blue-600">{tx.externalId}</td>
                    <td className="px-6 py-4 text-gray-700">{tx.customerId}</td>
                    <td className="px-6 py-4 text-gray-700">{tx.merchantId}</td>
                    <td className="px-6 py-4 text-right font-semibold text-gray-900">
                      R$ {tx.amount.toFixed(2)}
                    </td>
                    <td className="px-6 py-4">
                      <div className="flex items-center gap-2">
                        {getStatusIcon(tx.status)}
                        <Badge
                          variant="outline"
                          className={`${getStatusColor(tx.status)}`}
                        >
                          {tx.status === 'APPROVED'
                            ? 'Aprovada'
                            : tx.status === 'SUSPICIOUS'
                            ? 'Suspeita'
                            : 'Fraude'}
                        </Badge>
                      </div>
                    </td>
                    <td className="px-6 py-4 text-gray-600 text-xs">{tx.timestamp}</td>
                    <td className="px-6 py-4 text-center">
                      <button
                        className="p-2 hover:bg-blue-100 rounded-lg transition-colors"
                        aria-label={`Visualizar detalhes da transação ${tx.externalId}`}
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
                key={tx.id}
                className="p-4 border border-gray-200 rounded-lg hover:shadow-md transition-shadow"
              >
                <div className="flex items-start justify-between mb-3">
                  <div>
                    <p className="font-mono text-blue-600 font-semibold">{tx.externalId}</p>
                    <p className="text-xs text-gray-600 mt-1">{tx.timestamp}</p>
                  </div>
                  <div className="flex items-center gap-2">
                    {getStatusIcon(tx.status)}
                    <Badge
                      variant="outline"
                      className={`${getStatusColor(tx.status)}`}
                    >
                      {tx.status === 'APPROVED'
                        ? 'Aprovada'
                        : tx.status === 'SUSPICIOUS'
                        ? 'Suspeita'
                        : 'Fraude'}
                    </Badge>
                  </div>
                </div>
                <div className="grid grid-cols-2 gap-2 text-sm mb-3">
                  <div>
                    <p className="text-gray-600">Cliente</p>
                    <p className="font-medium text-gray-900">{tx.customerId}</p>
                  </div>
                  <div>
                    <p className="text-gray-600">Merchant</p>
                    <p className="font-medium text-gray-900">{tx.merchantId}</p>
                  </div>
                </div>
                <div className="flex items-center justify-between">
                  <div className="flex items-center gap-2">
                    <DollarSign className="w-4 h-4 text-gray-600" />
                    <span className="font-semibold text-gray-900">R$ {tx.amount.toFixed(2)}</span>
                  </div>
                  <button
                    className="p-2 hover:bg-blue-100 rounded-lg transition-colors"
                    aria-label={`Visualizar detalhes da transação ${tx.externalId}`}
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
