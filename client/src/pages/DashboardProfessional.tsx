import React, { useMemo, useState } from 'react';
import {
  BarChart, Bar, LineChart, Line, PieChart, Pie, Cell,
  XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer
} from 'recharts';
import {
  Card, CardContent, CardDescription, CardHeader, CardTitle
} from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import {
  CheckCircle, AlertCircle, XCircle, TrendingUp, TrendingDown,
  Shield, Lock, AlertTriangle, Activity, BarChart3, PieChart as PieChartIcon,
  RefreshCw, Download, Filter
} from 'lucide-react';
import { Button } from '@/components/ui/button';
import { useQuery } from '@tanstack/react-query';
import {
  getDashboardMetrics,
  getMetricsTimeline,
  getMetricsByMerchant,
  listAuditLogs,
  checkApiHealth
} from '@/lib/javaApi';
import { toast } from 'sonner';

/**
 * Dashboard Profissional RULEX
 * Design: Seguro, Tecnológico, Profissional
 * Acessibilidade: WCAG 2.1 AA
 */
export default function DashboardProfessional() {
  const [timeRange, setTimeRange] = useState<'1h' | '24h' | '7d' | '30d'>('24h');

  // Query principal de métricas
  const { data, isLoading, isError, error, refetch } = useQuery({
    queryKey: ['dashboardMetrics', timeRange],
    queryFn: () => getDashboardMetrics(timeRange),
    retry: 1,
  });

  // Query para timeline de transações
  const { data: timelineData } = useQuery({
    queryKey: ['metricsTimeline', timeRange],
    queryFn: () => getMetricsTimeline(timeRange === '30d' || timeRange === '7d' ? 'day' : 'hour'),
    retry: 1,
  });

  // Query para métricas por merchant (com taxa de fraude)
  const { data: merchantMetrics } = useQuery({
    queryKey: ['merchantMetrics', timeRange],
    queryFn: () => getMetricsByMerchant(timeRange),
    retry: 1,
  });

  // Query para alertas recentes (últimos logs de auditoria com fraude)
  const { data: recentAlerts } = useQuery({
    queryKey: ['recentAlerts'],
    queryFn: () => listAuditLogs({ size: 5 }),
    retry: 1,
  });

  // Query para saúde do sistema
  const { data: healthData } = useQuery({
    queryKey: ['apiHealth'],
    queryFn: () => checkApiHealth(),
    refetchInterval: 30000, // Atualiza a cada 30 segundos
    retry: 1,
  });

  const metricsData = useMemo(() => {
    if (data) {
      return {
        totalTransactions: data.totalTransactions ?? 0,
        approvedRate: data.approvalRate ?? 0,
        suspiciousRate: data.suspiciousRate ?? 0,
        fraudRate: data.fraudRate ?? 0,
        avgProcessingTime: healthData?.responseTime ?? 0,
        systemUptime: healthData?.status === 'UP' ? 99.98 : 0,
      };
    }
    return {
      totalTransactions: 0,
      approvedRate: 0,
      suspiciousRate: 0,
      fraudRate: 0,
      avgProcessingTime: 0,
      systemUptime: 0,
    };
  }, [data, healthData]);

  // Transforma dados da timeline para o gráfico
  const transactionTrendData = useMemo(() => {
    if (!timelineData?.buckets || timelineData.buckets.length === 0) {
      return [];
    }
    return timelineData.buckets.map((bucket) => {
      const date = new Date(bucket.bucket);
      const timeLabel = timelineData.granularity === 'day'
        ? date.toLocaleDateString('pt-BR', { day: '2-digit', month: '2-digit' })
        : date.toLocaleTimeString('pt-BR', { hour: '2-digit', minute: '2-digit' });

      // Calcula approved e suspicious baseado no total e fraud
      const approved = Math.max(0, (bucket.total ?? 0) - (bucket.fraud ?? 0));
      const suspicious = bucket.suspicious ?? 0;

      return {
        time: timeLabel,
        total: bucket.total ?? 0,
        approved: approved,
        suspicious: suspicious,
        fraud: bucket.fraud ?? 0,
      };
    });
  }, [timelineData]);

  const palette = ['#0052CC', '#10B981', '#F59E0B', '#8B5CF6', '#6B7280'];
  const mccDistributionData = useMemo(() => {
    const dist = data?.mccDistribution;
    if (dist && Object.keys(dist).length > 0) {
      const total = metricsData.totalTransactions || 1;
      return Object.entries(dist)
        .sort((a, b) => (b[1] ?? 0) - (a[1] ?? 0))
        .slice(0, 8)
        .map(([mcc, count], index) => ({
          name: `MCC ${mcc}`,
          value: ((count ?? 0) / total) * 100,
          color: palette[index % palette.length],
        }));
    }
    return [];
  }, [data, metricsData.totalTransactions]);

  const topMerchantsData = useMemo(() => {
    // Usa merchantMetrics se disponível (tem fraudRate), senão usa merchantDistribution
    if (merchantMetrics && Object.keys(merchantMetrics).length > 0) {
      return Object.entries(merchantMetrics)
        .sort((a, b) => (b[1]?.total ?? 0) - (a[1]?.total ?? 0))
        .slice(0, 8)
        .map(([merchantId, metrics], index) => ({
          name: metrics.merchantName || merchantId,
          transactions: metrics.total ?? 0,
          fraudRate: typeof metrics.fraudRate === 'number' ? metrics.fraudRate : 0,
          idx: index,
        }));
    }
    // Fallback para merchantDistribution do metrics principal
    const dist = data?.merchantDistribution;
    if (dist && Object.keys(dist).length > 0) {
      return Object.entries(dist)
        .sort((a, b) => (b[1] ?? 0) - (a[1] ?? 0))
        .slice(0, 8)
        .map(([merchantId, count], index) => ({
          name: merchantId,
          transactions: count ?? 0,
          fraudRate: 0,
          idx: index,
        }));
    }
    return [];
  }, [data, merchantMetrics]);

  const handleRefresh = async () => {
    const res = await refetch();
    if (res.error) {
      toast.error('Falha ao atualizar métricas');
    }
  };

  return (
    <div className="min-h-screen bg-gradient-to-br from-gray-50 to-gray-100 p-6 md:p-8">
      {/* Header */}
      <div className="mb-8">
        <div className="flex items-center justify-between mb-6">
          <div>
            <h1 className="text-3xl md:text-4xl font-bold text-gray-900 flex items-center gap-3">
              <Shield className="w-8 h-8 text-blue-600" />
              RULEX Dashboard
            </h1>
            <p className="text-gray-600 mt-2">
              Sistema de Análise e Prevenção de Fraudes em Transações de Crédito
            </p>
          </div>
          <div className="flex gap-3">
            <Button
              variant="outline"
              size="sm"
              onClick={handleRefresh}
              disabled={isLoading}
              className="flex items-center gap-2"
              aria-label="Atualizar dados do dashboard"
            >
              <RefreshCw className={`w-4 h-4 ${isLoading ? 'animate-spin' : ''}`} />
              Atualizar
            </Button>
            <Button
              variant="outline"
              size="sm"
              className="flex items-center gap-2"
              aria-label="Exportar relatório"
            >
              <Download className="w-4 h-4" />
              Exportar
            </Button>
          </div>
        </div>

        {/* Time Range Selector */}
        <div className="flex gap-2">
          {(['1h', '24h', '7d', '30d'] as const).map((range) => (
            <button
              key={range}
              onClick={() => setTimeRange(range)}
              className={`px-4 py-2 rounded-lg font-medium transition-all ${
                timeRange === range
                  ? 'bg-blue-600 text-white shadow-lg'
                  : 'bg-white text-gray-700 border border-gray-200 hover:border-blue-300'
              }`}
              aria-pressed={timeRange === range}
            >
              {range}
            </button>
          ))}
        </div>

        {isError && (
          <div
            className="mt-4 rounded-lg border border-red-200 bg-red-50 p-3 text-red-800"
            role="alert"
          >
            Falha ao carregar métricas: {error instanceof Error ? error.message : 'erro inesperado'}
          </div>
        )}
        {isLoading && (
          <p className="mt-4 text-sm text-muted-foreground" role="status" aria-live="polite">
            Carregando métricas em tempo real...
          </p>
        )}
      </div>

      {/* KPI Cards */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
        {/* Total Transactions */}
        <Card className="border-0 shadow-lg hover:shadow-xl transition-shadow">
          <CardHeader className="pb-3">
            <CardTitle className="text-sm font-medium text-gray-600 flex items-center gap-2">
              <Activity className="w-4 h-4 text-blue-600" />
              Total de Transações
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="text-3xl font-bold text-gray-900">
              {metricsData.totalTransactions.toLocaleString()}
            </div>
            <p className="text-xs text-muted-foreground mt-2">
              Período: {timeRange}
            </p>
          </CardContent>
        </Card>

        {/* Approval Rate */}
        <Card className="border-0 shadow-lg hover:shadow-xl transition-shadow">
          <CardHeader className="pb-3">
            <CardTitle className="text-sm font-medium text-gray-600 flex items-center gap-2">
              <CheckCircle className="w-4 h-4 text-green-600" />
              Taxa de Aprovação
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="text-3xl font-bold text-gray-900">
              {metricsData.approvedRate}%
            </div>
            <div className="mt-2">
              <div className="w-full bg-gray-200 rounded-full h-2">
                <div
                  className="bg-green-600 h-2 rounded-full transition-all"
                  style={{ width: `${metricsData.approvedRate}%` }}
                  role="progressbar"
                  aria-valuenow={metricsData.approvedRate}
                  aria-valuemin={0}
                  aria-valuemax={100}
                />
              </div>
            </div>
          </CardContent>
        </Card>

        {/* Suspicious Rate */}
        <Card className="border-0 shadow-lg hover:shadow-xl transition-shadow">
          <CardHeader className="pb-3">
            <CardTitle className="text-sm font-medium text-gray-600 flex items-center gap-2">
              <AlertCircle className="w-4 h-4 text-amber-600" />
              Transações Suspeitas
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="text-3xl font-bold text-gray-900">
              {metricsData.suspiciousRate}%
            </div>
            <div className="mt-2">
              <Badge variant="outline" className="bg-amber-50 text-amber-800 border-amber-200">
                Requer Revisão
              </Badge>
            </div>
          </CardContent>
        </Card>

        {/* Fraud Rate */}
        <Card className="border-0 shadow-lg hover:shadow-xl transition-shadow bg-gradient-to-br from-red-50 to-red-100">
          <CardHeader className="pb-3">
            <CardTitle className="text-sm font-medium text-gray-600 flex items-center gap-2">
              <XCircle className="w-4 h-4 text-red-600" />
              Taxa de Fraude
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="text-3xl font-bold text-red-700">
              {metricsData.fraudRate}%
            </div>
            <div className="mt-2">
              <Badge variant="outline" className="bg-red-50 text-red-700 border-red-200">
                {data?.fraudTransactions?.toLocaleString() ?? 0} fraudes detectadas
              </Badge>
            </div>
          </CardContent>
        </Card>
      </div>

      {/* System Health */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-6 mb-8">
        <Card className="border-0 shadow-lg">
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <Lock className="w-5 h-5 text-blue-600" />
              Saúde do Sistema
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="space-y-4">
              <div>
                <div className="flex justify-between mb-2">
                  <span className="text-sm font-medium text-gray-700">Tempo de Resposta API</span>
                  <span className={`text-sm font-bold ${metricsData.avgProcessingTime < 500 ? 'text-green-600' : metricsData.avgProcessingTime < 1000 ? 'text-amber-600' : 'text-red-600'}`}>
                    {metricsData.avgProcessingTime}ms
                  </span>
                </div>
                <div className="w-full bg-gray-200 rounded-full h-2">
                  <div
                    className={`h-2 rounded-full transition-all ${metricsData.avgProcessingTime < 500 ? 'bg-green-600' : metricsData.avgProcessingTime < 1000 ? 'bg-amber-600' : 'bg-red-600'}`}
                    style={{ width: `${Math.min(100, Math.max(5, 100 - (metricsData.avgProcessingTime / 20)))}%` }}
                  />
                </div>
              </div>
              <div>
                <div className="flex justify-between mb-2">
                  <span className="text-sm font-medium text-gray-700">Status da API</span>
                  <span className={`text-sm font-bold ${healthData?.status === 'UP' ? 'text-green-600' : 'text-red-600'}`}>
                    {healthData?.status === 'UP' ? 'Online' : 'Offline'}
                  </span>
                </div>
                <div className="w-full bg-gray-200 rounded-full h-2">
                  <div
                    className={`h-2 rounded-full ${healthData?.status === 'UP' ? 'bg-green-600' : 'bg-red-600'}`}
                    style={{ width: healthData?.status === 'UP' ? '100%' : '0%' }}
                  />
                </div>
              </div>
            </div>
          </CardContent>
        </Card>

        {/* Recent Alerts */}
        <Card className="border-0 shadow-lg">
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <AlertTriangle className="w-5 h-5 text-amber-600" />
              Atividade Recente
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="space-y-3">
              {recentAlerts?.content && recentAlerts.content.length > 0 ? (
                recentAlerts.content.slice(0, 4).map((alert) => {
                  const isError = alert.result === 'FAILURE' || alert.actionType?.includes('FRAUD');
                  const isWarning = alert.actionType?.includes('SUSPICIOUS') || alert.actionType?.includes('RULE');

                  return (
                    <div
                      key={alert.id}
                      className={`flex items-start gap-3 p-3 rounded-lg border ${
                        isError
                          ? 'bg-red-50 border-red-200'
                          : isWarning
                            ? 'bg-amber-50 border-amber-200'
                            : 'bg-blue-50 border-blue-200'
                      }`}
                    >
                      {isError ? (
                        <XCircle className="w-5 h-5 text-red-600 mt-0.5 flex-shrink-0" />
                      ) : isWarning ? (
                        <AlertCircle className="w-5 h-5 text-amber-600 mt-0.5 flex-shrink-0" />
                      ) : (
                        <CheckCircle className="w-5 h-5 text-blue-600 mt-0.5 flex-shrink-0" />
                      )}
                      <div className="flex-1 min-w-0">
                        <p className={`text-sm font-medium truncate ${
                          isError ? 'text-red-900' : isWarning ? 'text-amber-900' : 'text-blue-900'
                        }`}>
                          {alert.actionType?.replace(/_/g, ' ') || 'Ação'}
                        </p>
                        <p className={`text-xs truncate ${
                          isError ? 'text-red-700' : isWarning ? 'text-amber-700' : 'text-blue-700'
                        }`}>
                          {alert.description || 'Sem descrição'}
                        </p>
                        <p className="text-xs text-gray-500 mt-1">
                          {new Date(alert.createdAt).toLocaleString('pt-BR')}
                        </p>
                      </div>
                    </div>
                  );
                })
              ) : (
                <p className="text-sm text-muted-foreground text-center py-4">
                  Nenhuma atividade recente
                </p>
              )}
            </div>
          </CardContent>
        </Card>
      </div>

      {/* Charts */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-8">
        {/* Transaction Trend */}
        <Card className="border-0 shadow-lg">
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <BarChart3 className="w-5 h-5 text-blue-600" />
              Tendência de Transações
            </CardTitle>
            <CardDescription>
              {timeRange === '30d' || timeRange === '7d' ? 'Por dia' : 'Por hora'} - Período: {timeRange}
            </CardDescription>
          </CardHeader>
          <CardContent>
            {transactionTrendData.length === 0 ? (
              <div className="flex items-center justify-center h-[300px] text-muted-foreground">
                <p>Sem dados de timeline para este período</p>
              </div>
            ) : (
              <ResponsiveContainer width="100%" height={300}>
                <LineChart data={transactionTrendData}>
                  <CartesianGrid strokeDasharray="3 3" stroke="#E5E7EB" />
                  <XAxis dataKey="time" stroke="#6B7280" />
                  <YAxis stroke="#6B7280" />
                  <Tooltip
                    contentStyle={{
                      backgroundColor: '#FFFFFF',
                      border: '1px solid #E5E7EB',
                      borderRadius: '8px'
                    }}
                  />
                  <Legend />
                  <Line
                    type="monotone"
                    dataKey="total"
                    stroke="#3B82F6"
                    name="Total"
                    strokeWidth={2}
                    dot={{ fill: '#3B82F6', r: 3 }}
                  />
                  <Line
                    type="monotone"
                    dataKey="fraud"
                    stroke="#EF4444"
                    name="Fraudes"
                    strokeWidth={2}
                    dot={{ fill: '#EF4444', r: 3 }}
                  />
                </LineChart>
              </ResponsiveContainer>
            )}
          </CardContent>
        </Card>

        {/* MCC Distribution */}
        <Card className="border-0 shadow-lg">
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <PieChartIcon className="w-5 h-5 text-blue-600" />
              Distribuição por MCC
            </CardTitle>
            <CardDescription>Categorias de merchant</CardDescription>
          </CardHeader>
          <CardContent>
            <ResponsiveContainer width="100%" height={300}>
              <PieChart>
                <Pie
                  data={mccDistributionData}
                  cx="50%"
                  cy="50%"
                  labelLine={false}
                  label={({ name, value }) => `${name}: ${value}%`}
                  outerRadius={80}
                  fill="#8884d8"
                  dataKey="value"
                >
                  {mccDistributionData.map((entry, index) => (
                    <Cell key={`cell-${index}`} fill={entry.color} />
                  ))}
                </Pie>
                <Tooltip />
              </PieChart>
            </ResponsiveContainer>
          </CardContent>
        </Card>
      </div>

      {/* Rules Triggered */}
      <Card className="border-0 shadow-lg mb-8">
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Shield className="w-5 h-5 text-blue-600" />
            Top Merchants por Volume
          </CardTitle>
          <CardDescription>Volume processado e taxa de fraude</CardDescription>
        </CardHeader>
        <CardContent>
          {topMerchantsData.length === 0 ? (
            <p className="text-sm text-muted-foreground">Sem dados de merchants para este período.</p>
          ) : (
            <div className="space-y-3">
              {topMerchantsData.map((merchant) => (
                <div key={merchant.name} className="flex items-center justify-between p-4 bg-gray-50 rounded-lg border border-gray-200 hover:bg-gray-100 transition-colors">
                  <div className="flex-1">
                    <p className="font-medium text-gray-900">{merchant.name}</p>
                    <p className="text-sm text-gray-600">{merchant.transactions.toLocaleString()} transações</p>
                  </div>
                  <Badge variant="outline" className="bg-red-50 text-red-700 border-red-200">
                    Fraude: {merchant.fraudRate}%
                  </Badge>
                </div>
              ))}
            </div>
          )}
        </CardContent>
      </Card>

      {/* Footer */}
      <div className="text-center text-sm text-gray-600">
        <p>Última atualização: {new Date().toLocaleString('pt-BR')}</p>
        <p className="mt-1">RULEX v1.0 - Sistema de Prevenção de Fraudes © 2025</p>
      </div>
    </div>
  );
}
