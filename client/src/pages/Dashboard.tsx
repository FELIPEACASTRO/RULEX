import React, { useMemo } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { LineChart, Line, PieChart, Pie, Cell, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer } from 'recharts';
import { TrendingUp, TrendingDown, AlertCircle, CheckCircle, Clock } from 'lucide-react';
import { useQuery } from '@tanstack/react-query';
import { getDashboardMetrics, getMetricsTimeline } from '@/lib/javaApi';

/**
 * Dashboard com métricas em tempo real.
 */
export default function Dashboard() {
  const [period, setPeriod] = React.useState<'1h' | '24h' | '7d' | '30d'>('24h');

  const { data: metrics, isLoading } = useQuery({
    queryKey: ['dashboardMetrics', period],
    queryFn: () => getDashboardMetrics(period),
    refetchInterval: 30000, // Atualizar a cada 30 segundos
  });

  const { data: timelineResponse } = useQuery({
    queryKey: ['metricsTimeline', period],
    queryFn: () => getMetricsTimeline(period === '30d' || period === '7d' ? 'day' : 'hour'),
  });

  const classificationData = useMemo(() => {
    if (!metrics) return [];
    return [
      { name: 'Aprovadas', value: metrics.approvedTransactions ?? 0, fill: '#10b981' },
      { name: 'Suspeitas', value: metrics.suspiciousTransactions ?? 0, fill: '#f59e0b' },
      { name: 'Fraudes', value: metrics.fraudTransactions ?? 0, fill: '#ef4444' },
    ];
  }, [metrics]);

  const timelineData = useMemo(() => {
    if (!timelineResponse?.buckets) return [];
    return timelineResponse.buckets.map((bucket) => {
      const date = new Date(bucket.bucket);
      const timeLabel = timelineResponse.granularity === 'day'
        ? date.toLocaleDateString('pt-BR', { day: '2-digit', month: '2-digit' })
        : date.toLocaleTimeString('pt-BR', { hour: '2-digit', minute: '2-digit' });

      const total = bucket.total ?? 0;
      const fraud = bucket.fraud ?? 0;
      const approved = Math.max(0, total - fraud);

      return {
        time: timeLabel,
        approved,
        suspicious: 0,
        fraud,
      };
    });
  }, [timelineResponse]);

  if (isLoading || !metrics) {
    return (
      <div className="flex items-center justify-center h-screen">
        <div className="text-center">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-primary mx-auto mb-4"></div>
          <p className="text-muted-foreground">Carregando métricas...</p>
        </div>
      </div>
    );
  }

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex justify-between items-center">
        <div>
          <h1 className="text-3xl font-bold text-foreground">Dashboard</h1>
          <p className="text-muted-foreground mt-1">Visão geral de transações e fraudes</p>
        </div>
        <select
          value={period}
          onChange={(e) => setPeriod(e.target.value as '1h' | '24h' | '7d' | '30d')}
          className="px-4 py-2 border border-input rounded-lg bg-background text-foreground"
        >
          <option value="1h">Última 1 hora</option>
          <option value="24h">Últimas 24 horas</option>
          <option value="7d">Últimos 7 dias</option>
          <option value="30d">Últimos 30 dias</option>
        </select>
      </div>

      {/* KPI Cards */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
        {/* Total de Transações */}
        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Total de Transações</CardTitle>
            <TrendingUp className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{(metrics.totalTransactions ?? 0).toLocaleString()}</div>
            <p className="text-xs text-muted-foreground">Período: {period}</p>
          </CardContent>
        </Card>

        {/* Taxa de Aprovação */}
        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Taxa de Aprovação</CardTitle>
            <CheckCircle className="h-4 w-4 text-green-600" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{Number(metrics.approvalRate ?? 0).toFixed(2)}%</div>
            <p className="text-xs text-muted-foreground">{(metrics.approvedTransactions ?? 0).toLocaleString()} aprovadas</p>
          </CardContent>
        </Card>

        {/* Taxa de Fraude */}
        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Taxa de Fraude</CardTitle>
            <AlertCircle className="h-4 w-4 text-red-600" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold text-red-600">{Number(metrics.fraudRate ?? 0).toFixed(2)}%</div>
            <p className="text-xs text-muted-foreground">{(metrics.fraudTransactions ?? 0).toLocaleString()} fraudes</p>
          </CardContent>
        </Card>

        {/* Taxa de Suspeita */}
        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Taxa de Suspeita</CardTitle>
            <Clock className="h-4 w-4 text-amber-600" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold text-amber-600">{Number(metrics.suspiciousRate ?? 0).toFixed(2)}%</div>
            <p className="text-xs text-muted-foreground">{(metrics.suspiciousTransactions ?? 0).toLocaleString()} suspeitas</p>
          </CardContent>
        </Card>
      </div>

      {/* Charts */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Distribuição de Classificações */}
        <Card>
          <CardHeader>
            <CardTitle>Distribuição de Classificações</CardTitle>
            <CardDescription>Proporção de transações por classificação</CardDescription>
          </CardHeader>
          <CardContent>
            <ResponsiveContainer width="100%" height={300}>
              <PieChart>
                <Pie
                  data={classificationData}
                  cx="50%"
                  cy="50%"
                  labelLine={false}
                  label={({ name, value, percent }) => `${name}: ${value} (${(percent * 100).toFixed(0)}%)`}
                  outerRadius={80}
                  fill="#8884d8"
                  dataKey="value"
                >
                  {classificationData.map((entry, index) => (
                    <Cell key={`cell-${index}`} fill={entry.fill} />
                  ))}
                </Pie>
                <Tooltip />
              </PieChart>
            </ResponsiveContainer>
          </CardContent>
        </Card>

        {/* Tendência Temporal */}
        <Card>
          <CardHeader>
            <CardTitle>Tendência Temporal</CardTitle>
            <CardDescription>
              {period === '30d' || period === '7d' ? 'Transações por dia' : 'Transações por hora'}
            </CardDescription>
          </CardHeader>
          <CardContent>
            {timelineData.length === 0 ? (
              <div className="flex items-center justify-center h-[300px] text-muted-foreground">
                <p>Sem dados de timeline para este período</p>
              </div>
            ) : (
              <ResponsiveContainer width="100%" height={300}>
                <LineChart data={timelineData}>
                  <CartesianGrid strokeDasharray="3 3" />
                  <XAxis dataKey="time" />
                  <YAxis />
                  <Tooltip />
                  <Legend />
                  <Line type="monotone" dataKey="approved" stroke="#10b981" name="Aprovadas" />
                  <Line type="monotone" dataKey="fraud" stroke="#ef4444" name="Fraudes" />
                </LineChart>
              </ResponsiveContainer>
            )}
          </CardContent>
        </Card>
      </div>

      {/* Informações Adicionais */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
        <Card>
          <CardHeader>
            <CardTitle>Volume Total</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="text-3xl font-bold">
              R$ {(metrics.totalVolume ?? 0).toLocaleString('pt-BR', { minimumFractionDigits: 2 })}
            </div>
            <p className="text-sm text-muted-foreground mt-2">
              Ticket médio: R$ {(metrics.averageTransactionAmount ?? 0).toLocaleString('pt-BR', { minimumFractionDigits: 2 })}
            </p>
          </CardContent>
        </Card>

        <Card>
          <CardHeader>
            <CardTitle>Informações do Período</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="space-y-2">
              <div className="flex items-center justify-between">
                <span className="text-sm text-muted-foreground">Período selecionado</span>
                <span className="text-sm font-medium">{period}</span>
              </div>
              <div className="flex items-center justify-between">
                <span className="text-sm text-muted-foreground">Maior transação</span>
                <span className="text-sm font-medium">
                  R$ {(metrics.highestTransactionAmount ?? 0).toLocaleString('pt-BR', { minimumFractionDigits: 2 })}
                </span>
              </div>
              <div className="flex items-center justify-between">
                <span className="text-sm text-muted-foreground">Última atualização</span>
                <span className="text-xs text-muted-foreground">
                  {metrics.timestamp ? new Date(metrics.timestamp).toLocaleTimeString('pt-BR') : '-'}
                </span>
              </div>
            </div>
          </CardContent>
        </Card>
      </div>
    </div>
  );
}
