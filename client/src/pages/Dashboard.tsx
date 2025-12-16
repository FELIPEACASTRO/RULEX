import { useEffect, useState } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { LineChart, Line, BarChart, Bar, PieChart, Pie, Cell, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer } from 'recharts';
import { TrendingUp, TrendingDown, AlertCircle, CheckCircle, Clock } from 'lucide-react';

interface MetricsData {
  totalTransactions: number;
  approvedTransactions: number;
  suspiciousTransactions: number;
  fraudTransactions: number;
  approvalRate: number;
  fraudRate: number;
  suspiciousRate: number;
  totalVolume: number;
  averageTransactionAmount: number;
  period: string;
  timestamp: string;
}

/**
 * Dashboard com métricas em tempo real.
 */
export default function Dashboard() {
  const [metrics, setMetrics] = useState<MetricsData | null>(null);
  const [loading, setLoading] = useState(true);
  const [period, setPeriod] = useState('24h');

  useEffect(() => {
    fetchMetrics();
    const interval = setInterval(fetchMetrics, 30000); // Atualizar a cada 30 segundos
    return () => clearInterval(interval);
  }, [period]);

  const fetchMetrics = async () => {
    try {
      const response = await fetch(`/api/metrics?period=${period}`);
      const data = await response.json();
      setMetrics(data);
    } catch (error) {
      console.error('Erro ao buscar métricas:', error);
    } finally {
      setLoading(false);
    }
  };

  if (loading || !metrics) {
    return (
      <div className="flex items-center justify-center h-screen">
        <div className="text-center">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-primary mx-auto mb-4"></div>
          <p className="text-muted-foreground">Carregando métricas...</p>
        </div>
      </div>
    );
  }

  const classificationData = [
    { name: 'Aprovadas', value: metrics.approvedTransactions, fill: '#10b981' },
    { name: 'Suspeitas', value: metrics.suspiciousTransactions, fill: '#f59e0b' },
    { name: 'Fraudes', value: metrics.fraudTransactions, fill: '#ef4444' },
  ];

  const timelineData = [
    { time: '00:00', approved: 120, suspicious: 12, fraud: 3 },
    { time: '04:00', approved: 145, suspicious: 15, fraud: 4 },
    { time: '08:00', approved: 200, suspicious: 20, fraud: 5 },
    { time: '12:00', approved: 250, suspicious: 25, fraud: 6 },
    { time: '16:00', approved: 180, suspicious: 18, fraud: 4 },
    { time: '20:00', approved: 160, suspicious: 16, fraud: 4 },
  ];

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
          onChange={(e) => setPeriod(e.target.value)}
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
            <div className="text-2xl font-bold">{metrics.totalTransactions.toLocaleString()}</div>
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
            <div className="text-2xl font-bold">{metrics.approvalRate.toFixed(2)}%</div>
            <p className="text-xs text-muted-foreground">{metrics.approvedTransactions.toLocaleString()} aprovadas</p>
          </CardContent>
        </Card>

        {/* Taxa de Fraude */}
        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Taxa de Fraude</CardTitle>
            <AlertCircle className="h-4 w-4 text-red-600" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold text-red-600">{metrics.fraudRate.toFixed(2)}%</div>
            <p className="text-xs text-muted-foreground">{metrics.fraudTransactions.toLocaleString()} fraudes</p>
          </CardContent>
        </Card>

        {/* Taxa de Suspeita */}
        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Taxa de Suspeita</CardTitle>
            <Clock className="h-4 w-4 text-amber-600" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold text-amber-600">{metrics.suspiciousRate.toFixed(2)}%</div>
            <p className="text-xs text-muted-foreground">{metrics.suspiciousTransactions.toLocaleString()} suspeitas</p>
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
            <CardDescription>Transações por hora</CardDescription>
          </CardHeader>
          <CardContent>
            <ResponsiveContainer width="100%" height={300}>
              <LineChart data={timelineData}>
                <CartesianGrid strokeDasharray="3 3" />
                <XAxis dataKey="time" />
                <YAxis />
                <Tooltip />
                <Legend />
                <Line type="monotone" dataKey="approved" stroke="#10b981" name="Aprovadas" />
                <Line type="monotone" dataKey="suspicious" stroke="#f59e0b" name="Suspeitas" />
                <Line type="monotone" dataKey="fraud" stroke="#ef4444" name="Fraudes" />
              </LineChart>
            </ResponsiveContainer>
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
            <div className="text-3xl font-bold">R$ {metrics.totalVolume?.toLocaleString('pt-BR', { minimumFractionDigits: 2 })}</div>
            <p className="text-sm text-muted-foreground mt-2">
              Ticket médio: R$ {metrics.averageTransactionAmount?.toLocaleString('pt-BR', { minimumFractionDigits: 2 })}
            </p>
          </CardContent>
        </Card>

        <Card>
          <CardHeader>
            <CardTitle>Status do Sistema</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="space-y-2">
              <div className="flex items-center justify-between">
                <span className="text-sm text-muted-foreground">Motor de Regras</span>
                <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
                  Ativo
                </span>
              </div>
              <div className="flex items-center justify-between">
                <span className="text-sm text-muted-foreground">Banco de Dados</span>
                <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
                  Conectado
                </span>
              </div>
              <div className="flex items-center justify-between">
                <span className="text-sm text-muted-foreground">Última atualização</span>
                <span className="text-xs text-muted-foreground">{new Date(metrics.timestamp).toLocaleTimeString('pt-BR')}</span>
              </div>
            </div>
          </CardContent>
        </Card>
      </div>
    </div>
  );
}
