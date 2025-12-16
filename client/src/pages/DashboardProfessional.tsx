import React, { useState, useEffect } from 'react';
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

/**
 * Dashboard Profissional RULEX
 * Design: Seguro, Tecnológico, Profissional
 * Acessibilidade: WCAG 2.1 AA
 */
export default function DashboardProfessional() {
  const [timeRange, setTimeRange] = useState('24h');
  const [isLoading, setIsLoading] = useState(false);

  // Dados simulados - serão substituídos por chamadas à API
  const metricsData = {
    totalTransactions: 15847,
    approvedRate: 92.3,
    suspiciousRate: 5.2,
    fraudRate: 2.5,
    avgProcessingTime: 145, // ms
    systemUptime: 99.98
  };

  const transactionTrendData = [
    { time: '00:00', approved: 120, suspicious: 8, fraud: 2 },
    { time: '04:00', approved: 95, suspicious: 5, fraud: 1 },
    { time: '08:00', approved: 340, suspicious: 18, fraud: 5 },
    { time: '12:00', approved: 520, suspicious: 28, fraud: 8 },
    { time: '16:00', approved: 480, suspicious: 25, fraud: 7 },
    { time: '20:00', approved: 410, suspicious: 22, fraud: 6 },
    { time: '23:59', approved: 200, suspicious: 10, fraud: 3 }
  ];

  const mccDistributionData = [
    { name: 'Varejo', value: 35, color: '#0052CC' },
    { name: 'Alimentação', value: 25, color: '#10B981' },
    { name: 'Viagem', value: 20, color: '#F59E0B' },
    { name: 'Serviços', value: 15, color: '#8B5CF6' },
    { name: 'Outros', value: 5, color: '#6B7280' }
  ];

  const topMerchantsData = [
    { name: 'Merchant A', transactions: 1240, fraudRate: 0.8 },
    { name: 'Merchant B', transactions: 980, fraudRate: 1.2 },
    { name: 'Merchant C', transactions: 850, fraudRate: 0.5 },
    { name: 'Merchant D', transactions: 720, fraudRate: 2.1 },
    { name: 'Merchant E', transactions: 650, fraudRate: 0.3 }
  ];

  const rulesTriggeredData = [
    { rule: 'VELOCITY_CHECK', count: 145, severity: 'high' },
    { rule: 'EXPIRED_CARD', count: 89, severity: 'critical' },
    { rule: 'UNUSUAL_LOCATION', count: 67, severity: 'medium' },
    { rule: 'LOW_SCORE', count: 54, severity: 'high' },
    { rule: 'DUPLICATE_TRANSACTION', count: 32, severity: 'critical' }
  ];

  const handleRefresh = () => {
    setIsLoading(true);
    setTimeout(() => setIsLoading(false), 1000);
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
          {['1h', '24h', '7d', '30d'].map((range) => (
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
            <div className="flex items-center gap-2 mt-2 text-sm text-green-600">
              <TrendingUp className="w-4 h-4" />
              <span>+12.5% vs período anterior</span>
            </div>
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
            <div className="flex items-center gap-2 mt-2 text-sm text-red-600">
              <TrendingDown className="w-4 h-4" />
              <span>-8.3% vs período anterior</span>
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
                  <span className="text-sm font-medium text-gray-700">Tempo de Processamento</span>
                  <span className="text-sm font-bold text-blue-600">{metricsData.avgProcessingTime}ms</span>
                </div>
                <div className="w-full bg-gray-200 rounded-full h-2">
                  <div className="bg-blue-600 h-2 rounded-full" style={{ width: '45%' }} />
                </div>
              </div>
              <div>
                <div className="flex justify-between mb-2">
                  <span className="text-sm font-medium text-gray-700">Disponibilidade</span>
                  <span className="text-sm font-bold text-green-600">{metricsData.systemUptime}%</span>
                </div>
                <div className="w-full bg-gray-200 rounded-full h-2">
                  <div className="bg-green-600 h-2 rounded-full" style={{ width: '99.98%' }} />
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
              Alertas Recentes
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="space-y-3">
              <div className="flex items-start gap-3 p-3 bg-red-50 rounded-lg border border-red-200">
                <XCircle className="w-5 h-5 text-red-600 mt-0.5 flex-shrink-0" />
                <div className="flex-1">
                  <p className="text-sm font-medium text-red-900">Fraude Detectada</p>
                  <p className="text-xs text-red-700">Transação duplicada bloqueada</p>
                </div>
              </div>
              <div className="flex items-start gap-3 p-3 bg-amber-50 rounded-lg border border-amber-200">
                <AlertCircle className="w-5 h-5 text-amber-600 mt-0.5 flex-shrink-0" />
                <div className="flex-1">
                  <p className="text-sm font-medium text-amber-900">Suspeita de Fraude</p>
                  <p className="text-xs text-amber-700">Velocidade anômala detectada</p>
                </div>
              </div>
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
            <CardDescription>Últimas 24 horas</CardDescription>
          </CardHeader>
          <CardContent>
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
                  dataKey="approved"
                  stroke="#10B981"
                  name="Aprovadas"
                  strokeWidth={2}
                  dot={{ fill: '#10B981', r: 4 }}
                />
                <Line
                  type="monotone"
                  dataKey="suspicious"
                  stroke="#F59E0B"
                  name="Suspeitas"
                  strokeWidth={2}
                  dot={{ fill: '#F59E0B', r: 4 }}
                />
                <Line
                  type="monotone"
                  dataKey="fraud"
                  stroke="#EF4444"
                  name="Fraudes"
                  strokeWidth={2}
                  dot={{ fill: '#EF4444', r: 4 }}
                />
              </LineChart>
            </ResponsiveContainer>
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
            Regras Mais Acionadas
          </CardTitle>
          <CardDescription>Top 5 regras que detectaram fraudes</CardDescription>
        </CardHeader>
        <CardContent>
          <div className="space-y-3">
            {rulesTriggeredData.map((rule, index) => (
              <div key={index} className="flex items-center justify-between p-4 bg-gray-50 rounded-lg border border-gray-200 hover:bg-gray-100 transition-colors">
                <div className="flex-1">
                  <p className="font-medium text-gray-900">{rule.rule}</p>
                  <p className="text-sm text-gray-600">{rule.count} acionamentos</p>
                </div>
                <Badge
                  variant={
                    rule.severity === 'critical'
                      ? 'destructive'
                      : rule.severity === 'high'
                      ? 'default'
                      : 'secondary'
                  }
                >
                  {rule.severity === 'critical' ? 'Crítico' : rule.severity === 'high' ? 'Alto' : 'Médio'}
                </Badge>
              </div>
            ))}
          </div>
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
