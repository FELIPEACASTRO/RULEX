import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Activity, AlertTriangle, CheckCircle, Clock, TrendingUp, Zap, RefreshCw } from "lucide-react";
import { Button } from "@/components/ui/button";
import { useEffect, useState, useCallback } from "react";
import { getDashboardMetrics, checkApiHealth, getMetricsTimeline } from "@/lib/javaApi";
import type { DashboardMetrics, MetricsTimeline } from "@/lib/javaApi";

interface SystemHealth {
  apiGateway: "online" | "offline" | "loading";
  ruleEngine: "online" | "offline" | "loading";
  database: "online" | "offline" | "loading";
  cache: "online" | "offline" | "loading";
  responseTimeMs: number;
}

export default function Monitoring() {
  const [metrics, setMetrics] = useState<DashboardMetrics | null>(null);
  const [timeline, setTimeline] = useState<MetricsTimeline | null>(null);
  const [health, setHealth] = useState<SystemHealth>({
    apiGateway: "loading",
    ruleEngine: "loading",
    database: "loading",
    cache: "loading",
    responseTimeMs: 0,
  });
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [lastUpdate, setLastUpdate] = useState<Date | null>(null);

  const fetchData = useCallback(async () => {
    setIsLoading(true);
    setError(null);

    try {
      // Fetch health check
      const healthResult = await checkApiHealth();

      // If API is up, fetch metrics
      if (healthResult.status === "UP") {
        const [metricsData, timelineData] = await Promise.all([
          getDashboardMetrics("24h"),
          getMetricsTimeline("hour"),
        ]);

        setMetrics(metricsData);
        setTimeline(timelineData);
        setHealth({
          apiGateway: "online",
          ruleEngine: "online",
          database: "online",
          cache: "online",
          responseTimeMs: healthResult.responseTime,
        });
      } else {
        setHealth({
          apiGateway: "offline",
          ruleEngine: "offline",
          database: "offline",
          cache: "offline",
          responseTimeMs: healthResult.responseTime,
        });
        setError("API está offline");
      }

      setLastUpdate(new Date());
    } catch (err) {
      console.error("Erro ao carregar dados de monitoramento:", err);
      setError("Erro ao conectar com o servidor");
      setHealth({
        apiGateway: "offline",
        ruleEngine: "offline",
        database: "offline",
        cache: "offline",
        responseTimeMs: 0,
      });
    } finally {
      setIsLoading(false);
    }
  }, []);

  useEffect(() => {
    fetchData();

    // Auto-refresh a cada 30 segundos
    const interval = setInterval(fetchData, 30000);
    return () => clearInterval(interval);
  }, [fetchData]);

  const formatNumber = (num: number | undefined): string => {
    if (num === undefined || num === null) return "0";
    return new Intl.NumberFormat("pt-BR").format(num);
  };

  const formatPercentage = (num: number | undefined): string => {
    if (num === undefined || num === null) return "0.00%";
    return `${num.toFixed(2)}%`;
  };

  const getStatusColor = (status: "online" | "offline" | "loading"): string => {
    switch (status) {
      case "online":
        return "text-green-500";
      case "offline":
        return "text-red-500";
      default:
        return "text-yellow-500";
    }
  };

  const getStatusIcon = (status: "online" | "offline" | "loading") => {
    switch (status) {
      case "online":
        return <CheckCircle className="h-4 w-4 text-green-500" />;
      case "offline":
        return <AlertTriangle className="h-4 w-4 text-red-500" />;
      default:
        return <RefreshCw className="h-4 w-4 text-yellow-500 animate-spin" />;
    }
  };

  const systemStatus = health.apiGateway === "online" ? "Operacional" : "Indisponível";
  const statusColor = health.apiGateway === "online" ? "text-green-500" : "text-red-500";

  // Calcular alertas baseado nos dados reais
  const activeAlerts = metrics
    ? (metrics.fraudTransactions > 0 ? 1 : 0) +
      (metrics.suspiciousTransactions > 10 ? 1 : 0) +
      (health.responseTimeMs > 1000 ? 1 : 0)
    : 0;

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-3xl font-bold tracking-tight">Monitoramento</h1>
          <p className="text-muted-foreground">
            Acompanhe o status e desempenho do sistema em tempo real
          </p>
        </div>
        <div className="flex items-center gap-4">
          {lastUpdate && (
            <span className="text-sm text-muted-foreground">
              Última atualização: {lastUpdate.toLocaleTimeString("pt-BR")}
            </span>
          )}
          <Button variant="outline" size="sm" onClick={fetchData} disabled={isLoading}>
            <RefreshCw className={`h-4 w-4 mr-2 ${isLoading ? "animate-spin" : ""}`} />
            Atualizar
          </Button>
        </div>
      </div>

      {error && (
        <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded">
          {error}
        </div>
      )}

      {/* Status Cards */}
      <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-4">
        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Status do Sistema</CardTitle>
            {health.apiGateway === "online" ? (
              <CheckCircle className="h-4 w-4 text-green-500" />
            ) : (
              <AlertTriangle className="h-4 w-4 text-red-500" />
            )}
          </CardHeader>
          <CardContent>
            <div className={`text-2xl font-bold ${statusColor}`}>{systemStatus}</div>
            <p className="text-xs text-muted-foreground">
              {health.apiGateway === "online"
                ? "Todos os serviços funcionando"
                : "Verifique a conexão com o servidor"}
            </p>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Transações (24h)</CardTitle>
            <Activity className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">
              {isLoading ? "..." : formatNumber(metrics?.totalTransactions)}
            </div>
            <p className="text-xs text-muted-foreground">
              {metrics?.approvedTransactions
                ? `${formatNumber(metrics.approvedTransactions)} aprovadas`
                : "Carregando..."}
            </p>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Tempo de Resposta</CardTitle>
            <Clock className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">
              {health.responseTimeMs > 0 ? `${health.responseTimeMs}ms` : "..."}
            </div>
            <p className="text-xs text-muted-foreground">
              {health.responseTimeMs < 200
                ? "Excelente"
                : health.responseTimeMs < 500
                  ? "Bom"
                  : health.responseTimeMs < 1000
                    ? "Aceitável"
                    : "Lento"}
            </p>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Alertas Ativos</CardTitle>
            <AlertTriangle className={activeAlerts > 0 ? "h-4 w-4 text-yellow-500" : "h-4 w-4 text-muted-foreground"} />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{activeAlerts}</div>
            <p className="text-xs text-muted-foreground">
              {activeAlerts === 0 ? "Nenhum alerta" : `${activeAlerts} alerta(s) requerem atenção`}
            </p>
          </CardContent>
        </Card>
      </div>

      {/* Performance Section */}
      <div className="grid gap-4 md:grid-cols-2">
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <TrendingUp className="h-5 w-5" />
              Desempenho das Regras
            </CardTitle>
            <CardDescription>
              Métricas de execução das regras de fraude (últimas 24h)
            </CardDescription>
          </CardHeader>
          <CardContent>
            <div className="space-y-4">
              <div className="flex items-center justify-between">
                <span className="text-sm">Total de transações analisadas</span>
                <span className="font-bold">
                  {isLoading ? "..." : formatNumber(metrics?.totalTransactions)}
                </span>
              </div>
              <div className="flex items-center justify-between">
                <span className="text-sm">Taxa de detecção (fraude)</span>
                <span className="font-bold text-red-500">
                  {isLoading ? "..." : formatPercentage(metrics?.fraudRate)}
                </span>
              </div>
              <div className="flex items-center justify-between">
                <span className="text-sm">Taxa suspeitas</span>
                <span className="font-bold text-yellow-500">
                  {isLoading ? "..." : formatPercentage(metrics?.suspiciousRate)}
                </span>
              </div>
              <div className="flex items-center justify-between">
                <span className="text-sm">Taxa de aprovação</span>
                <span className="font-bold text-green-500">
                  {isLoading ? "..." : formatPercentage(metrics?.approvalRate)}
                </span>
              </div>
              <div className="flex items-center justify-between">
                <span className="text-sm">Tempo médio de resposta</span>
                <span className="font-bold">
                  {health.responseTimeMs > 0 ? `${health.responseTimeMs}ms` : "..."}
                </span>
              </div>
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <Zap className="h-5 w-5" />
              Saúde do Sistema
            </CardTitle>
            <CardDescription>Status dos componentes principais</CardDescription>
          </CardHeader>
          <CardContent>
            <div className="space-y-4">
              <div className="flex items-center justify-between">
                <span className="text-sm">API Gateway</span>
                <span className={`flex items-center gap-1 ${getStatusColor(health.apiGateway)}`}>
                  {getStatusIcon(health.apiGateway)}{" "}
                  {health.apiGateway === "online" ? "Online" : health.apiGateway === "offline" ? "Offline" : "Verificando..."}
                </span>
              </div>
              <div className="flex items-center justify-between">
                <span className="text-sm">Motor de Regras</span>
                <span className={`flex items-center gap-1 ${getStatusColor(health.ruleEngine)}`}>
                  {getStatusIcon(health.ruleEngine)}{" "}
                  {health.ruleEngine === "online" ? "Online" : health.ruleEngine === "offline" ? "Offline" : "Verificando..."}
                </span>
              </div>
              <div className="flex items-center justify-between">
                <span className="text-sm">Banco de Dados</span>
                <span className={`flex items-center gap-1 ${getStatusColor(health.database)}`}>
                  {getStatusIcon(health.database)}{" "}
                  {health.database === "online" ? "Online" : health.database === "offline" ? "Offline" : "Verificando..."}
                </span>
              </div>
              <div className="flex items-center justify-between">
                <span className="text-sm">Cache (Redis)</span>
                <span className={`flex items-center gap-1 ${getStatusColor(health.cache)}`}>
                  {getStatusIcon(health.cache)}{" "}
                  {health.cache === "online" ? "Online" : health.cache === "offline" ? "Offline" : "Verificando..."}
                </span>
              </div>
            </div>
          </CardContent>
        </Card>
      </div>

      {/* Timeline Chart */}
      {timeline && timeline.buckets.length > 0 && (
        <Card>
          <CardHeader>
            <CardTitle>Atividade nas Últimas 24 Horas</CardTitle>
            <CardDescription>Transações por hora</CardDescription>
          </CardHeader>
          <CardContent>
            <div className="space-y-2">
              {timeline.buckets.slice(-12).map((bucket, index) => {
                const maxTotal = Math.max(...timeline.buckets.map((b) => b.total), 1);
                const width = (bucket.total / maxTotal) * 100;
                const fraudWidth = bucket.total > 0 ? (bucket.fraud / bucket.total) * 100 : 0;
                const time = new Date(bucket.bucket).toLocaleTimeString("pt-BR", {
                  hour: "2-digit",
                  minute: "2-digit",
                });

                return (
                  <div key={index} className="flex items-center gap-2">
                    <span className="text-xs text-muted-foreground w-12">{time}</span>
                    <div className="flex-1 h-6 bg-muted rounded overflow-hidden">
                      <div
                        className="h-full bg-green-500 relative"
                        style={{ width: `${width}%` }}
                      >
                        {fraudWidth > 0 && (
                          <div
                            className="absolute right-0 top-0 h-full bg-red-500"
                            style={{ width: `${fraudWidth}%` }}
                          />
                        )}
                      </div>
                    </div>
                    <span className="text-xs text-muted-foreground w-16 text-right">
                      {bucket.total} tx
                    </span>
                  </div>
                );
              })}
            </div>
            <div className="flex items-center gap-4 mt-4 text-xs text-muted-foreground">
              <div className="flex items-center gap-1">
                <div className="w-3 h-3 bg-green-500 rounded" />
                <span>Aprovadas</span>
              </div>
              <div className="flex items-center gap-1">
                <div className="w-3 h-3 bg-red-500 rounded" />
                <span>Fraude</span>
              </div>
            </div>
          </CardContent>
        </Card>
      )}
    </div>
  );
}
