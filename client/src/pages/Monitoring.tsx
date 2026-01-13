import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Activity, AlertTriangle, CheckCircle, Clock, TrendingUp, Zap } from "lucide-react";

export default function Monitoring() {
  return (
    <div className="space-y-6">
      <div>
        <h1 className="text-3xl font-bold tracking-tight">Monitoramento</h1>
        <p className="text-muted-foreground">
          Acompanhe o status e desempenho do sistema em tempo real
        </p>
      </div>

      {/* Status Cards */}
      <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-4">
        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Status do Sistema</CardTitle>
            <CheckCircle className="h-4 w-4 text-green-500" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold text-green-500">Operacional</div>
            <p className="text-xs text-muted-foreground">
              Todos os serviços funcionando
            </p>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Transações/min</CardTitle>
            <Activity className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">1,234</div>
            <p className="text-xs text-muted-foreground">
              +12% em relação à última hora
            </p>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Tempo de Resposta</CardTitle>
            <Clock className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">45ms</div>
            <p className="text-xs text-muted-foreground">
              Média dos últimos 5 minutos
            </p>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Alertas Ativos</CardTitle>
            <AlertTriangle className="h-4 w-4 text-yellow-500" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">3</div>
            <p className="text-xs text-muted-foreground">
              2 warnings, 1 info
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
              Métricas de execução das regras de fraude
            </CardDescription>
          </CardHeader>
          <CardContent>
            <div className="space-y-4">
              <div className="flex items-center justify-between">
                <span className="text-sm">Regras executadas hoje</span>
                <span className="font-bold">45,678</span>
              </div>
              <div className="flex items-center justify-between">
                <span className="text-sm">Taxa de detecção</span>
                <span className="font-bold text-green-500">2.3%</span>
              </div>
              <div className="flex items-center justify-between">
                <span className="text-sm">Falsos positivos</span>
                <span className="font-bold text-yellow-500">0.8%</span>
              </div>
              <div className="flex items-center justify-between">
                <span className="text-sm">Tempo médio de processamento</span>
                <span className="font-bold">12ms</span>
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
            <CardDescription>
              Status dos componentes principais
            </CardDescription>
          </CardHeader>
          <CardContent>
            <div className="space-y-4">
              <div className="flex items-center justify-between">
                <span className="text-sm">API Gateway</span>
                <span className="flex items-center gap-1 text-green-500">
                  <CheckCircle className="h-4 w-4" /> Online
                </span>
              </div>
              <div className="flex items-center justify-between">
                <span className="text-sm">Motor de Regras</span>
                <span className="flex items-center gap-1 text-green-500">
                  <CheckCircle className="h-4 w-4" /> Online
                </span>
              </div>
              <div className="flex items-center justify-between">
                <span className="text-sm">Banco de Dados</span>
                <span className="flex items-center gap-1 text-green-500">
                  <CheckCircle className="h-4 w-4" /> Online
                </span>
              </div>
              <div className="flex items-center justify-between">
                <span className="text-sm">Cache</span>
                <span className="flex items-center gap-1 text-green-500">
                  <CheckCircle className="h-4 w-4" /> Online
                </span>
              </div>
            </div>
          </CardContent>
        </Card>
      </div>
    </div>
  );
}
