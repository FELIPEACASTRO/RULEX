import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Badge } from "@/components/ui/badge";

function Cmd({ children }: { children: string }) {
  return (
    <pre className="bg-muted rounded-md p-3 overflow-auto text-sm">
      <code>{children}</code>
    </pre>
  );
}

export function InfraRunbook() {
  return (
    <div className="space-y-6">
      <Card>
        <CardHeader>
          <CardTitle>🧰 Infra/Runbook</CardTitle>
          <CardDescription>
            Comandos e operação do ambiente (baseado em AGENTS.md do repositório)
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="flex flex-wrap gap-2">
            <Badge variant="secondary">Frontend: React 19 + Vite</Badge>
            <Badge variant="secondary">Backend: Spring Boot 3.x</Badge>
            <Badge variant="secondary">DB: Postgres + Flyway</Badge>
            <Badge variant="secondary">E2E: Playwright</Badge>
          </div>
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle>Setup</CardTitle>
        </CardHeader>
        <CardContent className="space-y-3">
          <Cmd>{`pnpm install --frozen-lockfile`}</Cmd>
          <Cmd>{`mvn -q -f backend/pom.xml dependency:resolve`}</Cmd>
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle>Desenvolvimento</CardTitle>
          <CardDescription>
            Você pode subir stack completa via Docker, ou rodar FE/BE separadamente.
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-3">
          <div className="space-y-2">
            <p className="font-medium">Full stack (Docker)</p>
            <Cmd>{`docker compose up -d --build`}</Cmd>
          </div>
          <div className="space-y-2">
            <p className="font-medium">Frontend (dev mode)</p>
            <Cmd>{`pnpm dev`}</Cmd>
          </div>
          <div className="space-y-2">
            <p className="font-medium">Backend (Spring Boot)</p>
            <Cmd>{`cd backend
mvn spring-boot:run`}</Cmd>
          </div>
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle>Testes</CardTitle>
        </CardHeader>
        <CardContent className="space-y-3">
          <div className="space-y-2">
            <p className="font-medium">Vitest (frontend)</p>
            <Cmd>{`pnpm test --run`}</Cmd>
          </div>
          <div className="space-y-2">
            <p className="font-medium">JUnit (backend)</p>
            <Cmd>{`mvn -f backend/pom.xml test`}</Cmd>
          </div>
          <div className="space-y-2">
            <p className="font-medium">Playwright (E2E)</p>
            <Cmd>{`pnpm exec playwright test`}</Cmd>
          </div>
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle>Lint / Formatação</CardTitle>
        </CardHeader>
        <CardContent className="space-y-3">
          <div className="space-y-2">
            <p className="font-medium">Frontend check</p>
            <Cmd>{`pnpm check`}</Cmd>
          </div>
          <div className="space-y-2">
            <p className="font-medium">Backend Spotless</p>
            <Cmd>{`mvn -f backend/pom.xml spotless:check`}</Cmd>
            <Cmd>{`mvn -f backend/pom.xml spotless:apply`}</Cmd>
          </div>
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle>Variáveis de Ambiente (dev)</CardTitle>
        </CardHeader>
        <CardContent className="space-y-3">
          <Cmd>{`# Database
POSTGRES_HOST=localhost
POSTGRES_PORT=5432
POSTGRES_DB=rulex
POSTGRES_USER=rulex
POSTGRES_PASSWORD=rulex

# Backend
SPRING_PROFILES_ACTIVE=dev
SERVER_PORT=8080

# Frontend
VITE_API_URL=http://localhost:8080`}</Cmd>
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle>Troubleshooting</CardTitle>
        </CardHeader>
        <CardContent className="space-y-3">
          <div className="space-y-2">
            <p className="font-medium">Flyway migration</p>
            <Cmd>{`cd backend
mvn flyway:info`}</Cmd>
            <Cmd>{`cd backend
mvn flyway:repair`}</Cmd>
          </div>
          <div className="space-y-2">
            <p className="font-medium">Porta em uso</p>
            <p className="text-sm text-muted-foreground">
              Em Windows, verifique o processo que ocupa a porta e finalize-o.
            </p>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}
