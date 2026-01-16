/**
 * SystemMap.tsx - Mapa do Sistema RULEX
 *
 * Visão geral da arquitetura:
 * - Frontend (React + Vite)
 * - Backend (Spring Boot)
 * - Banco de dados (PostgreSQL)
 * - Fluxo de dados
 * - Componentes principais
 */
import { 
  Server, 
  Database, 
  Layout, 
  FileJson, 
  Cpu, 
  Shield, 
  GitBranch,
  ArrowRight,
  ArrowDown,
  Check,
  Code,
  Layers,
  FolderTree
} from "lucide-react";
import { Badge } from "@/components/ui/badge";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";

export function SystemMap() {
  return (
    <div className="space-y-6">
      {/* Header */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Layers className="h-5 w-5" />
            Mapa do Sistema RULEX
          </CardTitle>
          <CardDescription>
            Arquitetura completa do motor de regras de detecção de fraude
          </CardDescription>
        </CardHeader>
      </Card>

      <Tabs defaultValue="architecture" className="w-full">
        <TabsList className="grid w-full grid-cols-4">
          <TabsTrigger value="architecture">Arquitetura</TabsTrigger>
          <TabsTrigger value="frontend">Frontend</TabsTrigger>
          <TabsTrigger value="backend">Backend</TabsTrigger>
          <TabsTrigger value="dataflow">Fluxo de Dados</TabsTrigger>
        </TabsList>

        {/* Arquitetura Geral */}
        <TabsContent value="architecture" className="space-y-4">
          <div className="grid gap-4 md:grid-cols-3">
            {/* Frontend */}
            <Card className="border-blue-500">
              <CardHeader className="pb-2">
                <CardTitle className="text-lg flex items-center gap-2">
                  <Layout className="h-5 w-5 text-blue-500" />
                  Frontend
                </CardTitle>
              </CardHeader>
              <CardContent className="space-y-2">
                <div className="space-y-1">
                  <Badge variant="outline">React 19</Badge>
                  <Badge variant="outline">TypeScript</Badge>
                  <Badge variant="outline">Vite</Badge>
                  <Badge variant="outline">Tailwind CSS</Badge>
                  <Badge variant="outline">Radix UI</Badge>
                </div>
                <p className="text-sm text-muted-foreground mt-2">
                  SPA responsiva para gestão de regras, visualização de transações e dashboards.
                </p>
                <div className="text-xs font-mono bg-muted rounded p-2 mt-2">
                  client/src/
                </div>
              </CardContent>
            </Card>

            {/* Backend */}
            <Card className="border-green-500">
              <CardHeader className="pb-2">
                <CardTitle className="text-lg flex items-center gap-2">
                  <Server className="h-5 w-5 text-green-500" />
                  Backend
                </CardTitle>
              </CardHeader>
              <CardContent className="space-y-2">
                <div className="space-y-1">
                  <Badge variant="outline">Spring Boot 3.x</Badge>
                  <Badge variant="outline">Java 17+</Badge>
                  <Badge variant="outline">JPA/Hibernate</Badge>
                  <Badge variant="outline">Flyway</Badge>
                </div>
                <p className="text-sm text-muted-foreground mt-2">
                  API REST com motor de regras, validação de transações e análise de fraude.
                </p>
                <div className="text-xs font-mono bg-muted rounded p-2 mt-2">
                  backend/src/main/java/
                </div>
              </CardContent>
            </Card>

            {/* Database */}
            <Card className="border-purple-500">
              <CardHeader className="pb-2">
                <CardTitle className="text-lg flex items-center gap-2">
                  <Database className="h-5 w-5 text-purple-500" />
                  Banco de Dados
                </CardTitle>
              </CardHeader>
              <CardContent className="space-y-2">
                <div className="space-y-1">
                  <Badge variant="outline">PostgreSQL 15+</Badge>
                  <Badge variant="outline">pgcrypto</Badge>
                  <Badge variant="outline">Flyway Migrations</Badge>
                </div>
                <p className="text-sm text-muted-foreground mt-2">
                  Armazena transações, regras, decisões e logs de auditoria.
                </p>
                <div className="text-xs font-mono bg-muted rounded p-2 mt-2">
                  db/migration/V1..V35
                </div>
              </CardContent>
            </Card>
          </div>

          {/* Diagrama de conexão */}
          <Card>
            <CardHeader>
              <CardTitle>Fluxo de Comunicação</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="flex items-center justify-center gap-4 flex-wrap">
                <div className="text-center">
                  <div className="w-24 h-16 border-2 border-blue-500 rounded flex items-center justify-center bg-blue-50 dark:bg-blue-950">
                    <Layout className="h-6 w-6 text-blue-500" />
                  </div>
                  <span className="text-sm mt-1 block">Browser</span>
                </div>
                <ArrowRight className="h-6 w-6 text-muted-foreground" />
                <div className="text-center">
                  <div className="w-24 h-16 border-2 border-blue-500 rounded flex items-center justify-center bg-blue-50 dark:bg-blue-950">
                    <span className="text-xs font-mono">:5173</span>
                  </div>
                  <span className="text-sm mt-1 block">Vite Dev</span>
                </div>
                <ArrowRight className="h-6 w-6 text-muted-foreground" />
                <div className="text-center">
                  <div className="w-24 h-16 border-2 border-green-500 rounded flex items-center justify-center bg-green-50 dark:bg-green-950">
                    <span className="text-xs font-mono">:8080</span>
                  </div>
                  <span className="text-sm mt-1 block">Spring Boot</span>
                </div>
                <ArrowRight className="h-6 w-6 text-muted-foreground" />
                <div className="text-center">
                  <div className="w-24 h-16 border-2 border-purple-500 rounded flex items-center justify-center bg-purple-50 dark:bg-purple-950">
                    <span className="text-xs font-mono">:5432</span>
                  </div>
                  <span className="text-sm mt-1 block">PostgreSQL</span>
                </div>
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Frontend Details */}
        <TabsContent value="frontend" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                <FolderTree className="h-5 w-5" />
                Estrutura do Frontend
              </CardTitle>
            </CardHeader>
            <CardContent>
              <div className="font-mono text-sm space-y-1">
                <div className="text-blue-600 font-semibold">client/src/</div>
                <div className="pl-4">├── components/</div>
                <div className="pl-8 text-muted-foreground">├── ui/              <span className="text-xs"># Radix UI wrappers</span></div>
                <div className="pl-8 text-muted-foreground">├── RuleFormDialog/  <span className="text-xs"># Popup de regras</span></div>
                <div className="pl-8 text-muted-foreground">├── ComplexRuleBuilder/ <span className="text-xs"># Builder avançado</span></div>
                <div className="pl-8 text-muted-foreground">└── ...</div>
                <div className="pl-4">├── pages/</div>
                <div className="pl-8 text-muted-foreground">├── Dashboard.tsx</div>
                <div className="pl-8 text-muted-foreground">├── Rules.tsx        <span className="text-xs"># Gestão de regras</span></div>
                <div className="pl-8 text-muted-foreground">├── Transactions.tsx</div>
                <div className="pl-8 text-muted-foreground">├── Manual.tsx       <span className="text-xs"># Esta documentação</span></div>
                <div className="pl-8 text-muted-foreground">└── ...</div>
                <div className="pl-4">├── lib/</div>
                <div className="pl-8 text-muted-foreground">├── javaApi.ts       <span className="text-xs"># Cliente API (fetch)</span></div>
                <div className="pl-8 text-muted-foreground">├── operators.ts     <span className="text-xs"># Operadores FE</span></div>
                <div className="pl-8 text-muted-foreground">└── utils.ts</div>
                <div className="pl-4">├── manual/</div>
                <div className="pl-8 text-muted-foreground">├── generated/       <span className="text-xs"># Dados extraídos do BE</span></div>
                <div className="pl-8 text-muted-foreground">└── *Catalog.tsx     <span className="text-xs"># Componentes do manual</span></div>
                <div className="pl-4">└── App.tsx</div>
              </div>
            </CardContent>
          </Card>

          <div className="grid gap-4 md:grid-cols-2">
            <Card>
              <CardHeader>
                <CardTitle className="text-base">Páginas Principais</CardTitle>
              </CardHeader>
              <CardContent className="space-y-2">
                <div className="flex justify-between items-center">
                  <code>/dashboard</code>
                  <span className="text-sm text-muted-foreground">Visão geral</span>
                </div>
                <div className="flex justify-between items-center">
                  <code>/rules</code>
                  <span className="text-sm text-muted-foreground">Gestão de regras</span>
                </div>
                <div className="flex justify-between items-center">
                  <code>/transactions</code>
                  <span className="text-sm text-muted-foreground">Lista transações</span>
                </div>
                <div className="flex justify-between items-center">
                  <code>/manual</code>
                  <span className="text-sm text-muted-foreground">Documentação</span>
                </div>
                <div className="flex justify-between items-center">
                  <code>/simulation</code>
                  <span className="text-sm text-muted-foreground">Testar regras</span>
                </div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle className="text-base">Bibliotecas Principais</CardTitle>
              </CardHeader>
              <CardContent className="space-y-2">
                <div className="flex justify-between items-center">
                  <Badge variant="outline">@radix-ui/*</Badge>
                  <span className="text-sm text-muted-foreground">UI Primitives</span>
                </div>
                <div className="flex justify-between items-center">
                  <Badge variant="outline">tailwindcss</Badge>
                  <span className="text-sm text-muted-foreground">Estilos</span>
                </div>
                <div className="flex justify-between items-center">
                  <Badge variant="outline">react-router-dom</Badge>
                  <span className="text-sm text-muted-foreground">Roteamento</span>
                </div>
                <div className="flex justify-between items-center">
                  <Badge variant="outline">lucide-react</Badge>
                  <span className="text-sm text-muted-foreground">Ícones</span>
                </div>
                <div className="flex justify-between items-center">
                  <Badge variant="outline">zod</Badge>
                  <span className="text-sm text-muted-foreground">Validação</span>
                </div>
              </CardContent>
            </Card>
          </div>
        </TabsContent>

        {/* Backend Details */}
        <TabsContent value="backend" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                <FolderTree className="h-5 w-5" />
                Estrutura do Backend
              </CardTitle>
            </CardHeader>
            <CardContent>
              <div className="font-mono text-sm space-y-1">
                <div className="text-green-600 font-semibold">backend/src/main/java/com/rulex/</div>
                <div className="pl-4">├── controller/</div>
                <div className="pl-8 text-muted-foreground">├── RuleController.java</div>
                <div className="pl-8 text-muted-foreground">├── TransactionController.java</div>
                <div className="pl-8 text-muted-foreground">└── AuthController.java</div>
                <div className="pl-4">├── service/</div>
                <div className="pl-8 text-muted-foreground">├── RuleService.java</div>
                <div className="pl-8 text-muted-foreground">├── FraudDetectionService.java</div>
                <div className="pl-8 text-muted-foreground">└── ExpressionEvaluator.java <span className="text-xs"># Motor de expressões</span></div>
                <div className="pl-4">├── entity/</div>
                <div className="pl-8 text-muted-foreground">├── RuleCondition.java <span className="text-xs"># Enum operadores</span></div>
                <div className="pl-8 text-muted-foreground">├── RuleAction.java <span className="text-xs"># Enum ações</span></div>
                <div className="pl-8 text-muted-foreground">└── Transaction.java</div>
                <div className="pl-4">├── repository/</div>
                <div className="pl-8 text-muted-foreground">└── *Repository.java</div>
                <div className="pl-4">├── validation/</div>
                <div className="pl-8 text-muted-foreground">└── AstValidator.java <span className="text-xs"># Validador V31</span></div>
                <div className="pl-4">└── config/</div>
                <div className="pl-8 text-muted-foreground">└── SecurityConfig.java</div>
              </div>
            </CardContent>
          </Card>

          <div className="grid gap-4 md:grid-cols-2">
            <Card>
              <CardHeader>
                <CardTitle className="text-base flex items-center gap-2">
                  <Cpu className="h-4 w-4" />
                  Motor de Regras
                </CardTitle>
              </CardHeader>
              <CardContent className="space-y-3">
                <p className="text-sm text-muted-foreground">
                  O motor de regras avalia transações em tempo real:
                </p>
                <ol className="text-sm space-y-1 list-decimal list-inside">
                  <li>Recebe payload da transação</li>
                  <li>Carrega regras ativas (cache)</li>
                  <li>Avalia condições com operadores</li>
                  <li>Calcula score de risco</li>
                  <li>Executa ações configuradas</li>
                  <li>Retorna decisão final</li>
                </ol>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle className="text-base flex items-center gap-2">
                  <Shield className="h-4 w-4" />
                  Validação AST (V31)
                </CardTitle>
              </CardHeader>
              <CardContent className="space-y-3">
                <p className="text-sm text-muted-foreground">
                  Validador de expressões com allowlist:
                </p>
                <ul className="text-sm space-y-1 list-disc list-inside">
                  <li>Funções: ABS, ROUND, IF, COALESCE...</li>
                  <li>Operadores: +, -, *, /, ==, !=, AND, OR...</li>
                  <li>Bloqueia código malicioso</li>
                  <li>Previne injeção de expressões</li>
                </ul>
              </CardContent>
            </Card>
          </div>
        </TabsContent>

        {/* Data Flow */}
        <TabsContent value="dataflow" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Fluxo de Análise de Transação</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="space-y-6">
                {/* Step 1 */}
                <div className="flex items-start gap-4">
                  <div className="w-8 h-8 rounded-full bg-blue-500 text-white flex items-center justify-center flex-shrink-0">
                    1
                  </div>
                  <div>
                    <h4 className="font-semibold">Recebimento</h4>
                    <p className="text-sm text-muted-foreground">
                      Transação chega via POST /api/transactions/analyze com payload JSON completo
                    </p>
                  </div>
                </div>

                <div className="ml-4">
                  <ArrowDown className="h-6 w-6 text-muted-foreground" />
                </div>

                {/* Step 2 */}
                <div className="flex items-start gap-4">
                  <div className="w-8 h-8 rounded-full bg-blue-500 text-white flex items-center justify-center flex-shrink-0">
                    2
                  </div>
                  <div>
                    <h4 className="font-semibold">Parsing & Validação</h4>
                    <p className="text-sm text-muted-foreground">
                      Payload é validado, normalizado e campos derivados são calculados
                    </p>
                  </div>
                </div>

                <div className="ml-4">
                  <ArrowDown className="h-6 w-6 text-muted-foreground" />
                </div>

                {/* Step 3 */}
                <div className="flex items-start gap-4">
                  <div className="w-8 h-8 rounded-full bg-green-500 text-white flex items-center justify-center flex-shrink-0">
                    3
                  </div>
                  <div>
                    <h4 className="font-semibold">Avaliação de Regras</h4>
                    <p className="text-sm text-muted-foreground">
                      Motor carrega regras ativas e avalia cada condição usando operadores (446+)
                    </p>
                    <div className="mt-2 flex gap-2">
                      <Badge variant="outline">EQUALS</Badge>
                      <Badge variant="outline">GREATER_THAN</Badge>
                      <Badge variant="outline">VELOCITY_*</Badge>
                      <Badge variant="outline">...</Badge>
                    </div>
                  </div>
                </div>

                <div className="ml-4">
                  <ArrowDown className="h-6 w-6 text-muted-foreground" />
                </div>

                {/* Step 4 */}
                <div className="flex items-start gap-4">
                  <div className="w-8 h-8 rounded-full bg-green-500 text-white flex items-center justify-center flex-shrink-0">
                    4
                  </div>
                  <div>
                    <h4 className="font-semibold">Cálculo de Score</h4>
                    <p className="text-sm text-muted-foreground">
                      Regras que "match" contribuem para o score final com seus pesos
                    </p>
                    <pre className="mt-2 bg-muted rounded p-2 text-xs">
                      score = Σ(regra.weight × regra.severity)
                    </pre>
                  </div>
                </div>

                <div className="ml-4">
                  <ArrowDown className="h-6 w-6 text-muted-foreground" />
                </div>

                {/* Step 5 */}
                <div className="flex items-start gap-4">
                  <div className="w-8 h-8 rounded-full bg-purple-500 text-white flex items-center justify-center flex-shrink-0">
                    5
                  </div>
                  <div>
                    <h4 className="font-semibold">Execução de Ações</h4>
                    <p className="text-sm text-muted-foreground">
                      Ações configuradas são executadas (BLOCK, FLAG, LOG, etc.)
                    </p>
                    <div className="mt-2 flex gap-2">
                      <Badge variant="destructive">BLOCK_TRANSACTION</Badge>
                      <Badge variant="secondary">FLAG_FOR_REVIEW</Badge>
                    </div>
                  </div>
                </div>

                <div className="ml-4">
                  <ArrowDown className="h-6 w-6 text-muted-foreground" />
                </div>

                {/* Step 6 */}
                <div className="flex items-start gap-4">
                  <div className="w-8 h-8 rounded-full bg-purple-500 text-white flex items-center justify-center flex-shrink-0">
                    6
                  </div>
                  <div>
                    <h4 className="font-semibold">Decisão & Persistência</h4>
                    <p className="text-sm text-muted-foreground">
                      Decisão final (APPROVED/SUSPICIOUS/FRAUD) é salva e retornada
                    </p>
                    <div className="mt-2 flex gap-2">
                      <Badge className="bg-green-600">APPROVED</Badge>
                      <Badge className="bg-yellow-600">SUSPICIOUS</Badge>
                      <Badge className="bg-red-600">FRAUD</Badge>
                    </div>
                  </div>
                </div>
              </div>
            </CardContent>
          </Card>

          {/* Exemplo de response */}
          <Card>
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                <FileJson className="h-5 w-5" />
                Exemplo de Response
              </CardTitle>
            </CardHeader>
            <CardContent>
              <pre className="bg-muted rounded p-4 text-sm overflow-x-auto">
{`{
  "transactionId": "TXN-2024-001234",
  "decision": "SUSPICIOUS",
  "riskScore": 72,
  "triggeredRules": [
    {
      "ruleName": "HIGH_VALUE_NEW_MERCHANT",
      "contribution": 35,
      "reason": "Valor alto em merchant não conhecido"
    },
    {
      "ruleName": "VELOCITY_5MIN_COUNT",
      "contribution": 37,
      "reason": "5 transações em 5 minutos"
    }
  ],
  "actions": ["FLAG_FOR_REVIEW", "LOG_SUSPICIOUS"],
  "processingTimeMs": 45
}`}
              </pre>
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>

      {/* Comandos úteis */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Code className="h-5 w-5" />
            Comandos de Desenvolvimento
          </CardTitle>
        </CardHeader>
        <CardContent>
          <div className="grid gap-4 md:grid-cols-2">
            <div className="bg-muted rounded-lg p-4">
              <h4 className="font-semibold mb-2 flex items-center gap-2">
                <Layout className="h-4 w-4" /> Frontend
              </h4>
              <pre className="text-sm bg-background rounded p-2 overflow-x-auto">
{`# Instalar deps
pnpm install

# Dev server
pnpm dev

# Build produção
pnpm build

# Testes
pnpm test`}
              </pre>
            </div>

            <div className="bg-muted rounded-lg p-4">
              <h4 className="font-semibold mb-2 flex items-center gap-2">
                <Server className="h-4 w-4" /> Backend
              </h4>
              <pre className="text-sm bg-background rounded p-2 overflow-x-auto">
{`# Build
mvn -f backend/pom.xml package

# Run
mvn -f backend/pom.xml spring-boot:run

# Testes
mvn -f backend/pom.xml test`}
              </pre>
            </div>

            <div className="bg-muted rounded-lg p-4">
              <h4 className="font-semibold mb-2 flex items-center gap-2">
                <GitBranch className="h-4 w-4" /> Docker
              </h4>
              <pre className="text-sm bg-background rounded p-2 overflow-x-auto">
{`# Stack completa
docker compose up -d --build

# Reset total
docker compose down -v
docker compose up -d --build`}
              </pre>
            </div>

            <div className="bg-muted rounded-lg p-4">
              <h4 className="font-semibold mb-2 flex items-center gap-2">
                <Check className="h-4 w-4" /> Lint
              </h4>
              <pre className="text-sm bg-background rounded p-2 overflow-x-auto">
{`# Frontend
pnpm check

# Backend
mvn -f backend/pom.xml spotless:check

# Fix backend
mvn -f backend/pom.xml spotless:apply`}
              </pre>
            </div>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}
