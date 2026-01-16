/**
 * QaAndE2EGuide.tsx - Guia de QA e Testes E2E do RULEX
 *
 * Documentação completa sobre:
 * - Testes unitários (Vitest)
 * - Testes de integração (JUnit)
 * - Testes E2E (Playwright)
 * - Cobertura e métricas
 */
import { 
  TestTube2, 
  CheckCircle2, 
  XCircle, 
  Play, 
  FileCode,
  Layers,
  Monitor,
  Server,
  AlertTriangle,
  Check
} from "lucide-react";
import { Badge } from "@/components/ui/badge";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table";

export function QaAndE2EGuide() {
  return (
    <div className="space-y-6">
      {/* Header */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <TestTube2 className="h-5 w-5" />
            Guia de QA e Testes
          </CardTitle>
          <CardDescription>
            Estratégia completa de testes: unitários, integração e E2E
          </CardDescription>
        </CardHeader>
      </Card>

      {/* Pirâmide de testes */}
      <Card>
        <CardHeader>
          <CardTitle>Pirâmide de Testes</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="flex flex-col items-center space-y-2">
            <div className="w-32 h-12 bg-purple-500 text-white flex items-center justify-center rounded text-sm">
              E2E (Playwright)
            </div>
            <div className="w-48 h-12 bg-blue-500 text-white flex items-center justify-center rounded text-sm">
              Integração (JUnit + TestContainers)
            </div>
            <div className="w-64 h-12 bg-green-500 text-white flex items-center justify-center rounded text-sm">
              Unitários (Vitest + JUnit)
            </div>
          </div>
          <p className="text-center text-sm text-muted-foreground mt-4">
            Maior quantidade de testes na base (rápidos), menos no topo (lentos mas completos)
          </p>
        </CardContent>
      </Card>

      <Tabs defaultValue="frontend" className="w-full">
        <TabsList className="grid w-full grid-cols-4">
          <TabsTrigger value="frontend">Frontend (Vitest)</TabsTrigger>
          <TabsTrigger value="backend">Backend (JUnit)</TabsTrigger>
          <TabsTrigger value="e2e">E2E (Playwright)</TabsTrigger>
          <TabsTrigger value="ci">CI/CD</TabsTrigger>
        </TabsList>

        {/* Frontend Tests */}
        <TabsContent value="frontend" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                <Monitor className="h-5 w-5" />
                Testes Frontend com Vitest
              </CardTitle>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="grid gap-4 md:grid-cols-2">
                <div className="bg-muted rounded-lg p-4">
                  <h4 className="font-semibold mb-2">Rodar testes</h4>
                  <pre className="text-sm bg-background rounded p-2">
{`# Rodar uma vez
pnpm test --run

# Watch mode
pnpm test

# Com coverage
pnpm test --coverage`}
                  </pre>
                </div>

                <div className="bg-muted rounded-lg p-4">
                  <h4 className="font-semibold mb-2">Arquivo de exemplo</h4>
                  <pre className="text-sm bg-background rounded p-2">
{`// Manual.test.tsx
import { render, screen } from '@testing-library/react'
import { Manual } from './Manual'

describe('Manual', () => {
  it('renders tabs', () => {
    render(<Manual />)
    expect(screen.getByText('Operadores'))
      .toBeInTheDocument()
  })
})`}
                  </pre>
                </div>
              </div>

              <div className="rounded-md border">
                <Table>
                  <TableHeader>
                    <TableRow>
                      <TableHead>Arquivo</TableHead>
                      <TableHead>O que testa</TableHead>
                      <TableHead>Localização</TableHead>
                    </TableRow>
                  </TableHeader>
                  <TableBody>
                    <TableRow>
                      <TableCell><code>Manual.test.tsx</code></TableCell>
                      <TableCell>Tabs, navegação, busca</TableCell>
                      <TableCell className="text-muted-foreground">client/src/pages/</TableCell>
                    </TableRow>
                    <TableRow>
                      <TableCell><code>operators.test.ts</code></TableCell>
                      <TableCell>Lista de operadores FE</TableCell>
                      <TableCell className="text-muted-foreground">client/src/lib/</TableCell>
                    </TableRow>
                    <TableRow>
                      <TableCell><code>RuleFormDialog.test.tsx</code></TableCell>
                      <TableCell>Formulário de regras</TableCell>
                      <TableCell className="text-muted-foreground">client/src/components/</TableCell>
                    </TableRow>
                    <TableRow>
                      <TableCell><code>javaApi.test.ts</code></TableCell>
                      <TableCell>Cliente API (mocks)</TableCell>
                      <TableCell className="text-muted-foreground">client/src/lib/</TableCell>
                    </TableRow>
                  </TableBody>
                </Table>
              </div>
            </CardContent>
          </Card>

          <Card className="border-green-500">
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                <Check className="h-5 w-5 text-green-500" />
                Boas práticas Vitest
              </CardTitle>
            </CardHeader>
            <CardContent>
              <ul className="space-y-2 text-sm">
                <li className="flex items-start gap-2">
                  <CheckCircle2 className="h-4 w-4 text-green-500 mt-0.5 flex-shrink-0" />
                  <span>Use <code>@testing-library/react</code> para testar como usuário interage</span>
                </li>
                <li className="flex items-start gap-2">
                  <CheckCircle2 className="h-4 w-4 text-green-500 mt-0.5 flex-shrink-0" />
                  <span>Mock APIs com <code>vi.mock()</code> ou <code>msw</code></span>
                </li>
                <li className="flex items-start gap-2">
                  <CheckCircle2 className="h-4 w-4 text-green-500 mt-0.5 flex-shrink-0" />
                  <span>Agrupe testes relacionados com <code>describe()</code></span>
                </li>
                <li className="flex items-start gap-2">
                  <CheckCircle2 className="h-4 w-4 text-green-500 mt-0.5 flex-shrink-0" />
                  <span>Use <code>data-testid</code> para elementos difíceis de selecionar</span>
                </li>
              </ul>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Backend Tests */}
        <TabsContent value="backend" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                <Server className="h-5 w-5" />
                Testes Backend com JUnit 5
              </CardTitle>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="grid gap-4 md:grid-cols-2">
                <div className="bg-muted rounded-lg p-4">
                  <h4 className="font-semibold mb-2">Rodar testes</h4>
                  <pre className="text-sm bg-background rounded p-2">
{`# Todos os testes
mvn -f backend/pom.xml test

# Classe específica
mvn -f backend/pom.xml test \\
  -Dtest=RuleServiceTest

# Com coverage (JaCoCo)
mvn -f backend/pom.xml verify`}
                  </pre>
                </div>

                <div className="bg-muted rounded-lg p-4">
                  <h4 className="font-semibold mb-2">Estrutura de teste</h4>
                  <pre className="text-sm bg-background rounded p-2 overflow-x-auto">
{`@SpringBootTest
class RuleServiceTest {
  @Autowired
  private RuleService service;

  @Test
  void shouldEvaluateCondition() {
    var result = service.evaluate(
      condition, payload
    );
    assertThat(result).isTrue();
  }
}`}
                  </pre>
                </div>
              </div>

              <div className="rounded-md border">
                <Table>
                  <TableHeader>
                    <TableRow>
                      <TableHead>Classe</TableHead>
                      <TableHead>O que testa</TableHead>
                      <TableHead>Tipo</TableHead>
                    </TableRow>
                  </TableHeader>
                  <TableBody>
                    <TableRow>
                      <TableCell><code>RuleServiceTest</code></TableCell>
                      <TableCell>Avaliação de regras</TableCell>
                      <TableCell><Badge variant="outline">Unit</Badge></TableCell>
                    </TableRow>
                    <TableRow>
                      <TableCell><code>ExpressionEvaluatorTest</code></TableCell>
                      <TableCell>Funções de expressão</TableCell>
                      <TableCell><Badge variant="outline">Unit</Badge></TableCell>
                    </TableRow>
                    <TableRow>
                      <TableCell><code>AstValidatorTest</code></TableCell>
                      <TableCell>Validação AST V31</TableCell>
                      <TableCell><Badge variant="outline">Unit</Badge></TableCell>
                    </TableRow>
                    <TableRow>
                      <TableCell><code>TransactionControllerIT</code></TableCell>
                      <TableCell>Endpoints REST</TableCell>
                      <TableCell><Badge variant="secondary">Integration</Badge></TableCell>
                    </TableRow>
                    <TableRow>
                      <TableCell><code>RuleRepositoryIT</code></TableCell>
                      <TableCell>Queries JPA</TableCell>
                      <TableCell><Badge variant="secondary">Integration</Badge></TableCell>
                    </TableRow>
                  </TableBody>
                </Table>
              </div>
            </CardContent>
          </Card>

          <Card>
            <CardHeader>
              <CardTitle>TestContainers para Integração</CardTitle>
            </CardHeader>
            <CardContent>
              <p className="text-sm text-muted-foreground mb-4">
                Use TestContainers para testes de integração com PostgreSQL real:
              </p>
              <pre className="bg-muted rounded p-4 text-sm overflow-x-auto">
{`@Testcontainers
@SpringBootTest
class RuleRepositoryIT {
  @Container
  static PostgreSQLContainer<?> postgres = 
    new PostgreSQLContainer<>("postgres:15")
      .withDatabaseName("rulex_test");

  @DynamicPropertySource
  static void props(DynamicPropertyRegistry reg) {
    reg.add("spring.datasource.url", postgres::getJdbcUrl);
    reg.add("spring.datasource.username", postgres::getUsername);
    reg.add("spring.datasource.password", postgres::getPassword);
  }

  @Test
  void shouldPersistRule() { ... }
}`}
              </pre>
            </CardContent>
          </Card>
        </TabsContent>

        {/* E2E Tests */}
        <TabsContent value="e2e" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                <Layers className="h-5 w-5" />
                Testes E2E com Playwright
              </CardTitle>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="grid gap-4 md:grid-cols-2">
                <div className="bg-muted rounded-lg p-4">
                  <h4 className="font-semibold mb-2">Rodar testes</h4>
                  <pre className="text-sm bg-background rounded p-2">
{`# Todos os testes
pnpm exec playwright test

# Com UI
pnpm exec playwright test --ui

# Arquivo específico
pnpm exec playwright test e2e/rules.spec.ts

# Debug mode
pnpm exec playwright test --debug`}
                  </pre>
                </div>

                <div className="bg-muted rounded-lg p-4">
                  <h4 className="font-semibold mb-2">Gerar relatório</h4>
                  <pre className="text-sm bg-background rounded p-2">
{`# Executar com trace
pnpm exec playwright test --trace on

# Abrir relatório HTML
pnpm exec playwright show-report

# Screenshots em falha
# (automático por padrão)`}
                  </pre>
                </div>
              </div>

              <div className="rounded-md border">
                <Table>
                  <TableHeader>
                    <TableRow>
                      <TableHead>Arquivo</TableHead>
                      <TableHead>Cenários</TableHead>
                      <TableHead>Prioridade</TableHead>
                    </TableRow>
                  </TableHeader>
                  <TableBody>
                    <TableRow>
                      <TableCell><code>auth.spec.ts</code></TableCell>
                      <TableCell>Login, logout, sessão expirada</TableCell>
                      <TableCell><Badge variant="destructive">Crítico</Badge></TableCell>
                    </TableRow>
                    <TableRow>
                      <TableCell><code>rules.spec.ts</code></TableCell>
                      <TableCell>CRUD completo de regras</TableCell>
                      <TableCell><Badge variant="destructive">Crítico</Badge></TableCell>
                    </TableRow>
                    <TableRow>
                      <TableCell><code>transactions.spec.ts</code></TableCell>
                      <TableCell>Visualização e filtros</TableCell>
                      <TableCell><Badge variant="secondary">Alto</Badge></TableCell>
                    </TableRow>
                    <TableRow>
                      <TableCell><code>simulation.spec.ts</code></TableCell>
                      <TableCell>Teste de regras ao vivo</TableCell>
                      <TableCell><Badge variant="secondary">Alto</Badge></TableCell>
                    </TableRow>
                    <TableRow>
                      <TableCell><code>manual.spec.ts</code></TableCell>
                      <TableCell>Navegação na documentação</TableCell>
                      <TableCell><Badge variant="outline">Médio</Badge></TableCell>
                    </TableRow>
                  </TableBody>
                </Table>
              </div>
            </CardContent>
          </Card>

          <Card>
            <CardHeader>
              <CardTitle>Exemplo de teste E2E</CardTitle>
            </CardHeader>
            <CardContent>
              <pre className="bg-muted rounded p-4 text-sm overflow-x-auto">
{`// e2e/rules.spec.ts
import { test, expect } from '@playwright/test';

test.describe('Gestão de Regras', () => {
  test.beforeEach(async ({ page }) => {
    // Login
    await page.goto('/login');
    await page.fill('[name="email"]', 'admin');
    await page.fill('[name="password"]', 'admin123');
    await page.click('button[type="submit"]');
    await page.waitForURL('/dashboard');
  });

  test('deve criar nova regra', async ({ page }) => {
    await page.goto('/rules');
    await page.click('text=Nova Regra');
    
    // Preencher formulário
    await page.fill('[name="ruleName"]', 'Teste E2E');
    await page.selectOption('[name="ruleType"]', 'SECURITY');
    
    // Adicionar condição
    await page.click('text=Adicionar Condição');
    await page.selectOption('[name="field"]', 'transactionAmount');
    await page.selectOption('[name="operator"]', 'GREATER_THAN');
    await page.fill('[name="value"]', '10000');
    
    // Salvar
    await page.click('text=Salvar');
    
    // Verificar sucesso
    await expect(page.locator('text=Regra criada')).toBeVisible();
    await expect(page.locator('text=Teste E2E')).toBeVisible();
  });
});`}
              </pre>
            </CardContent>
          </Card>

          <Card className="border-amber-500">
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                <AlertTriangle className="h-5 w-5 text-amber-500" />
                Pré-requisitos E2E
              </CardTitle>
            </CardHeader>
            <CardContent>
              <ul className="space-y-2 text-sm">
                <li className="flex items-start gap-2">
                  <Play className="h-4 w-4 mt-0.5 flex-shrink-0" />
                  <span>Backend deve estar rodando em <code>localhost:8080</code></span>
                </li>
                <li className="flex items-start gap-2">
                  <Play className="h-4 w-4 mt-0.5 flex-shrink-0" />
                  <span>Frontend deve estar rodando em <code>localhost:5173</code></span>
                </li>
                <li className="flex items-start gap-2">
                  <Play className="h-4 w-4 mt-0.5 flex-shrink-0" />
                  <span>Banco de dados com dados de seed (usuário admin)</span>
                </li>
              </ul>
              <pre className="bg-muted rounded p-2 text-sm mt-4">
{`# Subir stack completa para E2E
docker compose up -d --build
pnpm exec playwright test`}
              </pre>
            </CardContent>
          </Card>
        </TabsContent>

        {/* CI/CD */}
        <TabsContent value="ci" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Pipeline CI/CD</CardTitle>
              <CardDescription>Fluxo de integração contínua</CardDescription>
            </CardHeader>
            <CardContent>
              <div className="space-y-4">
                <div className="flex items-center gap-4">
                  <div className="w-8 h-8 rounded-full bg-blue-500 text-white flex items-center justify-center">1</div>
                  <div>
                    <h4 className="font-semibold">Lint & Format</h4>
                    <code className="text-sm text-muted-foreground">pnpm check && mvn spotless:check</code>
                  </div>
                </div>
                <div className="flex items-center gap-4">
                  <div className="w-8 h-8 rounded-full bg-blue-500 text-white flex items-center justify-center">2</div>
                  <div>
                    <h4 className="font-semibold">Build</h4>
                    <code className="text-sm text-muted-foreground">pnpm build && mvn package -DskipTests</code>
                  </div>
                </div>
                <div className="flex items-center gap-4">
                  <div className="w-8 h-8 rounded-full bg-green-500 text-white flex items-center justify-center">3</div>
                  <div>
                    <h4 className="font-semibold">Testes Unitários</h4>
                    <code className="text-sm text-muted-foreground">pnpm test --run && mvn test</code>
                  </div>
                </div>
                <div className="flex items-center gap-4">
                  <div className="w-8 h-8 rounded-full bg-green-500 text-white flex items-center justify-center">4</div>
                  <div>
                    <h4 className="font-semibold">Testes Integração</h4>
                    <code className="text-sm text-muted-foreground">mvn verify (TestContainers)</code>
                  </div>
                </div>
                <div className="flex items-center gap-4">
                  <div className="w-8 h-8 rounded-full bg-purple-500 text-white flex items-center justify-center">5</div>
                  <div>
                    <h4 className="font-semibold">Testes E2E</h4>
                    <code className="text-sm text-muted-foreground">playwright test (com Docker)</code>
                  </div>
                </div>
                <div className="flex items-center gap-4">
                  <div className="w-8 h-8 rounded-full bg-purple-500 text-white flex items-center justify-center">6</div>
                  <div>
                    <h4 className="font-semibold">Deploy</h4>
                    <code className="text-sm text-muted-foreground">docker push + kubernetes apply</code>
                  </div>
                </div>
              </div>
            </CardContent>
          </Card>

          <Card>
            <CardHeader>
              <CardTitle>Comando de Validação Completa</CardTitle>
            </CardHeader>
            <CardContent>
              <p className="text-sm text-muted-foreground mb-4">
                Script para validar tudo antes de merge:
              </p>
              <pre className="bg-muted rounded p-4 text-sm overflow-x-auto">
{`#!/bin/bash
set -e

echo "=== Lint Frontend ==="
pnpm check

echo "=== Lint Backend ==="
mvn -f backend/pom.xml spotless:check

echo "=== Build Frontend ==="
pnpm build

echo "=== Build Backend ==="
mvn -f backend/pom.xml package -DskipTests

echo "=== Tests Frontend ==="
pnpm test --run

echo "=== Tests Backend ==="
mvn -f backend/pom.xml test

echo "=== Manual Check ==="
pnpm manual:check

echo "✅ Tudo passou!"
`}
              </pre>
            </CardContent>
          </Card>

          <Card>
            <CardHeader>
              <CardTitle>Métricas de Qualidade</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="grid gap-4 md:grid-cols-3">
                <div className="bg-muted rounded-lg p-4 text-center">
                  <div className="text-3xl font-bold text-green-600">80%</div>
                  <div className="text-sm text-muted-foreground">Cobertura alvo</div>
                </div>
                <div className="bg-muted rounded-lg p-4 text-center">
                  <div className="text-3xl font-bold text-blue-600">&lt;5min</div>
                  <div className="text-sm text-muted-foreground">Tempo CI máximo</div>
                </div>
                <div className="bg-muted rounded-lg p-4 text-center">
                  <div className="text-3xl font-bold text-purple-600">0</div>
                  <div className="text-sm text-muted-foreground">Falhas permitidas</div>
                </div>
              </div>
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>
    </div>
  );
}
