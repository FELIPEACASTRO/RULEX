import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Badge } from "@/components/ui/badge";

export function ComplexRulesGuide() {
  return (
    <div className="space-y-6">
      <Card>
        <CardHeader>
          <CardTitle>🧩 Regras Complexas</CardTitle>
          <CardDescription>
            Guia do modo “complex” (árvore/grupos) e como ele aparece no RULEX
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="flex flex-wrap gap-2">
            <Badge variant="secondary">UI: /rules (lista unificada)</Badge>
            <Badge variant="secondary">API: /api/complex-rules</Badge>
            <Badge variant="secondary">Modelo: ConditionGroup/Condition</Badge>
          </div>

          <p className="text-sm text-muted-foreground">
            O RULEX possui dois “tipos” de regra na UI: <strong>simple</strong> (regra simples)
            e <strong>complex</strong> (regra com árvore de condições, grupos e validação por AST).
            Na página de Regras, ambas são combinadas em uma lista unificada e filtráveis por tipo.
          </p>
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle>Onde fica no frontend</CardTitle>
          <CardDescription>
            Pontos de referência reais no código do client.
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-3">
          <ul className="list-disc pl-5 space-y-2 text-sm">
            <li>
              Página principal de Regras (lista simples + complexas):
              <code className="ml-2">client/src/pages/Rules.tsx</code>
            </li>
            <li>
              Construtor de regras complexas:
              <code className="ml-2">client/src/components/ComplexRuleBuilder/</code>
            </li>
            <li>
              Tipos do builder:
              <code className="ml-2">client/src/components/ComplexRuleBuilder/types.ts</code>
            </li>
            <li>
              Teste do builder:
              <code className="ml-2">client/src/components/ComplexRuleBuilder/ComplexRuleBuilder.test.tsx</code>
            </li>
            <li>
              Cliente API (inclui listComplexRules):
              <code className="ml-2">client/src/lib/javaApi.ts</code>
            </li>
          </ul>

          <p className="text-sm text-muted-foreground">
            Dica prática: na UI, use o filtro “Tipo: complex” para ver apenas regras complexas.
          </p>
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle>Operadores e segurança (AST allowlist)</CardTitle>
          <CardDescription>
            Regras complexas são validadas com allowlist de funções e operadores.
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-3">
          <p className="text-sm text-muted-foreground">
            O manual é gerado a partir do código fonte e extrai a allowlist do validador AST.
            A checagem automática do manual garante que:
          </p>
          <ul className="list-disc pl-5 space-y-2 text-sm">
            <li>os operadores allowlisted existem no enum de operadores do backend</li>
            <li>aliases apontam para operadores válidos</li>
            <li>todas as funções do ExpressionEvaluator estão na FUNC_ALLOWLIST</li>
          </ul>
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle>Atalho</CardTitle>
        </CardHeader>
        <CardContent>
          <a href="/rules" className="text-primary hover:underline text-sm">
            Abrir a tela de Regras (inclui complex rules)
          </a>
        </CardContent>
      </Card>
    </div>
  );
}
