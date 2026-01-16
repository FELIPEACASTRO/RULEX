/**
 * Manual.tsx - Página de documentação completa do RULEX
 *
 * Guia Absoluto do Sistema de Regras de Fraude
 *
 * Features:
 * - 10 tabs (Visão Geral, Fluxo, Payload, Regras, Operadores, Operações, Ações, Exemplos, FAQ, Glossário)
 * - Busca global pesquisando operadores/campos/templates
 * - Estilo didático "Use a Cabeça"
 * - Dados 100% reais do código fonte
 */
import { useEffect, useMemo, useState } from "react";
import { 
  Search, 
  BookOpen, 
  Workflow, 
  Database, 
  FileCode, 
  Calculator, 
  Zap, 
  Target, 
  HelpCircle, 
  BookText,
  Layers,
  FunctionSquare,
  Play,
  Globe,
  TestTube2,
  Terminal,
  Braces
} from "lucide-react";

import { Badge } from "@/components/ui/badge";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Input } from "@/components/ui/input";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table";

import {
  OPERATORS,
  FIELD_LABELS,
  MANUAL_TEMPLATES,
  MANUAL_STATS,
  getOperatorCategories,
  RULE_TYPES,
  CLASSIFICATIONS,
  VALUE_TYPES,
  COMPLEX_LOGIC_OPERATORS,
  NULL_BEHAVIOR_LABELS,
  NULL_BEHAVIOR_DESCRIPTIONS,
  RULE_STATUSES,
  DECISION_TYPES,
  type NullBehavior,
} from "@/manual/manualData";
import { OperatorCatalog } from "@/manual/OperatorCatalog";
import { FieldDictionary } from "@/manual/FieldDictionary";
import { TemplatesGallery } from "@/manual/TemplatesGallery";

// New manual components from generated data
import { 
  ActionsCatalog, 
  FunctionsCatalog, 
  ApiCatalog, 
  DbCatalog, 
  SystemMap, 
  QaAndE2EGuide,
  InfraRunbook,
  ComplexRulesGuide,
} from "@/manual";

// ============================================================================
// TAB: VISÃO GERAL
// ============================================================================
function OverviewTab() {
  const categories = getOperatorCategories();

  return (
    <div className="space-y-6">
      {/* Cards de estatísticas */}
      <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-5">
        <Card>
          <CardHeader className="pb-2">
            <CardTitle className="text-sm font-medium">Operadores</CardTitle>
            <CardDescription>Total disponível</CardDescription>
          </CardHeader>
          <CardContent>
            <div className="text-3xl font-bold text-primary">{MANUAL_STATS.totalOperators}</div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="pb-2">
            <CardTitle className="text-sm font-medium">Campos</CardTitle>
            <CardDescription>Do payload</CardDescription>
          </CardHeader>
          <CardContent>
            <div className="text-3xl font-bold text-primary">{MANUAL_STATS.totalFields}</div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="pb-2">
            <CardTitle className="text-sm font-medium">Categorias</CardTitle>
            <CardDescription>De operadores</CardDescription>
          </CardHeader>
          <CardContent>
            <div className="text-3xl font-bold text-primary">{MANUAL_STATS.totalCategories}</div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="pb-2">
            <CardTitle className="text-sm font-medium">Templates</CardTitle>
            <CardDescription>Pré-definidos</CardDescription>
          </CardHeader>
          <CardContent>
            <div className="text-3xl font-bold text-primary">{MANUAL_STATS.totalTemplates}</div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="pb-2">
            <CardTitle className="text-sm font-medium">Cat. Campos</CardTitle>
            <CardDescription>Agrupamentos</CardDescription>
          </CardHeader>
          <CardContent>
            <div className="text-3xl font-bold text-primary">{MANUAL_STATS.totalFieldCategories}</div>
          </CardContent>
        </Card>
      </div>

      {/* O que é o RULEX */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <BookOpen className="h-5 w-5" />
            O que é o RULEX?
          </CardTitle>
        </CardHeader>
        <CardContent className="space-y-4">
          <p>
            O <strong>RULEX</strong> é um motor de regras de detecção de fraude que permite criar,
            gerenciar e executar regras em tempo real para identificar transações suspeitas.
          </p>
          <div className="grid gap-4 md:grid-cols-3">
            <div className="bg-muted rounded-lg p-4">
              <p className="font-semibold mb-2">🎯 Para que serve?</p>
              <p className="text-sm text-muted-foreground">
                Detectar fraudes em transações financeiras usando regras customizáveis
              </p>
            </div>
            <div className="bg-muted rounded-lg p-4">
              <p className="font-semibold mb-2">👥 Quem usa?</p>
              <p className="text-sm text-muted-foreground">
                Analistas de fraude, gestores de risco, times de compliance
              </p>
            </div>
            <div className="bg-muted rounded-lg p-4">
              <p className="font-semibold mb-2">⚡ Como funciona?</p>
              <p className="text-sm text-muted-foreground">
                Avalia cada transação contra regras configuradas e retorna uma decisão
              </p>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Categorias de operadores */}
      <Card>
        <CardHeader>
          <CardTitle>Categorias de Operadores</CardTitle>
          <CardDescription>
            Os {MANUAL_STATS.totalOperators} operadores estão organizados em {categories.length} categorias
          </CardDescription>
        </CardHeader>
        <CardContent>
          <div className="flex flex-wrap gap-2">
            {categories.map((cat) => (
              <Badge key={cat} variant="outline">
                {cat}
              </Badge>
            ))}
          </div>
        </CardContent>
      </Card>
    </div>
  );
}

// ============================================================================
// TAB: FLUXO
// ============================================================================
function FlowTab() {
  return (
    <div className="space-y-6">
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Workflow className="h-5 w-5" />
            Fluxo de Avaliação de Regras
          </CardTitle>
          <CardDescription>
            Como uma transação é processada pelo motor de regras
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-6">
          {/* Diagrama visual simplificado */}
          <div className="flex flex-col md:flex-row items-center justify-center gap-4 py-4">
            <div className="bg-blue-100 dark:bg-blue-900 rounded-lg p-4 text-center min-w-[150px]">
              <span className="text-2xl">📥</span>
              <p className="font-medium mt-2">1. Transação</p>
              <p className="text-xs text-muted-foreground">Recebe payload</p>
            </div>
            <span className="text-2xl">→</span>
            <div className="bg-yellow-100 dark:bg-yellow-900 rounded-lg p-4 text-center min-w-[150px]">
              <span className="text-2xl">⚙️</span>
              <p className="font-medium mt-2">2. Motor</p>
              <p className="text-xs text-muted-foreground">Avalia regras</p>
            </div>
            <span className="text-2xl">→</span>
            <div className="bg-green-100 dark:bg-green-900 rounded-lg p-4 text-center min-w-[150px]">
              <span className="text-2xl">✅</span>
              <p className="font-medium mt-2">3. Decisão</p>
              <p className="text-xs text-muted-foreground">Aprova/Suspeita/Fraude</p>
            </div>
          </div>

          {/* Detalhes do fluxo */}
          <div className="space-y-4">
            <div className="border-l-4 border-blue-500 pl-4">
              <h4 className="font-semibold">1. Recebimento da Transação</h4>
              <p className="text-sm text-muted-foreground">
                O sistema recebe um payload JSON com todos os dados da transação:
                valor, cartão, merchant, dispositivo, localização, etc.
              </p>
            </div>

            <div className="border-l-4 border-yellow-500 pl-4">
              <h4 className="font-semibold">2. Avaliação pelo Motor de Regras</h4>
              <p className="text-sm text-muted-foreground">
                Cada regra ativa é avaliada em ordem de prioridade. As condições são
                verificadas usando os operadores disponíveis. Se uma condição for verdadeira,
                a ação da regra é executada.
              </p>
            </div>

            <div className="border-l-4 border-green-500 pl-4">
              <h4 className="font-semibold">3. Decisão Final</h4>
              <p className="text-sm text-muted-foreground">
                O sistema retorna uma decisão: APROVADO, SUSPEITA_DE_FRAUDE ou FRAUDE.
                Junto com a decisão, são retornadas as regras que foram acionadas e seus scores.
              </p>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Tipos de Regra */}
      <Card>
        <CardHeader>
          <CardTitle>Tipos de Regra</CardTitle>
          <CardDescription>Categorização das regras no sistema</CardDescription>
        </CardHeader>
        <CardContent>
          <div className="rounded-md border">
            <Table>
              <TableHeader>
                <TableRow>
                  <TableHead>Tipo</TableHead>
                  <TableHead>Label</TableHead>
                  <TableHead>Descrição</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {RULE_TYPES.map((type) => (
                  <TableRow key={type.value}>
                    <TableCell className="font-mono">{type.value}</TableCell>
                    <TableCell>{type.label}</TableCell>
                    <TableCell className="text-muted-foreground text-sm">
                      {type.value === "SECURITY" && "Regras de segurança e verificação de dados"}
                      {type.value === "CONTEXT" && "Regras baseadas em contexto da transação"}
                      {type.value === "VELOCITY" && "Regras de contagem em janelas de tempo"}
                      {type.value === "ANOMALY" && "Regras de detecção de anomalias"}
                    </TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          </div>
        </CardContent>
      </Card>

      {/* Classificações */}
      <Card>
        <CardHeader>
          <CardTitle>Classificações de Decisão</CardTitle>
          <CardDescription>Possíveis resultados da avaliação</CardDescription>
        </CardHeader>
        <CardContent>
          <div className="flex gap-4">
            {CLASSIFICATIONS.map((cls) => (
              <div
                key={cls.value}
                className={`flex-1 rounded-lg p-4 text-center ${
                  cls.value === "APPROVED"
                    ? "bg-green-100 dark:bg-green-900"
                    : cls.value === "SUSPICIOUS"
                    ? "bg-yellow-100 dark:bg-yellow-900"
                    : "bg-red-100 dark:bg-red-900"
                }`}
              >
                <span className="text-2xl">
                  {cls.value === "APPROVED" && "✅"}
                  {cls.value === "SUSPICIOUS" && "⚠️"}
                  {cls.value === "FRAUD" && "🚫"}
                </span>
                <p className="font-semibold mt-2">{cls.label}</p>
                <p className="text-xs text-muted-foreground">{cls.value}</p>
              </div>
            ))}
          </div>
        </CardContent>
      </Card>
    </div>
  );
}

// ============================================================================
// TAB: REGRAS
// ============================================================================
function RulesTab() {
  return (
    <div className="space-y-6">
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <FileCode className="h-5 w-5" />
            Estrutura de uma Regra
          </CardTitle>
          <CardDescription>
            Anatomia completa de uma regra de fraude no RULEX
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="bg-muted rounded-lg p-4 font-mono text-sm overflow-auto">
            <pre>{`{
  "key": "HIGH_AMOUNT_NIGHT",
  "title": "Alto Valor à Noite",
  "description": "Bloqueia transações > R$10.000 entre 00:00 e 06:00",
  "status": "PUBLISHED",
  "priority": 100,
  "severity": "HIGH",
  "decision": "SUSPEITA_DE_FRAUDE",
  "conditions": {
    "logicOperator": "AND",
    "conditions": [
      {
        "fieldName": "transactionAmount",
        "operator": "GT",
        "value": 10000
      },
      {
        "fieldName": "transactionTime",
        "operator": "BETWEEN",
        "value": ["00:00", "06:00"]
      }
    ]
  }
}`}</pre>
          </div>
        </CardContent>
      </Card>

      {/* Status de Regra */}
      <Card>
        <CardHeader>
          <CardTitle>Status de Regra</CardTitle>
          <CardDescription>Ciclo de vida de uma regra</CardDescription>
        </CardHeader>
        <CardContent>
          <div className="grid gap-4 md:grid-cols-4">
            {RULE_STATUSES.map((status) => (
              <div
                key={status.value}
                className="border rounded-lg p-4 text-center"
              >
                <p className="font-mono text-sm mb-2">{status.value}</p>
                <p className="text-sm font-medium">{status.label}</p>
              </div>
            ))}
          </div>
        </CardContent>
      </Card>

      {/* Tipos de Decisão */}
      <Card>
        <CardHeader>
          <CardTitle>Tipos de Decisão</CardTitle>
          <CardDescription>Ações que uma regra pode tomar</CardDescription>
        </CardHeader>
        <CardContent>
          <div className="rounded-md border">
            <Table>
              <TableHeader>
                <TableRow>
                  <TableHead>Código</TableHead>
                  <TableHead>Label</TableHead>
                  <TableHead>Descrição</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {DECISION_TYPES.map((dt) => (
                  <TableRow key={dt.value}>
                    <TableCell className="font-mono">{dt.value}</TableCell>
                    <TableCell>{dt.label}</TableCell>
                    <TableCell className="text-muted-foreground text-sm">
                      {dt.value === 'APROVADO' && 'Transação aprovada sem restrições'}
                      {dt.value === 'SUSPEITA_DE_FRAUDE' && 'Transação marcada para revisão manual'}
                      {dt.value === 'FRAUDE' && 'Transação bloqueada por indicação de fraude'}
                    </TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          </div>
        </CardContent>
      </Card>

      {/* Operadores Lógicos */}
      <Card>
        <CardHeader>
          <CardTitle>Operadores Lógicos</CardTitle>
          <CardDescription>Conectores para combinar condições</CardDescription>
        </CardHeader>
        <CardContent>
          <div className="rounded-md border">
            <Table>
              <TableHeader>
                <TableRow>
                  <TableHead>Código</TableHead>
                  <TableHead>Label</TableHead>
                  <TableHead>Descrição</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {COMPLEX_LOGIC_OPERATORS.map((op) => (
                  <TableRow key={op.value}>
                    <TableCell className="font-mono">{op.value}</TableCell>
                    <TableCell>{op.label}</TableCell>
                    <TableCell className="text-muted-foreground text-sm">
                      {op.description}
                    </TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          </div>
        </CardContent>
      </Card>

      {/* Tipos de Valor */}
      <Card>
        <CardHeader>
          <CardTitle>Tipos de Valor</CardTitle>
          <CardDescription>Tipos de dados suportados nas condições</CardDescription>
        </CardHeader>
        <CardContent>
          <div className="rounded-md border">
            <Table>
              <TableHeader>
                <TableRow>
                  <TableHead>Código</TableHead>
                  <TableHead>Label</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {VALUE_TYPES.map((vt) => (
                  <TableRow key={vt.value}>
                    <TableCell className="font-mono">{vt.value}</TableCell>
                    <TableCell>{vt.label}</TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}

// ============================================================================
// TAB: OPERAÇÕES (NULL BEHAVIOR)
// ============================================================================
function OperationsTab() {
  const nullBehaviors: NullBehavior[] = [
    "returns_false",
    "returns_true",
    "checks_null",
    "context_dependent",
    "not_applicable",
  ];

  return (
    <div className="space-y-6">
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Calculator className="h-5 w-5" />
            Comportamento NULL dos Operadores
          </CardTitle>
          <CardDescription>
            Como os operadores se comportam quando o campo da condição é NULL
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="bg-yellow-50 dark:bg-yellow-950 border border-yellow-200 dark:border-yellow-800 rounded-lg p-4">
            <p className="font-semibold flex items-center gap-2">
              ⚠️ Por que isso importa?
            </p>
            <p className="text-sm mt-2">
              Em transações reais, nem todos os campos estão sempre preenchidos.
              Saber como cada operador lida com valores NULL é crucial para evitar
              falsos positivos ou falsos negativos nas suas regras.
            </p>
          </div>

          <div className="rounded-md border">
            <Table>
              <TableHeader>
                <TableRow>
                  <TableHead>Comportamento</TableHead>
                  <TableHead>Label</TableHead>
                  <TableHead>Descrição</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {nullBehaviors.map((nb) => (
                  <TableRow key={nb}>
                    <TableCell className="font-mono">{nb}</TableCell>
                    <TableCell>{NULL_BEHAVIOR_LABELS[nb]}</TableCell>
                    <TableCell className="text-muted-foreground text-sm">
                      {NULL_BEHAVIOR_DESCRIPTIONS[nb]}
                    </TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          </div>
        </CardContent>
      </Card>

      {/* Exemplos práticos */}
      <Card>
        <CardHeader>
          <CardTitle>Exemplos Práticos de NULL</CardTitle>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="grid gap-4 md:grid-cols-2">
            <div className="bg-red-50 dark:bg-red-950 rounded-lg p-4">
              <h4 className="font-semibold text-red-800 dark:text-red-200">
                NULL → FALSE
              </h4>
              <code className="text-sm block mt-2 bg-background rounded p-2">
                transactionAmount &gt; 100
              </code>
              <p className="text-sm mt-2 text-muted-foreground">
                Se `transactionAmount` for NULL, a condição retorna FALSE.
                A transação não é bloqueada por esta regra.
              </p>
            </div>

            <div className="bg-green-50 dark:bg-green-950 rounded-lg p-4">
              <h4 className="font-semibold text-green-800 dark:text-green-200">
                NULL → TRUE
              </h4>
              <code className="text-sm block mt-2 bg-background rounded p-2">
                mcc NOT_IN [7995, 6211]
              </code>
              <p className="text-sm mt-2 text-muted-foreground">
                Se `mcc` for NULL, a condição retorna TRUE.
                NULL não está na lista, então a condição é satisfeita.
              </p>
            </div>

            <div className="bg-blue-50 dark:bg-blue-950 rounded-lg p-4">
              <h4 className="font-semibold text-blue-800 dark:text-blue-200">
                Verificação de NULL
              </h4>
              <code className="text-sm block mt-2 bg-background rounded p-2">
                cvvResult IS_NULL
              </code>
              <p className="text-sm mt-2 text-muted-foreground">
                Verifica explicitamente se o campo é NULL.
                Útil para detectar dados faltantes.
              </p>
            </div>

            <div className="bg-yellow-50 dark:bg-yellow-950 rounded-lg p-4">
              <h4 className="font-semibold text-yellow-800 dark:text-yellow-200">
                Contexto-Dependente
              </h4>
              <code className="text-sm block mt-2 bg-background rounded p-2">
                VELOCITY_COUNT_24H &gt; 5
              </code>
              <p className="text-sm mt-2 text-muted-foreground">
                Se o Redis não tiver dados, pode retornar 0 ou falhar
                dependendo da configuração.
              </p>
            </div>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}

// ============================================================================
// TAB: AÇÕES (EXEMPLOS DE USO)
// ============================================================================
function ActionsTab({ highlightTemplateId }: { highlightTemplateId?: string }) {
  return (
    <div className="space-y-6">
      <TemplatesGallery highlightTemplateId={highlightTemplateId} />
    </div>
  );
}

// ============================================================================
// TAB: FAQ
// ============================================================================
function FaqTab() {
  const faqs = [
    {
      question: "Como criar uma regra simples?",
      answer:
        "Vá em 'Regras de Fraude' > 'Nova Regra'. Preencha o nome, selecione o tipo, adicione condições usando os campos do payload e operadores, defina a decisão (Aprovar, Suspeita ou Fraude) e salve.",
    },
    {
      question: "Qual a diferença entre AND e OR?",
      answer:
        "AND exige que TODAS as condições sejam verdadeiras. OR exige que PELO MENOS UMA seja verdadeira. Exemplo: 'valor > 1000 AND país = US' só ativa se ambas forem verdadeiras.",
    },
    {
      question: "O que são operadores Velocity?",
      answer:
        "Operadores que contam ou agregam transações em janelas de tempo. Exemplo: 'quantidade de transações do mesmo cartão nas últimas 24h'. São armazenados no Redis para performance.",
    },
    {
      question: "O que são operadores Neo4j?",
      answer:
        "Operadores que consultam um banco de dados de grafos para análise de redes. Úteis para detectar fraudes em anel, conexões suspeitas entre entidades, etc.",
    },
    {
      question: "Como testar uma regra antes de publicar?",
      answer:
        "Use o Simulador. Cole um payload JSON de transação e veja quais regras seriam acionadas, incluindo as em status DRAFT ou TESTING.",
    },
    {
      question: "O que acontece quando um campo é NULL?",
      answer:
        "Depende do operador. A maioria dos operadores de comparação retorna FALSE quando o campo é NULL. Operadores negativos como NOT_IN retornam TRUE. Veja a aba 'Operações' para detalhes.",
    },
    {
      question: "Como priorizar regras?",
      answer:
        "Use o campo 'priority'. Quanto MAIOR o número, MAIS PRIORIDADE. Regras são avaliadas em ordem de prioridade decrescente. A primeira regra de bloqueio que ativar define a decisão.",
    },
    {
      question: "Posso comparar dois campos entre si?",
      answer:
        "Sim! Use operadores como 'FIELD_EQ', 'FIELD_GT', etc. Exemplo: 'merchantCountry FIELD_NE cardCountry' verifica se o país do merchant é diferente do país do cartão.",
    },
  ];

  return (
    <div className="space-y-4">
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <HelpCircle className="h-5 w-5" />
            Perguntas Frequentes
          </CardTitle>
          <CardDescription>
            Dúvidas comuns sobre o uso do RULEX
          </CardDescription>
        </CardHeader>
      </Card>

      {faqs.map((faq, idx) => (
        <Card key={idx}>
          <CardHeader className="pb-2">
            <CardTitle className="text-base">{faq.question}</CardTitle>
          </CardHeader>
          <CardContent>
            <p className="text-sm text-muted-foreground">{faq.answer}</p>
          </CardContent>
        </Card>
      ))}
    </div>
  );
}

// ============================================================================
// TAB: GLOSSÁRIO
// ============================================================================
function GlossaryTab() {
  const terms = [
    { term: "Payload", definition: "Dados JSON da transação enviados para avaliação" },
    { term: "Condição", definition: "Expressão que compara um campo com um valor usando um operador" },
    { term: "Operador", definition: "Função de comparação (EQ, GT, CONTAINS, etc.)" },
    { term: "Regra", definition: "Conjunto de condições que, quando verdadeiras, geram uma decisão" },
    { term: "Decisão", definition: "Resultado da avaliação: APROVADO, SUSPEITA_DE_FRAUDE ou FRAUDE" },
    { term: "Velocity", definition: "Contagem ou agregação de eventos em janela de tempo" },
    { term: "MCC", definition: "Merchant Category Code - código que identifica o tipo de estabelecimento" },
    { term: "BIN", definition: "Bank Identification Number - primeiros 6-8 dígitos do cartão" },
    { term: "3DS", definition: "3D Secure - protocolo de autenticação do titular do cartão" },
    { term: "CAVV", definition: "Cardholder Authentication Verification Value - prova criptográfica do 3DS" },
    { term: "ECI", definition: "Electronic Commerce Indicator - indica resultado da autenticação" },
    { term: "CVV", definition: "Card Verification Value - código de segurança do cartão" },
    { term: "AVS", definition: "Address Verification System - verificação de endereço de cobrança" },
    { term: "POS", definition: "Point of Sale - terminal de pagamento físico" },
    { term: "CNP", definition: "Card Not Present - transação sem cartão físico (e-commerce)" },
    { term: "EMV", definition: "Europay, Mastercard, Visa - padrão de chip de cartão" },
    { term: "AML", definition: "Anti-Money Laundering - prevenção à lavagem de dinheiro" },
    { term: "KYC", definition: "Know Your Customer - verificação de identidade do cliente" },
    { term: "PEP", definition: "Politically Exposed Person - pessoa politicamente exposta" },
    { term: "OFAC", definition: "Office of Foreign Assets Control - lista de sanções dos EUA" },
    { term: "Chargeback", definition: "Contestação de transação pelo titular do cartão" },
    { term: "Fraud Ring", definition: "Grupo organizado de fraudadores que operam em rede" },
    { term: "Card Testing", definition: "Teste de validade de cartões roubados com transações pequenas" },
    { term: "Account Takeover", definition: "Roubo de conta de cliente legítimo" },
    { term: "Synthetic ID", definition: "Identidade falsa criada combinando dados reais e fictícios" },
  ];

  return (
    <div className="space-y-6">
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <BookText className="h-5 w-5" />
            Glossário de Termos
          </CardTitle>
          <CardDescription>
            Definições dos principais termos usados em detecção de fraude
          </CardDescription>
        </CardHeader>
        <CardContent>
          <div className="rounded-md border">
            <Table>
              <TableHeader>
                <TableRow>
                  <TableHead className="w-[200px]">Termo</TableHead>
                  <TableHead>Definição</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {terms.map((item) => (
                  <TableRow key={item.term}>
                    <TableCell className="font-semibold">{item.term}</TableCell>
                    <TableCell className="text-muted-foreground">
                      {item.definition}
                    </TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}

// ============================================================================
// BUSCA GLOBAL
// ============================================================================
interface GlobalSearchResult {
  type: "operator" | "field" | "template";
  value: string;
  label: string;
  description?: string;
}

function GlobalSearch({
  onSelect,
}: {
  onSelect: (result: GlobalSearchResult) => void;
}) {
  const [query, setQuery] = useState("");

  const results = useMemo((): GlobalSearchResult[] => {
    if (!query.trim() || query.length < 2) return [];

    const q = query.toLowerCase();
    const matches: GlobalSearchResult[] = [];

    // Buscar em operadores
    OPERATORS.forEach((op) => {
      const hay = `${op.value} ${op.label} ${op.description || ""}`.toLowerCase();
      if (hay.includes(q)) {
        matches.push({
          type: "operator",
          value: op.value,
          label: op.label,
          description: op.description,
        });
      }
    });

    // Buscar em campos
    Object.entries(FIELD_LABELS).forEach(([field, label]) => {
      const hay = `${field} ${label}`.toLowerCase();
      if (hay.includes(q)) {
        matches.push({
          type: "field",
          value: field,
          label: label,
        });
      }
    });

    // Buscar em templates
    MANUAL_TEMPLATES.forEach((t) => {
      const hay = `${t.id} ${t.name} ${t.description}`.toLowerCase();
      if (hay.includes(q)) {
        matches.push({
          type: "template",
          value: t.id,
          label: t.name,
          description: t.description,
        });
      }
    });

    return matches.slice(0, 20); // Limitar resultados
  }, [query]);

  return (
    <div className="relative">
      <div className="relative">
        <Search className="absolute left-3 top-1/2 -translate-y-1/2 h-4 w-4 text-muted-foreground" />
        <Input
          placeholder="Buscar operadores, campos, templates..."
          className="pl-10"
          value={query}
          onChange={(e) => setQuery(e.target.value)}
        />
      </div>

      {results.length > 0 && (
        <Card className="absolute z-50 w-full mt-2 max-h-[400px] overflow-auto shadow-lg">
          <CardContent className="p-2">
            {results.map((r, idx) => (
              <div
                key={`${r.type}-${r.value}-${idx}`}
                className="flex items-center gap-3 p-2 rounded hover:bg-muted cursor-pointer"
                onClick={() => {
                  onSelect(r);
                  setQuery("");
                }}
                role="button"
                tabIndex={0}
                onKeyDown={(e) => {
                  if (e.key === "Enter" || e.key === " ") {
                    e.preventDefault();
                    onSelect(r);
                    setQuery("");
                  }
                }}
              >
                <Badge
                  variant={
                    r.type === "operator"
                      ? "default"
                      : r.type === "field"
                      ? "secondary"
                      : "outline"
                  }
                  className="text-xs shrink-0"
                >
                  {r.type === "operator" && "Operador"}
                  {r.type === "field" && "Campo"}
                  {r.type === "template" && "Template"}
                </Badge>
                <div className="min-w-0">
                  <p className="font-mono text-sm truncate">{r.value}</p>
                  <p className="text-xs text-muted-foreground truncate">
                    {r.label}
                    {r.description && ` - ${r.description}`}
                  </p>
                </div>
              </div>
            ))}
          </CardContent>
        </Card>
      )}
    </div>
  );
}

// ============================================================================
// COMPONENTE PRINCIPAL
// ============================================================================
export default function Manual() {
  const [activeTab, setActiveTab] = useState("visao-geral");
  const [highlight, setHighlight] = useState<GlobalSearchResult | null>(null);

  useEffect(() => {
    if (!highlight) return;
    const t = setTimeout(() => setHighlight(null), 2000);
    return () => clearTimeout(t);
  }, [highlight]);

  const navigateFromSearch = (r: GlobalSearchResult) => {
    const tab =
      r.type === "operator"
        ? "operadores"
        : r.type === "field"
        ? "payload"
        : "exemplos";

    const anchorId =
      r.type === "operator"
        ? `manual-operator-${r.value}`
        : r.type === "field"
        ? `manual-field-${r.value}`
        : `manual-template-${r.value}`;

    setActiveTab(tab);
    setHighlight(r);

    // Tentar rolar após a troca de aba/render.
    const tryScroll = (attempt = 0) => {
      const el = document.getElementById(anchorId);
      if (el?.scrollIntoView) {
        el.scrollIntoView({ behavior: "smooth", block: "center" });
        return;
      }
      if (attempt < 4) {
        setTimeout(() => tryScroll(attempt + 1), attempt === 0 ? 0 : 80);
      }
    };
    tryScroll();
  };

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex flex-col md:flex-row md:items-center md:justify-between gap-4">
        <div>
          <h1 className="text-3xl font-bold tracking-tight flex items-center gap-2">
            📖 Manual do RULEX
          </h1>
          <p className="text-muted-foreground">
            Guia completo do sistema de detecção de fraude - {MANUAL_STATS.totalOperators} operadores,{" "}
            {MANUAL_STATS.totalFields} campos, {MANUAL_STATS.totalTemplates} templates
          </p>
        </div>
        <div className="w-full md:w-[400px]">
          <GlobalSearch onSelect={navigateFromSearch} />
        </div>
      </div>

      {/* Tabs principais */}
      <Tabs value={activeTab} onValueChange={setActiveTab} className="space-y-4">
        <TabsList className="flex flex-wrap h-auto gap-1">
          <TabsTrigger value="visao-geral" className="gap-2">
            <BookOpen className="h-4 w-4" />
            <span className="hidden sm:inline">Visão Geral</span>
          </TabsTrigger>
          <TabsTrigger value="mapa" className="gap-2">
            <Layers className="h-4 w-4" />
            <span className="hidden sm:inline">Mapa</span>
          </TabsTrigger>
          <TabsTrigger value="infra" className="gap-2">
            <Terminal className="h-4 w-4" />
            <span className="hidden sm:inline">Infra/Runbook</span>
          </TabsTrigger>
          <TabsTrigger value="fluxo" className="gap-2">
            <Workflow className="h-4 w-4" />
            <span className="hidden sm:inline">Fluxo</span>
          </TabsTrigger>
          <TabsTrigger value="payload" className="gap-2">
            <Database className="h-4 w-4" />
            <span className="hidden sm:inline">Payload</span>
          </TabsTrigger>
          <TabsTrigger value="regras" className="gap-2">
            <FileCode className="h-4 w-4" />
            <span className="hidden sm:inline">Regras</span>
          </TabsTrigger>
          <TabsTrigger value="regras-complexas" className="gap-2">
            <Braces className="h-4 w-4" />
            <span className="hidden sm:inline">Regras Complexas</span>
          </TabsTrigger>
          <TabsTrigger value="operadores" className="gap-2">
            <Calculator className="h-4 w-4" />
            <span className="hidden sm:inline">Operadores</span>
          </TabsTrigger>
          <TabsTrigger value="funcoes" className="gap-2">
            <FunctionSquare className="h-4 w-4" />
            <span className="hidden sm:inline">Funções</span>
          </TabsTrigger>
          <TabsTrigger value="acoes" className="gap-2">
            <Play className="h-4 w-4" />
            <span className="hidden sm:inline">Ações</span>
          </TabsTrigger>
          <TabsTrigger value="operacoes" className="gap-2">
            <Zap className="h-4 w-4" />
            <span className="hidden sm:inline">Operações</span>
          </TabsTrigger>
          <TabsTrigger value="api" className="gap-2">
            <Globe className="h-4 w-4" />
            <span className="hidden sm:inline">API</span>
          </TabsTrigger>
          <TabsTrigger value="banco" className="gap-2">
            <Database className="h-4 w-4" />
            <span className="hidden sm:inline">Banco</span>
          </TabsTrigger>
          <TabsTrigger value="exemplos" className="gap-2">
            <Target className="h-4 w-4" />
            <span className="hidden sm:inline">Exemplos</span>
          </TabsTrigger>
          <TabsTrigger value="qa" className="gap-2">
            <TestTube2 className="h-4 w-4" />
            <span className="hidden sm:inline">QA/E2E</span>
          </TabsTrigger>
          <TabsTrigger value="faq" className="gap-2">
            <HelpCircle className="h-4 w-4" />
            <span className="hidden sm:inline">FAQ</span>
          </TabsTrigger>
          <TabsTrigger value="glossario" className="gap-2">
            <BookText className="h-4 w-4" />
            <span className="hidden sm:inline">Glossário</span>
          </TabsTrigger>
        </TabsList>

        <TabsContent value="visao-geral">
          <OverviewTab />
        </TabsContent>

        <TabsContent value="mapa">
          <SystemMap />
        </TabsContent>

        <TabsContent value="infra">
          <InfraRunbook />
        </TabsContent>

        <TabsContent value="fluxo">
          <FlowTab />
        </TabsContent>

        <TabsContent value="payload">
          <FieldDictionary
            highlightField={highlight?.type === "field" ? highlight.value : undefined}
          />
        </TabsContent>

        <TabsContent value="regras">
          <RulesTab />
        </TabsContent>

        <TabsContent value="regras-complexas">
          <ComplexRulesGuide />
        </TabsContent>

        <TabsContent value="operadores">
          <OperatorCatalog
            highlightOperator={
              highlight?.type === "operator" ? highlight.value : undefined
            }
          />
        </TabsContent>

        <TabsContent value="funcoes">
          <FunctionsCatalog />
        </TabsContent>

        <TabsContent value="acoes">
          <ActionsCatalog />
        </TabsContent>

        <TabsContent value="operacoes">
          <OperationsTab />
        </TabsContent>

        <TabsContent value="api">
          <ApiCatalog />
        </TabsContent>

        <TabsContent value="banco">
          <DbCatalog />
        </TabsContent>

        <TabsContent value="exemplos">
          <ActionsTab
            highlightTemplateId={
              highlight?.type === "template" ? highlight.value : undefined
            }
          />
        </TabsContent>

        <TabsContent value="qa">
          <QaAndE2EGuide />
        </TabsContent>

        <TabsContent value="faq">
          <FaqTab />
        </TabsContent>

        <TabsContent value="glossario">
          <GlossaryTab />
        </TabsContent>
      </Tabs>

      {/* Footer */}
      <Card className="bg-muted/50">
        <CardContent className="pt-4">
          <p className="text-sm text-muted-foreground text-center">
            📚 Este manual é gerado automaticamente a partir do código fonte do RULEX.
            Todos os dados são reais e refletem as capacidades atuais do sistema.
          </p>
        </CardContent>
      </Card>
    </div>
  );
}
