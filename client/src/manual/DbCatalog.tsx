/**
 * DbCatalog.tsx - Catálogo de Schema do Banco de Dados RULEX
 *
 * Exibe a estrutura do banco de dados PostgreSQL:
 * - Tabelas principais e seus propósitos
 * - Migrations Flyway (V1 a V35+)
 * - Tipos ENUM e constraints
 * - Índices e relacionamentos
 *
 * Dados derivados de: backend/src/main/resources/db/migration/
 */
import { useState, useMemo } from "react";
import { Search, Database, Table, Key, ArrowRight, History, Check, AlertCircle } from "lucide-react";
import { Badge } from "@/components/ui/badge";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Input } from "@/components/ui/input";
import {
  Accordion,
  AccordionContent,
  AccordionItem,
  AccordionTrigger,
} from "@/components/ui/accordion";
import {
  Table as UITable,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";

// ===== DADOS DO SCHEMA (extraídos manualmente das migrations) =====

const MAIN_TABLES = [
  {
    name: "transactions",
    description: "Armazena todas as transações recebidas para análise",
    migration: "V2",
    category: "Core",
    columns: [
      { name: "id", type: "BIGSERIAL", pk: true, description: "Identificador único" },
      { name: "external_transaction_id", type: "VARCHAR(64)", description: "ID externo da transação (único)" },
      { name: "customer_id_from_header", type: "VARCHAR(64)", description: "ID do cliente no header" },
      { name: "customer_acct_number", type: "BIGINT", description: "Número da conta do cliente" },
      { name: "pan", type: "VARCHAR(64)", description: "PAN do cartão (mascarado)" },
      { name: "transaction_amount", type: "NUMERIC(18,2)", description: "Valor da transação" },
      { name: "transaction_date", type: "INTEGER", description: "Data YYYYMMDD" },
      { name: "transaction_time", type: "INTEGER", description: "Hora HHMMSS" },
      { name: "mcc", type: "INTEGER", description: "Merchant Category Code" },
      { name: "pos_entry_mode", type: "VARCHAR(3)", description: "Modo de entrada POS" },
      { name: "merchant_country_code", type: "VARCHAR(10)", description: "Código do país do merchant" },
    ],
  },
  {
    name: "transaction_decisions",
    description: "Decisões tomadas para cada transação analisada",
    migration: "V2",
    category: "Core",
    columns: [
      { name: "id", type: "BIGSERIAL", pk: true, description: "Identificador único" },
      { name: "transaction_id", type: "BIGINT", fk: "transactions", description: "FK para transactions" },
      { name: "classification", type: "VARCHAR(20)", description: "APPROVED, SUSPICIOUS, FRAUD" },
      { name: "risk_score", type: "INTEGER", description: "Score de risco (0-100)" },
      { name: "rules_applied", type: "TEXT", description: "JSON das regras aplicadas" },
      { name: "reason", type: "TEXT", description: "Motivo da decisão" },
    ],
  },
  {
    name: "rule_configurations",
    description: "Configurações das regras de detecção (tabela principal)",
    migration: "V2",
    category: "Rules",
    columns: [
      { name: "id", type: "BIGSERIAL", pk: true, description: "Identificador único" },
      { name: "rule_name", type: "VARCHAR(100)", description: "Nome único da regra" },
      { name: "description", type: "TEXT", description: "Descrição da regra" },
      { name: "rule_type", type: "VARCHAR(20)", description: "SECURITY, CONTEXT, VELOCITY, ANOMALY" },
      { name: "threshold", type: "INTEGER", description: "Threshold de ativação" },
      { name: "weight", type: "INTEGER", description: "Peso no score" },
      { name: "enabled", type: "BOOLEAN", description: "Se regra está ativa" },
      { name: "conditions_json", type: "TEXT", description: "JSON das condições" },
      { name: "logic_operator", type: "VARCHAR(3)", description: "AND ou OR" },
    ],
  },
  {
    name: "rules",
    description: "Tabela de regras com versionamento (UUID-based)",
    migration: "V1",
    category: "Rules",
    columns: [
      { name: "id", type: "UUID", pk: true, description: "Identificador único (UUID)" },
      { name: "key", type: "TEXT", description: "Chave única da regra" },
      { name: "title", type: "TEXT", description: "Título da regra" },
      { name: "created_by", type: "UUID", fk: "users", description: "Criador" },
    ],
  },
  {
    name: "rule_versions",
    description: "Versões das regras com histórico completo",
    migration: "V1",
    category: "Rules",
    columns: [
      { name: "id", type: "UUID", pk: true, description: "Identificador único" },
      { name: "rule_id", type: "UUID", fk: "rules", description: "FK para rules" },
      { name: "version", type: "INT", description: "Número da versão" },
      { name: "status", type: "rule_status", description: "DRAFT, PUBLISHED, DEPRECATED" },
      { name: "priority", type: "INT", description: "Prioridade de execução" },
      { name: "severity", type: "INT", description: "Severidade (0-100)" },
      { name: "decision", type: "decision_outcome", description: "APROVADO, SUSPEITA, FRAUDE" },
      { name: "conditions_json", type: "JSONB", description: "Condições da regra" },
    ],
  },
  {
    name: "users",
    description: "Usuários do sistema (RBAC)",
    migration: "V1",
    category: "Auth",
    columns: [
      { name: "id", type: "UUID", pk: true, description: "Identificador único" },
      { name: "email", type: "TEXT", description: "Email (único)" },
      { name: "display_name", type: "TEXT", description: "Nome de exibição" },
      { name: "password_hash", type: "TEXT", description: "Hash da senha (bcrypt)" },
      { name: "enabled", type: "BOOLEAN", description: "Se usuário está ativo" },
    ],
  },
  {
    name: "roles",
    description: "Papéis/perfis de acesso",
    migration: "V1",
    category: "Auth",
    columns: [
      { name: "id", type: "UUID", pk: true, description: "Identificador único" },
      { name: "name", type: "TEXT", description: "Nome do papel (único)" },
    ],
  },
  {
    name: "audit_log",
    description: "Log de auditoria de todas as ações",
    migration: "V1",
    category: "Audit",
    columns: [
      { name: "id", type: "UUID", pk: true, description: "Identificador único" },
      { name: "action_type", type: "audit_action_type", description: "Tipo da ação" },
      { name: "entity_type", type: "TEXT", description: "Tipo da entidade" },
      { name: "entity_id", type: "UUID", description: "ID da entidade" },
      { name: "performed_by", type: "UUID", fk: "users", description: "Quem executou" },
      { name: "diff_json", type: "JSONB", description: "Diferenças antes/depois" },
      { name: "result", type: "audit_result", description: "SUCCESS ou FAILURE" },
    ],
  },
  {
    name: "decision_log",
    description: "Log de decisões tomadas pelo motor",
    migration: "V1",
    category: "Audit",
    columns: [
      { name: "id", type: "UUID", pk: true, description: "Identificador único" },
      { name: "external_transaction_id", type: "TEXT", description: "ID externo da transação" },
      { name: "decision", type: "decision_outcome", description: "Decisão tomada" },
      { name: "risk_score", type: "INT", description: "Score de risco" },
      { name: "triggered_rules_json", type: "JSONB", description: "Regras acionadas" },
      { name: "payload_json", type: "JSONB", description: "Payload original" },
    ],
  },
  {
    name: "bin_lookup",
    description: "Tabela de lookup de BINs de cartão",
    migration: "V11",
    category: "Reference",
    columns: [
      { name: "bin", type: "VARCHAR(6)", pk: true, description: "6 dígitos do BIN" },
      { name: "brand", type: "VARCHAR(50)", description: "Bandeira (Visa, Master, etc)" },
      { name: "card_type", type: "VARCHAR(20)", description: "Crédito, Débito, Prepaid" },
      { name: "issuer_country", type: "VARCHAR(2)", description: "País emissor (ISO)" },
      { name: "issuer_name", type: "VARCHAR(255)", description: "Nome do emissor" },
    ],
  },
  {
    name: "geo_reference",
    description: "Referência geográfica para validações",
    migration: "V13",
    category: "Reference",
    columns: [
      { name: "id", type: "BIGSERIAL", pk: true, description: "Identificador único" },
      { name: "country_code", type: "VARCHAR(3)", description: "Código do país (ISO)" },
      { name: "city", type: "VARCHAR(255)", description: "Cidade" },
      { name: "latitude", type: "DECIMAL(9,6)", description: "Latitude" },
      { name: "longitude", type: "DECIMAL(9,6)", description: "Longitude" },
    ],
  },
  {
    name: "velocity_counters",
    description: "Contadores para regras de velocidade",
    migration: "V14",
    category: "Velocity",
    columns: [
      { name: "id", type: "BIGSERIAL", pk: true, description: "Identificador único" },
      { name: "counter_key", type: "VARCHAR(255)", description: "Chave do contador" },
      { name: "counter_value", type: "INTEGER", description: "Valor atual" },
      { name: "window_start", type: "TIMESTAMP", description: "Início da janela" },
      { name: "window_end", type: "TIMESTAMP", description: "Fim da janela" },
    ],
  },
  {
    name: "complex_rules",
    description: "Regras complexas com grupos de condições",
    migration: "V8/V12",
    category: "Rules",
    columns: [
      { name: "id", type: "BIGSERIAL", pk: true, description: "Identificador único" },
      { name: "name", type: "VARCHAR(255)", description: "Nome da regra" },
      { name: "condition_groups_json", type: "JSONB", description: "Grupos de condições" },
      { name: "enabled", type: "BOOLEAN", description: "Se regra está ativa" },
    ],
  },
  {
    name: "access_log",
    description: "Log de acesso ao sistema",
    migration: "V19",
    category: "Audit",
    columns: [
      { name: "id", type: "BIGSERIAL", pk: true, description: "Identificador único" },
      { name: "user_id", type: "UUID", fk: "users", description: "Usuário que acessou" },
      { name: "action", type: "VARCHAR(100)", description: "Ação realizada" },
      { name: "ip_address", type: "VARCHAR(45)", description: "IP de origem" },
      { name: "user_agent", type: "TEXT", description: "User-Agent" },
    ],
  },
];

const ENUM_TYPES = [
  {
    name: "decision_outcome",
    values: ["APROVADO", "SUSPEITA_DE_FRAUDE", "FRAUDE"],
    description: "Resultado da análise de transação",
  },
  {
    name: "rule_status",
    values: ["DRAFT", "PUBLISHED", "DEPRECATED"],
    description: "Status do ciclo de vida da regra",
  },
  {
    name: "logic_operator",
    values: ["AND", "OR"],
    description: "Operador lógico para combinar condições",
  },
  {
    name: "audit_action_type",
    values: ["RULE_CREATED", "RULE_UPDATED", "RULE_DELETED", "RULE_PUBLISHED", "RULESET_PUBLISHED", "RULESET_ACTIVATED", "TRANSACTION_ANALYZED", "SIMULATION_RUN"],
    description: "Tipos de ações auditadas",
  },
  {
    name: "audit_result",
    values: ["SUCCESS", "FAILURE"],
    description: "Resultado de uma ação auditada",
  },
];

const MIGRATIONS = [
  { version: "V1", name: "init", description: "Schema inicial: users, roles, rules, rule_versions, audit_log, decision_log" },
  { version: "V2", name: "core_schema", description: "Tabelas core: transactions, transaction_decisions, rule_configurations" },
  { version: "V3", name: "extend_workflow_length", description: "Aumenta tamanho do campo workflow" },
  { version: "V4", name: "raw_hash_idempotency", description: "Hash de idempotência para payloads" },
  { version: "V5", name: "raw_as_received", description: "Campo para payload original" },
  { version: "V6", name: "v31_exec_log_field_dictionary", description: "Dicionário de campos para exec log" },
  { version: "V7", name: "v31_exec_log_dedup", description: "Deduplicação de exec log" },
  { version: "V8", name: "complex_rules_support", description: "Suporte a regras complexas" },
  { version: "V9", name: "audit_compliance_enhancements", description: "Melhorias de compliance no audit" },
  { version: "V10", name: "derived_context_improvements", description: "Melhorias no contexto derivado" },
  { version: "V11", name: "bin_lookup_table", description: "Tabela de lookup de BINs" },
  { version: "V12", name: "complex_rules_crud", description: "CRUD completo para regras complexas" },
  { version: "V13", name: "geo_reference_table", description: "Tabela de referência geográfica" },
  { version: "V14", name: "velocity_counters", description: "Contadores de velocidade" },
  { version: "V15", name: "add_velocity_operators", description: "Operadores de velocidade" },
  { version: "V16-V17", name: "fix_geo_polygon_id_type", description: "Fix tipos de ID geo" },
  { version: "V18", name: "enable_condition_groups_constraint", description: "Constraint para grupos de condições" },
  { version: "V19", name: "access_log_table", description: "Tabela de log de acesso" },
  { version: "V20", name: "shadow_mode_and_device_fingerprinting", description: "Shadow mode e device fingerprint" },
  { version: "V21", name: "rule_configurations_shadow_mode", description: "Shadow mode em regras" },
  { version: "V22-V25", name: "fraud_detection_rules_seed", description: "Seed de regras de fraude (200+)" },
  { version: "V26-V27", name: "fix_complex_rules_conditions", description: "Fix condições de regras complexas" },
  { version: "V28", name: "add_missing_condition_operators", description: "Adiciona operadores faltantes" },
  { version: "V29-V30", name: "insert_advanced_fraud_rules", description: "Regras avançadas AML/ATO" },
  { version: "V31", name: "add_velocity_extended_fields", description: "Campos estendidos de velocidade" },
  { version: "V32", name: "add_missing_tables_for_operators", description: "Tabelas para novos operadores" },
  { version: "V33", name: "fix_pos_entry_mode_length", description: "Fix tamanho pos_entry_mode" },
  { version: "V34", name: "add_v31_plus_operators", description: "Operadores V31+" },
  { version: "V35", name: "add_velocity_temporal_indexes", description: "Índices temporais de velocidade" },
];

export function DbCatalog() {
  const [searchQuery, setSearchQuery] = useState("");
  const [selectedCategory, setSelectedCategory] = useState<string | null>(null);

  const categories = useMemo(() => {
    return [...new Set(MAIN_TABLES.map((t) => t.category))];
  }, []);

  const filteredTables = useMemo(() => {
    return MAIN_TABLES.filter((table) => {
      if (selectedCategory && table.category !== selectedCategory) return false;
      if (!searchQuery.trim()) return true;
      const q = searchQuery.toLowerCase();
      return (
        table.name.toLowerCase().includes(q) ||
        table.description.toLowerCase().includes(q) ||
        table.columns.some((c) => c.name.toLowerCase().includes(q))
      );
    });
  }, [searchQuery, selectedCategory]);

  return (
    <div className="space-y-6">
      {/* Header */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Database className="h-5 w-5" />
            Schema do Banco de Dados
          </CardTitle>
          <CardDescription>
            PostgreSQL 15+ com {MAIN_TABLES.length} tabelas principais e {MIGRATIONS.length} migrations Flyway
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          {/* Busca */}
          <div className="relative max-w-md">
            <Search className="absolute left-3 top-1/2 -translate-y-1/2 h-4 w-4 text-muted-foreground" />
            <Input
              placeholder="Buscar tabelas ou colunas..."
              className="pl-10"
              value={searchQuery}
              onChange={(e) => setSearchQuery(e.target.value)}
            />
          </div>

          {/* Filtros por categoria */}
          <div className="flex flex-wrap gap-2">
            <Badge
              variant={selectedCategory === null ? "default" : "outline"}
              className="cursor-pointer"
              onClick={() => setSelectedCategory(null)}
            >
              Todas
            </Badge>
            {categories.map((cat) => (
              <Badge
                key={cat}
                variant={selectedCategory === cat ? "default" : "outline"}
                className="cursor-pointer"
                onClick={() => setSelectedCategory(cat === selectedCategory ? null : cat)}
              >
                {cat}
              </Badge>
            ))}
          </div>
        </CardContent>
      </Card>

      {/* Tabs principais */}
      <Tabs defaultValue="tables" className="w-full">
        <TabsList className="grid w-full grid-cols-3">
          <TabsTrigger value="tables">Tabelas ({MAIN_TABLES.length})</TabsTrigger>
          <TabsTrigger value="enums">Tipos ENUM ({ENUM_TYPES.length})</TabsTrigger>
          <TabsTrigger value="migrations">Migrations ({MIGRATIONS.length})</TabsTrigger>
        </TabsList>

        {/* Tabelas */}
        <TabsContent value="tables" className="space-y-4">
          <Accordion type="multiple" className="w-full">
            {filteredTables.map((table) => (
              <AccordionItem key={table.name} value={table.name}>
                <AccordionTrigger className="hover:no-underline">
                  <div className="flex items-center gap-3 text-left">
                    <Table className="h-4 w-4 text-muted-foreground" />
                    <code className="font-mono font-semibold">{table.name}</code>
                    <Badge variant="outline">{table.category}</Badge>
                    <Badge variant="secondary">{table.migration}</Badge>
                  </div>
                </AccordionTrigger>
                <AccordionContent>
                  <div className="space-y-4 pt-2">
                    <p className="text-sm text-muted-foreground">{table.description}</p>
                    <div className="rounded-md border">
                      <UITable>
                        <TableHeader>
                          <TableRow>
                            <TableHead>Coluna</TableHead>
                            <TableHead>Tipo</TableHead>
                            <TableHead>Chaves</TableHead>
                            <TableHead>Descrição</TableHead>
                          </TableRow>
                        </TableHeader>
                        <TableBody>
                          {table.columns.map((col) => (
                            <TableRow key={col.name}>
                              <TableCell>
                                <code className="font-mono text-sm">{col.name}</code>
                              </TableCell>
                              <TableCell>
                                <code className="text-xs bg-muted px-1 rounded">{col.type}</code>
                              </TableCell>
                              <TableCell>
                                {col.pk && <Badge className="mr-1 bg-yellow-600">PK</Badge>}
                                {col.fk && (
                                  <Badge variant="outline" className="text-xs">
                                    FK → {col.fk}
                                  </Badge>
                                )}
                              </TableCell>
                              <TableCell className="text-sm text-muted-foreground">
                                {col.description}
                              </TableCell>
                            </TableRow>
                          ))}
                        </TableBody>
                      </UITable>
                    </div>
                  </div>
                </AccordionContent>
              </AccordionItem>
            ))}
          </Accordion>
        </TabsContent>

        {/* Tipos ENUM */}
        <TabsContent value="enums" className="space-y-4">
          <div className="grid gap-4 md:grid-cols-2">
            {ENUM_TYPES.map((enumType) => (
              <Card key={enumType.name}>
                <CardHeader className="pb-2">
                  <CardTitle className="text-base flex items-center gap-2">
                    <Key className="h-4 w-4" />
                    <code>{enumType.name}</code>
                  </CardTitle>
                  <CardDescription>{enumType.description}</CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="flex flex-wrap gap-1">
                    {enumType.values.map((val) => (
                      <Badge key={val} variant="secondary">
                        {val}
                      </Badge>
                    ))}
                  </div>
                </CardContent>
              </Card>
            ))}
          </div>
        </TabsContent>

        {/* Migrations */}
        <TabsContent value="migrations" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                <History className="h-5 w-5" />
                Histórico de Migrations Flyway
              </CardTitle>
              <CardDescription>
                Evoluções do schema desde V1 até V35
              </CardDescription>
            </CardHeader>
            <CardContent>
              <div className="rounded-md border">
                <UITable>
                  <TableHeader>
                    <TableRow>
                      <TableHead className="w-24">Versão</TableHead>
                      <TableHead className="w-64">Nome</TableHead>
                      <TableHead>Descrição</TableHead>
                    </TableRow>
                  </TableHeader>
                  <TableBody>
                    {MIGRATIONS.map((m) => (
                      <TableRow key={m.version}>
                        <TableCell>
                          <Badge variant="outline">{m.version}</Badge>
                        </TableCell>
                        <TableCell>
                          <code className="text-sm">{m.name}</code>
                        </TableCell>
                        <TableCell className="text-sm text-muted-foreground">
                          {m.description}
                        </TableCell>
                      </TableRow>
                    ))}
                  </TableBody>
                </UITable>
              </div>
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>

      {/* Diagrama ER simplificado */}
      <Card>
        <CardHeader>
          <CardTitle>Relacionamentos Principais</CardTitle>
          <CardDescription>Diagrama simplificado das FKs</CardDescription>
        </CardHeader>
        <CardContent>
          <div className="space-y-2 font-mono text-sm">
            <div className="flex items-center gap-2">
              <Badge>transactions</Badge>
              <ArrowRight className="h-4 w-4" />
              <Badge variant="secondary">transaction_decisions</Badge>
              <span className="text-muted-foreground text-xs">(1:N)</span>
            </div>
            <div className="flex items-center gap-2">
              <Badge>rules</Badge>
              <ArrowRight className="h-4 w-4" />
              <Badge variant="secondary">rule_versions</Badge>
              <span className="text-muted-foreground text-xs">(1:N)</span>
            </div>
            <div className="flex items-center gap-2">
              <Badge>users</Badge>
              <ArrowRight className="h-4 w-4" />
              <Badge variant="secondary">user_roles</Badge>
              <ArrowRight className="h-4 w-4" />
              <Badge variant="secondary">roles</Badge>
              <span className="text-muted-foreground text-xs">(N:M)</span>
            </div>
            <div className="flex items-center gap-2">
              <Badge>users</Badge>
              <ArrowRight className="h-4 w-4" />
              <Badge variant="secondary">audit_log</Badge>
              <span className="text-muted-foreground text-xs">(1:N)</span>
            </div>
            <div className="flex items-center gap-2">
              <Badge>rule_sets</Badge>
              <ArrowRight className="h-4 w-4" />
              <Badge variant="secondary">rule_set_versions</Badge>
              <ArrowRight className="h-4 w-4" />
              <Badge variant="secondary">rule_set_version_items</Badge>
              <span className="text-muted-foreground text-xs">(hierarquia)</span>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Dicas de consulta */}
      <Card className="border-green-500">
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Check className="h-5 w-5 text-green-600" />
            Consultas Úteis
          </CardTitle>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="bg-muted rounded-lg p-4">
            <h4 className="font-semibold mb-2">Listar regras ativas com condições</h4>
            <pre className="text-sm bg-background rounded p-2 overflow-x-auto">
{`SELECT rule_name, rule_type, conditions_json
FROM rule_configurations
WHERE enabled = true
ORDER BY rule_name;`}
            </pre>
          </div>

          <div className="bg-muted rounded-lg p-4">
            <h4 className="font-semibold mb-2">Decisões por classificação (últimas 24h)</h4>
            <pre className="text-sm bg-background rounded p-2 overflow-x-auto">
{`SELECT classification, COUNT(*) as total
FROM transaction_decisions
WHERE created_at > NOW() - INTERVAL '24 hours'
GROUP BY classification;`}
            </pre>
          </div>

          <div className="bg-muted rounded-lg p-4">
            <h4 className="font-semibold mb-2">Verificar status das migrations</h4>
            <pre className="text-sm bg-background rounded p-2 overflow-x-auto">
{`SELECT version, description, installed_on, success
FROM flyway_schema_history
ORDER BY installed_rank;`}
            </pre>
          </div>
        </CardContent>
      </Card>

      {/* Aviso importante */}
      <Card className="border-amber-500 bg-amber-50 dark:bg-amber-950">
        <CardContent className="pt-4">
          <div className="flex items-start gap-3">
            <AlertCircle className="h-5 w-5 text-amber-600 flex-shrink-0 mt-0.5" />
            <div>
              <p className="font-semibold text-amber-800 dark:text-amber-200">
                Importante: Flyway é fonte da verdade
              </p>
              <p className="text-sm text-amber-700 dark:text-amber-300 mt-1">
                O schema é gerenciado exclusivamente por migrations Flyway.
                Não use <code className="bg-amber-100 dark:bg-amber-800 px-1 rounded">ddl-auto</code> do Hibernate para criar/alterar tabelas.
                Todas as mudanças de schema devem ser feitas via nova migration (V36+).
              </p>
            </div>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}
