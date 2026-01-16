/**
 * ActionsCatalog.tsx - Catálogo de Ações do RULEX
 *
 * Exibe todas as ações disponíveis que podem ser executadas
 * quando uma regra é acionada (ex: SET_DECISION, ADD_TAG, BLOCK_TRANSACTION)
 *
 * Dados gerados automaticamente de: RuleAction.java
 */
import { useState } from "react";
import { Search, Zap, AlertTriangle, CheckCircle2 } from "lucide-react";
import { Badge } from "@/components/ui/badge";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Input } from "@/components/ui/input";
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table";
import {
  Accordion,
  AccordionContent,
  AccordionItem,
  AccordionTrigger,
} from "@/components/ui/accordion";

import { BACKEND_ACTIONS } from "./generated";

// Descrições detalhadas e exemplos para cada ação
const ACTION_DETAILS: Record<string, { 
  description: string; 
  example: string; 
  params: string[];
  level: 'critical' | 'high' | 'medium' | 'info';
}> = {
  SET_DECISION: {
    description: "Define a decisão final da transação. A decisão pode ser APROVADO, SUSPEITA_DE_FRAUDE ou FRAUDE.",
    example: '{ "actionType": "SET_DECISION", "actionConfig": { "decision": "SUSPEITA_DE_FRAUDE" } }',
    params: ["decision (APROVADO | SUSPEITA_DE_FRAUDE | FRAUDE)"],
    level: "critical"
  },
  SET_SCORE: {
    description: "Define ou incrementa o score de risco da transação. Scores altos indicam maior probabilidade de fraude.",
    example: '{ "actionType": "SET_SCORE", "actionConfig": { "score": 75, "increment": true } }',
    params: ["score (número)", "increment (boolean - opcional)"],
    level: "high"
  },
  ADD_TAG: {
    description: "Adiciona uma tag/label à transação para categorização e análise posterior.",
    example: '{ "actionType": "ADD_TAG", "actionConfig": { "tag": "HIGH_RISK_MERCHANT" } }',
    params: ["tag (string)"],
    level: "info"
  },
  REMOVE_TAG: {
    description: "Remove uma tag/label da transação.",
    example: '{ "actionType": "REMOVE_TAG", "actionConfig": { "tag": "VIP_CUSTOMER" } }',
    params: ["tag (string)"],
    level: "info"
  },
  SET_VARIABLE: {
    description: "Define uma variável que pode ser usada por regras subsequentes na mesma avaliação.",
    example: '{ "actionType": "SET_VARIABLE", "actionConfig": { "name": "risk_level", "value": "HIGH" } }',
    params: ["name (string)", "value (any)"],
    level: "medium"
  },
  CALL_WEBHOOK: {
    description: "Chama um webhook externo com dados da transação. Útil para integrações com sistemas externos.",
    example: '{ "actionType": "CALL_WEBHOOK", "actionConfig": { "url": "https://api.example.com/alert", "method": "POST" } }',
    params: ["url (string)", "method (GET|POST)", "headers (object - opcional)"],
    level: "medium"
  },
  SEND_NOTIFICATION: {
    description: "Envia notificação (email, SMS, push) sobre a transação para a equipe de análise.",
    example: '{ "actionType": "SEND_NOTIFICATION", "actionConfig": { "channel": "email", "template": "fraud_alert" } }',
    params: ["channel (email|sms|push)", "template (string)", "recipients (array - opcional)"],
    level: "medium"
  },
  BLOCK_TRANSACTION: {
    description: "Bloqueia imediatamente a transação. Ação mais severa, deve ser usada com cautela.",
    example: '{ "actionType": "BLOCK_TRANSACTION", "actionConfig": { "reason": "Suspected fraud ring activity" } }',
    params: ["reason (string - opcional)"],
    level: "critical"
  },
  FLAG_FOR_REVIEW: {
    description: "Marca a transação para revisão manual pela equipe de análise de fraude.",
    example: '{ "actionType": "FLAG_FOR_REVIEW", "actionConfig": { "priority": "HIGH", "queue": "fraud_team" } }',
    params: ["priority (LOW|MEDIUM|HIGH)", "queue (string - opcional)"],
    level: "high"
  },
  ESCALATE: {
    description: "Escala o caso para um nível superior de análise (supervisor, compliance, etc.).",
    example: '{ "actionType": "ESCALATE", "actionConfig": { "level": "supervisor", "reason": "High value suspicious pattern" } }',
    params: ["level (string)", "reason (string)"],
    level: "high"
  }
};

const LEVEL_CONFIG = {
  critical: { color: "bg-red-100 text-red-800 dark:bg-red-900 dark:text-red-200", icon: "🚨" },
  high: { color: "bg-orange-100 text-orange-800 dark:bg-orange-900 dark:text-orange-200", icon: "⚠️" },
  medium: { color: "bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-200", icon: "📋" },
  info: { color: "bg-blue-100 text-blue-800 dark:bg-blue-900 dark:text-blue-200", icon: "ℹ️" }
};

export function ActionsCatalog() {
  const [searchQuery, setSearchQuery] = useState("");

  const filteredActions = BACKEND_ACTIONS.filter((action) => {
    if (!searchQuery.trim()) return true;
    const q = searchQuery.toLowerCase();
    const details = ACTION_DETAILS[action.name] || {};
    return (
      action.name.toLowerCase().includes(q) ||
      action.description?.toLowerCase().includes(q) ||
      details.description?.toLowerCase().includes(q)
    );
  });

  return (
    <div className="space-y-6">
      {/* Header */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Zap className="h-5 w-5" />
            Catálogo de Ações
          </CardTitle>
          <CardDescription>
            {BACKEND_ACTIONS.length} ações disponíveis que podem ser executadas quando uma regra é acionada
          </CardDescription>
        </CardHeader>
        <CardContent>
          <div className="relative max-w-md">
            <Search className="absolute left-3 top-1/2 -translate-y-1/2 h-4 w-4 text-muted-foreground" />
            <Input
              placeholder="Buscar ações..."
              className="pl-10"
              value={searchQuery}
              onChange={(e) => setSearchQuery(e.target.value)}
            />
          </div>
        </CardContent>
      </Card>

      {/* Aviso importante */}
      <Card className="border-yellow-500 bg-yellow-50 dark:bg-yellow-950">
        <CardContent className="pt-4">
          <div className="flex items-start gap-3">
            <AlertTriangle className="h-5 w-5 text-yellow-600 flex-shrink-0 mt-0.5" />
            <div>
              <p className="font-semibold text-yellow-800 dark:text-yellow-200">
                Atenção: Ordem de Execução
              </p>
              <p className="text-sm text-yellow-700 dark:text-yellow-300 mt-1">
                As ações são executadas na ordem em que estão configuradas na regra.
                Ações como <code>SET_DECISION</code> e <code>BLOCK_TRANSACTION</code> podem
                interromper o fluxo de avaliação dependendo da configuração.
              </p>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Lista de ações */}
      <Card>
        <CardHeader>
          <CardTitle>Todas as Ações</CardTitle>
        </CardHeader>
        <CardContent>
          <Accordion type="multiple" className="w-full">
            {filteredActions.map((action) => {
              const details = ACTION_DETAILS[action.name] || {
                description: action.description || "Sem descrição disponível",
                example: "{}",
                params: [],
                level: "info" as const
              };
              const levelConfig = LEVEL_CONFIG[details.level];

              return (
                <AccordionItem
                  key={action.name}
                  value={action.name}
                  id={`manual-action-${action.name}`}
                >
                  <AccordionTrigger className="hover:no-underline">
                    <div className="flex items-center gap-3 text-left">
                      <span className="text-lg">{levelConfig.icon}</span>
                      <div>
                        <code className="font-mono font-semibold">{action.name}</code>
                        <p className="text-sm text-muted-foreground font-normal">
                          {details.description.slice(0, 80)}...
                        </p>
                      </div>
                    </div>
                  </AccordionTrigger>
                  <AccordionContent>
                    <div className="space-y-4 pt-2">
                      {/* Nível de impacto */}
                      <div className="flex items-center gap-2">
                        <span className="text-sm font-medium">Nível de Impacto:</span>
                        <Badge className={levelConfig.color}>
                          {details.level.toUpperCase()}
                        </Badge>
                      </div>

                      {/* Descrição completa */}
                      <div>
                        <h4 className="text-sm font-medium mb-1">Descrição</h4>
                        <p className="text-sm text-muted-foreground">
                          {details.description}
                        </p>
                      </div>

                      {/* Parâmetros */}
                      {details.params.length > 0 && (
                        <div>
                          <h4 className="text-sm font-medium mb-1">Parâmetros</h4>
                          <ul className="list-disc list-inside text-sm text-muted-foreground">
                            {details.params.map((param, idx) => (
                              <li key={idx}>
                                <code className="bg-muted px-1 rounded">{param}</code>
                              </li>
                            ))}
                          </ul>
                        </div>
                      )}

                      {/* Exemplo */}
                      <div>
                        <h4 className="text-sm font-medium mb-1">Exemplo de Configuração</h4>
                        <pre className="bg-muted rounded-lg p-3 text-xs overflow-auto">
                          {details.example}
                        </pre>
                      </div>
                    </div>
                  </AccordionContent>
                </AccordionItem>
              );
            })}
          </Accordion>
        </CardContent>
      </Card>

      {/* Tabela resumo */}
      <Card>
        <CardHeader>
          <CardTitle>Resumo Rápido</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="rounded-md border">
            <Table>
              <TableHeader>
                <TableRow>
                  <TableHead>Ação</TableHead>
                  <TableHead>Nível</TableHead>
                  <TableHead>Descrição Curta</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {BACKEND_ACTIONS.map((action) => {
                  const details = ACTION_DETAILS[action.name] || { level: "info" as const, description: "" };
                  const levelConfig = LEVEL_CONFIG[details.level];
                  return (
                    <TableRow key={action.name}>
                      <TableCell className="font-mono">{action.name}</TableCell>
                      <TableCell>
                        <Badge className={levelConfig.color}>
                          {details.level}
                        </Badge>
                      </TableCell>
                      <TableCell className="text-muted-foreground text-sm">
                        {details.description?.slice(0, 60)}...
                      </TableCell>
                    </TableRow>
                  );
                })}
              </TableBody>
            </Table>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}
