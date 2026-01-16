/**
 * ApiCatalog.tsx - Catálogo de Endpoints da API RULEX
 *
 * Exibe todos os endpoints disponíveis:
 * - Métodos HTTP (GET, POST, PUT, DELETE)
 * - Paths e descrições
 * - Parâmetros e payloads
 *
 * Dados gerados de: openapi/rulex.yaml
 */
import { useState, useMemo } from "react";
import { Search, Globe, FileJson, Lock, Check, AlertTriangle } from "lucide-react";
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
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table";

import { API_ENDPOINTS } from "./generated";

const METHOD_COLORS: Record<string, string> = {
  GET: "bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-200",
  POST: "bg-blue-100 text-blue-800 dark:bg-blue-900 dark:text-blue-200",
  PUT: "bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-200",
  DELETE: "bg-red-100 text-red-800 dark:bg-red-900 dark:text-red-200",
  PATCH: "bg-purple-100 text-purple-800 dark:bg-purple-900 dark:text-purple-200",
};

// Agrupar endpoints por recurso (primeira parte do path)
const groupByResource = (endpoints: typeof API_ENDPOINTS) => {
  const groups: Record<string, typeof API_ENDPOINTS> = {};
  endpoints.forEach((ep) => {
    const resource = ep.path.split("/").filter(Boolean)[1] || "other";
    if (!groups[resource]) groups[resource] = [];
    groups[resource].push(ep);
  });
  return groups;
};

export function ApiCatalog() {
  const [searchQuery, setSearchQuery] = useState("");
  const [selectedMethod, setSelectedMethod] = useState<string | null>(null);
  const [selectedResource, setSelectedResource] = useState<string | null>(null);

  // Agrupar por recurso
  const groupedEndpoints = useMemo(() => groupByResource(API_ENDPOINTS), []);
  const resources = Object.keys(groupedEndpoints);

  // Métodos únicos
  const methods = useMemo(() => {
    return [...new Set(API_ENDPOINTS.map((ep) => ep.method))];
  }, []);

  // Filtrar
  const filteredEndpoints = useMemo(() => {
    return API_ENDPOINTS.filter((ep) => {
      if (selectedMethod && ep.method !== selectedMethod) return false;
      if (selectedResource) {
        const resource = ep.path.split("/").filter(Boolean)[1] || "other";
        if (resource !== selectedResource) return false;
      }
      if (!searchQuery.trim()) return true;
      const q = searchQuery.toLowerCase();
      return (
        ep.path.toLowerCase().includes(q) ||
        ep.method.toLowerCase().includes(q) ||
        ep.summary?.toLowerCase().includes(q) ||
        ep.operationId?.toLowerCase().includes(q)
      );
    });
  }, [searchQuery, selectedMethod, selectedResource]);

  return (
    <div className="space-y-6">
      {/* Header */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Globe className="h-5 w-5" />
            Catálogo de API REST
          </CardTitle>
          <CardDescription>
            {API_ENDPOINTS.length} endpoints disponíveis na API RULEX
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          {/* Busca */}
          <div className="relative max-w-md">
            <Search className="absolute left-3 top-1/2 -translate-y-1/2 h-4 w-4 text-muted-foreground" />
            <Input
              placeholder="Buscar endpoints..."
              className="pl-10"
              value={searchQuery}
              onChange={(e) => setSearchQuery(e.target.value)}
            />
          </div>

          {/* Filtros por método */}
          <div className="flex flex-wrap gap-2">
            <span className="text-sm font-medium mr-2">Método:</span>
            <Badge
              variant={selectedMethod === null ? "default" : "outline"}
              className="cursor-pointer"
              onClick={() => setSelectedMethod(null)}
            >
              Todos
            </Badge>
            {methods.map((method) => (
              <Badge
                key={method}
                variant={selectedMethod === method ? "default" : "outline"}
                className={`cursor-pointer ${selectedMethod !== method ? METHOD_COLORS[method] : ""}`}
                onClick={() => setSelectedMethod(method === selectedMethod ? null : method)}
              >
                {method}
              </Badge>
            ))}
          </div>

          {/* Filtros por recurso */}
          <div className="flex flex-wrap gap-2">
            <span className="text-sm font-medium mr-2">Recurso:</span>
            <Badge
              variant={selectedResource === null ? "default" : "outline"}
              className="cursor-pointer"
              onClick={() => setSelectedResource(null)}
            >
              Todos
            </Badge>
            {resources.map((resource) => (
              <Badge
                key={resource}
                variant={selectedResource === resource ? "default" : "outline"}
                className="cursor-pointer"
                onClick={() => setSelectedResource(resource === selectedResource ? null : resource)}
              >
                /{resource}
              </Badge>
            ))}
          </div>
        </CardContent>
      </Card>

      {/* Estatísticas */}
      <div className="grid gap-4 md:grid-cols-4">
        {methods.map((method) => {
          const count = API_ENDPOINTS.filter((ep) => ep.method === method).length;
          return (
            <Card key={method}>
              <CardContent className="pt-4">
                <div className="flex justify-between items-center">
                  <Badge className={METHOD_COLORS[method]}>{method}</Badge>
                  <span className="text-2xl font-bold">{count}</span>
                </div>
              </CardContent>
            </Card>
          );
        })}
      </div>

      {/* Tabela de endpoints */}
      <Card>
        <CardHeader>
          <CardTitle>Lista de Endpoints</CardTitle>
          <CardDescription>
            {filteredEndpoints.length} endpoints {selectedMethod || selectedResource ? "filtrados" : ""}
          </CardDescription>
        </CardHeader>
        <CardContent>
          <div className="rounded-md border">
            <Table>
              <TableHeader>
                <TableRow>
                  <TableHead className="w-24">Método</TableHead>
                  <TableHead>Path</TableHead>
                  <TableHead>Descrição</TableHead>
                  <TableHead>Operation ID</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {filteredEndpoints.map((ep, idx) => (
                  <TableRow key={`${ep.method}-${ep.path}-${idx}`}>
                    <TableCell>
                      <Badge className={METHOD_COLORS[ep.method]}>{ep.method}</Badge>
                    </TableCell>
                    <TableCell>
                      <code className="text-sm font-mono">{ep.path}</code>
                    </TableCell>
                    <TableCell className="text-sm text-muted-foreground">
                      {ep.summary || "-"}
                    </TableCell>
                    <TableCell>
                      <code className="text-xs bg-muted px-1 rounded">
                        {ep.operationId || "-"}
                      </code>
                    </TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          </div>
        </CardContent>
      </Card>

      {/* Detalhes por Recurso */}
      <Card>
        <CardHeader>
          <CardTitle>Endpoints por Recurso</CardTitle>
          <CardDescription>
            Documentação detalhada agrupada por domínio
          </CardDescription>
        </CardHeader>
        <CardContent>
          <Accordion type="multiple" className="w-full">
            {resources.map((resource) => (
              <AccordionItem key={resource} value={resource}>
                <AccordionTrigger className="hover:no-underline">
                  <div className="flex items-center gap-3">
                    <FileJson className="h-4 w-4" />
                    <code className="font-mono">/{resource}</code>
                    <Badge variant="secondary">{groupedEndpoints[resource].length}</Badge>
                  </div>
                </AccordionTrigger>
                <AccordionContent>
                  <div className="space-y-3 pt-2">
                    {groupedEndpoints[resource].map((ep, idx) => (
                      <div key={idx} className="bg-muted rounded-lg p-3">
                        <div className="flex items-center gap-2">
                          <Badge className={METHOD_COLORS[ep.method]}>{ep.method}</Badge>
                          <code className="font-mono text-sm">{ep.path}</code>
                        </div>
                        {ep.summary && (
                          <p className="text-sm text-muted-foreground mt-2">{ep.summary}</p>
                        )}
                        {ep.operationId && (
                          <p className="text-xs mt-1">
                            <span className="font-medium">operationId:</span>{" "}
                            <code className="bg-background px-1 rounded">{ep.operationId}</code>
                          </p>
                        )}
                      </div>
                    ))}
                  </div>
                </AccordionContent>
              </AccordionItem>
            ))}
          </Accordion>
        </CardContent>
      </Card>

      {/* Exemplos de uso */}
      <Card>
        <CardHeader>
          <CardTitle>Exemplos de Requisições</CardTitle>
          <CardDescription>
            Exemplos práticos de uso da API
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="grid gap-4 lg:grid-cols-2">
            <div className="bg-muted rounded-lg p-4">
              <div className="flex items-center gap-2 mb-2">
                <Badge className={METHOD_COLORS.GET}>GET</Badge>
                <span className="font-semibold">Listar regras</span>
              </div>
              <pre className="text-sm bg-background rounded p-2 overflow-x-auto">
{`curl -X GET http://localhost:8080/api/rules \\
  -H "Authorization: Bearer <token>"`}
              </pre>
            </div>

            <div className="bg-muted rounded-lg p-4">
              <div className="flex items-center gap-2 mb-2">
                <Badge className={METHOD_COLORS.POST}>POST</Badge>
                <span className="font-semibold">Criar regra</span>
              </div>
              <pre className="text-sm bg-background rounded p-2 overflow-x-auto">
{`curl -X POST http://localhost:8080/api/rules \\
  -H "Content-Type: application/json" \\
  -d '{"name": "Nova Regra", ...}'`}
              </pre>
            </div>

            <div className="bg-muted rounded-lg p-4">
              <div className="flex items-center gap-2 mb-2">
                <Badge className={METHOD_COLORS.PUT}>PUT</Badge>
                <span className="font-semibold">Atualizar regra</span>
              </div>
              <pre className="text-sm bg-background rounded p-2 overflow-x-auto">
{`curl -X PUT http://localhost:8080/api/rules/123 \\
  -H "Content-Type: application/json" \\
  -d '{"name": "Regra Atualizada", ...}'`}
              </pre>
            </div>

            <div className="bg-muted rounded-lg p-4">
              <div className="flex items-center gap-2 mb-2">
                <Badge className={METHOD_COLORS.DELETE}>DELETE</Badge>
                <span className="font-semibold">Remover regra</span>
              </div>
              <pre className="text-sm bg-background rounded p-2 overflow-x-auto">
{`curl -X DELETE http://localhost:8080/api/rules/123 \\
  -H "Authorization: Bearer <token>"`}
              </pre>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Autenticação */}
      <Card className="border-amber-500">
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Lock className="h-5 w-5 text-amber-600" />
            Autenticação
          </CardTitle>
        </CardHeader>
        <CardContent className="space-y-3">
          <p className="text-sm">
            A API RULEX usa autenticação JWT (Bearer Token). Todas as requisições
            (exceto login) requerem o header:
          </p>
          <pre className="bg-muted rounded p-2 text-sm">
{`Authorization: Bearer <seu_token_jwt>`}
          </pre>
          <div className="flex items-start gap-2 bg-amber-50 dark:bg-amber-950 rounded p-3">
            <AlertTriangle className="h-5 w-5 text-amber-600 flex-shrink-0 mt-0.5" />
            <div className="text-sm">
              <p className="font-semibold text-amber-800 dark:text-amber-200">Importante:</p>
              <p className="text-amber-700 dark:text-amber-300">
                O token expira após a sessão. Faça login novamente se receber erro 401.
              </p>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Códigos de Status */}
      <Card>
        <CardHeader>
          <CardTitle>Códigos de Status HTTP</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="rounded-md border">
            <Table>
              <TableHeader>
                <TableRow>
                  <TableHead className="w-24">Código</TableHead>
                  <TableHead>Descrição</TableHead>
                  <TableHead>Quando ocorre</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                <TableRow>
                  <TableCell><Badge className="bg-green-600">200</Badge></TableCell>
                  <TableCell>OK</TableCell>
                  <TableCell className="text-sm text-muted-foreground">Requisição bem-sucedida</TableCell>
                </TableRow>
                <TableRow>
                  <TableCell><Badge className="bg-green-600">201</Badge></TableCell>
                  <TableCell>Created</TableCell>
                  <TableCell className="text-sm text-muted-foreground">Recurso criado com sucesso</TableCell>
                </TableRow>
                <TableRow>
                  <TableCell><Badge className="bg-yellow-600">400</Badge></TableCell>
                  <TableCell>Bad Request</TableCell>
                  <TableCell className="text-sm text-muted-foreground">Payload inválido ou campos obrigatórios faltando</TableCell>
                </TableRow>
                <TableRow>
                  <TableCell><Badge className="bg-red-600">401</Badge></TableCell>
                  <TableCell>Unauthorized</TableCell>
                  <TableCell className="text-sm text-muted-foreground">Token ausente ou inválido</TableCell>
                </TableRow>
                <TableRow>
                  <TableCell><Badge className="bg-red-600">403</Badge></TableCell>
                  <TableCell>Forbidden</TableCell>
                  <TableCell className="text-sm text-muted-foreground">Sem permissão para esta operação</TableCell>
                </TableRow>
                <TableRow>
                  <TableCell><Badge className="bg-red-600">404</Badge></TableCell>
                  <TableCell>Not Found</TableCell>
                  <TableCell className="text-sm text-muted-foreground">Recurso não encontrado</TableCell>
                </TableRow>
                <TableRow>
                  <TableCell><Badge className="bg-red-600">500</Badge></TableCell>
                  <TableCell>Server Error</TableCell>
                  <TableCell className="text-sm text-muted-foreground">Erro interno - entre em contato com suporte</TableCell>
                </TableRow>
              </TableBody>
            </Table>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}
