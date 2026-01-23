/**
 * RulesLibrary.tsx - Biblioteca de 60+ Regras de Exemplo do RULEX
 *
 * QUADRUPLE CHECK 10000X - Biblioteca completa de regras de fraude
 *
 * Organização:
 * - 15 regras SIMPLES (básicas, uma condição)
 * - 15 regras MÉDIAS (2-3 condições combinadas)
 * - 20 regras COMPLEXAS (grupos aninhados, múltiplas condições)
 * - 10 regras EXTREMAMENTE COMPLEXAS (cenários avançados de fraude)
 *
 * Cada regra contém:
 * - Narrativa do dia a dia (contexto real)
 * - Passo-a-passo na UI (como criar)
 * - JSON real (estrutura de dados)
 * - Payloads de exemplo (dispara / não dispara)
 * - Resultado esperado (decisão)
 */
import { useState, useMemo } from "react";
import { ChevronRight, Copy, Check, Search, Filter, PlayCircle, XCircle } from "lucide-react";

import { Badge } from "@/components/ui/badge";
import { Button } from "@/components/ui/button";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Input } from "@/components/ui/input";
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from "@/components/ui/select";
import {
  Accordion,
  AccordionContent,
  AccordionItem,
  AccordionTrigger,
} from "@/components/ui/accordion";
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
  DialogTrigger,
} from "@/components/ui/dialog";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import {
  RULES_LIBRARY,
  RULES_LIBRARY_STATS,
  RuleCategory,
  RuleComplexity,
  RuleExample,
} from "@/manual/data/rulesLibraryData";
// ============================================================================
// COMPONENTE: BIBLIOTECA DE REGRAS
// ============================================================================

function CopyButton({ text }: { text: string }) {
  const [copied, setCopied] = useState(false);

  const handleCopy = async () => {
    await navigator.clipboard.writeText(text);
    setCopied(true);
    setTimeout(() => setCopied(false), 2000);
  };

  return (
    <Button variant="ghost" size="sm" onClick={handleCopy}>
      {copied ? <Check className="h-4 w-4" /> : <Copy className="h-4 w-4" />}
    </Button>
  );
}

function RuleDetailDialog({ rule }: { rule: RuleExample }) {
  return (
    <DialogContent className="max-w-4xl max-h-[90vh] overflow-y-auto">
      <DialogHeader>
        <DialogTitle className="flex items-center gap-3">
          <Badge
            variant={
              rule.complexity === "simples"
                ? "secondary"
                : rule.complexity === "media"
                ? "default"
                : rule.complexity === "complexa"
                ? "destructive"
                : "outline"
            }
          >
            {rule.complexity.toUpperCase()}
          </Badge>
          {rule.name}
        </DialogTitle>
        <DialogDescription>ID: {rule.id} | Categoria: {rule.category}</DialogDescription>
      </DialogHeader>

      <Tabs defaultValue="narrativa" className="mt-4">
        <TabsList className="grid grid-cols-5 w-full">
          <TabsTrigger value="narrativa">📖 Narrativa</TabsTrigger>
          <TabsTrigger value="passos">👆 Passo-a-Passo</TabsTrigger>
          <TabsTrigger value="json">💻 JSON</TabsTrigger>
          <TabsTrigger value="payloads">📦 Payloads</TabsTrigger>
          <TabsTrigger value="info">ℹ️ Info</TabsTrigger>
        </TabsList>

        <TabsContent value="narrativa" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle className="text-base">🎯 Situação</CardTitle>
            </CardHeader>
            <CardContent>
              <p className="text-sm">{rule.narrativa.situacao}</p>
            </CardContent>
          </Card>
          <Card>
            <CardHeader>
              <CardTitle className="text-base">⚠️ Problema</CardTitle>
            </CardHeader>
            <CardContent>
              <p className="text-sm">{rule.narrativa.problema}</p>
            </CardContent>
          </Card>
          <Card>
            <CardHeader>
              <CardTitle className="text-base">✅ Solução</CardTitle>
            </CardHeader>
            <CardContent>
              <p className="text-sm">{rule.narrativa.solucao}</p>
            </CardContent>
          </Card>
        </TabsContent>

        <TabsContent value="passos" className="space-y-2">
          <Card>
            <CardHeader>
              <CardTitle className="text-base">Como criar esta regra na UI</CardTitle>
            </CardHeader>
            <CardContent>
              <ol className="list-none space-y-2">
                {rule.passoAPasso.map((passo, idx) => (
                  <li key={idx} className="text-sm flex items-start gap-2">
                    <ChevronRight className="h-4 w-4 mt-0.5 text-primary shrink-0" />
                    <span>{passo}</span>
                  </li>
                ))}
              </ol>
            </CardContent>
          </Card>
        </TabsContent>

        <TabsContent value="json">
          <Card>
            <CardHeader className="flex flex-row items-center justify-between">
              <CardTitle className="text-base">JSON da Regra</CardTitle>
              <CopyButton text={JSON.stringify(rule.json, null, 2)} />
            </CardHeader>
            <CardContent>
              <pre className="bg-muted p-4 rounded-lg text-xs overflow-x-auto">
                {JSON.stringify(rule.json, null, 2)}
              </pre>
            </CardContent>
          </Card>
        </TabsContent>

        <TabsContent value="payloads" className="space-y-4">
          {rule.payloads.map((payload, idx) => (
            <Card key={idx} className={payload.shouldTrigger ? "border-red-500" : "border-green-500"}>
              <CardHeader className="flex flex-row items-center justify-between pb-2">
                <div className="flex items-center gap-2">
                  {payload.shouldTrigger ? (
                    <PlayCircle className="h-5 w-5 text-red-500" />
                  ) : (
                    <XCircle className="h-5 w-5 text-green-500" />
                  )}
                  <CardTitle className="text-base">{payload.description}</CardTitle>
                </div>
                <CopyButton text={JSON.stringify(payload.data, null, 2)} />
              </CardHeader>
              <CardContent>
                <pre className="bg-muted p-4 rounded-lg text-xs overflow-x-auto">
                  {JSON.stringify(payload.data, null, 2)}
                </pre>
              </CardContent>
            </Card>
          ))}
        </TabsContent>

        <TabsContent value="info" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle className="text-base">Resultado Esperado</CardTitle>
            </CardHeader>
            <CardContent>
              <code className="bg-muted px-2 py-1 rounded text-sm">{rule.resultadoEsperado}</code>
            </CardContent>
          </Card>

          <Card>
            <CardHeader>
              <CardTitle className="text-base">Operadores Usados</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="flex flex-wrap gap-2">
                {rule.operadoresUsados.map((op) => (
                  <Badge key={op} variant="outline">
                    {op}
                  </Badge>
                ))}
              </div>
            </CardContent>
          </Card>

          <Card>
            <CardHeader>
              <CardTitle className="text-base">Tags</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="flex flex-wrap gap-2">
                {rule.tags.map((tag) => (
                  <Badge key={tag} variant="secondary">
                    {tag}
                  </Badge>
                ))}
              </div>
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>
    </DialogContent>
  );
}

export function RulesLibrary() {
  const [searchTerm, setSearchTerm] = useState("");
  const [complexityFilter, setComplexityFilter] = useState<RuleComplexity | "all">("all");
  const [categoryFilter, setCategoryFilter] = useState<RuleCategory | "all">("all");

  const filteredRules = useMemo(() => {
    return RULES_LIBRARY.filter((rule) => {
      const matchesSearch =
        searchTerm === "" ||
        rule.name.toLowerCase().includes(searchTerm.toLowerCase()) ||
        rule.narrativa.situacao.toLowerCase().includes(searchTerm.toLowerCase()) ||
        rule.tags.some((tag) => tag.toLowerCase().includes(searchTerm.toLowerCase()));

      const matchesComplexity =
        complexityFilter === "all" || rule.complexity === complexityFilter;

      const matchesCategory =
        categoryFilter === "all" || rule.category === categoryFilter;

      return matchesSearch && matchesComplexity && matchesCategory;
    });
  }, [searchTerm, complexityFilter, categoryFilter]);

  const categories = Array.from(new Set(RULES_LIBRARY.map((r) => r.category)));

  return (
    <div className="space-y-6">
      {/* Header */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            📚 Biblioteca de Regras de Exemplo
          </CardTitle>
          <CardDescription>
            {RULES_LIBRARY_STATS.total} regras completas: {RULES_LIBRARY_STATS.simples} simples,{" "}
            {RULES_LIBRARY_STATS.medias} médias, {RULES_LIBRARY_STATS.complexas} complexas,{" "}
            {RULES_LIBRARY_STATS.extremas} extremas
          </CardDescription>
        </CardHeader>
        <CardContent>
          {/* Filtros */}
          <div className="flex flex-col md:flex-row gap-4">
            <div className="flex-1">
              <div className="relative">
                <Search className="absolute left-3 top-1/2 -translate-y-1/2 h-4 w-4 text-muted-foreground" />
                <Input
                  placeholder="Buscar por nome, descrição ou tag..."
                  value={searchTerm}
                  onChange={(e) => setSearchTerm(e.target.value)}
                  className="pl-10"
                />
              </div>
            </div>
            <Select
              value={complexityFilter}
              onValueChange={(v) => setComplexityFilter(v as RuleComplexity | "all")}
            >
              <SelectTrigger className="w-[180px]">
                <Filter className="h-4 w-4 mr-2" />
                <SelectValue placeholder="Complexidade" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="all">Todas</SelectItem>
                <SelectItem value="simples">Simples</SelectItem>
                <SelectItem value="media">Média</SelectItem>
                <SelectItem value="complexa">Complexa</SelectItem>
                <SelectItem value="extrema">Extrema</SelectItem>
              </SelectContent>
            </Select>
            <Select
              value={categoryFilter}
              onValueChange={(v) => setCategoryFilter(v as RuleCategory | "all")}
            >
              <SelectTrigger className="w-[180px]">
                <Filter className="h-4 w-4 mr-2" />
                <SelectValue placeholder="Categoria" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="all">Todas</SelectItem>
                {categories.map((cat) => (
                  <SelectItem key={cat} value={cat}>
                    {cat}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>
        </CardContent>
      </Card>

      {/* Lista de Regras por Complexidade */}
      <Accordion type="multiple" defaultValue={["simples", "media", "complexa", "extrema"]}>
        {(["simples", "media", "complexa", "extrema"] as const).map((complexity) => {
          const rulesInGroup = filteredRules.filter((r) => r.complexity === complexity);
          if (rulesInGroup.length === 0) return null;

          return (
            <AccordionItem key={complexity} value={complexity}>
              <AccordionTrigger className="hover:no-underline">
                <div className="flex items-center gap-3">
                  <Badge
                    variant={
                      complexity === "simples"
                        ? "secondary"
                        : complexity === "media"
                        ? "default"
                        : complexity === "complexa"
                        ? "destructive"
                        : "outline"
                    }
                  >
                    {complexity.toUpperCase()}
                  </Badge>
                  <span>
                    {complexity === "simples" && "Regras Simples"}
                    {complexity === "media" && "Regras Médias"}
                    {complexity === "complexa" && "Regras Complexas"}
                    {complexity === "extrema" && "Regras Extremamente Complexas"}
                  </span>
                  <Badge variant="outline">{rulesInGroup.length}</Badge>
                </div>
              </AccordionTrigger>
              <AccordionContent>
                <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-3 pt-4">
                  {rulesInGroup.map((rule) => (
                    <Dialog key={rule.id}>
                      <DialogTrigger asChild>
                        <Card
                          id={`manual-example-${rule.id}`}
                          className="cursor-pointer hover:shadow-lg transition-shadow hover:border-primary"
                        >
                          <CardHeader className="pb-2">
                            <div className="flex items-center justify-between">
                              <Badge variant="outline">{rule.id}</Badge>
                              <Badge variant="secondary">{rule.category}</Badge>
                            </div>
                            <CardTitle className="text-base">{rule.name}</CardTitle>
                          </CardHeader>
                          <CardContent>
                            <p className="text-sm text-muted-foreground line-clamp-2">
                              {rule.narrativa.situacao}
                            </p>
                            <div className="flex flex-wrap gap-1 mt-2">
                              {rule.tags.slice(0, 3).map((tag) => (
                                <Badge key={tag} variant="outline" className="text-xs">
                                  {tag}
                                </Badge>
                              ))}
                            </div>
                          </CardContent>
                        </Card>
                      </DialogTrigger>
                      <RuleDetailDialog rule={rule} />
                    </Dialog>
                  ))}
                </div>
              </AccordionContent>
            </AccordionItem>
          );
        })}
      </Accordion>

      {filteredRules.length === 0 && (
        <Card>
          <CardContent className="py-8 text-center">
            <p className="text-muted-foreground">
              Nenhuma regra encontrada com os filtros selecionados.
            </p>
          </CardContent>
        </Card>
      )}
    </div>
  );
}
