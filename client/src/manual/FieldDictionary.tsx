/**
 * FieldDictionary.tsx - Dicionário completo dos campos do payload do RULEX
 *
 * Objetivo:
 * - Mostrar TODOS os campos disponíveis no payload
 * - Exibir descrição (quando disponível), valores esperados (supostos) e exemplo didático
 *
 * Fontes:
 * - client/src/lib/fieldLabels.ts: lista canônica de nomes + labels (garante completude)
 * - client/src/manual/generated/fieldDictionary.generated.ts: metadados gerados (descrição/constraints/exemplos)
 */
import { useEffect, useMemo, useState } from "react";

import {
  Accordion,
  AccordionContent,
  AccordionItem,
  AccordionTrigger,
} from "@/components/ui/accordion";
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

import { FIELD_LABELS } from "@/lib/fieldLabels";
import {
  FIELD_DICTIONARY_BY_NAME,
  type FieldDictionaryItem as GeneratedFieldDictionaryItem,
} from "@/manual/generated";

type FieldRow = {
  name: string;
  label: string;
  required: boolean;
  type: string;
  format?: string;
  description?: string;
  constraints: string[];
  possibleValues: string[];
  example?: string;
  category: string;
  categoryIcon: string;
};

function normalizeText(value: string): string {
  return value
    .normalize("NFD")
    .replace(/[\u0300-\u036f]/g, "")
    .toLowerCase();
}

function classifyField(name: string, label: string): { category: string; icon: string } {
  const n = normalizeText(name);
  const l = normalizeText(label);

  const has = (...tokens: string[]) => tokens.some((t) => n.includes(t) || l.includes(t));

  if (has("transaction", "transacao", "txn")) return { category: "Transação", icon: "💳" };
  if (has("customer", "client", "cliente")) return { category: "Cliente", icon: "👤" };
  if (has("pan", "card", "cartao")) return { category: "Cartão", icon: "💳" };
  if (has("token", "tokeniz")) return { category: "Token", icon: "🔑" };
  if (has("merchant", "mcc", "loja")) return { category: "Merchant", icon: "🏪" };
  if (has("terminal", "pos", "atm")) return { category: "Terminal/POS/ATM", icon: "🖥️" };
  if (has("auth", "authentication", "autoriz")) return { category: "Autorização/Autenticação", icon: "🔐" };
  if (has("cvv")) return { category: "CVV", icon: "🔢" };
  if (has("avs")) return { category: "AVS", icon: "📍" };
  if (has("pin")) return { category: "PIN", icon: "🔒" };
  if (has("emv", "cryptogram", "aip", "tvr", "atc")) return { category: "EMV/Criptografia", icon: "📟" };
  if (has("acquirer", "adquir")) return { category: "Adquirente", icon: "🏦" };
  if (has("network", "rede")) return { category: "Network", icon: "🌐" };
  if (has("score", "risk", "fraud")) return { category: "Scores", icon: "📊" };
  if (has("workflow", "portfolio")) return { category: "Workflow/Portfolio", icon: "⚙️" };
  if (has("credit", "limit", "credito")) return { category: "Crédito", icon: "💰" };
  if (has("geo", "latitude", "longitude", "ip", "device", "useragent", "session"))
    return { category: "Dispositivo/Geo", icon: "📡" };

  return { category: "Outros", icon: "📁" };
}

function inferFallbackType(name: string): { type: string; format?: string } {
  const n = name.toLowerCase();
  if (n.includes("date") || n.includes("time")) return { type: "string", format: "date-time" };
  if (n.includes("amount") || n.includes("score") || n.includes("count") || n.includes("limit"))
    return { type: "number" };
  if (n.includes("present") || n.startsWith("is") || n.includes("indicator") || n.includes("flag"))
    return { type: "boolean" };
  if (n.includes("id") || n.includes("code")) return { type: "string" };
  return { type: "string" };
}

function formatExpectedValues(item: FieldRow): string {
  if (item.possibleValues.length > 0) return item.possibleValues.join(", ");

  const parts: string[] = [];

  if (item.type === "boolean") {
    parts.push("true | false");
  } else if (item.type === "integer") {
    parts.push("inteiro");
  } else if (item.type === "number") {
    parts.push("numérico");
  } else {
    parts.push("texto");
  }

  if (item.format) parts.push(`formato: ${item.format}`);

  // Heurísticas de domínio (supostas)
  const n = item.name.toLowerCase();
  if (n.includes("country")) parts.push("código de país (ex.: ISO)");
  if (n.includes("currency")) parts.push("código de moeda (ex.: ISO)");
  if (n.includes("mcc")) parts.push("código MCC");
  if (n.includes("response") || n.includes("decision") || n.endsWith("code")) parts.push("código/enumerado do sistema");
  if (n.includes("id")) parts.push("identificador");

  if (item.constraints.length > 0) {
    parts.push(item.constraints.join("; "));
  }

  return parts.join(" • ");
}

function formatExample(item: FieldRow): string {
  if (item.example) return item.example;
  if (item.type === "boolean") return "true";
  if (item.type === "integer") return "123";
  if (item.type === "number") return "123.45";
  return '"EXEMPLO"';
}

export interface FieldDictionaryProps {
  highlightField?: string;
}

export function FieldDictionary({ highlightField }: FieldDictionaryProps) {
  const [search, setSearch] = useState("");
  const [openCategories, setOpenCategories] = useState<string[]>([]);

  useEffect(() => {
    if (!highlightField) return;
    setSearch(highlightField);
    // Abre automaticamente a categoria inferida do campo destacado
    const label = FIELD_LABELS[highlightField as keyof typeof FIELD_LABELS] ?? highlightField;
    const { category } = classifyField(highlightField, label);
    setOpenCategories((prev) => (prev.includes(category) ? prev : [category, ...prev]));
  }, [highlightField]);

  const totalFields = Object.keys(FIELD_LABELS).length;

  const allRows = useMemo((): FieldRow[] => {
    const names = Object.keys(FIELD_LABELS).sort();
    return names.map((name) => {
      const label = FIELD_LABELS[name as keyof typeof FIELD_LABELS] ?? name;
      const generated = (FIELD_DICTIONARY_BY_NAME as Record<string, GeneratedFieldDictionaryItem | undefined>)[name];
      const fallbackType = inferFallbackType(name);
      const classification = classifyField(name, label);

      return {
        name,
        label,
        required: generated?.required ?? false,
        type: generated?.type ?? fallbackType.type,
        format: generated?.format ?? fallbackType.format,
        description: generated?.description,
        constraints: generated?.constraints ?? [],
        possibleValues: generated?.possibleValues ?? [],
        example: generated?.example,
        category: classification.category,
        categoryIcon: classification.icon,
      };
    });
  }, []);

  const grouped = useMemo(() => {
    const term = search.trim();
    const rows = !term
      ? allRows
      : allRows.filter((row) => {
          const haystack = [
            row.name,
            row.label,
            row.description ?? "",
            row.category,
            row.type,
            row.format ?? "",
            row.constraints.join(" "),
            row.possibleValues.join(" "),
            row.example ?? "",
          ].join("\n");
          return normalizeText(haystack).includes(normalizeText(term));
        });

    const byCategory = new Map<string, FieldRow[]>();
    for (const row of rows) {
      const list = byCategory.get(row.category) ?? [];
      list.push(row);
      byCategory.set(row.category, list);
    }

    const categories = Array.from(byCategory.entries())
      .map(([category, rows]) => ({
        category,
        icon: rows[0]?.categoryIcon ?? "📁",
        rows: rows.sort((a, b) => a.name.localeCompare(b.name)),
      }))
      .sort((a, b) => a.category.localeCompare(b.category));

    return {
      categories,
      filteredCount: rows.length,
    };
  }, [allRows, search]);

  // UX: abre a primeira categoria por padrão, para o usuário já ver a tabela.
  useEffect(() => {
    if (openCategories.length > 0) return;
    if (search.trim()) return;
    const first = grouped.categories[0]?.category;
    if (first) setOpenCategories([first]);
  }, [grouped.categories, openCategories.length, search]);

  // UX: ao buscar, expande automaticamente todas as categorias com resultados.
  useEffect(() => {
    if (!search.trim()) return;
    const categories = grouped.categories.map((c) => c.category);
    setOpenCategories(categories);
  }, [grouped.categories, search]);

  return (
    <div className="space-y-6">
      {/* Header com estatísticas */}
      <Card>
        <CardHeader className="pb-3">
          <div className="flex items-center justify-between">
            <div>
              <CardTitle className="flex items-center gap-2">
                📋 Dicionário de Campos do Payload
                <Badge variant="secondary" className="text-lg">
                  {totalFields}
                </Badge>
              </CardTitle>
              <CardDescription>
                Todos os campos disponíveis para uso em condições de regras (com descrição, valores esperados e exemplo)
              </CardDescription>
            </div>
          </div>
        </CardHeader>
        <CardContent>
          {/* Busca */}
          <div className="space-y-2">
            <Input
              placeholder="Buscar por nome do campo ou label..."
              value={search}
              onChange={(e) => setSearch(e.target.value)}
            />
            <div className="text-sm text-muted-foreground">
              Exibindo{" "}
              <span className="font-semibold text-foreground">{grouped.filteredCount}</span> de{" "}
              <span className="font-semibold text-foreground">{totalFields}</span>{" "}
              campos em{" "}
              <span className="font-semibold text-foreground">
                {grouped.categories.length}
              </span>{" "}
              categorias
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Accordion por categoria */}
      <Accordion
        type="multiple"
        value={openCategories}
        onValueChange={setOpenCategories}
        className="space-y-2"
      >
        {grouped.categories.map((category) => {
          const icon = category.icon || "📁";

          return (
            <AccordionItem
              key={category.category}
              value={category.category}
              className="border rounded-lg px-4"
            >
              <AccordionTrigger className="hover:no-underline">
                <div className="flex items-center gap-3">
                  <span className="text-2xl">{icon}</span>
                  <span className="font-semibold">{category.category}</span>
                  <Badge variant="outline">{category.rows.length}</Badge>
                </div>
              </AccordionTrigger>
              <AccordionContent>
                <div className="rounded-md border overflow-auto">
                  <Table>
                    <TableHeader>
                      <TableRow>
                        <TableHead className="w-[260px]">Nome do Campo (API)</TableHead>
                        <TableHead className="w-[240px]">Label (PT-BR)</TableHead>
                        <TableHead className="w-[120px]">Tipo</TableHead>
                        <TableHead className="w-[120px]">Obrigatório</TableHead>
                        <TableHead className="min-w-[340px]">Descrição</TableHead>
                        <TableHead className="min-w-[260px]">Valores esperados (supostos)</TableHead>
                        <TableHead className="w-[240px]">Exemplo</TableHead>
                      </TableRow>
                    </TableHeader>
                    <TableBody>
                      {category.rows.map((row) => {
                        const isHighlighted = highlightField === row.name;
                        const description =
                          row.description ??
                          `Campo do payload: ${row.label}.`; 

                        const typeLabel = row.format ? `${row.type} (${row.format})` : row.type;

                        return (
                          <TableRow
                            key={row.name}
                            id={`manual-field-${row.name}`}
                            className={
                              isHighlighted
                                ? "bg-primary/10 ring-2 ring-primary ring-inset"
                                : undefined
                            }
                          >
                            <TableCell className="font-mono text-sm align-top">{row.name}</TableCell>
                            <TableCell className="align-top">{row.label}</TableCell>
                            <TableCell className="align-top">
                              <Badge variant="outline" className="text-xs">
                                {typeLabel}
                              </Badge>
                            </TableCell>
                            <TableCell className="align-top">
                              <Badge variant={row.required ? "default" : "secondary"} className="text-xs">
                                {row.required ? "Sim" : "Não"}
                              </Badge>
                            </TableCell>
                            <TableCell className="align-top">
                              <div className="space-y-1">
                                <div>{description}</div>
                                {row.constraints.length > 0 && (
                                  <div className="text-xs text-muted-foreground">
                                    {row.constraints.join(" • ")}
                                  </div>
                                )}
                              </div>
                            </TableCell>
                            <TableCell className="align-top">
                              <div className="text-sm text-muted-foreground">
                                {formatExpectedValues(row)}
                              </div>
                            </TableCell>
                            <TableCell className="align-top">
                              <code className="font-mono text-xs whitespace-pre-wrap">
                                {formatExample(row)}
                              </code>
                            </TableCell>
                          </TableRow>
                        );
                      })}
                    </TableBody>
                  </Table>
                </div>
              </AccordionContent>
            </AccordionItem>
          );
        })}
      </Accordion>

      {/* Mensagem quando nenhum resultado */}
      {grouped.filteredCount === 0 && (
        <Card className="py-8">
          <CardContent className="flex flex-col items-center justify-center text-center">
            <span className="text-4xl mb-4">🔍</span>
            <p className="text-lg font-medium">Nenhum campo encontrado</p>
            <p className="text-muted-foreground">
              Tente ajustar o termo de busca
            </p>
            <button
              onClick={() => setSearch("")}
              className="mt-4 text-sm text-primary hover:underline"
            >
              Limpar busca
            </button>
          </CardContent>
        </Card>
      )}

      {/* Legenda de tipos */}
      <Card>
        <CardHeader className="pb-2">
          <CardTitle className="text-sm">Legenda de Tipos</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="flex flex-wrap gap-4 text-sm">
            <div className="flex items-center gap-2">
              <Badge variant="outline">string</Badge>
              <span className="text-muted-foreground">Texto</span>
            </div>
            <div className="flex items-center gap-2">
              <Badge variant="outline">number</Badge>
              <span className="text-muted-foreground">Número</span>
            </div>
            <div className="flex items-center gap-2">
              <Badge variant="outline">date/time</Badge>
              <span className="text-muted-foreground">Data ou Hora</span>
            </div>
            <div className="flex items-center gap-2">
              <Badge variant="outline">boolean</Badge>
              <span className="text-muted-foreground">Verdadeiro/Falso</span>
            </div>
            <div className="flex items-center gap-2">
              <Badge variant="outline">geo</Badge>
              <span className="text-muted-foreground">Coordenada Geográfica</span>
            </div>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}

export default FieldDictionary;
