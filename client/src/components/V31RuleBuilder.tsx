import { useMemo, useState } from "react";
import { useMutation, useQuery } from "@tanstack/react-query";
import { toast } from "sonner";
import { Button } from "@/components/ui/button";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Input } from "@/components/ui/input";
import { Textarea } from "@/components/ui/textarea";
import { Badge } from "@/components/ui/badge";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import {
  AstValidationResult,
  FieldDictionaryEntry,
  FeatureDefinition,
  V31AstNode,
  listFieldDictionaryV31,
  listFeatureCatalog,
  simulateRulesV31,
  validateAstV31,
} from "@/lib/javaApi";
import { Database, Sparkles, FileJson } from "lucide-react";

type ConditionDraft = {
  id: string;
  jsonPath: string;
  dataType: string;
  operator: string;
  funcName?: string;
  valueRaw?: string;
  rangeMinRaw?: string;
  rangeMaxRaw?: string;
  sourceType: "payload" | "feature"; // NEW: track source type
  featureMetadata?: {  // NEW: metadata for derived features
    featureName: string;
    featureType: string;
    entityType?: string;
    windowName?: string;
    source: string;
  };
};

const OP_LABELS: Record<string, string> = {
  EQ: "Igual (EQ)",
  NE: "Diferente (NE)",
  GT: "Maior (GT)",
  GTE: "Maior/Igual (GTE)",
  LT: "Menor (LT)",
  LTE: "Menor/Igual (LTE)",
  IN: "Em lista (IN)",
  NOT_IN: "Não em lista (NOT_IN)",
  BETWEEN: "Entre (BETWEEN)",
  NOT_BETWEEN: "Não entre (NOT_BETWEEN)",
  CONTAINS: "Contém (CONTAINS)",
  NOT_CONTAINS: "Não contém (NOT_CONTAINS)",
  STARTS_WITH: "Começa com (STARTS_WITH)",
  ENDS_WITH: "Termina com (ENDS_WITH)",
  MATCHES_REGEX: "Regex (MATCHES_REGEX)",
  IS_NULL: "É nulo (IS_NULL)",
  IS_NOT_NULL: "Não é nulo (IS_NOT_NULL)",
  IS_TRUE: "É true (IS_TRUE)",
  IS_FALSE: "É false (IS_FALSE)",
  BEFORE: "Antes (BEFORE)",
  AFTER: "Depois (AFTER)",
  ON: "Na data (ON)",
  NOT_ON: "Não na data (NOT_ON)",
};

const FUNC_RETURN_TYPES: Record<string, string> = {
  TRIM: "string",
  LOWER: "string",
  UPPER: "string",
  NORMALIZE_TEXT: "string",
  REMOVE_DIACRITICS: "string",
  STRIP_PUNCTUATION: "string",
  BUCKET: "string",

  LEN: "number",
  ABS: "number",
  ENTROPY: "number",
  TOKEN_COUNT: "number",
  SIMILARITY_LEVENSHTEIN: "number",
  SIMILARITY_JARO: "number",
  TIME_SINCE_LAST: "number",

  IS_WEEKEND: "boolean",
  IN_TIME_RANGE: "boolean",
  SAME_DAY: "boolean",
  SAME_HOUR: "boolean",
  SAME_WEEK: "boolean",
  IS_HOLIDAY: "boolean",
  IS_BUSINESS_DAY: "boolean",
  HAS_FIELD: "boolean",
  MISSING_FIELD: "boolean",
  UNKNOWN_FIELDS_PRESENT: "boolean",
  FIELD_TYPE_MISMATCH: "boolean",
  JSON_PATH_EXISTS: "boolean",
  CONFIDENCE_GATE: "boolean",
  FUZZY_MATCH: "boolean",
  KEYWORD_BLACKLIST: "boolean",

  COALESCE: "string",
  TO_DATE_YYYYMMDD: "number",
  TO_TIME_PAD6_HHMMSS: "number",
  PARSE_GMTOFFSET: "number",
  JSON_PATH_VALUE: "object",
  FUZZY_SET_MEMBERSHIP: "object",
};

function normalizeType(t: string): "string" | "number" | "boolean" | "object" | "array" {
  const x = (t || "").toLowerCase();
  if (x.includes("bool")) return "boolean";
  if (x.includes("num") || x.includes("int") || x.includes("long") || x.includes("dec")) return "number";
  if (x.includes("arr")) return "array";
  if (x.includes("obj")) return "object";
  return "string";
}

function uuid() {
  return Math.random().toString(16).slice(2) + Date.now().toString(16);
}

function parseValue(raw: string | undefined, dataType: string) {
  const t = normalizeType(dataType);
  if (raw === undefined) return undefined;
  const s = raw.trim();
  if (s === "") return undefined;

  if (t === "number") {
    const n = Number(s.replace(",", "."));
    return Number.isFinite(n) ? n : s;
  }
  if (t === "boolean") {
    if (s.toLowerCase() === "true") return true;
    if (s.toLowerCase() === "false") return false;
    return s;
  }
  return s;
}

function buildLeftExpr(c: ConditionDraft): V31AstNode {
  const fieldNode = { type: "FIELD", jsonPath: c.jsonPath, dataType: normalizeType(c.dataType) };
  if (!c.funcName) {
    return fieldNode;
  }
  const fn = c.funcName.toUpperCase();
  return {
    type: "FUNC",
    name: fn,
    returnType: FUNC_RETURN_TYPES[fn] || "string",
    args: [fieldNode],
  };
}

function buildConditionNode(c: ConditionDraft): V31AstNode {
  const op = (c.operator || "").toUpperCase();
  const left = buildLeftExpr(c);

  if (["IS_NULL", "IS_NOT_NULL", "IS_TRUE", "IS_FALSE"].includes(op)) {
    return { type: "CONDITION", left, operator: op };
  }

  if (op === "BETWEEN" || op === "NOT_BETWEEN") {
    const min = parseValue(c.rangeMinRaw, c.dataType);
    const max = parseValue(c.rangeMaxRaw, c.dataType);
    return { type: "CONDITION", left, operator: op, right: { min, max } };
  }

  if (op === "IN" || op === "NOT_IN") {
    const raw = (c.valueRaw || "").trim();
    const items = raw
      .split(",")
      .map((x) => x.trim())
      .filter(Boolean)
      .map((x) => parseValue(x, c.dataType));
    return { type: "CONDITION", left, operator: op, right: items };
  }

  const right = parseValue(c.valueRaw, c.dataType);
  return { type: "CONDITION", left, operator: op, right };
}

export default function V31RuleBuilder() {
  const [sourceTab, setSourceTab] = useState<"payload" | "feature">("payload");
  const [fieldFilter, setFieldFilter] = useState("");
  const [featureFilter, setFeatureFilter] = useState("");
  const [featureTypeFilter, setFeatureTypeFilter] = useState("");
  const [selectedJsonPath, setSelectedJsonPath] = useState<string>("");
  const [selectedFeatureName, setSelectedFeatureName] = useState<string>("");
  const [selectedOperator, setSelectedOperator] = useState<string>("");
  const [selectedFunc, setSelectedFunc] = useState<string>("");
  const [valueRaw, setValueRaw] = useState<string>("");
  const [rangeMinRaw, setRangeMinRaw] = useState<string>("");
  const [rangeMaxRaw, setRangeMaxRaw] = useState<string>("");

  const [ruleName, setRuleName] = useState("RULE_BUILDER_TEST");
  const [decision, setDecision] = useState("SUSPEITA_DE_FRAUDE");
  const [payloadJson, setPayloadJson] = useState("{\n  \"externalTransactionId\": \"TXN-001\"\n}");

  const [conditions, setConditions] = useState<ConditionDraft[]>([]);
  const [lastValidation, setLastValidation] = useState<AstValidationResult | null>(null);

  // Fetch field dictionary
  const { data: dict, isLoading: dictLoading, isError: dictError } = useQuery({
    queryKey: ["v31-field-dictionary"],
    queryFn: () => listFieldDictionaryV31(),
    staleTime: 60_000,
    retry: 1,
  });

  // Fetch feature catalog
  const { data: features, isLoading: featuresLoading } = useQuery<FeatureDefinition[]>({
    queryKey: ["v31-feature-catalog"],
    queryFn: () => listFeatureCatalog(),
    staleTime: 60_000,
    retry: 1,
  });

  const entries = useMemo(() => (dict || []) as FieldDictionaryEntry[], [dict]);
  const featureList = useMemo(() => (features || []) as FeatureDefinition[], [features]);

  // Filter payload fields
  const filteredEntries = useMemo(() => {
    const q = fieldFilter.trim().toLowerCase();
    if (!q) return entries;
    return entries.filter((e) => (e.jsonPath || "").toLowerCase().includes(q));
  }, [entries, fieldFilter]);

  // Filter features
  const filteredFeatures = useMemo(() => {
    let list = featureList;
    if (featureTypeFilter) {
      list = list.filter((f) => f.featureType === featureTypeFilter);
    }
    const q = featureFilter.trim().toLowerCase();
    if (q) {
      list = list.filter(
        (f) =>
          f.featureName.toLowerCase().includes(q) ||
          (f.description?.toLowerCase().includes(q) ?? false)
      );
    }
    return list;
  }, [featureList, featureFilter, featureTypeFilter]);

  // Unique feature types for filter dropdown
  const featureTypes = useMemo(() => {
    const types = new Set(featureList.map((f) => f.featureType));
    return Array.from(types).sort();
  }, [featureList]);

  const selectedEntry = useMemo(
    () => entries.find((e) => e.jsonPath === selectedJsonPath) || null,
    [entries, selectedJsonPath]
  );

  const selectedFeature = useMemo(
    () => featureList.find((f) => f.featureName === selectedFeatureName) || null,
    [featureList, selectedFeatureName]
  );

  // Get operators based on source type
  const availableOperators = useMemo(() => {
    if (sourceTab === "payload" && selectedEntry) {
      return (selectedEntry.allowedOperators || []).map((o) => o.toUpperCase());
    }
    if (sourceTab === "feature" && selectedFeature) {
      return (selectedFeature.allowedOperators || []).map((o) => o.toUpperCase());
    }
    return [];
  }, [sourceTab, selectedEntry, selectedFeature]);

  const availableFunctions = useMemo(() => {
    const fns = selectedEntry?.allowedFunctions || [];
    return fns.map((f) => f.toUpperCase());
  }, [selectedEntry]);

  const ast: V31AstNode = useMemo(() => {
    return {
      type: "GROUP",
      op: "AND",
      children: conditions.map(buildConditionNode),
    };
  }, [conditions]);

  const validateMutation = useMutation({
    mutationFn: () => validateAstV31(ast),
    onSuccess: (r) => {
      setLastValidation(r);
      if (r.valid) toast.success("AST válida");
      else toast.error("AST inválida");
    },
    onError: () => toast.error("Falha ao validar AST"),
  });

  const simulateMutation = useMutation({
    mutationFn: async () => {
      let payload: any;
      try {
        payload = JSON.parse(payloadJson);
      } catch {
        throw new Error("JSON de payload inválido");
      }
      return simulateRulesV31(payload, [{ ruleName, decision, ast }]);
    },
    onSuccess: (r) => {
      toast.success(`Simulação OK: ${r.status}`);
    },
    onError: (e: any) => toast.error(e?.message || "Falha ao simular"),
  });

  const canAddCondition = !!selectedEntry && !!selectedOperator;

  const handleAdd = () => {
    if (!selectedEntry) {
      toast.error("Selecione um campo do dicionário");
      return;
    }
    if (!selectedOperator) {
      toast.error("Selecione um operador");
      return;
    }

    setConditions((prev) =>
      prev.concat({
        id: uuid(),
        jsonPath: selectedEntry.jsonPath,
        dataType: selectedEntry.type,
        operator: selectedOperator,
        funcName: selectedFunc || undefined,
        valueRaw: valueRaw || undefined,
        rangeMinRaw: rangeMinRaw || undefined,
        rangeMaxRaw: rangeMaxRaw || undefined,
      })
    );

    setValueRaw("");
    setRangeMinRaw("");
    setRangeMaxRaw("");
  };

  const handleRemove = (id: string) => setConditions((prev) => prev.filter((c) => c.id !== id));

  const copyAst = async () => {
    await navigator.clipboard.writeText(JSON.stringify(ast, null, 2));
    toast.success("AST copiada");
  };

  // Check if can add condition based on current source tab
  const canAddCondition =
    (sourceTab === "payload" && !!selectedEntry && !!selectedOperator) ||
    (sourceTab === "feature" && !!selectedFeature && !!selectedOperator);

  const handleAdd = () => {
    if (sourceTab === "payload") {
      if (!selectedEntry) {
        toast.error("Selecione um campo do dicionário");
        return;
      }
      if (!selectedOperator) {
        toast.error("Selecione um operador");
        return;
      }

      setConditions((prev) =>
        prev.concat({
          id: uuid(),
          jsonPath: selectedEntry.jsonPath,
          dataType: selectedEntry.type,
          operator: selectedOperator,
          funcName: selectedFunc || undefined,
          valueRaw: valueRaw || undefined,
          rangeMinRaw: rangeMinRaw || undefined,
          rangeMaxRaw: rangeMaxRaw || undefined,
          sourceType: "payload",
        })
      );
    } else {
      if (!selectedFeature) {
        toast.error("Selecione uma feature");
        return;
      }
      if (!selectedOperator) {
        toast.error("Selecione um operador");
        return;
      }

      setConditions((prev) =>
        prev.concat({
          id: uuid(),
          jsonPath: `$feature.${selectedFeature.featureName}`,
          dataType: selectedFeature.dataType || "number",
          operator: selectedOperator,
          funcName: undefined,
          valueRaw: valueRaw || undefined,
          rangeMinRaw: rangeMinRaw || undefined,
          rangeMaxRaw: rangeMaxRaw || undefined,
          sourceType: "feature",
          featureMetadata: {
            featureName: selectedFeature.featureName,
            featureType: selectedFeature.featureType,
            entityType: selectedFeature.entityType,
            windowName: selectedFeature.windowName,
            source: selectedFeature.source,
          },
        })
      );
    }

    setValueRaw("");
    setRangeMinRaw("");
    setRangeMaxRaw("");
  };

  return (
    <div className="space-y-6">
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Database className="h-5 w-5" />
            Rule Builder (v3.1)
          </CardTitle>
          <CardDescription>
            Constrói AST tipada usando campos do payload ou features derivadas (sem alterar o payload)
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          {/* Source Selection Tabs */}
          <Tabs value={sourceTab} onValueChange={(v) => {
            setSourceTab(v as "payload" | "feature");
            setSelectedJsonPath("");
            setSelectedFeatureName("");
            setSelectedOperator("");
            setSelectedFunc("");
          }}>
            <TabsList className="grid w-full grid-cols-2">
              <TabsTrigger value="payload" className="flex items-center gap-2">
                <FileJson className="h-4 w-4" />
                Campos do Payload
              </TabsTrigger>
              <TabsTrigger value="feature" className="flex items-center gap-2">
                <Sparkles className="h-4 w-4" />
                Features Derivadas
              </TabsTrigger>
            </TabsList>

            <TabsContent value="payload" className="space-y-4 mt-4">
              <div className="grid grid-cols-1 md:grid-cols-3 gap-3">
                <div>
                  <label className="block text-sm font-medium text-foreground mb-2">Filtrar campos</label>
                  <Input
                    value={fieldFilter}
                    onChange={(e) => setFieldFilter(e.target.value)}
                    placeholder="Ex: $.merchantName"
                  />
                </div>
                <div>
                  <label className="block text-sm font-medium text-foreground mb-2">Campo</label>
                  <select
                    value={selectedJsonPath}
                    onChange={(e) => {
                      setSelectedJsonPath(e.target.value);
                      setSelectedOperator("");
                      setSelectedFunc("");
                    }}
                    className="w-full px-3 py-2 border border-input rounded-lg bg-background text-foreground"
                    disabled={dictLoading || dictError}
                  >
                    <option value="">Selecione...</option>
                    {filteredEntries.slice(0, 500).map((e) => (
                      <option key={e.jsonPath} value={e.jsonPath}>
                        {e.jsonPath} ({e.type})
                      </option>
                    ))}
                  </select>
                  {filteredEntries.length > 500 && (
                    <p className="text-xs text-muted-foreground mt-1">Mostrando 500 primeiros (refine o filtro)</p>
                  )}
                </div>
                <div>
                  <label className="block text-sm font-medium text-foreground mb-2">Função (opcional)</label>
                  <select
                    value={selectedFunc}
                    onChange={(e) => setSelectedFunc(e.target.value)}
                    className="w-full px-3 py-2 border border-input rounded-lg bg-background text-foreground"
                    disabled={!selectedEntry}
                  >
                    <option value="">Nenhuma</option>
                    {availableFunctions.map((f) => (
                      <option key={f} value={f}>
                        {f}
                      </option>
                    ))}
                  </select>
                </div>
              </div>

              {selectedEntry && (
                <div className="rounded-lg border bg-muted/50 p-3">
                  <div className="text-sm">
                    <span className="font-semibold">Campo selecionado:</span>{" "}
                    <code className="text-primary">{selectedEntry.jsonPath}</code>
                  </div>
                  <div className="text-xs text-muted-foreground mt-1">
                    Tipo: {selectedEntry.type} | Operadores: {(selectedEntry.allowedOperators || []).join(", ")}
                  </div>
                </div>
              )}
            </TabsContent>

            <TabsContent value="feature" className="space-y-4 mt-4">
              <div className="grid grid-cols-1 md:grid-cols-3 gap-3">
                <div>
                  <label className="block text-sm font-medium text-foreground mb-2">Tipo de Feature</label>
                  <select
                    value={featureTypeFilter}
                    onChange={(e) => setFeatureTypeFilter(e.target.value)}
                    className="w-full px-3 py-2 border border-input rounded-lg bg-background text-foreground"
                  >
                    <option value="">Todos</option>
                    {featureTypes.map((t) => (
                      <option key={t} value={t}>
                        {t}
                      </option>
                    ))}
                  </select>
                </div>
                <div>
                  <label className="block text-sm font-medium text-foreground mb-2">Buscar</label>
                  <Input
                    value={featureFilter}
                    onChange={(e) => setFeatureFilter(e.target.value)}
                    placeholder="Buscar feature..."
                  />
                </div>
                <div>
                  <label className="block text-sm font-medium text-foreground mb-2">Feature</label>
                  <select
                    value={selectedFeatureName}
                    onChange={(e) => {
                      setSelectedFeatureName(e.target.value);
                      setSelectedOperator("");
                    }}
                    className="w-full px-3 py-2 border border-input rounded-lg bg-background text-foreground"
                    disabled={featuresLoading}
                  >
                    <option value="">Selecione...</option>
                    {filteredFeatures.slice(0, 200).map((f) => (
                      <option key={f.featureName} value={f.featureName}>
                        {f.featureName} ({f.featureType})
                      </option>
                    ))}
                  </select>
                </div>
              </div>

              {selectedFeature && (
                <div className="rounded-lg border bg-muted/50 p-3 space-y-2">
                  <div className="flex items-center gap-2">
                    <span className="font-semibold text-sm">Feature selecionada:</span>
                    <code className="text-primary font-mono">{selectedFeature.featureName}</code>
                    <Badge variant="outline">{selectedFeature.featureType}</Badge>
                    <Badge variant="secondary">{selectedFeature.source}</Badge>
                  </div>
                  {selectedFeature.description && (
                    <p className="text-xs text-muted-foreground">{selectedFeature.description}</p>
                  )}
                  <div className="grid grid-cols-2 md:grid-cols-4 gap-2 text-xs">
                    <div>
                      <span className="text-muted-foreground">Entidade:</span>{" "}
                      <span className="font-medium">{selectedFeature.entityType}</span>
                    </div>
                    {selectedFeature.windowName && (
                      <div>
                        <span className="text-muted-foreground">Janela:</span>{" "}
                        <span className="font-medium">{selectedFeature.windowName}</span>
                      </div>
                    )}
                    <div>
                      <span className="text-muted-foreground">Tipo:</span>{" "}
                      <span className="font-medium">{selectedFeature.dataType}</span>
                    </div>
                    <div>
                      <span className="text-muted-foreground">Refresh:</span>{" "}
                      <span className="font-medium">{selectedFeature.refreshStrategy}</span>
                    </div>
                  </div>
                  {selectedFeature.formula && (
                    <div className="text-xs">
                      <span className="text-muted-foreground">Fórmula:</span>{" "}
                      <code className="bg-muted px-1 rounded">{selectedFeature.formula}</code>
                    </div>
                  )}
                </div>
              )}
            </TabsContent>
          </Tabs>

          {/* Operator and Value Selection */}
          <div className="grid grid-cols-1 md:grid-cols-3 gap-3">
            <div>
              <label className="block text-sm font-medium text-foreground mb-2">Operador</label>
              <select
                value={selectedOperator}
                onChange={(e) => setSelectedOperator(e.target.value)}
                className="w-full px-3 py-2 border border-input rounded-lg bg-background text-foreground"
                disabled={sourceTab === "payload" ? !selectedEntry : !selectedFeature}
              >
                <option value="">Selecione...</option>
                {availableOperators.map((op) => (
                  <option key={op} value={op}>
                    {OP_LABELS[op] || op}
                  </option>
                ))}
              </select>
            </div>

            {(selectedOperator.toUpperCase() === "BETWEEN" || selectedOperator.toUpperCase() === "NOT_BETWEEN") && (
              <>
                <div>
                  <label className="block text-sm font-medium text-foreground mb-2">Mínimo</label>
                  <Input value={rangeMinRaw} onChange={(e) => setRangeMinRaw(e.target.value)} placeholder="min" />
                </div>
                <div>
                  <label className="block text-sm font-medium text-foreground mb-2">Máximo</label>
                  <Input value={rangeMaxRaw} onChange={(e) => setRangeMaxRaw(e.target.value)} placeholder="max" />
                </div>
              </>
            )}

            {!(["IS_NULL", "IS_NOT_NULL", "IS_TRUE", "IS_FALSE", "BETWEEN", "NOT_BETWEEN"].includes(selectedOperator.toUpperCase())) && selectedOperator && (
              <div className="md:col-span-2">
                <label className="block text-sm font-medium text-foreground mb-2">Valor</label>
                <Input
                  value={valueRaw}
                  onChange={(e) => setValueRaw(e.target.value)}
                  placeholder={selectedOperator.toUpperCase() === "IN" || selectedOperator.toUpperCase() === "NOT_IN" ? "a,b,c" : "valor"}
                />
              </div>
            )}
          </div>

          <div className="flex gap-2">
            <Button onClick={handleAdd} disabled={!canAddCondition}>
              Adicionar condição
            </Button>
            <Button variant="outline" onClick={() => validateMutation.mutate()} disabled={conditions.length === 0}>
              Validar AST
            </Button>
            <Button variant="outline" onClick={copyAst} disabled={conditions.length === 0}>
              Copiar AST
            </Button>
          </div>

          {lastValidation && !lastValidation.valid && (
            <div className="rounded-lg border border-red-200 bg-red-50 p-3 text-red-800" role="alert">
              <div className="font-semibold mb-2">Erros de validação</div>
              <ul className="list-disc pl-5 space-y-1">
                {lastValidation.errors?.map((e, idx) => (
                  <li key={idx}>
                    {e.path}: {e.message}
                  </li>
                ))}
              </ul>
            </div>
          )}
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle>Condições</CardTitle>
          <CardDescription>{conditions.length} condição(ões) no grupo AND</CardDescription>
        </CardHeader>
        <CardContent>
          {conditions.length === 0 ? (
            <p className="text-muted-foreground">Adicione pelo menos uma condição.</p>
          ) : (
            <div className="space-y-2">
              {conditions.map((c) => (
                <div key={c.id} className="flex items-center justify-between rounded-lg border p-3">
                  <div className="text-sm space-y-1">
                    <div className="flex items-center gap-2">
                      {c.sourceType === "feature" ? (
                        <Badge variant="secondary" className="text-xs">
                          <Sparkles className="h-3 w-3 mr-1" />
                          Feature
                        </Badge>
                      ) : (
                        <Badge variant="outline" className="text-xs">
                          <FileJson className="h-3 w-3 mr-1" />
                          Payload
                        </Badge>
                      )}
                      <span className="font-mono">{c.jsonPath}</span>
                      {c.funcName ? <span className="text-muted-foreground">({c.funcName})</span> : null}
                    </div>
                    <div className="text-muted-foreground">
                      <span className="font-medium text-foreground">{c.operator}</span>
                      {" "}
                      {c.operator.toUpperCase() === "BETWEEN" || c.operator.toUpperCase() === "NOT_BETWEEN" ? (
                        <span>{c.rangeMinRaw} .. {c.rangeMaxRaw}</span>
                      ) : ["IS_NULL", "IS_NOT_NULL", "IS_TRUE", "IS_FALSE"].includes(c.operator.toUpperCase()) ? (
                        <span>(sem valor)</span>
                      ) : (
                        <span>{c.valueRaw}</span>
                      )}
                    </div>
                    {c.featureMetadata && (
                      <div className="text-xs text-muted-foreground">
                        {c.featureMetadata.entityType && <span className="mr-2">Entidade: {c.featureMetadata.entityType}</span>}
                        {c.featureMetadata.windowName && <span className="mr-2">Janela: {c.featureMetadata.windowName}</span>}
                      </div>
                    )}
                  </div>
                  <Button variant="outline" size="sm" onClick={() => handleRemove(c.id)}>
                    Remover
                  </Button>
                </div>
              ))}
            </div>
          )}
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle>Simulação (v3.1)</CardTitle>
          <CardDescription>Executa /api/rules/simulate com 1 regra</CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
            <div>
              <label className="block text-sm font-medium text-foreground mb-2">Rule Name</label>
              <Input value={ruleName} onChange={(e) => setRuleName(e.target.value)} />
            </div>
            <div>
              <label className="block text-sm font-medium text-foreground mb-2">Decision</label>
              <select
                value={decision}
                onChange={(e) => setDecision(e.target.value)}
                className="w-full px-3 py-2 border border-input rounded-lg bg-background text-foreground"
              >
                <option value="APROVADA">APROVADA</option>
                <option value="SUSPEITA_DE_FRAUDE">SUSPEITA_DE_FRAUDE</option>
                <option value="FRAUDE">FRAUDE</option>
              </select>
            </div>
          </div>

          <div>
            <label className="block text-sm font-medium text-foreground mb-2">Payload (JSON)</label>
            <Textarea value={payloadJson} onChange={(e) => setPayloadJson(e.target.value)} className="min-h-40" />
          </div>

          <div className="flex gap-2">
            <Button
              onClick={() => simulateMutation.mutate()}
              disabled={conditions.length === 0 || validateMutation.isPending || simulateMutation.isPending}
            >
              Simular
            </Button>
            <Button
              variant="outline"
              onClick={() => validateMutation.mutate()}
              disabled={conditions.length === 0 || validateMutation.isPending}
            >
              Validar antes
            </Button>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}
