import { useState } from "react";
import { useQuery } from "@tanstack/react-query";
import {
  listFeatureCatalog,
  getFeatureTypes,
  getEntityTypes,
  getFeatureSources,
  type FeatureDefinition,
} from "@/lib/javaApi";
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";
import { Input } from "@/components/ui/input";
import { Badge } from "@/components/ui/badge";
import { Button } from "@/components/ui/button";
import { RefreshCw, Search, Database, Clock, GitBranch, MapPin, Type, Shield, Sparkles } from "lucide-react";
import { toast } from "sonner";

const FEATURE_TYPE_ICONS: Record<string, React.ReactNode> = {
  PAYLOAD_FIELD: <Database className="h-4 w-4" />,
  TEMPORAL: <Clock className="h-4 w-4" />,
  VELOCITY: <Sparkles className="h-4 w-4" />,
  GRAPH: <GitBranch className="h-4 w-4" />,
  GEO: <MapPin className="h-4 w-4" />,
  TEXT: <Type className="h-4 w-4" />,
  SCHEMA: <Shield className="h-4 w-4" />,
  DERIVED: <Sparkles className="h-4 w-4" />,
  CONTEXTUAL: <Database className="h-4 w-4" />,
};

const FEATURE_TYPE_COLORS: Record<string, string> = {
  PAYLOAD_FIELD: "bg-blue-100 text-blue-800",
  TEMPORAL: "bg-purple-100 text-purple-800",
  VELOCITY: "bg-orange-100 text-orange-800",
  GRAPH: "bg-green-100 text-green-800",
  GEO: "bg-cyan-100 text-cyan-800",
  TEXT: "bg-yellow-100 text-yellow-800",
  SCHEMA: "bg-red-100 text-red-800",
  DERIVED: "bg-pink-100 text-pink-800",
  CONTEXTUAL: "bg-indigo-100 text-indigo-800",
};

const SOURCE_COLORS: Record<string, string> = {
  payload: "bg-blue-50 text-blue-700 border-blue-200",
  velocity_store: "bg-orange-50 text-orange-700 border-orange-200",
  feature_store: "bg-green-50 text-green-700 border-green-200",
  runtime: "bg-purple-50 text-purple-700 border-purple-200",
};

export default function FeatureCatalog() {
  const [filters, setFilters] = useState({
    featureType: "",
    entityType: "",
    source: "",
  });
  const [searchTerm, setSearchTerm] = useState("");

  // Fetch feature catalog
  const {
    data: features = [],
    isLoading,
    refetch,
    error,
  } = useQuery<FeatureDefinition[]>({
    queryKey: ["feature-catalog", filters],
    queryFn: () =>
      listFeatureCatalog({
        featureType: filters.featureType || undefined,
        entityType: filters.entityType || undefined,
        source: filters.source || undefined,
      }),
  });

  // Fetch filter options
  const { data: featureTypes = [] } = useQuery<string[]>({
    queryKey: ["feature-types"],
    queryFn: getFeatureTypes,
  });

  const { data: entityTypes = [] } = useQuery<string[]>({
    queryKey: ["entity-types"],
    queryFn: getEntityTypes,
  });

  const { data: sources = [] } = useQuery<string[]>({
    queryKey: ["feature-sources"],
    queryFn: getFeatureSources,
  });

  // Filter by search term
  const filteredFeatures = features.filter(
    (f) =>
      f.featureName.toLowerCase().includes(searchTerm.toLowerCase()) ||
      (f.description?.toLowerCase().includes(searchTerm.toLowerCase()) ?? false)
  );

  // Stats
  const stats = {
    total: features.length,
    payloadFields: features.filter((f) => f.featureType === "PAYLOAD_FIELD").length,
    velocity: features.filter((f) => f.featureType === "VELOCITY").length,
    temporal: features.filter((f) => f.featureType === "TEMPORAL").length,
    graph: features.filter((f) => f.featureType === "GRAPH").length,
  };

  if (error) {
    toast.error("Erro ao carregar catálogo de features");
  }

  return (
    <div className="container mx-auto py-6 space-y-6">
      {/* Header */}
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-3xl font-bold">Feature Catalog</h1>
          <p className="text-muted-foreground">
            Catálogo oficial de features determinísticas para regras duras
          </p>
        </div>
        <Button variant="outline" onClick={() => refetch()}>
          <RefreshCw className="h-4 w-4 mr-2" />
          Atualizar
        </Button>
      </div>

      {/* Stats Cards */}
      <div className="grid gap-4 md:grid-cols-5">
        <Card>
          <CardHeader className="pb-2">
            <CardDescription>Total Features</CardDescription>
            <CardTitle className="text-2xl">{stats.total}</CardTitle>
          </CardHeader>
        </Card>
        <Card>
          <CardHeader className="pb-2">
            <CardDescription>Payload Fields</CardDescription>
            <CardTitle className="text-2xl text-blue-600">{stats.payloadFields}</CardTitle>
          </CardHeader>
        </Card>
        <Card>
          <CardHeader className="pb-2">
            <CardDescription>Velocity</CardDescription>
            <CardTitle className="text-2xl text-orange-600">{stats.velocity}</CardTitle>
          </CardHeader>
        </Card>
        <Card>
          <CardHeader className="pb-2">
            <CardDescription>Temporal</CardDescription>
            <CardTitle className="text-2xl text-purple-600">{stats.temporal}</CardTitle>
          </CardHeader>
        </Card>
        <Card>
          <CardHeader className="pb-2">
            <CardDescription>Graph</CardDescription>
            <CardTitle className="text-2xl text-green-600">{stats.graph}</CardTitle>
          </CardHeader>
        </Card>
      </div>

      {/* Filters */}
      <Card>
        <CardHeader>
          <CardTitle className="text-lg">Filtros</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="flex flex-wrap gap-4">
            <div className="flex-1 min-w-[200px]">
              <div className="relative">
                <Search className="absolute left-3 top-1/2 -translate-y-1/2 h-4 w-4 text-muted-foreground" />
                <Input
                  placeholder="Buscar por nome ou descrição..."
                  value={searchTerm}
                  onChange={(e) => setSearchTerm(e.target.value)}
                  className="pl-10"
                />
              </div>
            </div>
            <Select
              value={filters.featureType}
              onValueChange={(v) => setFilters({ ...filters, featureType: v === "all" ? "" : v })}
            >
              <SelectTrigger className="w-[180px]">
                <SelectValue placeholder="Tipo" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="all">Todos os tipos</SelectItem>
                {featureTypes.map((t) => (
                  <SelectItem key={t} value={t}>
                    {t}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
            <Select
              value={filters.entityType}
              onValueChange={(v) => setFilters({ ...filters, entityType: v === "all" ? "" : v })}
            >
              <SelectTrigger className="w-[180px]">
                <SelectValue placeholder="Entidade" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="all">Todas entidades</SelectItem>
                {entityTypes.map((e) => (
                  <SelectItem key={e} value={e}>
                    {e}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
            <Select
              value={filters.source}
              onValueChange={(v) => setFilters({ ...filters, source: v === "all" ? "" : v })}
            >
              <SelectTrigger className="w-[180px]">
                <SelectValue placeholder="Fonte" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="all">Todas fontes</SelectItem>
                {sources.map((s) => (
                  <SelectItem key={s} value={s}>
                    {s}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
            <Button
              variant="ghost"
              onClick={() => {
                setFilters({ featureType: "", entityType: "", source: "" });
                setSearchTerm("");
              }}
            >
              Limpar
            </Button>
          </div>
        </CardContent>
      </Card>

      {/* Feature Table */}
      <Card>
        <CardHeader>
          <CardTitle>Features ({filteredFeatures.length})</CardTitle>
          <CardDescription>
            Todas as features são determinísticas (mesma entrada → mesma saída)
          </CardDescription>
        </CardHeader>
        <CardContent>
          {isLoading ? (
            <div className="flex justify-center py-8">
              <RefreshCw className="h-6 w-6 animate-spin text-muted-foreground" />
            </div>
          ) : (
            <Table>
              <TableHeader>
                <TableRow>
                  <TableHead>Feature</TableHead>
                  <TableHead>Tipo</TableHead>
                  <TableHead>Entidade</TableHead>
                  <TableHead>Janela</TableHead>
                  <TableHead>Fonte</TableHead>
                  <TableHead>Data Type</TableHead>
                  <TableHead>Operadores</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {filteredFeatures.map((feature) => (
                  <TableRow key={feature.featureName}>
                    <TableCell>
                      <div className="flex flex-col">
                        <span className="font-mono text-sm font-medium">
                          {feature.featureName}
                        </span>
                        {feature.description && (
                          <span className="text-xs text-muted-foreground truncate max-w-[300px]">
                            {feature.description}
                          </span>
                        )}
                      </div>
                    </TableCell>
                    <TableCell>
                      <Badge
                        variant="secondary"
                        className={`${FEATURE_TYPE_COLORS[feature.featureType] || ""} flex items-center gap-1 w-fit`}
                      >
                        {FEATURE_TYPE_ICONS[feature.featureType]}
                        {feature.featureType}
                      </Badge>
                    </TableCell>
                    <TableCell>
                      {feature.entityType ? (
                        <Badge variant="outline">{feature.entityType}</Badge>
                      ) : (
                        <span className="text-muted-foreground">—</span>
                      )}
                    </TableCell>
                    <TableCell>
                      {feature.windowName ? (
                        <Badge variant="outline" className="font-mono">
                          {feature.windowName}
                        </Badge>
                      ) : (
                        <span className="text-muted-foreground">—</span>
                      )}
                    </TableCell>
                    <TableCell>
                      <Badge
                        variant="outline"
                        className={SOURCE_COLORS[feature.source] || ""}
                      >
                        {feature.source}
                      </Badge>
                    </TableCell>
                    <TableCell>
                      <code className="text-xs bg-muted px-1 py-0.5 rounded">
                        {feature.dataType}
                      </code>
                    </TableCell>
                    <TableCell>
                      <div className="flex flex-wrap gap-1 max-w-[200px]">
                        {feature.allowedOperators.slice(0, 4).map((op) => (
                          <code
                            key={op}
                            className="text-xs bg-muted px-1 py-0.5 rounded"
                          >
                            {op}
                          </code>
                        ))}
                        {feature.allowedOperators.length > 4 && (
                          <span className="text-xs text-muted-foreground">
                            +{feature.allowedOperators.length - 4}
                          </span>
                        )}
                      </div>
                    </TableCell>
                  </TableRow>
                ))}
                {filteredFeatures.length === 0 && (
                  <TableRow>
                    <TableCell colSpan={7} className="text-center py-8 text-muted-foreground">
                      Nenhuma feature encontrada
                    </TableCell>
                  </TableRow>
                )}
              </TableBody>
            </Table>
          )}
        </CardContent>
      </Card>

      {/* Legend */}
      <Card>
        <CardHeader>
          <CardTitle className="text-lg">Legenda de Tipos</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-2 md:grid-cols-3 lg:grid-cols-5 gap-4">
            <div className="flex items-center gap-2">
              <Badge className="bg-blue-100 text-blue-800">PAYLOAD_FIELD</Badge>
              <span className="text-sm text-muted-foreground">Campo do payload</span>
            </div>
            <div className="flex items-center gap-2">
              <Badge className="bg-purple-100 text-purple-800">TEMPORAL</Badge>
              <span className="text-sm text-muted-foreground">Janelas temporais</span>
            </div>
            <div className="flex items-center gap-2">
              <Badge className="bg-orange-100 text-orange-800">VELOCITY</Badge>
              <span className="text-sm text-muted-foreground">Contadores/rates</span>
            </div>
            <div className="flex items-center gap-2">
              <Badge className="bg-green-100 text-green-800">GRAPH</Badge>
              <span className="text-sm text-muted-foreground">Relacionamentos</span>
            </div>
            <div className="flex items-center gap-2">
              <Badge className="bg-cyan-100 text-cyan-800">GEO</Badge>
              <span className="text-sm text-muted-foreground">Geolocalização</span>
            </div>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}
