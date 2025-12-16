import { useState } from 'react';
import { trpc } from '@/lib/trpc';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Input } from '@/components/ui/input';
import { Dialog, DialogContent, DialogDescription, DialogHeader, DialogTitle, DialogTrigger } from '@/components/ui/dialog';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Tooltip, TooltipContent, TooltipTrigger } from '@/components/ui/tooltip';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { 
  Shield, Plus, Edit2, Trash2, ToggleRight, HelpCircle, AlertTriangle, 
  CheckCircle, XCircle, Info, Search, Filter, BookOpen, Lightbulb,
  Target, TrendingUp, Clock, MapPin, CreditCard, Lock, Zap
} from 'lucide-react';
import { toast } from 'sonner';

// ==================== DEFINIÇÕES DE CAMPOS DO PAYLOAD ====================
const CAMPOS_PAYLOAD = {
  identificacao: {
    titulo: '🔐 Identificação',
    descricao: 'Campos que identificam a transação, cliente e comerciante',
    campos: [
      { nome: 'customerIdFromHeader', tipo: 'texto', descricao: 'ID único do cliente no sistema', exemplo: 'CUST-12345' },
      { nome: 'merchantId', tipo: 'texto', descricao: 'ID único do comerciante/loja', exemplo: 'MERCH-67890' },
      { nome: 'pan', tipo: 'texto', descricao: 'Número do cartão (mascarado)', exemplo: '4111****1111' },
      { nome: 'externalTransactionId', tipo: 'texto', descricao: 'ID externo da transação', exemplo: 'TXN-ABC123' },
    ]
  },
  valores: {
    titulo: '💰 Valores e Datas',
    descricao: 'Campos relacionados a valores monetários e datas',
    campos: [
      { nome: 'transactionAmount', tipo: 'numero', descricao: 'Valor da transação em centavos', exemplo: '50000 (R$ 500,00)' },
      { nome: 'transactionDate', tipo: 'data', descricao: 'Data da transação (YYYYMMDD)', exemplo: '20241216' },
      { nome: 'transactionTime', tipo: 'hora', descricao: 'Hora da transação (HHMMSS)', exemplo: '143022' },
      { nome: 'cardExpirationDate', tipo: 'data', descricao: 'Data de expiração do cartão', exemplo: '202612' },
    ]
  },
  localizacao: {
    titulo: '🌍 Localização',
    descricao: 'Campos geográficos do comerciante e transação',
    campos: [
      { nome: 'merchantCountryCode', tipo: 'texto', descricao: 'Código do país do comerciante (ISO)', exemplo: 'BR, US, CN' },
      { nome: 'merchantPostalCode', tipo: 'texto', descricao: 'CEP do comerciante', exemplo: '01310-100' },
      { nome: 'gmtOffset', tipo: 'numero', descricao: 'Fuso horário GMT', exemplo: '-3' },
      { nome: 'acquirerCountryCode', tipo: 'texto', descricao: 'País do adquirente', exemplo: 'BR' },
    ]
  },
  seguranca: {
    titulo: '🛡️ Segurança e Autenticação',
    descricao: 'Campos de validação de segurança e autenticação 3DS',
    campos: [
      { nome: 'consumerAuthenticationScore', tipo: 'numero', descricao: 'Score de autenticação do cliente (0-999)', exemplo: '850 = Muito seguro' },
      { nome: 'externalScore3', tipo: 'numero', descricao: 'Score externo de risco (0-999)', exemplo: '750 = Baixo risco' },
      { nome: 'cryptogramValid', tipo: 'booleano', descricao: 'Criptograma EMV válido?', exemplo: 'Y = Válido, N = Inválido' },
      { nome: 'cvv2Response', tipo: 'texto', descricao: 'Resposta da validação CVV', exemplo: 'M = Match, N = No Match' },
      { nome: 'cavvResult', tipo: 'texto', descricao: 'Resultado da autenticação CAVV', exemplo: '2 = Sucesso' },
      { nome: 'eciIndicator', tipo: 'texto', descricao: 'Indicador de e-commerce', exemplo: '05 = 3DS autenticado' },
    ]
  },
  categoria: {
    titulo: '🏷️ Categoria e Tipo',
    descricao: 'Campos de categorização da transação',
    campos: [
      { nome: 'mcc', tipo: 'texto', descricao: 'Código de categoria do comerciante', exemplo: '5411 = Supermercado' },
      { nome: 'transactionType', tipo: 'texto', descricao: 'Tipo de transação', exemplo: 'PURCHASE, REFUND' },
      { nome: 'customerPresent', tipo: 'texto', descricao: 'Cliente presente na transação?', exemplo: 'Y = Sim, N = Não (CNP)' },
      { nome: 'posEntryMode', tipo: 'texto', descricao: 'Modo de entrada no POS', exemplo: '051 = Chip, 012 = Manual' },
    ]
  },
};

// ==================== OPERADORES ====================
const OPERADORES = {
  numero: [
    { valor: '>', label: 'Maior que (>)', descricao: 'O valor deve ser maior que o especificado', exemplo: 'transactionAmount > 500000 → Valor maior que R$ 5.000' },
    { valor: '<', label: 'Menor que (<)', descricao: 'O valor deve ser menor que o especificado', exemplo: 'consumerAuthenticationScore < 100 → Score muito baixo' },
    { valor: '>=', label: 'Maior ou igual (>=)', descricao: 'O valor deve ser maior ou igual ao especificado', exemplo: 'weight >= 80 → Peso alto' },
    { valor: '<=', label: 'Menor ou igual (<=)', descricao: 'O valor deve ser menor ou igual ao especificado', exemplo: 'externalScore3 <= 200 → Score de risco alto' },
    { valor: '==', label: 'Igual a (==)', descricao: 'O valor deve ser exatamente igual', exemplo: 'transactionAmount == 100 → Exatamente R$ 1,00' },
    { valor: '!=', label: 'Diferente de (!=)', descricao: 'O valor deve ser diferente', exemplo: 'gmtOffset != -3 → Fora do Brasil' },
  ],
  texto: [
    { valor: '==', label: 'Igual a (==)', descricao: 'O texto deve ser exatamente igual', exemplo: 'merchantCountryCode == "BR" → Brasil' },
    { valor: '!=', label: 'Diferente de (!=)', descricao: 'O texto deve ser diferente', exemplo: 'cvv2Response != "M" → CVV não confere' },
    { valor: 'IN', label: 'Está na lista (IN)', descricao: 'O valor está em uma lista de opções', exemplo: 'mcc IN ["7995", "6051"] → Jogos ou Cripto' },
    { valor: 'NOT_IN', label: 'Não está na lista (NOT_IN)', descricao: 'O valor NÃO está em uma lista', exemplo: 'merchantCountryCode NOT_IN ["NG", "RU"]' },
  ],
  booleano: [
    { valor: '==', label: 'É verdadeiro (==)', descricao: 'O valor deve ser Y ou true', exemplo: 'cryptogramValid == "Y" → Criptograma válido' },
    { valor: '!=', label: 'É falso (!=)', descricao: 'O valor deve ser N ou false', exemplo: 'cryptogramValid != "Y" → Criptograma inválido' },
  ],
};

// ==================== CLASSIFICAÇÕES ====================
const CLASSIFICACOES = {
  APPROVED: {
    label: '✅ Aprovada',
    cor: 'bg-green-100 text-green-800 border-green-200',
    descricao: 'Transação segura, pode ser aprovada automaticamente',
    icone: CheckCircle,
  },
  SUSPICIOUS: {
    label: '⚠️ Suspeita',
    cor: 'bg-amber-100 text-amber-800 border-amber-200',
    descricao: 'Transação requer análise manual ou verificação adicional',
    icone: AlertTriangle,
  },
  FRAUD: {
    label: '🚫 Fraude',
    cor: 'bg-red-100 text-red-800 border-red-200',
    descricao: 'Transação deve ser bloqueada imediatamente',
    icone: XCircle,
  },
};

// ==================== CATEGORIAS DE REGRAS ====================
const CATEGORIAS_REGRAS = {
  VALUE: { titulo: '💰 Valor', icone: TrendingUp, cor: 'text-green-600' },
  TEMPORAL: { titulo: '🕐 Temporal', icone: Clock, cor: 'text-blue-600' },
  GEOGRAPHIC: { titulo: '🌍 Geográfica', icone: MapPin, cor: 'text-purple-600' },
  MCC: { titulo: '🏷️ Categoria (MCC)', icone: Target, cor: 'text-orange-600' },
  AUTHENTICATION: { titulo: '🔐 Autenticação', icone: Lock, cor: 'text-indigo-600' },
  CVV_PIN: { titulo: '🔢 CVV/PIN', icone: CreditCard, cor: 'text-pink-600' },
  TERMINAL: { titulo: '📟 Terminal', icone: Zap, cor: 'text-cyan-600' },
  EMV: { titulo: '💳 EMV/Chip', icone: Shield, cor: 'text-teal-600' },
  CARD: { titulo: '💳 Cartão', icone: CreditCard, cor: 'text-rose-600' },
  CONTEXT: { titulo: '📋 Contexto', icone: Info, cor: 'text-slate-600' },
  COMBINED: { titulo: '🔗 Combinada', icone: Lightbulb, cor: 'text-amber-600' },
  BRAZIL: { titulo: '🇧🇷 Brasil', icone: Shield, cor: 'text-green-600' },
};

// ==================== INTERFACE DA REGRA ====================
interface Condicao {
  field: string;
  operator: string;
  value: string | number | string[];
  logicOperator?: 'AND' | 'OR';
}

interface Regra {
  id: number;
  name: string;
  description: string;
  category: string;
  classification: 'APPROVED' | 'SUSPICIOUS' | 'FRAUD';
  weight: number;
  conditions: Condicao[];
  logicOperator: 'AND' | 'OR';
  isActive: boolean;
  version: number;
  source?: string;
  createdAt: Date;
  updatedAt: Date;
}

// ==================== COMPONENTE PRINCIPAL ====================
export default function RulesDidactic() {
  const [filtroCategoria, setFiltroCategoria] = useState<string>('all');
  const [filtroClassificacao, setFiltroClassificacao] = useState<string>('all');
  const [filtroBusca, setFiltroBusca] = useState('');
  const [showNovaRegra, setShowNovaRegra] = useState(false);
  const [showDocumentacao, setShowDocumentacao] = useState(false);
  const [regraExpandida, setRegraExpandida] = useState<number | null>(null);

  // Buscar regras do banco de dados
  const { data: regrasData, isLoading, refetch } = trpc.rules.list.useQuery();
  const regras: Regra[] = (regrasData || []).map((r: any) => ({
    id: r.id,
    name: r.name,
    description: r.description,
    category: r.category,
    classification: r.classification,
    weight: r.weight,
    conditions: r.conditions || [],
    logicOperator: r.logicOperator || 'AND',
    isActive: r.isActive,
    version: r.version,
    source: r.source,
    createdAt: r.createdAt,
    updatedAt: r.updatedAt,
  }));

  // Mutations
  const toggleMutation = trpc.rules.toggle.useMutation({
    onSuccess: () => {
      toast.success('Regra atualizada com sucesso!');
      refetch();
    },
    onError: () => toast.error('Erro ao atualizar regra'),
  });

  const deleteMutation = trpc.rules.delete.useMutation({
    onSuccess: () => {
      toast.success('Regra excluída com sucesso!');
      refetch();
    },
    onError: () => toast.error('Erro ao excluir regra'),
  });

  // Filtrar regras
  const regrasFiltradas = regras.filter((regra) => {
    if (filtroCategoria !== 'all' && regra.category !== filtroCategoria) return false;
    if (filtroClassificacao !== 'all' && regra.classification !== filtroClassificacao) return false;
    if (filtroBusca && !regra.name.toLowerCase().includes(filtroBusca.toLowerCase()) && 
        !regra.description.toLowerCase().includes(filtroBusca.toLowerCase())) return false;
    return true;
  });

  // Renderizar condição de forma didática
  const renderizarCondicao = (condicao: Condicao, index: number) => {
    const campo = Object.values(CAMPOS_PAYLOAD)
      .flatMap(cat => cat.campos)
      .find(c => c.nome === condicao.field);

    const operadorInfo = OPERADORES[campo?.tipo as keyof typeof OPERADORES]?.find(
      op => op.valor === condicao.operator
    );

    return (
      <div key={index} className="flex items-center gap-2 p-3 bg-slate-50 rounded-lg border border-slate-200">
        {index > 0 && (
          <Badge variant="outline" className="bg-amber-50 text-amber-700 border-amber-200 font-semibold">
            {condicao.logicOperator || 'E'}
          </Badge>
        )}
        <div className="flex-1">
          <div className="flex items-center gap-2 flex-wrap">
            <Badge variant="secondary" className="font-mono text-xs">
              {condicao.field}
            </Badge>
            <span className="text-sm font-medium text-slate-600">
              {operadorInfo?.label || condicao.operator}
            </span>
            <Badge className="bg-blue-100 text-blue-800 border-blue-200">
              {Array.isArray(condicao.value) ? condicao.value.join(', ') : String(condicao.value)}
            </Badge>
          </div>
          {campo && (
            <p className="text-xs text-slate-500 mt-1">
              📝 {campo.descricao}
            </p>
          )}
        </div>
      </div>
    );
  };

  // Renderizar card de regra
  const renderizarRegra = (regra: Regra) => {
    const categoria = CATEGORIAS_REGRAS[regra.category as keyof typeof CATEGORIAS_REGRAS] || CATEGORIAS_REGRAS.COMBINED;
    const classificacao = CLASSIFICACOES[regra.classification];
    const CategoriaIcone = categoria.icone;
    const ClassificacaoIcone = classificacao.icone;
    const expandida = regraExpandida === regra.id;

    return (
      <Card 
        key={regra.id} 
        className={`transition-all duration-300 ${expandida ? 'ring-2 ring-primary shadow-lg' : 'hover:shadow-md'} ${!regra.isActive ? 'opacity-60' : ''}`}
      >
        <CardHeader className="pb-3">
          <div className="flex items-start justify-between">
            <div className="flex items-center gap-3">
              <div className={`p-2 rounded-lg bg-slate-100 ${categoria.cor}`}>
                <CategoriaIcone className="h-5 w-5" />
              </div>
              <div>
                <CardTitle className="text-lg flex items-center gap-2">
                  {regra.name}
                  <Badge variant={regra.isActive ? 'default' : 'secondary'} className="text-xs">
                    {regra.isActive ? '✅ Ativa' : '⏸️ Inativa'}
                  </Badge>
                </CardTitle>
                <CardDescription className="mt-1">
                  {regra.description}
                </CardDescription>
              </div>
            </div>
            <div className="flex items-center gap-2">
              <Badge className={`${classificacao.cor} border`}>
                <ClassificacaoIcone className="h-3 w-3 mr-1" />
                {classificacao.label}
              </Badge>
            </div>
          </div>
        </CardHeader>

        <CardContent>
          {/* Métricas da Regra */}
          <div className="grid grid-cols-3 gap-4 mb-4 p-3 bg-slate-50 rounded-lg">
            <div className="text-center">
              <p className="text-xs text-slate-500 uppercase">Peso</p>
              <p className="text-xl font-bold text-slate-800">{regra.weight}%</p>
            </div>
            <div className="text-center border-x border-slate-200">
              <p className="text-xs text-slate-500 uppercase">Versão</p>
              <p className="text-xl font-bold text-slate-800">v{regra.version}</p>
            </div>
            <div className="text-center">
              <p className="text-xs text-slate-500 uppercase">Categoria</p>
              <p className="text-sm font-semibold text-slate-800">{categoria.titulo}</p>
            </div>
          </div>

          {/* Condições */}
          <div className="mb-4">
            <div className="flex items-center gap-2 mb-2">
              <Target className="h-4 w-4 text-slate-500" />
              <h4 className="font-semibold text-slate-700">
                Condições ({regra.conditions?.length || 0})
              </h4>
              {regra.conditions?.length > 1 && (
                <Badge variant="outline" className="text-xs">
                  Operador: {regra.logicOperator === 'AND' ? 'E (todas devem ser verdadeiras)' : 'OU (pelo menos uma)'}
                </Badge>
              )}
            </div>
            <div className="space-y-2">
              {regra.conditions?.slice(0, expandida ? undefined : 2).map((cond, idx) => 
                renderizarCondicao(cond, idx)
              )}
              {!expandida && regra.conditions?.length > 2 && (
                <p className="text-sm text-slate-500 text-center py-2">
                  ... e mais {regra.conditions.length - 2} condição(ões)
                </p>
              )}
            </div>
          </div>

          {/* Fonte */}
          {regra.source && (
            <div className="flex items-center gap-2 text-xs text-slate-500 mb-4">
              <BookOpen className="h-3 w-3" />
              <span>Fonte: {regra.source}</span>
            </div>
          )}

          {/* Ações */}
          <div className="flex items-center justify-between pt-3 border-t border-slate-200">
            <Button
              variant="ghost"
              size="sm"
              onClick={() => setRegraExpandida(expandida ? null : regra.id)}
            >
              {expandida ? '🔼 Recolher' : '🔽 Expandir detalhes'}
            </Button>
            <div className="flex gap-2">
              <Button
                variant="outline"
                size="sm"
                onClick={() => toggleMutation.mutate({ id: regra.id, isActive: !regra.isActive })}
                className={regra.isActive ? 'text-amber-600 hover:text-amber-700' : 'text-green-600 hover:text-green-700'}
              >
                <ToggleRight className="h-4 w-4 mr-1" />
                {regra.isActive ? 'Desativar' : 'Ativar'}
              </Button>
              <Button
                variant="outline"
                size="sm"
                onClick={() => deleteMutation.mutate({ id: regra.id })}
                className="text-red-600 hover:text-red-700"
              >
                <Trash2 className="h-4 w-4 mr-1" />
                Excluir
              </Button>
            </div>
          </div>
        </CardContent>
      </Card>
    );
  };

  return (
    <div className="p-6 space-y-6 max-w-7xl mx-auto">
      {/* Cabeçalho */}
      <div className="flex flex-col md:flex-row md:items-center md:justify-between gap-4">
        <div>
          <h1 className="text-3xl font-bold text-slate-800 flex items-center gap-3">
            <Shield className="h-8 w-8 text-primary" />
            Motor de Regras Duras
          </h1>
          <p className="text-slate-600 mt-1">
            Sistema de regras configuráveis para detecção de fraudes em transações de crédito
          </p>
        </div>
        <div className="flex gap-2">
          <Dialog open={showDocumentacao} onOpenChange={setShowDocumentacao}>
            <DialogTrigger asChild>
              <Button variant="outline">
                <BookOpen className="h-4 w-4 mr-2" />
                Documentação
              </Button>
            </DialogTrigger>
            <DialogContent className="max-w-4xl max-h-[80vh] overflow-y-auto">
              <DialogHeader>
                <DialogTitle>📚 Documentação do Motor de Regras</DialogTitle>
                <DialogDescription>
                  Guia completo para entender e criar regras de detecção de fraude
                </DialogDescription>
              </DialogHeader>
              <Tabs defaultValue="campos">
                <TabsList className="grid w-full grid-cols-4">
                  <TabsTrigger value="campos">Campos</TabsTrigger>
                  <TabsTrigger value="operadores">Operadores</TabsTrigger>
                  <TabsTrigger value="classificacoes">Classificações</TabsTrigger>
                  <TabsTrigger value="exemplos">Exemplos</TabsTrigger>
                </TabsList>
                <TabsContent value="campos" className="space-y-4 mt-4">
                  {Object.entries(CAMPOS_PAYLOAD).map(([key, categoria]) => (
                    <Card key={key}>
                      <CardHeader className="pb-2">
                        <CardTitle className="text-lg">{categoria.titulo}</CardTitle>
                        <CardDescription>{categoria.descricao}</CardDescription>
                      </CardHeader>
                      <CardContent>
                        <div className="space-y-2">
                          {categoria.campos.map((campo) => (
                            <div key={campo.nome} className="flex items-start gap-3 p-2 bg-slate-50 rounded">
                              <Badge variant="secondary" className="font-mono text-xs">{campo.nome}</Badge>
                              <div className="flex-1">
                                <p className="text-sm text-slate-700">{campo.descricao}</p>
                                <p className="text-xs text-slate-500">Exemplo: {campo.exemplo}</p>
                              </div>
                              <Badge variant="outline" className="text-xs">{campo.tipo}</Badge>
                            </div>
                          ))}
                        </div>
                      </CardContent>
                    </Card>
                  ))}
                </TabsContent>
                <TabsContent value="operadores" className="space-y-4 mt-4">
                  {Object.entries(OPERADORES).map(([tipo, ops]) => (
                    <Card key={tipo}>
                      <CardHeader className="pb-2">
                        <CardTitle className="text-lg capitalize">Operadores para {tipo}</CardTitle>
                      </CardHeader>
                      <CardContent>
                        <div className="space-y-2">
                          {ops.map((op) => (
                            <div key={op.valor} className="p-3 bg-slate-50 rounded">
                              <div className="flex items-center gap-2 mb-1">
                                <Badge className="font-mono">{op.valor}</Badge>
                                <span className="font-medium">{op.label}</span>
                              </div>
                              <p className="text-sm text-slate-600">{op.descricao}</p>
                              <p className="text-xs text-slate-500 mt-1">📝 {op.exemplo}</p>
                            </div>
                          ))}
                        </div>
                      </CardContent>
                    </Card>
                  ))}
                </TabsContent>
                <TabsContent value="classificacoes" className="space-y-4 mt-4">
                  {Object.entries(CLASSIFICACOES).map(([key, classif]) => {
                    const Icone = classif.icone;
                    return (
                      <Card key={key} className={`${classif.cor} border`}>
                        <CardHeader className="pb-2">
                          <CardTitle className="text-lg flex items-center gap-2">
                            <Icone className="h-5 w-5" />
                            {classif.label}
                          </CardTitle>
                        </CardHeader>
                        <CardContent>
                          <p>{classif.descricao}</p>
                        </CardContent>
                      </Card>
                    );
                  })}
                </TabsContent>
                <TabsContent value="exemplos" className="space-y-4 mt-4">
                  <Card>
                    <CardHeader>
                      <CardTitle>Exemplo 1: Transação de Alto Valor</CardTitle>
                    </CardHeader>
                    <CardContent className="space-y-2">
                      <p className="text-sm text-slate-600">
                        <strong>Objetivo:</strong> Detectar transações com valor acima de R$ 5.000,00
                      </p>
                      <div className="p-3 bg-slate-50 rounded font-mono text-sm">
                        transactionAmount &gt; 500000
                      </div>
                      <p className="text-xs text-slate-500">
                        💡 O valor é em centavos, então R$ 5.000,00 = 500000 centavos
                      </p>
                    </CardContent>
                  </Card>
                  <Card>
                    <CardHeader>
                      <CardTitle>Exemplo 2: País de Alto Risco</CardTitle>
                    </CardHeader>
                    <CardContent className="space-y-2">
                      <p className="text-sm text-slate-600">
                        <strong>Objetivo:</strong> Bloquear transações de países conhecidos por fraude
                      </p>
                      <div className="p-3 bg-slate-50 rounded font-mono text-sm">
                        merchantCountryCode IN ["NG", "RU", "CN", "KP", "IR"]
                      </div>
                      <p className="text-xs text-slate-500">
                        💡 NG=Nigéria, RU=Rússia, CN=China, KP=Coreia do Norte, IR=Irã
                      </p>
                    </CardContent>
                  </Card>
                  <Card>
                    <CardHeader>
                      <CardTitle>Exemplo 3: Regra Combinada (Card Testing)</CardTitle>
                    </CardHeader>
                    <CardContent className="space-y-2">
                      <p className="text-sm text-slate-600">
                        <strong>Objetivo:</strong> Detectar padrão de teste de cartão (múltiplas transações pequenas)
                      </p>
                      <div className="p-3 bg-slate-50 rounded font-mono text-sm space-y-1">
                        <p>transactionAmount &lt; 500 <span className="text-amber-600 font-bold">E</span></p>
                        <p>customerPresent == "N" <span className="text-amber-600 font-bold">E</span></p>
                        <p>consumerAuthenticationScore &lt; 200</p>
                      </div>
                      <p className="text-xs text-slate-500">
                        💡 Transação pequena + Sem cliente presente + Score baixo = Possível Card Testing
                      </p>
                    </CardContent>
                  </Card>
                </TabsContent>
              </Tabs>
            </DialogContent>
          </Dialog>
          <Button onClick={() => setShowNovaRegra(true)}>
            <Plus className="h-4 w-4 mr-2" />
            Nova Regra
          </Button>
        </div>
      </div>

      {/* Estatísticas */}
      <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
        <Card className="bg-gradient-to-br from-blue-50 to-blue-100 border-blue-200">
          <CardContent className="pt-4">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-blue-600 font-medium">Total de Regras</p>
                <p className="text-3xl font-bold text-blue-800">{regras.length}</p>
              </div>
              <Shield className="h-10 w-10 text-blue-400" />
            </div>
          </CardContent>
        </Card>
        <Card className="bg-gradient-to-br from-green-50 to-green-100 border-green-200">
          <CardContent className="pt-4">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-green-600 font-medium">Regras Ativas</p>
                <p className="text-3xl font-bold text-green-800">
                  {regras.filter(r => r.isActive).length}
                </p>
              </div>
              <CheckCircle className="h-10 w-10 text-green-400" />
            </div>
          </CardContent>
        </Card>
        <Card className="bg-gradient-to-br from-amber-50 to-amber-100 border-amber-200">
          <CardContent className="pt-4">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-amber-600 font-medium">Regras Suspeita</p>
                <p className="text-3xl font-bold text-amber-800">
                  {regras.filter(r => r.classification === 'SUSPICIOUS').length}
                </p>
              </div>
              <AlertTriangle className="h-10 w-10 text-amber-400" />
            </div>
          </CardContent>
        </Card>
        <Card className="bg-gradient-to-br from-red-50 to-red-100 border-red-200">
          <CardContent className="pt-4">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-red-600 font-medium">Regras Fraude</p>
                <p className="text-3xl font-bold text-red-800">
                  {regras.filter(r => r.classification === 'FRAUD').length}
                </p>
              </div>
              <XCircle className="h-10 w-10 text-red-400" />
            </div>
          </CardContent>
        </Card>
      </div>

      {/* Filtros */}
      <Card>
        <CardContent className="pt-4">
          <div className="flex flex-col md:flex-row gap-4">
            <div className="flex-1">
              <div className="relative">
                <Search className="absolute left-3 top-1/2 -translate-y-1/2 h-4 w-4 text-slate-400" />
                <Input
                  placeholder="Buscar por nome ou descrição..."
                  value={filtroBusca}
                  onChange={(e) => setFiltroBusca(e.target.value)}
                  className="pl-10"
                />
              </div>
            </div>
            <Select value={filtroCategoria} onValueChange={setFiltroCategoria}>
              <SelectTrigger className="w-full md:w-48">
                <SelectValue placeholder="Categoria" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="all">Todas as Categorias</SelectItem>
                {Object.entries(CATEGORIAS_REGRAS).map(([key, cat]) => (
                  <SelectItem key={key} value={key}>{cat.titulo}</SelectItem>
                ))}
              </SelectContent>
            </Select>
            <Select value={filtroClassificacao} onValueChange={setFiltroClassificacao}>
              <SelectTrigger className="w-full md:w-48">
                <SelectValue placeholder="Classificação" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="all">Todas as Classificações</SelectItem>
                {Object.entries(CLASSIFICACOES).map(([key, classif]) => (
                  <SelectItem key={key} value={key}>{classif.label}</SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>
        </CardContent>
      </Card>

      {/* Lista de Regras */}
      {isLoading ? (
        <div className="flex items-center justify-center h-64">
          <div className="text-center">
            <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-primary mx-auto mb-4"></div>
            <p className="text-slate-500">Carregando regras...</p>
          </div>
        </div>
      ) : regrasFiltradas.length === 0 ? (
        <Card className="p-12">
          <div className="text-center">
            <Shield className="h-16 w-16 text-slate-300 mx-auto mb-4" />
            <h3 className="text-xl font-semibold text-slate-700 mb-2">
              Nenhuma regra encontrada
            </h3>
            <p className="text-slate-500 mb-4">
              {filtroBusca || filtroCategoria !== 'all' || filtroClassificacao !== 'all'
                ? 'Tente ajustar os filtros de busca'
                : 'Comece criando sua primeira regra de detecção de fraude'}
            </p>
            <Button onClick={() => setShowNovaRegra(true)}>
              <Plus className="h-4 w-4 mr-2" />
              Criar Primeira Regra
            </Button>
          </div>
        </Card>
      ) : (
        <div className="grid gap-4">
          {regrasFiltradas.map(renderizarRegra)}
        </div>
      )}

      {/* Rodapé com informações */}
      <Card className="bg-slate-50">
        <CardContent className="pt-4">
          <div className="flex items-start gap-3">
            <Lightbulb className="h-5 w-5 text-amber-500 mt-0.5" />
            <div>
              <h4 className="font-semibold text-slate-700">Dica de Uso</h4>
              <p className="text-sm text-slate-600">
                As regras são avaliadas em ordem de peso (maior peso = maior prioridade). 
                Quando uma transação é analisada, todas as regras ativas são verificadas e 
                a classificação final é determinada pela regra de maior peso que foi acionada.
                Use a <strong>Documentação</strong> para entender todos os campos disponíveis.
              </p>
            </div>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}
