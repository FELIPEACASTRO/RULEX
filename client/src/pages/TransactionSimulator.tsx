import { useState } from "react";
import { Button } from "@/components/ui/button";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from "@/components/ui/select";
import { Switch } from "@/components/ui/switch";
import { Badge } from "@/components/ui/badge";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { ScrollArea } from "@/components/ui/scroll-area";
import { Separator } from "@/components/ui/separator";
import { toast } from "sonner";
import { 
  Play, 
  AlertTriangle, 
  CheckCircle, 
  XCircle, 
  Loader2,
  FileJson,
  Zap,
  Shield,
  Info,
  Copy,
  RotateCcw
} from "lucide-react";
import { 
  TransactionRequest, 
  TransactionResponse,
  analyzeTransaction,
  analyzeTransactionAdvanced 
} from "@/lib/javaApi";

// Campos disponíveis organizados por categoria
const FIELD_CATEGORIES = {
  identification: {
    label: "Identificação",
    icon: "🔐",
    fields: [
      { name: "externalTransactionId", label: "ID Externo", type: "text", placeholder: "TXN-2024-001" },
      { name: "customerIdFromHeader", label: "ID do Cliente", type: "text", placeholder: "CUST-12345" },
      { name: "merchantId", label: "ID do Merchant", type: "text", placeholder: "MERCH-001" },
      { name: "merchantName", label: "Nome do Merchant", type: "text", placeholder: "Loja ABC" },
      { name: "pan", label: "PAN (Cartão)", type: "text", placeholder: "4111111111111111" },
    ]
  },
  values: {
    label: "Valores e Datas",
    icon: "💰",
    fields: [
      { name: "transactionAmount", label: "Valor (centavos)", type: "number", placeholder: "15000" },
      { name: "transactionDate", label: "Data", type: "text", placeholder: "20241216" },
      { name: "transactionTime", label: "Hora", type: "text", placeholder: "143000" },
      { name: "transactionCurrencyCode", label: "Moeda", type: "text", placeholder: "986" },
      { name: "conversionRate", label: "Taxa de Conversão", type: "number", placeholder: "1.0" },
    ]
  },
  location: {
    label: "Localização",
    icon: "🌍",
    fields: [
      { name: "merchantCountryCode", label: "País do Merchant", type: "text", placeholder: "BR" },
      { name: "merchantPostalCode", label: "CEP do Merchant", type: "text", placeholder: "01310-100" },
      { name: "acquirerCountryCode", label: "País do Adquirente", type: "text", placeholder: "076" },
      { name: "gmtOffset", label: "GMT Offset", type: "text", placeholder: "-0300" },
    ]
  },
  security: {
    label: "Segurança",
    icon: "🛡️",
    fields: [
      { name: "consumerAuthenticationScore", label: "Score de Autenticação", type: "number", placeholder: "500" },
      { name: "externalScore3", label: "Score Externo", type: "number", placeholder: "85" },
      { name: "cvv2Response", label: "Resposta CVV", type: "select", options: ["M", "N", "P", "S", "U"] },
      { name: "cavvResult", label: "Resultado CAVV", type: "select", options: ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"] },
      { name: "eciIndicator", label: "ECI Indicator", type: "number", placeholder: "5" },
      { name: "cryptogramValid", label: "Criptograma Válido", type: "boolean" },
      { name: "cvv2EntryLimitExceeded", label: "Limite CVV Excedido", type: "boolean" },
      { name: "pinEntryLimitExceeded", label: "Limite PIN Excedido", type: "boolean" },
    ]
  },
  terminal: {
    label: "Terminal/POS",
    icon: "🖥️",
    fields: [
      { name: "posSecurity", label: "Segurança POS", type: "number", placeholder: "1" },
      { name: "posOffPremises", label: "POS Off-Premises", type: "number", placeholder: "0" },
      { name: "posEntryMode", label: "Modo de Entrada", type: "select", options: ["C", "M", "E", "F"] },
      { name: "customerPresent", label: "Cliente Presente", type: "select", options: ["Y", "N"] },
      { name: "cardCaptured", label: "Cartão Capturado", type: "boolean" },
      { name: "recurringTransaction", label: "Transação Recorrente", type: "boolean" },
    ]
  },
  emv: {
    label: "EMV/Chip",
    icon: "💳",
    fields: [
      { name: "cardAipStatic", label: "AIP Estático", type: "number", placeholder: "1" },
      { name: "cardAipDynamic", label: "AIP Dinâmico", type: "number", placeholder: "1" },
      { name: "terminalVerificationResults", label: "TVR", type: "text", placeholder: "0000000000" },
      { name: "cardExpireDate", label: "Data Expiração", type: "text", placeholder: "2612" },
    ]
  },
  category: {
    label: "Categoria",
    icon: "🏷️",
    fields: [
      { name: "mcc", label: "MCC", type: "text", placeholder: "5411" },
    ]
  }
};

// Templates de transação para testes rápidos
const TRANSACTION_TEMPLATES = {
  legitimate: {
    name: "Transação Legítima",
    description: "Compra normal em supermercado",
    data: {
      externalTransactionId: "TXN-LEG-001",
      transactionAmount: 15000,
      transactionDate: "20241216",
      transactionTime: "143000",
      mcc: "5411",
      merchantCountryCode: "BR",
      merchantId: "MERCH-SUPER-001",
      merchantName: "Supermercado ABC",
      customerIdFromHeader: "CUST-12345",
      customerPresent: "Y",
      consumerAuthenticationScore: 500,
      externalScore3: 85,
      cvv2Response: "M",
      cryptogramValid: true,
      eciIndicator: 5,
    }
  },
  cardTesting: {
    name: "Card Testing",
    description: "Padrão de teste de cartão",
    data: {
      externalTransactionId: "TXN-TEST-001",
      transactionAmount: 100,
      transactionDate: "20241216",
      transactionTime: "030000",
      mcc: "6051",
      merchantCountryCode: "NG",
      merchantId: "MERCH-CRYPTO-001",
      customerIdFromHeader: "CUST-UNKNOWN",
      customerPresent: "N",
      consumerAuthenticationScore: 50,
      externalScore3: 20,
      cvv2Response: "N",
      cryptogramValid: false,
    }
  },
  highRisk: {
    name: "Alto Risco",
    description: "Transação de alto valor em país de risco",
    data: {
      externalTransactionId: "TXN-RISK-001",
      transactionAmount: 800000,
      transactionDate: "20241216",
      transactionTime: "020000",
      mcc: "7995",
      merchantCountryCode: "RU",
      merchantId: "MERCH-GAMBLING-001",
      customerIdFromHeader: "CUST-67890",
      customerPresent: "N",
      consumerAuthenticationScore: 100,
      externalScore3: 30,
      cvv2Response: "N",
      eciIndicator: 7,
      cryptogramValid: false,
    }
  },
  ato: {
    name: "Account Takeover",
    description: "Possível roubo de conta",
    data: {
      externalTransactionId: "TXN-ATO-001",
      transactionAmount: 500000,
      transactionDate: "20241216",
      transactionTime: "040000",
      mcc: "4829",
      merchantCountryCode: "BR",
      merchantId: "MERCH-TRANSFER-001",
      customerIdFromHeader: "CUST-11111",
      customerPresent: "N",
      consumerAuthenticationScore: 80,
      externalScore3: 40,
      cvv2Response: "N",
      cvv2EntryLimitExceeded: true,
      pinEntryLimitExceeded: true,
      cryptogramValid: false,
    }
  }
};

export default function TransactionSimulator() {
  const [formData, setFormData] = useState<TransactionRequest>({});
  const [result, setResult] = useState<TransactionResponse | null>(null);
  const [isLoading, setIsLoading] = useState(false);
  const [useAdvancedRules, setUseAdvancedRules] = useState(true);
  const [activeTab, setActiveTab] = useState("identification");

  const parseNumericInput = (rawValue: string): number | undefined => {
    if (!rawValue.trim()) return undefined;
    const parsed = Number(rawValue.replace(",", "."));
    return Number.isFinite(parsed) ? parsed : undefined;
  };

  // Atualizar campo do formulário
  const updateField = (field: string, value: string | number | boolean | undefined) => {
    setFormData(prev => ({
      ...prev,
      [field]: value
    }));
  };

  // Aplicar template
  const applyTemplate = (templateKey: keyof typeof TRANSACTION_TEMPLATES) => {
    const template = TRANSACTION_TEMPLATES[templateKey];
    setFormData(template.data as TransactionRequest);
    toast.success(`Template "${template.name}" aplicado`);
  };

  // Limpar formulário
  const clearForm = () => {
    setFormData({});
    setResult(null);
    toast.info("Formulário limpo");
  };

  // Copiar JSON
  const copyJson = () => {
    navigator.clipboard.writeText(JSON.stringify(formData, null, 2));
    toast.success("JSON copiado para a área de transferência");
  };

  // Analisar transação
  const handleAnalyze = async () => {
    if (Object.keys(formData).length === 0) {
      toast.error("Preencha pelo menos um campo para análise");
      return;
    }

    setIsLoading(true);
    setResult(null);

    try {
      const response = useAdvancedRules 
        ? await analyzeTransactionAdvanced(formData)
        : await analyzeTransaction(formData);
      
      setResult(response);
      
      if (response.classification === "FRAUD") {
        toast.error("🚨 FRAUDE DETECTADA!", { duration: 5000 });
      } else if (response.classification === "SUSPICIOUS") {
        toast.warning("⚠️ Transação Suspeita", { duration: 4000 });
      } else {
        toast.success("✅ Transação Aprovada", { duration: 3000 });
      }
    } catch (error) {
      console.error("Erro na análise:", error);
      toast.error("Erro ao analisar transação. Verifique se a API Java está ativa.");
      
      // Simular resultado para demonstração quando API não está disponível
      const simulatedResult = simulateAnalysis(formData);
      setResult({
        ...simulatedResult,
        reason: `${simulatedResult.reason} (resultado simulado; API indisponível)`
      });
      toast.info("Exibindo resultado simulado enquanto a API está offline.");
    } finally {
      setIsLoading(false);
    }
  };

  // Simulação local quando API não está disponível
  const simulateAnalysis = (request: TransactionRequest): TransactionResponse => {
    const triggeredRules: string[] = [];
    const ruleDetails: { ruleName: string; ruleDescription: string; score: number; threshold: number; triggered: boolean }[] = [];
    let totalScore = 0;

    // Regras simuladas
    if (request.transactionAmount && request.transactionAmount < 500) {
      if (request.mcc && ["7995", "6051", "5967"].includes(request.mcc)) {
        triggeredRules.push("CARD_TESTING_PATTERN");
        ruleDetails.push({ ruleName: "CARD_TESTING_PATTERN", ruleDescription: "Transação < R$5 em MCC de alto risco", score: 90, threshold: 500, triggered: true });
        totalScore += 90;
      }
    }

    if (request.transactionAmount && request.transactionAmount > 500000) {
      triggeredRules.push("HIGH_AMOUNT_THRESHOLD");
      ruleDetails.push({ ruleName: "HIGH_AMOUNT_THRESHOLD", ruleDescription: "Transação > R$5.000", score: 50, threshold: 500000, triggered: true });
      totalScore += 50;
    }

    if (request.merchantCountryCode && ["RU", "NG", "CN"].includes(request.merchantCountryCode)) {
      triggeredRules.push("HIGH_RISK_COUNTRY");
      ruleDetails.push({ ruleName: "HIGH_RISK_COUNTRY", ruleDescription: "País de alto risco", score: 65, threshold: 0, triggered: true });
      totalScore += 65;
    }

    if (request.consumerAuthenticationScore && request.consumerAuthenticationScore < 100) {
      triggeredRules.push("LOW_AUTHENTICATION_SCORE");
      ruleDetails.push({ ruleName: "LOW_AUTHENTICATION_SCORE", ruleDescription: "Score de autenticação baixo", score: 70, threshold: 100, triggered: true });
      totalScore += 70;
    }

    if (request.cvv2Response && request.cvv2Response !== "M") {
      triggeredRules.push("CVV_MISMATCH");
      ruleDetails.push({ ruleName: "CVV_MISMATCH", ruleDescription: "CVV não corresponde", score: 65, threshold: 0, triggered: true });
      totalScore += 65;
    }

    if (request.cryptogramValid === false) {
      triggeredRules.push("CRYPTOGRAM_INVALID");
      ruleDetails.push({ ruleName: "CRYPTOGRAM_INVALID", ruleDescription: "Criptograma inválido", score: 85, threshold: 0, triggered: true });
      totalScore += 85;
    }

    if (request.cvv2EntryLimitExceeded) {
      triggeredRules.push("CVV_ENTRY_LIMIT_EXCEEDED");
      ruleDetails.push({ ruleName: "CVV_ENTRY_LIMIT_EXCEEDED", ruleDescription: "Limite de tentativas CVV excedido", score: 90, threshold: 0, triggered: true });
      totalScore += 90;
    }

    if (request.pinEntryLimitExceeded) {
      triggeredRules.push("PIN_ENTRY_LIMIT_EXCEEDED");
      ruleDetails.push({ ruleName: "PIN_ENTRY_LIMIT_EXCEEDED", ruleDescription: "Limite de tentativas PIN excedido", score: 90, threshold: 0, triggered: true });
      totalScore += 90;
    }

    let classification: "APPROVED" | "SUSPICIOUS" | "FRAUD";
    if (totalScore >= 150) {
      classification = "FRAUD";
    } else if (totalScore >= 80) {
      classification = "SUSPICIOUS";
    } else {
      classification = "APPROVED";
    }

    return {
      transactionId: request.externalTransactionId || "SIM-" + Date.now(),
      classification,
      totalScore,
      triggeredRules,
      ruleDetails,
      reason: `Score total: ${totalScore} | ${triggeredRules.length} regra(s) acionada(s)`,
      processedAt: new Date().toISOString(),
    };
  };

  // Renderizar campo do formulário
  const renderField = (field: { name: string; label: string; type: string; placeholder?: string; options?: string[] }) => {
    const value = formData[field.name as keyof TransactionRequest];

    if (field.type === "boolean") {
      return (
        <div className="flex items-center justify-between" key={field.name}>
          <Label htmlFor={field.name} className="text-sm">{field.label}</Label>
          <Switch
            id={field.name}
            checked={value as boolean || false}
            onCheckedChange={(checked) => updateField(field.name, checked)}
          />
        </div>
      );
    }

    if (field.type === "select" && field.options) {
      return (
        <div className="space-y-2" key={field.name}>
          <Label htmlFor={field.name} className="text-sm">{field.label}</Label>
          <Select
            value={value as string || ""}
            onValueChange={(val) => updateField(field.name, val)}
          >
            <SelectTrigger id={field.name}>
              <SelectValue placeholder="Selecione..." />
            </SelectTrigger>
            <SelectContent>
              {field.options.map(opt => (
                <SelectItem key={opt} value={opt}>{opt}</SelectItem>
              ))}
            </SelectContent>
          </Select>
        </div>
      );
    }

    return (
      <div className="space-y-2" key={field.name}>
        <Label htmlFor={field.name} className="text-sm">{field.label}</Label>
        <Input
          id={field.name}
          type={field.type === "number" ? "number" : "text"}
          placeholder={field.placeholder}
          value={typeof value === "boolean" ? String(value) : (value ?? "")}
          onChange={(e) => {
            const newValue = field.type === "number"
              ? parseNumericInput(e.target.value)
              : e.target.value;
            updateField(field.name, newValue);
          }}
        />
      </div>
    );
  };

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex flex-col gap-4 md:flex-row md:items-center md:justify-between">
        <div>
          <h1 className="text-2xl font-bold text-gray-900 flex items-center gap-2">
            <Zap className="h-6 w-6 text-blue-600" />
            Simulador de Transação
          </h1>
          <p className="text-gray-600 mt-1">
            Analise transações em tempo real com as 50+ regras duras do RULEX
          </p>
        </div>
        <div className="flex items-center gap-4">
          <div className="flex items-center gap-2">
            <Switch
              id="advanced-rules"
              checked={useAdvancedRules}
              onCheckedChange={setUseAdvancedRules}
            />
            <Label htmlFor="advanced-rules" className="text-sm">
              Regras Avançadas (50+)
            </Label>
          </div>
        </div>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
        {/* Formulário */}
        <div className="lg:col-span-2 space-y-6">
          {/* Templates */}
          <Card>
            <CardHeader className="pb-3">
              <CardTitle className="text-lg flex items-center gap-2">
                <FileJson className="h-5 w-5" />
                Templates de Teste
              </CardTitle>
              <CardDescription>
                Use templates pré-configurados para testar cenários comuns
              </CardDescription>
            </CardHeader>
            <CardContent>
              <div className="grid grid-cols-2 md:grid-cols-4 gap-3">
                {Object.entries(TRANSACTION_TEMPLATES).map(([key, template]) => (
                  <Button
                    key={key}
                    variant="outline"
                    className="h-auto py-3 flex flex-col items-start text-left"
                    onClick={() => applyTemplate(key as keyof typeof TRANSACTION_TEMPLATES)}
                  >
                    <span className="font-medium text-sm">{template.name}</span>
                    <span className="text-xs text-gray-500 mt-1">{template.description}</span>
                  </Button>
                ))}
              </div>
            </CardContent>
          </Card>

          {/* Campos do Formulário */}
          <Card>
            <CardHeader className="pb-3">
              <div className="flex items-center justify-between">
                <CardTitle className="text-lg">Dados da Transação</CardTitle>
                <div className="flex gap-2">
                  <Button variant="ghost" size="sm" onClick={copyJson}>
                    <Copy className="h-4 w-4 mr-1" />
                    Copiar JSON
                  </Button>
                  <Button variant="ghost" size="sm" onClick={clearForm}>
                    <RotateCcw className="h-4 w-4 mr-1" />
                    Limpar
                  </Button>
                </div>
              </div>
            </CardHeader>
            <CardContent>
              <Tabs value={activeTab} onValueChange={setActiveTab}>
                <TabsList className="grid grid-cols-4 lg:grid-cols-7 mb-4">
                  {Object.entries(FIELD_CATEGORIES).map(([key, category]) => (
                    <TabsTrigger key={key} value={key} className="text-xs">
                      <span className="hidden lg:inline mr-1">{category.icon}</span>
                      {category.label}
                    </TabsTrigger>
                  ))}
                </TabsList>

                {Object.entries(FIELD_CATEGORIES).map(([key, category]) => (
                  <TabsContent key={key} value={key}>
                    <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                      {category.fields.map(field => renderField(field))}
                    </div>
                  </TabsContent>
                ))}
              </Tabs>
            </CardContent>
          </Card>

          {/* Botão de Análise */}
          <Button 
            className="w-full h-12 text-lg"
            onClick={handleAnalyze}
            disabled={isLoading}
          >
            {isLoading ? (
              <>
                <Loader2 className="h-5 w-5 mr-2 animate-spin" />
                Analisando...
              </>
            ) : (
              <>
                <Play className="h-5 w-5 mr-2" />
                Analisar Transação
              </>
            )}
          </Button>
        </div>

        {/* Resultado */}
        <div className="space-y-6">
          {/* Status da Análise */}
          <Card className={
            result?.classification === "FRAUD" ? "border-red-500 bg-red-50" :
            result?.classification === "SUSPICIOUS" ? "border-yellow-500 bg-yellow-50" :
            result?.classification === "APPROVED" ? "border-green-500 bg-green-50" :
            ""
          }>
            <CardHeader className="pb-3">
              <CardTitle className="text-lg flex items-center gap-2">
                <Shield className="h-5 w-5" />
                Resultado da Análise
              </CardTitle>
            </CardHeader>
            <CardContent>
              {result ? (
                <div className="space-y-4">
                  {/* Classificação */}
                  <div className="flex items-center justify-between">
                    <span className="text-sm font-medium">Classificação:</span>
                    <Badge
                      variant={
                        result.classification === "FRAUD" ? "destructive" :
                        result.classification === "SUSPICIOUS" ? "secondary" :
                        "default"
                      }
                      className={
                        result.classification === "APPROVED" ? "bg-green-500 hover:bg-green-600" : ""
                      }
                    >
                      {result.classification === "FRAUD" && <XCircle className="h-3 w-3 mr-1" />}
                      {result.classification === "SUSPICIOUS" && <AlertTriangle className="h-3 w-3 mr-1" />}
                      {result.classification === "APPROVED" && <CheckCircle className="h-3 w-3 mr-1" />}
                      {result.classification}
                    </Badge>
                  </div>

                  {/* Score */}
                  <div className="flex items-center justify-between">
                    <span className="text-sm font-medium">Score Total:</span>
                    <span className={`text-2xl font-bold ${
                      result.totalScore >= 150 ? "text-red-600" :
                      result.totalScore >= 80 ? "text-yellow-600" :
                      "text-green-600"
                    }`}>
                      {result.totalScore}
                    </span>
                  </div>

                  {/* Regras Acionadas */}
                  <div className="flex items-center justify-between">
                    <span className="text-sm font-medium">Regras Acionadas:</span>
                    <span className="text-lg font-semibold">{result.triggeredRules.length}</span>
                  </div>

                  <Separator />

                  {/* Motivo */}
                  <div className="p-3 bg-white rounded-lg border">
                    <div className="flex items-start gap-2">
                      <Info className="h-4 w-4 mt-0.5 text-gray-500" />
                      <p className="text-sm text-gray-700">{result.reason}</p>
                    </div>
                  </div>
                </div>
              ) : (
                <div className="text-center py-8 text-gray-500">
                  <Shield className="h-12 w-12 mx-auto mb-3 opacity-50" />
                  <p>Preencha os dados e clique em "Analisar Transação"</p>
                </div>
              )}
            </CardContent>
          </Card>

          {/* Regras Acionadas */}
          {result && result.ruleDetails.length > 0 && (
            <Card>
              <CardHeader className="pb-3">
                <CardTitle className="text-lg flex items-center gap-2">
                  <AlertTriangle className="h-5 w-5 text-yellow-500" />
                  Regras Acionadas ({result.ruleDetails.length})
                </CardTitle>
              </CardHeader>
              <CardContent>
                <ScrollArea className="h-[300px]">
                  <div className="space-y-3">
                    {result.ruleDetails.map((rule, index) => (
                      <div 
                        key={index}
                        className="p-3 bg-gray-50 rounded-lg border"
                      >
                        <div className="flex items-center justify-between mb-1">
                          <span className="font-medium text-sm">{rule.ruleName}</span>
                          <Badge variant="outline" className="text-xs">
                            +{rule.score} pts
                          </Badge>
                        </div>
                        <p className="text-xs text-gray-600">{rule.ruleDescription}</p>
                      </div>
                    ))}
                  </div>
                </ScrollArea>
              </CardContent>
            </Card>
          )}

          {/* JSON Preview */}
          <Card>
            <CardHeader className="pb-3">
              <CardTitle className="text-lg flex items-center gap-2">
                <FileJson className="h-5 w-5" />
                JSON da Requisição
              </CardTitle>
            </CardHeader>
            <CardContent>
              <ScrollArea className="h-[200px]">
                <pre className="text-xs bg-gray-900 text-green-400 p-3 rounded-lg overflow-x-auto">
                  {JSON.stringify(formData, null, 2) || "{}"}
                </pre>
              </ScrollArea>
            </CardContent>
          </Card>
        </div>
      </div>
    </div>
  );
}
