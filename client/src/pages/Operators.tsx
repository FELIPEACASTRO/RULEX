import { BACKEND_OPERATORS } from "@/manual/generated/backendOperators.generated";

type Operator = (typeof BACKEND_OPERATORS)[number];

const normalizeCategory = (category?: string) => {
  const normalized = category?.trim();
  if (!normalized || normalized === "=" || normalized.toLowerCase() === "natural") {
    return "Geral";
  }
  return normalized;
};

const CATEGORY_GUIDE: Record<string, { title: string; purpose: string; example: string }> = {
  "Comparação básica": {
    title: "Comparações simples",
    purpose: "Comparar valores numéricos ou textuais.",
    example: "Ex.: valor > 100",
  },
  Listas: {
    title: "Listas",
    purpose: "Verificar se um item está dentro de uma lista.",
    example: "Ex.: canal em [\"WEB\", \"APP\"]",
  },
  Strings: {
    title: "Texto",
    purpose: "Validar trechos, padrões e formatos de texto.",
    example: "Ex.: descrição contém \"frete\"",
  },
  Nulos: {
    title: "Campos vazios",
    purpose: "Checar se um campo está vazio ou preenchido.",
    example: "Ex.: telefone está vazio",
  },
  Booleanos: {
    title: "Verdadeiro/Falso",
    purpose: "Validar campos booleanos.",
    example: "Ex.: fraude = verdadeiro",
  },
  Range: {
    title: "Faixas",
    purpose: "Confirmar se o valor está entre limites.",
    example: "Ex.: valor entre 10 e 50",
  },
  "Comparação entre campos": {
    title: "Campo vs campo",
    purpose: "Comparar dois campos do mesmo registro.",
    example: "Ex.: total = subtotal + frete",
  },
  "Funções de data/tempo": {
    title: "Datas e horas",
    purpose: "Aplicar regras de calendário e horário.",
    example: "Ex.: compra fora do horário comercial",
  },
  "Funções de lista/array": {
    title: "Listas/arrays",
    purpose: "Medir tamanho e conteúdo de listas.",
    example: "Ex.: itens.size > 3",
  },
  "Funções matemáticas": {
    title: "Matemática",
    purpose: "Calcular valores e diferenças.",
    example: "Ex.: abs(valor) > 1000",
  },
  Geolocalização: {
    title: "Localização",
    purpose: "Verificar país, cidade ou distância.",
    example: "Ex.: compra em cidade diferente",
  },
  "Operadores lógicos": {
    title: "Lógica",
    purpose: "Combinar condições (AND/OR/NOT).",
    example: "Ex.: (valor > 100) e (país = \"BR\")",
  },
  Geral: {
    title: "Geral",
    purpose: "Operadores variados para regras comuns.",
    example: "Ex.: aplicar regra ao contexto",
  },
};

const derivePurpose = (operator: Operator) => {
  const comment = operator.comment?.trim();
  if (comment) {
    return comment;
  }

  const name = operator.name.toUpperCase();

  if (["AND", "OR", "NOT", "NAND", "NOR", "XOR"].includes(name)) {
    return "Combinar condições lógicas.";
  }
  if (name.includes("BETWEEN")) {
    return "Validar se um valor está dentro de uma faixa.";
  }
  if (
    name.includes("CONTAINS") ||
    name.includes("REGEX") ||
    name.includes("STARTS_WITH") ||
    name.includes("ENDS_WITH")
  ) {
    return "Verificar padrões ou trechos em texto.";
  }
  if (name.includes("IN_LIST") || name.includes("NOT_IN") || name.endsWith("_IN") || name === "IN") {
    return "Checar pertencimento a uma lista.";
  }
  if (
    name.includes("COUNT") ||
    name.includes("SUM") ||
    name.includes("AVG") ||
    name.includes("MAX") ||
    name.includes("MIN") ||
    name.includes("PERCENT")
  ) {
    return "Calcular agregações e indicadores.";
  }
  if (
    name.includes("GT") ||
    name.includes("GTE") ||
    name.includes("LT") ||
    name.includes("LTE") ||
    name.includes("EQ") ||
    name.includes("NEQ")
  ) {
    return "Comparar valores e limites.";
  }
  if (name.startsWith("IS_") || name.includes("NULL")) {
    return "Validar estado ou presença de dados.";
  }
  if (
    name.includes("DATE") ||
    name.includes("DAY") ||
    name.includes("HOUR") ||
    name.includes("WEEK") ||
    name.includes("TIME")
  ) {
    return "Aplicar regras de tempo e calendário.";
  }
  if (name.includes("GEO") || name.includes("DISTANCE")) {
    return "Validar localização e distância.";
  }
  if (name.includes("DEVICE") || name.includes("FINGERPRINT")) {
    return "Verificar sinais do dispositivo.";
  }
  if (name.startsWith("FATF_")) {
    return "Aplicar tipologias e controles de AML.";
  }
  if (name.startsWith("SCA_") || name.startsWith("PSD") || name.startsWith("DORA")) {
    return "Aplicar requisitos regulatórios de autenticação e resiliência.";
  }
  if (name.startsWith("BSL_")) {
    return "Aplicar políticas de risco operacional.";
  }
  if (name.startsWith("NEO4J_")) {
    return "Analisar relações em grafo.";
  }
  if (name.startsWith("PLT_")) {
    return "Aplicar boas práticas de plataforma.";
  }
  if (name.includes("MCC")) {
    return "Avaliar categoria do merchant (MCC).";
  }
  if (name.includes("EMAIL") || name.includes("PHONE") || name.includes("CPF")) {
    return "Validar dados cadastrais.";
  }
  if (name.includes("AMOUNT")) {
    return "Avaliar comportamento de valor da transação.";
  }
  if (name.includes("CARD")) {
    return "Avaliar dados e uso do cartão.";
  }

  return `Regra aplicada para a categoria ${normalizeCategory(operator.category)}.`;
};

const deriveExample = (name: string) => {
  const upper = name.toUpperCase();

  if (upper === "AND") return "Ex.: (valor > 100) e (país = \"BR\")";
  if (upper === "OR") return "Ex.: (canal = \"APP\") ou (canal = \"WEB\")";
  if (upper === "NOT") return "Ex.: não (status = \"APROVADO\")";
  if (upper === "EQ") return "Ex.: status = \"APROVADO\"";
  if (upper === "NEQ") return "Ex.: país != \"BR\"";
  if (upper === "GT") return "Ex.: valor > 100";
  if (upper === "GTE") return "Ex.: valor >= 100";
  if (upper === "LT") return "Ex.: valor < 50";
  if (upper === "LTE") return "Ex.: valor <= 50";
  if (upper.includes("BETWEEN")) return "Ex.: valor entre 10 e 50";
  if (upper.includes("NOT_BETWEEN")) return "Ex.: valor fora de 10 a 50";
  if (upper.includes("IN_LIST") || upper === "IN") return "Ex.: canal em [\"WEB\", \"APP\"]";
  if (upper.includes("NOT_IN")) return "Ex.: país não está em [\"BR\", \"AR\"]";
  if (upper.includes("CONTAINS")) return "Ex.: descrição contém \"frete\"";
  if (upper.includes("NOT_CONTAINS")) return "Ex.: email não contém \"teste\"";
  if (upper.includes("REGEX")) return "Ex.: email casa com /@empresa\\.com/";
  if (upper.includes("DATE_BEFORE")) return "Ex.: data antes de 2025-01-01";
  if (upper.includes("DATE_AFTER")) return "Ex.: data depois de 2025-01-01";
  if (upper.includes("IS_TRUE")) return "Ex.: fraude = verdadeiro";
  if (upper.includes("IS_FALSE")) return "Ex.: chargeback = falso";
  if (upper.includes("NOT_NULL")) return "Ex.: campo preenchido";
  if (upper.includes("IS_NULL")) return "Ex.: campo vazio";
  if (upper.includes("ARRAY_CONTAINS")) return "Ex.: tags contém \"vip\"";
  if (upper.includes("ARRAY_SIZE_GT")) return "Ex.: itens.size > 3";
  if (upper.includes("ARRAY_SIZE_LT")) return "Ex.: itens.size < 3";
  if (upper.includes("ARRAY_SIZE_EQ")) return "Ex.: itens.size = 3";
  if (upper.includes("COUNT")) return "Ex.: contar eventos nas últimas 24h";
  if (upper.includes("SUM")) return "Ex.: soma de valores no dia";
  if (upper.includes("AVG")) return "Ex.: média de valores na semana";
  if (upper.includes("MAX")) return "Ex.: maior valor nos últimos 7 dias";
  if (upper.includes("MIN")) return "Ex.: menor valor nos últimos 7 dias";
  if (upper.includes("GEO") || upper.includes("DISTANCE")) return "Ex.: compra em cidade diferente";
  if (upper.includes("DEVICE") || upper.includes("FINGERPRINT")) return "Ex.: dispositivo novo na sessão";
  if (upper.startsWith("FATF_")) return "Ex.: país em lista de alto risco";
  if (upper.startsWith("SCA_") || upper.startsWith("PSD") || upper.startsWith("DORA")) {
    return "Ex.: exigir autenticação forte";
  }
  if (upper.startsWith("BSL_")) return "Ex.: aplicar política interna";
  if (upper.startsWith("NEO4J_")) return "Ex.: ligação com conta suspeita";
  if (upper.startsWith("PLT_")) return "Ex.: padrão suspeito na plataforma";
  if (upper.includes("MCC")) return "Ex.: MCC de apostas";
  if (upper.includes("EMAIL") || upper.includes("PHONE") || upper.includes("CPF")) {
    return "Ex.: CPF em múltiplas contas";
  }
  if (upper.includes("AMOUNT")) return "Ex.: valor acima do habitual";
  if (upper.includes("CARD")) return "Ex.: cartão em blacklist";

  return "Ex.: aplicar regra ao contexto da transação.";
};

const getCategoryGuide = (category: string) =>
  CATEGORY_GUIDE[category] ?? {
    title: category,
    purpose: `Entenda esta categoria com base no nome: ${category}.`,
    example: "Ex.: aplique a regra ao cenário do seu negócio.",
  };

export default function Operators() {
  const operators = BACKEND_OPERATORS.map((operator) => ({
    ...operator,
    type: normalizeCategory(operator.category),
    purpose: derivePurpose(operator),
    example: deriveExample(operator.name),
  }));

  const grouped = operators.reduce<Record<string, typeof operators>>((acc, op) => {
    acc[op.type] ??= [];
    acc[op.type].push(op);
    return acc;
  }, {});

  const categories = Object.keys(grouped).sort((a, b) => a.localeCompare(b, "pt-BR"));

  return (
    <div className="space-y-6">
      <div className="rounded-lg border bg-card p-5">
        <h1 className="text-xl font-semibold text-foreground">Operadores</h1>
        <p className="text-sm text-muted-foreground">
          Lista completa dos operadores suportados pelo RULEX ({operators.length}).
        </p>
        <p className="text-xs text-muted-foreground">Exemplos didáticos, linguagem simples.</p>
        <div className="mt-4 rounded-md border bg-background px-4 py-3 text-sm text-muted-foreground">
          <div className="font-medium text-foreground">Como ler esta página</div>
          <ul className="mt-2 list-disc space-y-1 pl-5">
            <li>
              <strong>Nome:</strong> o identificador do operador no motor.
            </li>
            <li>
              <strong>Tipo:</strong> a categoria do operador no backend.
            </li>
            <li>
              <strong>Para que serve:</strong> explicação simples do uso.
            </li>
            <li>
              <strong>Exemplo:</strong> um caso real do dia a dia.
            </li>
          </ul>
        </div>
      </div>

      {categories.map((category) => {
        const guide = getCategoryGuide(category);
        const list = grouped[category];

        return (
          <section key={category} className="space-y-3">
            <div className="rounded-lg border bg-card p-4">
              <div className="text-sm font-semibold text-foreground">{guide.title}</div>
              <p className="text-sm text-muted-foreground">{guide.purpose}</p>
              <p className="text-xs text-muted-foreground">{guide.example}</p>
            </div>

            <div className="grid gap-3 sm:grid-cols-2 lg:grid-cols-3">
              {list.map((operator) => (
                <div key={operator.name} className="rounded-lg border bg-card p-4 shadow-sm">
                  <div className="flex items-start justify-between gap-2">
                    <h2 className="text-sm font-semibold text-foreground">{operator.name}</h2>
                    <span className="rounded-full border px-2 py-0.5 text-xs text-muted-foreground">
                      {operator.type}
                    </span>
                  </div>
                  <div className="mt-2 space-y-2 text-sm text-muted-foreground">
                    <p>
                      <span className="font-medium text-foreground">Para que serve:</span>{" "}
                      {operator.purpose}
                    </p>
                    <p>
                      <span className="font-medium text-foreground">Exemplo:</span> {operator.example}
                    </p>
                  </div>
                </div>
              ))}
            </div>
          </section>
        );
      })}
    </div>
  );
}
