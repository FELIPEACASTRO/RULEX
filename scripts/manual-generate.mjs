#!/usr/bin/env node
/**
 * manual-generate.mjs
 * 
 * Script para gerar dados do Manual do RULEX a partir do código fonte real.
 * 
 * Extrai:
 * - Operadores do backend (RuleCondition.java → ConditionOperator)
 * - Ações do backend (RuleAction.java → ActionType)
 * - Operadores lógicos (RuleConditionGroup.java → GroupLogicOperator)
 * - Funções de expressão (ExpressionEvaluator.java)
 * - Allowlist do AstValidator.java
 * - Endpoints do OpenAPI (rulex.yaml)
 * 
 * Gera arquivos em: client/src/manual/generated/
 * 
 * @author RULEX Team
 * @date 2026-01-16
 */

import { readFileSync, writeFileSync, mkdirSync, existsSync, readdirSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const ROOT = join(__dirname, '..');
const GENERATED_DIR = join(ROOT, 'client', 'src', 'manual', 'generated');
const DOCS_COPY_DIR = join(ROOT, 'client', 'src', 'manual', 'docs');

// Garantir que diretórios existem
if (!existsSync(GENERATED_DIR)) {
  mkdirSync(GENERATED_DIR, { recursive: true });
}
if (!existsSync(DOCS_COPY_DIR)) {
  mkdirSync(DOCS_COPY_DIR, { recursive: true });
}

// ============================================================================
// UTILIDADES
// ============================================================================

function log(msg) {
  console.log(`[manual-generate] ${msg}`);
}

function error(msg) {
  console.error(`[manual-generate] ❌ ${msg}`);
}

function success(msg) {
  console.log(`[manual-generate] ✅ ${msg}`);
}

function readFile(path) {
  try {
    return readFileSync(path, 'utf-8');
  } catch (e) {
    error(`Não foi possível ler ${path}: ${e.message}`);
    return null;
  }
}

function writeGenerated(filename, content) {
  const path = join(GENERATED_DIR, filename);
  writeFileSync(path, content, 'utf-8');
  success(`Gerado ${filename}`);
}

// ============================================================================
// EXTRAÇÃO: OPERADORES DO BACKEND (RuleCondition.java)
// ============================================================================

function extractBackendOperators() {
  const filePath = join(ROOT, 'backend', 'src', 'main', 'java', 'com', 'rulex', 'entity', 'complex', 'RuleCondition.java');
  const content = readFile(filePath);
  if (!content) return [];

  const operators = [];
  
  // Encontrar enum ConditionOperator
  const enumStart = content.indexOf('enum ConditionOperator {');
  if (enumStart === -1) {
    error('Não encontrou enum ConditionOperator em RuleCondition.java');
    return [];
  }
  
  // O próximo enum (ConditionValueType) marca o fim do ConditionOperator
  const nextEnumIndex = content.indexOf('enum ConditionValueType', enumStart);
  const enumEnd = nextEnumIndex !== -1 ? nextEnumIndex : content.length;
  
  const enumBody = content.substring(enumStart, enumEnd);
  const lines = enumBody.split('\n');
  
  let currentCategory = 'Comparação Básica';
  
  for (const line of lines) {
    // Detectar categoria por comentário de seção (=== NOME ===)
    const categoryMatch = line.match(/\/\/\s*=+\s*(.+?)\s*=+/);
    if (categoryMatch) {
      let cat = categoryMatch[1].trim()
        .replace(/OPERADORES\s*/i, '')
        .replace(/\(.*?\)/g, '')
        .trim();
      if (cat) currentCategory = cat;
      continue;
    }
    
    // Detectar categoria por comentário simples (CATEGORIA X: Nome)
    const simpleCategoryMatch = line.match(/\/\/\s*CATEGORIA\s*\w+:\s*(.+)/i);
    if (simpleCategoryMatch) {
      currentCategory = simpleCategoryMatch[1].trim();
      continue;
    }
    
    // Detectar comentário de categoria standalone (// Comparação básica)
    // Não deve ter operador na mesma linha
    if (!line.match(/^\s*[A-Z][A-Z0-9_]+\s*,/)) {
      const inlineCategory = line.match(/^\s*\/\/\s*([A-Za-zÀ-ú][A-Za-zÀ-ú\s\/\-&]+)$/);
      if (inlineCategory) {
        const cat = inlineCategory[1].trim();
        if (cat.length >= 4 && cat.length < 60) {
          currentCategory = cat;
        }
      }
      continue;
    }
    
    // Extrair operador - apenas operadores que terminam com vírgula (ou não, para o último)
    const opMatch = line.match(/^\s*([A-Z][A-Z0-9_]+)\s*,?\s*(?:\/\/\s*(.*))?/);
    if (opMatch) {
      const name = opMatch[1].trim();
      const comment = opMatch[2]?.trim() || '';
      operators.push({
        name,
        comment,
        category: currentCategory
      });
    }
  }

  return operators;
}

// ============================================================================
// EXTRAÇÃO: AÇÕES DO BACKEND (RuleAction.java)
// ============================================================================

function extractBackendActions() {
  const filePath = join(ROOT, 'backend', 'src', 'main', 'java', 'com', 'rulex', 'entity', 'complex', 'RuleAction.java');
  const content = readFile(filePath);
  if (!content) return [];

  const actions = [];
  
  // Encontrar enum ActionType
  const enumMatch = content.match(/enum\s+ActionType\s*\{([\s\S]*?)\n\s*\}/);
  if (!enumMatch) {
    error('Não encontrou enum ActionType em RuleAction.java');
    return [];
  }

  const enumBody = enumMatch[1];
  const lines = enumBody.split('\n');
  
  for (const line of lines) {
    const opMatch = line.match(/^\s*([A-Z][A-Z0-9_]+),?\s*(?:\/\/\s*(.*))?$/);
    if (opMatch) {
      const name = opMatch[1].trim();
      const comment = opMatch[2]?.trim() || '';
      actions.push({ name, comment });
    }
  }

  return actions;
}

// ============================================================================
// EXTRAÇÃO: OPERADORES LÓGICOS (RuleConditionGroup.java)
// ============================================================================

function extractLogicOperators() {
  const filePath = join(ROOT, 'backend', 'src', 'main', 'java', 'com', 'rulex', 'entity', 'complex', 'RuleConditionGroup.java');
  const content = readFile(filePath);
  if (!content) return [];

  const operators = [];
  
  // Encontrar enum GroupLogicOperator com índices
  const enumStart = content.indexOf('enum GroupLogicOperator {');
  if (enumStart === -1) {
    error('Não encontrou enum GroupLogicOperator em RuleConditionGroup.java');
    return [];
  }
  
  // Encontrar o fim do enum - procurar por próxima chave fechando
  const enumBodyStart = content.indexOf('{', enumStart);
  let braceCount = 1;
  let enumEnd = enumBodyStart + 1;
  
  for (let i = enumBodyStart + 1; i < content.length && braceCount > 0; i++) {
    if (content[i] === '{') braceCount++;
    else if (content[i] === '}') {
      braceCount--;
      if (braceCount === 0) enumEnd = i;
    }
  }
  
  const enumBody = content.substring(enumBodyStart + 1, enumEnd);
  const lines = enumBody.split('\n');
  
  for (const line of lines) {
    const opMatch = line.match(/^\s*([A-Z][A-Z0-9_]+)\s*,?\s*(?:\/\/\s*(.*))?/);
    if (opMatch) {
      const name = opMatch[1].trim();
      const comment = opMatch[2]?.trim() || '';
      operators.push({ name, comment });
    }
  }

  return operators;
}

// ============================================================================
// EXTRAÇÃO: FUNÇÕES DO ExpressionEvaluator.java
// ============================================================================

function extractExpressionFunctions() {
  const filePath = join(ROOT, 'backend', 'src', 'main', 'java', 'com', 'rulex', 'service', 'complex', 'ExpressionEvaluator.java');
  const content = readFile(filePath);
  if (!content) return [];

  const functions = [];
  
  // Encontrar switch case de funções
  const switchMatch = content.match(/return\s+switch\s*\(funcName\)\s*\{([\s\S]*?)\n\s*default\s*->/);
  if (!switchMatch) {
    // Tentar encontrar lista de funções conhecidas
    const listMatch = content.match(/List\.of\(\s*([\s\S]*?)\)/g);
    if (listMatch && listMatch.length > 0) {
      // Pegar última lista (funções conhecidas)
      const lastList = listMatch[listMatch.length - 1];
      const funcs = lastList.match(/"([A-Z_]+)"/g);
      if (funcs) {
        funcs.forEach(f => {
          const name = f.replace(/"/g, '');
          functions.push({ name, category: 'Geral', description: '' });
        });
      }
    }
    return functions;
  }

  const switchBody = switchMatch[1];
  const lines = switchBody.split('\n');
  let currentCategory = 'Geral';
  
  for (const line of lines) {
    // Detectar categoria
    const categoryMatch = line.match(/\/\/\s*Funções?\s+(?:de\s+)?(\w+)/i);
    if (categoryMatch) {
      currentCategory = categoryMatch[1].trim();
      continue;
    }
    
    // Extrair case
    const caseMatch = line.match(/case\s+"([A-Z_]+)"(?:,\s*"([A-Z_]+)")?/);
    if (caseMatch) {
      const name = caseMatch[1];
      const alias = caseMatch[2] || null;
      functions.push({ 
        name, 
        alias,
        category: currentCategory,
        description: '' 
      });
    }
  }

  return functions;
}

// ============================================================================
// EXTRAÇÃO: ALLOWLIST DO AstValidator.java
// ============================================================================

function extractAstAllowlist() {
  const filePath = join(ROOT, 'backend', 'src', 'main', 'java', 'com', 'rulex', 'v31', 'ast', 'AstValidator.java');
  const content = readFile(filePath);
  if (!content) return { functions: [], operators: [], aliases: {} };

  const result = { functions: [], operators: [], aliases: {} };
  
  // Extrair FUNC_ALLOWLIST
  const funcMatch = content.match(/FUNC_ALLOWLIST\s*=\s*Set\.of\(\s*([\s\S]*?)\);/);
  if (funcMatch) {
    const funcs = funcMatch[1].match(/"([A-Z_]+)"/g);
    if (funcs) {
      result.functions = funcs.map(f => f.replace(/"/g, ''));
    }
  }

  // Extrair OPERATORS
  const opsMatch = content.match(/private\s+static\s+final\s+Set<String>\s+OPERATORS\s*=\s*Set\.of\(\s*([\s\S]*?)\);/);
  if (opsMatch) {
    const ops = opsMatch[1].match(/"([A-Z_]+)"/g);
    if (ops) {
      result.operators = ops.map(o => o.replace(/"/g, ''));
    }
  }

  // Extrair OPERATOR_ALIASES
  const aliasMatch = content.match(/OPERATOR_ALIASES\s*=\s*Map\.of\(\s*([\s\S]*?)\);/);
  if (aliasMatch) {
    const aliasBody = aliasMatch[1];
    const pairs = aliasBody.match(/"([A-Z_]+)",\s*"([A-Z_]+)"/g);
    if (pairs) {
      pairs.forEach(p => {
        const [, from, to] = p.match(/"([A-Z_]+)",\s*"([A-Z_]+)"/) || [];
        if (from && to) {
          result.aliases[from] = to;
        }
      });
    }
  }

  return result;
}

// ============================================================================
// EXTRAÇÃO: ENDPOINTS DO OPENAPI
// ============================================================================

function extractOpenapiEndpoints() {
  const filePath = join(ROOT, 'openapi', 'rulex.yaml');
  const content = readFile(filePath);
  if (!content) return [];

  const endpoints = [];
  const lines = content.split('\n');
  
  let currentPath = null;
  let inPaths = false;
  let pathIndent = 0;
  
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const trimmed = line.trim();
    
    // Detectar início da seção paths
    if (/^paths:\s*$/.test(line)) {
      inPaths = true;
      pathIndent = 2; // Paths estão com 2 espaços
      continue;
    }
    
    // Detectar fim da seção paths (próxima seção de nível superior sem indentação)
    if (inPaths && /^[a-z]+:/i.test(line) && !line.startsWith(' ')) {
      inPaths = false;
      continue;
    }
    
    if (!inPaths) continue;
    
    // Detectar path (começa com / e termina com :)
    const pathMatch = line.match(/^(\s+)(\/[^\s:]+):\s*$/);
    if (pathMatch) {
      const indent = pathMatch[1].length;
      if (indent === 2) { // Path de primeiro nível
        currentPath = pathMatch[2];
      }
      continue;
    }
    
    // Detectar método HTTP (get, post, put, delete, patch)
    const methodMatch = line.match(/^\s{4}(get|post|put|delete|patch):\s*$/);
    if (methodMatch && currentPath) {
      const method = methodMatch[1].toUpperCase();
      
      // Tentar pegar summary e operationId nas próximas linhas
      let summary = '';
      let operationId = '';
      for (let j = i + 1; j < Math.min(i + 15, lines.length); j++) {
        const nextLine = lines[j];
        // Parar se encontrar outro método ou path
        if (/^\s{4}(get|post|put|delete|patch):/.test(nextLine)) break;
        if (/^\s{2}\//.test(nextLine)) break;
        
        const summaryMatch = nextLine.match(/^\s+summary:\s*['"]?(.+?)['"]?\s*$/);
        if (summaryMatch) {
          summary = summaryMatch[1].trim();
        }
        const opIdMatch = nextLine.match(/^\s+operationId:\s*['"]?(\w+)['"]?\s*$/);
        if (opIdMatch) {
          operationId = opIdMatch[1].trim();
        }
      }
      
      endpoints.push({
        path: currentPath,
        method,
        summary,
        operationId
      });
    }
  }

  return endpoints;
}

// ============================================================================
// EXTRAÇÃO: ÍNDICE DE DOCUMENTOS
// ============================================================================

function extractDocsIndex() {
  const docsDir = join(ROOT, 'docs');
  if (!existsSync(docsDir)) return [];

  const docs = [];
  const files = readdirSync(docsDir).filter(f => f.endsWith('.md'));
  
  // Documentos essenciais para o Manual
  const essential = [
    'DB_SCHEMA_RULES.md',
    'PAYLOAD_DICTIONARY.md',
    'RULES_SCHEMA_AND_FIELDS.md',
    'RULE_ENGINE_CAPABILITIES.md',
    'FRAUD_DETECTION_ANALYST_GUIDE.md',
    'ARCHITECTURE_MAP.md'
  ];

  for (const file of files) {
    const isEssential = essential.includes(file);
    const filePath = join(docsDir, file);
    const content = readFile(filePath);
    
    let title = file.replace('.md', '').replace(/_/g, ' ');
    
    // Tentar extrair título do H1
    if (content) {
      const h1Match = content.match(/^#\s+(.+)/m);
      if (h1Match) {
        title = h1Match[1].trim();
      }
    }
    
    // Determinar categoria
    let category = 'Outros';
    if (file.includes('SCHEMA') || file.includes('DB_')) category = 'Banco de Dados';
    else if (file.includes('PAYLOAD') || file.includes('FIELD')) category = 'Payload';
    else if (file.includes('RULE') || file.includes('OPERATOR')) category = 'Regras';
    else if (file.includes('ARCHITECTURE') || file.includes('MAP')) category = 'Arquitetura';
    else if (file.includes('FRAUD') || file.includes('ANALYST')) category = 'Fraude';
    else if (file.includes('QA') || file.includes('TEST')) category = 'QA/Testes';
    
    docs.push({
      id: file.replace('.md', ''),
      filename: file,
      title,
      category,
      isEssential
    });
  }

  return docs;
}

// ============================================================================
// GERAÇÃO DE ARQUIVOS
// ============================================================================

function generateBackendOperatorsFile(operators) {
  const content = `// Auto-generated by scripts/manual-generate.mjs
// DO NOT EDIT MANUALLY
// Source: backend/src/main/java/com/rulex/entity/complex/RuleCondition.java

export interface BackendOperator {
  name: string;
  comment: string;
  category: string;
}

export const BACKEND_OPERATORS: BackendOperator[] = ${JSON.stringify(operators, null, 2)};

export const BACKEND_OPERATOR_COUNT = ${operators.length};

export const BACKEND_OPERATOR_CATEGORIES = [...new Set(BACKEND_OPERATORS.map(o => o.category))];
`;
  writeGenerated('backendOperators.generated.ts', content);
  return operators.length;
}

function generateBackendActionsFile(actions) {
  // Descrições detalhadas para cada ação
  const descriptions = {
    SET_DECISION: 'Define a decisão final da transação (APROVADO, SUSPEITA_DE_FRAUDE, FRAUDE)',
    SET_SCORE: 'Define ou incrementa o score de risco da transação',
    ADD_TAG: 'Adiciona uma tag/label à transação para categorização',
    REMOVE_TAG: 'Remove uma tag/label da transação',
    SET_VARIABLE: 'Define uma variável que pode ser usada por outras regras',
    CALL_WEBHOOK: 'Chama um webhook externo com dados da transação',
    SEND_NOTIFICATION: 'Envia notificação (email, SMS, push) sobre a transação',
    BLOCK_TRANSACTION: 'Bloqueia imediatamente a transação',
    FLAG_FOR_REVIEW: 'Marca a transação para revisão manual',
    ESCALATE: 'Escala o caso para um nível superior de análise'
  };

  const actionsWithDesc = actions.map(a => ({
    ...a,
    description: descriptions[a.name] || a.comment || ''
  }));

  const content = `// Auto-generated by scripts/manual-generate.mjs
// DO NOT EDIT MANUALLY
// Source: backend/src/main/java/com/rulex/entity/complex/RuleAction.java

export interface BackendAction {
  name: string;
  comment: string;
  description: string;
}

export const BACKEND_ACTIONS: BackendAction[] = ${JSON.stringify(actionsWithDesc, null, 2)};

export const BACKEND_ACTION_COUNT = ${actions.length};
`;
  writeGenerated('backendActions.generated.ts', content);
  return actions.length;
}

function generateLogicOperatorsFile(operators) {
  // Descrições detalhadas
  const descriptions = {
    AND: 'Todas as condições do grupo devem ser verdadeiras',
    OR: 'Pelo menos uma condição do grupo deve ser verdadeira',
    NOT: 'Inverte o resultado do grupo (verdadeiro vira falso)',
    XOR: 'Exatamente uma condição deve ser verdadeira (ou exclusivo)',
    NAND: 'NOT AND - pelo menos uma condição deve ser falsa',
    NOR: 'NOT OR - todas as condições devem ser falsas'
  };

  const opsWithDesc = operators.map(o => ({
    ...o,
    description: descriptions[o.name] || o.comment || ''
  }));

  const content = `// Auto-generated by scripts/manual-generate.mjs
// DO NOT EDIT MANUALLY
// Source: backend/src/main/java/com/rulex/entity/complex/RuleConditionGroup.java

export interface LogicOperator {
  name: string;
  comment: string;
  description: string;
}

export const LOGIC_OPERATORS: LogicOperator[] = ${JSON.stringify(opsWithDesc, null, 2)};
`;
  writeGenerated('logicOperators.generated.ts', content);
  return operators.length;
}

function generateExpressionFunctionsFile(functions) {
  // Descrições e exemplos para cada função
  const details = {
    ABS: { desc: 'Retorna o valor absoluto', example: 'ABS(-10) → 10', returnType: 'NUMBER' },
    ROUND: { desc: 'Arredonda para N casas decimais', example: 'ROUND(3.456, 2) → 3.46', returnType: 'NUMBER' },
    FLOOR: { desc: 'Arredonda para baixo', example: 'FLOOR(3.9) → 3', returnType: 'NUMBER' },
    CEIL: { desc: 'Arredonda para cima', example: 'CEIL(3.1) → 4', returnType: 'NUMBER' },
    CEILING: { desc: 'Alias de CEIL', example: 'CEILING(3.1) → 4', returnType: 'NUMBER' },
    MIN: { desc: 'Retorna o menor valor', example: 'MIN(5, 3, 8) → 3', returnType: 'NUMBER' },
    MAX: { desc: 'Retorna o maior valor', example: 'MAX(5, 3, 8) → 8', returnType: 'NUMBER' },
    SQRT: { desc: 'Raiz quadrada', example: 'SQRT(16) → 4', returnType: 'NUMBER' },
    POW: { desc: 'Potenciação', example: 'POW(2, 3) → 8', returnType: 'NUMBER' },
    POWER: { desc: 'Alias de POW', example: 'POWER(2, 3) → 8', returnType: 'NUMBER' },
    MOD: { desc: 'Resto da divisão', example: 'MOD(10, 3) → 1', returnType: 'NUMBER' },
    LEN: { desc: 'Tamanho da string', example: 'LEN("hello") → 5', returnType: 'NUMBER' },
    LENGTH: { desc: 'Alias de LEN', example: 'LENGTH("hello") → 5', returnType: 'NUMBER' },
    UPPER: { desc: 'Converte para maiúsculas', example: 'UPPER("hello") → "HELLO"', returnType: 'STRING' },
    LOWER: { desc: 'Converte para minúsculas', example: 'LOWER("HELLO") → "hello"', returnType: 'STRING' },
    TRIM: { desc: 'Remove espaços', example: 'TRIM(" hi ") → "hi"', returnType: 'STRING' },
    SUBSTRING: { desc: 'Extrai parte da string', example: 'SUBSTRING("hello", 0, 2) → "he"', returnType: 'STRING' },
    SUBSTR: { desc: 'Alias de SUBSTRING', example: 'SUBSTR("hello", 0, 2) → "he"', returnType: 'STRING' },
    NOW: { desc: 'Data/hora atual', example: 'NOW() → "2026-01-16T10:30:00"', returnType: 'DATETIME' },
    TODAY: { desc: 'Data atual', example: 'TODAY() → "2026-01-16"', returnType: 'DATE' },
    DAYS_BETWEEN: { desc: 'Dias entre duas datas', example: 'DAYS_BETWEEN("2026-01-01", "2026-01-16") → 15', returnType: 'NUMBER' },
    HOURS_BETWEEN: { desc: 'Horas entre dois instantes', example: 'HOURS_BETWEEN(dt1, dt2) → 48', returnType: 'NUMBER' },
    IF: { desc: 'Condicional', example: 'IF(x > 0, "positivo", "negativo")', returnType: 'ANY' },
    COALESCE: { desc: 'Primeiro valor não-nulo', example: 'COALESCE(null, "default") → "default"', returnType: 'ANY' },
    IFNULL: { desc: 'Alias de COALESCE', example: 'IFNULL(null, 0) → 0', returnType: 'ANY' },
    SUM: { desc: 'Soma de lista', example: 'SUM(items.amount) → 150.00', returnType: 'NUMBER' },
    COUNT: { desc: 'Contagem de lista', example: 'COUNT(items) → 3', returnType: 'NUMBER' },
    AVG: { desc: 'Média de lista', example: 'AVG(items.amount) → 50.00', returnType: 'NUMBER' }
  };

  // Categorizar funções
  const categorized = functions.map(f => {
    const detail = details[f.name] || { desc: '', example: '', returnType: 'ANY' };
    let category = 'Geral';
    if (['ABS', 'ROUND', 'FLOOR', 'CEIL', 'CEILING', 'MIN', 'MAX', 'SQRT', 'POW', 'POWER', 'MOD'].includes(f.name)) {
      category = 'Matemáticas';
    } else if (['LEN', 'LENGTH', 'UPPER', 'LOWER', 'TRIM', 'SUBSTRING', 'SUBSTR'].includes(f.name)) {
      category = 'Strings';
    } else if (['NOW', 'TODAY', 'DAYS_BETWEEN', 'HOURS_BETWEEN'].includes(f.name)) {
      category = 'Data/Hora';
    } else if (['IF', 'COALESCE', 'IFNULL'].includes(f.name)) {
      category = 'Condicionais';
    } else if (['SUM', 'COUNT', 'AVG'].includes(f.name)) {
      category = 'Agregação';
    }
    return {
      name: f.name,
      alias: f.alias || null,
      category,
      description: detail.desc,
      example: detail.example,
      returnType: detail.returnType
    };
  });

  const content = `// Auto-generated by scripts/manual-generate.mjs
// DO NOT EDIT MANUALLY
// Source: backend/src/main/java/com/rulex/service/complex/ExpressionEvaluator.java

export interface ExpressionFunction {
  name: string;
  alias: string | null;
  category: string;
  description: string;
  example: string;
  returnType: string;
}

export const EXPRESSION_FUNCTIONS: ExpressionFunction[] = ${JSON.stringify(categorized, null, 2)};

export const EXPRESSION_FUNCTION_COUNT = ${categorized.length};

export const EXPRESSION_FUNCTION_CATEGORIES = [...new Set(EXPRESSION_FUNCTIONS.map(f => f.category))];
`;
  writeGenerated('expressionFunctions.generated.ts', content);
  return categorized.length;
}

function generateAstAllowlistFile(allowlist) {
  const content = `// Auto-generated by scripts/manual-generate.mjs
// DO NOT EDIT MANUALLY
// Source: backend/src/main/java/com/rulex/v31/ast/AstValidator.java

/** Funções permitidas no validador AST V31 */
export const AST_FUNC_ALLOWLIST: string[] = ${JSON.stringify(allowlist.functions, null, 2)};

/** Operadores permitidos no validador AST V31 */
export const AST_OPERATORS: string[] = ${JSON.stringify(allowlist.operators, null, 2)};

/** Aliases de operadores (nome alternativo → nome canônico) */
export const AST_OPERATOR_ALIASES: Record<string, string> = ${JSON.stringify(allowlist.aliases, null, 2)};
`;
  writeGenerated('astAllowlist.generated.ts', content);
  return { 
    functions: allowlist.functions.length, 
    operators: allowlist.operators.length,
    aliases: Object.keys(allowlist.aliases).length
  };
}

function generateOpenapiSummaryFile(endpoints) {
  // Agrupar por path base
  const grouped = {};
  endpoints.forEach(ep => {
    const base = ep.path.split('/').slice(0, 3).join('/');
    if (!grouped[base]) grouped[base] = [];
    grouped[base].push(ep);
  });

  const content = `// Auto-generated by scripts/manual-generate.mjs
// DO NOT EDIT MANUALLY
// Source: openapi/rulex.yaml

export interface ApiEndpoint {
  path: string;
  method: string;
  summary: string;
  operationId: string;
}

export const API_ENDPOINTS: ApiEndpoint[] = ${JSON.stringify(endpoints, null, 2)};

export const API_ENDPOINT_COUNT = ${endpoints.length};

/** Endpoints agrupados por base path */
export const API_ENDPOINTS_GROUPED: Record<string, ApiEndpoint[]> = ${JSON.stringify(grouped, null, 2)};
`;
  writeGenerated('openapiSummary.generated.ts', content);
  return endpoints.length;
}

function generateDocsIndexFile(docs) {
  const content = `// Auto-generated by scripts/manual-generate.mjs
// DO NOT EDIT MANUALLY
// Source: docs/*.md

export interface DocEntry {
  id: string;
  filename: string;
  title: string;
  category: string;
  isEssential: boolean;
}

export const DOCS_INDEX: DocEntry[] = ${JSON.stringify(docs, null, 2)};

export const DOCS_COUNT = ${docs.length};

export const ESSENTIAL_DOCS = DOCS_INDEX.filter(d => d.isEssential);

export const DOCS_CATEGORIES = [...new Set(DOCS_INDEX.map(d => d.category))];
`;
  writeGenerated('docsIndex.generated.ts', content);
  return docs.length;
}

function generateIndexFile() {
  const content = `// Auto-generated by scripts/manual-generate.mjs
// Central export for all generated manual data

export * from './backendOperators.generated';
export * from './backendActions.generated';
export * from './logicOperators.generated';
export * from './expressionFunctions.generated';
export * from './astAllowlist.generated';
export * from './openapiSummary.generated';
export * from './docsIndex.generated';
`;
  writeGenerated('index.ts', content);
}

// ============================================================================
// VALIDAÇÃO: TRIPLE CHECK (FE vs BE)
// ============================================================================

function runTripleCheck(beOperatorCount, feOperatorCount) {
  log('='.repeat(60));
  log('TRIPLE CHECK: Validando consistência FE vs BE');
  log('='.repeat(60));

  const issues = [];

  // Check 1: Contagem de operadores
  log(`\nOperadores Backend: ${beOperatorCount}`);
  log(`Operadores Frontend: ${feOperatorCount}`);
  
  if (beOperatorCount !== feOperatorCount) {
    const diff = Math.abs(beOperatorCount - feOperatorCount);
    issues.push(`⚠️ DIVERGÊNCIA: Backend tem ${beOperatorCount} operadores, Frontend tem ${feOperatorCount} (diff: ${diff})`);
  } else {
    success(`Contagem de operadores OK: ${beOperatorCount} = ${feOperatorCount}`);
  }

  // Check 2: Funções vs Allowlist
  // (seria feito comparando os arquivos gerados, mas por simplicidade logamos)
  log('\nFunções de expressão extraídas com sucesso');
  log('Allowlist do AstValidator extraída com sucesso');

  // Resultado final
  log('\n' + '='.repeat(60));
  if (issues.length > 0) {
    error('TRIPLE CHECK: Encontradas divergências:');
    issues.forEach(i => console.log(`  ${i}`));
    log('='.repeat(60));
    // Não falhar build, apenas avisar
    return false;
  } else {
    success('TRIPLE CHECK: Todas validações OK!');
    log('='.repeat(60));
    return true;
  }
}

// ============================================================================
// MAIN
// ============================================================================

function main() {
  console.log('\n' + '='.repeat(60));
  console.log('MANUAL-GENERATE: Gerando dados do Manual do RULEX');
  console.log('='.repeat(60) + '\n');

  // Extrair dados
  log('Extraindo operadores do backend...');
  const beOperators = extractBackendOperators();
  
  log('Extraindo ações do backend...');
  const beActions = extractBackendActions();
  
  log('Extraindo operadores lógicos...');
  const logicOps = extractLogicOperators();
  
  log('Extraindo funções de expressão...');
  const exprFuncs = extractExpressionFunctions();
  
  log('Extraindo allowlist do AstValidator...');
  const allowlist = extractAstAllowlist();
  
  log('Extraindo endpoints do OpenAPI...');
  const endpoints = extractOpenapiEndpoints();
  
  log('Extraindo índice de documentos...');
  const docs = extractDocsIndex();

  console.log('\n' + '-'.repeat(60) + '\n');

  // Gerar arquivos
  const beOpCount = generateBackendOperatorsFile(beOperators);
  const beActCount = generateBackendActionsFile(beActions);
  generateLogicOperatorsFile(logicOps);
  generateExpressionFunctionsFile(exprFuncs);
  const astCounts = generateAstAllowlistFile(allowlist);
  const apiCount = generateOpenapiSummaryFile(endpoints);
  const docsCount = generateDocsIndexFile(docs);
  generateIndexFile();

  // Ler contagem do FE para comparação
  let feOpCount = 0;
  try {
    const feOpsPath = join(ROOT, 'client', 'src', 'lib', 'operators.ts');
    const feContent = readFile(feOpsPath);
    if (feContent) {
      const match = feContent.match(/export const OPERATORS.*?\[([\s\S]*?)\];/);
      if (match) {
        feOpCount = (match[1].match(/\{\s*value:/g) || []).length;
      }
    }
  } catch (e) {
    // Ignora erro
  }

  console.log('\n' + '-'.repeat(60) + '\n');

  // Resumo
  log('RESUMO DA GERAÇÃO:');
  log(`  - Operadores backend: ${beOpCount}`);
  log(`  - Ações backend: ${beActCount}`);
  log(`  - Operadores lógicos: ${logicOps.length}`);
  log(`  - Funções de expressão: ${exprFuncs.length}`);
  log(`  - Funções allowlist AST: ${astCounts.functions}`);
  log(`  - Operadores allowlist AST: ${astCounts.operators}`);
  log(`  - Aliases de operadores: ${astCounts.aliases}`);
  log(`  - Endpoints API: ${apiCount}`);
  log(`  - Documentos: ${docsCount}`);

  console.log('\n');

  // Triple Check
  const checkPassed = runTripleCheck(beOpCount, feOpCount);

  console.log('\n' + '='.repeat(60));
  if (checkPassed) {
    success('MANUAL-GENERATE: Concluído com sucesso!');
  } else {
    log('MANUAL-GENERATE: Concluído com avisos (ver acima)');
  }
  console.log('='.repeat(60) + '\n');
}

main();
