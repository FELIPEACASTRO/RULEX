#!/usr/bin/env node
/**
 * sync-operators.mjs
 * 
 * Script para sincronizar operadores do backend para o frontend.
 * Gera um novo arquivo operators.ts com todos os 496 operadores.
 */

import { readFileSync, writeFileSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const ROOT = join(__dirname, '..');

// Ler operadores do backend gerado
const backendPath = join(ROOT, 'client/src/manual/generated/backendOperators.generated.ts');
const backendContent = readFileSync(backendPath, 'utf-8');
const backendMatch = backendContent.match(/export const BACKEND_OPERATORS.*?=\s*(\[[\s\S]*?\]);/);
const backendOperators = JSON.parse(backendMatch[1].replace(/,\s*\]/g, ']'));

// Ler operadores do frontend atual
const frontendPath = join(ROOT, 'client/src/lib/operators.ts');
const frontendContent = readFileSync(frontendPath, 'utf-8');

// Extrair operadores existentes do frontend com suas definições
const existingOperators = new Map();
const operatorRegex = /\{\s*value:\s*['"]([^'"]+)['"]\s*,\s*label:\s*['"]([^'"]+)['"]\s*,\s*description:\s*['"]([^'"]+)['"]\s*,\s*requiresValue:\s*(true|false)\s*,\s*category:\s*['"]([^'"]+)['"]\s*\}/g;
let match;
while ((match = operatorRegex.exec(frontendContent)) !== null) {
  existingOperators.set(match[1], {
    value: match[1],
    label: match[2],
    description: match[3],
    requiresValue: match[4] === 'true',
    category: match[5]
  });
}

console.log(`Backend operators: ${backendOperators.length}`);
console.log(`Frontend operators (existing): ${existingOperators.size}`);

// Operadores que não requerem valor
const noValueOperators = new Set([
  'AND', 'OR', 'NOT', 'XOR', 'NAND', 'NOR',
  'IS_NULL', 'NOT_NULL', 'IS_TRUE', 'IS_FALSE',
  'IS_WEEKEND', 'IS_HOLIDAY'
]);

// Criar lista completa de operadores
const allOperators = backendOperators.map(op => {
  // Se já existe no frontend, usar a definição existente
  if (existingOperators.has(op.name)) {
    return existingOperators.get(op.name);
  }
  
  // Criar nova definição
  const label = op.name.split('_').map(w => w.charAt(0) + w.slice(1).toLowerCase()).join(' ');
  const description = op.comment || `Operador ${label}`;
  const requiresValue = !noValueOperators.has(op.name);
  
  return {
    value: op.name,
    label: label,
    description: description,
    requiresValue: requiresValue,
    category: op.category
  };
});

// Encontrar operadores faltantes
const missingOperators = backendOperators.filter(op => !existingOperators.has(op.name));
console.log(`Missing operators: ${missingOperators.length}`);
console.log('\nMissing operators:');
missingOperators.forEach(op => {
  console.log(`  - ${op.name} (${op.category})`);
});

// Gerar novo arquivo operators.ts
const newContent = `// Auto-generated from ConditionOperator.java - ${allOperators.length} operators
// DO NOT EDIT MANUALLY - Run scripts/sync-operators.mjs to update

export interface OperatorDefinition {
  value: string;
  label: string;
  description: string;
  requiresValue?: boolean;
  category?: string;
}

export const OPERATORS: OperatorDefinition[] = [
${allOperators.map(op => `  { value: '${op.value}', label: '${op.label}', description: '${op.description.replace(/'/g, "\\'")}', requiresValue: ${op.requiresValue}, category: '${op.category}' },`).join('\n')}
];

// Mapa para acesso rápido por valor
export const OPERATOR_MAP = new Map(OPERATORS.map(op => [op.value, op]));

// Categorias únicas
export const OPERATOR_CATEGORIES = [...new Set(OPERATORS.map(op => op.category).filter(Boolean))];

// Total de operadores
export const OPERATOR_COUNT = OPERATORS.length;
`;

// Salvar novo arquivo
writeFileSync(frontendPath, newContent);
console.log(`\n✅ Updated ${frontendPath} with ${allOperators.length} operators`);
