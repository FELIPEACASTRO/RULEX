#!/usr/bin/env node
/**
 * ULTRA20 - check-code-references.mjs
 * 
 * Detecta referências a arquivos em código fonte.
 * Verifica imports, fs operations, classpath, etc.
 * 
 * Saída: JSON com matches encontrados
 */

import { readFileSync, existsSync, readdirSync, statSync } from 'fs';
import { join, basename, extname } from 'path';

const ROOT_DIR = process.cwd();

// Extensões de código a verificar
const CODE_EXTENSIONS = ['.java', '.ts', '.tsx', '.js', '.mjs', '.cjs', '.sh'];

// Padrões de referência a detectar
const REFERENCE_PATTERNS = [
  // JavaScript/TypeScript
  /import\s+.*from\s+['"]([^'"]+)['"]/g,
  /require\s*\(\s*['"]([^'"]+)['"]\s*\)/g,
  /import\s*\(\s*['"]([^'"]+)['"]\s*\)/g,
  /readFileSync\s*\(\s*['"]([^'"]+)['"]/g,
  /readFile\s*\(\s*['"]([^'"]+)['"]/g,
  /createReadStream\s*\(\s*['"]([^'"]+)['"]/g,
  /existsSync\s*\(\s*['"]([^'"]+)['"]/g,
  /readdirSync\s*\(\s*['"]([^'"]+)['"]/g,
  /glob\s*\(\s*['"]([^'"]+)['"]/g,
  
  // Java
  /classpath:\s*([^\s"']+)/g,
  /getResource\s*\(\s*['"]([^'"]+)['"]\s*\)/g,
  /getResourceAsStream\s*\(\s*['"]([^'"]+)['"]\s*\)/g,
  /ResourceLoader.*['"]([^'"]+)['"]/g,
  /@Value\s*\(\s*['"]([^'"]+)['"]\s*\)/g,
  
  // Paths genéricos em strings
  /['"]\.\/([^'"]+)['"]/g,
  /['"]\.\.\/([^'"]+)['"]/g,
  /['"]docs\/([^'"]+)['"]/g,
  /['"]reports\/([^'"]+)['"]/g,
  /['"]audit\/([^'"]+)['"]/g,
  /['"]arq\/([^'"]+)['"]/g,
  /['"]artifacts\/([^'"]+)['"]/g,
  /['"]\.serena\/([^'"]+)['"]/g,
];

// Coletar arquivos de código
function collectCodeFiles(dir, files = [], maxDepth = 10, currentDepth = 0) {
  if (!existsSync(dir) || currentDepth > maxDepth) return files;
  
  try {
    const entries = readdirSync(dir);
    for (const entry of entries) {
      if (['node_modules', '.git', 'dist', 'build', 'target', '.mvn'].includes(entry)) continue;
      
      const fullPath = join(dir, entry);
      const stat = statSync(fullPath);
      
      if (stat.isDirectory()) {
        collectCodeFiles(fullPath, files, maxDepth, currentDepth + 1);
      } else if (CODE_EXTENSIONS.includes(extname(entry))) {
        files.push(fullPath);
      }
    }
  } catch (e) {
    // Ignorar erros
  }
  
  return files;
}

// Extrair referências de um arquivo
function extractReferences(filePath) {
  const refs = [];
  
  try {
    const content = readFileSync(filePath, 'utf-8');
    const lines = content.split('\n');
    
    lines.forEach((line, idx) => {
      for (const pattern of REFERENCE_PATTERNS) {
        // Reset lastIndex para regex global
        pattern.lastIndex = 0;
        let match;
        while ((match = pattern.exec(line)) !== null) {
          refs.push({
            file: filePath.replace(ROOT_DIR + '/', ''),
            line: idx + 1,
            pattern: pattern.source.substring(0, 30) + '...',
            match: match[0],
            captured: match[1] || match[0],
            excerpt: line.trim().substring(0, 120)
          });
        }
      }
    });
  } catch (e) {
    // Ignorar erros de leitura
  }
  
  return refs;
}

// Verificar se candidatos são referenciados
function checkCandidates(allRefs, candidates) {
  const results = {};
  
  for (const candidate of candidates) {
    const candidateBasename = basename(candidate);
    const candidateNormalized = candidate.replace(/\/$/, '');
    
    const matches = allRefs.filter(ref => {
      const captured = ref.captured || '';
      const matchStr = ref.match || '';
      return captured.includes(candidateNormalized) || 
             captured.includes(candidateBasename) ||
             matchStr.includes(candidateNormalized) ||
             matchStr.includes(candidateBasename);
    });
    
    results[candidate] = {
      referenced: matches.length > 0,
      matchCount: matches.length,
      matches: matches.slice(0, 20) // Limitar para não explodir
    };
  }
  
  return results;
}

function main() {
  console.log('=== ULTRA20: Verificador de Referências em Código ===\n');
  
  const codeFiles = [];
  
  // Coletar de diretórios de código
  collectCodeFiles(join(ROOT_DIR, 'backend/src'), codeFiles);
  collectCodeFiles(join(ROOT_DIR, 'client/src'), codeFiles);
  collectCodeFiles(join(ROOT_DIR, 'scripts'), codeFiles);
  collectCodeFiles(join(ROOT_DIR, 'e2e'), codeFiles);
  
  console.log(`Arquivos de código encontrados: ${codeFiles.length}`);
  
  // Extrair todas as referências
  const allRefs = [];
  for (const cf of codeFiles) {
    const refs = extractReferences(cf);
    allRefs.push(...refs);
  }
  
  console.log(`Total de referências extraídas: ${allRefs.length}`);
  
  // Candidatos padrão
  const defaultCandidates = [
    'audit/',
    'reports/manual/',
    'reports/UNUSED_FILES/',
    'reports/ULTRA20/',
    '.serena/',
    'arq/',
    'artifacts/'
  ];
  
  const candidatesArg = process.argv.find(a => a.startsWith('--candidates='));
  const candidates = candidatesArg 
    ? candidatesArg.split('=')[1].split(',')
    : defaultCandidates;
  
  const results = checkCandidates(allRefs, candidates);
  
  // Output
  console.log('\n=== RESULTADO ===');
  
  let hasRefs = false;
  for (const [candidate, data] of Object.entries(results)) {
    if (data.referenced) {
      hasRefs = true;
      console.log(`\n⚠️  ${candidate} - REFERENCIADO em código (${data.matchCount} matches):`);
      for (const m of data.matches.slice(0, 5)) {
        console.log(`   ${m.file}:${m.line}`);
        console.log(`   -> ${m.excerpt.substring(0, 80)}...`);
      }
      if (data.matchCount > 5) {
        console.log(`   ... e mais ${data.matchCount - 5} matches`);
      }
    } else {
      console.log(`✅ ${candidate} - Não referenciado em código`);
    }
  }
  
  // JSON output
  const output = {
    codeFilesScanned: codeFiles.length,
    totalReferences: allRefs.length,
    candidateResults: results
  };
  
  console.log('\n--- JSON OUTPUT ---');
  console.log(JSON.stringify(output, null, 2));
  
  process.exit(hasRefs ? 1 : 0);
}

main();
