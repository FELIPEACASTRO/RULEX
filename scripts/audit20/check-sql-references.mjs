#!/usr/bin/env node
/**
 * ULTRA20 - check-sql-references.mjs
 * 
 * Detecta referências a arquivos externos em SQL.
 * Verifica COPY, \i, LOAD, paths em strings.
 * 
 * Saída: JSON com matches encontrados
 */

import { readFileSync, existsSync, readdirSync, statSync } from 'fs';
import { join, basename } from 'path';

const ROOT_DIR = process.cwd();

// Padrões SQL que podem referenciar arquivos
const SQL_PATTERNS = [
  /COPY\s+.*FROM\s+['"]([^'"]+)['"]/gi,
  /\\i\s+['"]?([^\s'"]+)['"]?/g,
  /\\include\s+['"]?([^\s'"]+)['"]?/g,
  /LOAD\s+DATA\s+.*['"]([^'"]+)['"]/gi,
  /SOURCE\s+['"]?([^\s'"]+)['"]?/gi,
  /['"]\.\/([^'"]+\.(?:sql|csv|json))['"]/g,
  /['"]\.\.\/([^'"]+\.(?:sql|csv|json))['"]/g,
];

// Coletar arquivos SQL
function collectSqlFiles(dir, files = [], maxDepth = 10, currentDepth = 0) {
  if (!existsSync(dir) || currentDepth > maxDepth) return files;
  
  try {
    const entries = readdirSync(dir);
    for (const entry of entries) {
      if (['node_modules', '.git', 'dist', 'build', 'target'].includes(entry)) continue;
      
      const fullPath = join(dir, entry);
      const stat = statSync(fullPath);
      
      if (stat.isDirectory()) {
        collectSqlFiles(fullPath, files, maxDepth, currentDepth + 1);
      } else if (entry.endsWith('.sql')) {
        files.push(fullPath);
      }
    }
  } catch (e) {
    // Ignorar erros
  }
  
  return files;
}

// Extrair referências de um arquivo SQL
function extractReferences(filePath) {
  const refs = [];
  
  try {
    const content = readFileSync(filePath, 'utf-8');
    const lines = content.split('\n');
    
    lines.forEach((line, idx) => {
      for (const pattern of SQL_PATTERNS) {
        pattern.lastIndex = 0;
        let match;
        while ((match = pattern.exec(line)) !== null) {
          refs.push({
            file: filePath.replace(ROOT_DIR + '/', ''),
            line: idx + 1,
            match: match[0],
            captured: match[1] || match[0],
            excerpt: line.trim().substring(0, 100)
          });
        }
      }
    });
  } catch (e) {
    // Ignorar erros
  }
  
  return refs;
}

// Verificar candidatos
function checkCandidates(allRefs, candidates) {
  const results = {};
  
  for (const candidate of candidates) {
    const candidateBasename = basename(candidate);
    const candidateNormalized = candidate.replace(/\/$/, '');
    
    const matches = allRefs.filter(ref => {
      const captured = ref.captured || '';
      return captured.includes(candidateNormalized) || 
             captured.includes(candidateBasename);
    });
    
    results[candidate] = {
      referenced: matches.length > 0,
      matches: matches
    };
  }
  
  return results;
}

function main() {
  console.log('=== ULTRA20: Verificador de Referências SQL ===\n');
  
  const sqlFiles = [];
  
  // Coletar de locais conhecidos
  collectSqlFiles(join(ROOT_DIR, 'backend/src/main/resources'), sqlFiles);
  collectSqlFiles(join(ROOT_DIR, 'backend/src/test/resources'), sqlFiles);
  collectSqlFiles(ROOT_DIR, sqlFiles, 2);
  
  console.log(`Arquivos SQL encontrados: ${sqlFiles.length}`);
  
  // Extrair referências
  const allRefs = [];
  for (const sf of sqlFiles) {
    const refs = extractReferences(sf);
    allRefs.push(...refs);
  }
  
  console.log(`Total de referências a arquivos externos: ${allRefs.length}`);
  
  // Candidatos
  const defaultCandidates = [
    'audit/',
    'reports/',
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
      console.log(`\n⚠️  ${candidate} - REFERENCIADO em SQL:`);
      for (const m of data.matches) {
        console.log(`   ${m.file}:${m.line} -> ${m.match}`);
      }
    } else {
      console.log(`✅ ${candidate} - Não referenciado em SQL`);
    }
  }
  
  // JSON output
  const output = {
    sqlFilesScanned: sqlFiles.length,
    totalReferences: allRefs.length,
    candidateResults: results,
    allReferences: allRefs
  };
  
  console.log('\n--- JSON OUTPUT ---');
  console.log(JSON.stringify(output, null, 2));
  
  process.exit(hasRefs ? 1 : 0);
}

main();
