#!/usr/bin/env node
/**
 * ULTRA20 - check-yaml-references.mjs
 * 
 * Detecta referências a paths em arquivos YAML/YML.
 * Usado para verificar se candidatos a deleção são referenciados em configs.
 * 
 * Saída: JSON com matches encontrados
 */

import { readFileSync, existsSync, readdirSync, statSync } from 'fs';
import { join, basename } from 'path';

const ROOT_DIR = process.cwd();

// Padrões de path a detectar
const PATH_PATTERNS = [
  /['"]\.\/[^'"]+['"]/g,
  /['"]\.\.\/[^'"]+['"]/g,
  /['"]docs\/[^'"]+['"]/g,
  /['"]reports\/[^'"]+['"]/g,
  /['"]audit\/[^'"]+['"]/g,
  /['"]scripts\/[^'"]+['"]/g,
  /['"]openapi\/[^'"]+['"]/g,
  /['"]backend\/[^'"]+['"]/g,
  /['"]client\/[^'"]+['"]/g,
  /['"]arq\/[^'"]+['"]/g,
  /['"]artifacts\/[^'"]+['"]/g,
  /['"]\.\serena\/[^'"]+['"]/g,
];

// Coletar arquivos YAML recursivamente
function collectYamlFiles(dir, files = [], maxDepth = 5, currentDepth = 0) {
  if (!existsSync(dir) || currentDepth > maxDepth) return files;
  
  try {
    const entries = readdirSync(dir);
    for (const entry of entries) {
      if (['node_modules', '.git', 'dist', 'build', 'target'].includes(entry)) continue;
      
      const fullPath = join(dir, entry);
      const stat = statSync(fullPath);
      
      if (stat.isDirectory()) {
        collectYamlFiles(fullPath, files, maxDepth, currentDepth + 1);
      } else if (entry.endsWith('.yml') || entry.endsWith('.yaml')) {
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
      for (const pattern of PATH_PATTERNS) {
        const matches = line.matchAll(pattern);
        for (const match of matches) {
          refs.push({
            file: filePath.replace(ROOT_DIR + '/', ''),
            line: idx + 1,
            match: match[0],
            excerpt: line.trim().substring(0, 100)
          });
        }
      }
    });
  } catch (e) {
    console.error(`Erro ao ler ${filePath}: ${e.message}`);
  }
  
  return refs;
}

// Verificar se candidatos são referenciados
function checkCandidates(allRefs, candidates) {
  const results = {};
  
  for (const candidate of candidates) {
    const candidateBasename = basename(candidate);
    const matches = allRefs.filter(ref => 
      ref.match.includes(candidate) || ref.match.includes(candidateBasename)
    );
    
    results[candidate] = {
      referenced: matches.length > 0,
      matches: matches
    };
  }
  
  return results;
}

function main() {
  console.log('=== ULTRA20: Verificador de Referências YAML ===\n');
  
  const yamlFiles = [];
  
  // Coletar de locais conhecidos
  collectYamlFiles(join(ROOT_DIR, '.github'), yamlFiles);
  collectYamlFiles(ROOT_DIR, yamlFiles, 1); // Apenas raiz
  
  // docker-compose
  const dcFiles = ['docker-compose.yml', 'docker-compose.yaml', 'docker-compose.override.yml'];
  for (const dc of dcFiles) {
    const dcPath = join(ROOT_DIR, dc);
    if (existsSync(dcPath) && !yamlFiles.includes(dcPath)) {
      yamlFiles.push(dcPath);
    }
  }
  
  console.log(`Arquivos YAML encontrados: ${yamlFiles.length}`);
  
  // Extrair todas as referências
  const allRefs = [];
  for (const yf of yamlFiles) {
    const refs = extractReferences(yf);
    allRefs.push(...refs);
  }
  
  console.log(`Total de referências a paths: ${allRefs.length}`);
  
  // Listar candidatos típicos (pode ser passado como argumento)
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
      console.log(`\n⚠️  ${candidate} - REFERENCIADO em YAML:`);
      for (const m of data.matches) {
        console.log(`   ${m.file}:${m.line} -> ${m.match}`);
      }
    } else {
      console.log(`✅ ${candidate} - Não referenciado em YAML`);
    }
  }
  
  // JSON output
  const output = {
    yamlFilesScanned: yamlFiles.length,
    totalReferences: allRefs.length,
    candidateResults: results,
    allReferences: allRefs
  };
  
  console.log('\n--- JSON OUTPUT ---');
  console.log(JSON.stringify(output, null, 2));
  
  process.exit(hasRefs ? 1 : 0);
}

main();
