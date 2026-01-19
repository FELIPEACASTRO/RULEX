#!/usr/bin/env node
/**
 * ULTRA20 - check-generated-artifacts.mjs
 * 
 * Verifica scripts de geração e seus artefatos.
 * Detecta dependências de geradores em arquivos candidatos.
 * 
 * Saída: Relatório de geradores e dependências
 */

import { readFileSync, existsSync } from 'fs';
import { join } from 'path';
import { execSync } from 'child_process';

const ROOT_DIR = process.cwd();

// Detectar scripts de geração no package.json
function detectGeneratorScripts() {
  const pkgPath = join(ROOT_DIR, 'package.json');
  
  if (!existsSync(pkgPath)) {
    return [];
  }
  
  try {
    const pkg = JSON.parse(readFileSync(pkgPath, 'utf-8'));
    const scripts = pkg.scripts || {};
    
    // Padrões que indicam scripts de geração
    const generatorPatterns = [
      /generate/i,
      /gen$/i,
      /build:.*gen/i,
      /sync/i,
      /codegen/i,
      /openapi/i,
      /manual/i,
    ];
    
    const generators = [];
    for (const [name, cmd] of Object.entries(scripts)) {
      if (generatorPatterns.some(p => p.test(name))) {
        generators.push({ name, command: cmd });
      }
    }
    
    return generators;
  } catch (e) {
    console.error('Erro ao ler package.json:', e.message);
    return [];
  }
}

// Analisar dependências de um script
function analyzeScriptDependencies(scriptCmd) {
  const deps = [];
  
  // Padrões de leitura de arquivos
  const patterns = [
    /['"]([^'"]+\.(?:json|yaml|yml|md|txt))['"]/g,
    /--input[=\s]+['"]?([^\s'"]+)['"]?/g,
    /--config[=\s]+['"]?([^\s'"]+)['"]?/g,
    /-i[=\s]+['"]?([^\s'"]+)['"]?/g,
  ];
  
  for (const pattern of patterns) {
    pattern.lastIndex = 0;
    let match;
    while ((match = pattern.exec(scriptCmd)) !== null) {
      deps.push(match[1]);
    }
  }
  
  return deps;
}

// Verificar se candidatos são dependências de geradores
function checkCandidatesInGenerators(generators, candidates) {
  const results = {};
  
  for (const candidate of candidates) {
    const matches = [];
    
    for (const gen of generators) {
      // Verificar no comando
      if (gen.command.includes(candidate)) {
        matches.push({
          generator: gen.name,
          type: 'command',
          evidence: gen.command
        });
      }
      
      // Verificar nas dependências analisadas
      const deps = analyzeScriptDependencies(gen.command);
      for (const dep of deps) {
        if (dep.includes(candidate)) {
          matches.push({
            generator: gen.name,
            type: 'dependency',
            evidence: dep
          });
        }
      }
    }
    
    results[candidate] = {
      usedByGenerator: matches.length > 0,
      matches: matches
    };
  }
  
  return results;
}

// Tentar rodar gerador em dry-run (apenas verificar se funciona)
function testGenerator(scriptName) {
  try {
    // Apenas verificar se o comando existe, não executar
    const result = execSync(`pnpm run ${scriptName} --help 2>&1 || true`, {
      cwd: ROOT_DIR,
      timeout: 5000,
      encoding: 'utf-8'
    });
    
    return {
      name: scriptName,
      testable: true,
      output: result.substring(0, 200)
    };
  } catch (e) {
    return {
      name: scriptName,
      testable: false,
      error: e.message
    };
  }
}

function main() {
  console.log('=== ULTRA20: Verificador de Artefatos Gerados ===\n');
  
  // Detectar geradores
  const generators = detectGeneratorScripts();
  console.log(`Scripts de geração encontrados: ${generators.length}`);
  
  for (const gen of generators) {
    console.log(`  - ${gen.name}: ${gen.command.substring(0, 60)}...`);
  }
  
  // Candidatos padrão
  const defaultCandidates = [
    'audit/',
    'reports/',
    '.serena/',
    'arq/',
    'artifacts/',
    'docs/'
  ];
  
  const candidatesArg = process.argv.find(a => a.startsWith('--candidates='));
  const candidates = candidatesArg 
    ? candidatesArg.split('=')[1].split(',')
    : defaultCandidates;
  
  // Verificar dependências
  const results = checkCandidatesInGenerators(generators, candidates);
  
  // Output
  console.log('\n=== RESULTADO ===');
  
  let hasGeneratorDeps = false;
  for (const [candidate, data] of Object.entries(results)) {
    if (data.usedByGenerator) {
      hasGeneratorDeps = true;
      console.log(`\n⚠️  ${candidate} - USADO por gerador:`);
      for (const m of data.matches) {
        console.log(`   Gerador: ${m.generator}`);
        console.log(`   Tipo: ${m.type}`);
        console.log(`   Evidência: ${m.evidence.substring(0, 80)}`);
      }
    } else {
      console.log(`✅ ${candidate} - Não usado por geradores`);
    }
  }
  
  // JSON output
  const output = {
    generatorsFound: generators,
    candidateResults: results,
    hasGeneratorDependencies: hasGeneratorDeps
  };
  
  console.log('\n--- JSON OUTPUT ---');
  console.log(JSON.stringify(output, null, 2));
  
  // Não falhar, apenas reportar
  process.exit(0);
}

main();
