#!/usr/bin/env node
/**
 * ULTRA20 - check-vite-assets.mjs
 * 
 * Verifica integridade de assets referenciados no frontend.
 * Detecta imports de imagens/fonts/css e valida existência.
 * 
 * Saída: Lista de assets faltantes
 */

import { readFileSync, existsSync, readdirSync, statSync } from 'fs';
import { join, dirname, resolve, extname } from 'path';

const ROOT_DIR = process.cwd();
const CLIENT_DIR = join(ROOT_DIR, 'client');

// Extensões de assets
const ASSET_EXTENSIONS = ['.png', '.jpg', '.jpeg', '.gif', '.svg', '.webp', '.ico', '.woff', '.woff2', '.ttf', '.eot', '.css'];

// Padrões de import de assets
const ASSET_PATTERNS = [
  /import\s+.*from\s+['"]([^'"]+\.(?:png|jpg|jpeg|gif|svg|webp|ico|woff|woff2|ttf|eot|css))['"]/gi,
  /url\s*\(\s*['"]?([^'")]+\.(?:png|jpg|jpeg|gif|svg|webp|ico|woff|woff2|ttf|eot))['"]?\s*\)/gi,
  /src\s*=\s*['"]([^'"]+\.(?:png|jpg|jpeg|gif|svg|webp|ico))['"]/gi,
  /href\s*=\s*['"]([^'"]+\.(?:css|ico))['"]/gi,
];

// Coletar arquivos frontend
function collectFrontendFiles(dir, files = [], maxDepth = 10, currentDepth = 0) {
  if (!existsSync(dir) || currentDepth > maxDepth) return files;
  
  try {
    const entries = readdirSync(dir);
    for (const entry of entries) {
      if (['node_modules', '.git', 'dist', 'build'].includes(entry)) continue;
      
      const fullPath = join(dir, entry);
      const stat = statSync(fullPath);
      
      if (stat.isDirectory()) {
        collectFrontendFiles(fullPath, files, maxDepth, currentDepth + 1);
      } else {
        const ext = extname(entry);
        if (['.ts', '.tsx', '.js', '.jsx', '.css', '.scss', '.html'].includes(ext)) {
          files.push(fullPath);
        }
      }
    }
  } catch (e) {
    // Ignorar erros
  }
  
  return files;
}

// Extrair referências a assets
function extractAssetReferences(filePath) {
  const refs = [];
  
  try {
    const content = readFileSync(filePath, 'utf-8');
    
    for (const pattern of ASSET_PATTERNS) {
      pattern.lastIndex = 0;
      let match;
      while ((match = pattern.exec(content)) !== null) {
        const assetPath = match[1];
        
        // Ignorar URLs externas e data URIs
        if (assetPath.startsWith('http') || assetPath.startsWith('data:') || assetPath.startsWith('//')) {
          continue;
        }
        
        refs.push({
          sourceFile: filePath.replace(ROOT_DIR + '/', ''),
          assetPath: assetPath,
          match: match[0].substring(0, 80)
        });
      }
    }
  } catch (e) {
    // Ignorar erros
  }
  
  return refs;
}

// Verificar se asset existe
function validateAsset(ref) {
  const sourceDir = dirname(join(ROOT_DIR, ref.sourceFile));
  let assetPath = ref.assetPath;
  
  // Resolver path
  let fullPath;
  if (assetPath.startsWith('/')) {
    // Path absoluto do public
    fullPath = join(CLIENT_DIR, 'public', assetPath);
    if (!existsSync(fullPath)) {
      fullPath = join(CLIENT_DIR, assetPath);
    }
  } else if (assetPath.startsWith('@/')) {
    // Alias comum
    fullPath = join(CLIENT_DIR, 'src', assetPath.substring(2));
  } else {
    // Path relativo
    fullPath = resolve(sourceDir, assetPath);
  }
  
  return {
    ...ref,
    resolvedPath: fullPath.replace(ROOT_DIR + '/', ''),
    exists: existsSync(fullPath)
  };
}

function main() {
  console.log('=== ULTRA20: Verificador de Assets Vite ===\n');
  
  if (!existsSync(CLIENT_DIR)) {
    console.log('Diretório client/ não encontrado. Pulando verificação.');
    process.exit(0);
  }
  
  const frontendFiles = [];
  collectFrontendFiles(join(CLIENT_DIR, 'src'), frontendFiles);
  collectFrontendFiles(join(CLIENT_DIR, 'public'), frontendFiles);
  
  console.log(`Arquivos frontend encontrados: ${frontendFiles.length}`);
  
  // Extrair referências
  const allRefs = [];
  for (const ff of frontendFiles) {
    const refs = extractAssetReferences(ff);
    allRefs.push(...refs);
  }
  
  console.log(`Total de referências a assets: ${allRefs.length}`);
  
  // Validar cada asset
  const validated = allRefs.map(validateAsset);
  const missing = validated.filter(v => !v.exists);
  
  // Output
  console.log('\n=== RESULTADO ===');
  console.log(`Assets válidos: ${validated.length - missing.length}`);
  console.log(`Assets faltantes: ${missing.length}`);
  
  if (missing.length > 0) {
    console.log('\n⚠️  ASSETS FALTANTES:');
    for (const m of missing) {
      console.log(`\n  Arquivo: ${m.sourceFile}`);
      console.log(`  Asset: ${m.assetPath}`);
      console.log(`  Resolvido: ${m.resolvedPath}`);
    }
  }
  
  // JSON output
  const output = {
    frontendFilesScanned: frontendFiles.length,
    totalAssetRefs: allRefs.length,
    validAssets: validated.length - missing.length,
    missingAssets: missing
  };
  
  console.log('\n--- JSON OUTPUT ---');
  console.log(JSON.stringify(output, null, 2));
  
  process.exit(missing.length > 0 ? 1 : 0);
}

main();
