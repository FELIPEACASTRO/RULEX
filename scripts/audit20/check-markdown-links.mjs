#!/usr/bin/env node
/**
 * ULTRA20 - check-markdown-links.mjs
 * 
 * Verificador rigoroso de links Markdown internos.
 * - Valida links para arquivos locais
 * - Valida âncoras (headings) com slug GitHub-like
 * - Valida imagens locais
 * - Ignora links externos (http/https/mailto)
 * 
 * Exit codes:
 * - 0: Todos os links válidos
 * - 1: Links quebrados encontrados
 */

import { readFileSync, existsSync, readdirSync, statSync } from 'fs';
import { join, dirname, resolve, extname } from 'path';

const ROOT_DIR = process.cwd();
const VERBOSE = process.argv.includes('--verbose') || process.argv.includes('-v');

// Coletar arquivos MD recursivamente
function collectMarkdownFiles(dir, files = []) {
  if (!existsSync(dir)) return files;
  
  const entries = readdirSync(dir);
  for (const entry of entries) {
    const fullPath = join(dir, entry);
    
    // Ignorar diretórios problemáticos
    if (['node_modules', '.git', 'dist', 'build', 'target', '.mvn'].includes(entry)) {
      continue;
    }
    
    try {
      const stat = statSync(fullPath);
      if (stat.isDirectory()) {
        collectMarkdownFiles(fullPath, files);
      } else if (entry.endsWith('.md')) {
        files.push(fullPath);
      }
    } catch (e) {
      // Ignorar erros de permissão
    }
  }
  return files;
}

// Converter heading para slug estilo GitHub
function headingToSlug(heading) {
  return heading
    .toLowerCase()
    // Remove emojis
    .replace(/[\u{1F300}-\u{1F9FF}]/gu, '')
    .replace(/[\u{2600}-\u{26FF}]/gu, '')
    .replace(/[\u{2700}-\u{27BF}]/gu, '')
    .replace(/[\u{1F600}-\u{1F64F}]/gu, '')
    // Remove pontuação mas mantém letras acentuadas e números
    .replace(/[^\p{L}\p{N}\s\-]/gu, '')
    .trim()
    .replace(/\s+/g, '-')
    .replace(/-+/g, '-')
    .replace(/^-|-$/g, '');
}

// Extrair headings de um arquivo MD
function extractHeadings(content) {
  const headings = new Map();
  const lines = content.split('\n');
  
  for (const line of lines) {
    const match = line.match(/^#{1,6}\s+(.+)$/);
    if (match) {
      const headingText = match[1].trim();
      const slug = headingToSlug(headingText);
      
      if (slug) {
        const count = headings.get(slug) || 0;
        headings.set(slug, count + 1);
        if (count > 0) {
          headings.set(`${slug}-${count}`, 1);
        }
      }
    }
  }
  
  return headings;
}

// Extrair links de um arquivo MD
function extractLinks(content) {
  const links = [];
  const linkRegex = /!?\[([^\]]*)\]\(([^)]+)\)/g;
  let match;
  
  while ((match = linkRegex.exec(content)) !== null) {
    const [fullMatch, text, href] = match;
    const isImage = fullMatch.startsWith('!');
    
    // Ignorar links externos
    if (href.startsWith('http://') || 
        href.startsWith('https://') || 
        href.startsWith('mailto:') ||
        href.startsWith('tel:') ||
        href.startsWith('data:')) {
      continue;
    }
    
    links.push({ text, href, isImage, position: match.index });
  }
  
  return links;
}

// Validar um link
function validateLink(link, sourceFile, headingsCache) {
  const { href } = link;
  const sourceDir = dirname(sourceFile);
  
  const [pathPart, anchorPart] = href.split('#');
  
  // Link apenas com âncora (mesmo arquivo)
  if (!pathPart && anchorPart) {
    const sourceContent = readFileSync(sourceFile, 'utf-8');
    const headings = extractHeadings(sourceContent);
    
    if (!headings.has(anchorPart)) {
      return {
        valid: false,
        reason: `Âncora '#${anchorPart}' não encontrada no próprio arquivo`
      };
    }
    return { valid: true };
  }
  
  // Resolver caminho
  let targetPath;
  if (pathPart.startsWith('/')) {
    targetPath = join(ROOT_DIR, pathPart);
  } else {
    targetPath = resolve(sourceDir, pathPart);
  }
  
  // Verificar existência
  if (!existsSync(targetPath)) {
    if (!extname(targetPath) && existsSync(targetPath + '.md')) {
      targetPath = targetPath + '.md';
    } else {
      return {
        valid: false,
        reason: `Arquivo não encontrado: ${pathPart}`
      };
    }
  }
  
  // Validar âncora se existir
  if (anchorPart && targetPath.endsWith('.md')) {
    let headings = headingsCache.get(targetPath);
    
    if (!headings) {
      try {
        const content = readFileSync(targetPath, 'utf-8');
        headings = extractHeadings(content);
        headingsCache.set(targetPath, headings);
      } catch (e) {
        return { valid: false, reason: `Erro ao ler arquivo: ${e.message}` };
      }
    }
    
    if (!headings.has(anchorPart)) {
      return {
        valid: false,
        reason: `Âncora '#${anchorPart}' não encontrada em ${pathPart}`
      };
    }
  }
  
  return { valid: true };
}

// Main
function main() {
  console.log('=== ULTRA20: Verificador de Links Markdown ===\n');
  console.log(`Diretório raiz: ${ROOT_DIR}`);
  
  const mdFiles = [];
  
  // Arquivos na raiz
  try {
    const rootFiles = readdirSync(ROOT_DIR)
      .filter(f => f.endsWith('.md'))
      .map(f => join(ROOT_DIR, f));
    mdFiles.push(...rootFiles);
  } catch (e) {
    console.error('Erro ao ler raiz:', e.message);
  }
  
  // Arquivos em docs/
  collectMarkdownFiles(join(ROOT_DIR, 'docs'), mdFiles);
  
  // Arquivos em arq/ (se existir)
  collectMarkdownFiles(join(ROOT_DIR, 'arq'), mdFiles);
  
  // Arquivos em reports/
  collectMarkdownFiles(join(ROOT_DIR, 'reports'), mdFiles);
  
  // Arquivos em artifacts/
  collectMarkdownFiles(join(ROOT_DIR, 'artifacts'), mdFiles);
  
  console.log(`\nArquivos MD encontrados: ${mdFiles.length}`);
  
  const headingsCache = new Map();
  let totalLinks = 0;
  const brokenLinks = [];
  
  for (const mdFile of mdFiles) {
    const relativePath = mdFile.replace(ROOT_DIR + '/', '');
    
    try {
      const content = readFileSync(mdFile, 'utf-8');
      const links = extractLinks(content);
      
      for (const link of links) {
        totalLinks++;
        const result = validateLink(link, mdFile, headingsCache);
        
        if (!result.valid) {
          brokenLinks.push({
            source: relativePath,
            href: link.href,
            text: link.text,
            reason: result.reason
          });
        }
      }
    } catch (e) {
      console.error(`Erro ao processar ${relativePath}: ${e.message}`);
    }
  }
  
  // Relatório
  console.log(`\n=== RESULTADO ===`);
  console.log(`Total de links analisados: ${totalLinks}`);
  console.log(`Links quebrados: ${brokenLinks.length}`);
  
  if (brokenLinks.length > 0) {
    console.log(`\n=== LINKS QUEBRADOS ===`);
    for (const broken of brokenLinks) {
      console.log(`\n  Arquivo: ${broken.source}`);
      console.log(`  Link: ${broken.href}`);
      console.log(`  Texto: ${broken.text || '(imagem)'}`);
      console.log(`  Motivo: ${broken.reason}`);
    }
    
    console.log(`\n❌ FALHA: ${brokenLinks.length} link(s) quebrado(s)`);
    
    // Exportar JSON para análise
    const jsonOutput = JSON.stringify({ totalLinks, brokenLinks }, null, 2);
    console.log('\n--- JSON OUTPUT ---');
    console.log(jsonOutput);
    
    process.exit(1);
  } else {
    console.log(`\n✅ SUCESSO: Todos os links internos válidos`);
    process.exit(0);
  }
}

main();
