#!/usr/bin/env node
/**
 * check-markdown-links.mjs
 *
 * Script para verificar links internos em arquivos Markdown.
 * Valida:
 * - Links para arquivos locais (relativos)
 * - Links para imagens locais
 * - Âncoras (headings) dentro de arquivos
 *
 * Ignora links externos (http://, https://, mailto:)
 *
 * Exit code:
 * - 0: Todos os links válidos
 * - 1: Links quebrados encontrados
 */

import { readFileSync, existsSync, readdirSync, statSync } from 'fs';
import { join, dirname, resolve, extname } from 'path';

// Configuração
const ROOT_DIR = process.cwd();
const DOCS_DIR = join(ROOT_DIR, 'docs');

// Coletar todos os arquivos .md
function collectMarkdownFiles(dir, files = []) {
  if (!existsSync(dir)) return files;

  const entries = readdirSync(dir);
  for (const entry of entries) {
    const fullPath = join(dir, entry);
    const stat = statSync(fullPath);

    if (stat.isDirectory()) {
      // Ignorar node_modules, .git, etc.
      if (!['node_modules', '.git', 'dist', 'build', 'target'].includes(entry)) {
        collectMarkdownFiles(fullPath, files);
      }
    } else if (entry.endsWith('.md')) {
      files.push(fullPath);
    }
  }
  return files;
}

// Converter heading para slug estilo GitHub
// GitHub mantém acentos nos slugs, apenas converte para lowercase e substitui espaços por hífens
function headingToSlug(heading) {
  return heading
    .toLowerCase()
    // Remove emojis e caracteres especiais Unicode (exceto letras acentuadas)
    .replace(/[\u{1F300}-\u{1F9FF}]/gu, '')  // Emojis
    .replace(/[\u{2600}-\u{26FF}]/gu, '')    // Símbolos misc
    .replace(/[\u{2700}-\u{27BF}]/gu, '')    // Dingbats
    // Remove pontuação mas mantém letras acentuadas e números
    .replace(/[^\p{L}\p{N}\s\-]/gu, '')
    .trim()
    .replace(/\s+/g, '-')
    .replace(/-+/g, '-')
    .replace(/^-|-$/g, '');  // Remove hífens no início/fim
}

// Extrair headings de um arquivo MD
function extractHeadings(content) {
  const headings = new Map(); // slug -> count (para duplicados)
  const lines = content.split('\n');

  for (const line of lines) {
    // Match headings: # Heading, ## Heading, etc.
    const match = line.match(/^#{1,6}\s+(.+)$/);
    if (match) {
      const headingText = match[1].trim();
      const slug = headingToSlug(headingText);

      if (slug) {
        const count = headings.get(slug) || 0;
        headings.set(slug, count + 1);

        // Para duplicados, adicionar sufixo -1, -2, etc.
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

  // Regex para links Markdown: [texto](caminho) e ![alt](caminho)
  const linkRegex = /!?\[([^\]]*)\]\(([^)]+)\)/g;
  let match;

  while ((match = linkRegex.exec(content)) !== null) {
    const [fullMatch, text, href] = match;
    const isImage = fullMatch.startsWith('!');

    // Ignorar links externos
    if (href.startsWith('http://') ||
        href.startsWith('https://') ||
        href.startsWith('mailto:') ||
        href.startsWith('tel:')) {
      continue;
    }

    links.push({
      text,
      href,
      isImage,
      position: match.index
    });
  }

  return links;
}

// Validar um link
function validateLink(link, sourceFile, headingsCache) {
  const { href } = link;
  const sourceDir = dirname(sourceFile);

  // Separar caminho e âncora
  const [pathPart, anchorPart] = href.split('#');

  // Se for apenas âncora (mesmo arquivo)
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

  // Resolver caminho relativo
  let targetPath;
  if (pathPart.startsWith('/')) {
    // Caminho absoluto a partir da raiz
    targetPath = join(ROOT_DIR, pathPart);
  } else {
    // Caminho relativo
    targetPath = resolve(sourceDir, pathPart);
  }

  // Verificar se arquivo existe
  if (!existsSync(targetPath)) {
    // Tentar com extensão .md se não tiver extensão
    if (!extname(targetPath) && existsSync(targetPath + '.md')) {
      targetPath = targetPath + '.md';
    } else {
      return {
        valid: false,
        reason: `Arquivo não encontrado: ${pathPart}`
      };
    }
  }

  // Se tiver âncora, validar
  if (anchorPart) {
    // Só validar âncoras em arquivos .md
    if (targetPath.endsWith('.md')) {
      let headings = headingsCache.get(targetPath);

      if (!headings) {
        const content = readFileSync(targetPath, 'utf-8');
        headings = extractHeadings(content);
        headingsCache.set(targetPath, headings);
      }

      if (!headings.has(anchorPart)) {
        return {
          valid: false,
          reason: `Âncora '#${anchorPart}' não encontrada em ${pathPart}`
        };
      }
    }
  }

  return { valid: true };
}

// Main
function main() {
  console.log('=== Verificador de Links Markdown ===\n');
  console.log(`Diretório raiz: ${ROOT_DIR}`);

  // Coletar arquivos MD
  const mdFiles = [];

  // Arquivos na raiz
  const rootFiles = readdirSync(ROOT_DIR)
    .filter(f => f.endsWith('.md'))
    .map(f => join(ROOT_DIR, f));
  mdFiles.push(...rootFiles);

  // Arquivos em docs/
  if (existsSync(DOCS_DIR)) {
    collectMarkdownFiles(DOCS_DIR, mdFiles);
  }

  // Arquivos em reports/ (podem ter referências)
  const reportsDir = join(ROOT_DIR, 'reports');
  if (existsSync(reportsDir)) {
    collectMarkdownFiles(reportsDir, mdFiles);
  }

  console.log(`\nArquivos MD encontrados: ${mdFiles.length}`);

  // Cache de headings
  const headingsCache = new Map();

  // Estatísticas
  let totalLinks = 0;
  const brokenLinks = [];

  // Processar cada arquivo
  for (const mdFile of mdFiles) {
    const relativePath = mdFile.replace(ROOT_DIR + '/', '');
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

    console.log(`\n❌ FALHA: ${brokenLinks.length} link(s) quebrado(s) encontrado(s)`);
    process.exit(1);
  } else {
    console.log(`\n✅ SUCESSO: Todos os links internos estão válidos`);
    process.exit(0);
  }
}

main();
