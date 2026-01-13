# PROMPT ÚNICO (COMPLETO) — DEVIN
**Missão:** Auditoria forense extrema + geração de “PROMPT BACKUP” **sem gaps**, reconstruível **byte-a-byte**, com prova criptográfica, a partir do ZIP anexado: **`RULEX-cursor-rulex-project-review-1c58 (11).zip`**.

> **Idioma obrigatório:** Português (Brasil) em todas as saídas (comentários e relatórios).  
> **Regra central:** **anti-alucinação total** — nada pode ser deduzido ou inventado.

---

## 0) Contexto do anexo (use como orientação, mas valide no ZIP)
O ZIP contém um monorepo com, no mínimo, estas áreas (confirme no inventário real do ZIP):
- `backend/` (Java/Spring Boot)  
- `client/` (React + TypeScript/Vite)
- `e2e/` (Playwright)
- `openapi/` (ex.: `openapi/rulex.yaml`)
- `docs/` (inclui `PAYLOAD_DICTIONARY.md`, catálogos e guias)
- Infra: `docker-compose.yml`, `Dockerfile.web`, `backend/Dockerfile`
- Outros: exports YAML (ex.: `FRAUDE_REGRAS_DURAS_EXPORT.yaml`), `perf/` etc.

⚠️ **Atenção (inconsistência interna observada):** se houver arquivos em `audit/` (ex.: inventários `inventory_*.txt`) que listem caminhos **não existentes** no ZIP, trate-os como **possivelmente desatualizados**.  
✅ **Fonte de verdade** sempre será **o conteúdo real das entradas do ZIP** (não confie em listas antigas).

---

## 1) Definição de “BACKUP PERFEITO” (SEM GAPS)
Você deve produzir **dois backups verificáveis**:

### (A) FS-BACKUP (filesystem restaurado)
Reconstituir a árvore de arquivos do projeto (conteúdo de arquivos/dirs/symlinks quando suportado) **byte-a-byte**, com verificação de hashes.

### (B) ZIP-BACKUP (ZIP restaurado)
Reconstituir um **ZIP restaurado** com as **mesmas entradas**, preservando:
- **ordem das entries**
- **CRC32 por entry**
- **tamanhos comprimido/descomprimido**
- **método de compressão** quando possível  
Se algo não for reprodutível por limitações do ambiente, declarar **LIMITAÇÃO DO AMBIENTE** com evidência e manter prova por entry.

---

## 2) Regras anti-alucinação (obrigatórias)
- Proibido inventar conteúdo, caminhos, arquivos, trechos, comportamentos, “etc.”, “…” ou placeholders.
- **Fonte de verdade = ENTRADAS DO ZIP** (ler bytes diretamente do ZIP via biblioteca; não dependa da extração do SO como fonte primária).
- Toda afirmação importante deve ter evidência (caminho + linhas, quando aplicável).
- Se algo não existir no ZIP, declarar: **“NÃO EXISTE NO ZIP”** e mostrar evidência (busca no inventário).
- Se houver conflito entre inventários antigos e o ZIP real, o ZIP real vence; registrar a divergência.

---

## 3) Segurança do restore (path traversal, colisões e SO)
Ao restaurar arquivos, você deve **bloquear** caminhos perigosos:
- PATH absoluto (`/`, `C:\`, `\\server\share`)
- PATH com `..`, `~`, `:` (drive letter), ou caracteres inválidos
- garantir que o destino final esteja **sempre** dentro de `./_restored/rulex`

Além disso:
- Detectar colisões por case-insensitive (Windows/macOS) — ex.: `A.txt` e `a.txt` — e reportar.
- Considerar Unicode (NFC/NFD) e registrar observações quando detectável.
- Symlinks e permissões podem não ser reproduzíveis no Windows; registre “LIMITAÇÃO DO AMBIENTE” se necessário.

---

## 4) Etapas obrigatórias (NÃO pular nenhuma)

### ETAPA 0 — Localizar ZIP e hashear
1) Localize o arquivo `.zip` na workspace (nome contém “RULEX-cursor-rulex-project-review-1c58”).  
2) Calcule **ZIP_SHA256** do arquivo bruto.

### ETAPA 1 — Inventário do ZIP (source of truth real)
Gere `ZIP_MANIFEST.json` com **1 registro por entry do ZIP** (inclui duplicados), em ordem.

Para cada entry, registrar:
- `ENTRY_INDEX` (ordem no ZIP, iniciando em 0)
- `ZIP_PATH` (string exata como está no ZIP)
- `KIND`: `FILE` | `DIR` | `SYMLINK` | `UNKNOWN`
- `CRC32`
- `COMP_METHOD` (store/deflate/...)
- `COMPRESSED_BYTES`
- `UNCOMPRESSED_BYTES`
- `SHA256_UNCOMPRESSED` (hash dos bytes descomprimidos)
- `FLAGS/EXTRA` e `TIMESTAMPS` (se disponível)
- `NOTE` (unicode/case-collision/limitações)

Validações obrigatórias:
- detectar `ZIP_PATH` perigoso (path traversal) e marcar como **inválido para FS restore** (mas manter no ZIP backup).
- reportar entradas duplicadas (mesmo `ZIP_PATH`) **sem perder fidelidade** (usar `ENTRY_INDEX` para diferenciar).

### ETAPA 2 — Bundle robusto (sem depender de Markdown)
Crie um diretório `BACKUP_BUNDLE/` com:
- `BACKUP_BUNDLE/entries/<ENTRY_INDEX>.b64`  
  Conteúdo: **base64 RFC 4648 padrão**, com padding; pode conter quebras de linha.
- O `ZIP_MANIFEST.json` deve apontar para cada `<ENTRY_INDEX>.b64`.

Regras base64 (obrigatórias):
- base64 padrão RFC 4648
- decoder deve **ignorar whitespace**
- após decode, validar **tamanho**: bytes decodificados == `UNCOMPRESSED_BYTES`
- recalcular `SHA256_UNCOMPRESSED` do byte-stream decodificado e comparar

⚠️ Motivo: evita “reflow”/alteração em Markdown e evita linhas gigantes (NDJSON com linha única).

### ETAPA 3 — Extração controlada para análise (FS de trabalho)
1) Extraia as entries do ZIP usando leitura programática (não “unzip” cego do SO) para `./_work/rulex_zip`, com regras de segurança de PATH.
2) Gere `FS_MANIFEST.json` (do FS criado) contendo:
- `PATH` relativo
- `KIND` (FILE/DIR/SYMLINK quando suportado)
- `BYTES` e `SHA256` para FILE
- contagens por tipo
3) Comparar ZIP vs FS:
- entradas que não podem ser materializadas no FS (por segurança/limitação) devem ser reportadas (mas continuam no ZIP backup).

### ETAPA 4 — FS restore + prova (MODO A)
Crie `restore_and_verify_fs.py` que:
- lê `ZIP_MANIFEST.json` + `BACKUP_BUNDLE/entries/*`
- recria FS em `./_restored/rulex` com regras de segurança (anti path traversal)
- para cada FILE:
  - decodifica base64
  - escreve bytes
  - valida bytes e `SHA256_UNCOMPRESSED` (ou SHA256 do FS, equivalente)
- aplica permissões/exec quando possível; se não, registrar limitação.

Relatório final obrigatório:
- `TOTAL FILES OK: X/X`
- `TOTAL DIRS OK: Y/Y`
- `TOTAL SYMLINKS OK: Z/Z`
- `BYTES MATCH: YES/NO`
- `HASH MATCH: YES/NO`
- divergências completas (path + esperado + obtido)

### ETAPA 5 — ZIP repack + prova (MODO B)
Crie `repack_and_verify_zip.py` que:
- recria `restored.zip` a partir de `ZIP_MANIFEST.json` + `BACKUP_BUNDLE/entries/*`
- preserva a **ordem** por `ENTRY_INDEX`
- tenta preservar `COMP_METHOD` quando possível
- verifica:
  - lista de `ZIP_PATH` na mesma ordem
  - `CRC32`, `UNCOMPRESSED_BYTES`, `COMPRESSED_BYTES` (quando verificável), método
  - (se reprodutível) `ZIP_SHA256` do zip final; se não, explicar por que e manter prova por entry

### ETAPA 6 — PROMPT BACKUP “para qualquer IA” (humano, mas à prova de UI)
Gere `PROMPT_BACKUP_RULEX.md` contendo:
1) Instruções de restauração (como usar os scripts)
2) `ZIP_SHA256`
3) cópia de `ZIP_MANIFEST.json`
4) cópia de `FS_MANIFEST.json`
5) instrução explícita: **scripts devem consumir o `BACKUP_BUNDLE/`** (não confiar em formatação de Markdown para bytes)
6) (opcional, recomendado) um índice do bundle (quantos entries, ranges, etc.)

Se o bundle for grande:
- dividir `BACKUP_BUNDLE` em partes (`entries_part_01/`, `entries_part_02/`...)
- criar `INDEX_MASTER.json` com:
  - lista de partes
  - mapping `ENTRY_INDEX -> parte`
- scripts devem recusar execução se faltar qualquer entry.

---

## 5) Auditoria do projeto (análise “extrema” sem pular arquivos)
Após a extração controlada (FS de trabalho), você deve analisar **todos os arquivos de texto** do projeto, sem exceção:

Para cada arquivo de texto:
- **Propósito**: o que faz, entradas/saídas, regras de negócio, integrações.
- **Riscos/Gaps**: segurança, validação, performance, consistência, migrations, regras, testes, observabilidade.
- **Evidências**: sempre citar como `PATH:L1-Ln` (linhas iniciando em 1, do arquivo como está no FS de trabalho).
Para binários:
- não interpretar internamente; apenas registrar metadados e uso provável por contexto.

Organize o relatório por pastas:
- `backend/` (Java, Spring, segurança, DB, migrations, APIs, validações)
- `client/` (UI, estado, chamadas API, validação, build)
- `e2e/` (Playwright)
- `docs/` (coerência, completude, consistência com código)
- `openapi/` (contrato x implementação)
- infraestrutura (Docker, compose, envs, scripts)
- exports YAML/regras (consistência com o engine)

---

## 6) Entregáveis finais (ordem obrigatória)
1) **Resumo executivo** do projeto (stack, módulos, o que existe no ZIP)
2) `ZIP_MANIFEST.json` + resumo `ZIP_MANIFEST.md`
3) `FS_MANIFEST.json` + resumo
4) `BACKUP_BUNDLE/entries/*` (ou partes + `INDEX_MASTER.json`)
5) `restore_and_verify_fs.py` + relatório do restore
6) `repack_and_verify_zip.py` + relatório do repack
7) `PROMPT_BACKUP_RULEX.md` (instruções + manifestos)
8) **Relatório de auditoria** (por pastas, com evidências por linhas)

---

## 7) Critério de conclusão (sem gaps)
Você só pode encerrar a missão se:
- o inventário do ZIP (entries) estiver completo,
- o bundle cobrir **100%** das entries,
- o restore FS validar bytes/hashes **para todas as entries materializáveis**,
- o repack ZIP validar ordem + CRC32 + tamanhos (e SHA do ZIP se aplicável),
- todas as limitações forem explicitadas com evidência.

**COMECE AGORA.**
