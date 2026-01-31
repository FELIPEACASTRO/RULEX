# GAPS_DA_SOLUCAO (FASE 0)

Este documento lista **gaps reais** (com evidência) e **pontos a validar** antes de avançar para FASE 1.

## GAP-001 — Endpoint export/import com path duplo `/api/api/v1/...`

**Descrição**
- O app define `server.servlet.context-path: /api`.
- O controller de export/import usa `@RequestMapping("/api/v1/rules/export-import")`.
- Isso tende a produzir endpoint final `/api/api/v1/rules/export-import/...`.

**Impacto**
- Confusão no frontend/clients e documentação.

**Ação sugerida (VALIDAR)**
- Decidir se o prefixo `/api` deve ser removido do controller (deixar `/v1/...`) **ou** se existe uma configuração que elimina o prefixo (NÃO ENCONTREI EVIDÊNCIA).

**EVIDÊNCIA**
- Context-path: [backend/src/main/resources/application.yml](../backend/src/main/resources/application.yml#L41-L44)
- Controller mapping: [backend/src/main/java/com/rulex/controller/RuleExportImportController.java](../backend/src/main/java/com/rulex/controller/RuleExportImportController.java#L1-L30)

## GAP-002 — Regex hardening não integrado ao operador MATCHES_REGEX

**Descrição**
- O engine usa `Pattern.compile(rawRegex)` diretamente em `MATCHES_REGEX`.
- Existe `RegexValidator` com timeout/denylist/limites anti-ReDoS.

**Impacto**
- Risco de ReDoS e/ou latência imprevisível com regex maliciosa.

**Ação sugerida**
- Integrar `RegexValidator.safeCompile` e `matchWithTimeout` no caminho do operador.

**EVIDÊNCIA**
- Engine regex atual: [backend/src/main/java/com/rulex/service/engine/RuleEngineConditionHelper.java](../backend/src/main/java/com/rulex/service/engine/RuleEngineConditionHelper.java#L98-L250)
- RegexValidator: [backend/src/main/java/com/rulex/util/RegexValidator.java](../backend/src/main/java/com/rulex/util/RegexValidator.java#L1-L120)

## GAP-003 — Shadow mode duplicado em schemas/entidades (tabela `rules` vs `rule_configurations`)

**Descrição**
- Migration V20 adiciona `shadow_mode` e `canary_percentage` à tabela `rules`.
- O runtime do engine (regras simples) usa `RuleConfiguration.getShadowMode()`.

**Impacto**
- Potencial inconsistência: alterações em `rules` podem não afetar o engine atual.

**Ação sugerida (VALIDAR)**
- Verificar onde a tabela `rules` ainda é usada.
- Se for legado, considerar documentação/remoção controlada.

**EVIDÊNCIA**
- Migration V20: [backend/src/main/resources/db/migration/V20__shadow_mode_and_device_fingerprinting.sql](../backend/src/main/resources/db/migration/V20__shadow_mode_and_device_fingerprinting.sql#L1-L20)
- Engine usa shadow em `rule_configurations`: [backend/src/main/java/com/rulex/core/engine/usecase/RuleEngineUseCase.java](../backend/src/main/java/com/rulex/core/engine/usecase/RuleEngineUseCase.java#L534-L635)

## GAP-004 — Catalog coverage limitada ao TransactionRequest (CRTRAN25)

**Descrição**
- O `FieldDictionarySeeder` deriva catálogo por reflexão do DTO `TransactionRequest` e fixa defaults de workflow/recordType/portfolio.

**Impacto**
- Se existirem outros recordTypes/workflows, o catálogo pode ficar incompleto.

**Ação sugerida (VALIDAR)**
- Confirmar se há outros DTOs/recordTypes.
- Se houver, expandir seeder para múltiplos contratos.

**EVIDÊNCIA**
- Seeder defaults e reflexão do DTO: [backend/src/main/java/com/rulex/v31/field/FieldDictionarySeeder.java](../backend/src/main/java/com/rulex/v31/field/FieldDictionarySeeder.java#L15-L120)

## GAP-005 — Endpoints públicos de análise vs modelo de segurança

**Descrição**
- Mesmo com segurança habilitada, os endpoints `POST /transactions/analyze`, `POST /transactions/analyze-advanced` e `POST /evaluate` são `permitAll()` e também ignorados pelo CSRF.

**Impacto**
- Pode ser intencional (demo/tests), mas em produção amplia superfície de ataque.

**Ação sugerida (VALIDAR)**
- Decidir se isso deve ser protegido em perfis não-dev.

**EVIDÊNCIA**
- Regras de security: [backend/src/main/java/com/rulex/config/SecurityConfig.java](../backend/src/main/java/com/rulex/config/SecurityConfig.java#L60-L120)

## GAP-006 — Campos antifraude essenciais ausentes/sem garantia no payload

**Descrição**
- Documentação de payload indica ausência de campos críticos para fraude bancária (deviceId, ipAddress, userAgent, sessionId, email, phone, geo).
- Sem esses dados, regras avançadas de device/IP/session e validação de contato ficam inviáveis.

**Impacto**
- Limita cobertura de fraudes modernas (ATO, bots, device-farming, proxy/VPN, account recovery fraud).

**Ação sugerida (ROADMAP)**
- Formalizar plano de enriquecimento de payload e integrações externas.
- Consolidar em um plano único de execução com fases e owners.

**EVIDÊNCIA**
- Gaps de payload: [docs/PAYLOAD_DICTIONARY.md](PAYLOAD_DICTIONARY.md#L227-L242)
- Capacidades bloqueadas por dados: [docs/02_CAPABILITIES_EXTRACTION.md](02_CAPABILITIES_EXTRACTION.md#L239-L260)
- Plano de campos derivados: [docs/PLANO_IMPLEMENTACAO_CAMPOS_DERIVADOS.md](PLANO_IMPLEMENTACAO_CAMPOS_DERIVADOS.md)
