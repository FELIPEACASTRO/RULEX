# Changelog

Todas as mudanças notáveis neste projeto serão documentadas neste arquivo.

O formato é baseado em [Keep a Changelog](https://keepachangelog.com/pt-BR/1.0.0/),
e este projeto adere ao [Semantic Versioning](https://semver.org/lang/pt-BR/).

## [Unreleased]

### Added
- `RulexProperties.java` - Configurações centralizadas substituindo magic numbers (QUAL-002)
- Rate limiting por usuário autenticado além de IP (SEC-005)
- Métricas de cache expostas via Actuator

### Changed
- CORS: Whitelist explícita de headers permitidos, removido wildcard (SEC-007)
- HikariCP: Pool reduzido de 100 para 30 conexões (PERF-001)
- SecurityConfig: Validação de senhas expandida para staging/homolog/uat (SEC-003)
- ComplexRuleEvaluator: Integrado com OperatorEvaluatorRegistry (ARCH-001)

### Fixed
- Credenciais default removidas do docker-compose.yml (SEC-001)
- Neo4j health check habilitado (INFRA-001)
- Avisos de segurança adicionados para localStorage no frontend (SEC-002)

### Security
- Todas as variáveis de ambiente de credenciais agora são obrigatórias
- Rate limiting melhorado para prevenir bypass via proxy/VPN
- Headers de segurança (CSP, HSTS, X-Frame-Options) já implementados

## [2.1.0] - 2026-01-19

### Added
- 496 operadores de fraude implementados
- Sistema de shadow mode para testes seguros de regras
- A/B testing para comparação de variantes de regras
- 31 OperatorEvaluators modulares
- DeviceFingerprintService com detecção de device farming
- BloomFilterService para lookups O(1)
- StatisticalAnalysisService com 15 operadores estatísticos
- TransactionEnrichmentFacade consolidando 8 serviços de enriquecimento

### Changed
- Migração para Spring Boot 3.5
- Atualização para Java 21 com Virtual Threads
- React 19 no frontend
- Prometheus alerts profissionais configurados

### Fixed
- RegexValidator com proteção ReDoS completa
- GlobalExceptionHandler com tratamento robusto de erros
- VelocityTransactionLogRepository com query agregada anti-N+1

## [2.0.0] - 2025-12-01

### Added
- Arquitetura de regras complexas com grupos aninhados
- Suporte a operadores lógicos: AND, OR, NOT, XOR, NAND, NOR
- Neo4j para análise de grafos de relacionamento
- Redis para cache de velocity
- OpenTelemetry para tracing distribuído

### Changed
- Refatoração completa do motor de regras
- Nova estrutura de DTOs para regras complexas

### Removed
- Suporte a regras simples legadas (migradas para formato complexo)

## [1.0.0] - 2025-06-01

### Added
- Versão inicial do RULEX
- Motor de regras básico
- API REST para gerenciamento de regras
- Dashboard de monitoramento
- Autenticação Basic Auth
- Integração com PostgreSQL
