/**
 * Application Ports (OUT) - RULEX Fraud Detection Engine.
 *
 * <p>Este pacote contém as interfaces (Ports) que definem como a camada de aplicação acessa
 * infraestrutura externa. Implementações são fornecidas na camada infrastructure.
 *
 * <h2>Ports Disponíveis:</h2>
 *
 * <ul>
 *   <li><b>RulePersistencePort</b> - CRUD de regras de fraude
 *   <li><b>TransactionPersistencePort</b> - CRUD de transações
 *   <li><b>DecisionPersistencePort</b> - CRUD de decisões
 *   <li><b>RuleCachePort</b> - Cache de regras (Redis/Caffeine)
 *   <li><b>AuditEventPort</b> - Registro de eventos de auditoria
 *   <li><b>MetricsPort</b> - Métricas de performance
 * </ul>
 *
 * <h2>Padrão Hexagonal:</h2>
 *
 * <pre>
 *     ┌─────────────────────────────────────────┐
 *     │           Application Layer             │
 *     │                                         │
 *     │   ┌───────────┐     ┌───────────────┐  │
 *     │   │ Use Cases │────▶│ Ports (OUT)   │  │
 *     │   └───────────┘     └───────┬───────┘  │
 *     └───────────────────────────────┼────────┘
 *                                     │
 *     ┌───────────────────────────────▼────────┐
 *     │         Infrastructure Layer           │
 *     │                                        │
 *     │   ┌────────────────────────────────┐   │
 *     │   │  Adapters (implementações)     │   │
 *     │   │  - JPA Repository              │   │
 *     │   │  - Redis Cache                 │   │
 *     │   │  - Kafka Producer              │   │
 *     │   └────────────────────────────────┘   │
 *     └────────────────────────────────────────┘
 * </pre>
 *
 * <p><b>Regras:</b>
 *
 * <ul>
 *   <li>Interfaces NEVER dependem de frameworks
 *   <li>Usam apenas Domain Models
 *   <li>Implementações ficam em infrastructure/
 * </ul>
 *
 * @see com.rulex.infrastructure.persistence Para implementações JPA
 * @see com.rulex.infrastructure.cache Para implementações de cache
 */
package com.rulex.application.port.out;
