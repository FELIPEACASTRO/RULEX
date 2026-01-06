/**
 * Application Use Cases - RULEX Fraud Detection Engine.
 *
 * <p>Este pacote contém as implementações dos Use Cases que orquestram lógica de aplicação.
 *
 * <h2>Use Cases:</h2>
 *
 * <ul>
 *   <li><b>AnalyzeTransactionUseCaseImpl</b> - Análise de fraude em transações
 *   <li><b>ManageRulesUseCaseImpl</b> - CRUD de regras (TODO)
 * </ul>
 *
 * <h2>Responsabilidades:</h2>
 *
 * <ul>
 *   <li>Orquestrar Domain Services
 *   <li>Coordenar Ports (in/out)
 *   <li>Gerenciar transações (@Transactional)
 *   <li>Registrar métricas e auditoria
 * </ul>
 *
 * <h2>Dependências Permitidas:</h2>
 *
 * <ul>
 *   <li>✅ domain.model - Domain Models
 *   <li>✅ domain.service - Domain Services
 *   <li>✅ domain.exception - Domain Exceptions
 *   <li>✅ application.port.in - Ports de entrada
 *   <li>✅ application.port.out - Ports de saída
 *   <li>❌ entity - Entidades JPA
 *   <li>❌ repository - Repositórios Spring Data
 *   <li>❌ controller - Controllers REST
 * </ul>
 *
 * @see com.rulex.application.port.in Para contratos de entrada
 * @see com.rulex.application.port.out Para contratos de saída
 */
package com.rulex.application.usecase;
