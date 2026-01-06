/**
 * Domain Models - RULEX Fraud Detection Engine.
 *
 * <p>Este pacote contém os modelos de domínio puros (POJOs) sem dependências de framework. Segue os
 * princípios da Clean Architecture / Hexagonal Architecture:
 *
 * <ul>
 *   <li><b>Rule</b> - Regra de detecção de fraude
 *   <li><b>RuleCondition</b> - Condição individual de uma regra
 *   <li><b>Classification</b> - Classificação de risco (APPROVED, SUSPICIOUS, FRAUD)
 *   <li><b>TransactionData</b> - Dados da transação para análise
 *   <li><b>Decision</b> - Resultado da análise de fraude
 * </ul>
 *
 * <p><b>Regras:</b>
 *
 * <ul>
 *   <li>Nenhuma anotação JPA (@Entity, @Table, etc.)
 *   <li>Nenhuma anotação Spring (@Service, @Component, etc.)
 *   <li>Imutáveis por design (usar @Value e builders)
 *   <li>Dependem apenas de java.* e lombok
 * </ul>
 *
 * @see com.rulex.infrastructure.persistence Para mapeamento Entity↔Domain
 */
package com.rulex.domain.model;
