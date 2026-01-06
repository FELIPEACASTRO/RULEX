/**
 * Domain Exceptions - RULEX Fraud Detection Engine.
 *
 * <p>Este pacote contém as exceções de domínio sem dependências de framework. Segue os princípios
 * da Clean Architecture:
 *
 * <ul>
 *   <li><b>DomainException</b> - Exceção base abstrata
 *   <li><b>TamperDetectedException</b> - Adulteração de payload detectada
 *   <li><b>ContractViolationException</b> - Payload malformado ou incompleto
 *   <li><b>TransactionProcessingException</b> - Erro durante processamento
 *   <li><b>ResourceNotFoundException</b> - Recurso não encontrado
 *   <li><b>BusinessRuleException</b> - Violação de regra de negócio
 * </ul>
 *
 * <p><b>Mapeamento para HTTP (via ControllerAdvice):</b>
 *
 * <ul>
 *   <li>TamperDetectedException → 403 Forbidden
 *   <li>ContractViolationException → 400 Bad Request
 *   <li>ResourceNotFoundException → 404 Not Found
 *   <li>BusinessRuleException → 422 Unprocessable Entity
 *   <li>TransactionProcessingException → 500 Internal Server Error
 * </ul>
 *
 * @see com.rulex.interfaces.rest.advice Para mapeamento de exceções → HTTP
 */
package com.rulex.domain.exception;
