package com.rulex.interfaces.rest;

import com.rulex.domain.exception.BusinessRuleException;
import com.rulex.domain.exception.ContractViolationException;
import com.rulex.domain.exception.DomainException;
import com.rulex.domain.exception.ResourceNotFoundException;
import com.rulex.domain.exception.TamperDetectedException;
import com.rulex.domain.exception.TransactionProcessingException;
import java.time.Instant;
import java.util.HashMap;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.annotation.Order;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

/**
 * Global exception handler for hexagonal architecture REST endpoints.
 *
 * <p>Handles domain exceptions and translates them to appropriate HTTP responses.
 */
@RestControllerAdvice(basePackages = "com.rulex.interfaces.rest")
@Order(1) // Higher priority than default handlers
@Slf4j
public class HexagonalExceptionHandler {

  /**
   * Handles tamper detection - security breach.
   *
   * @return 403 Forbidden
   */
  @ExceptionHandler(TamperDetectedException.class)
  public ResponseEntity<Map<String, Object>> handleTamperDetected(TamperDetectedException ex) {
    log.error("SECURITY: Tamper detected - {}", ex.getMessage());
    return buildErrorResponse(
        HttpStatus.FORBIDDEN, ex.getErrorCode(), "Violação de segurança detectada", ex);
  }

  /**
   * Handles contract violations - bad requests.
   *
   * @return 400 Bad Request
   */
  @ExceptionHandler(ContractViolationException.class)
  public ResponseEntity<Map<String, Object>> handleContractViolation(
      ContractViolationException ex) {
    log.warn("Contract violation: {} - violations: {}", ex.getMessage(), ex.getViolations());
    Map<String, Object> body = buildErrorBody(HttpStatus.BAD_REQUEST, ex.getErrorCode(), ex);
    body.put("violations", ex.getViolations());
    return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(body);
  }

  /**
   * Handles resource not found.
   *
   * @return 404 Not Found
   */
  @ExceptionHandler(ResourceNotFoundException.class)
  public ResponseEntity<Map<String, Object>> handleResourceNotFound(ResourceNotFoundException ex) {
    log.debug("Resource not found: {} - id: {}", ex.getResourceType(), ex.getResourceId());
    Map<String, Object> body = buildErrorBody(HttpStatus.NOT_FOUND, ex.getErrorCode(), ex);
    body.put("resourceType", ex.getResourceType());
    body.put("resourceId", ex.getResourceId());
    return ResponseEntity.status(HttpStatus.NOT_FOUND).body(body);
  }

  /**
   * Handles business rule exceptions.
   *
   * @return 422 Unprocessable Entity
   */
  @ExceptionHandler(BusinessRuleException.class)
  public ResponseEntity<Map<String, Object>> handleBusinessRule(BusinessRuleException ex) {
    log.warn("Business rule violation: {} - rule: {}", ex.getMessage(), ex.getRuleName());
    Map<String, Object> body =
        buildErrorBody(HttpStatus.UNPROCESSABLE_ENTITY, ex.getErrorCode(), ex);
    body.put("ruleName", ex.getRuleName());
    if (ex.getContext() != null) {
      body.put("context", ex.getContext());
    }
    return ResponseEntity.status(HttpStatus.UNPROCESSABLE_ENTITY).body(body);
  }

  /**
   * Handles transaction processing errors.
   *
   * @return 500 Internal Server Error
   */
  @ExceptionHandler(TransactionProcessingException.class)
  public ResponseEntity<Map<String, Object>> handleTransactionProcessing(
      TransactionProcessingException ex) {
    log.error(
        "Transaction processing error: {} - txId: {}",
        ex.getMessage(),
        ex.getTransactionId(),
        ex.getCause());
    Map<String, Object> body =
        buildErrorBody(HttpStatus.INTERNAL_SERVER_ERROR, ex.getErrorCode(), ex);
    body.put("transactionId", ex.getTransactionId());
    return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(body);
  }

  /**
   * Handles generic domain exceptions.
   *
   * @return 500 Internal Server Error
   */
  @ExceptionHandler(DomainException.class)
  public ResponseEntity<Map<String, Object>> handleDomainException(DomainException ex) {
    log.error("Domain exception: {}", ex.getMessage(), ex);
    return buildErrorResponse(
        HttpStatus.INTERNAL_SERVER_ERROR, ex.getErrorCode(), ex.getMessage(), ex);
  }

  /**
   * Handles validation errors from @Valid annotations.
   *
   * @return 400 Bad Request
   */
  @ExceptionHandler(MethodArgumentNotValidException.class)
  public ResponseEntity<Map<String, Object>> handleValidationErrors(
      MethodArgumentNotValidException ex) {
    Map<String, String> fieldErrors = new HashMap<>();
    ex.getBindingResult()
        .getFieldErrors()
        .forEach(error -> fieldErrors.put(error.getField(), error.getDefaultMessage()));

    log.warn("Validation failed: {}", fieldErrors);

    Map<String, Object> body = new HashMap<>();
    body.put("timestamp", Instant.now());
    body.put("status", HttpStatus.BAD_REQUEST.value());
    body.put("error", "Bad Request");
    body.put("code", "VALIDATION_ERROR");
    body.put("message", "Erro de validação nos campos");
    body.put("fieldErrors", fieldErrors);

    return ResponseEntity.badRequest().body(body);
  }

  private ResponseEntity<Map<String, Object>> buildErrorResponse(
      HttpStatus status, String code, String message, Exception ex) {
    return ResponseEntity.status(status).body(buildErrorBody(status, code, ex));
  }

  private Map<String, Object> buildErrorBody(HttpStatus status, String code, Exception ex) {
    Map<String, Object> body = new HashMap<>();
    body.put("timestamp", Instant.now());
    body.put("status", status.value());
    body.put("error", status.getReasonPhrase());
    body.put("code", code);
    body.put("message", ex.getMessage());
    return body;
  }
}
