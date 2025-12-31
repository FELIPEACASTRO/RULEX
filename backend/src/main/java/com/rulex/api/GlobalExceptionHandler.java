package com.rulex.api;

import jakarta.servlet.http.HttpServletRequest;
import java.time.OffsetDateTime;
import java.time.format.DateTimeParseException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.validation.BindException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice
public class GlobalExceptionHandler {

  private static final Logger log = LoggerFactory.getLogger(GlobalExceptionHandler.class);

  @ExceptionHandler(IllegalArgumentException.class)
  public ResponseEntity<ApiErrorResponse> handleBadRequest(
      IllegalArgumentException ex, HttpServletRequest req) {
    log.warn("⚠️ Requisição inválida path={} msg={}", req.getRequestURI(), ex.getMessage());
    return build(HttpStatus.BAD_REQUEST, ex.getMessage(), req);
  }

  @ExceptionHandler(DateTimeParseException.class)
  public ResponseEntity<ApiErrorResponse> handleDateParse(
      DateTimeParseException ex, HttpServletRequest req) {
    log.warn("⚠️ Data/hora inválida path={} msg={}", req.getRequestURI(), ex.getMessage());
    return build(HttpStatus.BAD_REQUEST, "Formato de data/hora inválido", req);
  }

  @ExceptionHandler(NotFoundException.class)
  public ResponseEntity<ApiErrorResponse> handleNotFound(
      NotFoundException ex, HttpServletRequest req) {
    log.warn("⚠️ Recurso não encontrado path={} msg={}", req.getRequestURI(), ex.getMessage());
    return build(HttpStatus.NOT_FOUND, ex.getMessage(), req);
  }

  @ExceptionHandler({MethodArgumentNotValidException.class, BindException.class})
  public ResponseEntity<ApiErrorResponse> handleValidation(Exception ex, HttpServletRequest req) {
    log.warn("⚠️ Validação falhou path={}", req.getRequestURI());
    return build(HttpStatus.BAD_REQUEST, "Falha de validação", req);
  }

  @ExceptionHandler(IllegalStateException.class)
  public ResponseEntity<ApiErrorResponse> handleConflict(
      IllegalStateException ex, HttpServletRequest req) {
    log.warn("⚠️ Estado inválido path={} msg={}", req.getRequestURI(), ex.getMessage());
    return build(HttpStatus.CONFLICT, ex.getMessage(), req);
  }

  @ExceptionHandler(ObjectOptimisticLockingFailureException.class)
  public ResponseEntity<ApiErrorResponse> handleOptimisticLock(
      ObjectOptimisticLockingFailureException ex, HttpServletRequest req) {
    log.warn("⚠️ Conflito de versão path={} msg={}", req.getRequestURI(), ex.getMessage());
    return build(
        HttpStatus.CONFLICT,
        "Este registro foi modificado por outro usuário. Recarregue e tente novamente.",
        req);
  }

  @ExceptionHandler(Exception.class)
  public ResponseEntity<ApiErrorResponse> handleGeneric(Exception ex, HttpServletRequest req) {
    log.error("🚫 Erro inesperado path={}", req.getRequestURI(), ex);
    return build(HttpStatus.INTERNAL_SERVER_ERROR, "Erro interno", req);
  }

  private ResponseEntity<ApiErrorResponse> build(
      HttpStatus status, String message, HttpServletRequest req) {
    ApiErrorResponse body =
        new ApiErrorResponse(
            OffsetDateTime.now(),
            status.value(),
            status.getReasonPhrase(),
            message,
            req.getRequestURI());
    return ResponseEntity.status(status).body(body);
  }
}
