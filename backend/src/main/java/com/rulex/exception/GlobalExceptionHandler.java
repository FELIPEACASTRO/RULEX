package com.rulex.exception;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.ConstraintViolationException;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.web.HttpMediaTypeNotSupportedException;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;
import org.springframework.web.multipart.MaxUploadSizeExceededException;

/**
 * Global Exception Handler para tratamento centralizado de erros.
 *
 * <p>Fornece respostas de erro padronizadas e amigáveis para diferentes tipos de exceções.
 *
 * <p>FIX: BUG-001 (Payload >1MB sem erro claro)
 *
 * @version 2.0.0
 */
@RestControllerAdvice
@Slf4j
public class GlobalExceptionHandler {

  private static final long MAX_PAYLOAD_SIZE = 1_000_000; // 1 MB

  /**
   * Tratamento de payload muito grande.
   *
   * <p>FIX: BUG-001 - Retorna erro claro quando payload excede 1MB
   */
  @ExceptionHandler(MaxUploadSizeExceededException.class)
  public ResponseEntity<ErrorResponse> handleMaxUploadSizeExceeded(
      MaxUploadSizeExceededException ex, HttpServletRequest request) {
    log.warn("Payload muito grande: {}", ex.getMessage());

    ErrorResponse error =
        ErrorResponse.builder()
            .timestamp(LocalDateTime.now())
            .status(HttpStatus.PAYLOAD_TOO_LARGE.value())
            .error("Payload Too Large")
            .message(
                String.format(
                    "O tamanho do payload excede o limite permitido de %d bytes (%.2f MB). "
                        + "Por favor, reduza o tamanho da requisição.",
                    MAX_PAYLOAD_SIZE, MAX_PAYLOAD_SIZE / 1_000_000.0))
            .path(request.getRequestURI())
            .details(
                Map.of(
                    "maxSize",
                    MAX_PAYLOAD_SIZE,
                    "maxSizeMB",
                    String.format("%.2f MB", MAX_PAYLOAD_SIZE / 1_000_000.0),
                    "suggestion",
                    "Considere dividir a requisição em múltiplas chamadas menores"))
            .build();

    return ResponseEntity.status(HttpStatus.PAYLOAD_TOO_LARGE).body(error);
  }

  /** Tratamento de JSON inválido ou mal formatado. */
  @ExceptionHandler(HttpMessageNotReadableException.class)
  public ResponseEntity<ErrorResponse> handleHttpMessageNotReadable(
      HttpMessageNotReadableException ex, HttpServletRequest request) {
    log.warn("JSON inválido: {}", ex.getMessage());

    String message = "JSON inválido ou mal formatado";
    if (ex.getMessage().contains("Required request body is missing")) {
      message = "Body da requisição é obrigatório";
    } else if (ex.getMessage().contains("JSON parse error")) {
      message = "Erro ao fazer parse do JSON. Verifique a sintaxe.";
    }

    ErrorResponse error =
        ErrorResponse.builder()
            .timestamp(LocalDateTime.now())
            .status(HttpStatus.BAD_REQUEST.value())
            .error("Bad Request")
            .message(message)
            .path(request.getRequestURI())
            .details(
                Map.of(
                    "cause",
                    ex.getMostSpecificCause().getMessage(),
                    "suggestion",
                    "Valide o JSON usando um validador online"))
            .build();

    return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(error);
  }

  /** Tratamento de validação de campos (@Valid). */
  @ExceptionHandler(MethodArgumentNotValidException.class)
  public ResponseEntity<ErrorResponse> handleMethodArgumentNotValid(
      MethodArgumentNotValidException ex, HttpServletRequest request) {
    log.warn("Validação falhou: {}", ex.getMessage());

    Map<String, String> fieldErrors = new HashMap<>();
    ex.getBindingResult()
        .getFieldErrors()
        .forEach(error -> fieldErrors.put(error.getField(), error.getDefaultMessage()));

    ErrorResponse error =
        ErrorResponse.builder()
            .timestamp(LocalDateTime.now())
            .status(HttpStatus.BAD_REQUEST.value())
            .error("Validation Failed")
            .message("Um ou mais campos não passaram na validação")
            .path(request.getRequestURI())
            .details(Map.of("fieldErrors", fieldErrors))
            .build();

    return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(error);
  }

  /** Tratamento de constraint violation. */
  @ExceptionHandler(ConstraintViolationException.class)
  public ResponseEntity<ErrorResponse> handleConstraintViolation(
      ConstraintViolationException ex, HttpServletRequest request) {
    log.warn("Constraint violation: {}", ex.getMessage());

    Map<String, String> violations = new HashMap<>();
    ex.getConstraintViolations()
        .forEach(
            violation ->
                violations.put(violation.getPropertyPath().toString(), violation.getMessage()));

    ErrorResponse error =
        ErrorResponse.builder()
            .timestamp(LocalDateTime.now())
            .status(HttpStatus.BAD_REQUEST.value())
            .error("Constraint Violation")
            .message("Violação de constraints de validação")
            .path(request.getRequestURI())
            .details(Map.of("violations", violations))
            .build();

    return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(error);
  }

  /** Tratamento de tipo de argumento inválido. */
  @ExceptionHandler(MethodArgumentTypeMismatchException.class)
  public ResponseEntity<ErrorResponse> handleMethodArgumentTypeMismatch(
      MethodArgumentTypeMismatchException ex, HttpServletRequest request) {
    log.warn("Tipo de argumento inválido: {}", ex.getMessage());

    ErrorResponse error =
        ErrorResponse.builder()
            .timestamp(LocalDateTime.now())
            .status(HttpStatus.BAD_REQUEST.value())
            .error("Type Mismatch")
            .message(
                String.format(
                    "O parâmetro '%s' deve ser do tipo '%s'",
                    ex.getName(), ex.getRequiredType().getSimpleName()))
            .path(request.getRequestURI())
            .details(
                Map.of(
                    "parameter", ex.getName(),
                    "expectedType", ex.getRequiredType().getSimpleName(),
                    "providedValue", ex.getValue() != null ? ex.getValue().toString() : "null"))
            .build();

    return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(error);
  }

  /** Tratamento de Content-Type não suportado. */
  @ExceptionHandler(HttpMediaTypeNotSupportedException.class)
  public ResponseEntity<ErrorResponse> handleHttpMediaTypeNotSupported(
      HttpMediaTypeNotSupportedException ex, HttpServletRequest request) {
    log.warn("Content-Type não suportado: {}", ex.getMessage());

    ErrorResponse error =
        ErrorResponse.builder()
            .timestamp(LocalDateTime.now())
            .status(HttpStatus.UNSUPPORTED_MEDIA_TYPE.value())
            .error("Unsupported Media Type")
            .message(
                String.format(
                    "Content-Type '%s' não é suportado. Use 'application/json'",
                    ex.getContentType()))
            .path(request.getRequestURI())
            .details(
                Map.of(
                    "providedContentType",
                    ex.getContentType() != null ? ex.getContentType().toString() : "null",
                    "supportedContentTypes",
                    ex.getSupportedMediaTypes().toString()))
            .build();

    return ResponseEntity.status(HttpStatus.UNSUPPORTED_MEDIA_TYPE).body(error);
  }

  /** Tratamento de método HTTP não suportado. */
  @ExceptionHandler(HttpRequestMethodNotSupportedException.class)
  public ResponseEntity<ErrorResponse> handleHttpRequestMethodNotSupported(
      HttpRequestMethodNotSupportedException ex, HttpServletRequest request) {
    log.warn("Método HTTP não suportado: {}", ex.getMessage());

    ErrorResponse error =
        ErrorResponse.builder()
            .timestamp(LocalDateTime.now())
            .status(HttpStatus.METHOD_NOT_ALLOWED.value())
            .error("Method Not Allowed")
            .message(
                String.format(
                    "Método HTTP '%s' não é suportado para este endpoint", ex.getMethod()))
            .path(request.getRequestURI())
            .details(
                Map.of(
                    "providedMethod", ex.getMethod(),
                    "supportedMethods", ex.getSupportedHttpMethods().toString()))
            .build();

    return ResponseEntity.status(HttpStatus.METHOD_NOT_ALLOWED).body(error);
  }

  /** Tratamento de recurso não encontrado (404). */
  @ExceptionHandler(com.rulex.api.NotFoundException.class)
  public ResponseEntity<ErrorResponse> handleNotFoundException(
      com.rulex.api.NotFoundException ex, HttpServletRequest request) {
    log.warn("Recurso não encontrado: {}", ex.getMessage());

    ErrorResponse error =
        ErrorResponse.builder()
            .timestamp(LocalDateTime.now())
            .status(HttpStatus.NOT_FOUND.value())
            .error("Not Found")
            .message(ex.getMessage())
            .path(request.getRequestURI())
            .build();

    return ResponseEntity.status(HttpStatus.NOT_FOUND).body(error);
  }

  /** Tratamento de operador não suportado (501). */
  @ExceptionHandler(UnsupportedOperatorException.class)
  public ResponseEntity<ErrorResponse> handleUnsupportedOperator(
      UnsupportedOperatorException ex, HttpServletRequest request) {
    log.warn("Operador não suportado: {}", ex.getOperator());

    ErrorResponse error =
        ErrorResponse.builder()
            .timestamp(LocalDateTime.now())
            .status(HttpStatus.NOT_IMPLEMENTED.value())
            .error("Not Implemented")
            .message(ex.getMessage())
            .path(request.getRequestURI())
            .details(
                Map.of(
                    "operator", ex.getOperator().name(),
                    "status", ex.getStatus(),
                    "suggestion", "Consulte GET /api/operators/status para ver operadores disponíveis"))
            .build();

    return ResponseEntity.status(HttpStatus.NOT_IMPLEMENTED).body(error);
  }

  /** Tratamento de erro na avaliação de regra (422). */
  @ExceptionHandler(RuleEvaluationException.class)
  public ResponseEntity<ErrorResponse> handleRuleEvaluation(
      RuleEvaluationException ex, HttpServletRequest request) {
    log.error("Erro na avaliação de regra: {}", ex.getMessage(), ex);

    ErrorResponse error =
        ErrorResponse.builder()
            .timestamp(LocalDateTime.now())
            .status(HttpStatus.UNPROCESSABLE_ENTITY.value())
            .error("Rule Evaluation Error")
            .message(ex.getMessage())
            .path(request.getRequestURI())
            .details(
                Map.of(
                    "ruleId", ex.getRuleId() != null ? ex.getRuleId() : "unknown",
                    "ruleName", ex.getRuleName() != null ? ex.getRuleName() : "unknown"))
            .build();

    return ResponseEntity.status(HttpStatus.UNPROCESSABLE_ENTITY).body(error);
  }

  /** Tratamento de erro de conexão Neo4j (503). */
  @ExceptionHandler(Neo4jConnectionException.class)
  public ResponseEntity<ErrorResponse> handleNeo4jConnection(
      Neo4jConnectionException ex, HttpServletRequest request) {
    log.error("Erro de conexão Neo4j: {}", ex.getMessage());

    ErrorResponse error =
        ErrorResponse.builder()
            .timestamp(LocalDateTime.now())
            .status(HttpStatus.SERVICE_UNAVAILABLE.value())
            .error("Service Unavailable")
            .message("Serviço de análise de grafos temporariamente indisponível")
            .path(request.getRequestURI())
            .details(
                Map.of(
                    "service", "neo4j",
                    "suggestion", "Tente novamente em alguns segundos"))
            .build();

    return ResponseEntity.status(HttpStatus.SERVICE_UNAVAILABLE).body(error);
  }

  /** Tratamento de erro de conexão Redis (503). */
  @ExceptionHandler(RedisConnectionException.class)
  public ResponseEntity<ErrorResponse> handleRedisConnection(
      RedisConnectionException ex, HttpServletRequest request) {
    log.error("Erro de conexão Redis: {}", ex.getMessage());

    ErrorResponse error =
        ErrorResponse.builder()
            .timestamp(LocalDateTime.now())
            .status(HttpStatus.SERVICE_UNAVAILABLE.value())
            .error("Service Unavailable")
            .message("Serviço de cache temporariamente indisponível")
            .path(request.getRequestURI())
            .details(
                Map.of(
                    "service", "redis",
                    "suggestion", "Tente novamente em alguns segundos"))
            .build();

    return ResponseEntity.status(HttpStatus.SERVICE_UNAVAILABLE).body(error);
  }

  /** Tratamento de payload inválido (400). */
  @ExceptionHandler(InvalidPayloadException.class)
  public ResponseEntity<ErrorResponse> handleInvalidPayload(
      InvalidPayloadException ex, HttpServletRequest request) {
    log.warn("Payload inválido: {}", ex.getMessage());

    ErrorResponse error =
        ErrorResponse.builder()
            .timestamp(LocalDateTime.now())
            .status(HttpStatus.BAD_REQUEST.value())
            .error("Invalid Payload")
            .message(ex.getMessage())
            .path(request.getRequestURI())
            .details(
                Map.of(
                    "field", ex.getField() != null ? ex.getField() : "unknown"))
            .build();

    return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(error);
  }

  /** Tratamento de erro de configuração (500). */
  @ExceptionHandler(ConfigurationException.class)
  public ResponseEntity<ErrorResponse> handleConfiguration(
      ConfigurationException ex, HttpServletRequest request) {
    log.error("Erro de configuração: {}", ex.getMessage());

    ErrorResponse error =
        ErrorResponse.builder()
            .timestamp(LocalDateTime.now())
            .status(HttpStatus.INTERNAL_SERVER_ERROR.value())
            .error("Configuration Error")
            .message("Erro de configuração do sistema. Contate o administrador.")
            .path(request.getRequestURI())
            .build();

    return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(error);
  }

  /** Tratamento de exceção base RULEX (500). */
  @ExceptionHandler(RulexException.class)
  public ResponseEntity<ErrorResponse> handleRulexException(
      RulexException ex, HttpServletRequest request) {
    log.error("Erro RULEX: {}", ex.getMessage(), ex);

    ErrorResponse error =
        ErrorResponse.builder()
            .timestamp(LocalDateTime.now())
            .status(HttpStatus.INTERNAL_SERVER_ERROR.value())
            .error("RULEX Error")
            .message(ex.getMessage())
            .path(request.getRequestURI())
            .build();

    return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(error);
  }

  /** Tratamento de exceções genéricas. */
  @ExceptionHandler(Exception.class)
  public ResponseEntity<ErrorResponse> handleGenericException(
      Exception ex, HttpServletRequest request) {
    log.error("Erro interno do servidor", ex);

    ErrorResponse error =
        ErrorResponse.builder()
            .timestamp(LocalDateTime.now())
            .status(HttpStatus.INTERNAL_SERVER_ERROR.value())
            .error("Internal Server Error")
            .message("Ocorreu um erro interno no servidor. Por favor, tente novamente mais tarde.")
            .path(request.getRequestURI())
            .details(
                Map.of(
                    "type",
                    ex.getClass().getSimpleName(),
                    "suggestion",
                    "Se o problema persistir, contate o suporte"))
            .build();

    return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(error);
  }

  /** Classe interna para resposta de erro padronizada. */
  @lombok.Data
  @lombok.Builder
  public static class ErrorResponse {
    private LocalDateTime timestamp;
    private int status;
    private String error;
    private String message;
    private String path;
    private Map<String, Object> details;
  }
}
