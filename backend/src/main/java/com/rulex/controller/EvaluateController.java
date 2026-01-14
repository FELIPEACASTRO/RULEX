package com.rulex.controller;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.api.RawPayloadCaptureFilter;
import com.rulex.dto.EvaluateRequestDTO;
import com.rulex.dto.EvaluateResponse;
import com.rulex.service.RuleEngineService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/** Endpoint de avaliação: decisão final + rule hits + popups agregados. */
@RestController
@RequestMapping("/evaluate")
@RequiredArgsConstructor
@Slf4j
@Tag(name = "Avaliação", description = "Endpoints para avaliação de transações e decisão de risco")
public class EvaluateController {

  private final RuleEngineService ruleEngineService;
  private final ObjectMapper objectMapper;

  /**
   * Avalia uma transação usando o DTO validado. Este é o endpoint preferido para novas integrações.
   */
  @Operation(
      summary = "Avaliar transação",
      description =
          "Avalia uma transação usando regras configuradas e retorna decisão de risco, score e regras acionadas")
  @ApiResponses(
      value = {
        @ApiResponse(
            responseCode = "200",
            description = "Avaliação realizada com sucesso",
            content =
                @Content(
                    mediaType = "application/json",
                    schema = @Schema(implementation = EvaluateResponse.class))),
        @ApiResponse(responseCode = "400", description = "Requisição inválida"),
        @ApiResponse(responseCode = "413", description = "Payload muito grande (limite: 1MB)")
      })
  @PostMapping
  public ResponseEntity<EvaluateResponse> evaluate(
      @Valid @RequestBody EvaluateRequestDTO request, HttpServletRequest httpRequest) {
    byte[] rawBytes = (byte[]) httpRequest.getAttribute(RawPayloadCaptureFilter.RAW_BYTES_ATTR);
    String contentType = httpRequest.getContentType();

    // Converte o payload do DTO para JSON string para compatibilidade com o serviço existente
    String rawBody;
    try {
      rawBody = objectMapper.writeValueAsString(request.getPayload());
    } catch (JsonProcessingException e) {
      log.error("Erro ao serializar payload: {}", e.getMessage());
      return ResponseEntity.badRequest().build();
    }

    return ResponseEntity.ok(ruleEngineService.evaluateRaw(rawBody, rawBytes, contentType));
  }

  /**
   * Endpoint legado que aceita raw JSON. Mantido para compatibilidade com integrações existentes.
   *
   * @deprecated Use o endpoint principal com EvaluateRequestDTO
   */
  @Operation(
      summary = "Avaliar transação (raw JSON)",
      description =
          "Endpoint legado que aceita JSON raw. Use o endpoint principal /evaluate para novas integrações",
      deprecated = true)
  @ApiResponses(
      value = {
        @ApiResponse(
            responseCode = "200",
            description = "Avaliação realizada com sucesso",
            content =
                @Content(
                    mediaType = "application/json",
                    schema = @Schema(implementation = EvaluateResponse.class))),
        @ApiResponse(responseCode = "400", description = "Body vazio ou inválido"),
        @ApiResponse(responseCode = "413", description = "Payload muito grande (limite: 1MB)")
      })
  @Deprecated
  @PostMapping("/raw")
  public ResponseEntity<EvaluateResponse> evaluateRaw(
      @RequestBody(required = false) String rawBody, HttpServletRequest httpRequest) {
    if (rawBody == null || rawBody.isBlank()) {
      log.warn("Requisição de avaliação com body vazio");
      return ResponseEntity.badRequest().build();
    }

    // FIX: BUG-001 - Validação de tamanho com mensagem clara
    if (rawBody.length() > 1_000_000) { // 1MB max
      log.warn("Payload muito grande: {} bytes (limite: 1MB)", rawBody.length());
      return ResponseEntity.status(413)
          .header("Content-Type", "application/json")
          .body(
              EvaluateResponse.builder()
                  .transactionId(null)
                  .classification("ERROR")
                  .riskScore(0)
                  .reason(
                      String.format(
                          "Payload muito grande: %d bytes. Limite máximo: 1.000.000 bytes (1 MB). "
                              + "Por favor, reduza o tamanho da requisição.",
                          rawBody.length()))
                  .timestamp(java.time.LocalDateTime.now())
                  .build());
    }

    byte[] rawBytes = (byte[]) httpRequest.getAttribute(RawPayloadCaptureFilter.RAW_BYTES_ATTR);
    String contentType = httpRequest.getContentType();
    return ResponseEntity.ok(ruleEngineService.evaluateRaw(rawBody, rawBytes, contentType));
  }
}
