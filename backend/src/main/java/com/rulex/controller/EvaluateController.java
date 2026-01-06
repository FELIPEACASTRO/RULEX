package com.rulex.controller;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.api.RawPayloadCaptureFilter;
import com.rulex.dto.EvaluateRequestDTO;
import com.rulex.dto.EvaluateResponse;
import com.rulex.service.RuleEngineService;
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
public class EvaluateController {

  private final RuleEngineService ruleEngineService;
  private final ObjectMapper objectMapper;

  /**
   * Avalia uma transação usando o DTO validado. Este é o endpoint preferido para novas integrações.
   */
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
              new EvaluateResponse(
                  null,
                  0,
                  null,
                  String.format(
                      "Payload muito grande: %d bytes. Limite máximo: 1.000.000 bytes (1 MB). "
                          + "Por favor, reduza o tamanho da requisição.",
                      rawBody.length())));
    }

    byte[] rawBytes = (byte[]) httpRequest.getAttribute(RawPayloadCaptureFilter.RAW_BYTES_ATTR);
    String contentType = httpRequest.getContentType();
    return ResponseEntity.ok(ruleEngineService.evaluateRaw(rawBody, rawBytes, contentType));
  }
}
