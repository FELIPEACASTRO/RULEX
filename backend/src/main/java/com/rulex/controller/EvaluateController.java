package com.rulex.controller;

import com.rulex.api.RawPayloadCaptureFilter;
import com.rulex.dto.EvaluateResponse;
import com.rulex.service.RuleEngineService;
import jakarta.servlet.http.HttpServletRequest;
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

  @PostMapping
  public ResponseEntity<EvaluateResponse> evaluate(
      @RequestBody String rawBody, HttpServletRequest httpRequest) {
    byte[] rawBytes = (byte[]) httpRequest.getAttribute(RawPayloadCaptureFilter.RAW_BYTES_ATTR);
    String contentType = httpRequest.getContentType();
    return ResponseEntity.ok(ruleEngineService.evaluateRaw(rawBody, rawBytes, contentType));
  }
}
