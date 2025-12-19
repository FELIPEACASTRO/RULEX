package com.rulex.controller;

import com.rulex.dto.EvaluateResponse;
import com.rulex.dto.TransactionRequest;
import com.rulex.service.RuleEngineService;
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

  @PostMapping
  public ResponseEntity<EvaluateResponse> evaluate(@Valid @RequestBody TransactionRequest request) {
    log.info("Evaluating transaction: {}", request.getExternalTransactionId());
    return ResponseEntity.ok(ruleEngineService.evaluate(request));
  }
}
