package com.rulex.v31.execlog;

import java.util.LinkedHashMap;
import java.util.Map;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * V3.1 audit console endpoint.
 *
 * <p>Read-only, safe-by-default: does not return payload contents, only hashes and decisions.
 */
@RestController
@RequestMapping("/audit/executions")
public class RuleExecutionLogController {

  private final RuleExecutionLogRepository repository;

  public RuleExecutionLogController(RuleExecutionLogRepository repository) {
    this.repository = repository;
  }

  @GetMapping
  public ResponseEntity<?> list(
      @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "20") int size,
      @RequestParam(required = false) String externalTransactionId) {

    int safeSize = Math.min(Math.max(size, 1), 200);
    int safePage = Math.max(page, 0);

    PageRequest pr = PageRequest.of(safePage, safeSize, Sort.by(Sort.Direction.DESC, "createdAt"));

    Page<RuleExecutionLogEntity> result;
    if (externalTransactionId != null && !externalTransactionId.isBlank()) {
      result = repository.findByExternalTransactionId(externalTransactionId.trim(), pr);
    } else {
      result = repository.findAll(pr);
    }

    Map<String, Object> resp = new LinkedHashMap<>();
    resp.put("content", result.getContent());
    resp.put("page", result.getNumber());
    resp.put("size", result.getSize());
    resp.put("totalElements", result.getTotalElements());
    resp.put("totalPages", result.getTotalPages());

    return ResponseEntity.ok(resp);
  }
}
