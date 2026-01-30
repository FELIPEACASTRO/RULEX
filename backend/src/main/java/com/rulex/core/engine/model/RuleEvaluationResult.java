package com.rulex.core.engine.model;

import com.rulex.dto.TriggeredRuleDTO;
import com.rulex.entity.TransactionDecision;
import java.util.List;
import java.util.Map;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/** Resultado da avaliação de regras. */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class RuleEvaluationResult {
  private int riskScore;
  private List<TriggeredRuleDTO> triggeredRules;
  private Map<String, Object> scoreDetails;
  private TransactionDecision.TransactionClassification classification;
  private String reason;
}
