package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import java.util.List;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class SuspiciousKeywordEvaluator {

  private SuspiciousKeywordEvaluator() {}

  public static boolean containsSuspiciousKeywords(Object fieldValue, RuleCondition condition) {
    try {
      if (fieldValue == null) return false;

      String text = String.valueOf(fieldValue).toLowerCase();
      List<String> keywords = condition.getValueArray();

      if ((keywords == null || keywords.isEmpty()) && condition.getValueSingle() != null) {
        keywords = java.util.Arrays.asList(condition.getValueSingle().split("\\|"));
      }

      if (keywords == null || keywords.isEmpty()) {
        keywords =
            List.of(
                "urgente",
                "transferir agora",
                "bloqueio",
                "seguranca",
                "atualizar dados",
                "conta suspensa",
                "premio",
                "heranca",
                "emprestimo aprovado",
                "divida",
                "spc",
                "serasa",
                "banco central",
                "pix devolvido",
                "comprovante falso");
      }

      return keywords.stream().map(String::toLowerCase).map(String::trim).anyMatch(text::contains);
    } catch (Exception e) {
      log.error("Erro ao avaliar CONTAINS_SUSPICIOUS_KEYWORDS: {}", e.getMessage());
      return false;
    }
  }
}
