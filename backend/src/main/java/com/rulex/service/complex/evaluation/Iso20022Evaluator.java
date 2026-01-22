package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import com.rulex.service.complex.context.FieldValueExtractor;
import com.rulex.service.complex.parsing.BooleanParser;
import com.rulex.service.complex.parsing.StringNormalizer;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class Iso20022Evaluator {

  private Iso20022Evaluator() {}

  public static boolean evaluatePacs008FieldValidation(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      String requiredCsv = condition.getValueSingle();
      if (requiredCsv == null || requiredCsv.isBlank()) {
        requiredCsv = "uetr,instgAgt,instgAgtBic,dbtrNm,cdtrNm,cdtrAcct";
      }
      String[] required =
          Arrays.stream(requiredCsv.split(","))
              .map(String::trim)
              .filter(s -> !s.isBlank())
              .toArray(String[]::new);

      // Retorna true quando a mensagem está inválida (campo ausente)
      for (String key : required) {
        Object v = context.getPayload().get(key);
        if (v == null) {
          // tenta variantes camelCase
          String camel = toCamelCaseFromSnake(key);
          v = context.getPayload().get(camel);
        }
        if (v == null || String.valueOf(v).isBlank()) return true;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar PACS008_FIELD_VALIDATION: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateRemittanceInfoAnalysis(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object infoObj =
          FieldValueExtractor.getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
      if (infoObj == null) {
        infoObj = context.getPayload().get("remittance_info");
        if (infoObj == null) infoObj = context.getPayload().get("remittanceInfo");
      }
      if (infoObj == null) return false;
      String text = StringNormalizer.normalizeForMatch(String.valueOf(infoObj));
      if (text.isBlank()) return false;

      List<String> keywords = condition.getValueArray();
      if (keywords == null || keywords.isEmpty()) {
        String csv = condition.getValueSingle();
        if (csv == null || csv.isBlank()) {
          keywords =
              List.of("gift", "loan", "crypto", "investment", "refund", "cash", "commission");
        } else {
          keywords =
              Arrays.stream(csv.split(",")).map(String::trim).filter(s -> !s.isBlank()).toList();
        }
      }

      for (String kw : keywords) {
        String needle = StringNormalizer.normalizeForMatch(kw);
        if (needle.isBlank()) continue;
        if (text.contains(needle)) return true;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar REMITTANCE_INFO_ANALYSIS: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluatePurposeCodeMismatch(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object purposeObj =
          FieldValueExtractor.getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
      if (purposeObj == null) {
        purposeObj = context.getPayload().get("purpose_code");
        if (purposeObj == null) purposeObj = context.getPayload().get("purposeCode");
      }
      if (purposeObj == null) return false;
      String purpose = String.valueOf(purposeObj).trim().toUpperCase(Locale.ROOT);
      if (purpose.isBlank()) return false;

      List<String> allowed = condition.getValueArray();
      if (allowed == null || allowed.isEmpty()) {
        String csv = condition.getValueSingle();
        if (csv == null || csv.isBlank()) return false;
        allowed =
            Arrays.stream(csv.split(",")).map(String::trim).filter(s -> !s.isBlank()).toList();
      }

      for (String entry : allowed) {
        if (purpose.equalsIgnoreCase(String.valueOf(entry).trim())) return false;
      }
      // mismatch quando não está na allowlist
      return true;
    } catch (Exception e) {
      log.error("Erro ao avaliar PURPOSE_CODE_MISMATCH: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateUetrDuplicateCheck(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object dupObj = context.getPayload().get("uetr_duplicate");
      if (dupObj == null) dupObj = context.getPayload().get("uetrDuplicate");
      return Boolean.TRUE.equals(BooleanParser.toBoolean(dupObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar UETR_DUPLICATE_CHECK: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateCreditorNameValidation(
      RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts =
          (condition.getValueSingle() == null ? "" : condition.getValueSingle()).split(":");
      String otherField = parts.length >= 1 && !parts[0].isBlank() ? parts[0].trim() : null;
      int threshold =
          parts.length >= 2 && !parts[1].isBlank() ? Integer.parseInt(parts[1].trim()) : 85;

      Object v1 =
          FieldValueExtractor.getFieldValue(
              condition.getFieldName(), condition.getFieldPath(), context);
      if (v1 == null) {
        v1 = context.getPayload() != null ? context.getPayload().get("creditor_name") : null;
        if (v1 == null && context.getPayload() != null)
          v1 = context.getPayload().get("creditorName");
      }
      if (v1 == null) return false;
      String creditorName = StringNormalizer.normalizeForMatch(String.valueOf(v1));
      if (creditorName.isBlank()) return true; // inválido

      if (otherField == null) return false;
      Object v2 = FieldValueExtractor.getFieldValue(otherField, null, context);
      if (v2 == null) return false;
      String referenceName = StringNormalizer.normalizeForMatch(String.valueOf(v2));
      if (referenceName.isBlank()) return false;

      return NameSimilarityEvaluator.calculateSimilarity(creditorName, referenceName) < threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar CREDITOR_NAME_VALIDATION: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateStructuredAddressCheck(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      String requiredCsv = condition.getValueSingle();
      if (requiredCsv == null || requiredCsv.isBlank()) {
        requiredCsv = "street,building,postCode,townName,country";
      }
      String[] required =
          Arrays.stream(requiredCsv.split(","))
              .map(String::trim)
              .filter(s -> !s.isBlank())
              .toArray(String[]::new);

      // Retorna true quando está inválido (faltando algo)
      for (String key : required) {
        Object v = context.getPayload().get(key);
        if (v == null || String.valueOf(v).isBlank()) return true;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar STRUCTURED_ADDRESS_CHECK: {}", e.getMessage());
      return false;
    }
  }

  private static String toCamelCaseFromSnake(String value) {
    if (value == null || value.isBlank()) return "";
    StringBuilder sb = new StringBuilder(value.length());
    boolean upperNext = false;
    for (int i = 0; i < value.length(); i++) {
      char c = value.charAt(i);
      if (c == '_') {
        upperNext = true;
        continue;
      }
      if (upperNext) {
        sb.append(Character.toUpperCase(c));
        upperNext = false;
      } else {
        sb.append(c);
      }
    }
    return sb.toString();
  }
}
