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
public final class SanctionsNameMatchingEvaluator {

  private SanctionsNameMatchingEvaluator() {}

  public static boolean evaluateOfacListCheck(Object fieldValue, RuleCondition condition) {
    try {
      if (fieldValue == null) return false;
      String name = StringNormalizer.normalizeForMatch(String.valueOf(fieldValue));
      if (name.isBlank()) return false;

      List<String> list = condition.getValueArray();
      if (list == null || list.isEmpty()) {
        String csv = condition.getValueSingle();
        if (csv == null || csv.isBlank()) return false;
        list = Arrays.stream(csv.split(",")).map(String::trim).filter(s -> !s.isBlank()).toList();
      }

      for (String entry : list) {
        String normalizedEntry = StringNormalizer.normalizeForMatch(entry);
        if (normalizedEntry.isBlank()) continue;
        if (name.equals(normalizedEntry)) return true;
        if (NameSimilarityEvaluator.calculateSimilarity(name, normalizedEntry) >= 92) return true;
      }

      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar OFAC_LIST_CHECK: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluatePepListCheck(Object fieldValue, RuleCondition condition) {
    try {
      if (fieldValue == null) return false;
      String name = StringNormalizer.normalizeForMatch(String.valueOf(fieldValue));
      if (name.isBlank()) return false;

      List<String> list = condition.getValueArray();
      if (list == null || list.isEmpty()) {
        String csv = condition.getValueSingle();
        if (csv == null || csv.isBlank()) return false;
        list = Arrays.stream(csv.split(",")).map(String::trim).filter(s -> !s.isBlank()).toList();
      }

      for (String entry : list) {
        String normalizedEntry = StringNormalizer.normalizeForMatch(entry);
        if (normalizedEntry.isBlank()) continue;
        if (name.equals(normalizedEntry)) return true;
        if (NameSimilarityEvaluator.calculateSimilarity(name, normalizedEntry) >= 92) return true;
      }

      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar PEP_LIST_CHECK: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateAdverseMediaCheck(Object fieldValue, RuleCondition condition) {
    try {
      if (fieldValue == null) return false;

      String text = StringNormalizer.normalizeForMatch(String.valueOf(fieldValue));
      if (text.isBlank()) return false;

      List<String> keywords = condition.getValueArray();
      if (keywords == null || keywords.isEmpty()) {
        String csv = condition.getValueSingle();
        if (csv == null || csv.isBlank()) {
          keywords =
              List.of("fraud", "scam", "money laundering", "sanction", "terror", "corruption");
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
      log.error("Erro ao avaliar ADVERSE_MEDIA_CHECK: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateSanctionsCountryCheck(Object fieldValue, RuleCondition condition) {
    try {
      if (fieldValue == null) return false;
      String country = String.valueOf(fieldValue).trim().toUpperCase(Locale.ROOT);
      if (country.isBlank()) return false;

      List<String> list = condition.getValueArray();
      if (list == null || list.isEmpty()) {
        String csv = condition.getValueSingle();
        if (csv == null || csv.isBlank()) return false;
        list = Arrays.stream(csv.split(",")).map(String::trim).filter(s -> !s.isBlank()).toList();
      }

      for (String entry : list) {
        if (country.equalsIgnoreCase(String.valueOf(entry).trim())) return true;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar SANCTIONS_COUNTRY_CHECK: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateHighRiskJurisdiction(Object fieldValue, RuleCondition condition) {
    try {
      if (fieldValue == null) return false;
      String jurisdiction = String.valueOf(fieldValue).trim().toUpperCase(Locale.ROOT);
      if (jurisdiction.isBlank()) return false;

      List<String> list = condition.getValueArray();
      if (list == null || list.isEmpty()) {
        String csv = condition.getValueSingle();
        if (csv == null || csv.isBlank()) return false;
        list = Arrays.stream(csv.split(",")).map(String::trim).filter(s -> !s.isBlank()).toList();
      }

      for (String entry : list) {
        if (jurisdiction.equalsIgnoreCase(String.valueOf(entry).trim())) return true;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar HIGH_RISK_JURISDICTION: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateNameTransliterationMatch(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      String otherField = null;
      int threshold = 90;

      if (condition.getValueSingle() != null && !condition.getValueSingle().isBlank()) {
        String[] parts = condition.getValueSingle().split(":");
        if (parts.length >= 1 && !parts[0].isBlank()) otherField = parts[0].trim();
        if (parts.length >= 2 && !parts[1].isBlank()) threshold = Integer.parseInt(parts[1].trim());
      }

      Object v1 = FieldValueExtractor.getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
      Object v2 = otherField != null ? FieldValueExtractor.getFieldValue(otherField, null, context) : null;
      if (v2 == null) {
        v2 = context.getPayload().get("transliterated_name");
        if (v2 == null) v2 = context.getPayload().get("transliteratedName");
      }

      if (v1 == null || v2 == null) return false;
      String s1 = StringNormalizer.normalizeForMatch(String.valueOf(v1));
      String s2 = StringNormalizer.normalizeForMatch(String.valueOf(v2));
      if (s1.isBlank() || s2.isBlank()) return false;

      int similarity = NameSimilarityEvaluator.calculateSimilarity(s1, s2);
      return similarity >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar NAME_TRANSLITERATION_MATCH: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateAliasDetection(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object aliasObj = context.getPayload().get("alias_detected");
      if (aliasObj == null) aliasObj = context.getPayload().get("aliasDetected");
      if (aliasObj != null) {
        return Boolean.TRUE.equals(BooleanParser.toBoolean(aliasObj));
      }

      Object fieldValue =
          FieldValueExtractor.getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
      if (fieldValue == null) return false;
      String name = StringNormalizer.normalizeForMatch(String.valueOf(fieldValue));
      if (name.isBlank()) return false;

      List<String> aliases = condition.getValueArray();
      if (aliases == null || aliases.isEmpty()) {
        String csv = condition.getValueSingle();
        if (csv == null || csv.isBlank()) return false;
        aliases = Arrays.stream(csv.split(",")).map(String::trim).filter(s -> !s.isBlank()).toList();
      }

      for (String alias : aliases) {
        String a = StringNormalizer.normalizeForMatch(alias);
        if (a.isBlank()) continue;
        if (name.contains(a) || a.contains(name)) return true;
        if (NameSimilarityEvaluator.calculateSimilarity(name, a) >= 90) return true;
      }

      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar ALIAS_DETECTION: {}", e.getMessage());
      return false;
    }
  }
}
