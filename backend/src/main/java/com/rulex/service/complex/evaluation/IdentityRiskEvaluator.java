package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import com.rulex.service.complex.context.FieldValueExtractor;
import com.rulex.service.complex.parsing.BooleanParser;
import com.rulex.service.complex.parsing.StringNormalizer;
import java.util.Arrays;
import java.util.List;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class IdentityRiskEvaluator {

  private IdentityRiskEvaluator() {}

  public static boolean evaluateCpfSsnValidation(Object fieldValue, RuleCondition condition) {
    try {
      if (fieldValue == null) return false;
      String raw = String.valueOf(fieldValue);
      String digits = raw.replaceAll("\\D", "");
      if (digits.length() == 11) {
        return isValidCpf(digits);
      }
      if (digits.length() == 9) {
        // SSN básico: não pode ser tudo zero, e não pode iniciar com 000.
        if (digits.chars().allMatch(ch -> ch == '0')) return false;
        if (digits.startsWith("000")) return false;
        return true;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar CPF_SSN_VALIDATION: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluatePhoneCarrierCheck(Object fieldValue, RuleCondition condition) {
    try {
      if (fieldValue == null) return false;
      String carrier = StringNormalizer.normalizeForMatch(String.valueOf(fieldValue));
      if (carrier.isBlank()) return false;

      List<String> list = condition.getValueArray();
      if (list == null || list.isEmpty()) {
        String csv = condition.getValueSingle();
        if (csv == null || csv.isBlank()) return false;
        list = Arrays.stream(csv.split(",")).map(String::trim).filter(s -> !s.isBlank()).toList();
      }

      for (String entry : list) {
        if (carrier.equals(StringNormalizer.normalizeForMatch(entry))) return true;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar PHONE_CARRIER_CHECK: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateEmailDomainAge(Object fieldValue, RuleCondition condition) {
    try {
      int thresholdDays = 30;
      if (condition.getValueSingle() != null && !condition.getValueSingle().isBlank()) {
        thresholdDays = Integer.parseInt(condition.getValueSingle().trim());
      }

      Integer ageDays = null;
      if (fieldValue instanceof Number) {
        ageDays = ((Number) fieldValue).intValue();
      } else if (fieldValue != null) {
        String email = String.valueOf(fieldValue).trim();
        // Se vier como email, procuramos a idade do domínio no payload/variáveis.
        // (feature pré-computada)
        // Sem payload aqui, então só validamos formato básico e falhamos fechado.
        if (!email.contains("@")) return false;
      }

      if (ageDays == null) return false;
      // Regra de risco: domínio muito novo.
      return ageDays < thresholdDays;
    } catch (Exception e) {
      log.error("Erro ao avaliar EMAIL_DOMAIN_AGE: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateAddressVerification(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object verifiedObj = context.getPayload().get("address_verified");
      if (verifiedObj == null) verifiedObj = context.getPayload().get("addressVerified");
      if (verifiedObj == null) {
        verifiedObj =
            FieldValueExtractor.getFieldValue(
                condition.getFieldName(), condition.getFieldPath(), context);
      }
      Boolean verified = BooleanParser.toBoolean(verifiedObj);
      if (verified == null) return false;

      if (condition.getValueSingle() == null || condition.getValueSingle().isBlank()) {
        return verified;
      }
      boolean expected = Boolean.parseBoolean(condition.getValueSingle().trim());
      return verified == expected;
    } catch (Exception e) {
      log.error("Erro ao avaliar ADDRESS_VERIFICATION: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateIdentityVelocity(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      String[] parts =
          (condition.getValueSingle() == null ? "" : condition.getValueSingle()).split("\\|");
      long threshold =
          parts.length >= 1 && !parts[0].isBlank() ? Long.parseLong(parts[0].trim()) : 3L;

      Object countObj = context.getPayload().get("identity_velocity_count");
      if (countObj == null) countObj = context.getPayload().get("identityVelocityCount");
      if (countObj == null)
        countObj =
            FieldValueExtractor.getFieldValue(
                condition.getFieldName(), condition.getFieldPath(), context);
      long count = countObj instanceof Number ? ((Number) countObj).longValue() : 0L;
      return count >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar IDENTITY_VELOCITY: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateDeviceAccountRatio(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object ratioObj = context.getPayload().get("device_account_ratio");
      if (ratioObj == null) ratioObj = context.getPayload().get("deviceAccountRatio");
      if (ratioObj == null)
        ratioObj =
            FieldValueExtractor.getFieldValue(
                condition.getFieldName(), condition.getFieldPath(), context);

      double ratio = ratioObj instanceof Number ? ((Number) ratioObj).doubleValue() : 0.0;
      return ratio > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar DEVICE_ACCOUNT_RATIO: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateEmailPhoneMismatch(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object mismatchObj = context.getPayload().get("email_phone_mismatch");
      if (mismatchObj == null) mismatchObj = context.getPayload().get("emailPhoneMismatch");
      if (mismatchObj == null)
        mismatchObj =
            FieldValueExtractor.getFieldValue(
                condition.getFieldName(), condition.getFieldPath(), context);
      return Boolean.TRUE.equals(BooleanParser.toBoolean(mismatchObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar EMAIL_PHONE_MISMATCH: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateCreditFileThin(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      int minMonths = Integer.parseInt(condition.getValueSingle().trim());

      Object monthsObj = context.getPayload().get("credit_file_months");
      if (monthsObj == null) monthsObj = context.getPayload().get("creditFileMonths");
      if (monthsObj == null)
        monthsObj =
            FieldValueExtractor.getFieldValue(
                condition.getFieldName(), condition.getFieldPath(), context);

      int months =
          monthsObj instanceof Number ? ((Number) monthsObj).intValue() : Integer.MAX_VALUE;
      return months < minMonths;
    } catch (Exception e) {
      log.error("Erro ao avaliar CREDIT_FILE_THIN: {}", e.getMessage());
      return false;
    }
  }

  private static boolean isValidCpf(String digits11) {
    if (digits11 == null) return false;
    String cpf = digits11.replaceAll("\\D", "");
    if (cpf.length() != 11) return false;
    // Bloqueia CPFs com todos os dígitos iguais
    boolean allSame = cpf.chars().distinct().count() == 1;
    if (allSame) return false;

    int d1 = calcCpfDigit(cpf, 9, 10);
    int d2 = calcCpfDigit(cpf, 10, 11);
    return d1 == (cpf.charAt(9) - '0') && d2 == (cpf.charAt(10) - '0');
  }

  private static int calcCpfDigit(String cpf, int len, int weightStart) {
    int sum = 0;
    int weight = weightStart;
    for (int i = 0; i < len; i++) {
      int n = cpf.charAt(i) - '0';
      sum += n * weight;
      weight--;
    }
    int mod = sum % 11;
    return (mod < 2) ? 0 : (11 - mod);
  }
}
