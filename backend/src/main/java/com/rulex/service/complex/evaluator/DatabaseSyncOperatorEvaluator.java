package com.rulex.service.complex.evaluator;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Evaluator para operadores sincronizados do banco de dados.
 * Implementa os 50 operadores que estavam no DB mas não no backend.
 */
@Component
@Slf4j
public class DatabaseSyncOperatorEvaluator implements OperatorEvaluator {

  private static final Set<ConditionOperator> SUPPORTED_OPERATORS = Set.of(
      // Operadores lógicos
      ConditionOperator.AND,
      ConditionOperator.OR,
      ConditionOperator.NOT,
      ConditionOperator.XOR,
      ConditionOperator.NAND,
      ConditionOperator.NOR,
      // Contexto e lista
      ConditionOperator.CONTEXT,
      ConditionOperator.NOT_IN_LIST,
      // Conta e idade
      ConditionOperator.ACCOUNT_AGE_LT_DAYS,
      ConditionOperator.EMAIL_DOMAIN_AGE_LT_DAYS,
      // Fraude e segurança
      ConditionOperator.FRAUD,
      ConditionOperator.SUSPICIOUS,
      ConditionOperator.SECURITY,
      ConditionOperator.VELOCITY,
      ConditionOperator.VELOCITY_ANOMALY,
      // Transação
      ConditionOperator.TRANSFER_AMOUNT_GT,
      ConditionOperator.TRANSFER_VELOCITY_GT,
      ConditionOperator.ROUND_AMOUNT,
      ConditionOperator.TIME_ANOMALY,
      ConditionOperator.AMOUNT_ANOMALY,
      // Device e sessão
      ConditionOperator.IS_NEW_DEVICE,
      ConditionOperator.IS_NEW_LOCATION,
      ConditionOperator.DEVICE_FINGERPRINT_MISMATCH,
      ConditionOperator.SESSION_DURATION_LT,
      // Comportamento
      ConditionOperator.CLICK_VELOCITY_GT,
      ConditionOperator.MOUSE_MOVEMENT_ANOMALY,
      ConditionOperator.TYPING_SPEED_ANOMALY,
      ConditionOperator.USER_AGENT_SUSPICIOUS,
      ConditionOperator.CAPTCHA_FAILED,
      // Cartão e terminal
      ConditionOperator.EXPIRED_CARD,
      ConditionOperator.CARD_CAPTURE_FRAUD,
      ConditionOperator.SUSPICIOUS_TERMINAL,
      ConditionOperator.TERMINAL_VERIFICATION_FAILED,
      ConditionOperator.POS_SECURITY_MISSING,
      ConditionOperator.UNUSUAL_CARD_MEDIA,
      ConditionOperator.OFFLINE_PIN_FAILED,
      ConditionOperator.PIN_CVV_LIMIT_EXCEEDED,
      ConditionOperator.EMV_SECURITY_CHECK,
      ConditionOperator.ECOMMERCE_NO_AVS,
      // Viagem e localização
      ConditionOperator.IMPOSSIBLE_TRAVEL,
      ConditionOperator.PHONE_COUNTRY_MISMATCH,
      ConditionOperator.ADDRESS_MISMATCH,
      // Merchant e MCC
      ConditionOperator.MERCHANT_ANOMALY,
      ConditionOperator.MCC_ANOMALY,
      ConditionOperator.SUSPICIOUS_TRANSACTION_TYPE,
      // Beneficiário e destinatário
      ConditionOperator.RECIPIENT_IN_WATCHLIST,
      ConditionOperator.RECIPIENT_IS_NEW,
      ConditionOperator.NAME_SIMILARITY_GT,
      // Contagem
      ConditionOperator.COUNT_DISTINCT_COUNTRIES_LAST_N_DAYS
  );

  @Override
  public Set<ConditionOperator> getSupportedOperators() {
    return SUPPORTED_OPERATORS;
  }

  @Override
  public boolean evaluate(RuleCondition condition, EvaluationContext context) {
    if (condition == null || context == null) {
      return false;
    }

    Object fieldValue = getFieldValue(context, condition.getFieldName());
    String conditionValue = condition.getValueSingle();
    Map<String, Object> payload = context.getPayload();

    return switch (condition.getOperator()) {
      // Operadores lógicos
      case AND -> evaluateAnd(fieldValue, conditionValue, payload);
      case OR -> evaluateOr(fieldValue, conditionValue, payload);
      case NOT -> evaluateNot(fieldValue, conditionValue, payload);
      case XOR -> evaluateXor(fieldValue, conditionValue, payload);
      case NAND -> evaluateNand(fieldValue, conditionValue, payload);
      case NOR -> evaluateNor(fieldValue, conditionValue, payload);
      
      // Contexto e lista
      case CONTEXT -> evaluateContext(fieldValue, conditionValue, payload);
      case NOT_IN_LIST -> evaluateNotInList(fieldValue, condition.getValueArray(), payload);
      
      // Conta e idade
      case ACCOUNT_AGE_LT_DAYS -> evaluateAccountAgeLtDays(fieldValue, conditionValue);
      case EMAIL_DOMAIN_AGE_LT_DAYS -> evaluateEmailDomainAgeLtDays(fieldValue, conditionValue);
      
      // Fraude e segurança
      case FRAUD -> evaluateFraud(fieldValue);
      case SUSPICIOUS -> evaluateSuspicious(fieldValue);
      case SECURITY -> evaluateSecurity(fieldValue);
      case VELOCITY -> evaluateVelocity(fieldValue, conditionValue);
      case VELOCITY_ANOMALY -> evaluateVelocityAnomaly(fieldValue, conditionValue, payload);
      
      // Transação
      case TRANSFER_AMOUNT_GT -> evaluateTransferAmountGt(fieldValue, conditionValue);
      case TRANSFER_VELOCITY_GT -> evaluateTransferVelocityGt(fieldValue, conditionValue);
      case ROUND_AMOUNT -> evaluateRoundAmount(fieldValue);
      case TIME_ANOMALY -> evaluateTimeAnomaly(fieldValue);
      case AMOUNT_ANOMALY -> evaluateAmountAnomaly(fieldValue, conditionValue, payload);
      
      // Device e sessão
      case IS_NEW_DEVICE -> evaluateIsNewDevice(fieldValue);
      case IS_NEW_LOCATION -> evaluateIsNewLocation(fieldValue);
      case DEVICE_FINGERPRINT_MISMATCH -> evaluateDeviceFingerprintMismatch(fieldValue);
      case SESSION_DURATION_LT -> evaluateSessionDurationLt(fieldValue, conditionValue);
      
      // Comportamento
      case CLICK_VELOCITY_GT -> evaluateClickVelocityGt(fieldValue, conditionValue);
      case MOUSE_MOVEMENT_ANOMALY -> evaluateMouseMovementAnomaly(fieldValue);
      case TYPING_SPEED_ANOMALY -> evaluateTypingSpeedAnomaly(fieldValue);
      case USER_AGENT_SUSPICIOUS -> evaluateUserAgentSuspicious(fieldValue);
      case CAPTCHA_FAILED -> evaluateCaptchaFailed(fieldValue);
      
      // Cartão e terminal
      case EXPIRED_CARD -> evaluateExpiredCard(fieldValue);
      case CARD_CAPTURE_FRAUD -> evaluateCardCaptureFraud(fieldValue);
      case SUSPICIOUS_TERMINAL -> evaluateSuspiciousTerminal(fieldValue);
      case TERMINAL_VERIFICATION_FAILED -> evaluateTerminalVerificationFailed(fieldValue);
      case POS_SECURITY_MISSING -> evaluatePosSecurityMissing(fieldValue);
      case UNUSUAL_CARD_MEDIA -> evaluateUnusualCardMedia(fieldValue);
      case OFFLINE_PIN_FAILED -> evaluateOfflinePinFailed(fieldValue);
      case PIN_CVV_LIMIT_EXCEEDED -> evaluatePinCvvLimitExceeded(fieldValue);
      case EMV_SECURITY_CHECK -> evaluateEmvSecurityCheck(fieldValue);
      case ECOMMERCE_NO_AVS -> evaluateEcommerceNoAvs(fieldValue);
      
      // Viagem e localização
      case IMPOSSIBLE_TRAVEL -> evaluateImpossibleTravel(fieldValue);
      case PHONE_COUNTRY_MISMATCH -> evaluatePhoneCountryMismatch(fieldValue);
      case ADDRESS_MISMATCH -> evaluateAddressMismatch(fieldValue);
      
      // Merchant e MCC
      case MERCHANT_ANOMALY -> evaluateMerchantAnomaly(fieldValue);
      case MCC_ANOMALY -> evaluateMccAnomaly(fieldValue);
      case SUSPICIOUS_TRANSACTION_TYPE -> evaluateSuspiciousTransactionType(fieldValue);
      
      // Beneficiário e destinatário
      case RECIPIENT_IN_WATCHLIST -> evaluateRecipientInWatchlist(fieldValue);
      case RECIPIENT_IS_NEW -> evaluateRecipientIsNew(fieldValue);
      case NAME_SIMILARITY_GT -> evaluateNameSimilarityGt(fieldValue, conditionValue);
      
      // Contagem
      case COUNT_DISTINCT_COUNTRIES_LAST_N_DAYS -> evaluateCountDistinctCountriesLastNDays(fieldValue, conditionValue);
      
      default -> false;
    };
  }

  private Object getFieldValue(EvaluationContext context, String fieldName) {
    if (context == null || fieldName == null) {
      return null;
    }
    Map<String, Object> payload = context.getPayload();
    if (payload != null && payload.containsKey(fieldName)) {
      return payload.get(fieldName);
    }
    if (context.getTransactionRequest() != null) {
      try {
        var request = context.getTransactionRequest();
        var field = request.getClass().getDeclaredField(fieldName);
        field.setAccessible(true);
        return field.get(request);
      } catch (Exception e) {
        log.trace("Field {} not found in TransactionRequest", fieldName);
      }
    }
    return null;
  }

  // ========== Operadores Lógicos ==========
  
  private boolean evaluateAnd(Object fieldValue, String conditionValue, Map<String, Object> context) {
    if (fieldValue instanceof Boolean && conditionValue != null) {
      return (Boolean) fieldValue && Boolean.parseBoolean(conditionValue);
    }
    return Boolean.TRUE.equals(fieldValue) && Boolean.parseBoolean(conditionValue);
  }

  private boolean evaluateOr(Object fieldValue, String conditionValue, Map<String, Object> context) {
    if (fieldValue instanceof Boolean && conditionValue != null) {
      return (Boolean) fieldValue || Boolean.parseBoolean(conditionValue);
    }
    return Boolean.TRUE.equals(fieldValue) || Boolean.parseBoolean(conditionValue);
  }

  private boolean evaluateNot(Object fieldValue, String conditionValue, Map<String, Object> context) {
    if (fieldValue instanceof Boolean) {
      return !(Boolean) fieldValue;
    }
    return !Boolean.TRUE.equals(fieldValue);
  }

  private boolean evaluateXor(Object fieldValue, String conditionValue, Map<String, Object> context) {
    boolean a = Boolean.TRUE.equals(fieldValue);
    boolean b = Boolean.parseBoolean(conditionValue);
    return a ^ b;
  }

  private boolean evaluateNand(Object fieldValue, String conditionValue, Map<String, Object> context) {
    return !evaluateAnd(fieldValue, conditionValue, context);
  }

  private boolean evaluateNor(Object fieldValue, String conditionValue, Map<String, Object> context) {
    return !evaluateOr(fieldValue, conditionValue, context);
  }

  // ========== Contexto e Lista ==========
  
  private boolean evaluateContext(Object fieldValue, String conditionValue, Map<String, Object> context) {
    if (conditionValue == null || context == null) return false;
    Object contextValue = context.get(conditionValue);
    return fieldValue != null && fieldValue.equals(contextValue);
  }

  private boolean evaluateNotInList(Object fieldValue, java.util.List<String> valueArray, Map<String, Object> context) {
    if (fieldValue == null) return true;
    if (valueArray == null || valueArray.isEmpty()) return true;
    String fieldStr = fieldValue.toString();
    return !valueArray.contains(fieldStr);
  }

  // ========== Conta e Idade ==========
  
  private boolean evaluateAccountAgeLtDays(Object fieldValue, String conditionValue) {
    try {
      double accountAgeDays = Double.parseDouble(fieldValue.toString());
      double threshold = Double.parseDouble(conditionValue);
      return accountAgeDays < threshold;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateEmailDomainAgeLtDays(Object fieldValue, String conditionValue) {
    try {
      double domainAgeDays = Double.parseDouble(fieldValue.toString());
      double threshold = Double.parseDouble(conditionValue);
      return domainAgeDays < threshold;
    } catch (Exception e) {
      return false;
    }
  }

  // ========== Fraude e Segurança ==========
  
  private boolean evaluateFraud(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue)) 
        || "FRAUD".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateSuspicious(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "SUSPICIOUS".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateSecurity(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateVelocity(Object fieldValue, String conditionValue) {
    try {
      double velocity = Double.parseDouble(fieldValue.toString());
      double threshold = Double.parseDouble(conditionValue);
      return velocity > threshold;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateVelocityAnomaly(Object fieldValue, String conditionValue, Map<String, Object> context) {
    try {
      double velocity = Double.parseDouble(fieldValue.toString());
      double avgVelocity = context != null && context.containsKey("avgVelocity") 
          ? Double.parseDouble(context.get("avgVelocity").toString()) : 0;
      double stdDev = context != null && context.containsKey("velocityStdDev") 
          ? Double.parseDouble(context.get("velocityStdDev").toString()) : 1;
      double threshold = conditionValue != null ? Double.parseDouble(conditionValue) : 2.0;
      return Math.abs(velocity - avgVelocity) > threshold * stdDev;
    } catch (Exception e) {
      return false;
    }
  }

  // ========== Transação ==========
  
  private boolean evaluateTransferAmountGt(Object fieldValue, String conditionValue) {
    try {
      double amount = Double.parseDouble(fieldValue.toString());
      double threshold = Double.parseDouble(conditionValue);
      return amount > threshold;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateTransferVelocityGt(Object fieldValue, String conditionValue) {
    try {
      double velocity = Double.parseDouble(fieldValue.toString());
      double threshold = Double.parseDouble(conditionValue);
      return velocity > threshold;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateRoundAmount(Object fieldValue) {
    try {
      double amount = Double.parseDouble(fieldValue.toString());
      return amount == Math.round(amount) && amount % 100 == 0;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateTimeAnomaly(Object fieldValue) {
    try {
      int hour = Integer.parseInt(fieldValue.toString());
      return hour >= 2 && hour <= 5;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateAmountAnomaly(Object fieldValue, String conditionValue, Map<String, Object> context) {
    try {
      double amount = Double.parseDouble(fieldValue.toString());
      double avgAmount = context != null && context.containsKey("avgAmount") 
          ? Double.parseDouble(context.get("avgAmount").toString()) : 0;
      double stdDev = context != null && context.containsKey("amountStdDev") 
          ? Double.parseDouble(context.get("amountStdDev").toString()) : 1;
      double threshold = conditionValue != null ? Double.parseDouble(conditionValue) : 3.0;
      return Math.abs(amount - avgAmount) > threshold * stdDev;
    } catch (Exception e) {
      return false;
    }
  }

  // ========== Device e Sessão ==========
  
  private boolean evaluateIsNewDevice(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "NEW".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateIsNewLocation(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "NEW".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateDeviceFingerprintMismatch(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "MISMATCH".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateSessionDurationLt(Object fieldValue, String conditionValue) {
    try {
      double duration = Double.parseDouble(fieldValue.toString());
      double threshold = Double.parseDouble(conditionValue);
      return duration < threshold;
    } catch (Exception e) {
      return false;
    }
  }

  // ========== Comportamento ==========
  
  private boolean evaluateClickVelocityGt(Object fieldValue, String conditionValue) {
    try {
      double velocity = Double.parseDouble(fieldValue.toString());
      double threshold = Double.parseDouble(conditionValue);
      return velocity > threshold;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateMouseMovementAnomaly(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "ANOMALY".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateTypingSpeedAnomaly(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "ANOMALY".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateUserAgentSuspicious(Object fieldValue) {
    if (fieldValue == null) return false;
    String userAgent = fieldValue.toString().toLowerCase();
    return userAgent.contains("bot") || userAgent.contains("crawler") || userAgent.contains("spider")
        || userAgent.contains("headless") || userAgent.contains("phantom");
  }

  private boolean evaluateCaptchaFailed(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "FAILED".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  // ========== Cartão e Terminal ==========
  
  private boolean evaluateExpiredCard(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "EXPIRED".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateCardCaptureFraud(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateSuspiciousTerminal(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "SUSPICIOUS".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateTerminalVerificationFailed(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "FAILED".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluatePosSecurityMissing(Object fieldValue) {
    return fieldValue == null || Boolean.TRUE.equals(fieldValue) 
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "MISSING".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateUnusualCardMedia(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "UNUSUAL".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateOfflinePinFailed(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "FAILED".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluatePinCvvLimitExceeded(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "EXCEEDED".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateEmvSecurityCheck(Object fieldValue) {
    return Boolean.FALSE.equals(fieldValue) || "false".equalsIgnoreCase(String.valueOf(fieldValue))
        || "FAILED".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateEcommerceNoAvs(Object fieldValue) {
    return fieldValue == null || Boolean.TRUE.equals(fieldValue) 
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "NO_AVS".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  // ========== Viagem e Localização ==========
  
  private boolean evaluateImpossibleTravel(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "IMPOSSIBLE".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluatePhoneCountryMismatch(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "MISMATCH".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateAddressMismatch(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "MISMATCH".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  // ========== Merchant e MCC ==========
  
  private boolean evaluateMerchantAnomaly(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "ANOMALY".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateMccAnomaly(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "ANOMALY".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateSuspiciousTransactionType(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "SUSPICIOUS".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  // ========== Beneficiário e Destinatário ==========
  
  private boolean evaluateRecipientInWatchlist(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "WATCHLIST".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateRecipientIsNew(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "NEW".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateNameSimilarityGt(Object fieldValue, String conditionValue) {
    try {
      double similarity = Double.parseDouble(fieldValue.toString());
      double threshold = Double.parseDouble(conditionValue);
      return similarity > threshold;
    } catch (Exception e) {
      return false;
    }
  }

  // ========== Contagem ==========
  
  private boolean evaluateCountDistinctCountriesLastNDays(Object fieldValue, String conditionValue) {
    try {
      int count = Integer.parseInt(fieldValue.toString());
      int threshold = Integer.parseInt(conditionValue);
      return count > threshold;
    } catch (Exception e) {
      return false;
    }
  }
}
