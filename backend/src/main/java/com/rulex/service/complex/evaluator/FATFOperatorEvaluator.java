package com.rulex.service.complex.evaluator;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Evaluator para operadores FATF (Financial Action Task Force).
 * Implementa tipologias de lavagem de dinheiro conforme FATF.
 * 
 * Categorias:
 * - Placement: Colocação de fundos ilícitos no sistema financeiro
 * - Layering: Camadas de transações para ocultar origem
 * - Integration: Integração de fundos lavados na economia legítima
 * - TBML: Trade-Based Money Laundering
 */
@Component
@Slf4j
public class FATFOperatorEvaluator implements OperatorEvaluator {

  private static final Set<ConditionOperator> SUPPORTED = Set.of(
      // Placement
      ConditionOperator.FATF_PLACEMENT_CASH_INTENSIVE,
      ConditionOperator.FATF_PLACEMENT_STRUCTURING,
      ConditionOperator.FATF_PLACEMENT_SMURFING,
      ConditionOperator.FATF_PLACEMENT_CURRENCY_EXCHANGE,
      ConditionOperator.FATF_PLACEMENT_CASINO_GAMBLING,
      // Layering
      ConditionOperator.FATF_LAYERING_RAPID_MOVEMENT,
      ConditionOperator.FATF_LAYERING_SHELL_COMPANY,
      ConditionOperator.FATF_LAYERING_OFFSHORE,
      ConditionOperator.FATF_LAYERING_WIRE_CHAINS,
      ConditionOperator.FATF_LAYERING_CONVERTIBLE_INSTRUMENTS,
      // Integration
      ConditionOperator.FATF_INTEGRATION_REAL_ESTATE,
      ConditionOperator.FATF_INTEGRATION_LUXURY_GOODS,
      ConditionOperator.FATF_INTEGRATION_BUSINESS_INVESTMENT,
      ConditionOperator.FATF_INTEGRATION_LOAN_REPAYMENT,
      // TBML
      ConditionOperator.FATF_TBML_OVER_INVOICING,
      ConditionOperator.FATF_TBML_UNDER_INVOICING,
      ConditionOperator.FATF_TBML_PHANTOM_SHIPPING,
      ConditionOperator.FATF_TBML_MULTIPLE_INVOICING,
      ConditionOperator.FATF_TBML_FALSE_DESCRIPTION,
      // Others
      ConditionOperator.FATF_HAWALA_INFORMAL,
      ConditionOperator.FATF_NEW_PAYMENT_EXPLOITATION,
      ConditionOperator.FATF_CRYPTO_MIXING,
      ConditionOperator.FATF_CRYPTO_ATM_CASHOUT,
      ConditionOperator.FATF_PEP_TRANSACTION,
      ConditionOperator.FATF_CORRESPONDENT_LAYERING,
      ConditionOperator.FATF_ROUND_TRIPPING,
      ConditionOperator.FATF_BLACK_MARKET_EXCHANGE,
      ConditionOperator.FATF_INSURANCE_CASH_VALUE
  );

  @Override
  public Set<ConditionOperator> getSupportedOperators() {
    return SUPPORTED;
  }

  @Override
  public boolean evaluate(RuleCondition condition, EvaluationContext context) {
    if (condition == null || context == null) {
      return false;
    }

    Object fieldValue = getFieldValue(context, condition.getFieldName());
    String threshold = condition.getValueSingle();
    Map<String, Object> payload = context.getPayload();

    return switch (condition.getOperator()) {
      // Placement
      case FATF_PLACEMENT_CASH_INTENSIVE -> evaluateCashIntensive(fieldValue, threshold, payload);
      case FATF_PLACEMENT_STRUCTURING -> evaluateStructuring(fieldValue, threshold, payload);
      case FATF_PLACEMENT_SMURFING -> evaluateSmurfing(fieldValue, threshold, payload);
      case FATF_PLACEMENT_CURRENCY_EXCHANGE -> evaluateCurrencyExchange(fieldValue, threshold, payload);
      case FATF_PLACEMENT_CASINO_GAMBLING -> evaluateCasinoGambling(fieldValue, threshold, payload);
      // Layering
      case FATF_LAYERING_RAPID_MOVEMENT -> evaluateRapidMovement(fieldValue, threshold, payload);
      case FATF_LAYERING_SHELL_COMPANY -> evaluateShellCompany(fieldValue, threshold, payload);
      case FATF_LAYERING_OFFSHORE -> evaluateOffshore(fieldValue, threshold, payload);
      case FATF_LAYERING_WIRE_CHAINS -> evaluateWireChains(fieldValue, threshold, payload);
      case FATF_LAYERING_CONVERTIBLE_INSTRUMENTS -> evaluateConvertibleInstruments(fieldValue, threshold, payload);
      // Integration
      case FATF_INTEGRATION_REAL_ESTATE -> evaluateRealEstate(fieldValue, threshold, payload);
      case FATF_INTEGRATION_LUXURY_GOODS -> evaluateLuxuryGoods(fieldValue, threshold, payload);
      case FATF_INTEGRATION_BUSINESS_INVESTMENT -> evaluateBusinessInvestment(fieldValue, threshold, payload);
      case FATF_INTEGRATION_LOAN_REPAYMENT -> evaluateLoanRepayment(fieldValue, threshold, payload);
      // TBML
      case FATF_TBML_OVER_INVOICING -> evaluateOverInvoicing(fieldValue, threshold, payload);
      case FATF_TBML_UNDER_INVOICING -> evaluateUnderInvoicing(fieldValue, threshold, payload);
      case FATF_TBML_PHANTOM_SHIPPING -> evaluatePhantomShipping(fieldValue, threshold, payload);
      case FATF_TBML_MULTIPLE_INVOICING -> evaluateMultipleInvoicing(fieldValue, threshold, payload);
      case FATF_TBML_FALSE_DESCRIPTION -> evaluateFalseDescription(fieldValue, threshold, payload);
      // Others
      case FATF_HAWALA_INFORMAL -> evaluateHawala(fieldValue, threshold, payload);
      case FATF_NEW_PAYMENT_EXPLOITATION -> evaluateNewPaymentExploitation(fieldValue, threshold, payload);
      case FATF_CRYPTO_MIXING -> evaluateCryptoMixing(fieldValue, threshold, payload);
      case FATF_CRYPTO_ATM_CASHOUT -> evaluateCryptoAtmCashout(fieldValue, threshold, payload);
      case FATF_PEP_TRANSACTION -> evaluatePepTransaction(fieldValue, threshold, payload);
      case FATF_CORRESPONDENT_LAYERING -> evaluateCorrespondentLayering(fieldValue, threshold, payload);
      case FATF_ROUND_TRIPPING -> evaluateRoundTripping(fieldValue, threshold, payload);
      case FATF_BLACK_MARKET_EXCHANGE -> evaluateBlackMarketExchange(fieldValue, threshold, payload);
      case FATF_INSURANCE_CASH_VALUE -> evaluateInsuranceCashValue(fieldValue, threshold, payload);
      default -> false;
    };
  }

  private Object getFieldValue(EvaluationContext context, String fieldName) {
    if (context == null || fieldName == null) return null;
    Map<String, Object> payload = context.getPayload();
    if (payload != null && payload.containsKey(fieldName)) {
      return payload.get(fieldName);
    }
    return null;
  }

  // ========== Placement ==========

  private boolean evaluateCashIntensive(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica se o negócio é intensivo em dinheiro (restaurantes, varejo, etc.)
    if (fieldValue == null) return false;
    String mcc = fieldValue.toString();
    // MCCs de negócios intensivos em cash
    return mcc.startsWith("58") || mcc.startsWith("54") || mcc.startsWith("55");
  }

  private boolean evaluateStructuring(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica estruturação (valores abaixo do limite de reporte)
    try {
      double amount = Double.parseDouble(fieldValue.toString());
      double ctrThreshold = threshold != null ? Double.parseDouble(threshold) : 10000.0;
      // Estruturação: valores entre 80% e 99% do limite
      return amount >= ctrThreshold * 0.8 && amount < ctrThreshold;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateSmurfing(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica smurfing (múltiplas transações pequenas)
    try {
      int count = Integer.parseInt(fieldValue.toString());
      int minCount = threshold != null ? Integer.parseInt(threshold) : 5;
      return count >= minCount;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateCurrencyExchange(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica troca de moeda em grande volume
    try {
      double amount = Double.parseDouble(fieldValue.toString());
      double minAmount = threshold != null ? Double.parseDouble(threshold) : 50000.0;
      return amount >= minAmount;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateCasinoGambling(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica transações em cassinos/apostas
    if (fieldValue == null) return false;
    String mcc = fieldValue.toString();
    return mcc.equals("7995") || mcc.equals("7800") || mcc.equals("7801") || mcc.equals("7802");
  }

  // ========== Layering ==========

  private boolean evaluateRapidMovement(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica movimento rápido de fundos (tempo entre transações)
    try {
      int minutes = Integer.parseInt(fieldValue.toString());
      int maxMinutes = threshold != null ? Integer.parseInt(threshold) : 30;
      return minutes <= maxMinutes;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateShellCompany(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica indicadores de empresa de fachada
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "SHELL".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateOffshore(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica transações para jurisdições offshore
    if (fieldValue == null) return false;
    String country = fieldValue.toString().toUpperCase();
    // Lista de jurisdições offshore comuns
    return country.equals("KY") || country.equals("VG") || country.equals("PA") 
        || country.equals("BZ") || country.equals("SC") || country.equals("MU");
  }

  private boolean evaluateWireChains(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica cadeias de transferências
    try {
      int hops = Integer.parseInt(fieldValue.toString());
      int maxHops = threshold != null ? Integer.parseInt(threshold) : 3;
      return hops >= maxHops;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateConvertibleInstruments(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica uso de instrumentos conversíveis (cheques, ordens de pagamento)
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  // ========== Integration ==========

  private boolean evaluateRealEstate(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica transações imobiliárias
    if (fieldValue == null) return false;
    String mcc = fieldValue.toString();
    return mcc.equals("6513") || mcc.startsWith("65");
  }

  private boolean evaluateLuxuryGoods(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica compra de bens de luxo
    if (fieldValue == null) return false;
    String mcc = fieldValue.toString();
    // MCCs de joalherias, carros de luxo, arte
    return mcc.equals("5944") || mcc.equals("5571") || mcc.equals("5932");
  }

  private boolean evaluateBusinessInvestment(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica investimento em negócios
    try {
      double amount = Double.parseDouble(fieldValue.toString());
      double minAmount = threshold != null ? Double.parseDouble(threshold) : 100000.0;
      return amount >= minAmount;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateLoanRepayment(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica pagamento de empréstimo com fundos suspeitos
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  // ========== TBML ==========

  private boolean evaluateOverInvoicing(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica superfaturamento
    try {
      double ratio = Double.parseDouble(fieldValue.toString());
      double maxRatio = threshold != null ? Double.parseDouble(threshold) : 1.5;
      return ratio > maxRatio;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateUnderInvoicing(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica subfaturamento
    try {
      double ratio = Double.parseDouble(fieldValue.toString());
      double minRatio = threshold != null ? Double.parseDouble(threshold) : 0.5;
      return ratio < minRatio;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluatePhantomShipping(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica envio fantasma
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "PHANTOM".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateMultipleInvoicing(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica múltiplas faturas para mesma mercadoria
    try {
      int count = Integer.parseInt(fieldValue.toString());
      int maxCount = threshold != null ? Integer.parseInt(threshold) : 1;
      return count > maxCount;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateFalseDescription(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica descrição falsa de mercadorias
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  // ========== Others ==========

  private boolean evaluateHawala(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica indicadores de hawala/transferência informal
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "HAWALA".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateNewPaymentExploitation(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica exploração de novos métodos de pagamento
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateCryptoMixing(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica uso de mixers de criptomoedas
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "MIXER".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateCryptoAtmCashout(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica saque em ATM de criptomoedas
    try {
      double amount = Double.parseDouble(fieldValue.toString());
      double maxAmount = threshold != null ? Double.parseDouble(threshold) : 5000.0;
      return amount >= maxAmount;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluatePepTransaction(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica transação com PEP (Pessoa Politicamente Exposta)
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "PEP".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateCorrespondentLayering(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica layering via bancos correspondentes
    try {
      int hops = Integer.parseInt(fieldValue.toString());
      int maxHops = threshold != null ? Integer.parseInt(threshold) : 2;
      return hops >= maxHops;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateRoundTripping(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica round-tripping (dinheiro sai e volta)
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateBlackMarketExchange(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica câmbio no mercado negro
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateInsuranceCashValue(Object fieldValue, String threshold, Map<String, Object> payload) {
    // Verifica extração de valor em dinheiro de seguros
    try {
      double amount = Double.parseDouble(fieldValue.toString());
      double minAmount = threshold != null ? Double.parseDouble(threshold) : 50000.0;
      return amount >= minAmount;
    } catch (Exception e) {
      return false;
    }
  }

  @Override
  public String getCategory() {
    return "FATF_AML";
  }
}
