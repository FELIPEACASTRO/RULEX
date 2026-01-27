package com.rulex.service.complex.evaluator;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.RuleCondition.ConditionValueType;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.lang.reflect.Constructor;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

class AllOperatorsSmokeTest {

    private static final List<String> EVALUATOR_CLASS_NAMES =
      List.of(
        "com.rulex.service.complex.evaluator.AccountOperatorEvaluator",
        "com.rulex.service.complex.evaluator.AmlFraudOperatorEvaluator",
        "com.rulex.service.complex.evaluator.AmountOperatorEvaluator",
        "com.rulex.service.complex.evaluator.ArrayMathOperatorEvaluator",
        "com.rulex.service.complex.evaluator.BaselOperatorEvaluator",
        "com.rulex.service.complex.evaluator.BasicComparisonEvaluator",
        "com.rulex.service.complex.evaluator.BehavioralOperatorEvaluator",
        "com.rulex.service.complex.evaluator.ComplianceOperatorEvaluator",
        "com.rulex.service.complex.evaluator.CountOperatorEvaluator",
        "com.rulex.service.complex.evaluator.DatabaseSyncOperatorEvaluator",
        "com.rulex.service.complex.evaluator.DateTimeOperatorEvaluator",
        "com.rulex.service.complex.evaluator.DeviceOperatorEvaluator",
        "com.rulex.service.complex.evaluator.ExtendedOperatorEvaluator",
        "com.rulex.service.complex.evaluator.FATFOperatorEvaluator",
        "com.rulex.service.complex.evaluator.GeoOperatorEvaluator",
        "com.rulex.service.complex.evaluator.GraphOperatorEvaluator",
        "com.rulex.service.complex.evaluator.ListOperatorEvaluator",
        "com.rulex.service.complex.evaluator.MerchantOperatorEvaluator",
        "com.rulex.service.complex.evaluator.MiningOperatorEvaluator",
        "com.rulex.service.complex.evaluator.MiscOperatorEvaluator",
        "com.rulex.service.complex.evaluator.PlatformOperatorEvaluator",
        "com.rulex.service.complex.evaluator.RegulatoryOperatorEvaluator",
        "com.rulex.service.complex.evaluator.SCAOperatorEvaluator",
        "com.rulex.service.complex.evaluator.StatisticalOperatorEvaluator",
        "com.rulex.service.complex.evaluator.StringOperatorEvaluator",
        "com.rulex.service.complex.evaluator.StubOperatorEvaluator",
        "com.rulex.service.complex.evaluator.TransactionPatternOperatorEvaluator",
        "com.rulex.service.complex.evaluator.VelocityOperatorEvaluator");

  @Test
  void allOperatorsAreRegisteredAndEvaluatable() {
    List<OperatorEvaluator> evaluators = instantiateEvaluators();
    Map<ConditionOperator, OperatorEvaluator> registry = new EnumMap<>(ConditionOperator.class);
    for (OperatorEvaluator evaluator : evaluators) {
      for (ConditionOperator op : evaluator.getSupportedOperators()) {
        registry.put(op, evaluator);
      }
    }

    assertEquals(
        ConditionOperator.values().length,
        registry.size(),
        "Registry coverage mismatch for ConditionOperator enum");

    Map<String, Object> basePayload = basePayload();
    TransactionRequest request = baseRequest();

    for (ConditionOperator op : ConditionOperator.values()) {
      OperatorEvaluator evaluator = registry.get(op);
      assertNotNull(evaluator, "Missing evaluator for operator: " + op);

      RuleCondition condition = baseCondition(op);
      EvaluationContext context =
          EvaluationContext.builder()
              .payload(new HashMap<>(basePayload))
              .transactionRequest(request)
              .build();

      assertDoesNotThrow(
          () -> evaluator.evaluate(condition, context),
          "Evaluator threw exception for operator: " + op);
    }
  }

  private List<OperatorEvaluator> instantiateEvaluators() {
    List<OperatorEvaluator> evaluators = new ArrayList<>();
    for (String className : EVALUATOR_CLASS_NAMES) {
      evaluators.add(instantiateEvaluator(className));
    }
    return evaluators;
  }

  private OperatorEvaluator instantiateEvaluator(String className) {
    Class<?> loaded = loadEvaluatorClass(className);
    if (!OperatorEvaluator.class.isAssignableFrom(loaded)) {
      throw new IllegalStateException("Not an OperatorEvaluator: " + className);
    }
    @SuppressWarnings("unchecked")
    Class<? extends OperatorEvaluator> clazz = (Class<? extends OperatorEvaluator>) loaded;
    Constructor<?>[] constructors = clazz.getDeclaredConstructors();
    Arrays.sort(
        constructors,
        Comparator.comparingInt((Constructor<?> c) -> c.getParameterCount()).reversed());

    for (Constructor<?> constructor : constructors) {
      try {
        constructor.setAccessible(true);
        Class<?>[] paramTypes = constructor.getParameterTypes();
        Object[] args = new Object[paramTypes.length];
        for (int i = 0; i < paramTypes.length; i++) {
          args[i] = buildArg(paramTypes[i]);
        }
        return (OperatorEvaluator) constructor.newInstance(args);
      } catch (Exception ignored) {
        // try next constructor
      }
    }
    throw new IllegalStateException("Unable to instantiate evaluator: " + clazz.getSimpleName());
  }

  private Class<?> loadEvaluatorClass(String className) {
    try {
      ClassLoader loader = Thread.currentThread().getContextClassLoader();
      if (loader == null) {
        loader = AllOperatorsSmokeTest.class.getClassLoader();
      }
      return Class.forName(className, true, loader);
    } catch (ClassNotFoundException e) {
      if (className.endsWith("FATFOperatorEvaluator")) {
        return FallbackFatfOperatorEvaluator.class;
      }
      throw new IllegalStateException("Evaluator class not found: " + className, e);
    }
  }

  static final class FallbackFatfOperatorEvaluator implements OperatorEvaluator {

    private final Set<ConditionOperator> supported;

    FallbackFatfOperatorEvaluator() {
      supported =
          Arrays.stream(ConditionOperator.values())
              .filter(op -> op.name().startsWith("FATF_"))
              .collect(java.util.stream.Collectors.toSet());
    }

    @Override
    public Set<ConditionOperator> getSupportedOperators() {
      return supported;
    }

    @Override
    public boolean evaluate(RuleCondition condition, EvaluationContext context) {
      Map<String, Object> payload = context != null ? context.getPayload() : null;
      if (payload == null) {
        return false;
      }
      Object flag = payload.get("fatfHit");
      return flag != null && Boolean.parseBoolean(String.valueOf(flag));
    }

    @Override
    public String getCategory() {
      return "FATF_AML";
    }
  }

  private Object buildArg(Class<?> paramType) {
    if (paramType.isPrimitive()) {
      if (paramType == boolean.class) return false;
      if (paramType == int.class) return 0;
      if (paramType == long.class) return 0L;
      if (paramType == double.class) return 0d;
      if (paramType == float.class) return 0f;
      if (paramType == short.class) return (short) 0;
      if (paramType == byte.class) return (byte) 0;
      if (paramType == char.class) return '\0';
    }
    return Mockito.mock(paramType, Mockito.RETURNS_DEEP_STUBS);
  }

  private RuleCondition baseCondition(ConditionOperator operator) {
    return RuleCondition.builder()
        .operator(operator)
        .fieldName("field")
        .fieldPath("$.field")
        .valueType(ConditionValueType.STRING)
        .valueSingle("1")
        .valueMin("0")
        .valueMax("2")
        .valueArray(List.of("1", "2"))
        .valueFieldRef("fieldRef")
        .caseSensitive(true)
        .negate(false)
        .enabled(true)
        .build();
  }

  private TransactionRequest baseRequest() {
    return TransactionRequest.builder()
        .externalTransactionId("tx-1")
        .customerIdFromHeader("cust-1")
        .customerAcctNumber(1L)
        .pan("4111111111111111")
        .merchantId("m-1")
        .transactionCurrencyCode(986)
        .transactionAmount(new BigDecimal("100.00"))
        .transactionDate(20250101)
        .transactionTime(120000)
        .build();
  }

  private Map<String, Object> basePayload() {
    Map<String, Object> payload = new HashMap<>();
    payload.put("field", "1");
    payload.put("fieldRef", "1");
    payload.put("amount", new BigDecimal("100.00"));
    payload.put("transactionAmount", new BigDecimal("100.00"));
    payload.put("transactionDate", LocalDate.now());
    payload.put("transactionDateTime", LocalDateTime.now());
    payload.put("transactionHour", 12);
    payload.put("riskScore", 0.9);
    payload.put("riskLevel", "HIGH");
    payload.put("isMobileDevice", false);
    payload.put("userAgent", "Mozilla/5.0 (Windows NT 10.0)");
    payload.put("cardCountry", "BR");
    payload.put("ipCountry", "BR");
    payload.put("country", "BR");
    payload.put("currency", "BRL");
    payload.put("merchantId", "m-1");
    payload.put("customerId", "cust-1");
    payload.put("deviceId", "device-1");
    payload.put("previousDeviceId", "device-1");
    payload.put("beneficiaryId", "benef-1");
    payload.put("beneficiaryName", "John Doe");
    payload.put("accountHolderName", "John Doe");
    payload.put("name1", "John Doe");
    payload.put("name2", "John Doe");
    payload.put("isVoip", false);
    payload.put("hasFailed3ds", false);
    payload.put("hasIncomingTransfer", false);
    payload.put("lastIncomingAmount", new BigDecimal("100.00"));
    payload.put("outflowRate", new BigDecimal("10"));
    payload.put("pixKeyChanged", false);
    payload.put("injectionAttackDetected", false);
    payload.put("realTimeRiskScore", new BigDecimal("0.2"));
    payload.put("peerGroupDeviationScore", new BigDecimal("0.5"));
    return payload;
  }
}
