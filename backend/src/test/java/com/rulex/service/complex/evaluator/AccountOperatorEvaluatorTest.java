package com.rulex.service.complex.evaluator;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Testes unitários para AccountOperatorEvaluator.
 *
 * <p>GAP-2 FIX: Cobertura de testes para evaluators.
 *
 * @version 1.0.0
 */
@DisplayName("AccountOperatorEvaluator Tests")
class AccountOperatorEvaluatorTest {

    private AccountOperatorEvaluator evaluator;

    @BeforeEach
    void setUp() {
        evaluator = new AccountOperatorEvaluator();
    }

    // ========== HELPER METHODS ==========

    private RuleCondition createCondition(String fieldName, ConditionOperator operator, String value) {
        RuleCondition condition = new RuleCondition();
        condition.setFieldName(fieldName);
        condition.setOperator(operator);
        condition.setValueSingle(value);
        return condition;
    }

    private EvaluationContext createContext(Map<String, Object> payload) {
        return EvaluationContext.builder()
            .payload(payload)
            .build();
    }

    // ========== SUPPORTED OPERATORS ==========

    @Test
    @DisplayName("Deve suportar operadores de conta")
    void shouldSupportAccountOperators() {
        var supported = evaluator.getSupportedOperators();

        assertThat(supported).contains(
            ConditionOperator.ACCOUNT_AGE_LT_MINUTES,
            ConditionOperator.ACCOUNT_LINK_DEPTH,
            ConditionOperator.ACCOUNT_TAKEOVER_PATTERN,
            ConditionOperator.DAYS_SINCE_LAST_ACTIVITY,
            ConditionOperator.CHARGEBACK_RATE_GT,
            ConditionOperator.IS_NEW,
            ConditionOperator.IS_FIRST
        );
    }

    @Test
    @DisplayName("Deve retornar categoria correta")
    void shouldReturnCorrectCategory() {
        assertThat(evaluator.getCategory()).isEqualTo("ACCOUNT");
    }

    // ========== ACCOUNT_AGE_LT_MINUTES ==========

    @Nested
    @DisplayName("Operador ACCOUNT_AGE_LT_MINUTES")
    class AccountAgeLtMinutesTests {

        @Test
        @DisplayName("Deve retornar true quando conta é mais nova que threshold")
        void shouldReturnTrueWhenAccountYoungerThanThreshold() {
            RuleCondition condition = createCondition("accountAge", ConditionOperator.ACCOUNT_AGE_LT_MINUTES, "60");
            EvaluationContext context = createContext(Map.of("accountAgeMinutes", 30));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isTrue();
        }

        @Test
        @DisplayName("Deve retornar false quando conta é mais velha que threshold")
        void shouldReturnFalseWhenAccountOlderThanThreshold() {
            RuleCondition condition = createCondition("accountAge", ConditionOperator.ACCOUNT_AGE_LT_MINUTES, "60");
            EvaluationContext context = createContext(Map.of("accountAgeMinutes", 120));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isFalse();
        }

        @ParameterizedTest
        @CsvSource({
            "30, 60, true",   // 30 < 60
            "60, 60, false",  // 60 < 60
            "90, 60, false"   // 90 < 60
        })
        @DisplayName("ACCOUNT_AGE_LT_MINUTES deve comparar corretamente")
        void accountAgeLtMinutesShouldCompareCorrectly(int ageMinutes, int threshold, boolean expected) {
            RuleCondition condition = createCondition("accountAge", ConditionOperator.ACCOUNT_AGE_LT_MINUTES, String.valueOf(threshold));
            EvaluationContext context = createContext(Map.of("accountAgeMinutes", ageMinutes));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isEqualTo(expected);
        }
    }

    // ========== IS_NEW ==========

    @Nested
    @DisplayName("Operador IS_NEW")
    class IsNewTests {

        @Test
        @DisplayName("Deve retornar true para conta nova")
        void shouldReturnTrueForNewAccount() {
            RuleCondition condition = createCondition("account", ConditionOperator.IS_NEW, null);
            EvaluationContext context = createContext(Map.of("isNewAccount", true));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isTrue();
        }

        @Test
        @DisplayName("Deve retornar false para conta existente")
        void shouldReturnFalseForExistingAccount() {
            RuleCondition condition = createCondition("account", ConditionOperator.IS_NEW, null);
            EvaluationContext context = createContext(Map.of("isNewAccount", false));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isFalse();
        }

        @Test
        @DisplayName("Deve funcionar com string 'true'")
        void shouldWorkWithStringTrue() {
            RuleCondition condition = createCondition("account", ConditionOperator.IS_NEW, null);
            EvaluationContext context = createContext(Map.of("isNewAccount", "true"));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isTrue();
        }

        @Test
        @DisplayName("Deve considerar conta com poucas transações como nova")
        void shouldConsiderAccountWithFewTransactionsAsNew() {
            RuleCondition condition = createCondition("account", ConditionOperator.IS_NEW, null);
            EvaluationContext context = createContext(Map.of("previousTransactionCount", 0));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isTrue();
        }
    }

    // ========== IS_FIRST ==========

    @Nested
    @DisplayName("Operador IS_FIRST")
    class IsFirstTests {

        @Test
        @DisplayName("Deve retornar true para primeira transação")
        void shouldReturnTrueForFirstTransaction() {
            RuleCondition condition = createCondition("transaction", ConditionOperator.IS_FIRST, null);
            EvaluationContext context = createContext(Map.of("isFirstTransaction", true));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isTrue();
        }

        @Test
        @DisplayName("Deve retornar false para transação subsequente")
        void shouldReturnFalseForSubsequentTransaction() {
            RuleCondition condition = createCondition("transaction", ConditionOperator.IS_FIRST, null);
            EvaluationContext context = createContext(Map.of("isFirstTransaction", false));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isFalse();
        }

        @Test
        @DisplayName("Deve considerar transação com count=0 como primeira")
        void shouldConsiderTransactionWithZeroCountAsFirst() {
            RuleCondition condition = createCondition("transaction", ConditionOperator.IS_FIRST, null);
            EvaluationContext context = createContext(Map.of("transaction_count", 0));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isTrue();
        }
    }

    // ========== DAYS_SINCE_LAST_ACTIVITY ==========

    @Nested
    @DisplayName("Operador DAYS_SINCE_LAST_ACTIVITY")
    class DaysSinceLastActivityTests {

        @Test
        @DisplayName("Deve retornar true quando dias desde última atividade excede threshold")
        void shouldReturnTrueWhenDaysExceedThreshold() {
            RuleCondition condition = createCondition("activity", ConditionOperator.DAYS_SINCE_LAST_ACTIVITY, "30");
            EvaluationContext context = createContext(Map.of("daysSinceLastActivity", 45));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isTrue();
        }

        @Test
        @DisplayName("Deve retornar false quando dias desde última atividade não excede threshold")
        void shouldReturnFalseWhenDaysDoNotExceedThreshold() {
            RuleCondition condition = createCondition("activity", ConditionOperator.DAYS_SINCE_LAST_ACTIVITY, "30");
            EvaluationContext context = createContext(Map.of("daysSinceLastActivity", 15));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isFalse();
        }
    }

    // ========== CHARGEBACK_RATE_GT ==========

    @Nested
    @DisplayName("Operador CHARGEBACK_RATE_GT")
    class ChargebackRateGtTests {

        @Test
        @DisplayName("Deve retornar true quando taxa de chargeback excede threshold")
        void shouldReturnTrueWhenChargebackRateExceedsThreshold() {
            RuleCondition condition = createCondition("chargebackRate", ConditionOperator.CHARGEBACK_RATE_GT, "0.01");
            EvaluationContext context = createContext(Map.of("chargebackRate", 0.02));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isTrue();
        }

        @Test
        @DisplayName("Deve retornar false quando taxa de chargeback não excede threshold")
        void shouldReturnFalseWhenChargebackRateDoesNotExceedThreshold() {
            RuleCondition condition = createCondition("chargebackRate", ConditionOperator.CHARGEBACK_RATE_GT, "0.01");
            EvaluationContext context = createContext(Map.of("chargebackRate", 0.005));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isFalse();
        }

        @ParameterizedTest
        @CsvSource({
            "0.02, 0.01, true",   // 2% > 1%
            "0.01, 0.01, false",  // 1% > 1%
            "0.005, 0.01, false"  // 0.5% > 1%
        })
        @DisplayName("CHARGEBACK_RATE_GT deve comparar corretamente")
        void chargebackRateGtShouldCompareCorrectly(double rate, double threshold, boolean expected) {
            RuleCondition condition = createCondition("chargebackRate", ConditionOperator.CHARGEBACK_RATE_GT, String.valueOf(threshold));
            EvaluationContext context = createContext(Map.of("chargebackRate", rate));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isEqualTo(expected);
        }
    }

    // ========== ACCOUNT_TAKEOVER_PATTERN ==========

    @Nested
    @DisplayName("Operador ACCOUNT_TAKEOVER_PATTERN")
    class AccountTakeoverPatternTests {

        @Test
        @DisplayName("Deve detectar padrão de account takeover")
        void shouldDetectAccountTakeoverPattern() {
            RuleCondition condition = createCondition("account", ConditionOperator.ACCOUNT_TAKEOVER_PATTERN, null);
            EvaluationContext context = createContext(Map.of(
                "passwordChangedRecently", true,
                "emailChangedRecently", true,
                "phoneChangedRecently", true,
                "newDeviceUsed", true
            ));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isTrue();
        }

        @Test
        @DisplayName("Deve retornar false quando não há padrão de takeover")
        void shouldReturnFalseWhenNoTakeoverPattern() {
            RuleCondition condition = createCondition("account", ConditionOperator.ACCOUNT_TAKEOVER_PATTERN, null);
            EvaluationContext context = createContext(Map.of(
                "passwordChangedRecently", false,
                "emailChangedRecently", false,
                "phoneChangedRecently", false,
                "newDeviceUsed", false
            ));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isFalse();
        }
    }

    // ========== ACCOUNT_LINK_DEPTH ==========

    @Nested
    @DisplayName("Operador ACCOUNT_LINK_DEPTH")
    class AccountLinkDepthTests {

        @Test
        @DisplayName("Deve retornar true quando profundidade de link excede threshold")
        void shouldReturnTrueWhenLinkDepthExceedsThreshold() {
            RuleCondition condition = createCondition("accountLinks", ConditionOperator.ACCOUNT_LINK_DEPTH, "3");
            EvaluationContext context = createContext(Map.of("accountLinkDepth", 5));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isTrue();
        }

        @Test
        @DisplayName("Deve retornar false quando profundidade de link não excede threshold")
        void shouldReturnFalseWhenLinkDepthDoesNotExceedThreshold() {
            RuleCondition condition = createCondition("accountLinks", ConditionOperator.ACCOUNT_LINK_DEPTH, "3");
            EvaluationContext context = createContext(Map.of("accountLinkDepth", 2));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isFalse();
        }
    }

    // ========== EDGE CASES ==========

    @Nested
    @DisplayName("Casos de Borda")
    class EdgeCaseTests {

        @Test
        @DisplayName("Deve tratar campo ausente graciosamente")
        void shouldHandleMissingFieldGracefully() {
            RuleCondition condition = createCondition("nonExistent", ConditionOperator.IS_NEW, null);
            EvaluationContext context = createContext(Map.of());

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isFalse();
        }

        @Test
        @DisplayName("Deve tratar valor nulo graciosamente")
        void shouldHandleNullValueGracefully() {
            RuleCondition condition = createCondition("accountAge", ConditionOperator.ACCOUNT_AGE_LT_MINUTES, "60");
            java.util.HashMap<String, Object> payload = new java.util.HashMap<>();
            payload.put("accountAgeMinutes", null);
            EvaluationContext context = createContext(payload);

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isFalse();
        }

        @Test
        @DisplayName("Deve tratar threshold inválido graciosamente")
        void shouldHandleInvalidThresholdGracefully() {
            RuleCondition condition = createCondition("accountAge", ConditionOperator.ACCOUNT_AGE_LT_MINUTES, "invalid");
            EvaluationContext context = createContext(Map.of("accountAgeMinutes", 30));

            boolean result = evaluator.evaluate(condition, context);

            // Com threshold inválido (0), 30 < 0 = false
            assertThat(result).isFalse();
        }

        @Test
        @DisplayName("Deve funcionar com valores como strings")
        void shouldWorkWithValuesAsStrings() {
            RuleCondition condition = createCondition("accountAge", ConditionOperator.ACCOUNT_AGE_LT_MINUTES, "60");
            EvaluationContext context = createContext(Map.of("accountAgeMinutes", "30"));

            boolean result = evaluator.evaluate(condition, context);

            assertThat(result).isTrue();
        }
    }
}
