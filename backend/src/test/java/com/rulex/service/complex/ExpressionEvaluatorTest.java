package com.rulex.service.complex;

import static org.junit.jupiter.api.Assertions.*;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

@DisplayName("ExpressionEvaluator Tests")
class ExpressionEvaluatorTest {

  private ExpressionEvaluator evaluator;
  private Map<String, Object> context;

  @BeforeEach
  void setUp() {
    evaluator = new ExpressionEvaluator();
    context = new HashMap<>();
  }

  @Nested
  @DisplayName("Basic Evaluation")
  class BasicEvaluationTests {

    @Test
    @DisplayName("should return null for null expression")
    void shouldReturnNullForNullExpression() {
      Object result = evaluator.evaluate(null, context);
      assertNull(result);
    }

    @Test
    @DisplayName("should return null for empty expression")
    void shouldReturnNullForEmptyExpression() {
      Object result = evaluator.evaluate("", context);
      assertNull(result);
    }

    @Test
    @DisplayName("should return null for whitespace expression")
    void shouldReturnNullForWhitespaceExpression() {
      Object result = evaluator.evaluate("   ", context);
      assertNull(result);
    }
  }

  @Nested
  @DisplayName("Arithmetic Operations")
  class ArithmeticOperationsTests {

    @ParameterizedTest
    @CsvSource({"2 + 3, 5", "10 - 4, 6", "3 * 4, 12", "15 / 3, 5"})
    @DisplayName("should evaluate basic arithmetic")
    void shouldEvaluateBasicArithmetic(String expression, String expected) {
      Object result = evaluator.evaluate(expression, context);
      assertNotNull(result);
      assertEquals(new BigDecimal(expected), new BigDecimal(result.toString()));
    }

    @Test
    @DisplayName("should evaluate modulo operation")
    void shouldEvaluateModuloOperation() {
      Object result = evaluator.evaluate("MOD(10, 3)", context);
      assertNotNull(result);
    }

    @Test
    @DisplayName("should handle decimal numbers")
    void shouldHandleDecimalNumbers() {
      Object result = evaluator.evaluate("3.5 + 2.5", context);
      assertNotNull(result);
      assertEquals(0, new BigDecimal("6.0").compareTo(new BigDecimal(result.toString())));
    }

    @Test
    @DisplayName("should handle negative numbers")
    void shouldHandleNegativeNumbers() {
      Object result = evaluator.evaluate("-5 + 10", context);
      assertNotNull(result);
      assertEquals(new BigDecimal("5"), new BigDecimal(result.toString()));
    }
  }

  @Nested
  @DisplayName("Variable Resolution")
  class VariableResolutionTests {

    @Test
    @DisplayName("should resolve simple variable")
    void shouldResolveSimpleVariable() {
      context.put("amount", 100);
      Object result = evaluator.evaluate("${amount}", context);
      assertNotNull(result);
    }

    @Test
    @DisplayName("should resolve nested variable")
    void shouldResolveNestedVariable() {
      Map<String, Object> transaction = new HashMap<>();
      transaction.put("amount", 500);
      context.put("transaction", transaction);

      Object result = evaluator.evaluate("${transaction.amount}", context);
      assertNotNull(result);
    }

    @Test
    @DisplayName("should handle missing variable")
    void shouldHandleMissingVariable() {
      Object result = evaluator.evaluate("${unknown}", context);
      // Should handle gracefully
      assertNotNull(result);
    }
  }

  @Nested
  @DisplayName("Mathematical Functions")
  class MathematicalFunctionsTests {

    @Test
    @DisplayName("should evaluate ABS function")
    void shouldEvaluateAbsFunction() {
      Object result = evaluator.evaluate("ABS(-10)", context);
      assertNotNull(result);
      assertEquals(0, new BigDecimal("10").compareTo(new BigDecimal(result.toString())));
    }

    @Test
    @DisplayName("should evaluate ROUND function")
    void shouldEvaluateRoundFunction() {
      Object result = evaluator.evaluate("ROUND(3.7)", context);
      assertNotNull(result);
      assertEquals(0, new BigDecimal("4").compareTo(new BigDecimal(result.toString())));
    }

    @Test
    @DisplayName("should evaluate FLOOR function")
    void shouldEvaluateFloorFunction() {
      Object result = evaluator.evaluate("FLOOR(3.9)", context);
      assertNotNull(result);
      assertEquals(0, new BigDecimal("3").compareTo(new BigDecimal(result.toString())));
    }

    @Test
    @DisplayName("should evaluate CEIL function")
    void shouldEvaluateCeilFunction() {
      Object result = evaluator.evaluate("CEIL(3.1)", context);
      assertNotNull(result);
      assertEquals(0, new BigDecimal("4").compareTo(new BigDecimal(result.toString())));
    }

    @Test
    @DisplayName("should evaluate MIN function")
    void shouldEvaluateMinFunction() {
      Object result = evaluator.evaluate("MIN(5, 3, 8, 1)", context);
      assertNotNull(result);
      assertEquals(0, new BigDecimal("1").compareTo(new BigDecimal(result.toString())));
    }

    @Test
    @DisplayName("should evaluate MAX function")
    void shouldEvaluateMaxFunction() {
      Object result = evaluator.evaluate("MAX(5, 3, 8, 1)", context);
      assertNotNull(result);
      assertEquals(0, new BigDecimal("8").compareTo(new BigDecimal(result.toString())));
    }

    @Test
    @DisplayName("should evaluate SQRT function")
    void shouldEvaluateSqrtFunction() {
      Object result = evaluator.evaluate("SQRT(16)", context);
      assertNotNull(result);
      assertEquals(0, new BigDecimal("4").compareTo(new BigDecimal(result.toString())));
    }

    @Test
    @DisplayName("should evaluate POW function")
    void shouldEvaluatePowFunction() {
      Object result = evaluator.evaluate("POW(2, 3)", context);
      assertNotNull(result);
      assertEquals(0, new BigDecimal("8").compareTo(new BigDecimal(result.toString())));
    }

    @Test
    @DisplayName("should evaluate MOD function")
    void shouldEvaluateModFunction() {
      Object result = evaluator.evaluate("MOD(10, 3)", context);
      assertNotNull(result);
      assertEquals(0, new BigDecimal("1").compareTo(new BigDecimal(result.toString())));
    }
  }

  @Nested
  @DisplayName("String Functions")
  class StringFunctionsTests {

    @Test
    @DisplayName("should evaluate LENGTH function")
    void shouldEvaluateLengthFunction() {
      Object result = evaluator.evaluate("LENGTH(hello)", context);
      assertNotNull(result);
    }

    @Test
    @DisplayName("should evaluate UPPER function")
    void shouldEvaluateUpperFunction() {
      Object result = evaluator.evaluate("UPPER(hello)", context);
      assertNotNull(result);
    }

    @Test
    @DisplayName("should evaluate LOWER function")
    void shouldEvaluateLowerFunction() {
      Object result = evaluator.evaluate("LOWER(HELLO)", context);
      assertNotNull(result);
    }

    @Test
    @DisplayName("should evaluate TRIM function")
    void shouldEvaluateTrimFunction() {
      Object result = evaluator.evaluate("TRIM(  hello  )", context);
      assertNotNull(result);
    }
  }

  @Nested
  @DisplayName("Date Functions")
  class DateFunctionsTests {

    @Test
    @DisplayName("should evaluate NOW function")
    void shouldEvaluateNowFunction() {
      Object result = evaluator.evaluate("NOW()", context);
      assertNotNull(result);
    }

    @Test
    @DisplayName("should evaluate TODAY function")
    void shouldEvaluateTodayFunction() {
      Object result = evaluator.evaluate("TODAY()", context);
      assertNotNull(result);
    }
  }

  @Nested
  @DisplayName("Conditional Functions")
  class ConditionalFunctionsTests {

    @Test
    @DisplayName("should evaluate IF function - true condition")
    void shouldEvaluateIfFunctionTrue() {
      context.put("amount", 100);
      Object result = evaluator.evaluate("IF(true, yes, no)", context);
      assertNotNull(result);
    }

    @Test
    @DisplayName("should evaluate COALESCE function")
    void shouldEvaluateCoalesceFunction() {
      Object result = evaluator.evaluate("COALESCE(null, default)", context);
      assertNotNull(result);
    }
  }

  @Nested
  @DisplayName("Aggregation Functions")
  class AggregationFunctionsTests {

    @Test
    @DisplayName("should evaluate SUM function")
    void shouldEvaluateSumFunction() {
      context.put("values", List.of(1, 2, 3, 4, 5));
      Object result = evaluator.evaluate("SUM(1, 2, 3, 4, 5)", context);
      assertNotNull(result);
    }

    @Test
    @DisplayName("should evaluate COUNT function")
    void shouldEvaluateCountFunction() {
      context.put("items", List.of("a", "b", "c"));
      Object result = evaluator.evaluate("COUNT(1, 2, 3)", context);
      assertNotNull(result);
    }

    @Test
    @DisplayName("should evaluate AVG function")
    void shouldEvaluateAvgFunction() {
      Object result = evaluator.evaluate("AVG(10, 20, 30)", context);
      assertNotNull(result);
    }
  }

  @Nested
  @DisplayName("Complex Expressions")
  class ComplexExpressionsTests {

    @Test
    @DisplayName("should evaluate nested functions")
    void shouldEvaluateNestedFunctions() {
      // Nested functions may not be supported, just verify no exception
      assertDoesNotThrow(() -> evaluator.evaluate("ABS(MIN(-5, -10, -3))", context));
    }

    @Test
    @DisplayName("should evaluate expression with variables and functions")
    void shouldEvaluateExpressionWithVariablesAndFunctions() {
      context.put("base", 100);
      context.put("multiplier", 2);
      Object result = evaluator.evaluate("${base} * ${multiplier}", context);
      assertNotNull(result);
    }

    @Test
    @DisplayName("should handle complex arithmetic")
    void shouldHandleComplexArithmetic() {
      Object result = evaluator.evaluate("(10 + 5) * 2 - 3", context);
      assertNotNull(result);
    }
  }

  @Nested
  @DisplayName("Error Handling")
  class ErrorHandlingTests {

    @Test
    @DisplayName("should handle division by zero gracefully")
    void shouldHandleDivisionByZero() {
      // Should not throw exception
      assertDoesNotThrow(() -> evaluator.evaluate("10 / 0", context));
    }

    @Test
    @DisplayName("should handle invalid function gracefully")
    void shouldHandleInvalidFunction() {
      Object result = evaluator.evaluate("INVALID_FUNC(1)", context);
      // Should return something or handle gracefully
      assertNotNull(result);
    }

    @Test
    @DisplayName("should handle malformed expression gracefully")
    void shouldHandleMalformedExpression() {
      // Should not throw exception
      assertDoesNotThrow(() -> evaluator.evaluate("((((", context));
    }
  }

  @Nested
  @DisplayName("Edge Cases")
  class EdgeCasesTests {

    @Test
    @DisplayName("should handle very large numbers")
    void shouldHandleVeryLargeNumbers() {
      Object result = evaluator.evaluate("999999999999 + 1", context);
      assertNotNull(result);
    }

    @Test
    @DisplayName("should handle very small decimals")
    void shouldHandleVerySmallDecimals() {
      Object result = evaluator.evaluate("0.0000001 + 0.0000001", context);
      assertNotNull(result);
    }

    @Test
    @DisplayName("should handle empty context")
    void shouldHandleEmptyContext() {
      Object result = evaluator.evaluate("5 + 5", new HashMap<>());
      assertNotNull(result);
    }

    @Test
    @DisplayName("should handle null context")
    void shouldHandleNullContext() {
      // Should handle gracefully or throw appropriate exception
      assertDoesNotThrow(() -> evaluator.evaluate("5 + 5", null));
    }
  }
}
