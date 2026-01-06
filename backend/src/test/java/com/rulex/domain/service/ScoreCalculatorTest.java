package com.rulex.domain.service;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.domain.model.Classification;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

@DisplayName("ScoreCalculator - Domain Service")
class ScoreCalculatorTest {

  private ScoreCalculator calculator;

  @BeforeEach
  void setUp() {
    calculator = new ScoreCalculator();
  }

  @Nested
  @DisplayName("calculateTotal() - Score Aggregation")
  class CalculateTotal {

    @Test
    @DisplayName("Should return 0 for empty contributions")
    void testCalculateTotal_Empty() {
      int result = calculator.calculateTotal();

      assertThat(result).isZero();
    }

    @Test
    @DisplayName("Should sum single contribution")
    void testCalculateTotal_Single() {
      int result = calculator.calculateTotal(50);

      assertThat(result).isEqualTo(50);
    }

    @Test
    @DisplayName("Should sum multiple contributions")
    void testCalculateTotal_Multiple() {
      int result = calculator.calculateTotal(20, 30, 15);

      assertThat(result).isEqualTo(65);
    }

    @Test
    @DisplayName("Should clamp total at 100")
    void testCalculateTotal_ClampAt100() {
      int result = calculator.calculateTotal(60, 70); // Total would be 130

      assertThat(result).isEqualTo(100);
    }

    @Test
    @DisplayName("Should clamp total at 0 for negative contributions")
    void testCalculateTotal_NegativeContributions() {
      int result = calculator.calculateTotal(-10, -20);

      assertThat(result).isZero();
    }

    @Test
    @DisplayName("Should clamp individual negative contributions before summing")
    void testCalculateTotal_MixedNegativePositive() {
      int result = calculator.calculateTotal(-10, 50, 20);

      // -10 clamped to 0, then 0 + 50 + 20 = 70
      assertThat(result).isEqualTo(70);
    }

    @Test
    @DisplayName("Should handle large number of contributions")
    void testCalculateTotal_ManyContributions() {
      int result = calculator.calculateTotal(10, 10, 10, 10, 10, 10, 10, 10, 10, 10);

      assertThat(result).isEqualTo(100); // 10 * 10 = 100
    }

    @Test
    @DisplayName("Should clamp when many small contributions exceed 100")
    void testCalculateTotal_ManySmallContributionsExceed() {
      int result = calculator.calculateTotal(15, 15, 15, 15, 15, 15, 15, 15); // 8 * 15 = 120

      assertThat(result).isEqualTo(100);
    }
  }

  @Nested
  @DisplayName("classifyByScore() - Score Classification")
  class ClassifyByScore {

    @ParameterizedTest
    @ValueSource(ints = {0, 10, 20, 29})
    @DisplayName("Should classify as APPROVED when score < 30")
    void testClassifyByScore_Approved(int score) {
      Classification result = calculator.classifyByScore(score);

      assertThat(result).isEqualTo(Classification.APPROVED);
    }

    @ParameterizedTest
    @ValueSource(ints = {30, 40, 50, 60, 69})
    @DisplayName("Should classify as SUSPICIOUS when 30 <= score < 70")
    void testClassifyByScore_Suspicious(int score) {
      Classification result = calculator.classifyByScore(score);

      assertThat(result).isEqualTo(Classification.SUSPICIOUS);
    }

    @ParameterizedTest
    @ValueSource(ints = {70, 80, 90, 100})
    @DisplayName("Should classify as FRAUD when score >= 70")
    void testClassifyByScore_Fraud(int score) {
      Classification result = calculator.classifyByScore(score);

      assertThat(result).isEqualTo(Classification.FRAUD);
    }

    @Test
    @DisplayName("Should classify boundary: 29 as APPROVED")
    void testClassifyByScore_Boundary29() {
      assertThat(calculator.classifyByScore(29)).isEqualTo(Classification.APPROVED);
    }

    @Test
    @DisplayName("Should classify boundary: 30 as SUSPICIOUS")
    void testClassifyByScore_Boundary30() {
      assertThat(calculator.classifyByScore(30)).isEqualTo(Classification.SUSPICIOUS);
    }

    @Test
    @DisplayName("Should classify boundary: 69 as SUSPICIOUS")
    void testClassifyByScore_Boundary69() {
      assertThat(calculator.classifyByScore(69)).isEqualTo(Classification.SUSPICIOUS);
    }

    @Test
    @DisplayName("Should classify boundary: 70 as FRAUD")
    void testClassifyByScore_Boundary70() {
      assertThat(calculator.classifyByScore(70)).isEqualTo(Classification.FRAUD);
    }

    @Test
    @DisplayName("Should classify negative score as APPROVED")
    void testClassifyByScore_Negative() {
      Classification result = calculator.classifyByScore(-10);

      assertThat(result).isEqualTo(Classification.APPROVED);
    }

    @Test
    @DisplayName("Should classify score > 100 as FRAUD")
    void testClassifyByScore_Over100() {
      Classification result = calculator.classifyByScore(150);

      assertThat(result).isEqualTo(Classification.FRAUD);
    }
  }

  @Nested
  @DisplayName("calculateContribution() - Weighted Contributions")
  class CalculateContribution {

    @Test
    @DisplayName("Should calculate contribution with multiplier 1.0")
    void testCalculateContribution_Multiplier1() {
      int result = calculator.calculateContribution(50, 1.0);

      assertThat(result).isEqualTo(50);
    }

    @Test
    @DisplayName("Should calculate contribution with multiplier < 1.0")
    void testCalculateContribution_MultiplierLessThan1() {
      int result = calculator.calculateContribution(50, 0.5);

      assertThat(result).isEqualTo(25);
    }

    @Test
    @DisplayName("Should calculate contribution with multiplier > 1.0")
    void testCalculateContribution_MultiplierGreaterThan1() {
      int result = calculator.calculateContribution(50, 2.0);

      assertThat(result).isEqualTo(100); // Clamped at 100
    }

    @Test
    @DisplayName("Should clamp contribution at 100")
    void testCalculateContribution_ClampAt100() {
      int result = calculator.calculateContribution(80, 2.0); // 80 * 2.0 = 160

      assertThat(result).isEqualTo(100);
    }

    @Test
    @DisplayName("Should clamp contribution at 0 for negative weight")
    void testCalculateContribution_NegativeWeight() {
      int result = calculator.calculateContribution(-50, 1.0);

      assertThat(result).isZero();
    }

    @Test
    @DisplayName("Should handle zero weight")
    void testCalculateContribution_ZeroWeight() {
      int result = calculator.calculateContribution(0, 2.0);

      assertThat(result).isZero();
    }

    @Test
    @DisplayName("Should handle zero multiplier")
    void testCalculateContribution_ZeroMultiplier() {
      int result = calculator.calculateContribution(50, 0.0);

      assertThat(result).isZero();
    }

    @Test
    @DisplayName("Should handle fractional multipliers correctly")
    void testCalculateContribution_FractionalMultiplier() {
      int result = calculator.calculateContribution(50, 0.75);

      assertThat(result).isEqualTo(37); // 50 * 0.75 = 37.5, cast to int = 37
    }

    @Test
    @DisplayName("Should handle negative multiplier")
    void testCalculateContribution_NegativeMultiplier() {
      int result = calculator.calculateContribution(50, -1.0);

      assertThat(result).isZero(); // -50 clamped to 0
    }
  }

  @Nested
  @DisplayName("clamp() - Value Normalization")
  class Clamp {

    @Test
    @DisplayName("Should return value when within range [0, 100]")
    void testClamp_WithinRange() {
      assertThat(calculator.clamp(50)).isEqualTo(50);
      assertThat(calculator.clamp(0)).isZero();
      assertThat(calculator.clamp(100)).isEqualTo(100);
    }

    @Test
    @DisplayName("Should clamp value below 0 to 0")
    void testClamp_BelowMin() {
      assertThat(calculator.clamp(-1)).isZero();
      assertThat(calculator.clamp(-100)).isZero();
    }

    @Test
    @DisplayName("Should clamp value above 100 to 100")
    void testClamp_AboveMax() {
      assertThat(calculator.clamp(101)).isEqualTo(100);
      assertThat(calculator.clamp(500)).isEqualTo(100);
    }

    @Test
    @DisplayName("Should handle boundary values")
    void testClamp_Boundaries() {
      assertThat(calculator.clamp(0)).isZero();
      assertThat(calculator.clamp(100)).isEqualTo(100);
    }

    @Test
    @DisplayName("Should handle extreme negative values")
    void testClamp_ExtremeNegative() {
      assertThat(calculator.clamp(Integer.MIN_VALUE)).isZero();
    }

    @Test
    @DisplayName("Should handle extreme positive values")
    void testClamp_ExtremePositive() {
      assertThat(calculator.clamp(Integer.MAX_VALUE)).isEqualTo(100);
    }
  }

  @Nested
  @DisplayName("shouldApprove() - Approval Logic")
  class ShouldApprove {

    @ParameterizedTest
    @ValueSource(ints = {0, 10, 20, 29})
    @DisplayName("Should return true when score < 30")
    void testShouldApprove_True(int score) {
      assertThat(calculator.shouldApprove(score)).isTrue();
    }

    @ParameterizedTest
    @ValueSource(ints = {30, 40, 50, 70, 100})
    @DisplayName("Should return false when score >= 30")
    void testShouldApprove_False(int score) {
      assertThat(calculator.shouldApprove(score)).isFalse();
    }

    @Test
    @DisplayName("Should return true for negative scores")
    void testShouldApprove_Negative() {
      assertThat(calculator.shouldApprove(-10)).isTrue();
    }

    @Test
    @DisplayName("Should return false at boundary 30")
    void testShouldApprove_Boundary30() {
      assertThat(calculator.shouldApprove(30)).isFalse();
    }

    @Test
    @DisplayName("Should return true at boundary 29")
    void testShouldApprove_Boundary29() {
      assertThat(calculator.shouldApprove(29)).isTrue();
    }
  }

  @Nested
  @DisplayName("shouldBlock() - Blocking Logic")
  class ShouldBlock {

    @ParameterizedTest
    @ValueSource(ints = {70, 80, 90, 100})
    @DisplayName("Should return true when score >= 70")
    void testShouldBlock_True(int score) {
      assertThat(calculator.shouldBlock(score)).isTrue();
    }

    @ParameterizedTest
    @ValueSource(ints = {0, 30, 50, 69})
    @DisplayName("Should return false when score < 70")
    void testShouldBlock_False(int score) {
      assertThat(calculator.shouldBlock(score)).isFalse();
    }

    @Test
    @DisplayName("Should return false for negative scores")
    void testShouldBlock_Negative() {
      assertThat(calculator.shouldBlock(-10)).isFalse();
    }

    @Test
    @DisplayName("Should return true at boundary 70")
    void testShouldBlock_Boundary70() {
      assertThat(calculator.shouldBlock(70)).isTrue();
    }

    @Test
    @DisplayName("Should return false at boundary 69")
    void testShouldBlock_Boundary69() {
      assertThat(calculator.shouldBlock(69)).isFalse();
    }

    @Test
    @DisplayName("Should return true for scores > 100")
    void testShouldBlock_Over100() {
      assertThat(calculator.shouldBlock(150)).isTrue();
    }
  }

  @Nested
  @DisplayName("shouldReview() - Manual Review Logic")
  class ShouldReview {

    @ParameterizedTest
    @ValueSource(ints = {30, 40, 50, 60, 69})
    @DisplayName("Should return true when 30 <= score < 70")
    void testShouldReview_True(int score) {
      assertThat(calculator.shouldReview(score)).isTrue();
    }

    @ParameterizedTest
    @ValueSource(ints = {0, 29, 70, 100})
    @DisplayName("Should return false when score < 30 or score >= 70")
    void testShouldReview_False(int score) {
      assertThat(calculator.shouldReview(score)).isFalse();
    }

    @Test
    @DisplayName("Should return false for negative scores")
    void testShouldReview_Negative() {
      assertThat(calculator.shouldReview(-10)).isFalse();
    }

    @Test
    @DisplayName("Should return false at boundary 29")
    void testShouldReview_Boundary29() {
      assertThat(calculator.shouldReview(29)).isFalse();
    }

    @Test
    @DisplayName("Should return true at boundary 30")
    void testShouldReview_Boundary30() {
      assertThat(calculator.shouldReview(30)).isTrue();
    }

    @Test
    @DisplayName("Should return true at boundary 69")
    void testShouldReview_Boundary69() {
      assertThat(calculator.shouldReview(69)).isTrue();
    }

    @Test
    @DisplayName("Should return false at boundary 70")
    void testShouldReview_Boundary70() {
      assertThat(calculator.shouldReview(70)).isFalse();
    }
  }

  @Nested
  @DisplayName("Constants Verification")
  class ConstantsVerification {

    @Test
    @DisplayName("Should have correct MIN_SCORE constant")
    void testMinScoreConstant() {
      assertThat(ScoreCalculator.MIN_SCORE).isZero();
    }

    @Test
    @DisplayName("Should have correct MAX_SCORE constant")
    void testMaxScoreConstant() {
      assertThat(ScoreCalculator.MAX_SCORE).isEqualTo(100);
    }

    @Test
    @DisplayName("Should have correct SUSPICIOUS_THRESHOLD constant")
    void testSuspiciousThresholdConstant() {
      assertThat(ScoreCalculator.SUSPICIOUS_THRESHOLD).isEqualTo(30);
    }

    @Test
    @DisplayName("Should have correct FRAUD_THRESHOLD constant")
    void testFraudThresholdConstant() {
      assertThat(ScoreCalculator.FRAUD_THRESHOLD).isEqualTo(70);
    }

    @Test
    @DisplayName("Should have valid threshold ordering")
    void testThresholdOrdering() {
      assertThat(ScoreCalculator.MIN_SCORE)
          .isLessThan(ScoreCalculator.SUSPICIOUS_THRESHOLD)
          .isLessThan(ScoreCalculator.FRAUD_THRESHOLD)
          .isLessThan(ScoreCalculator.MAX_SCORE);
    }
  }

  @Nested
  @DisplayName("Integration Scenarios")
  class IntegrationScenarios {

    @Test
    @DisplayName("Low risk transaction: approved")
    void testLowRiskTransaction() {
      int score = calculator.calculateTotal(10, 5, 8); // Total: 23

      assertThat(score).isEqualTo(23);
      assertThat(calculator.classifyByScore(score)).isEqualTo(Classification.APPROVED);
      assertThat(calculator.shouldApprove(score)).isTrue();
      assertThat(calculator.shouldReview(score)).isFalse();
      assertThat(calculator.shouldBlock(score)).isFalse();
    }

    @Test
    @DisplayName("Medium risk transaction: manual review")
    void testMediumRiskTransaction() {
      int score = calculator.calculateTotal(20, 15, 10); // Total: 45

      assertThat(score).isEqualTo(45);
      assertThat(calculator.classifyByScore(score)).isEqualTo(Classification.SUSPICIOUS);
      assertThat(calculator.shouldApprove(score)).isFalse();
      assertThat(calculator.shouldReview(score)).isTrue();
      assertThat(calculator.shouldBlock(score)).isFalse();
    }

    @Test
    @DisplayName("High risk transaction: blocked")
    void testHighRiskTransaction() {
      int score = calculator.calculateTotal(40, 30, 25); // Total: 95

      assertThat(score).isEqualTo(95);
      assertThat(calculator.classifyByScore(score)).isEqualTo(Classification.FRAUD);
      assertThat(calculator.shouldApprove(score)).isFalse();
      assertThat(calculator.shouldReview(score)).isFalse();
      assertThat(calculator.shouldBlock(score)).isTrue();
    }

    @Test
    @DisplayName("Boundary case: exactly 30 (first suspicious score)")
    void testBoundaryCase30() {
      int score = calculator.calculateTotal(15, 15); // Total: 30

      assertThat(score).isEqualTo(30);
      assertThat(calculator.classifyByScore(score)).isEqualTo(Classification.SUSPICIOUS);
      assertThat(calculator.shouldApprove(score)).isFalse();
      assertThat(calculator.shouldReview(score)).isTrue();
      assertThat(calculator.shouldBlock(score)).isFalse();
    }

    @Test
    @DisplayName("Boundary case: exactly 70 (first fraud score)")
    void testBoundaryCase70() {
      int score = calculator.calculateTotal(35, 35); // Total: 70

      assertThat(score).isEqualTo(70);
      assertThat(calculator.classifyByScore(score)).isEqualTo(Classification.FRAUD);
      assertThat(calculator.shouldApprove(score)).isFalse();
      assertThat(calculator.shouldReview(score)).isFalse();
      assertThat(calculator.shouldBlock(score)).isTrue();
    }

    @ParameterizedTest
    @CsvSource({
      "10, 0.8, 8, APPROVED",
      "50, 1.0, 50, SUSPICIOUS",
      "80, 1.5, 100, FRAUD"
    })
    @DisplayName("Weighted contributions with multipliers")
    void testWeightedContributions(int weight, double multiplier, int expected, Classification expectedClass) {
      int contribution = calculator.calculateContribution(weight, multiplier);
      int score = calculator.calculateTotal(contribution);

      assertThat(score).isEqualTo(expected);
      assertThat(calculator.classifyByScore(score)).isEqualTo(expectedClass);
    }
  }
}
