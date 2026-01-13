package com.rulex.service;

import java.util.HashMap;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * Serviço de lógica fuzzy para detecção de fraude. Implementa os operadores FUZZY001-FUZZY015 para
 * análise baseada em conjuntos fuzzy.
 */
@Service
@Slf4j
public class FuzzyLogicService {

  /** FUZZY001: Membership function - Triangular */
  public double triangularMembership(double value, double a, double b, double c) {
    if (value <= a || value >= c) return 0.0;
    if (value == b) return 1.0;
    if (value < b) return (value - a) / (b - a);
    return (c - value) / (c - b);
  }

  /** FUZZY002: Membership function - Trapezoidal */
  public double trapezoidalMembership(double value, double a, double b, double c, double d) {
    if (value <= a || value >= d) return 0.0;
    if (value >= b && value <= c) return 1.0;
    if (value < b) return (value - a) / (b - a);
    return (d - value) / (d - c);
  }

  /** FUZZY003: Membership function - Gaussian */
  public double gaussianMembership(double value, double mean, double sigma) {
    return Math.exp(-Math.pow(value - mean, 2) / (2 * sigma * sigma));
  }

  /** FUZZY004: Membership function - Sigmoid */
  public double sigmoidMembership(double value, double a, double c) {
    return 1.0 / (1.0 + Math.exp(-a * (value - c)));
  }

  /** FUZZY005: Fuzzy AND (minimum) */
  public double fuzzyAnd(double... memberships) {
    double min = 1.0;
    for (double m : memberships) {
      min = Math.min(min, m);
    }
    return min;
  }

  /** FUZZY006: Fuzzy OR (maximum) */
  public double fuzzyOr(double... memberships) {
    double max = 0.0;
    for (double m : memberships) {
      max = Math.max(max, m);
    }
    return max;
  }

  /** FUZZY007: Fuzzy NOT */
  public double fuzzyNot(double membership) {
    return 1.0 - membership;
  }

  /** FUZZY008: Defuzzification - Centroid method */
  public double defuzzifyCentroid(double[] values, double[] memberships) {
    if (values.length != memberships.length || values.length == 0) return 0.0;

    double numerator = 0.0;
    double denominator = 0.0;

    for (int i = 0; i < values.length; i++) {
      numerator += values[i] * memberships[i];
      denominator += memberships[i];
    }

    return denominator == 0 ? 0.0 : numerator / denominator;
  }

  /** FUZZY009: Defuzzification - Mean of Maximum */
  public double defuzzifyMeanOfMax(double[] values, double[] memberships) {
    if (values.length != memberships.length || values.length == 0) return 0.0;

    double maxMembership = 0.0;
    for (double m : memberships) {
      maxMembership = Math.max(maxMembership, m);
    }

    double sum = 0.0;
    int count = 0;
    for (int i = 0; i < memberships.length; i++) {
      if (memberships[i] == maxMembership) {
        sum += values[i];
        count++;
      }
    }

    return count == 0 ? 0.0 : sum / count;
  }

  /** FUZZY010: Fuzzy fraud risk assessment */
  public FuzzyFraudAssessment assessFraudRisk(
      double transactionAmount,
      double velocityCount,
      double distanceFromHome,
      double timeOfDayHour) {

    // Fuzzify transaction amount
    double amountLow = trapezoidalMembership(transactionAmount, 0, 0, 100, 500);
    double amountMedium = triangularMembership(transactionAmount, 100, 500, 2000);
    double amountHigh = trapezoidalMembership(transactionAmount, 1000, 5000, 100000, 100000);

    // Fuzzify velocity
    double velocityLow = trapezoidalMembership(velocityCount, 0, 0, 2, 5);
    double velocityMedium = triangularMembership(velocityCount, 3, 7, 15);
    double velocityHigh = trapezoidalMembership(velocityCount, 10, 20, 100, 100);

    // Fuzzify distance
    double distanceNear = trapezoidalMembership(distanceFromHome, 0, 0, 10, 50);
    double distanceMedium = triangularMembership(distanceFromHome, 20, 100, 500);
    double distanceFar = trapezoidalMembership(distanceFromHome, 200, 1000, 20000, 20000);

    // Fuzzify time (unusual hours: 0-6, 22-24)
    double timeNormal = trapezoidalMembership(timeOfDayHour, 6, 8, 20, 22);
    double timeUnusual =
        fuzzyOr(
            trapezoidalMembership(timeOfDayHour, 0, 0, 4, 6),
            trapezoidalMembership(timeOfDayHour, 22, 23, 24, 24));

    // Apply fuzzy rules
    double riskVeryLow = fuzzyAnd(amountLow, velocityLow, distanceNear, timeNormal);
    double riskLow = fuzzyAnd(amountMedium, velocityLow, distanceNear);
    double riskMedium =
        fuzzyOr(
            fuzzyAnd(amountMedium, velocityMedium),
            fuzzyAnd(amountLow, distanceMedium),
            fuzzyAnd(amountLow, timeUnusual));
    double riskHigh =
        fuzzyOr(
            fuzzyAnd(amountHigh, velocityMedium),
            fuzzyAnd(amountMedium, velocityHigh),
            fuzzyAnd(distanceFar, velocityMedium));
    double riskVeryHigh =
        fuzzyOr(
            fuzzyAnd(amountHigh, velocityHigh),
            fuzzyAnd(amountHigh, distanceFar, timeUnusual),
            fuzzyAnd(velocityHigh, distanceFar));

    // Defuzzify to get final risk score
    double[] riskLevels = {0, 25, 50, 75, 100};
    double[] memberships = {riskVeryLow, riskLow, riskMedium, riskHigh, riskVeryHigh};
    double riskScore = defuzzifyCentroid(riskLevels, memberships);

    Map<String, Double> inputMemberships = new HashMap<>();
    inputMemberships.put("amountLow", amountLow);
    inputMemberships.put("amountMedium", amountMedium);
    inputMemberships.put("amountHigh", amountHigh);
    inputMemberships.put("velocityLow", velocityLow);
    inputMemberships.put("velocityMedium", velocityMedium);
    inputMemberships.put("velocityHigh", velocityHigh);
    inputMemberships.put("distanceNear", distanceNear);
    inputMemberships.put("distanceMedium", distanceMedium);
    inputMemberships.put("distanceFar", distanceFar);
    inputMemberships.put("timeNormal", timeNormal);
    inputMemberships.put("timeUnusual", timeUnusual);

    Map<String, Double> outputMemberships = new HashMap<>();
    outputMemberships.put("riskVeryLow", riskVeryLow);
    outputMemberships.put("riskLow", riskLow);
    outputMemberships.put("riskMedium", riskMedium);
    outputMemberships.put("riskHigh", riskHigh);
    outputMemberships.put("riskVeryHigh", riskVeryHigh);

    return new FuzzyFraudAssessment(riskScore, inputMemberships, outputMemberships);
  }

  /** FUZZY011: Adaptive threshold based on fuzzy membership */
  public double calculateAdaptiveThreshold(
      double baseThreshold, double customerRiskScore, double merchantRiskScore) {

    // Customer risk membership
    double customerLowRisk = trapezoidalMembership(customerRiskScore, 0, 0, 20, 40);
    double customerMediumRisk = triangularMembership(customerRiskScore, 30, 50, 70);
    double customerHighRisk = trapezoidalMembership(customerRiskScore, 60, 80, 100, 100);

    // Merchant risk membership
    double merchantLowRisk = trapezoidalMembership(merchantRiskScore, 0, 0, 20, 40);
    double merchantHighRisk = trapezoidalMembership(merchantRiskScore, 60, 80, 100, 100);

    // Adjust threshold based on risk
    double adjustment = 0.0;

    // Lower threshold (more strict) for high risk
    adjustment -= 0.2 * baseThreshold * customerHighRisk;
    adjustment -= 0.15 * baseThreshold * merchantHighRisk;

    // Raise threshold (more lenient) for low risk
    adjustment += 0.1 * baseThreshold * fuzzyAnd(customerLowRisk, merchantLowRisk);

    return Math.max(0, Math.min(baseThreshold * 2, baseThreshold + adjustment));
  }

  /** FUZZY012: Fuzzy pattern matching */
  public double fuzzyPatternMatch(double[] pattern, double[] observed) {
    if (pattern.length != observed.length || pattern.length == 0) return 0.0;

    double similarity = 0.0;
    for (int i = 0; i < pattern.length; i++) {
      double diff = Math.abs(pattern[i] - observed[i]);
      double maxVal = Math.max(Math.abs(pattern[i]), Math.abs(observed[i]));
      if (maxVal > 0) {
        similarity += 1.0 - (diff / maxVal);
      } else {
        similarity += 1.0;
      }
    }

    return similarity / pattern.length;
  }

  /** FUZZY013: Fuzzy time decay */
  public double fuzzyTimeDecay(double hoursAgo, double halfLife) {
    return Math.exp(-0.693 * hoursAgo / halfLife);
  }

  /** FUZZY014: Fuzzy aggregation with weights */
  public double fuzzyWeightedAggregation(double[] values, double[] weights) {
    if (values.length != weights.length || values.length == 0) return 0.0;

    double weightedSum = 0.0;
    double totalWeight = 0.0;

    for (int i = 0; i < values.length; i++) {
      weightedSum += values[i] * weights[i];
      totalWeight += weights[i];
    }

    return totalWeight == 0 ? 0.0 : weightedSum / totalWeight;
  }

  /** FUZZY015: Fuzzy inference system for fraud detection */
  public double fuzzyInference(Map<String, Double> inputs, Map<String, FuzzyRule> rules) {
    double totalWeight = 0.0;
    double weightedOutput = 0.0;

    for (FuzzyRule rule : rules.values()) {
      double ruleStrength = rule.evaluate(inputs);
      weightedOutput += ruleStrength * rule.getConsequent();
      totalWeight += ruleStrength;
    }

    return totalWeight == 0 ? 0.0 : weightedOutput / totalWeight;
  }

  /** Resultado da avaliação fuzzy de fraude */
  public record FuzzyFraudAssessment(
      double riskScore,
      Map<String, Double> inputMemberships,
      Map<String, Double> outputMemberships) {}

  /** Regra fuzzy */
  public static class FuzzyRule {
    private final String[] antecedents;
    private final double[] thresholds;
    private final double consequent;

    public FuzzyRule(String[] antecedents, double[] thresholds, double consequent) {
      this.antecedents = antecedents;
      this.thresholds = thresholds;
      this.consequent = consequent;
    }

    public double evaluate(Map<String, Double> inputs) {
      double minMembership = 1.0;
      for (int i = 0; i < antecedents.length; i++) {
        Double value = inputs.get(antecedents[i]);
        if (value == null) return 0.0;
        minMembership =
            Math.min(minMembership, value >= thresholds[i] ? 1.0 : value / thresholds[i]);
      }
      return minMembership;
    }

    public double getConsequent() {
      return consequent;
    }
  }
}
