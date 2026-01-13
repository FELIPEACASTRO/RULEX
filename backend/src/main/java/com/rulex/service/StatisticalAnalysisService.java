package com.rulex.service;

import java.util.Arrays;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.math3.distribution.NormalDistribution;
import org.apache.commons.math3.distribution.TDistribution;
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics;
import org.apache.commons.math3.stat.inference.ChiSquareTest;
import org.apache.commons.math3.stat.inference.KolmogorovSmirnovTest;
import org.apache.commons.math3.stat.inference.MannWhitneyUTest;
import org.apache.commons.math3.stat.inference.TTest;
import org.springframework.stereotype.Service;

/**
 * Serviço de análise estatística para detecção de fraude. Implementa os operadores STAT001-STAT015
 * para testes estatísticos avançados.
 */
@Service
@Slf4j
public class StatisticalAnalysisService {

  private final ChiSquareTest chiSquareTest = new ChiSquareTest();
  private final KolmogorovSmirnovTest ksTest = new KolmogorovSmirnovTest();
  private final TTest tTest = new TTest();
  private final MannWhitneyUTest mannWhitneyTest = new MannWhitneyUTest();

  /** STAT001: Z-Score - Calcula o Z-score de um valor em relação a uma distribuição */
  public double calculateZScore(double value, double mean, double stdDev) {
    if (stdDev == 0) return 0;
    return (value - mean) / stdDev;
  }

  /** STAT001: Z-Score para array de dados */
  public double calculateZScore(double value, double[] data) {
    DescriptiveStatistics stats = new DescriptiveStatistics(data);
    return calculateZScore(value, stats.getMean(), stats.getStandardDeviation());
  }

  /** STAT002: Modified Z-Score (mais robusto a outliers) */
  public double calculateModifiedZScore(double value, double[] data) {
    double median = calculateMedian(data);
    double mad = calculateMAD(data);
    if (mad == 0) return 0;
    return 0.6745 * (value - median) / mad;
  }

  /** STAT003: Chi-Square Test - Teste de aderência */
  public double chiSquareTest(long[] observed, double[] expected) {
    try {
      return chiSquareTest.chiSquareTest(expected, observed);
    } catch (Exception e) {
      log.debug("Chi-square test failed: {}", e.getMessage());
      return 1.0; // p-value alto = não significativo
    }
  }

  /** STAT003: Verifica se distribuição segue Benford's Law */
  public boolean followsBenfordsLaw(long[] digitCounts, double significanceLevel) {
    // Frequências esperadas de Benford
    double[] benfordExpected = {0.301, 0.176, 0.125, 0.097, 0.079, 0.067, 0.058, 0.051, 0.046};

    long total = Arrays.stream(digitCounts).sum();
    double[] expected = new double[9];
    for (int i = 0; i < 9; i++) {
      expected[i] = benfordExpected[i] * total;
    }

    double pValue = chiSquareTest(digitCounts, expected);
    return pValue > significanceLevel; // Se p > alpha, segue Benford
  }

  /** STAT004: Kolmogorov-Smirnov Test - Teste de distribuição */
  public double kolmogorovSmirnovTest(double[] sample1, double[] sample2) {
    try {
      return ksTest.kolmogorovSmirnovTest(sample1, sample2);
    } catch (Exception e) {
      log.debug("KS test failed: {}", e.getMessage());
      return 1.0;
    }
  }

  /** STAT004: KS Test contra distribuição normal */
  public double kolmogorovSmirnovTestNormal(double[] sample) {
    DescriptiveStatistics stats = new DescriptiveStatistics(sample);
    NormalDistribution normal =
        new NormalDistribution(stats.getMean(), stats.getStandardDeviation());

    try {
      return ksTest.kolmogorovSmirnovTest(normal, sample);
    } catch (Exception e) {
      log.debug("KS normal test failed: {}", e.getMessage());
      return 1.0;
    }
  }

  /** STAT005: Anderson-Darling Test (aproximação) */
  public double andersonDarlingStatistic(double[] sample) {
    Arrays.sort(sample);
    int n = sample.length;
    DescriptiveStatistics stats = new DescriptiveStatistics(sample);
    NormalDistribution normal =
        new NormalDistribution(stats.getMean(), stats.getStandardDeviation());

    double sum = 0;
    for (int i = 0; i < n; i++) {
      double cdf_i = normal.cumulativeProbability(sample[i]);
      double cdf_ni = normal.cumulativeProbability(sample[n - 1 - i]);
      sum += (2 * (i + 1) - 1) * (Math.log(cdf_i) + Math.log(1 - cdf_ni));
    }

    return -n - sum / n;
  }

  /** STAT006: T-Test - Teste t de Student */
  public double tTestOneSample(double[] sample, double hypothesizedMean) {
    try {
      return tTest.tTest(hypothesizedMean, sample);
    } catch (Exception e) {
      log.debug("T-test failed: {}", e.getMessage());
      return 1.0;
    }
  }

  /** STAT006: T-Test para duas amostras */
  public double tTestTwoSample(double[] sample1, double[] sample2) {
    try {
      return tTest.tTest(sample1, sample2);
    } catch (Exception e) {
      log.debug("Two-sample t-test failed: {}", e.getMessage());
      return 1.0;
    }
  }

  /** STAT007: Mann-Whitney U Test (não paramétrico) */
  public double mannWhitneyUTest(double[] sample1, double[] sample2) {
    try {
      return mannWhitneyTest.mannWhitneyUTest(sample1, sample2);
    } catch (Exception e) {
      log.debug("Mann-Whitney test failed: {}", e.getMessage());
      return 1.0;
    }
  }

  /** STAT008: Gaussian Mixture Model Probability (simplificado) */
  public double gaussianMixtureProbability(
      double value, double[] means, double[] stdDevs, double[] weights) {
    if (means.length != stdDevs.length || means.length != weights.length) {
      return 0.0;
    }

    double probability = 0.0;
    for (int i = 0; i < means.length; i++) {
      NormalDistribution normal = new NormalDistribution(means[i], stdDevs[i]);
      probability += weights[i] * normal.density(value);
    }
    return probability;
  }

  /** STAT009: Mahalanobis Distance (simplificado para 1D) */
  public double mahalanobisDistance(double value, double mean, double variance) {
    if (variance == 0) return 0;
    return Math.sqrt(Math.pow(value - mean, 2) / variance);
  }

  /** STAT010: Grubbs Test for Outliers */
  public boolean isOutlierGrubbs(double value, double[] data, double significanceLevel) {
    DescriptiveStatistics stats = new DescriptiveStatistics(data);
    double mean = stats.getMean();
    double stdDev = stats.getStandardDeviation();
    int n = data.length;

    if (stdDev == 0) return false;

    double g = Math.abs(value - mean) / stdDev;

    // Valor crítico de Grubbs (aproximação)
    TDistribution tDist = new TDistribution(n - 2);
    double tCritical = tDist.inverseCumulativeProbability(1 - significanceLevel / (2 * n));
    double gCritical =
        ((n - 1) / Math.sqrt(n))
            * Math.sqrt(tCritical * tCritical / (n - 2 + tCritical * tCritical));

    return g > gCritical;
  }

  /** STAT011: Dixon's Q Test for Outliers */
  public boolean isOutlierDixon(double value, double[] data) {
    if (data.length < 3) return false;

    double[] sorted = data.clone();
    Arrays.sort(sorted);

    double range = sorted[sorted.length - 1] - sorted[0];
    if (range == 0) return false;

    // Q para o valor mais baixo ou mais alto
    double qLow = (sorted[1] - sorted[0]) / range;
    double qHigh = (sorted[sorted.length - 1] - sorted[sorted.length - 2]) / range;

    // Valores críticos de Dixon (n=3-10, alpha=0.05)
    double qCritical = 0.941; // Para n=3

    if (value == sorted[0]) return qLow > qCritical;
    if (value == sorted[sorted.length - 1]) return qHigh > qCritical;

    return false;
  }

  /** STAT012: Shapiro-Wilk Test (aproximação) */
  public boolean isNormallyDistributed(double[] data, double significanceLevel) {
    // Usando KS test como aproximação
    double pValue = kolmogorovSmirnovTestNormal(data);
    return pValue > significanceLevel;
  }

  /** STAT013: Levene's Test for Homogeneity of Variance (simplificado) */
  public double leveneTest(double[] group1, double[] group2) {
    double mean1 = calculateMean(group1);
    double mean2 = calculateMean(group2);

    double[] deviations1 = new double[group1.length];
    double[] deviations2 = new double[group2.length];

    for (int i = 0; i < group1.length; i++) {
      deviations1[i] = Math.abs(group1[i] - mean1);
    }
    for (int i = 0; i < group2.length; i++) {
      deviations2[i] = Math.abs(group2[i] - mean2);
    }

    return tTestTwoSample(deviations1, deviations2);
  }

  /** STAT014: Welch's T-Test (variâncias desiguais) */
  public double welchTTest(double[] sample1, double[] sample2) {
    DescriptiveStatistics stats1 = new DescriptiveStatistics(sample1);
    DescriptiveStatistics stats2 = new DescriptiveStatistics(sample2);

    double mean1 = stats1.getMean();
    double mean2 = stats2.getMean();
    double var1 = stats1.getVariance();
    double var2 = stats2.getVariance();
    int n1 = sample1.length;
    int n2 = sample2.length;

    double t = (mean1 - mean2) / Math.sqrt(var1 / n1 + var2 / n2);

    // Graus de liberdade de Welch-Satterthwaite
    double df =
        Math.pow(var1 / n1 + var2 / n2, 2)
            / (Math.pow(var1 / n1, 2) / (n1 - 1) + Math.pow(var2 / n2, 2) / (n2 - 1));

    TDistribution tDist = new TDistribution(df);
    return 2 * (1 - tDist.cumulativeProbability(Math.abs(t)));
  }

  /** STAT015: Bootstrap Confidence Interval */
  public double[] bootstrapConfidenceInterval(
      double[] data, int numBootstraps, double confidenceLevel) {
    double[] bootstrapMeans = new double[numBootstraps];
    java.util.Random random = new java.util.Random();

    for (int i = 0; i < numBootstraps; i++) {
      double[] sample = new double[data.length];
      for (int j = 0; j < data.length; j++) {
        sample[j] = data[random.nextInt(data.length)];
      }
      bootstrapMeans[i] = calculateMean(sample);
    }

    Arrays.sort(bootstrapMeans);
    double alpha = 1 - confidenceLevel;
    int lowerIndex = (int) (alpha / 2 * numBootstraps);
    int upperIndex = (int) ((1 - alpha / 2) * numBootstraps);

    return new double[] {bootstrapMeans[lowerIndex], bootstrapMeans[upperIndex]};
  }

  // Métodos auxiliares

  private double calculateMean(double[] data) {
    return Arrays.stream(data).average().orElse(0);
  }

  private double calculateMedian(double[] data) {
    double[] sorted = data.clone();
    Arrays.sort(sorted);
    int n = sorted.length;
    if (n % 2 == 0) {
      return (sorted[n / 2 - 1] + sorted[n / 2]) / 2;
    }
    return sorted[n / 2];
  }

  private double calculateMAD(double[] data) {
    double median = calculateMedian(data);
    double[] deviations = new double[data.length];
    for (int i = 0; i < data.length; i++) {
      deviations[i] = Math.abs(data[i] - median);
    }
    return calculateMedian(deviations);
  }

  /** Calcula IQR (Interquartile Range) */
  public double calculateIQR(double[] data) {
    double[] sorted = data.clone();
    Arrays.sort(sorted);
    int n = sorted.length;
    double q1 = sorted[n / 4];
    double q3 = sorted[3 * n / 4];
    return q3 - q1;
  }

  /** Verifica se valor é outlier pelo método IQR */
  public boolean isOutlierIQR(double value, double[] data, double multiplier) {
    double[] sorted = data.clone();
    Arrays.sort(sorted);
    int n = sorted.length;
    double q1 = sorted[n / 4];
    double q3 = sorted[3 * n / 4];
    double iqr = q3 - q1;

    double lowerBound = q1 - multiplier * iqr;
    double upperBound = q3 + multiplier * iqr;

    return value < lowerBound || value > upperBound;
  }
}
