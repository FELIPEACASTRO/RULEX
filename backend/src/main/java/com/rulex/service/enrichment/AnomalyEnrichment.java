package com.rulex.service.enrichment;

import com.rulex.dto.TransactionRequest;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.DayOfWeek;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Enriquecimento de detecção de anomalias para regras de fraude.
 *
 * <p>Consolida todos os campos de anomalia em um formato fácil de usar pelo motor de regras. Campos
 * disponíveis:
 *
 * <ul>
 *   <li>anomaly.unusual_hour - Se a hora é fora do padrão
 *   <li>anomaly.unusual_day - Se o dia da semana é fora do padrão
 *   <li>anomaly.value_deviation_ratio - Desvio do valor vs média
 *   <li>anomaly.behavior_change_score - Score de mudança de comportamento
 *   <li>anomaly.is_anomalous - Flag geral de anomalia
 *   <li>anomaly.is_night_transaction - Se é transação noturna
 *   <li>anomaly.is_weekend_transaction - Se é transação de fim de semana
 *   <li>anomaly.is_holiday - Se é feriado
 * </ul>
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class AnomalyEnrichment {

  // Horários considerados de alto risco (madrugada)
  private static final int NIGHT_START_HOUR = 0;
  private static final int NIGHT_END_HOUR = 6;

  // Horário comercial típico
  private static final int BUSINESS_START_HOUR = 8;
  private static final int BUSINESS_END_HOUR = 22;

  /** Resultado do enriquecimento de anomalia. */
  @Data
  @Builder
  public static class AnomalyContext {
    // Anomalias temporais
    private final boolean isUnusualHour;
    private final boolean isUnusualDay;
    private final boolean isNightTransaction;
    private final boolean isWeekendTransaction;
    private final boolean isHoliday;
    private final int transactionHour;
    private final String dayOfWeek;

    // Anomalias de valor
    private final double valueDeviationRatio;
    private final boolean isValueAnomaly;
    private final boolean isRoundAmount;
    private final boolean isJustBelowLimit;

    // Anomalias de comportamento
    private final int behaviorChangeScore;
    private final boolean isBehaviorAnomaly;
    private final boolean isNewMerchantCategory;
    private final boolean isNewLocation;
    private final boolean isNewChannel;

    // Anomalias de padrão
    private final boolean isSequentialAmount;
    private final boolean isRepeatedAmount;
    private final boolean isSplitTransaction;

    // Score geral
    private final int anomalyScore;
    private final boolean isAnomalous;

    /** Converte para Map para uso no evaluator. */
    public Map<String, Object> toMap() {
      Map<String, Object> map = new HashMap<>();

      // Temporais
      map.put("anomaly.unusual_hour", isUnusualHour);
      map.put("anomaly.unusual_day", isUnusualDay);
      map.put("anomaly.is_night_transaction", isNightTransaction);
      map.put("anomaly.is_weekend_transaction", isWeekendTransaction);
      map.put("anomaly.is_holiday", isHoliday);
      map.put("anomaly.transaction_hour", transactionHour);
      map.put("anomaly.day_of_week", dayOfWeek);

      // Aliases
      map.put("unusualHour", isUnusualHour);
      map.put("isNightTransaction", isNightTransaction);
      map.put("isWeekendTransaction", isWeekendTransaction);

      // Valor
      map.put("anomaly.value_deviation_ratio", valueDeviationRatio);
      map.put("anomaly.is_value_anomaly", isValueAnomaly);
      map.put("anomaly.is_round_amount", isRoundAmount);
      map.put("anomaly.is_just_below_limit", isJustBelowLimit);

      // Aliases
      map.put("valueDeviationRatio", valueDeviationRatio);
      map.put("isRoundAmount", isRoundAmount);

      // Comportamento
      map.put("anomaly.behavior_change_score", behaviorChangeScore);
      map.put("anomaly.is_behavior_anomaly", isBehaviorAnomaly);
      map.put("anomaly.is_new_merchant_category", isNewMerchantCategory);
      map.put("anomaly.is_new_location", isNewLocation);
      map.put("anomaly.is_new_channel", isNewChannel);

      // Aliases
      map.put("behaviorChangeScore", behaviorChangeScore);

      // Padrão
      map.put("anomaly.is_sequential_amount", isSequentialAmount);
      map.put("anomaly.is_repeated_amount", isRepeatedAmount);
      map.put("anomaly.is_split_transaction", isSplitTransaction);

      // Score
      map.put("anomaly.score", anomalyScore);
      map.put("anomaly.is_anomalous", isAnomalous);

      // Aliases
      map.put("anomalyScore", anomalyScore);
      map.put("isAnomalous", isAnomalous);

      return map;
    }

    public static AnomalyContext empty() {
      return AnomalyContext.builder()
          .isUnusualHour(false)
          .isUnusualDay(false)
          .isNightTransaction(false)
          .isWeekendTransaction(false)
          .isHoliday(false)
          .transactionHour(0)
          .dayOfWeek(null)
          .valueDeviationRatio(0.0)
          .isValueAnomaly(false)
          .isRoundAmount(false)
          .isJustBelowLimit(false)
          .behaviorChangeScore(0)
          .isBehaviorAnomaly(false)
          .isNewMerchantCategory(false)
          .isNewLocation(false)
          .isNewChannel(false)
          .isSequentialAmount(false)
          .isRepeatedAmount(false)
          .isSplitTransaction(false)
          .anomalyScore(0)
          .isAnomalous(false)
          .build();
    }
  }

  /**
   * Enriquece uma transação com detecção de anomalias.
   *
   * @param request A transação a ser enriquecida
   * @param customerProfile Perfil do cliente com histórico (opcional)
   * @param velocityContext Contexto de velocidade (opcional)
   * @return Contexto de anomalia com todos os campos calculados
   */
  public AnomalyContext enrich(
      TransactionRequest request,
      Map<String, Object> customerProfile,
      VelocityEnrichment.VelocityContext velocityContext) {

    if (request == null) {
      log.debug("TransactionRequest é null, retornando contexto vazio");
      return AnomalyContext.empty();
    }

    try {
      // Analisar anomalias temporais
      TimeAnalysis timeAnalysis = analyzeTime(request, customerProfile);

      // Analisar anomalias de valor
      ValueAnalysis valueAnalysis = analyzeValue(request, velocityContext);

      // Analisar anomalias de comportamento
      BehaviorAnalysis behaviorAnalysis = analyzeBehavior(request, customerProfile);

      // Analisar padrões suspeitos
      PatternAnalysis patternAnalysis = analyzePatterns(request);

      // Calcular score de anomalia
      int anomalyScore =
          calculateAnomalyScore(timeAnalysis, valueAnalysis, behaviorAnalysis, patternAnalysis);
      boolean isAnomalous = anomalyScore >= 50;

      return AnomalyContext.builder()
          .isUnusualHour(timeAnalysis.isUnusualHour)
          .isUnusualDay(timeAnalysis.isUnusualDay)
          .isNightTransaction(timeAnalysis.isNightTransaction)
          .isWeekendTransaction(timeAnalysis.isWeekendTransaction)
          .isHoliday(timeAnalysis.isHoliday)
          .transactionHour(timeAnalysis.hour)
          .dayOfWeek(timeAnalysis.dayOfWeek)
          .valueDeviationRatio(valueAnalysis.deviationRatio)
          .isValueAnomaly(valueAnalysis.isAnomaly)
          .isRoundAmount(valueAnalysis.isRoundAmount)
          .isJustBelowLimit(valueAnalysis.isJustBelowLimit)
          .behaviorChangeScore(behaviorAnalysis.changeScore)
          .isBehaviorAnomaly(behaviorAnalysis.isAnomaly)
          .isNewMerchantCategory(behaviorAnalysis.isNewMerchantCategory)
          .isNewLocation(behaviorAnalysis.isNewLocation)
          .isNewChannel(behaviorAnalysis.isNewChannel)
          .isSequentialAmount(patternAnalysis.isSequentialAmount)
          .isRepeatedAmount(patternAnalysis.isRepeatedAmount)
          .isSplitTransaction(patternAnalysis.isSplitTransaction)
          .anomalyScore(anomalyScore)
          .isAnomalous(isAnomalous)
          .build();

    } catch (Exception e) {
      log.warn("Erro ao enriquecer anomaly: {}", e.getMessage());
      return AnomalyContext.empty();
    }
  }

  /** Versão simplificada. */
  public AnomalyContext enrich(TransactionRequest request) {
    return enrich(request, new HashMap<>(), null);
  }

  /** Análise temporal. */
  private static class TimeAnalysis {
    boolean isUnusualHour = false;
    boolean isUnusualDay = false;
    boolean isNightTransaction = false;
    boolean isWeekendTransaction = false;
    boolean isHoliday = false;
    int hour = 0;
    String dayOfWeek = null;
  }

  /** Análise de valor. */
  private static class ValueAnalysis {
    double deviationRatio = 0.0;
    boolean isAnomaly = false;
    boolean isRoundAmount = false;
    boolean isJustBelowLimit = false;
  }

  /** Análise de comportamento. */
  private static class BehaviorAnalysis {
    int changeScore = 0;
    boolean isAnomaly = false;
    boolean isNewMerchantCategory = false;
    boolean isNewLocation = false;
    boolean isNewChannel = false;
  }

  /** Análise de padrões. */
  private static class PatternAnalysis {
    boolean isSequentialAmount = false;
    boolean isRepeatedAmount = false;
    boolean isSplitTransaction = false;
  }

  /** Analisa anomalias temporais. */
  private TimeAnalysis analyzeTime(
      TransactionRequest request, Map<String, Object> customerProfile) {
    TimeAnalysis analysis = new TimeAnalysis();

    Integer transactionTime = request.getTransactionTime();
    if (transactionTime == null) return analysis;

    // Extrair hora (formato HHMMSS)
    String timeStr = String.format("%06d", transactionTime);
    analysis.hour = Integer.parseInt(timeStr.substring(0, 2));

    // Verificar se é noturno
    analysis.isNightTransaction =
        analysis.hour >= NIGHT_START_HOUR && analysis.hour < NIGHT_END_HOUR;

    // Verificar se é fora do horário comercial
    analysis.isUnusualHour =
        analysis.hour < BUSINESS_START_HOUR || analysis.hour >= BUSINESS_END_HOUR;

    // Verificar dia da semana (se disponível na data)
    Integer transactionDate = request.getTransactionDate();
    if (transactionDate != null) {
      try {
        String dateStr = String.format("%08d", transactionDate);
        int year = Integer.parseInt(dateStr.substring(0, 4));
        int month = Integer.parseInt(dateStr.substring(4, 6));
        int day = Integer.parseInt(dateStr.substring(6, 8));

        java.time.LocalDate date = java.time.LocalDate.of(year, month, day);
        DayOfWeek dow = date.getDayOfWeek();
        analysis.dayOfWeek = dow.name();
        analysis.isWeekendTransaction = dow == DayOfWeek.SATURDAY || dow == DayOfWeek.SUNDAY;
      } catch (Exception e) {
        log.debug("Erro ao parsear data: {}", e.getMessage());
      }
    }

    // Verificar se hora é incomum para o cliente
    Set<Integer> usualHours = getUsualHours(customerProfile);
    if (!usualHours.isEmpty() && !usualHours.contains(analysis.hour)) {
      analysis.isUnusualHour = true;
    }

    return analysis;
  }

  /** Analisa anomalias de valor. */
  private ValueAnalysis analyzeValue(
      TransactionRequest request, VelocityEnrichment.VelocityContext velocityContext) {

    ValueAnalysis analysis = new ValueAnalysis();

    BigDecimal amount = request.getTransactionAmount();
    if (amount == null) return analysis;

    double amountValue = amount.doubleValue();

    // Verificar se é valor redondo
    analysis.isRoundAmount = isRoundAmount(amountValue);

    // Verificar se está logo abaixo de limites de reporte (CTR)
    analysis.isJustBelowLimit = isJustBelowLimit(amountValue);

    // Calcular desvio em relação à média
    if (velocityContext != null && velocityContext.getAvgAmount24h() != null) {
      BigDecimal avg = velocityContext.getAvgAmount24h();
      if (avg.compareTo(BigDecimal.ZERO) > 0) {
        analysis.deviationRatio = amount.divide(avg, 2, RoundingMode.HALF_UP).doubleValue();
        analysis.isAnomaly = analysis.deviationRatio > 3.0; // 3x a média
      }
    }

    return analysis;
  }

  /** Analisa anomalias de comportamento. */
  private BehaviorAnalysis analyzeBehavior(
      TransactionRequest request, Map<String, Object> customerProfile) {

    BehaviorAnalysis analysis = new BehaviorAnalysis();

    if (customerProfile == null || customerProfile.isEmpty()) {
      return analysis;
    }

    // Verificar se MCC é novo para o cliente
    @SuppressWarnings("unchecked")
    Set<Integer> usualMccs = (Set<Integer>) customerProfile.get("usualMccs");
    if (usualMccs != null && request.getMcc() != null) {
      analysis.isNewMerchantCategory = !usualMccs.contains(request.getMcc());
    }

    // Verificar se localização é nova
    @SuppressWarnings("unchecked")
    Set<String> usualCountries = (Set<String>) customerProfile.get("usualCountries");
    if (usualCountries != null && request.getMerchantCountryCode() != null) {
      analysis.isNewLocation = !usualCountries.contains(request.getMerchantCountryCode());
    }

    // Verificar se canal é novo
    @SuppressWarnings("unchecked")
    Set<String> usualChannels = (Set<String>) customerProfile.get("usualChannels");
    String channel = deriveChannel(request);
    if (usualChannels != null && channel != null) {
      analysis.isNewChannel = !usualChannels.contains(channel);
    }

    // Calcular score de mudança
    int score = 0;
    if (analysis.isNewMerchantCategory) score += 30;
    if (analysis.isNewLocation) score += 40;
    if (analysis.isNewChannel) score += 20;

    analysis.changeScore = score;
    analysis.isAnomaly = score >= 50;

    return analysis;
  }

  /** Analisa padrões suspeitos. */
  private PatternAnalysis analyzePatterns(TransactionRequest request) {
    PatternAnalysis analysis = new PatternAnalysis();

    BigDecimal amount = request.getTransactionAmount();
    if (amount == null) return analysis;

    double amountValue = amount.doubleValue();

    // Verificar se é valor sequencial (ex: 100, 200, 300)
    analysis.isSequentialAmount = isSequentialAmount(amountValue);

    // Verificar se parece split transaction (valores logo abaixo de limites)
    analysis.isSplitTransaction =
        amountValue > 9000 && amountValue < 10000; // Logo abaixo de R$ 10k

    return analysis;
  }

  /** Verifica se é valor redondo. */
  private boolean isRoundAmount(double amount) {
    // Valores múltiplos de 100 ou 500 ou 1000
    return amount % 1000 == 0 || amount % 500 == 0 || (amount >= 100 && amount % 100 == 0);
  }

  /** Verifica se está logo abaixo de limites de reporte. */
  private boolean isJustBelowLimit(double amount) {
    // Limites comuns: R$ 10.000 (CTR), R$ 50.000, R$ 100.000
    return (amount >= 9000 && amount < 10000)
        || (amount >= 49000 && amount < 50000)
        || (amount >= 99000 && amount < 100000);
  }

  /** Verifica se é valor sequencial. */
  private boolean isSequentialAmount(double amount) {
    // Valores como 1000, 2000, 3000, etc.
    return amount >= 1000 && amount % 1000 == 0 && amount <= 10000;
  }

  /** Deriva canal da transação. */
  private String deriveChannel(TransactionRequest request) {
    String posEntryMode = request.getPosEntryMode();
    if (posEntryMode == null) return "UNKNOWN";

    if (posEntryMode.startsWith("01") || posEntryMode.startsWith("81")) {
      return "ECOMMERCE";
    } else if (posEntryMode.startsWith("05") || posEntryMode.startsWith("07")) {
      return "CHIP";
    } else if (posEntryMode.startsWith("90") || posEntryMode.startsWith("02")) {
      return "MAGSTRIPE";
    } else if (posEntryMode.startsWith("91")) {
      return "CONTACTLESS";
    }
    return "OTHER";
  }

  /** Obtém horas usuais do perfil do cliente. */
  @SuppressWarnings("unchecked")
  private Set<Integer> getUsualHours(Map<String, Object> customerProfile) {
    if (customerProfile == null) return Set.of();
    Object hours = customerProfile.get("usualHours");
    if (hours instanceof Set) {
      return (Set<Integer>) hours;
    }
    return Set.of();
  }

  /** Calcula score de anomalia geral. */
  private int calculateAnomalyScore(
      TimeAnalysis time, ValueAnalysis value, BehaviorAnalysis behavior, PatternAnalysis pattern) {

    int score = 0;

    // Anomalias temporais
    if (time.isNightTransaction) score += 20;
    if (time.isUnusualHour) score += 15;
    if (time.isWeekendTransaction) score += 5;

    // Anomalias de valor
    if (value.isAnomaly) score += 25;
    if (value.isRoundAmount) score += 10;
    if (value.isJustBelowLimit) score += 20;

    // Anomalias de comportamento
    score += behavior.changeScore / 2;

    // Padrões suspeitos
    if (pattern.isSequentialAmount) score += 10;
    if (pattern.isSplitTransaction) score += 25;

    return Math.min(100, score);
  }
}
