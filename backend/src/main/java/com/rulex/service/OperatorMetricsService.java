package com.rulex.service;

import com.rulex.entity.complex.ConditionOperator;
import io.micrometer.core.instrument.Counter;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.Timer;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * OBS-004 FIX: Serviço de métricas por tipo de operador.
 *
 * <p>Expõe métricas Micrometer para:
 * - Contagem de avaliações por operador
 * - Tempo de execução por operador
 * - Taxa de match por operador
 * - Erros por operador
 *
 * <p>Métricas expostas em /actuator/prometheus:
 * - rulex_operator_evaluations_total{operator="...", category="..."}
 * - rulex_operator_duration_seconds{operator="...", category="..."}
 * - rulex_operator_matches_total{operator="...", category="..."}
 * - rulex_operator_errors_total{operator="...", category="..."}
 */
@Service
@Slf4j
public class OperatorMetricsService {

  private final MeterRegistry meterRegistry;

  // Cache de counters e timers para evitar criação repetida
  private final Map<String, Counter> evaluationCounters = new ConcurrentHashMap<>();
  private final Map<String, Counter> matchCounters = new ConcurrentHashMap<>();
  private final Map<String, Counter> errorCounters = new ConcurrentHashMap<>();
  private final Map<String, Timer> durationTimers = new ConcurrentHashMap<>();

  public OperatorMetricsService(MeterRegistry meterRegistry) {
    this.meterRegistry = meterRegistry;
    log.info("OBS-004: OperatorMetricsService initialized - metrics available at /actuator/prometheus");
  }

  /**
   * Registra uma avaliação de operador.
   *
   * @param operator O operador avaliado
   * @param matched Se o operador retornou match
   * @param durationNanos Duração da avaliação em nanosegundos
   */
  public void recordEvaluation(ConditionOperator operator, boolean matched, long durationNanos) {
    String operatorName = operator.name();
    String category = getOperatorCategory(operator);

    // Incrementar contador de avaliações
    getEvaluationCounter(operatorName, category).increment();

    // Incrementar contador de matches se aplicável
    if (matched) {
      getMatchCounter(operatorName, category).increment();
    }

    // Registrar duração
    getDurationTimer(operatorName, category).record(durationNanos, TimeUnit.NANOSECONDS);
  }

  /**
   * Registra um erro durante avaliação de operador.
   *
   * @param operator O operador que falhou
   * @param errorType Tipo do erro
   */
  public void recordError(ConditionOperator operator, String errorType) {
    String operatorName = operator.name();
    String category = getOperatorCategory(operator);

    getErrorCounter(operatorName, category, errorType).increment();
  }

  /**
   * Registra avaliação com medição automática de tempo.
   *
   * @param operator O operador a avaliar
   * @param evaluation Função de avaliação
   * @return Resultado da avaliação
   */
  public boolean recordTimed(ConditionOperator operator, java.util.function.Supplier<Boolean> evaluation) {
    long startNanos = System.nanoTime();
    boolean result;
    try {
      result = evaluation.get();
      recordEvaluation(operator, result, System.nanoTime() - startNanos);
      return result;
    } catch (Exception e) {
      recordError(operator, e.getClass().getSimpleName());
      throw e;
    }
  }

  private Counter getEvaluationCounter(String operator, String category) {
    String key = operator + "_eval";
    return evaluationCounters.computeIfAbsent(
        key,
        k ->
            Counter.builder("rulex.operator.evaluations")
                .description("Total de avaliações por operador")
                .tag("operator", operator)
                .tag("category", category)
                .register(meterRegistry));
  }

  private Counter getMatchCounter(String operator, String category) {
    String key = operator + "_match";
    return matchCounters.computeIfAbsent(
        key,
        k ->
            Counter.builder("rulex.operator.matches")
                .description("Total de matches por operador")
                .tag("operator", operator)
                .tag("category", category)
                .register(meterRegistry));
  }

  private Counter getErrorCounter(String operator, String category, String errorType) {
    String key = operator + "_error_" + errorType;
    return errorCounters.computeIfAbsent(
        key,
        k ->
            Counter.builder("rulex.operator.errors")
                .description("Total de erros por operador")
                .tag("operator", operator)
                .tag("category", category)
                .tag("error_type", errorType)
                .register(meterRegistry));
  }

  private Timer getDurationTimer(String operator, String category) {
    String key = operator + "_duration";
    return durationTimers.computeIfAbsent(
        key,
        k ->
            Timer.builder("rulex.operator.duration")
                .description("Duração de avaliação por operador")
                .tag("operator", operator)
                .tag("category", category)
                .publishPercentiles(0.5, 0.95, 0.99)
                .register(meterRegistry));
  }

  /**
   * Determina a categoria do operador baseado no prefixo.
   */
  private String getOperatorCategory(ConditionOperator operator) {
    String name = operator.name();

    if (name.startsWith("VELOCITY_")) return "VELOCITY";
    if (name.startsWith("NEO4J_")) return "NEO4J";
    if (name.startsWith("GEO_")) return "GEO";
    if (name.startsWith("TIME_")) return "TIME";
    if (name.startsWith("DEVICE_")) return "DEVICE";
    if (name.startsWith("MCC_")) return "MCC";
    if (name.startsWith("AMOUNT_")) return "AMOUNT";
    if (name.startsWith("CARD_")) return "CARD";
    if (name.startsWith("CUSTOMER_")) return "CUSTOMER";
    if (name.startsWith("MERCHANT_")) return "MERCHANT";
    if (name.startsWith("FATF_")) return "FATF";
    if (name.startsWith("PSD_")) return "PSD";
    if (name.startsWith("SCA_")) return "SCA";
    if (name.startsWith("PLT_")) return "PLT";

    // Operadores básicos
    if (name.equals("EQUALS") || name.equals("NOT_EQUALS") || name.equals("GREATER_THAN")
        || name.equals("LESS_THAN") || name.equals("IN") || name.equals("NOT_IN")
        || name.equals("CONTAINS") || name.equals("STARTS_WITH") || name.equals("ENDS_WITH")
        || name.equals("REGEX_MATCH") || name.equals("IS_NULL") || name.equals("IS_NOT_NULL")) {
      return "BASIC";
    }

    return "OTHER";
  }

  /**
   * Retorna estatísticas agregadas por categoria.
   */
  public Map<String, Map<String, Object>> getCategoryStats() {
    Map<String, Map<String, Object>> stats = new ConcurrentHashMap<>();

    // Agregar por categoria
    evaluationCounters.forEach(
        (key, counter) -> {
          String category =
              counter.getId().getTag("category") != null
                  ? counter.getId().getTag("category")
                  : "UNKNOWN";

          stats.computeIfAbsent(category, k -> new ConcurrentHashMap<>());
          Map<String, Object> catStats = stats.get(category);

          catStats.merge("evaluations", (long) counter.count(), (a, b) -> (Long) a + (Long) b);
        });

    matchCounters.forEach(
        (key, counter) -> {
          String category =
              counter.getId().getTag("category") != null
                  ? counter.getId().getTag("category")
                  : "UNKNOWN";

          stats.computeIfAbsent(category, k -> new ConcurrentHashMap<>());
          stats.get(category).merge("matches", (long) counter.count(), (a, b) -> (Long) a + (Long) b);
        });

    return stats;
  }
}
