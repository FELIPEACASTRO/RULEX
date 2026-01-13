package com.rulex.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.rulex.dto.TransactionRequest;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

/**
 * Testes para o VelocityServiceFacade.
 *
 * <p>GAP-FIX #3: Aumentar cobertura de testes para classes críticas.
 *
 * <p>O VelocityServiceFacade é responsável por escolher entre o cache em memória
 * (RedisVelocityService) e o banco de dados (VelocityService) para consultas de velocidade.
 */
class VelocityServiceFacadeTest {

  private VelocityService velocityService;
  private RedisVelocityService redisVelocityService;
  private VelocityServiceFacade facade;

  @BeforeEach
  void setUp() {
    velocityService = Mockito.mock(VelocityService.class);
    redisVelocityService = Mockito.mock(RedisVelocityService.class);
    // RedisVelocityCacheService é opcional (null quando Redis não está disponível)
    facade = new VelocityServiceFacade(velocityService, redisVelocityService, null);
  }

  private void setRedisEnabled(boolean enabled) throws Exception {
    Field field = VelocityServiceFacade.class.getDeclaredField("redisEnabled");
    field.setAccessible(true);
    field.set(facade, enabled);
  }

  @Nested
  @DisplayName("Quando Redis está desabilitado")
  class WhenRedisDisabled {

    @BeforeEach
    void setUp() throws Exception {
      setRedisEnabled(false);
    }

    @Test
    @DisplayName("Deve usar VelocityService do banco de dados")
    void shouldUseVelocityService() {
      TransactionRequest request = createRequest();
      VelocityService.VelocityStats dbStats = createDbStats(5, "100.00");

      when(velocityService.getStats(
              any(TransactionRequest.class),
              any(VelocityService.KeyType.class),
              any(VelocityService.TimeWindow.class)))
          .thenReturn(dbStats);

      VelocityService.VelocityStats result =
          facade.getStats(request, VelocityService.KeyType.PAN, VelocityService.TimeWindow.HOUR_1);

      assertThat(result.getTransactionCount()).isEqualTo(5);
      assertThat(result.getTotalAmount()).isEqualByComparingTo("100.00");
      verify(velocityService)
          .getStats(request, VelocityService.KeyType.PAN, VelocityService.TimeWindow.HOUR_1);
      verify(redisVelocityService, never()).getStats(any(), any(), any());
    }

    @Test
    @DisplayName("Deve retornar status DATABASE_ONLY")
    void shouldReturnDatabaseOnlyStatus() {
      assertThat(facade.getCacheStatus()).isEqualTo("DATABASE_ONLY");
    }

    @Test
    @DisplayName("isRedisEnabled deve retornar false")
    void shouldReturnRedisDisabled() {
      assertThat(facade.isRedisEnabled()).isFalse();
    }
  }

  @Nested
  @DisplayName("Quando Redis está habilitado")
  class WhenRedisEnabled {

    @BeforeEach
    void setUp() throws Exception {
      setRedisEnabled(true);
    }

    @Test
    @DisplayName("Deve usar RedisVelocityService para cache")
    void shouldUseRedisVelocityService() {
      TransactionRequest request = createRequest();
      RedisVelocityService.VelocityStats redisStats = createRedisStats(10, "500.00");

      when(redisVelocityService.getStats(
              any(TransactionRequest.class),
              any(RedisVelocityService.KeyType.class),
              any(RedisVelocityService.TimeWindow.class)))
          .thenReturn(redisStats);

      VelocityService.VelocityStats result =
          facade.getStats(request, VelocityService.KeyType.PAN, VelocityService.TimeWindow.HOUR_1);

      assertThat(result.getTransactionCount()).isEqualTo(10);
      assertThat(result.getTotalAmount()).isEqualByComparingTo("500.00");
      assertThat(result.getSource()).isEqualTo("MEMORY_CACHE");
      verify(redisVelocityService).getStats(any(), any(), any());
      verify(velocityService, never()).getStats(any(), any(), any());
    }

    @Test
    @DisplayName("Deve fazer fallback para banco quando Redis falha")
    void shouldFallbackToDatabaseOnRedisError() {
      TransactionRequest request = createRequest();
      VelocityService.VelocityStats dbStats = createDbStats(3, "75.00");

      when(redisVelocityService.getStats(any(), any(), any()))
          .thenThrow(new RuntimeException("Redis error"));
      when(velocityService.getStats(
              any(TransactionRequest.class),
              any(VelocityService.KeyType.class),
              any(VelocityService.TimeWindow.class)))
          .thenReturn(dbStats);

      VelocityService.VelocityStats result =
          facade.getStats(request, VelocityService.KeyType.PAN, VelocityService.TimeWindow.HOUR_1);

      assertThat(result.getTransactionCount()).isEqualTo(3);
      verify(velocityService)
          .getStats(request, VelocityService.KeyType.PAN, VelocityService.TimeWindow.HOUR_1);
    }

    @Test
    @DisplayName("Deve retornar status MEMORY_CACHE_ENABLED quando Redis real não disponível")
    void shouldReturnMemoryCacheEnabledStatus() {
      // Sem RedisVelocityCacheService, usa cache em memória
      assertThat(facade.getCacheStatus()).isEqualTo("MEMORY_CACHE_ENABLED");
    }

    @Test
    @DisplayName("isRedisEnabled deve retornar true")
    void shouldReturnRedisEnabled() {
      assertThat(facade.isRedisEnabled()).isTrue();
    }
  }

  @Nested
  @DisplayName("Métodos de conveniência")
  class ConvenienceMethods {

    @BeforeEach
    void setUp() throws Exception {
      setRedisEnabled(false);
    }

    @Test
    @DisplayName("countPanTransactionsInHours deve retornar contagem correta")
    void shouldCountPanTransactions() {
      TransactionRequest request = createRequest();
      VelocityService.VelocityStats stats = createDbStats(15, "1000.00");

      when(velocityService.getStats(any(), any(), any())).thenReturn(stats);

      long count = facade.countPanTransactionsInHours(request, 24);

      assertThat(count).isEqualTo(15);
    }

    @Test
    @DisplayName("sumPanAmountInHours deve retornar soma correta")
    void shouldSumPanAmount() {
      TransactionRequest request = createRequest();
      VelocityService.VelocityStats stats = createDbStats(5, "2500.00");

      when(velocityService.getStats(any(), any(), any())).thenReturn(stats);

      BigDecimal sum = facade.sumPanAmountInHours(request, 24);

      assertThat(sum).isEqualByComparingTo("2500.00");
    }

    @Test
    @DisplayName("countDistinctMerchantsIn24h deve retornar contagem de merchants")
    void shouldCountDistinctMerchants() {
      TransactionRequest request = createRequest();
      VelocityService.VelocityStats stats =
          VelocityService.VelocityStats.builder()
              .transactionCount(10)
              .totalAmount(new BigDecimal("1000.00"))
              .avgAmount(new BigDecimal("100.00"))
              .distinctMerchants(5)
              .distinctMccs(3)
              .distinctCountries(2)
              .found(true)
              .source("DB")
              .build();

      when(velocityService.getStats(any(), any(), any())).thenReturn(stats);

      long distinctMerchants = facade.countDistinctMerchantsIn24h(request);

      assertThat(distinctMerchants).isEqualTo(5);
    }

    @Test
    @DisplayName("isBurst deve detectar burst de transações")
    void shouldDetectBurst() {
      TransactionRequest request = createRequest();
      VelocityService.VelocityStats burstStats = createDbStats(10, "500.00");

      when(velocityService.getStats(any(), any(), any())).thenReturn(burstStats);

      boolean isBurst = facade.isBurst(request, 5, 5);

      assertThat(isBurst).isTrue();
    }

    @Test
    @DisplayName("isBurst deve retornar false quando não há burst")
    void shouldNotDetectBurstWhenBelowThreshold() {
      TransactionRequest request = createRequest();
      VelocityService.VelocityStats normalStats = createDbStats(2, "100.00");

      when(velocityService.getStats(any(), any(), any())).thenReturn(normalStats);

      boolean isBurst = facade.isBurst(request, 5, 5);

      assertThat(isBurst).isFalse();
    }
  }

  @Nested
  @DisplayName("Agregações")
  class Aggregations {

    @BeforeEach
    void setUp() throws Exception {
      setRedisEnabled(false);
    }

    @Test
    @DisplayName("getAggregation COUNT deve retornar contagem")
    void shouldReturnCount() {
      TransactionRequest request = createRequest();
      VelocityService.VelocityStats stats = createDbStats(25, "1000.00");

      when(velocityService.getStats(any(), any(), any())).thenReturn(stats);

      BigDecimal count =
          facade.getAggregation(
              request,
              VelocityService.KeyType.PAN,
              VelocityService.TimeWindow.HOUR_24,
              VelocityService.AggregationType.COUNT);

      assertThat(count).isEqualByComparingTo("25");
    }

    @Test
    @DisplayName("getAggregation SUM deve retornar soma")
    void shouldReturnSum() {
      TransactionRequest request = createRequest();
      VelocityService.VelocityStats stats = createDbStats(10, "5000.00");

      when(velocityService.getStats(any(), any(), any())).thenReturn(stats);

      BigDecimal sum =
          facade.getAggregation(
              request,
              VelocityService.KeyType.PAN,
              VelocityService.TimeWindow.HOUR_24,
              VelocityService.AggregationType.SUM);

      assertThat(sum).isEqualByComparingTo("5000.00");
    }

    @Test
    @DisplayName("getAggregation AVG deve retornar média")
    void shouldReturnAvg() {
      TransactionRequest request = createRequest();
      VelocityService.VelocityStats stats =
          VelocityService.VelocityStats.builder()
              .transactionCount(10)
              .totalAmount(new BigDecimal("1000.00"))
              .avgAmount(new BigDecimal("100.00"))
              .found(true)
              .source("DB")
              .build();

      when(velocityService.getStats(any(), any(), any())).thenReturn(stats);

      BigDecimal avg =
          facade.getAggregation(
              request,
              VelocityService.KeyType.PAN,
              VelocityService.TimeWindow.HOUR_24,
              VelocityService.AggregationType.AVG);

      assertThat(avg).isEqualByComparingTo("100.00");
    }

    @Test
    @DisplayName("getAggregation DISTINCT_MERCHANTS deve retornar contagem distinta")
    void shouldReturnDistinctMerchants() {
      TransactionRequest request = createRequest();
      VelocityService.VelocityStats stats =
          VelocityService.VelocityStats.builder()
              .transactionCount(10)
              .totalAmount(new BigDecimal("1000.00"))
              .avgAmount(new BigDecimal("100.00"))
              .distinctMerchants(7)
              .found(true)
              .source("DB")
              .build();

      when(velocityService.getStats(any(), any(), any())).thenReturn(stats);

      BigDecimal distinctMerchants =
          facade.getAggregation(
              request,
              VelocityService.KeyType.PAN,
              VelocityService.TimeWindow.HOUR_24,
              VelocityService.AggregationType.DISTINCT_MERCHANTS);

      assertThat(distinctMerchants).isEqualByComparingTo("7");
    }
  }

  // ========== Helpers ==========

  private TransactionRequest createRequest() {
    return TransactionRequest.builder()
        .externalTransactionId("tx-facade-test-1")
        .customerIdFromHeader("cust-123")
        .pan("4111111111111111")
        .transactionAmount(new BigDecimal("100.00"))
        .transactionDate(20251219)
        .transactionTime(120000)
        .merchantId("merchant-1")
        .mcc(5411)
        .merchantCountryCode("076")
        .build();
  }

  private VelocityService.VelocityStats createDbStats(long count, String totalAmount) {
    return VelocityService.VelocityStats.builder()
        .transactionCount(count)
        .totalAmount(new BigDecimal(totalAmount))
        .avgAmount(
            count > 0
                ? new BigDecimal(totalAmount)
                    .divide(BigDecimal.valueOf(count), 2, java.math.RoundingMode.HALF_UP)
                : BigDecimal.ZERO)
        .distinctMerchants(0)
        .distinctMccs(0)
        .distinctCountries(0)
        .found(count > 0)
        .source("DB")
        .build();
  }

  private RedisVelocityService.VelocityStats createRedisStats(long count, String totalAmount) {
    return RedisVelocityService.VelocityStats.builder()
        .transactionCount(count)
        .totalAmount(new BigDecimal(totalAmount))
        .avgAmount(
            count > 0
                ? new BigDecimal(totalAmount)
                    .divide(BigDecimal.valueOf(count), 2, java.math.RoundingMode.HALF_UP)
                : BigDecimal.ZERO)
        .distinctMerchants(0)
        .distinctMccs(0)
        .distinctCountries(0)
        .distinctDevices(0)
        .fromCache(true)
        .latencyMicros(100)
        .build();
  }
}
