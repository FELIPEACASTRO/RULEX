package com.rulex.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.VelocityTransactionLog;
import com.rulex.repository.VelocityCounterRepository;
import com.rulex.repository.VelocityTransactionLogRepository;
import com.rulex.service.VelocityService.AggregationType;
import com.rulex.service.VelocityService.KeyType;
import com.rulex.service.VelocityService.TimeWindow;
import com.rulex.service.VelocityService.VelocityStats;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.simple.SimpleMeterRegistry;
import java.math.BigDecimal;
import java.time.OffsetDateTime;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
@DisplayName("VelocityService")
class VelocityServiceTest {

  @Mock private VelocityCounterRepository counterRepository;
  @Mock private VelocityTransactionLogRepository logRepository;

  private MeterRegistry meterRegistry;
  private VelocityService velocityService;

  @BeforeEach
  void setUp() {
    // PERF-002 FIX: Adicionado MeterRegistry para métricas de cache
    meterRegistry = new SimpleMeterRegistry();
    velocityService = new VelocityService(counterRepository, logRepository, meterRegistry);
  }

  private TransactionRequest createValidRequest() {
    return TransactionRequest.builder()
        .externalTransactionId("TXN-001")
        .customerIdFromHeader("CUST-001")
        .pan("4111111111111111")
        .merchantId("MERCH-001")
        .transactionAmount(BigDecimal.valueOf(100.00))
        .transactionCurrencyCode(986)
        .mcc(5411)
        .merchantCountryCode("BR")
        .build();
  }

  @Nested
  @DisplayName("getStats()")
  class GetStatsTests {

    @Test
    @DisplayName("Deve retornar stats vazias para request null")
    void shouldReturnEmptyStatsForNullRequest() {
      VelocityStats stats = velocityService.getStats(null, KeyType.PAN, TimeWindow.HOUR_1);

      assertThat(stats).isNotNull();
      assertThat(stats.getTransactionCount()).isZero();
      assertThat(stats.isFound()).isFalse();
      assertThat(stats.getSource()).isEqualTo("EMPTY");
    }

    @Test
    @DisplayName("Deve retornar stats vazias para PAN null")
    void shouldReturnEmptyStatsForNullPan() {
      TransactionRequest request =
          TransactionRequest.builder()
              .externalTransactionId("TXN-001")
              .customerIdFromHeader("CUST-001")
              .pan(null)
              .build();

      VelocityStats stats = velocityService.getStats(request, KeyType.PAN, TimeWindow.HOUR_1);

      assertThat(stats).isNotNull();
      assertThat(stats.isFound()).isFalse();
    }

    @Test
    @DisplayName("Deve retornar stats vazias para PAN vazio")
    void shouldReturnEmptyStatsForBlankPan() {
      TransactionRequest request =
          TransactionRequest.builder()
              .externalTransactionId("TXN-001")
              .customerIdFromHeader("CUST-001")
              .pan("  ")
              .build();

      VelocityStats stats = velocityService.getStats(request, KeyType.PAN, TimeWindow.HOUR_1);

      assertThat(stats).isNotNull();
      assertThat(stats.isFound()).isFalse();
    }

    @Test
    @DisplayName("Deve calcular stats do banco de dados")
    void shouldComputeStatsFromDatabase() {
      TransactionRequest request = createValidRequest();

      // Mock repository responses
      when(logRepository.countByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(5L);
      when(logRepository.sumAmountByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(BigDecimal.valueOf(500.00));
      when(logRepository.avgAmountByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(BigDecimal.valueOf(100.00));
      when(logRepository.countDistinctMerchantsByPanHashSince(
              anyString(), any(OffsetDateTime.class)))
          .thenReturn(3L);
      when(logRepository.countDistinctMccsByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(2L);
      when(logRepository.countDistinctCountriesByPanHashSince(
              anyString(), any(OffsetDateTime.class)))
          .thenReturn(1L);
      when(logRepository.countFraudByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);

      VelocityStats stats = velocityService.getStats(request, KeyType.PAN, TimeWindow.HOUR_24);

      assertThat(stats).isNotNull();
      assertThat(stats.getTransactionCount()).isEqualTo(5);
      assertThat(stats.getTotalAmount()).isEqualByComparingTo(BigDecimal.valueOf(500.00));
      assertThat(stats.getAvgAmount()).isEqualByComparingTo(BigDecimal.valueOf(100.00));
      assertThat(stats.getDistinctMerchants()).isEqualTo(3);
      assertThat(stats.getDistinctMccs()).isEqualTo(2);
      assertThat(stats.getDistinctCountries()).isEqualTo(1);
      assertThat(stats.getFraudCount()).isZero();
      assertThat(stats.isFound()).isTrue();
      assertThat(stats.getSource()).isEqualTo("DB");
    }

    @Test
    @DisplayName("Deve usar cache na segunda chamada")
    void shouldUseCacheOnSecondCall() {
      TransactionRequest request = createValidRequest();

      // Mock para primeira chamada
      when(logRepository.countByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(5L);
      when(logRepository.sumAmountByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(BigDecimal.valueOf(500.00));
      when(logRepository.avgAmountByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(BigDecimal.valueOf(100.00));
      when(logRepository.countDistinctMerchantsByPanHashSince(
              anyString(), any(OffsetDateTime.class)))
          .thenReturn(3L);
      when(logRepository.countDistinctMccsByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(2L);
      when(logRepository.countDistinctCountriesByPanHashSince(
              anyString(), any(OffsetDateTime.class)))
          .thenReturn(1L);
      when(logRepository.countFraudByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);

      // Primeira chamada - deve ir ao DB
      VelocityStats stats1 = velocityService.getStats(request, KeyType.PAN, TimeWindow.HOUR_24);

      // Segunda chamada - deve usar cache
      VelocityStats stats2 = velocityService.getStats(request, KeyType.PAN, TimeWindow.HOUR_24);

      // Verificar que o repository só foi chamado uma vez
      verify(logRepository).countByPanHashSince(anyString(), any(OffsetDateTime.class));

      assertThat(stats1.getTransactionCount()).isEqualTo(stats2.getTransactionCount());
    }

    @Test
    @DisplayName("Deve calcular stats por CUSTOMER_ID")
    void shouldComputeStatsByCustomerId() {
      TransactionRequest request = createValidRequest();

      when(logRepository.countByCustomerIdSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(10L);
      when(logRepository.sumAmountByCustomerIdSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(BigDecimal.valueOf(1000.00));

      VelocityStats stats =
          velocityService.getStats(request, KeyType.CUSTOMER_ID, TimeWindow.HOUR_1);

      assertThat(stats).isNotNull();
      assertThat(stats.getTransactionCount()).isEqualTo(10);
      assertThat(stats.getTotalAmount()).isEqualByComparingTo(BigDecimal.valueOf(1000.00));
      assertThat(stats.isFound()).isTrue();
    }
  }

  @Nested
  @DisplayName("getAggregation()")
  class GetAggregationTests {

    @Test
    @DisplayName("Deve retornar COUNT correto")
    void shouldReturnCorrectCount() {
      TransactionRequest request = createValidRequest();

      when(logRepository.countByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(7L);
      when(logRepository.sumAmountByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(BigDecimal.ZERO);
      when(logRepository.avgAmountByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(BigDecimal.ZERO);
      when(logRepository.countDistinctMerchantsByPanHashSince(
              anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);
      when(logRepository.countDistinctMccsByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);
      when(logRepository.countDistinctCountriesByPanHashSince(
              anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);
      when(logRepository.countFraudByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);

      BigDecimal count =
          velocityService.getAggregation(
              request, KeyType.PAN, TimeWindow.HOUR_1, AggregationType.COUNT);

      assertThat(count).isEqualByComparingTo(BigDecimal.valueOf(7));
    }

    @Test
    @DisplayName("Deve retornar SUM correto")
    void shouldReturnCorrectSum() {
      TransactionRequest request = createValidRequest();

      when(logRepository.countByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(3L);
      when(logRepository.sumAmountByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(BigDecimal.valueOf(750.50));
      when(logRepository.avgAmountByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(BigDecimal.valueOf(250.17));
      when(logRepository.countDistinctMerchantsByPanHashSince(
              anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);
      when(logRepository.countDistinctMccsByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);
      when(logRepository.countDistinctCountriesByPanHashSince(
              anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);
      when(logRepository.countFraudByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);

      BigDecimal sum =
          velocityService.getAggregation(
              request, KeyType.PAN, TimeWindow.HOUR_6, AggregationType.SUM);

      assertThat(sum).isEqualByComparingTo(BigDecimal.valueOf(750.50));
    }

    @Test
    @DisplayName("Deve retornar DISTINCT_MERCHANTS correto")
    void shouldReturnCorrectDistinctMerchants() {
      TransactionRequest request = createValidRequest();

      when(logRepository.countByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(5L);
      when(logRepository.sumAmountByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(BigDecimal.ZERO);
      when(logRepository.avgAmountByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(BigDecimal.ZERO);
      when(logRepository.countDistinctMerchantsByPanHashSince(
              anyString(), any(OffsetDateTime.class)))
          .thenReturn(4L);
      when(logRepository.countDistinctMccsByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);
      when(logRepository.countDistinctCountriesByPanHashSince(
              anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);
      when(logRepository.countFraudByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);

      BigDecimal distinctMerchants =
          velocityService.getAggregation(
              request, KeyType.PAN, TimeWindow.HOUR_24, AggregationType.DISTINCT_MERCHANTS);

      assertThat(distinctMerchants).isEqualByComparingTo(BigDecimal.valueOf(4));
    }
  }

  @Nested
  @DisplayName("logTransaction()")
  class LogTransactionTests {

    @Test
    @DisplayName("Não deve logar transação null")
    void shouldNotLogNullTransaction() {
      velocityService.logTransaction(null, "APPROVED", 0);

      verify(logRepository, never()).save(any(VelocityTransactionLog.class));
    }

    @Test
    @DisplayName("Não deve logar transação sem externalTransactionId")
    void shouldNotLogTransactionWithoutExternalId() {
      TransactionRequest request =
          TransactionRequest.builder()
              .customerIdFromHeader("CUST-001")
              .pan("4111111111111111")
              .build();

      velocityService.logTransaction(request, "APPROVED", 0);

      verify(logRepository, never()).save(any(VelocityTransactionLog.class));
    }

    @Test
    @DisplayName("Não deve duplicar transação já existente")
    void shouldNotDuplicateExistingTransaction() {
      TransactionRequest request = createValidRequest();

      when(logRepository.findByExternalTransactionId("TXN-001"))
          .thenReturn(Optional.of(new VelocityTransactionLog()));

      velocityService.logTransaction(request, "APPROVED", 10);

      verify(logRepository, never()).save(any(VelocityTransactionLog.class));
    }

    @Test
    @DisplayName("Deve logar nova transação corretamente")
    void shouldLogNewTransactionCorrectly() {
      TransactionRequest request = createValidRequest();

      when(logRepository.findByExternalTransactionId("TXN-001")).thenReturn(Optional.empty());

      velocityService.logTransaction(request, "SUSPICIOUS", 75);

      ArgumentCaptor<VelocityTransactionLog> captor =
          ArgumentCaptor.forClass(VelocityTransactionLog.class);
      verify(logRepository).save(captor.capture());

      VelocityTransactionLog saved = captor.getValue();
      assertThat(saved.getExternalTransactionId()).isEqualTo("TXN-001");
      assertThat(saved.getCustomerId()).isEqualTo("CUST-001");
      assertThat(saved.getMerchantId()).isEqualTo("MERCH-001");
      assertThat(saved.getAmount()).isEqualByComparingTo(BigDecimal.valueOf(100.00));
      assertThat(saved.getCurrencyCode()).isEqualTo(986);
      assertThat(saved.getMcc()).isEqualTo(5411);
      assertThat(saved.getMerchantCountry()).isEqualTo("BR");
      assertThat(saved.getDecision()).isEqualTo("SUSPICIOUS");
      assertThat(saved.getRiskScore()).isEqualTo(75);
      assertThat(saved.getPanHash()).isNotNull().hasSize(64); // SHA-256 hex
    }
  }

  @Nested
  @DisplayName("Convenience methods")
  class ConvenienceMethodsTests {

    @Test
    @DisplayName("countPanTransactionsInHours deve retornar contagem correta")
    void countPanTransactionsInHoursShouldReturnCorrectCount() {
      TransactionRequest request = createValidRequest();

      when(logRepository.countByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(15L);
      when(logRepository.sumAmountByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(BigDecimal.ZERO);
      when(logRepository.avgAmountByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(BigDecimal.ZERO);
      when(logRepository.countDistinctMerchantsByPanHashSince(
              anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);
      when(logRepository.countDistinctMccsByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);
      when(logRepository.countDistinctCountriesByPanHashSince(
              anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);
      when(logRepository.countFraudByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);

      long count = velocityService.countPanTransactionsInHours(request, 24);

      assertThat(count).isEqualTo(15);
    }

    @Test
    @DisplayName("sumPanAmountInHours deve retornar soma correta")
    void sumPanAmountInHoursShouldReturnCorrectSum() {
      TransactionRequest request = createValidRequest();

      when(logRepository.countByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(5L);
      when(logRepository.sumAmountByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(BigDecimal.valueOf(2500.00));
      when(logRepository.avgAmountByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(BigDecimal.valueOf(500.00));
      when(logRepository.countDistinctMerchantsByPanHashSince(
              anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);
      when(logRepository.countDistinctMccsByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);
      when(logRepository.countDistinctCountriesByPanHashSince(
              anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);
      when(logRepository.countFraudByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);

      BigDecimal sum = velocityService.sumPanAmountInHours(request, 6);

      assertThat(sum).isEqualByComparingTo(BigDecimal.valueOf(2500.00));
    }

    @Test
    @DisplayName("isBurst deve detectar burst de transações")
    void isBurstShouldDetectTransactionBurst() {
      TransactionRequest request = createValidRequest();

      when(logRepository.countByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(10L);
      when(logRepository.sumAmountByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(BigDecimal.ZERO);
      when(logRepository.avgAmountByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(BigDecimal.ZERO);
      when(logRepository.countDistinctMerchantsByPanHashSince(
              anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);
      when(logRepository.countDistinctMccsByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);
      when(logRepository.countDistinctCountriesByPanHashSince(
              anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);
      when(logRepository.countFraudByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);

      boolean isBurst = velocityService.isBurst(request, 5, 5);

      assertThat(isBurst).isTrue();
    }

    @Test
    @DisplayName("isBurst deve retornar false quando não há burst")
    void isBurstShouldReturnFalseWhenNoBurst() {
      TransactionRequest request = createValidRequest();

      when(logRepository.countByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(2L);
      when(logRepository.sumAmountByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(BigDecimal.ZERO);
      when(logRepository.avgAmountByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(BigDecimal.ZERO);
      when(logRepository.countDistinctMerchantsByPanHashSince(
              anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);
      when(logRepository.countDistinctMccsByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);
      when(logRepository.countDistinctCountriesByPanHashSince(
              anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);
      when(logRepository.countFraudByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);

      boolean isBurst = velocityService.isBurst(request, 5, 5);

      assertThat(isBurst).isFalse();
    }
  }

  @Nested
  @DisplayName("Cache management")
  class CacheManagementTests {

    @Test
    @DisplayName("clearCache deve limpar o cache")
    void clearCacheShouldClearAllEntries() {
      TransactionRequest request = createValidRequest();

      // Populate cache
      when(logRepository.countByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(5L);
      when(logRepository.sumAmountByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(BigDecimal.ZERO);
      when(logRepository.avgAmountByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(BigDecimal.ZERO);
      when(logRepository.countDistinctMerchantsByPanHashSince(
              anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);
      when(logRepository.countDistinctMccsByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);
      when(logRepository.countDistinctCountriesByPanHashSince(
              anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);
      when(logRepository.countFraudByPanHashSince(anyString(), any(OffsetDateTime.class)))
          .thenReturn(0L);

      velocityService.getStats(request, KeyType.PAN, TimeWindow.HOUR_1);

      assertThat(velocityService.getCacheStats().get("statsCache")).isEqualTo(1L);

      velocityService.clearCache();

      assertThat(velocityService.getCacheStats().get("statsCache")).isZero();
    }
  }

  @Nested
  @DisplayName("VelocityStats")
  class VelocityStatsTests {

    @Test
    @DisplayName("empty() deve criar stats vazias")
    void emptyShouldCreateEmptyStats() {
      VelocityStats empty = VelocityStats.empty();

      assertThat(empty.getTransactionCount()).isZero();
      assertThat(empty.getTotalAmount()).isEqualByComparingTo(BigDecimal.ZERO);
      assertThat(empty.getAvgAmount()).isEqualByComparingTo(BigDecimal.ZERO);
      assertThat(empty.getDistinctMerchants()).isZero();
      assertThat(empty.getDistinctMccs()).isZero();
      assertThat(empty.getDistinctCountries()).isZero();
      assertThat(empty.getFraudCount()).isZero();
      assertThat(empty.isFound()).isFalse();
      assertThat(empty.getSource()).isEqualTo("EMPTY");
    }
  }
}
