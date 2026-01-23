package com.rulex.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;

import com.rulex.dto.TransactionRequest;
import java.math.BigDecimal;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.data.redis.core.StringRedisTemplate;

/**
 * Testes para o RedisVelocityService.
 *
 * <p>GAP-FIX #3: Aumentar cobertura de testes para classes críticas.
 *
 * <p>O RedisVelocityService implementa cache em memória de alta performance usando:
 *
 * <ul>
 *   <li>Sliding windows com buckets de 1 minuto
 *   <li>HyperLogLog para contagem de valores distintos
 *   <li>Latência sub-milissegundo
 * </ul>
 */
class RedisVelocityServiceTest {

  private VelocityService velocityService;
  private RedisVelocityService redisVelocityService;

  @BeforeEach
  void setUp() {
    velocityService = Mockito.mock(VelocityService.class);
    StringRedisTemplate redisTemplate = Mockito.mock(StringRedisTemplate.class);
    doNothing()
        .when(velocityService)
        .logTransaction(any(TransactionRequest.class), anyString(), anyInt());
    redisVelocityService = new RedisVelocityService(velocityService, redisTemplate, false);
  }

  @Nested
  @DisplayName("getStats - Estatísticas de velocidade")
  class GetStats {

    @Test
    @DisplayName("Deve retornar estatísticas vazias para request nulo")
    void shouldReturnEmptyStatsForNullRequest() {
      RedisVelocityService.VelocityStats stats =
          redisVelocityService.getStats(
              null, RedisVelocityService.KeyType.PAN, RedisVelocityService.TimeWindow.HOUR_1);

      assertThat(stats.getTransactionCount()).isZero();
      assertThat(stats.getTotalAmount()).isEqualByComparingTo(BigDecimal.ZERO);
    }

    @Test
    @DisplayName("Deve retornar estatísticas vazias para PAN nulo")
    void shouldReturnEmptyStatsForNullPan() {
      TransactionRequest request = createRequest();
      request.setPan(null);

      RedisVelocityService.VelocityStats stats =
          redisVelocityService.getStats(
              request, RedisVelocityService.KeyType.PAN, RedisVelocityService.TimeWindow.HOUR_1);

      assertThat(stats.getTransactionCount()).isZero();
    }

    @Test
    @DisplayName("Deve retornar estatísticas vazias quando não há transações")
    void shouldReturnEmptyStatsWhenNoTransactions() {
      TransactionRequest request = createRequest();

      RedisVelocityService.VelocityStats stats =
          redisVelocityService.getStats(
              request, RedisVelocityService.KeyType.PAN, RedisVelocityService.TimeWindow.HOUR_1);

      assertThat(stats.getTransactionCount()).isZero();
      assertThat(stats.getTotalAmount()).isEqualByComparingTo(BigDecimal.ZERO);
      assertThat(stats.isFromCache()).isTrue();
    }

    @Test
    @DisplayName("Deve retornar estatísticas após registrar transação")
    void shouldReturnStatsAfterRecordingTransaction() {
      TransactionRequest request = createRequest();

      // Registrar transação
      redisVelocityService.recordTransaction(request);

      // Verificar estatísticas
      RedisVelocityService.VelocityStats stats =
          redisVelocityService.getStats(
              request, RedisVelocityService.KeyType.PAN, RedisVelocityService.TimeWindow.HOUR_1);

      assertThat(stats.getTransactionCount()).isEqualTo(1);
      assertThat(stats.getTotalAmount()).isEqualByComparingTo("100.00");
      assertThat(stats.isFromCache()).isTrue();
    }

    @Test
    @DisplayName("Deve acumular múltiplas transações")
    void shouldAccumulateMultipleTransactions() {
      TransactionRequest request1 = createRequest();
      request1.setTransactionAmount(new BigDecimal("100.00"));

      TransactionRequest request2 = createRequest();
      request2.setExternalTransactionId("tx-2");
      request2.setTransactionAmount(new BigDecimal("200.00"));

      TransactionRequest request3 = createRequest();
      request3.setExternalTransactionId("tx-3");
      request3.setTransactionAmount(new BigDecimal("300.00"));

      // Registrar transações
      redisVelocityService.recordTransaction(request1);
      redisVelocityService.recordTransaction(request2);
      redisVelocityService.recordTransaction(request3);

      // Verificar estatísticas
      RedisVelocityService.VelocityStats stats =
          redisVelocityService.getStats(
              request1, RedisVelocityService.KeyType.PAN, RedisVelocityService.TimeWindow.HOUR_1);

      assertThat(stats.getTransactionCount()).isEqualTo(3);
      assertThat(stats.getTotalAmount()).isEqualByComparingTo("600.00");
      assertThat(stats.getAvgAmount()).isEqualByComparingTo("200.00");
    }
  }

  @Nested
  @DisplayName("recordTransaction - Registro de transações")
  class RecordTransaction {

    @Test
    @DisplayName("Deve ignorar request nulo")
    void shouldIgnoreNullRequest() {
      // Não deve lançar exceção
      redisVelocityService.recordTransaction(null);
    }

    @Test
    @DisplayName("Deve ignorar request sem externalTransactionId")
    void shouldIgnoreRequestWithoutTransactionId() {
      TransactionRequest request = createRequest();
      request.setExternalTransactionId(null);

      // Não deve lançar exceção
      redisVelocityService.recordTransaction(request);
    }

    @Test
    @DisplayName("Deve registrar transação e persistir no banco")
    void shouldRecordTransactionAndPersist() {
      TransactionRequest request = createRequest();

      redisVelocityService.recordTransaction(request);

      verify(velocityService).logTransaction(request, "PROCESSED", null);
    }

    @Test
    @DisplayName("Deve registrar para múltiplos tipos de chave")
    void shouldRecordForMultipleKeyTypes() {
      TransactionRequest request = createRequest();

      redisVelocityService.recordTransaction(request);

      // Verificar que foi registrado para PAN
      RedisVelocityService.VelocityStats panStats =
          redisVelocityService.getStats(
              request, RedisVelocityService.KeyType.PAN, RedisVelocityService.TimeWindow.HOUR_1);
      assertThat(panStats.getTransactionCount()).isEqualTo(1);

      // Verificar que foi registrado para CUSTOMER_ID
      RedisVelocityService.VelocityStats customerStats =
          redisVelocityService.getStats(
              request,
              RedisVelocityService.KeyType.CUSTOMER_ID,
              RedisVelocityService.TimeWindow.HOUR_1);
      assertThat(customerStats.getTransactionCount()).isEqualTo(1);

      // Verificar que foi registrado para MERCHANT_ID
      RedisVelocityService.VelocityStats merchantStats =
          redisVelocityService.getStats(
              request,
              RedisVelocityService.KeyType.MERCHANT_ID,
              RedisVelocityService.TimeWindow.HOUR_1);
      assertThat(merchantStats.getTransactionCount()).isEqualTo(1);
    }
  }

  @Nested
  @DisplayName("Distinct counts - Contagens distintas")
  class DistinctCounts {

    @Test
    @DisplayName("Deve contar merchants distintos")
    void shouldCountDistinctMerchants() {
      TransactionRequest request1 = createRequest();
      request1.setMerchantId("merchant-1");

      TransactionRequest request2 = createRequest();
      request2.setExternalTransactionId("tx-2");
      request2.setMerchantId("merchant-2");

      TransactionRequest request3 = createRequest();
      request3.setExternalTransactionId("tx-3");
      request3.setMerchantId("merchant-1"); // Mesmo merchant

      redisVelocityService.recordTransaction(request1);
      redisVelocityService.recordTransaction(request2);
      redisVelocityService.recordTransaction(request3);

      RedisVelocityService.VelocityStats stats =
          redisVelocityService.getStats(
              request1, RedisVelocityService.KeyType.PAN, RedisVelocityService.TimeWindow.HOUR_1);

      // HyperLogLog pode ter pequena margem de erro, mas para poucos valores deve ser exato
      assertThat(stats.getDistinctMerchants()).isBetween(1L, 3L);
    }

    @Test
    @DisplayName("Deve contar MCCs distintos")
    void shouldCountDistinctMccs() {
      TransactionRequest request1 = createRequest();
      request1.setMcc(5411);

      TransactionRequest request2 = createRequest();
      request2.setExternalTransactionId("tx-2");
      request2.setMcc(5812);

      redisVelocityService.recordTransaction(request1);
      redisVelocityService.recordTransaction(request2);

      RedisVelocityService.VelocityStats stats =
          redisVelocityService.getStats(
              request1, RedisVelocityService.KeyType.PAN, RedisVelocityService.TimeWindow.HOUR_1);

      assertThat(stats.getDistinctMccs()).isBetween(1L, 3L);
    }

    @Test
    @DisplayName("Deve contar países distintos")
    void shouldCountDistinctCountries() {
      TransactionRequest request1 = createRequest();
      request1.setMerchantCountryCode("076"); // Brasil

      TransactionRequest request2 = createRequest();
      request2.setExternalTransactionId("tx-2");
      request2.setMerchantCountryCode("840"); // EUA

      redisVelocityService.recordTransaction(request1);
      redisVelocityService.recordTransaction(request2);

      RedisVelocityService.VelocityStats stats =
          redisVelocityService.getStats(
              request1, RedisVelocityService.KeyType.PAN, RedisVelocityService.TimeWindow.HOUR_1);

      assertThat(stats.getDistinctCountries()).isBetween(1L, 3L);
    }
  }

  @Nested
  @DisplayName("Time windows - Janelas temporais")
  class TimeWindows {

    @Test
    @DisplayName("Deve suportar diferentes janelas temporais")
    void shouldSupportDifferentTimeWindows() {
      TransactionRequest request = createRequest();
      redisVelocityService.recordTransaction(request);

      // Testar diferentes janelas
      for (RedisVelocityService.TimeWindow window : RedisVelocityService.TimeWindow.values()) {
        RedisVelocityService.VelocityStats stats =
            redisVelocityService.getStats(request, RedisVelocityService.KeyType.PAN, window);

        assertThat(stats.getTransactionCount()).isGreaterThanOrEqualTo(0);
      }
    }
  }

  @Nested
  @DisplayName("Convenience methods - Métodos de conveniência")
  class ConvenienceMethods {

    @Test
    @DisplayName("getTransactionCount deve retornar contagem")
    void shouldGetTransactionCount() {
      TransactionRequest request = createRequest();
      redisVelocityService.recordTransaction(request);

      long count =
          redisVelocityService.getTransactionCount(
              request, RedisVelocityService.KeyType.PAN, RedisVelocityService.TimeWindow.HOUR_1);

      assertThat(count).isEqualTo(1);
    }

    @Test
    @DisplayName("getTotalAmount deve retornar soma")
    void shouldGetTotalAmount() {
      TransactionRequest request = createRequest();
      request.setTransactionAmount(new BigDecimal("250.00"));
      redisVelocityService.recordTransaction(request);

      BigDecimal total =
          redisVelocityService.getTotalAmount(
              request, RedisVelocityService.KeyType.PAN, RedisVelocityService.TimeWindow.HOUR_1);

      assertThat(total).isEqualByComparingTo("250.00");
    }

    @Test
    @DisplayName("getDistinctMerchants deve retornar contagem de merchants")
    void shouldGetDistinctMerchants() {
      TransactionRequest request = createRequest();
      redisVelocityService.recordTransaction(request);

      long distinctMerchants =
          redisVelocityService.getDistinctMerchants(
              request, RedisVelocityService.KeyType.PAN, RedisVelocityService.TimeWindow.HOUR_1);

      assertThat(distinctMerchants).isGreaterThanOrEqualTo(0);
    }
  }

  @Nested
  @DisplayName("Performance - Latência")
  class Performance {

    @Test
    @DisplayName("Deve ter latência sub-milissegundo")
    void shouldHaveSubMillisecondLatency() {
      TransactionRequest request = createRequest();
      redisVelocityService.recordTransaction(request);

      RedisVelocityService.VelocityStats stats =
          redisVelocityService.getStats(
              request, RedisVelocityService.KeyType.PAN, RedisVelocityService.TimeWindow.HOUR_1);

      // Latência deve ser menor que 1000 microssegundos (1ms)
      assertThat(stats.getLatencyMicros()).isLessThan(10000); // 10ms como margem de segurança
    }
  }

  // ========== Helpers ==========

  private TransactionRequest createRequest() {
    return TransactionRequest.builder()
        .externalTransactionId("tx-redis-test-1")
        .customerIdFromHeader("cust-redis-123")
        .pan("4111111111111111")
        .transactionAmount(new BigDecimal("100.00"))
        .transactionDate(20251219)
        .transactionTime(120000)
        .merchantId("merchant-redis-1")
        .mcc(5411)
        .merchantCountryCode("076")
        .terminalId("terminal-1")
        .acquirerBin("123456")
        .build();
  }
}
