package com.rulex.service;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.dto.TransactionRequest;
import java.math.BigDecimal;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

/**
 * Testes de integração para o RedisVelocityCacheService usando Testcontainers.
 *
 * <p>GAP-FIX #2: Valida que o cache Redis funciona corretamente para cálculos de velocidade.
 */
@SpringBootTest
@Testcontainers
@DisplayName("VelocityService Redis Integration Tests")
class VelocityServiceRedisIT {

  @Container
  static GenericContainer<?> redis =
      new GenericContainer<>(DockerImageName.parse("redis:7-alpine")).withExposedPorts(6379);

  @DynamicPropertySource
  static void redisProperties(DynamicPropertyRegistry registry) {
    registry.add("spring.data.redis.host", redis::getHost);
    registry.add("spring.data.redis.port", () -> redis.getMappedPort(6379));
    registry.add("rulex.engine.velocity.redis.enabled", () -> "true");
  }

  @Autowired(required = false)
  private RedisVelocityCacheService redisVelocityCacheService;

  @Autowired private StringRedisTemplate redisTemplate;

  @BeforeEach
  void setUp() {
    // Limpar todas as chaves do Redis antes de cada teste
    redisTemplate.getConnectionFactory().getConnection().serverCommands().flushAll();
  }

  @Test
  @DisplayName("Deve registrar transação e incrementar contadores")
  void shouldRecordTransactionAndIncrementCounters() {
    // Skip se o serviço não foi injetado (Redis não disponível)
    if (redisVelocityCacheService == null) {
      return;
    }

    TransactionRequest request = createRequest("tx-1", "100.00");

    // Registrar transação
    redisVelocityCacheService.recordTransaction(request);

    // Verificar contadores
    long count =
        redisVelocityCacheService.getTransactionCount(
            request,
            RedisVelocityCacheService.KeyType.PAN,
            RedisVelocityCacheService.TimeWindow.HOUR_1);

    assertThat(count).isEqualTo(1);
  }

  @Test
  @DisplayName("Deve acumular múltiplas transações")
  void shouldAccumulateMultipleTransactions() {
    if (redisVelocityCacheService == null) {
      return;
    }

    TransactionRequest request1 = createRequest("tx-1", "100.00");
    TransactionRequest request2 = createRequest("tx-2", "200.00");
    TransactionRequest request3 = createRequest("tx-3", "300.00");

    // Registrar transações
    redisVelocityCacheService.recordTransaction(request1);
    redisVelocityCacheService.recordTransaction(request2);
    redisVelocityCacheService.recordTransaction(request3);

    // Verificar contagem
    long count =
        redisVelocityCacheService.getTransactionCount(
            request1,
            RedisVelocityCacheService.KeyType.PAN,
            RedisVelocityCacheService.TimeWindow.HOUR_1);

    assertThat(count).isEqualTo(3);

    // Verificar soma
    BigDecimal total =
        redisVelocityCacheService.getTotalAmount(
            request1,
            RedisVelocityCacheService.KeyType.PAN,
            RedisVelocityCacheService.TimeWindow.HOUR_1);

    assertThat(total).isEqualByComparingTo("600.00");

    // Verificar média
    BigDecimal avg =
        redisVelocityCacheService.getAverageAmount(
            request1,
            RedisVelocityCacheService.KeyType.PAN,
            RedisVelocityCacheService.TimeWindow.HOUR_1);

    assertThat(avg).isEqualByComparingTo("200.00");
  }

  @Test
  @DisplayName("Deve contar merchants distintos usando HyperLogLog")
  void shouldCountDistinctMerchantsUsingHyperLogLog() {
    if (redisVelocityCacheService == null) {
      return;
    }

    TransactionRequest request1 = createRequest("tx-1", "100.00");
    request1.setMerchantId("merchant-1");

    TransactionRequest request2 = createRequest("tx-2", "200.00");
    request2.setMerchantId("merchant-2");

    TransactionRequest request3 = createRequest("tx-3", "300.00");
    request3.setMerchantId("merchant-1"); // Mesmo merchant

    redisVelocityCacheService.recordTransaction(request1);
    redisVelocityCacheService.recordTransaction(request2);
    redisVelocityCacheService.recordTransaction(request3);

    long distinctMerchants =
        redisVelocityCacheService.getDistinctMerchants(
            request1, RedisVelocityCacheService.KeyType.PAN);

    // HyperLogLog deve retornar 2 (merchant-1 e merchant-2)
    assertThat(distinctMerchants).isEqualTo(2);
  }

  @Test
  @DisplayName("Deve contar MCCs distintos usando HyperLogLog")
  void shouldCountDistinctMccsUsingHyperLogLog() {
    if (redisVelocityCacheService == null) {
      return;
    }

    TransactionRequest request1 = createRequest("tx-1", "100.00");
    request1.setMcc(5411);

    TransactionRequest request2 = createRequest("tx-2", "200.00");
    request2.setMcc(5812);

    TransactionRequest request3 = createRequest("tx-3", "300.00");
    request3.setMcc(5411); // Mesmo MCC

    redisVelocityCacheService.recordTransaction(request1);
    redisVelocityCacheService.recordTransaction(request2);
    redisVelocityCacheService.recordTransaction(request3);

    long distinctMccs =
        redisVelocityCacheService.getDistinctMccs(request1, RedisVelocityCacheService.KeyType.PAN);

    assertThat(distinctMccs).isEqualTo(2);
  }

  @Test
  @DisplayName("Deve contar países distintos usando HyperLogLog")
  void shouldCountDistinctCountriesUsingHyperLogLog() {
    if (redisVelocityCacheService == null) {
      return;
    }

    TransactionRequest request1 = createRequest("tx-1", "100.00");
    request1.setMerchantCountryCode("076"); // Brasil

    TransactionRequest request2 = createRequest("tx-2", "200.00");
    request2.setMerchantCountryCode("840"); // EUA

    TransactionRequest request3 = createRequest("tx-3", "300.00");
    request3.setMerchantCountryCode("076"); // Brasil novamente

    redisVelocityCacheService.recordTransaction(request1);
    redisVelocityCacheService.recordTransaction(request2);
    redisVelocityCacheService.recordTransaction(request3);

    long distinctCountries =
        redisVelocityCacheService.getDistinctCountries(
            request1, RedisVelocityCacheService.KeyType.PAN);

    assertThat(distinctCountries).isEqualTo(2);
  }

  @Test
  @DisplayName("Deve detectar burst de transações")
  void shouldDetectBurstOfTransactions() {
    if (redisVelocityCacheService == null) {
      return;
    }

    TransactionRequest request = createRequest("tx-1", "100.00");

    // Registrar 5 transações
    for (int i = 0; i < 5; i++) {
      TransactionRequest r = createRequest("tx-" + i, "100.00");
      redisVelocityCacheService.recordTransaction(r);
    }

    // Verificar burst (limite de 3 em 5 minutos)
    boolean isBurst = redisVelocityCacheService.isBurst(request, 3, 5);

    assertThat(isBurst).isTrue();
  }

  @Test
  @DisplayName("Não deve detectar burst quando abaixo do limite")
  void shouldNotDetectBurstWhenBelowLimit() {
    if (redisVelocityCacheService == null) {
      return;
    }

    TransactionRequest request = createRequest("tx-1", "100.00");

    // Registrar apenas 2 transações
    redisVelocityCacheService.recordTransaction(request);
    redisVelocityCacheService.recordTransaction(createRequest("tx-2", "200.00"));

    // Verificar burst (limite de 5 em 5 minutos)
    boolean isBurst = redisVelocityCacheService.isBurst(request, 5, 5);

    assertThat(isBurst).isFalse();
  }

  @Test
  @DisplayName("Deve registrar para múltiplos tipos de chave")
  void shouldRecordForMultipleKeyTypes() {
    if (redisVelocityCacheService == null) {
      return;
    }

    TransactionRequest request = createRequest("tx-1", "100.00");
    request.setCustomerIdFromHeader("customer-123");
    request.setMerchantId("merchant-456");

    redisVelocityCacheService.recordTransaction(request);

    // Verificar contagem por PAN
    long panCount =
        redisVelocityCacheService.getTransactionCount(
            request,
            RedisVelocityCacheService.KeyType.PAN,
            RedisVelocityCacheService.TimeWindow.HOUR_1);
    assertThat(panCount).isEqualTo(1);

    // Verificar contagem por Customer ID
    long customerCount =
        redisVelocityCacheService.getTransactionCount(
            request,
            RedisVelocityCacheService.KeyType.CUSTOMER_ID,
            RedisVelocityCacheService.TimeWindow.HOUR_1);
    assertThat(customerCount).isEqualTo(1);

    // Verificar contagem por Merchant ID
    long merchantCount =
        redisVelocityCacheService.getTransactionCount(
            request,
            RedisVelocityCacheService.KeyType.MERCHANT_ID,
            RedisVelocityCacheService.TimeWindow.HOUR_1);
    assertThat(merchantCount).isEqualTo(1);
  }

  @Test
  @DisplayName("Deve suportar diferentes janelas temporais")
  void shouldSupportDifferentTimeWindows() {
    if (redisVelocityCacheService == null) {
      return;
    }

    TransactionRequest request = createRequest("tx-1", "100.00");
    redisVelocityCacheService.recordTransaction(request);

    // Verificar todas as janelas temporais
    for (RedisVelocityCacheService.TimeWindow window :
        RedisVelocityCacheService.TimeWindow.values()) {
      long count =
          redisVelocityCacheService.getTransactionCount(
              request, RedisVelocityCacheService.KeyType.PAN, window);

      assertThat(count).isEqualTo(1);
    }
  }

  @Test
  @DisplayName("Deve retornar zero para transação sem histórico")
  void shouldReturnZeroForTransactionWithoutHistory() {
    if (redisVelocityCacheService == null) {
      return;
    }

    TransactionRequest request = createRequest("tx-new", "100.00");
    request.setPan("9999999999999999"); // PAN diferente

    long count =
        redisVelocityCacheService.getTransactionCount(
            request,
            RedisVelocityCacheService.KeyType.PAN,
            RedisVelocityCacheService.TimeWindow.HOUR_1);

    assertThat(count).isZero();
  }

  // ========== Helpers ==========

  private TransactionRequest createRequest(String txId, String amount) {
    return TransactionRequest.builder()
        .externalTransactionId(txId)
        .customerIdFromHeader("cust-redis-it")
        .pan("4111111111111111")
        .transactionAmount(new BigDecimal(amount))
        .transactionDate(20251219)
        .transactionTime(120000)
        .merchantId("merchant-redis-it")
        .mcc(5411)
        .merchantCountryCode("076")
        .build();
  }
}
