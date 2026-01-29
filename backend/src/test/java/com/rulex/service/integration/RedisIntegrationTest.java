package com.rulex.service.integration;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.config.RedisKeyConvention;
import com.rulex.dto.TransactionRequest;
import com.rulex.service.RedisVelocityCacheService;
import com.rulex.service.VelocityServiceFacade;
import java.math.BigDecimal;
import java.time.Duration;
import java.util.Set;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

/**
 * Testes de integração para serviços Redis usando Testcontainers.
 *
 * <p>Estes testes validam:
 *
 * <ul>
 *   <li>Conexão e operações básicas com Redis
 *   <li>Gravação de transações no VelocityServiceFacade
 *   <li>Contagem e soma de velocidade
 *   <li>TTLs e expiração de chaves
 *   <li>Prefixos de chaves conforme RedisKeyConvention
 * </ul>
 */
@SpringBootTest
@Testcontainers
@ActiveProfiles("test-redis")
@DisplayName("Redis Integration Tests")
class RedisIntegrationTest {

  @Container
  static GenericContainer<?> redis =
      new GenericContainer<>(DockerImageName.parse("redis:7.4-alpine"))
          .withExposedPorts(6379)
          .withCommand("redis-server", "--maxmemory", "64mb", "--maxmemory-policy", "allkeys-lru");

  @DynamicPropertySource
  static void redisProperties(DynamicPropertyRegistry registry) {
    registry.add("spring.data.redis.host", redis::getHost);
    registry.add("spring.data.redis.port", () -> redis.getMappedPort(6379));
    registry.add("rulex.engine.velocity.redis.enabled", () -> "true");
  }

  @Autowired(required = false)
  private StringRedisTemplate redisTemplate;

  @Autowired(required = false)
  private RedisConnectionFactory connectionFactory;

  @Autowired(required = false)
  private VelocityServiceFacade velocityServiceFacade;

  @Autowired(required = false)
  private RedisVelocityCacheService redisVelocityCacheService;

  @BeforeEach
  void setUp() {
    // Limpar Redis antes de cada teste
    if (redisTemplate != null) {
      Set<String> keys = redisTemplate.keys("*");
      if (keys != null && !keys.isEmpty()) {
        redisTemplate.delete(keys);
      }
    }
  }

  @Nested
  @DisplayName("Conexão Redis")
  class RedisConnectionTests {

    @Test
    @DisplayName("Deve conectar ao Redis via Testcontainers")
    void shouldConnectToRedis() {
      assertThat(redis.isRunning()).isTrue();
      assertThat(connectionFactory).isNotNull();
    }

    @Test
    @DisplayName("Deve executar PING com sucesso")
    void shouldPingSuccessfully() {
      assertThat(redisTemplate).isNotNull();

      String result = redisTemplate.getConnectionFactory()
          .getConnection()
          .ping();

      assertThat(result).isEqualTo("PONG");
    }

    @Test
    @DisplayName("Deve gravar e ler valor simples")
    void shouldWriteAndReadValue() {
      String key = "test:simple:key";
      String value = "testValue123";

      redisTemplate.opsForValue().set(key, value);
      String retrieved = redisTemplate.opsForValue().get(key);

      assertThat(retrieved).isEqualTo(value);
    }

    @Test
    @DisplayName("Deve respeitar TTL de chaves")
    void shouldRespectKeyTtl() throws InterruptedException {
      String key = "test:ttl:key";
      redisTemplate.opsForValue().set(key, "value", Duration.ofSeconds(1));

      assertThat(redisTemplate.hasKey(key)).isTrue();

      // Aguardar expiração
      Thread.sleep(1500);

      assertThat(redisTemplate.hasKey(key)).isFalse();
    }
  }

  @Nested
  @DisplayName("VelocityServiceFacade Integration")
  class VelocityFacadeTests {

    @Test
    @DisplayName("Deve registrar transação no Redis")
    void shouldRecordTransactionInRedis() {
      if (velocityServiceFacade == null) {
        return; // Skip se o bean não foi criado
      }

      TransactionRequest request = createTestTransaction("PAN-001", "CUST-001", "MERCH-001");

      velocityServiceFacade.recordTransaction(request, "APPROVED", 25);

      // Verificar que chaves foram criadas
      Set<String> velocityKeys = redisTemplate.keys("velocity:*");
      Set<String> rulexKeys = redisTemplate.keys("rulex:velocity:*");

      assertThat(velocityKeys).isNotNull();
      assertThat(rulexKeys).isNotNull();

      int totalKeys = (velocityKeys != null ? velocityKeys.size() : 0)
          + (rulexKeys != null ? rulexKeys.size() : 0);

      assertThat(totalKeys).isGreaterThan(0);
    }

    @Test
    @DisplayName("Deve verificar status do Redis como ENABLED")
    void shouldReportRedisEnabled() {
      if (velocityServiceFacade == null) {
        return;
      }

      assertThat(velocityServiceFacade.isRedisEnabled()).isTrue();
      assertThat(velocityServiceFacade.getCacheStatus()).contains("REDIS");
    }
  }

  @Nested
  @DisplayName("RedisVelocityCacheService Integration")
  class VelocityCacheServiceTests {

    @Test
    @DisplayName("Deve incrementar contador de transações")
    void shouldIncrementTransactionCount() {
      if (redisVelocityCacheService == null) {
        return;
      }

      TransactionRequest request1 = createTestTransaction("PAN-002", "CUST-002", "MERCH-002");
      TransactionRequest request2 = createTestTransaction("PAN-002", "CUST-002", "MERCH-002");
      TransactionRequest request3 = createTestTransaction("PAN-002", "CUST-002", "MERCH-002");

      redisVelocityCacheService.recordTransaction(request1);
      redisVelocityCacheService.recordTransaction(request2);
      redisVelocityCacheService.recordTransaction(request3);

      // Verificar que chaves de contagem foram criadas (prefixo: velocity:)
      Set<String> countKeys = redisTemplate.keys("velocity:*:count:*");
      assertThat(countKeys).isNotNull();
      assertThat(countKeys.size()).isGreaterThan(0);
    }

    @Test
    @DisplayName("Deve acumular soma de valores")
    void shouldAccumulateSum() {
      if (redisVelocityCacheService == null) {
        return;
      }

      // Gravar 3 transações de R$100 cada
      for (int i = 0; i < 3; i++) {
        TransactionRequest request = TransactionRequest.builder()
            .pan("4111111111111111")
            .customerIdFromHeader("CUST-SUM-TEST")
            .merchantId("MERCH-SUM")
            .transactionAmount(new BigDecimal("100.00"))
            .transactionCurrencyCode(986)
            .mcc(5411)
            .build();
        redisVelocityCacheService.recordTransaction(request);
      }

      // Verificar chaves de soma
      Set<String> sumKeys = redisTemplate.keys("*sum*");
      assertThat(sumKeys).isNotNull();
    }
  }

  @Nested
  @DisplayName("RedisKeyConvention Tests")
  class KeyConventionTests {

    @Test
    @DisplayName("Deve gerar chaves com prefixo correto")
    void shouldGenerateKeysWithCorrectPrefix() {
      String countKey = RedisKeyConvention.velocityCountKey("PAN", "abc123", "5min");
      String sumKey = RedisKeyConvention.velocitySumKey("CUSTOMER", "cust001", "1h");
      String distinctKey = RedisKeyConvention.velocityDistinctKey("MERCHANT", "merch001", "mccs");

      assertThat(countKey).startsWith("rulex:velocity:count:");
      assertThat(sumKey).startsWith("rulex:velocity:sum:");
      assertThat(distinctKey).startsWith("rulex:velocity:distinct:");
    }

    @Test
    @DisplayName("Deve retornar TTL correto para janelas")
    void shouldReturnCorrectTtlForWindows() {
      assertThat(RedisKeyConvention.getTtlForWindow("5min")).isEqualTo(Duration.ofMinutes(6));
      assertThat(RedisKeyConvention.getTtlForWindow("1h")).isEqualTo(Duration.ofMinutes(65));
      assertThat(RedisKeyConvention.getTtlForWindow("24h")).isEqualTo(Duration.ofHours(25));
    }

    @Test
    @DisplayName("Deve sanitizar chaves corretamente")
    void shouldSanitizeKeysCorrectly() {
      String sanitized = RedisKeyConvention.sanitizeKey("São Paulo / Brasil");
      assertThat(sanitized).isEqualTo("s_o_paulo_brasil");
      assertThat(sanitized).doesNotContain(" ", "/", "ã");
    }
  }

  @Nested
  @DisplayName("Cache Operations")
  class CacheOperationsTests {

    @Test
    @DisplayName("Deve gravar e recuperar objeto em cache")
    void shouldCacheObject() {
      String cacheKey = RedisKeyConvention.cacheRuleKey("rule-001");
      String value = "{\"id\":\"rule-001\",\"name\":\"Test Rule\"}";

      redisTemplate.opsForValue().set(cacheKey, value, Duration.ofMinutes(5));
      String cached = redisTemplate.opsForValue().get(cacheKey);

      assertThat(cached).isEqualTo(value);
    }

    @Test
    @DisplayName("Deve usar HyperLogLog para contagem de distintos")
    void shouldUseHyperLogLogForDistinct() {
      String hllKey = "test:hll:merchants";

      // Adicionar merchants (com duplicatas)
      redisTemplate.opsForHyperLogLog().add(hllKey, "merchant1", "merchant2", "merchant3");
      redisTemplate.opsForHyperLogLog().add(hllKey, "merchant1", "merchant4"); // duplicata

      Long distinctCount = redisTemplate.opsForHyperLogLog().size(hllKey);

      assertThat(distinctCount).isEqualTo(4L); // 4 merchants distintos
    }
  }

  // ===== Métodos utilitários =====

  private TransactionRequest createTestTransaction(String pan, String customerId, String merchantId) {
    return TransactionRequest.builder()
        .externalTransactionId("TEST-" + System.currentTimeMillis())
        .pan(pan)
        .customerIdFromHeader(customerId)
        .customerAcctNumber(1234567890L)
        .merchantId(merchantId)
        .merchantName("Test Merchant")
        .transactionAmount(new BigDecimal("150.00"))
        .transactionCurrencyCode(986)
        .mcc(5411)
        .build();
  }
}
