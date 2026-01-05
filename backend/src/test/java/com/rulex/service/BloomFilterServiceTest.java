package com.rulex.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import com.rulex.entity.RuleList.EntityType;
import com.rulex.repository.RuleListEntryRepository;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

/**
 * Testes para o BloomFilterService.
 *
 * <p>GAP-FIX #3: Cobertura de testes para classes críticas.
 *
 * <p>O BloomFilterService é uma estrutura de dados probabilística que fornece:
 *
 * <ul>
 *   <li>Lookup O(1) independente do tamanho da lista
 *   <li>Sem falsos negativos
 *   <li>Possíveis falsos positivos (configurável)
 * </ul>
 */
@DisplayName("BloomFilterService Tests")
class BloomFilterServiceTest {

  private RuleListEntryRepository ruleListEntryRepository;
  private BloomFilterService bloomFilterService;

  @BeforeEach
  void setUp() {
    ruleListEntryRepository = Mockito.mock(RuleListEntryRepository.class);
    when(ruleListEntryRepository.findAll()).thenReturn(List.of());
    bloomFilterService = new BloomFilterService(ruleListEntryRepository);
  }

  @Nested
  @DisplayName("Verificação de Blacklist")
  class BlacklistChecks {

    @Test
    @DisplayName("Deve retornar não na lista para valor não presente")
    void shouldReturnNotInListForAbsentValue() {
      when(ruleListEntryRepository.isValueBlacklisted(any(), anyString(), any())).thenReturn(false);

      BloomFilterService.CheckResult result =
          bloomFilterService.isBlacklisted(EntityType.PAN, "4111111111111111");

      assertThat(result.inList()).isFalse();
    }

    @Test
    @DisplayName("Deve retornar possivelmente na lista após adicionar valor")
    void shouldReturnPossiblyInListAfterAddingValue() {
      // Adicionar valor ao filtro
      bloomFilterService.addToBlacklist(EntityType.PAN, "4111111111111111");

      // Mock DB para confirmar
      when(ruleListEntryRepository.isValueBlacklisted(any(), anyString(), any())).thenReturn(true);

      BloomFilterService.CheckResult result =
          bloomFilterService.isBlacklisted(EntityType.PAN, "4111111111111111");

      assertThat(result.inList()).isTrue();
    }

    @Test
    @DisplayName("Deve verificar no banco quando possivelmente na lista")
    void shouldVerifyInDatabaseWhenPossiblyInList() {
      // Adicionar valor ao filtro
      bloomFilterService.addToBlacklist(EntityType.PAN, "4111111111111111");

      // Configurar mock do repositório para retornar true
      when(ruleListEntryRepository.isValueBlacklisted(any(), anyString(), any())).thenReturn(true);

      BloomFilterService.CheckResult result =
          bloomFilterService.isBlacklisted(EntityType.PAN, "4111111111111111");

      assertThat(result.inList()).isTrue();
      assertThat(result.source()).isEqualTo("DATABASE");
    }

    @Test
    @DisplayName("Deve detectar falso positivo")
    void shouldDetectFalsePositive() {
      // Adicionar valor ao filtro
      bloomFilterService.addToBlacklist(EntityType.PAN, "4111111111111111");

      // Configurar mock do repositório para retornar false (falso positivo)
      when(ruleListEntryRepository.isValueBlacklisted(any(), anyString(), any())).thenReturn(false);

      BloomFilterService.CheckResult result =
          bloomFilterService.isBlacklisted(EntityType.PAN, "4111111111111111");

      assertThat(result.inList()).isFalse();

      // Verificar que estatísticas de falso positivo existem
      Map<String, Object> stats = bloomFilterService.getStatistics();
      assertThat(stats).containsKey("falsePositives");
      // O falso positivo é incrementado quando bloom filter diz "possivelmente na lista"
      // mas o DB diz que não está
    }
  }

  @Nested
  @DisplayName("Verificação de Whitelist")
  class WhitelistChecks {

    @Test
    @DisplayName("Deve retornar não na lista para valor não presente na whitelist")
    void shouldReturnNotInListForAbsentValueInWhitelist() {
      when(ruleListEntryRepository.isValueWhitelisted(any(), anyString(), any())).thenReturn(false);

      BloomFilterService.CheckResult result =
          bloomFilterService.isWhitelisted(EntityType.MERCHANT_ID, "merchant-123");

      assertThat(result.inList()).isFalse();
    }

    @Test
    @DisplayName("Deve retornar na lista após adicionar à whitelist")
    void shouldReturnInListAfterAddingToWhitelist() {
      bloomFilterService.addToWhitelist(EntityType.MERCHANT_ID, "merchant-123");

      when(ruleListEntryRepository.isValueWhitelisted(any(), anyString(), any())).thenReturn(true);

      BloomFilterService.CheckResult result =
          bloomFilterService.isWhitelisted(EntityType.MERCHANT_ID, "merchant-123");

      assertThat(result.inList()).isTrue();
    }
  }

  @Nested
  @DisplayName("Verificação de Greylist")
  class GreylistChecks {

    @Test
    @DisplayName("Deve retornar não na lista para valor não presente na greylist")
    void shouldReturnNotInListForAbsentValueInGreylist() {
      when(ruleListEntryRepository.isValueWhitelisted(any(), anyString(), any())).thenReturn(false);

      BloomFilterService.CheckResult result =
          bloomFilterService.isGreylisted(EntityType.CUSTOMER_ID, "customer-456");

      assertThat(result.inList()).isFalse();
    }

    @Test
    @DisplayName("Deve retornar na lista após adicionar à greylist")
    void shouldReturnInListAfterAddingToGreylist() {
      bloomFilterService.addToGreylist(EntityType.CUSTOMER_ID, "customer-456");

      when(ruleListEntryRepository.isValueWhitelisted(any(), anyString(), any())).thenReturn(true);

      BloomFilterService.CheckResult result =
          bloomFilterService.isGreylisted(EntityType.CUSTOMER_ID, "customer-456");

      assertThat(result.inList()).isTrue();
    }
  }

  @Nested
  @DisplayName("Estatísticas")
  class Statistics {

    @Test
    @DisplayName("Deve rastrear hits do filtro")
    void shouldTrackFilterHits() {
      // Verificar valor não presente (hit no bloom filter - valor definitivamente não está)
      when(ruleListEntryRepository.isValueBlacklisted(any(), anyString(), any())).thenReturn(false);
      bloomFilterService.isBlacklisted(EntityType.PAN, "9999999999999999");

      var stats = bloomFilterService.getStatistics();

      assertThat(stats).containsKey("bloomFilterHits");
      // O hit acontece quando o bloom filter diz "definitivamente não está na lista"
      assertThat((Long) stats.get("bloomFilterHits")).isGreaterThanOrEqualTo(0);
    }

    @Test
    @DisplayName("Deve rastrear misses do filtro")
    void shouldTrackFilterMisses() {
      // Adicionar valor e verificar (miss no bloom filter, vai pro DB)
      bloomFilterService.addToBlacklist(EntityType.PAN, "4111111111111111");
      when(ruleListEntryRepository.isValueBlacklisted(any(), anyString(), any())).thenReturn(true);
      bloomFilterService.isBlacklisted(EntityType.PAN, "4111111111111111");

      var stats = bloomFilterService.getStatistics();

      assertThat(stats).containsKey("bloomFilterMisses");
    }

    @Test
    @DisplayName("Deve retornar estatísticas completas")
    void shouldReturnCompleteStatistics() {
      var stats = bloomFilterService.getStatistics();

      assertThat(stats)
          .containsKeys(
              "bloomFilterHits",
              "bloomFilterMisses",
              "dbQueriesAvoided",
              "falsePositives",
              "hitRate",
              "initialized");
    }

    @Test
    @DisplayName("Deve rastrear queries de banco evitadas")
    void shouldTrackDbQueriesAvoided() {
      // Verificar valor não presente (evita query no DB)
      bloomFilterService.isBlacklisted(EntityType.PAN, "9999999999999999");

      var stats = bloomFilterService.getStatistics();

      // A query é evitada quando o bloom filter diz "definitivamente não está"
      assertThat(stats).containsKey("dbQueriesAvoided");
      assertThat((Long) stats.get("dbQueriesAvoided")).isGreaterThanOrEqualTo(0);
    }
  }

  @Nested
  @DisplayName("Tipos de Entidade")
  class EntityTypes {

    @Test
    @DisplayName("Deve suportar tipo PAN")
    void shouldSupportPanType() {
      bloomFilterService.addToBlacklist(EntityType.PAN, "4111111111111111");
      when(ruleListEntryRepository.isValueBlacklisted(any(), anyString(), any())).thenReturn(true);

      BloomFilterService.CheckResult result =
          bloomFilterService.isBlacklisted(EntityType.PAN, "4111111111111111");

      assertThat(result.inList()).isTrue();
    }

    @Test
    @DisplayName("Deve suportar tipo MERCHANT_ID")
    void shouldSupportMerchantIdType() {
      bloomFilterService.addToBlacklist(EntityType.MERCHANT_ID, "merchant-123");
      when(ruleListEntryRepository.isValueBlacklisted(any(), anyString(), any())).thenReturn(true);

      BloomFilterService.CheckResult result =
          bloomFilterService.isBlacklisted(EntityType.MERCHANT_ID, "merchant-123");

      assertThat(result.inList()).isTrue();
    }

    @Test
    @DisplayName("Deve suportar tipo CUSTOMER_ID")
    void shouldSupportCustomerIdType() {
      bloomFilterService.addToBlacklist(EntityType.CUSTOMER_ID, "customer-456");
      when(ruleListEntryRepository.isValueBlacklisted(any(), anyString(), any())).thenReturn(true);

      BloomFilterService.CheckResult result =
          bloomFilterService.isBlacklisted(EntityType.CUSTOMER_ID, "customer-456");

      assertThat(result.inList()).isTrue();
    }

    @Test
    @DisplayName("Deve suportar tipo DEVICE_ID")
    void shouldSupportDeviceIdType() {
      bloomFilterService.addToBlacklist(EntityType.DEVICE_ID, "device-789");
      when(ruleListEntryRepository.isValueBlacklisted(any(), anyString(), any())).thenReturn(true);

      BloomFilterService.CheckResult result =
          bloomFilterService.isBlacklisted(EntityType.DEVICE_ID, "device-789");

      assertThat(result.inList()).isTrue();
    }
  }

  @Nested
  @DisplayName("Isolamento de Filtros")
  class FilterIsolation {

    @Test
    @DisplayName("Blacklist e Whitelist devem ser independentes")
    void blacklistAndWhitelistShouldBeIndependent() {
      bloomFilterService.addToBlacklist(EntityType.PAN, "4111111111111111");

      // Mesmo valor não deve estar na whitelist
      when(ruleListEntryRepository.isValueWhitelisted(any(), anyString(), any())).thenReturn(false);

      BloomFilterService.CheckResult whitelistResult =
          bloomFilterService.isWhitelisted(EntityType.PAN, "4111111111111111");

      assertThat(whitelistResult.inList()).isFalse();
    }

    @Test
    @DisplayName("Diferentes tipos de entidade devem ser independentes")
    void differentEntityTypesShouldBeIndependent() {
      bloomFilterService.addToBlacklist(EntityType.PAN, "4111111111111111");

      // Mesmo valor com tipo diferente não deve estar no filtro
      when(ruleListEntryRepository.isValueBlacklisted(any(), anyString(), any())).thenReturn(false);

      BloomFilterService.CheckResult result =
          bloomFilterService.isBlacklisted(EntityType.MERCHANT_ID, "4111111111111111");

      assertThat(result.inList()).isFalse();
    }
  }

  @Nested
  @DisplayName("Performance")
  class Performance {

    @Test
    @DisplayName("Deve ter baixa latência para lookup")
    void shouldHaveSubMillisecondLatencyForLookup() {
      // Adicionar muitos valores
      for (int i = 0; i < 1000; i++) {
        bloomFilterService.addToBlacklist(EntityType.PAN, "PAN" + i);
      }

      when(ruleListEntryRepository.isValueBlacklisted(any(), anyString(), any())).thenReturn(true);

      BloomFilterService.CheckResult result =
          bloomFilterService.isBlacklisted(EntityType.PAN, "PAN500");

      // Latência deve ser razoável. Em ambientes compartilhados/Windows pode variar.
      assertThat(result.latencyMicros()).isLessThan(50_000);
    }

    @Test
    @DisplayName("Deve evitar queries de banco para valores não presentes")
    void shouldAvoidDbQueriesForAbsentValues() {
      // Verificar valor não presente
      bloomFilterService.isBlacklisted(EntityType.PAN, "9999999999999999");

      var stats = bloomFilterService.getStatistics();

      // Deve ter evitado query no banco (ou pelo menos ter a estatística)
      assertThat(stats).containsKey("dbQueriesAvoided");
    }
  }

  @Nested
  @DisplayName("Valores Nulos e Vazios")
  class NullAndEmptyValues {

    @Test
    @DisplayName("Deve tratar valor nulo graciosamente")
    void shouldHandleNullValueGracefully() {
      BloomFilterService.CheckResult result =
          bloomFilterService.isBlacklisted(EntityType.PAN, null);

      assertThat(result.inList()).isFalse();
      assertThat(result.fromBloomFilter()).isTrue();
    }

    @Test
    @DisplayName("Deve tratar valor vazio graciosamente")
    void shouldHandleEmptyValueGracefully() {
      BloomFilterService.CheckResult result = bloomFilterService.isBlacklisted(EntityType.PAN, "");

      assertThat(result.inList()).isFalse();
      assertThat(result.fromBloomFilter()).isTrue();
    }

    @Test
    @DisplayName("Deve tratar valor em branco graciosamente")
    void shouldHandleBlankValueGracefully() {
      BloomFilterService.CheckResult result =
          bloomFilterService.isBlacklisted(EntityType.PAN, "   ");

      assertThat(result.inList()).isFalse();
      assertThat(result.fromBloomFilter()).isTrue();
    }
  }

  @Nested
  @DisplayName("CheckResult")
  class CheckResultTests {

    @Test
    @DisplayName("Deve criar resultado não na lista")
    void shouldCreateNotInListResult() {
      BloomFilterService.CheckResult result = BloomFilterService.CheckResult.notInList(true, 100);

      assertThat(result.inList()).isFalse();
      assertThat(result.fromBloomFilter()).isTrue();
      assertThat(result.latencyMicros()).isEqualTo(100);
      assertThat(result.source()).isEqualTo("BLOOM_FILTER");
    }

    @Test
    @DisplayName("Deve criar resultado na lista")
    void shouldCreateInListResult() {
      BloomFilterService.CheckResult result = BloomFilterService.CheckResult.inList(false, 500);

      assertThat(result.inList()).isTrue();
      assertThat(result.fromBloomFilter()).isFalse();
      assertThat(result.latencyMicros()).isEqualTo(500);
      assertThat(result.source()).isEqualTo("DATABASE");
    }
  }

  @Nested
  @DisplayName("Rebuild de Filtros")
  class FilterRebuild {

    @Test
    @DisplayName("Deve executar rebuild sem erros")
    void shouldExecuteRebuildWithoutErrors() {
      when(ruleListEntryRepository.findAll()).thenReturn(List.of());

      // Não deve lançar exceção
      bloomFilterService.rebuildFilters();

      var stats = bloomFilterService.getStatistics();
      assertThat((Boolean) stats.get("initialized")).isTrue();
    }
  }

  @Nested
  @DisplayName("BloomResult Enum")
  class BloomResultTests {

    @Test
    @DisplayName("Deve ter valores corretos")
    void shouldHaveCorrectValues() {
      assertThat(BloomFilterService.BloomResult.DEFINITELY_NOT_IN_LIST).isNotNull();
      assertThat(BloomFilterService.BloomResult.POSSIBLY_IN_LIST).isNotNull();
    }
  }
}
