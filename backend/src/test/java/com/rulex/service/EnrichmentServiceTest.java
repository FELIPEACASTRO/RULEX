package com.rulex.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.when;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.BinLookup;
import com.rulex.entity.MccCategory;
import com.rulex.repository.BinLookupRepository;
import com.rulex.repository.MccCategoryRepository;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Testes unitários para EnrichmentService. Valida enriquecimento de BIN e MCC com fallback para
 * valores padrão.
 */
@ExtendWith(MockitoExtension.class)
class EnrichmentServiceTest {

  @Mock private BinLookupRepository binLookupRepository;

  @Mock private MccCategoryRepository mccCategoryRepository;

  private EnrichmentService enrichmentService;

  @BeforeEach
  void setUp() {
    enrichmentService = new EnrichmentService(binLookupRepository, mccCategoryRepository);
  }

  @Nested
  @DisplayName("enrichBin - Enriquecimento de BIN")
  class EnrichBinTests {

    @Test
    @DisplayName("Deve enriquecer BIN encontrado na tabela")
    void shouldEnrichFoundBin() {
      BinLookup lookup =
          BinLookup.builder()
              .bin("411111")
              .cardBrand("VISA")
              .cardType("CREDIT")
              .cardLevel("PLATINUM")
              .issuerName("Banco Teste")
              .issuerCountry("BRA")
              .issuerCountryNumeric("076")
              .isRegulated(false)
              .isCommercial(false)
              .isPrepaid(false)
              .build();

      when(binLookupRepository.findByBinPrefix("411111", "411111")).thenReturn(List.of(lookup));

      EnrichmentService.BinEnrichment result = enrichmentService.enrichBin("411111");

      assertThat(result.isFound()).isTrue();
      assertThat(result.getCardBrand()).isEqualTo("VISA");
      assertThat(result.getCardType()).isEqualTo("CREDIT");
      assertThat(result.getCardLevel()).isEqualTo("PLATINUM");
      assertThat(result.getIssuerName()).isEqualTo("Banco Teste");
      assertThat(result.getIssuerCountry()).isEqualTo("BRA");
      assertThat(result.getIssuerCountryNumeric()).isEqualTo("076");
    }

    @Test
    @DisplayName("Deve retornar UNKNOWN para BIN não encontrado")
    void shouldReturnUnknownForNotFoundBin() {
      when(binLookupRepository.findByBinPrefix(any(), any())).thenReturn(List.of());
      when(binLookupRepository.findByBin(any())).thenReturn(Optional.empty());

      EnrichmentService.BinEnrichment result = enrichmentService.enrichBin("999999");

      assertThat(result.isFound()).isFalse();
      assertThat(result.getCardBrand()).isEqualTo("UNKNOWN");
      assertThat(result.getCardType()).isEqualTo("UNKNOWN");
      assertThat(result.getIssuerName()).isEqualTo("UNKNOWN");
    }

    @Test
    @DisplayName("Deve retornar UNKNOWN para BIN null")
    void shouldReturnUnknownForNullBin() {
      EnrichmentService.BinEnrichment result = enrichmentService.enrichBin(null);

      assertThat(result.isFound()).isFalse();
      assertThat(result.getCardBrand()).isEqualTo("UNKNOWN");
    }

    @Test
    @DisplayName("Deve retornar UNKNOWN para BIN muito curto")
    void shouldReturnUnknownForShortBin() {
      EnrichmentService.BinEnrichment result = enrichmentService.enrichBin("4111");

      assertThat(result.isFound()).isFalse();
      assertThat(result.getCardBrand()).isEqualTo("UNKNOWN");
    }

    @Test
    @DisplayName("Deve converter BinEnrichment para Map")
    void shouldConvertToMap() {
      EnrichmentService.BinEnrichment enrichment =
          EnrichmentService.BinEnrichment.builder()
              .bin("411111")
              .cardBrand("VISA")
              .cardType("CREDIT")
              .found(true)
              .build();

      Map<String, Object> map = enrichment.toMap();

      assertThat(map).containsEntry("bin", "411111");
      assertThat(map).containsEntry("cardBrand", "VISA");
      assertThat(map).containsEntry("cardType", "CREDIT");
      assertThat(map).containsEntry("binFound", true);
    }
  }

  @Nested
  @DisplayName("enrichMcc - Enriquecimento de MCC")
  class EnrichMccTests {

    @Test
    @DisplayName("Deve enriquecer MCC encontrado na tabela")
    void shouldEnrichFoundMcc() {
      MccCategory category =
          MccCategory.builder()
              .mcc(7995)
              .category("GAMBLING")
              .subcategory("BETTING")
              .description("Apostas e jogos de azar")
              .riskLevel("CRITICAL")
              .isHighRisk(true)
              .isGambling(true)
              .isCrypto(false)
              .isAdult(false)
              .isCashAdvance(false)
              .build();

      when(mccCategoryRepository.findByMcc(7995)).thenReturn(Optional.of(category));

      EnrichmentService.MccEnrichment result = enrichmentService.enrichMcc(7995);

      assertThat(result.isFound()).isTrue();
      assertThat(result.getCategory()).isEqualTo("GAMBLING");
      assertThat(result.getSubcategory()).isEqualTo("BETTING");
      assertThat(result.getRiskLevel()).isEqualTo("CRITICAL");
      assertThat(result.isHighRisk()).isTrue();
      assertThat(result.isGambling()).isTrue();
    }

    @Test
    @DisplayName("Deve retornar UNKNOWN para MCC não encontrado")
    void shouldReturnUnknownForNotFoundMcc() {
      when(mccCategoryRepository.findByMcc(anyInt())).thenReturn(Optional.empty());

      EnrichmentService.MccEnrichment result = enrichmentService.enrichMcc(9999);

      assertThat(result.isFound()).isFalse();
      assertThat(result.getCategory()).isEqualTo("UNKNOWN");
      assertThat(result.getRiskLevel()).isEqualTo("UNKNOWN");
      assertThat(result.isHighRisk()).isFalse();
    }

    @Test
    @DisplayName("Deve retornar UNKNOWN para MCC null")
    void shouldReturnUnknownForNullMcc() {
      EnrichmentService.MccEnrichment result = enrichmentService.enrichMcc(null);

      assertThat(result.isFound()).isFalse();
      assertThat(result.getCategory()).isEqualTo("UNKNOWN");
    }

    @Test
    @DisplayName("Deve converter MccEnrichment para Map")
    void shouldConvertToMap() {
      EnrichmentService.MccEnrichment enrichment =
          EnrichmentService.MccEnrichment.builder()
              .mcc(7995)
              .category("GAMBLING")
              .riskLevel("CRITICAL")
              .isHighRisk(true)
              .isGambling(true)
              .found(true)
              .build();

      Map<String, Object> map = enrichment.toMap();

      assertThat(map).containsEntry("mcc", 7995);
      assertThat(map).containsEntry("mccCategory", "GAMBLING");
      assertThat(map).containsEntry("mccRiskLevel", "CRITICAL");
      assertThat(map).containsEntry("mccIsHighRisk", true);
      assertThat(map).containsEntry("mccIsGambling", true);
      assertThat(map).containsEntry("mccFound", true);
    }
  }

  @Nested
  @DisplayName("enrich - Enriquecimento Completo")
  class EnrichTests {

    @Test
    @DisplayName("Deve enriquecer transação completa")
    void shouldEnrichCompleteTransaction() {
      TransactionRequest request =
          TransactionRequest.builder()
              .externalTransactionId("TX-001")
              .customerIdFromHeader("CUST-001")
              .customerAcctNumber(123456789L)
              .pan("4111111111111111")
              .transactionAmount(BigDecimal.valueOf(1000))
              .transactionCurrencyCode(986)
              .transactionDate(20251231)
              .transactionTime(143052)
              .gmtOffset("-0300")
              .mcc(5411)
              .consumerAuthenticationScore(100)
              .externalScore3(100)
              .cavvResult(0)
              .eciIndicator(5)
              .atcCard(1)
              .atcHost(1)
              .tokenAssuranceLevel(0)
              .availableCredit(BigDecimal.valueOf(5000))
              .cardCashBalance(BigDecimal.ZERO)
              .cardDelinquentAmount(BigDecimal.ZERO)
              .build();

      BinLookup binLookup =
          BinLookup.builder()
              .bin("411111")
              .cardBrand("VISA")
              .cardType("CREDIT")
              .issuerCountry("BRA")
              .build();

      MccCategory mccCategory =
          MccCategory.builder()
              .mcc(5411)
              .category("RETAIL")
              .subcategory("GROCERY")
              .riskLevel("LOW")
              .isHighRisk(false)
              .build();

      when(binLookupRepository.findByBinPrefix(any(), any())).thenReturn(List.of(binLookup));
      when(mccCategoryRepository.findByMcc(5411)).thenReturn(Optional.of(mccCategory));

      EnrichmentService.EnrichmentContext result = enrichmentService.enrich(request);

      assertThat(result.getBinEnrichment().isFound()).isTrue();
      assertThat(result.getBinEnrichment().getCardBrand()).isEqualTo("VISA");
      assertThat(result.getMccEnrichment().isFound()).isTrue();
      assertThat(result.getMccEnrichment().getCategory()).isEqualTo("RETAIL");
      assertThat(result.getDerivedContext().getMaskedPan()).isEqualTo("411111******1111");
    }

    @Test
    @DisplayName("Deve retornar contexto vazio para request null")
    void shouldReturnEmptyContextForNullRequest() {
      EnrichmentService.EnrichmentContext result = enrichmentService.enrich(null);

      assertThat(result.getBinEnrichment().isFound()).isFalse();
      assertThat(result.getMccEnrichment().isFound()).isFalse();
    }

    @Test
    @DisplayName("Deve converter EnrichmentContext para Map")
    void shouldConvertContextToMap() {
      TransactionRequest request =
          TransactionRequest.builder()
              .externalTransactionId("TX-001")
              .customerIdFromHeader("CUST-001")
              .customerAcctNumber(123456789L)
              .pan("4111111111111111")
              .transactionAmount(BigDecimal.valueOf(100))
              .transactionCurrencyCode(986)
              .transactionDate(20251231)
              .transactionTime(120000)
              .mcc(5411)
              .consumerAuthenticationScore(100)
              .externalScore3(100)
              .cavvResult(0)
              .eciIndicator(5)
              .atcCard(1)
              .atcHost(1)
              .tokenAssuranceLevel(0)
              .availableCredit(BigDecimal.valueOf(5000))
              .cardCashBalance(BigDecimal.ZERO)
              .cardDelinquentAmount(BigDecimal.ZERO)
              .build();

      when(binLookupRepository.findByBinPrefix(any(), any())).thenReturn(List.of());
      when(binLookupRepository.findByBin(any())).thenReturn(Optional.empty());
      when(mccCategoryRepository.findByMcc(anyInt())).thenReturn(Optional.empty());

      EnrichmentService.EnrichmentContext context = enrichmentService.enrich(request);
      Map<String, Object> map = context.toMap();

      // BIN enrichment fields
      assertThat(map).containsKey("cardBrand");
      assertThat(map).containsKey("binFound");

      // MCC enrichment fields
      assertThat(map).containsKey("mccCategory");
      assertThat(map).containsKey("mccRiskLevel");

      // Derived context fields
      assertThat(map).containsKey("maskedPan");
      assertThat(map).containsKey("bin");
    }
  }

  @Nested
  @DisplayName("isHighRiskMcc - Verificação de MCC de Alto Risco")
  class IsHighRiskMccTests {

    @Test
    @DisplayName("Deve retornar true para MCC de alto risco na tabela")
    void shouldReturnTrueForHighRiskMccInTable() {
      when(mccCategoryRepository.isHighRisk(7995)).thenReturn(true);

      boolean result = enrichmentService.isHighRiskMcc(7995);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve retornar false para MCC não encontrado na tabela")
    void shouldReturnFalseForMccNotInTable() {
      when(mccCategoryRepository.isHighRisk(7995)).thenReturn(null);

      boolean result = enrichmentService.isHighRiskMcc(7995);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("Deve retornar false para MCC de baixo risco")
    void shouldReturnFalseForLowRiskMcc() {
      when(mccCategoryRepository.isHighRisk(5411)).thenReturn(false);

      boolean result = enrichmentService.isHighRiskMcc(5411);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("Deve retornar false para MCC null")
    void shouldReturnFalseForNullMcc() {
      boolean result = enrichmentService.isHighRiskMcc(null);

      assertThat(result).isFalse();
    }
  }

  @Nested
  @DisplayName("getMccRiskLevel - Nível de Risco do MCC")
  class GetMccRiskLevelTests {

    @Test
    @DisplayName("Deve retornar nível de risco da tabela")
    void shouldReturnRiskLevelFromTable() {
      when(mccCategoryRepository.getRiskLevel(7995)).thenReturn("CRITICAL");

      String result = enrichmentService.getMccRiskLevel(7995);

      assertThat(result).isEqualTo("CRITICAL");
    }

    @Test
    @DisplayName("Deve retornar UNKNOWN para MCC não encontrado")
    void shouldReturnUnknownForNotFoundMcc() {
      when(mccCategoryRepository.getRiskLevel(9999)).thenReturn(null);

      String result = enrichmentService.getMccRiskLevel(9999);

      assertThat(result).isEqualTo("UNKNOWN");
    }

    @Test
    @DisplayName("Deve retornar UNKNOWN para MCC null")
    void shouldReturnUnknownForNullMcc() {
      String result = enrichmentService.getMccRiskLevel(null);

      assertThat(result).isEqualTo("UNKNOWN");
    }
  }
}
