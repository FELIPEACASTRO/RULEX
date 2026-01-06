package com.rulex.service;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.BinLookup;
import com.rulex.entity.MccCategory;
import com.rulex.repository.BinLookupRepository;
import com.rulex.repository.MccCategoryRepository;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * Serviço de enriquecimento de transações. Adiciona informações derivadas do BIN e MCC sem alterar
 * o payload original.
 *
 * <p>Todos os enriquecimentos têm fallback para valores padrão ("UNKNOWN") para garantir que o
 * fluxo nunca seja interrompido por falta de dados.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class EnrichmentService {

  private final BinLookupRepository binLookupRepository;
  private final MccCategoryRepository mccCategoryRepository;

  /** Resultado do enriquecimento de BIN. */
  @Data
  @Builder
  public static class BinEnrichment {
    private final String bin;
    private final String cardBrand;
    private final String cardType;
    private final String cardLevel;
    private final String issuerName;
    private final String issuerCountry;
    private final String issuerCountryNumeric;
    private final boolean isRegulated;
    private final boolean isCommercial;
    private final boolean isPrepaid;
    private final boolean found;

    public static BinEnrichment unknown(String bin) {
      return BinEnrichment.builder()
          .bin(bin)
          .cardBrand("UNKNOWN")
          .cardType("UNKNOWN")
          .cardLevel("UNKNOWN")
          .issuerName("UNKNOWN")
          .issuerCountry("UNKNOWN")
          .issuerCountryNumeric("UNKNOWN")
          .isRegulated(false)
          .isCommercial(false)
          .isPrepaid(false)
          .found(false)
          .build();
    }

    public Map<String, Object> toMap() {
      Map<String, Object> map = new HashMap<>();
      map.put("bin", bin);
      map.put("cardBrand", cardBrand);
      map.put("cardType", cardType);
      map.put("cardLevel", cardLevel);
      map.put("issuerName", issuerName);
      map.put("issuerCountry", issuerCountry);
      map.put("issuerCountryNumeric", issuerCountryNumeric);
      map.put("isRegulated", isRegulated);
      map.put("isCommercial", isCommercial);
      map.put("isPrepaid", isPrepaid);
      map.put("binFound", found);
      return map;
    }
  }

  /** Resultado do enriquecimento de MCC. */
  @Data
  @Builder
  public static class MccEnrichment {
    private final Integer mcc;
    private final String category;
    private final String subcategory;
    private final String description;
    private final String riskLevel;
    private final boolean isHighRisk;
    private final boolean isGambling;
    private final boolean isCrypto;
    private final boolean isAdult;
    private final boolean isCashAdvance;
    private final boolean found;

    public static MccEnrichment unknown(Integer mcc) {
      return MccEnrichment.builder()
          .mcc(mcc)
          .category("UNKNOWN")
          .subcategory("UNKNOWN")
          .description("MCC não categorizado")
          .riskLevel("UNKNOWN")
          .isHighRisk(false)
          .isGambling(false)
          .isCrypto(false)
          .isAdult(false)
          .isCashAdvance(false)
          .found(false)
          .build();
    }

    public Map<String, Object> toMap() {
      Map<String, Object> map = new HashMap<>();
      map.put("mcc", mcc);
      map.put("mccCategory", category);
      map.put("mccSubcategory", subcategory);
      map.put("mccDescription", description);
      map.put("mccRiskLevel", riskLevel);
      map.put("mccIsHighRisk", isHighRisk);
      map.put("mccIsGambling", isGambling);
      map.put("mccIsCrypto", isCrypto);
      map.put("mccIsAdult", isAdult);
      map.put("mccIsCashAdvance", isCashAdvance);
      map.put("mccFound", found);
      return map;
    }
  }

  /** Contexto de enriquecimento completo. */
  @Data
  @Builder
  public static class EnrichmentContext {
    private final BinEnrichment binEnrichment;
    private final MccEnrichment mccEnrichment;
    private final DerivedContext derivedContext;

    public Map<String, Object> toMap() {
      Map<String, Object> map = new HashMap<>();
      if (binEnrichment != null) {
        map.putAll(binEnrichment.toMap());
      }
      if (mccEnrichment != null) {
        map.putAll(mccEnrichment.toMap());
      }
      if (derivedContext != null) {
        map.putAll(derivedContext.toMap());
      }
      return map;
    }
  }

  /**
   * Enriquece uma transação com informações de BIN e MCC.
   *
   * @param request O payload original da transação (não modificado)
   * @return Contexto de enriquecimento com todas as informações derivadas
   */
  public EnrichmentContext enrich(TransactionRequest request) {
    if (request == null) {
      log.warn("TransactionRequest é null, retornando contexto de enriquecimento vazio");
      return EnrichmentContext.builder()
          .binEnrichment(BinEnrichment.unknown(null))
          .mccEnrichment(MccEnrichment.unknown(null))
          .derivedContext(DerivedContext.from(null))
          .build();
    }

    // Derivar contexto básico (datas, PAN mascarado, etc.)
    DerivedContext derivedContext = DerivedContext.from(request);

    // Enriquecer BIN
    BinEnrichment binEnrichment = enrichBin(derivedContext.getBin());

    // Enriquecer MCC
    MccEnrichment mccEnrichment = enrichMcc(request.getMcc());

    log.debug(
        "Enriquecimento completo: bin={}, cardBrand={}, mcc={}, mccCategory={}, mccRiskLevel={}",
        derivedContext.getBin(),
        binEnrichment.getCardBrand(),
        request.getMcc(),
        mccEnrichment.getCategory(),
        mccEnrichment.getRiskLevel());

    return EnrichmentContext.builder()
        .binEnrichment(binEnrichment)
        .mccEnrichment(mccEnrichment)
        .derivedContext(derivedContext)
        .build();
  }

  /**
   * Enriquece informações do BIN.
   *
   * @param bin O BIN extraído do PAN (6-8 dígitos)
   * @return Informações do BIN ou valores padrão se não encontrado
   */
  public BinEnrichment enrichBin(String bin) {
    if (bin == null || bin.length() < 6) {
      log.debug("BIN inválido ou muito curto: {}", bin);
      return BinEnrichment.unknown(bin);
    }

    try {
      // Tentar buscar com 8 dígitos primeiro, depois com 6
      String bin8 = bin.length() >= 8 ? bin.substring(0, 8) : bin;
      String bin6 = bin.substring(0, 6);

      List<BinLookup> results = binLookupRepository.findByBinPrefix(bin8, bin6);

      if (results.isEmpty()) {
        // Tentar busca exata com 6 dígitos
        Optional<BinLookup> exact = binLookupRepository.findByBin(bin6);
        if (exact.isEmpty()) {
          log.debug("BIN não encontrado na tabela de lookup: {}", bin);
          return BinEnrichment.unknown(bin);
        }
        results = List.of(exact.get());
      }

      BinLookup lookup = results.get(0);
      return BinEnrichment.builder()
          .bin(bin)
          .cardBrand(lookup.getCardBrand() != null ? lookup.getCardBrand() : "UNKNOWN")
          .cardType(lookup.getCardType() != null ? lookup.getCardType() : "UNKNOWN")
          .cardLevel(lookup.getCardLevel() != null ? lookup.getCardLevel() : "UNKNOWN")
          .issuerName(lookup.getIssuerName() != null ? lookup.getIssuerName() : "UNKNOWN")
          .issuerCountry(lookup.getIssuerCountry() != null ? lookup.getIssuerCountry() : "UNKNOWN")
          .issuerCountryNumeric(
              lookup.getIssuerCountryNumeric() != null
                  ? lookup.getIssuerCountryNumeric()
                  : "UNKNOWN")
          .isRegulated(Boolean.TRUE.equals(lookup.getIsRegulated()))
          .isCommercial(Boolean.TRUE.equals(lookup.getIsCommercial()))
          .isPrepaid(Boolean.TRUE.equals(lookup.getIsPrepaid()))
          .found(true)
          .build();

    } catch (Exception e) {
      log.warn("Erro ao buscar BIN {}: {}", bin, e.getMessage());
      return BinEnrichment.unknown(bin);
    }
  }

  /**
   * Enriquece informações do MCC.
   *
   * @param mcc O Merchant Category Code
   * @return Informações do MCC ou valores padrão se não encontrado
   */
  public MccEnrichment enrichMcc(Integer mcc) {
    if (mcc == null) {
      log.debug("MCC é null");
      return MccEnrichment.unknown(null);
    }

    try {
      Optional<MccCategory> categoryOpt = mccCategoryRepository.findByMcc(mcc);

      if (categoryOpt.isEmpty()) {
        log.debug("MCC não encontrado na tabela de categorização: {}", mcc);
        return MccEnrichment.unknown(mcc);
      }

      MccCategory category = categoryOpt.get();
      return MccEnrichment.builder()
          .mcc(mcc)
          .category(category.getCategory() != null ? category.getCategory() : "UNKNOWN")
          .subcategory(category.getSubcategory())
          .description(category.getDescription())
          .riskLevel(category.getRiskLevel() != null ? category.getRiskLevel() : "UNKNOWN")
          .isHighRisk(Boolean.TRUE.equals(category.getIsHighRisk()))
          .isGambling(Boolean.TRUE.equals(category.getIsGambling()))
          .isCrypto(Boolean.TRUE.equals(category.getIsCrypto()))
          .isAdult(Boolean.TRUE.equals(category.getIsAdult()))
          .isCashAdvance(Boolean.TRUE.equals(category.getIsCashAdvance()))
          .found(true)
          .build();

    } catch (Exception e) {
      log.warn("Erro ao buscar MCC {}: {}", mcc, e.getMessage());
      return MccEnrichment.unknown(mcc);
    }
  }

  /**
   * Verifica se um MCC é de alto risco (com fallback para lista hardcoded).
   *
   * @param mcc O Merchant Category Code
   * @return true se for alto risco
   */
  public boolean isHighRiskMcc(Integer mcc) {
    if (mcc == null) {
      return false;
    }

    try {
      // Primeiro tentar na tabela
      Boolean isHighRisk = mccCategoryRepository.isHighRisk(mcc);
      return Boolean.TRUE.equals(isHighRisk);
    } catch (Exception e) {
      log.debug("Erro ao verificar MCC na tabela: {}", e.getMessage());
      return false;
    }
  }

  /**
   * Retorna o nível de risco de um MCC.
   *
   * @param mcc O Merchant Category Code
   * @return Nível de risco (LOW, MEDIUM, HIGH, CRITICAL, UNKNOWN)
   */
  public String getMccRiskLevel(Integer mcc) {
    if (mcc == null) {
      return "UNKNOWN";
    }

    try {
      String riskLevel = mccCategoryRepository.getRiskLevel(mcc);
      return riskLevel != null ? riskLevel : "UNKNOWN";
    } catch (Exception e) {
      log.debug("Erro ao buscar nível de risco do MCC: {}", e.getMessage());
      return "UNKNOWN";
    }
  }
}
