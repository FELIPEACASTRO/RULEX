package com.rulex.service.enrichment;

import com.rulex.dto.TransactionRequest;
import com.rulex.service.EnrichmentService;
import com.rulex.service.EnrichmentService.BinEnrichment;
import java.time.LocalDate;
import java.time.YearMonth;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Enriquecimento de dados do cartão para regras de fraude.
 *
 * <p>Consolida todos os campos de cartão em um formato fácil de usar pelo motor de regras. Campos
 * disponíveis:
 *
 * <ul>
 *   <li>card.is_expired - Se o cartão está expirado
 *   <li>card.days_to_expire - Dias até expiração (negativo se expirado)
 *   <li>card.has_chip - Se o cartão tem chip
 *   <li>card.is_contactless - Se o cartão é contactless
 *   <li>card.is_virtual - Se é cartão virtual
 *   <li>card.is_prepaid - Se é cartão pré-pago
 *   <li>card.is_commercial - Se é cartão comercial
 *   <li>card.bin_blacklisted - Se o BIN está em lista negra
 *   <li>card.testing_pattern - Se há padrão de teste de cartão
 *   <li>card.brand - Bandeira do cartão
 *   <li>card.type - Tipo (crédito/débito)
 *   <li>card.level - Nível (classic/gold/platinum)
 * </ul>
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class CardEnrichment {

  private final EnrichmentService enrichmentService;

  // BINs conhecidos como problemáticos (exemplo - em produção seria uma tabela)
  private static final Set<String> BLACKLISTED_BINS =
      Set.of(
          "400000", // BIN de teste Visa
          "411111", // BIN de teste comum
          "555555", // BIN de teste Mastercard
          "378282" // BIN de teste Amex
          );

  // Padrões de PAN de teste
  private static final Set<String> TEST_PAN_PATTERNS =
      Set.of(
          "4111111111111111", // Visa test
          "5500000000000004", // Mastercard test
          "340000000000009", // Amex test
          "30000000000004", // Diners test
          "6011000000000004" // Discover test
          );

  /** Resultado do enriquecimento de cartão. */
  @Data
  @Builder
  public static class CardContext {
    // Expiração
    private final boolean isExpired;
    private final int daysToExpire;
    private final String expirationDate;

    // Características físicas
    private final boolean hasChip;
    private final boolean isContactless;
    private final boolean isVirtual;
    private final boolean isTokenized;

    // Tipo de cartão (do BIN)
    private final boolean isPrepaid;
    private final boolean isCommercial;
    private final boolean isRegulated;
    private final String cardType;
    private final String cardLevel;
    private final String cardBrand;

    // Emissor
    private final String issuerName;
    private final String issuerCountry;
    private final boolean isDomesticIssuer;

    // Segurança
    private final boolean isBinBlacklisted;
    private final boolean isTestCard;
    private final boolean hasTestingPattern;

    // BIN
    private final String bin;
    private final boolean binFound;

    // Score
    private final int riskScore;

    /** Converte para Map para uso no evaluator. */
    public Map<String, Object> toMap() {
      Map<String, Object> map = new HashMap<>();

      // Expiração
      map.put("card.is_expired", isExpired);
      map.put("card.days_to_expire", daysToExpire);
      map.put("card.expiration_date", expirationDate);

      // Aliases
      map.put("cardExpired", isExpired);
      map.put("daysToExpire", daysToExpire);

      // Características
      map.put("card.has_chip", hasChip);
      map.put("card.is_contactless", isContactless);
      map.put("card.is_virtual", isVirtual);
      map.put("card.is_tokenized", isTokenized);

      // Aliases
      map.put("cardHasChip", hasChip);
      map.put("cardIsVirtual", isVirtual);

      // Tipo
      map.put("card.is_prepaid", isPrepaid);
      map.put("card.is_commercial", isCommercial);
      map.put("card.is_regulated", isRegulated);
      map.put("card.type", cardType);
      map.put("card.level", cardLevel);
      map.put("card.brand", cardBrand);

      // Aliases
      map.put("cardIsPrepaid", isPrepaid);
      map.put("cardIsCommercial", isCommercial);
      map.put("cardType", cardType);
      map.put("cardBrand", cardBrand);

      // Emissor
      map.put("card.issuer_name", issuerName);
      map.put("card.issuer_country", issuerCountry);
      map.put("card.is_domestic_issuer", isDomesticIssuer);

      // Segurança
      map.put("card.bin_blacklisted", isBinBlacklisted);
      map.put("card.is_test_card", isTestCard);
      map.put("card.has_testing_pattern", hasTestingPattern);

      // Aliases
      map.put("binBlacklisted", isBinBlacklisted);
      map.put("cardTestingPattern", hasTestingPattern);

      // BIN
      map.put("card.bin", bin);
      map.put("card.bin_found", binFound);

      // Score
      map.put("card.risk_score", riskScore);
      map.put("cardRiskScore", riskScore);

      return map;
    }

    public static CardContext empty() {
      return CardContext.builder()
          .isExpired(false)
          .daysToExpire(0)
          .expirationDate(null)
          .hasChip(false)
          .isContactless(false)
          .isVirtual(false)
          .isTokenized(false)
          .isPrepaid(false)
          .isCommercial(false)
          .isRegulated(false)
          .cardType("UNKNOWN")
          .cardLevel("UNKNOWN")
          .cardBrand("UNKNOWN")
          .issuerName("UNKNOWN")
          .issuerCountry("UNKNOWN")
          .isDomesticIssuer(false)
          .isBinBlacklisted(false)
          .isTestCard(false)
          .hasTestingPattern(false)
          .bin(null)
          .binFound(false)
          .riskScore(0)
          .build();
    }
  }

  /**
   * Enriquece uma transação com dados do cartão.
   *
   * @param request A transação a ser enriquecida
   * @param merchantCountry País do merchant (para verificar se emissor é doméstico)
   * @return Contexto de cartão com todos os campos calculados
   */
  public CardContext enrich(TransactionRequest request, String merchantCountry) {
    if (request == null) {
      log.debug("TransactionRequest é null, retornando contexto vazio");
      return CardContext.empty();
    }

    try {
      // Extrair BIN do PAN
      String pan = request.getPan();
      String bin = extractBin(pan);

      // Buscar enriquecimento de BIN
      BinEnrichment binEnrichment = enrichmentService.enrichBin(bin);

      // Analisar expiração
      ExpirationInfo expInfo = analyzeExpiration(request.getCardExpireDate());

      // Analisar características do cartão
      boolean hasChip = hasChip(request);
      boolean isContactless = isContactless(request);
      boolean isVirtual = isVirtual(request);
      boolean isTokenized = isTokenized(request);

      // Verificar segurança
      boolean isBinBlacklisted = BLACKLISTED_BINS.contains(bin);
      boolean isTestCard = isTestCard(pan);
      boolean hasTestingPattern = detectTestingPattern(request);

      // Verificar se emissor é doméstico
      boolean isDomesticIssuer =
          isDomesticIssuer(binEnrichment.getIssuerCountry(), merchantCountry);

      // Calcular score de risco
      int riskScore =
          calculateRiskScore(
              expInfo.isExpired,
              expInfo.daysToExpire,
              binEnrichment.isPrepaid(),
              isBinBlacklisted,
              isTestCard,
              hasTestingPattern,
              binEnrichment.isFound());

      return CardContext.builder()
          .isExpired(expInfo.isExpired)
          .daysToExpire(expInfo.daysToExpire)
          .expirationDate(expInfo.expirationDate)
          .hasChip(hasChip)
          .isContactless(isContactless)
          .isVirtual(isVirtual)
          .isTokenized(isTokenized)
          .isPrepaid(binEnrichment.isPrepaid())
          .isCommercial(binEnrichment.isCommercial())
          .isRegulated(binEnrichment.isRegulated())
          .cardType(binEnrichment.getCardType())
          .cardLevel(binEnrichment.getCardLevel())
          .cardBrand(binEnrichment.getCardBrand())
          .issuerName(binEnrichment.getIssuerName())
          .issuerCountry(binEnrichment.getIssuerCountry())
          .isDomesticIssuer(isDomesticIssuer)
          .isBinBlacklisted(isBinBlacklisted)
          .isTestCard(isTestCard)
          .hasTestingPattern(hasTestingPattern)
          .bin(bin)
          .binFound(binEnrichment.isFound())
          .riskScore(riskScore)
          .build();

    } catch (Exception e) {
      log.warn("Erro ao enriquecer card: {}", e.getMessage());
      return CardContext.empty();
    }
  }

  /** Versão simplificada sem país do merchant. */
  public CardContext enrich(TransactionRequest request) {
    return enrich(request, null);
  }

  /** Informações de expiração. */
  private static class ExpirationInfo {
    boolean isExpired = false;
    int daysToExpire = 0;
    String expirationDate = null;
  }

  /** Extrai BIN do PAN. */
  private String extractBin(String pan) {
    if (pan == null || pan.length() < 6) return null;
    String digitsOnly = pan.replaceAll("[^0-9]", "");
    return digitsOnly.length() >= 6 ? digitsOnly.substring(0, 6) : null;
  }

  /** Analisa data de expiração. */
  private ExpirationInfo analyzeExpiration(Integer expireDate) {
    ExpirationInfo info = new ExpirationInfo();

    if (expireDate == null) {
      return info;
    }

    try {
      // Formato esperado: YYMM ou MMYY
      String expStr = String.format("%04d", expireDate);
      int year;
      int month;

      // Tentar detectar formato
      if (expStr.length() == 4) {
        // Assumir YYMM
        year = 2000 + Integer.parseInt(expStr.substring(0, 2));
        month = Integer.parseInt(expStr.substring(2, 4));

        // Se mês > 12, provavelmente é MMYY
        if (month > 12) {
          month = Integer.parseInt(expStr.substring(0, 2));
          year = 2000 + Integer.parseInt(expStr.substring(2, 4));
        }
      } else {
        return info;
      }

      YearMonth expiration = YearMonth.of(year, month);
      YearMonth now = YearMonth.now();

      info.expirationDate = expiration.format(DateTimeFormatter.ofPattern("MM/yy"));
      info.isExpired = expiration.isBefore(now);

      // Calcular dias até expiração (último dia do mês)
      LocalDate expDate = expiration.atEndOfMonth();
      LocalDate today = LocalDate.now();
      info.daysToExpire = (int) ChronoUnit.DAYS.between(today, expDate);

    } catch (Exception e) {
      log.debug("Erro ao parsear data de expiração {}: {}", expireDate, e.getMessage());
    }

    return info;
  }

  /** Verifica se cartão tem chip. */
  private boolean hasChip(TransactionRequest request) {
    String posEntryMode = request.getPosEntryMode();
    if (posEntryMode == null) return false;

    // Códigos de entrada que indicam chip
    return posEntryMode.startsWith("05") // Chip
        || posEntryMode.startsWith("07") // Contactless chip
        || posEntryMode.startsWith("95"); // Chip fallback
  }

  /** Verifica se é transação contactless. */
  private boolean isContactless(TransactionRequest request) {
    String posEntryMode = request.getPosEntryMode();
    if (posEntryMode == null) return false;

    return posEntryMode.startsWith("07") // Contactless chip
        || posEntryMode.startsWith("91"); // Contactless mag stripe
  }

  /** Verifica se é cartão virtual. */
  private boolean isVirtual(TransactionRequest request) {
    String cardMediaType = request.getCardMediaType();
    if (cardMediaType == null) return false;

    return "VIRTUAL".equalsIgnoreCase(cardMediaType)
        || "V".equalsIgnoreCase(cardMediaType)
        || "DIGITAL".equalsIgnoreCase(cardMediaType);
  }

  /** Verifica se é transação tokenizada. */
  private boolean isTokenized(TransactionRequest request) {
    String tokenIndicator = request.getTokenizationIndicator();
    return tokenIndicator != null
        && ("Y".equalsIgnoreCase(tokenIndicator)
            || "1".equals(tokenIndicator)
            || "TRUE".equalsIgnoreCase(tokenIndicator));
  }

  /** Verifica se é cartão de teste. */
  private boolean isTestCard(String pan) {
    if (pan == null) return false;
    String digitsOnly = pan.replaceAll("[^0-9]", "");
    return TEST_PAN_PATTERNS.contains(digitsOnly);
  }

  /** Detecta padrão de teste de cartão. */
  private boolean detectTestingPattern(TransactionRequest request) {
    // Padrões que indicam teste:
    // 1. Valor muito baixo (< R$ 1)
    // 2. Valor exato de R$ 1
    // 3. Múltiplas transações de mesmo valor baixo

    if (request.getTransactionAmount() == null) return false;

    double amount = request.getTransactionAmount().doubleValue();

    // Valores típicos de teste
    return amount == 0.01 || amount == 0.10 || amount == 1.00 || amount == 0.50 || amount < 0.01;
  }

  /** Verifica se emissor é doméstico. */
  private boolean isDomesticIssuer(String issuerCountry, String merchantCountry) {
    if (issuerCountry == null || merchantCountry == null) return false;
    return issuerCountry.equalsIgnoreCase(merchantCountry);
  }

  /** Calcula score de risco do cartão. */
  private int calculateRiskScore(
      boolean isExpired,
      int daysToExpire,
      boolean isPrepaid,
      boolean isBinBlacklisted,
      boolean isTestCard,
      boolean hasTestingPattern,
      boolean binFound) {

    int score = 0;

    // Cartão expirado é crítico
    if (isExpired) score += 50;

    // Próximo de expirar
    if (daysToExpire > 0 && daysToExpire < 30) score += 10;

    // BIN em lista negra
    if (isBinBlacklisted) score += 40;

    // Cartão de teste
    if (isTestCard) score += 30;

    // Padrão de teste
    if (hasTestingPattern) score += 20;

    // Pré-pago tem risco maior
    if (isPrepaid) score += 15;

    // BIN não encontrado
    if (!binFound) score += 10;

    return Math.min(100, score);
  }
}
