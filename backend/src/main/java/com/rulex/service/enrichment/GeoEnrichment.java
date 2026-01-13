package com.rulex.service.enrichment;

import com.rulex.dto.TransactionRequest;
import com.rulex.service.GeoService;
import com.rulex.service.GeoService.GeoCoordinates;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Enriquecimento de geolocalização para regras de fraude.
 *
 * <p>Consolida todos os campos de geo em um formato fácil de usar pelo motor de regras. Campos
 * disponíveis:
 *
 * <ul>
 *   <li>geo.latitude - Latitude derivada da localização do merchant
 *   <li>geo.longitude - Longitude derivada da localização do merchant
 *   <li>geo.country - País normalizado
 *   <li>geo.state - Estado normalizado
 *   <li>geo.city - Cidade normalizada
 *   <li>geo.ip_country - País do IP (se disponível)
 *   <li>geo.ip_country_mismatch - IP de país diferente do merchant
 *   <li>geo.travel_speed_kmh - Velocidade de viagem desde última transação
 *   <li>geo.travel_distance_km - Distância desde última transação
 *   <li>geo.is_impossible_travel - Flag de viagem impossível
 *   <li>geo.is_high_risk_country - País em lista de alto risco
 *   <li>geo.is_sanctioned_country - País sancionado (OFAC/FATF)
 *   <li>geo.is_domestic - Transação doméstica
 *   <li>geo.is_cross_border - Transação internacional
 *   <li>geo.region_risk_score - Score de risco da região
 * </ul>
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class GeoEnrichment {

  private final GeoService geoService;

  // Países de alto risco (FATF + outros)
  private static final Set<String> HIGH_RISK_COUNTRIES =
      Set.of(
          "AF", "004", // Afeganistão
          "BY", "112", // Belarus
          "CF", "140", // República Centro-Africana
          "CU", "192", // Cuba
          "CD", "180", // Congo
          "IR", "364", // Irã
          "IQ", "368", // Iraque
          "KP", "408", // Coreia do Norte
          "LY", "434", // Líbia
          "ML", "466", // Mali
          "MM", "104", // Myanmar
          "NI", "558", // Nicarágua
          "RU", "643", // Rússia
          "SO", "706", // Somália
          "SS", "728", // Sudão do Sul
          "SD", "736", // Sudão
          "SY", "760", // Síria
          "VE", "862", // Venezuela
          "YE", "887", // Iêmen
          "ZW", "716" // Zimbábue
          );

  // Países sancionados (OFAC)
  private static final Set<String> SANCTIONED_COUNTRIES =
      Set.of(
          "CU", "192", // Cuba
          "IR", "364", // Irã
          "KP", "408", // Coreia do Norte
          "SY", "760", // Síria
          "RU", "643" // Rússia (parcial)
          );

  /** Resultado do enriquecimento de geo. */
  @Data
  @Builder
  public static class GeoContext {
    // Coordenadas
    private final Double latitude;
    private final Double longitude;
    private final String coordinateSource;

    // Localização normalizada
    private final String country;
    private final String countryCode;
    private final String state;
    private final String city;
    private final String postalCode;

    // IP
    private final String ipCountry;
    private final boolean ipCountryMismatch;

    // Viagem
    private final double travelSpeedKmh;
    private final double travelDistanceKm;
    private final int timeSinceLastMinutes;
    private final boolean isImpossibleTravel;

    // Risco de país
    private final boolean isHighRiskCountry;
    private final boolean isSanctionedCountry;
    private final int countryRiskScore;

    // Tipo de transação
    private final boolean isDomestic;
    private final boolean isCrossBorder;
    private final String homeCountry;

    // Score geral
    private final int regionRiskScore;

    /** Converte para Map para uso no evaluator. */
    public Map<String, Object> toMap() {
      Map<String, Object> map = new HashMap<>();

      // Coordenadas
      map.put("geo.latitude", latitude);
      map.put("geo.longitude", longitude);
      map.put("geo.coordinate_source", coordinateSource);

      // Localização
      map.put("geo.country", country);
      map.put("geo.country_code", countryCode);
      map.put("geo.state", state);
      map.put("geo.city", city);
      map.put("geo.postal_code", postalCode);

      // IP
      map.put("geo.ip_country", ipCountry);
      map.put("geo.ip_country_mismatch", ipCountryMismatch);

      // Aliases
      map.put("ipCountryMismatch", ipCountryMismatch);

      // Viagem
      map.put("geo.travel_speed_kmh", travelSpeedKmh);
      map.put("geo.travel_distance_km", travelDistanceKm);
      map.put("geo.time_since_last_minutes", timeSinceLastMinutes);
      map.put("geo.is_impossible_travel", isImpossibleTravel);

      // Aliases
      map.put("geoVelocityKmH", travelSpeedKmh);
      map.put("travelSpeedKmh", travelSpeedKmh);
      map.put("impossibleTravel", isImpossibleTravel);

      // Risco de país
      map.put("geo.is_high_risk_country", isHighRiskCountry);
      map.put("geo.is_sanctioned_country", isSanctionedCountry);
      map.put("geo.country_risk_score", countryRiskScore);

      // Aliases
      map.put("highRiskCountry", isHighRiskCountry);
      map.put("sanctionedCountry", isSanctionedCountry);

      // Tipo de transação
      map.put("geo.is_domestic", isDomestic);
      map.put("geo.is_cross_border", isCrossBorder);
      map.put("geo.home_country", homeCountry);

      // Score
      map.put("geo.region_risk_score", regionRiskScore);
      map.put("regionRiskScore", regionRiskScore);

      return map;
    }

    public static GeoContext empty() {
      return GeoContext.builder()
          .latitude(null)
          .longitude(null)
          .coordinateSource("NONE")
          .country(null)
          .countryCode(null)
          .state(null)
          .city(null)
          .postalCode(null)
          .ipCountry(null)
          .ipCountryMismatch(false)
          .travelSpeedKmh(0)
          .travelDistanceKm(0)
          .timeSinceLastMinutes(0)
          .isImpossibleTravel(false)
          .isHighRiskCountry(false)
          .isSanctionedCountry(false)
          .countryRiskScore(0)
          .isDomestic(true)
          .isCrossBorder(false)
          .homeCountry(null)
          .regionRiskScore(0)
          .build();
    }
  }

  /**
   * Enriquece uma transação com dados de geolocalização.
   *
   * @param request A transação a ser enriquecida
   * @param ipCountry País do IP (opcional, do header ou serviço externo)
   * @param homeCountry País de origem do cliente (opcional)
   * @return Contexto de geo com todos os campos calculados
   */
  public GeoContext enrich(TransactionRequest request, String ipCountry, String homeCountry) {
    if (request == null) {
      log.debug("TransactionRequest é null, retornando contexto vazio");
      return GeoContext.empty();
    }

    try {
      // Obter coordenadas do merchant usando deriveCoordinates
      GeoCoordinates coords = geoService.deriveCoordinates(request);

      // Normalizar localização
      String countryCode = normalizeCountryCode(request.getMerchantCountryCode());
      String state = normalizeState(request.getMerchantState());
      String city = normalizeCity(request.getMerchantCity());

      // Verificar mismatch de IP
      boolean ipMismatch = checkIpCountryMismatch(countryCode, ipCountry);

      // Verificar risco de país
      boolean isHighRisk = isHighRiskCountry(countryCode);
      boolean isSanctioned = isSanctionedCountry(countryCode);
      int countryRiskScore = calculateCountryRiskScore(countryCode);

      // Verificar tipo de transação
      boolean isDomestic = isDomesticTransaction(countryCode, homeCountry);
      boolean isCrossBorder = !isDomestic;

      // Por enquanto, não temos análise de viagem impossível integrada
      // TODO: Integrar com ImpossibleTravelService quando a API estiver disponível
      boolean isImpossibleTravel = false;
      double travelSpeedKmh = 0;
      double travelDistanceKm = 0;
      int timeSinceLastMinutes = 0;

      // Calcular score de risco regional
      int regionRiskScore =
          calculateRegionRiskScore(
              isHighRisk, isSanctioned, ipMismatch, isImpossibleTravel, countryRiskScore);

      return GeoContext.builder()
          .latitude(coords.isFound() ? coords.getLatitude() : null)
          .longitude(coords.isFound() ? coords.getLongitude() : null)
          .coordinateSource(coords.getSource())
          .country(getCountryName(countryCode))
          .countryCode(countryCode)
          .state(state)
          .city(city)
          .postalCode(request.getMerchantPostalCode())
          .ipCountry(ipCountry)
          .ipCountryMismatch(ipMismatch)
          .travelSpeedKmh(travelSpeedKmh)
          .travelDistanceKm(travelDistanceKm)
          .timeSinceLastMinutes(timeSinceLastMinutes)
          .isImpossibleTravel(isImpossibleTravel)
          .isHighRiskCountry(isHighRisk)
          .isSanctionedCountry(isSanctioned)
          .countryRiskScore(countryRiskScore)
          .isDomestic(isDomestic)
          .isCrossBorder(isCrossBorder)
          .homeCountry(homeCountry)
          .regionRiskScore(regionRiskScore)
          .build();

    } catch (Exception e) {
      log.warn("Erro ao enriquecer geo: {}", e.getMessage());
      return GeoContext.empty();
    }
  }

  /** Versão simplificada sem IP e home country. */
  public GeoContext enrich(TransactionRequest request) {
    return enrich(request, null, null);
  }

  /** Normaliza código de país. */
  private String normalizeCountryCode(String code) {
    if (code == null) return null;
    return code.trim().toUpperCase();
  }

  /** Normaliza estado. */
  private String normalizeState(String state) {
    if (state == null) return null;
    return state.trim().toUpperCase();
  }

  /** Normaliza cidade. */
  private String normalizeCity(String city) {
    if (city == null) return null;
    return city.trim().toUpperCase();
  }

  /** Verifica mismatch de país do IP. */
  private boolean checkIpCountryMismatch(String merchantCountry, String ipCountry) {
    if (merchantCountry == null || ipCountry == null) return false;
    return !merchantCountry.equalsIgnoreCase(ipCountry);
  }

  /** Verifica se é país de alto risco. */
  private boolean isHighRiskCountry(String countryCode) {
    if (countryCode == null) return false;
    return HIGH_RISK_COUNTRIES.contains(countryCode.toUpperCase());
  }

  /** Verifica se é país sancionado. */
  private boolean isSanctionedCountry(String countryCode) {
    if (countryCode == null) return false;
    return SANCTIONED_COUNTRIES.contains(countryCode.toUpperCase());
  }

  /** Calcula score de risco do país (0-100). */
  private int calculateCountryRiskScore(String countryCode) {
    if (countryCode == null) return 0;

    if (isSanctionedCountry(countryCode)) return 100;
    if (isHighRiskCountry(countryCode)) return 80;

    // Países de risco médio
    Set<String> mediumRiskCountries =
        Set.of(
            "NG", "566", // Nigéria
            "GH", "288", // Gana
            "KE", "404", // Quênia
            "PH", "608", // Filipinas
            "ID", "360", // Indonésia
            "VN", "704" // Vietnã
            );

    if (mediumRiskCountries.contains(countryCode.toUpperCase())) return 50;

    return 0;
  }

  /** Verifica se é transação doméstica. */
  private boolean isDomesticTransaction(String merchantCountry, String homeCountry) {
    if (merchantCountry == null || homeCountry == null) return true;
    return merchantCountry.equalsIgnoreCase(homeCountry);
  }

  /** Calcula score de risco regional. */
  private int calculateRegionRiskScore(
      boolean isHighRisk,
      boolean isSanctioned,
      boolean ipMismatch,
      boolean impossibleTravel,
      int countryRiskScore) {

    int score = countryRiskScore;

    if (isSanctioned) score = Math.max(score, 100);
    if (isHighRisk) score = Math.max(score, 80);
    if (impossibleTravel) score += 30;
    if (ipMismatch) score += 20;

    return Math.min(100, score);
  }

  /** Obtém nome do país a partir do código. */
  private String getCountryName(String countryCode) {
    if (countryCode == null) return null;

    return switch (countryCode.toUpperCase()) {
      case "BR", "076" -> "BRAZIL";
      case "US", "840" -> "UNITED STATES";
      case "GB", "826" -> "UNITED KINGDOM";
      case "DE", "276" -> "GERMANY";
      case "FR", "250" -> "FRANCE";
      case "ES", "724" -> "SPAIN";
      case "IT", "380" -> "ITALY";
      case "PT", "620" -> "PORTUGAL";
      case "AR", "032" -> "ARGENTINA";
      case "MX", "484" -> "MEXICO";
      case "CL", "152" -> "CHILE";
      case "CO", "170" -> "COLOMBIA";
      case "PE", "604" -> "PERU";
      default -> countryCode;
    };
  }
}
