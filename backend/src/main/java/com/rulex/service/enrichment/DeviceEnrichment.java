package com.rulex.service.enrichment;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.DeviceFingerprint;
import com.rulex.repository.DeviceFingerprintRepository;
import com.rulex.repository.DevicePanAssociationRepository;
import com.rulex.service.BloomFilterService;
import com.rulex.service.DeviceFingerprintService;
import com.rulex.service.DeviceFingerprintService.FingerprintAnalysis;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.time.OffsetDateTime;
import java.time.temporal.ChronoUnit;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Enriquecimento de device fingerprint para regras de fraude.
 *
 * <p>Consolida todos os campos de device em um formato fácil de usar pelo motor de regras. Campos
 * disponíveis:
 *
 * <ul>
 *   <li>device.fingerprint - Hash do fingerprint do dispositivo
 *   <li>device.is_new - Se é um dispositivo novo para este PAN
 *   <li>device.risk_score - Score de risco do dispositivo (0-100)
 *   <li>device.distinct_devices_24h - Dispositivos distintos nas últimas 24h
 *   <li>device.distinct_pans_24h - PANs distintos neste dispositivo nas últimas 24h
 *   <li>device.age_days - Dias desde primeira vez visto
 *   <li>device.is_emulator - Flag de emulador detectado
 *   <li>device.is_rooted - Flag de dispositivo rooteado
 *   <li>device.is_vpn - Flag de VPN detectado
 *   <li>device.is_proxy - Flag de proxy detectado
 *   <li>device.is_tor - Flag de Tor detectado
 *   <li>device.is_datacenter_ip - Flag de IP de datacenter
 *   <li>device.fingerprint_blocked - Fingerprint em lista de bloqueio
 *   <li>device.trust_score - Score de confiança do device (0-100)
 * </ul>
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class DeviceEnrichment {

  private final DeviceFingerprintService deviceFingerprintService;
  private final DeviceFingerprintRepository deviceFingerprintRepository;
  private final DevicePanAssociationRepository devicePanAssociationRepository;
  private final BloomFilterService bloomFilterService;

  /** Resultado do enriquecimento de device. */
  @Data
  @Builder
  public static class DeviceContext {
    // Identificação
    private final String fingerprint;
    private final String fingerprintHash;

    // Status
    private final boolean isNew;
    private final boolean isKnown;
    private final int ageDays;
    private final int lastSeenHours;

    // Contagens
    private final int distinctDevices24h;
    private final int distinctDevices7d;
    private final int distinctPans24h;
    private final int distinctPans7d;

    // Detecção de fraude
    private final boolean isEmulator;
    private final boolean isRooted;
    private final boolean isVpn;
    private final boolean isProxy;
    private final boolean isTor;
    private final boolean isDatacenterIp;
    private final boolean isFingerprintBlocked;

    // Anomalias
    private final boolean hasTimezoneMismatch;
    private final boolean hasLanguageMismatch;
    private final boolean hasScreenAnomaly;
    private final boolean hasBrowserAnomaly;

    // Scores
    private final int riskScore;
    private final int trustScore;
    private final int anomalyScore;

    // Flags compostas
    private final boolean isHighRisk;
    private final boolean isSuspicious;

    /** Converte para Map para uso no evaluator. */
    public Map<String, Object> toMap() {
      Map<String, Object> map = new HashMap<>();

      // Identificação
      map.put("device.fingerprint", fingerprint);
      map.put("device.fingerprint_hash", fingerprintHash);

      // Status
      map.put("device.is_new", isNew);
      map.put("device.is_known", isKnown);
      map.put("device.age_days", ageDays);
      map.put("device.last_seen_hours", lastSeenHours);

      // Aliases para compatibilidade
      map.put("deviceIsNew", isNew);
      map.put("deviceAgeDays", ageDays);

      // Contagens
      map.put("device.distinct_devices_24h", distinctDevices24h);
      map.put("device.distinct_devices_7d", distinctDevices7d);
      map.put("device.distinct_pans_24h", distinctPans24h);
      map.put("device.distinct_pans_7d", distinctPans7d);

      // Detecção de fraude
      map.put("device.is_emulator", isEmulator);
      map.put("device.is_rooted", isRooted);
      map.put("device.is_vpn", isVpn);
      map.put("device.is_proxy", isProxy);
      map.put("device.is_tor", isTor);
      map.put("device.is_datacenter_ip", isDatacenterIp);
      map.put("device.fingerprint_blocked", isFingerprintBlocked);

      // Aliases
      map.put("emulatorDetected", isEmulator);
      map.put("rootedDevice", isRooted);
      map.put("vpnDetected", isVpn);
      map.put("proxyDetected", isProxy);
      map.put("torDetected", isTor);

      // Anomalias
      map.put("device.timezone_mismatch", hasTimezoneMismatch);
      map.put("device.language_mismatch", hasLanguageMismatch);
      map.put("device.screen_anomaly", hasScreenAnomaly);
      map.put("device.browser_anomaly", hasBrowserAnomaly);

      // Scores
      map.put("device.risk_score", riskScore);
      map.put("device.trust_score", trustScore);
      map.put("device.anomaly_score", anomalyScore);

      // Aliases
      map.put("deviceRiskScore", riskScore);
      map.put("deviceTrustScore", trustScore);

      // Flags compostas
      map.put("device.is_high_risk", isHighRisk);
      map.put("device.is_suspicious", isSuspicious);

      return map;
    }

    public static DeviceContext empty() {
      return DeviceContext.builder()
          .fingerprint(null)
          .fingerprintHash(null)
          .isNew(true)
          .isKnown(false)
          .ageDays(0)
          .lastSeenHours(0)
          .distinctDevices24h(0)
          .distinctDevices7d(0)
          .distinctPans24h(0)
          .distinctPans7d(0)
          .isEmulator(false)
          .isRooted(false)
          .isVpn(false)
          .isProxy(false)
          .isTor(false)
          .isDatacenterIp(false)
          .isFingerprintBlocked(false)
          .hasTimezoneMismatch(false)
          .hasLanguageMismatch(false)
          .hasScreenAnomaly(false)
          .hasBrowserAnomaly(false)
          .riskScore(0)
          .trustScore(100)
          .anomalyScore(0)
          .isHighRisk(false)
          .isSuspicious(false)
          .build();
    }
  }

  /**
   * Enriquece uma transação com dados de device fingerprint.
   *
   * @param request A transação a ser enriquecida
   * @param deviceData Dados do dispositivo (do payload ou header)
   * @return Contexto de device com todos os campos calculados
   */
  public DeviceContext enrich(TransactionRequest request, Map<String, Object> deviceData) {
    if (request == null) {
      log.debug("TransactionRequest é null, retornando contexto vazio");
      return DeviceContext.empty();
    }

    try {
      // Gerar hash do PAN para análise
      String panHash = hashPan(request.getPan());

      // Analisar device usando o service existente
      FingerprintAnalysis analysis =
          deviceFingerprintService.analyzeFingerprint(panHash, deviceData);

      // Extrair flags de detecção do deviceData
      boolean isEmulator = getBooleanFromMap(deviceData, "isEmulator", false);
      boolean isRooted = getBooleanFromMap(deviceData, "isRooted", false);
      boolean isVpn = getBooleanFromMap(deviceData, "isVpn", false);
      boolean isProxy = getBooleanFromMap(deviceData, "isProxy", false);
      boolean isTor = getBooleanFromMap(deviceData, "isTor", false);
      boolean isDatacenterIp = getBooleanFromMap(deviceData, "isDatacenterIp", false);

      // Calcular anomalias
      boolean hasTimezoneMismatch = detectTimezoneMismatch(request, deviceData);
      boolean hasLanguageMismatch = detectLanguageMismatch(request, deviceData);
      boolean hasScreenAnomaly = detectScreenAnomaly(deviceData);
      boolean hasBrowserAnomaly = detectBrowserAnomaly(deviceData);

      // Calcular score de risco baseado no nível
      int riskScore = calculateRiskScoreFromLevel(analysis.riskLevel());
      int trustScore = 100 - riskScore;
      int anomalyScore =
          calculateAnomalyScore(
              isEmulator,
              isRooted,
              isVpn,
              isProxy,
              isTor,
              isDatacenterIp,
              hasTimezoneMismatch,
              hasLanguageMismatch,
              hasScreenAnomaly,
              hasBrowserAnomaly);

      // Calcular ageDays e lastSeenHours a partir do histórico
      int ageDays = 0;
      int lastSeenHours = 0;
      if (analysis.fingerprintHash() != null) {
        Optional<DeviceFingerprint> deviceOpt =
            deviceFingerprintRepository.findByFingerprintHash(analysis.fingerprintHash());
        if (deviceOpt.isPresent()) {
          DeviceFingerprint device = deviceOpt.get();
          if (device.getFirstSeen() != null) {
            ageDays = (int) ChronoUnit.DAYS.between(device.getFirstSeen(), OffsetDateTime.now());
          }
          if (device.getLastSeen() != null) {
            lastSeenHours = (int) ChronoUnit.HOURS.between(device.getLastSeen(), OffsetDateTime.now());
          }
        }
      }

      // Calcular distinctDevices7d e distinctPans7d
      // Por enquanto, usamos os valores de 24h como aproximação
      // Uma implementação completa requer queries com janela temporal
      int distinctDevices7d = analysis.devicesForCard();
      int distinctPans7d = analysis.cardsOnDevice();

      // Verificar se fingerprint está em lista de bloqueio
      boolean isFingerprintBlocked = false;
      if (analysis.fingerprintHash() != null) {
        try {
          var blocklistResult = bloomFilterService.isBlacklisted(
              com.rulex.entity.RuleList.EntityType.DEVICE_ID,
              analysis.fingerprintHash());
          isFingerprintBlocked = blocklistResult.inList();
        } catch (Exception e) {
          log.debug("Erro ao verificar blocklist de fingerprint: {}", e.getMessage());
        }
      }

      // Flags compostas
      boolean isHighRisk = riskScore >= 70 || isEmulator || isRooted || isTor || isFingerprintBlocked;
      boolean isSuspicious = riskScore >= 50 || isVpn || isProxy || anomalyScore >= 30;

      return DeviceContext.builder()
          .fingerprint(analysis.fingerprintHash())
          .fingerprintHash(analysis.fingerprintHash())
          .isNew(analysis.isNewDevice())
          .isKnown(!analysis.isNewDevice())
          .ageDays(ageDays)
          .lastSeenHours(lastSeenHours)
          .distinctDevices24h(analysis.devicesForCard())
          .distinctDevices7d(distinctDevices7d)
          .distinctPans24h(analysis.cardsOnDevice())
          .distinctPans7d(distinctPans7d)
          .isEmulator(isEmulator)
          .isRooted(isRooted)
          .isVpn(isVpn)
          .isProxy(isProxy)
          .isTor(isTor)
          .isDatacenterIp(isDatacenterIp)
          .isFingerprintBlocked(isFingerprintBlocked)
          .hasTimezoneMismatch(hasTimezoneMismatch)
          .hasLanguageMismatch(hasLanguageMismatch)
          .hasScreenAnomaly(hasScreenAnomaly)
          .hasBrowserAnomaly(hasBrowserAnomaly)
          .riskScore(riskScore)
          .trustScore(trustScore)
          .anomalyScore(anomalyScore)
          .isHighRisk(isHighRisk)
          .isSuspicious(isSuspicious)
          .build();

    } catch (Exception e) {
      log.warn("Erro ao enriquecer device: {}", e.getMessage());
      return DeviceContext.empty();
    }
  }

  /** Versão simplificada sem deviceData. */
  public DeviceContext enrich(TransactionRequest request) {
    return enrich(request, new HashMap<>());
  }

  /** Detecta mismatch de timezone. */
  private boolean detectTimezoneMismatch(
      TransactionRequest request, Map<String, Object> deviceData) {
    if (deviceData == null) return false;

    String deviceTimezone = getStringFromMap(deviceData, "timezone", null);
    String expectedTimezone = getExpectedTimezone(request);

    if (deviceTimezone == null || expectedTimezone == null) return false;

    // Simplificação: verificar se o offset é muito diferente
    return !deviceTimezone.equals(expectedTimezone);
  }

  /** Detecta mismatch de idioma. */
  private boolean detectLanguageMismatch(
      TransactionRequest request, Map<String, Object> deviceData) {
    if (deviceData == null) return false;

    String deviceLanguage = getStringFromMap(deviceData, "language", null);
    String merchantCountry = request.getMerchantCountryCode();

    if (deviceLanguage == null || merchantCountry == null) return false;

    // Verificar se o idioma é compatível com o país
    return !isLanguageCompatibleWithCountry(deviceLanguage, merchantCountry);
  }

  /** Detecta anomalia de tela. */
  private boolean detectScreenAnomaly(Map<String, Object> deviceData) {
    if (deviceData == null) return false;

    Integer screenWidth = getIntegerFromMap(deviceData, "screenWidth", null);
    Integer screenHeight = getIntegerFromMap(deviceData, "screenHeight", null);

    if (screenWidth == null || screenHeight == null) return false;

    // Resoluções suspeitas (muito pequenas ou muito grandes)
    if (screenWidth < 320 || screenHeight < 480) return true;
    if (screenWidth > 7680 || screenHeight > 4320) return true;

    // Proporções estranhas
    double ratio = (double) screenWidth / screenHeight;
    return ratio < 0.3 || ratio > 3.0;
  }

  /** Detecta anomalia de browser. */
  private boolean detectBrowserAnomaly(Map<String, Object> deviceData) {
    if (deviceData == null) return false;

    String userAgent = getStringFromMap(deviceData, "userAgent", null);
    if (userAgent == null) return false;

    // Detectar user agents suspeitos
    String ua = userAgent.toLowerCase();
    return ua.contains("headless")
        || ua.contains("phantom")
        || ua.contains("selenium")
        || ua.contains("puppeteer")
        || ua.contains("playwright");
  }

  /** Calcula score de anomalia. */
  private int calculateAnomalyScore(
      boolean isEmulator,
      boolean isRooted,
      boolean isVpn,
      boolean isProxy,
      boolean isTor,
      boolean isDatacenterIp,
      boolean hasTimezoneMismatch,
      boolean hasLanguageMismatch,
      boolean hasScreenAnomaly,
      boolean hasBrowserAnomaly) {

    int score = 0;

    if (isEmulator) score += 30;
    if (isRooted) score += 20;
    if (isTor) score += 25;
    if (isVpn) score += 15;
    if (isProxy) score += 15;
    if (isDatacenterIp) score += 20;
    if (hasTimezoneMismatch) score += 10;
    if (hasLanguageMismatch) score += 10;
    if (hasScreenAnomaly) score += 15;
    if (hasBrowserAnomaly) score += 25;

    return Math.min(100, score);
  }

  /** Obtém timezone esperado baseado na localização do merchant. */
  private String getExpectedTimezone(TransactionRequest request) {
    // Simplificação: retornar timezone baseado no país
    String country = request.getMerchantCountryCode();
    if (country == null) return null;

    return switch (country.toUpperCase()) {
      case "BR", "076" -> "America/Sao_Paulo";
      case "US", "840" -> "America/New_York";
      case "GB", "826" -> "Europe/London";
      default -> null;
    };
  }

  /** Verifica se idioma é compatível com país. */
  private boolean isLanguageCompatibleWithCountry(String language, String country) {
    if (language == null || country == null) return true;

    String lang = language.toLowerCase().substring(0, Math.min(2, language.length()));
    String countryCode = country.toUpperCase();

    return switch (countryCode) {
      case "BR", "076" -> lang.equals("pt");
      case "US", "840", "GB", "826" -> lang.equals("en");
      case "ES", "724" -> lang.equals("es");
      case "FR", "250" -> lang.equals("fr");
      case "DE", "276" -> lang.equals("de");
      default -> true; // Não verificar países desconhecidos
    };
  }

  /** Gera hash SHA-256 do PAN para privacidade. */
  private String hashPan(String pan) {
    if (pan == null || pan.isBlank()) {
      return null;
    }
    try {
      MessageDigest digest = MessageDigest.getInstance("SHA-256");
      byte[] hash = digest.digest(pan.getBytes(StandardCharsets.UTF_8));
      StringBuilder hexString = new StringBuilder();
      for (byte b : hash) {
        String hex = Integer.toHexString(0xff & b);
        if (hex.length() == 1) hexString.append('0');
        hexString.append(hex);
      }
      return hexString.toString();
    } catch (Exception e) {
      log.error("Erro ao gerar hash do PAN: {}", e.getMessage());
      return null;
    }
  }

  /** Calcula score de risco baseado no nível. */
  private int calculateRiskScoreFromLevel(DeviceFingerprintService.RiskLevel level) {
    if (level == null) return 0;
    return switch (level) {
      case CRITICAL -> 100;
      case HIGH -> 80;
      case MEDIUM -> 50;
      case LOW -> 20;
    };
  }

  // Helpers
  private boolean getBooleanFromMap(Map<String, Object> map, String key, boolean defaultValue) {
    if (map == null) return defaultValue;
    Object value = map.get(key);
    if (value instanceof Boolean) return (Boolean) value;
    if (value instanceof String) return Boolean.parseBoolean((String) value);
    return defaultValue;
  }

  private String getStringFromMap(Map<String, Object> map, String key, String defaultValue) {
    if (map == null) return defaultValue;
    Object value = map.get(key);
    return value instanceof String ? (String) value : defaultValue;
  }

  private Integer getIntegerFromMap(Map<String, Object> map, String key, Integer defaultValue) {
    if (map == null) return defaultValue;
    Object value = map.get(key);
    if (value instanceof Integer) return (Integer) value;
    if (value instanceof Number) return ((Number) value).intValue();
    return defaultValue;
  }
}
