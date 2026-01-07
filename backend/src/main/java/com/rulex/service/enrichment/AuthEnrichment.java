package com.rulex.service.enrichment;

import com.rulex.dto.TransactionRequest;
import java.util.HashMap;
import java.util.Map;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Enriquecimento de dados de autenticação para regras de fraude.
 *
 * <p>Consolida todos os campos de autenticação em um formato fácil de usar pelo motor de regras.
 * Campos disponíveis:
 *
 * <ul>
 *   <li>auth.consecutive_failures - Falhas consecutivas de autenticação
 *   <li>auth.attempts_5min - Tentativas nos últimos 5 minutos
 *   <li>auth.attempts_1h - Tentativas na última hora
 *   <li>auth.last_failure_minutes - Minutos desde última falha
 *   <li>cvv.consecutive_failures - Falhas consecutivas de CVV
 *   <li>cvv.attempts_24h - Tentativas de CVV em 24h
 *   <li>cvv.is_valid - Se CVV é válido
 *   <li>pin.consecutive_failures - Falhas consecutivas de PIN
 *   <li>mfa.completed - Se MFA foi completado
 *   <li>auth.3ds_status - Status do 3D Secure
 * </ul>
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class AuthEnrichment {

  /** Resultado do enriquecimento de autenticação. */
  @Data
  @Builder
  public static class AuthContext {
    // Autenticação geral
    private final int consecutiveFailures;
    private final int attempts5min;
    private final int attempts1h;
    private final int attempts24h;
    private final int lastFailureMinutes;

    // CVV
    private final int cvvConsecutiveFailures;
    private final int cvvAttempts24h;
    private final boolean cvvValid;
    private final boolean cvvPresent;
    private final String cvvResponse;

    // PIN
    private final int pinConsecutiveFailures;
    private final boolean pinValid;
    private final String pinResponse;

    // MFA
    private final boolean mfaRequested;
    private final boolean mfaCompleted;
    private final String mfaMethod;

    // 3D Secure
    private final boolean is3dsAuthenticated;
    private final String eciIndicator;
    private final String cavvResult;
    private final int authenticationScore;

    // Criptograma
    private final boolean cryptogramValid;

    // Score
    private final int riskScore;
    private final int trustScore;

    /** Converte para Map para uso no evaluator. */
    public Map<String, Object> toMap() {
      Map<String, Object> map = new HashMap<>();

      // Autenticação geral
      map.put("auth.consecutive_failures", consecutiveFailures);
      map.put("auth.attempts_5min", attempts5min);
      map.put("auth.attempts_1h", attempts1h);
      map.put("auth.attempts_24h", attempts24h);
      map.put("auth.last_failure_minutes", lastFailureMinutes);

      // Aliases
      map.put("authConsecutiveFailures", consecutiveFailures);
      map.put("authAttempts5min", attempts5min);

      // CVV
      map.put("cvv.consecutive_failures", cvvConsecutiveFailures);
      map.put("cvv.attempts_24h", cvvAttempts24h);
      map.put("cvv.is_valid", cvvValid);
      map.put("cvv.is_present", cvvPresent);
      map.put("cvv.response", cvvResponse);

      // Aliases
      map.put("cvvConsecutiveFailures", cvvConsecutiveFailures);
      map.put("cvvValid", cvvValid);

      // PIN
      map.put("pin.consecutive_failures", pinConsecutiveFailures);
      map.put("pin.is_valid", pinValid);
      map.put("pin.response", pinResponse);

      // Aliases
      map.put("pinConsecutiveFailures", pinConsecutiveFailures);

      // MFA
      map.put("mfa.requested", mfaRequested);
      map.put("mfa.completed", mfaCompleted);
      map.put("mfa.method", mfaMethod);

      // Aliases
      map.put("mfaCompleted", mfaCompleted);

      // 3D Secure
      map.put("auth.is_3ds_authenticated", is3dsAuthenticated);
      map.put("auth.eci_indicator", eciIndicator);
      map.put("auth.cavv_result", cavvResult);
      map.put("auth.authentication_score", authenticationScore);

      // Aliases
      map.put("is3dsAuthenticated", is3dsAuthenticated);
      map.put("eciIndicator", eciIndicator);

      // Criptograma
      map.put("auth.cryptogram_valid", cryptogramValid);
      map.put("cryptogramValid", cryptogramValid);

      // Score
      map.put("auth.risk_score", riskScore);
      map.put("auth.trust_score", trustScore);

      return map;
    }

    public static AuthContext empty() {
      return AuthContext.builder()
          .consecutiveFailures(0)
          .attempts5min(0)
          .attempts1h(0)
          .attempts24h(0)
          .lastFailureMinutes(0)
          .cvvConsecutiveFailures(0)
          .cvvAttempts24h(0)
          .cvvValid(false)
          .cvvPresent(false)
          .cvvResponse(null)
          .pinConsecutiveFailures(0)
          .pinValid(false)
          .pinResponse(null)
          .mfaRequested(false)
          .mfaCompleted(false)
          .mfaMethod(null)
          .is3dsAuthenticated(false)
          .eciIndicator(null)
          .cavvResult(null)
          .authenticationScore(0)
          .cryptogramValid(false)
          .riskScore(50)
          .trustScore(50)
          .build();
    }
  }

  /**
   * Enriquece uma transação com dados de autenticação.
   *
   * @param request A transação a ser enriquecida
   * @param authHistory Histórico de autenticação (opcional)
   * @return Contexto de autenticação com todos os campos calculados
   */
  public AuthContext enrich(TransactionRequest request, Map<String, Object> authHistory) {
    if (request == null) {
      log.debug("TransactionRequest é null, retornando contexto vazio");
      return AuthContext.empty();
    }

    try {
      // Extrair dados de CVV
      boolean cvvPresent = isCvvPresent(request);
      boolean cvvValid = isCvvValid(request);
      String cvvResponse = request.getCvv2Response();

      // Extrair dados de PIN
      boolean pinValid = isPinValid(request);
      String pinResponse = request.getPinVerifyCode();

      // Extrair dados de 3DS
      boolean is3dsAuthenticated = is3dsAuthenticated(request);
      String eciIndicator =
          request.getEciIndicator() != null ? String.valueOf(request.getEciIndicator()) : null;
      String cavvResult =
          request.getCavvResult() != null ? String.valueOf(request.getCavvResult()) : null;
      int authScore =
          request.getConsumerAuthenticationScore() != null
              ? request.getConsumerAuthenticationScore()
              : 0;

      // Extrair criptograma
      boolean cryptogramValid = isCryptogramValid(request);

      // Extrair MFA (do request ou authHistory)
      boolean mfaRequested = getBooleanFromMap(authHistory, "mfaRequested", false);
      boolean mfaCompleted = getBooleanFromMap(authHistory, "mfaCompleted", false);
      String mfaMethod = getStringFromMap(authHistory, "mfaMethod", null);

      // Extrair histórico de falhas (do authHistory)
      int consecutiveFailures = getIntegerFromMap(authHistory, "consecutiveFailures", 0);
      int attempts5min = getIntegerFromMap(authHistory, "attempts5min", 0);
      int attempts1h = getIntegerFromMap(authHistory, "attempts1h", 0);
      int attempts24h = getIntegerFromMap(authHistory, "attempts24h", 0);
      int lastFailureMinutes = getIntegerFromMap(authHistory, "lastFailureMinutes", 0);
      int cvvConsecutiveFailures = getIntegerFromMap(authHistory, "cvvConsecutiveFailures", 0);
      int cvvAttempts24h = getIntegerFromMap(authHistory, "cvvAttempts24h", 0);
      int pinConsecutiveFailures = getIntegerFromMap(authHistory, "pinConsecutiveFailures", 0);

      // Calcular scores
      int riskScore =
          calculateRiskScore(
              consecutiveFailures,
              cvvConsecutiveFailures,
              pinConsecutiveFailures,
              cvvValid,
              pinValid,
              is3dsAuthenticated,
              cryptogramValid,
              mfaCompleted);
      int trustScore = 100 - riskScore;

      return AuthContext.builder()
          .consecutiveFailures(consecutiveFailures)
          .attempts5min(attempts5min)
          .attempts1h(attempts1h)
          .attempts24h(attempts24h)
          .lastFailureMinutes(lastFailureMinutes)
          .cvvConsecutiveFailures(cvvConsecutiveFailures)
          .cvvAttempts24h(cvvAttempts24h)
          .cvvValid(cvvValid)
          .cvvPresent(cvvPresent)
          .cvvResponse(cvvResponse)
          .pinConsecutiveFailures(pinConsecutiveFailures)
          .pinValid(pinValid)
          .pinResponse(pinResponse)
          .mfaRequested(mfaRequested)
          .mfaCompleted(mfaCompleted)
          .mfaMethod(mfaMethod)
          .is3dsAuthenticated(is3dsAuthenticated)
          .eciIndicator(eciIndicator)
          .cavvResult(cavvResult)
          .authenticationScore(authScore)
          .cryptogramValid(cryptogramValid)
          .riskScore(riskScore)
          .trustScore(trustScore)
          .build();

    } catch (Exception e) {
      log.warn("Erro ao enriquecer auth: {}", e.getMessage());
      return AuthContext.empty();
    }
  }

  /** Versão simplificada sem histórico. */
  public AuthContext enrich(TransactionRequest request) {
    return enrich(request, new HashMap<>());
  }

  /** Verifica se CVV está presente. */
  private boolean isCvvPresent(TransactionRequest request) {
    String cvv2Present = request.getCvv2Present();
    return cvv2Present != null
        && ("Y".equalsIgnoreCase(cvv2Present)
            || "1".equals(cvv2Present)
            || "TRUE".equalsIgnoreCase(cvv2Present));
  }

  /** Verifica se CVV é válido. */
  private boolean isCvvValid(TransactionRequest request) {
    String cvv2Response = request.getCvv2Response();
    if (cvv2Response == null) return false;

    // Códigos de resposta CVV válidos
    return "M".equalsIgnoreCase(cvv2Response) // Match
        || "Y".equalsIgnoreCase(cvv2Response) // Valid
        || "1".equals(cvv2Response); // Valid
  }

  /** Verifica se PIN é válido. */
  private boolean isPinValid(TransactionRequest request) {
    String pinVerifyCode = request.getPinVerifyCode();
    if (pinVerifyCode == null) return false;

    // Códigos de verificação PIN válidos
    return "00".equals(pinVerifyCode) // Valid
        || "Y".equalsIgnoreCase(pinVerifyCode);
  }

  /** Verifica se 3DS está autenticado. */
  private boolean is3dsAuthenticated(TransactionRequest request) {
    Integer eciIndicator = request.getEciIndicator();
    if (eciIndicator == null) return false;

    // ECI indicators que indicam autenticação bem-sucedida
    return eciIndicator == 5 // Visa fully authenticated
        || eciIndicator == 2 // Mastercard fully authenticated
        || eciIndicator == 1; // Mastercard attempted
  }

  /** Verifica se criptograma é válido. */
  private boolean isCryptogramValid(TransactionRequest request) {
    String cryptogramValid = request.getCryptogramValid();
    return cryptogramValid != null
        && ("Y".equalsIgnoreCase(cryptogramValid)
            || "1".equals(cryptogramValid)
            || "TRUE".equalsIgnoreCase(cryptogramValid));
  }

  /** Calcula score de risco de autenticação. */
  private int calculateRiskScore(
      int consecutiveFailures,
      int cvvConsecutiveFailures,
      int pinConsecutiveFailures,
      boolean cvvValid,
      boolean pinValid,
      boolean is3dsAuthenticated,
      boolean cryptogramValid,
      boolean mfaCompleted) {

    int score = 50; // Base

    // Falhas consecutivas aumentam risco
    score += consecutiveFailures * 10;
    score += cvvConsecutiveFailures * 15;
    score += pinConsecutiveFailures * 15;

    // Validações bem-sucedidas reduzem risco
    if (cvvValid) score -= 15;
    if (pinValid) score -= 10;
    if (is3dsAuthenticated) score -= 20;
    if (cryptogramValid) score -= 10;
    if (mfaCompleted) score -= 15;

    // CVV inválido aumenta risco
    if (!cvvValid) score += 10;

    return Math.max(0, Math.min(100, score));
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
