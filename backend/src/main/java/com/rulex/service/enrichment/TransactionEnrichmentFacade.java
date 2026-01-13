package com.rulex.service.enrichment;

import com.rulex.dto.TransactionRequest;
import com.rulex.service.EnrichmentService;
import com.rulex.service.EnrichmentService.EnrichmentContext;
import java.util.HashMap;
import java.util.Map;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * Facade para enriquecimento completo de transações.
 *
 * <p>Consolida todos os serviços de enriquecimento em uma única interface, facilitando a integração
 * com o motor de regras. Todos os campos derivados são disponibilizados em um Map plano.
 *
 * <p>Serviços integrados:
 *
 * <ul>
 *   <li>EnrichmentService - BIN e MCC
 *   <li>DerivedContext - Campos básicos derivados
 *   <li>VelocityEnrichment - Métricas de velocidade
 *   <li>DeviceEnrichment - Fingerprint de dispositivo
 *   <li>GeoEnrichment - Geolocalização
 * </ul>
 *
 * <p>Uso:
 *
 * <pre>{@code
 * FullEnrichmentContext ctx = facade.enrichFull(request);
 * Map<String, Object> allFields = ctx.toFlatMap();
 * // allFields contém todos os 100+ campos derivados
 * }</pre>
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class TransactionEnrichmentFacade {

  private final EnrichmentService enrichmentService;
  private final VelocityEnrichment velocityEnrichment;
  private final DeviceEnrichment deviceEnrichment;
  private final GeoEnrichment geoEnrichment;
  private final CustomerEnrichment customerEnrichment;
  private final CardEnrichment cardEnrichment;
  private final AuthEnrichment authEnrichment;
  private final AnomalyEnrichment anomalyEnrichment;

  /** Contexto completo de enriquecimento. */
  @Data
  @Builder
  public static class FullEnrichmentContext {
    private final EnrichmentContext basicEnrichment;
    private final VelocityEnrichment.VelocityContext velocityContext;
    private final DeviceEnrichment.DeviceContext deviceContext;
    private final GeoEnrichment.GeoContext geoContext;
    private final CustomerEnrichment.CustomerContext customerContext;
    private final CardEnrichment.CardContext cardContext;
    private final AuthEnrichment.AuthContext authContext;
    private final AnomalyEnrichment.AnomalyContext anomalyContext;
    private final TransactionRequest originalRequest;
    private final long enrichmentTimeMs;

    /**
     * Converte todos os contextos para um Map plano.
     *
     * <p>Este é o formato esperado pelo ComplexRuleEvaluator.
     *
     * @return Map com todos os campos derivados
     */
    public Map<String, Object> toFlatMap() {
      Map<String, Object> map = new HashMap<>();

      // Adicionar campos do payload original
      if (originalRequest != null) {
        map.putAll(requestToMap(originalRequest));
      }

      // Adicionar enriquecimento básico (BIN, MCC, DerivedContext)
      if (basicEnrichment != null) {
        map.putAll(basicEnrichment.toMap());
      }

      // Adicionar velocidade
      if (velocityContext != null) {
        map.putAll(velocityContext.toMap());
      }

      // Adicionar device
      if (deviceContext != null) {
        map.putAll(deviceContext.toMap());
      }

      // Adicionar geo
      if (geoContext != null) {
        map.putAll(geoContext.toMap());
      }

      // Adicionar customer
      if (customerContext != null) {
        map.putAll(customerContext.toMap());
      }

      // Adicionar card
      if (cardContext != null) {
        map.putAll(cardContext.toMap());
      }

      // Adicionar auth
      if (authContext != null) {
        map.putAll(authContext.toMap());
      }

      // Adicionar anomaly
      if (anomalyContext != null) {
        map.putAll(anomalyContext.toMap());
      }

      // Adicionar metadados
      map.put("_enrichmentTimeMs", enrichmentTimeMs);

      return map;
    }

    /** Converte TransactionRequest para Map. */
    private Map<String, Object> requestToMap(TransactionRequest request) {
      Map<String, Object> map = new HashMap<>();

      // Campos principais
      map.put("externalTransactionId", request.getExternalTransactionId());
      map.put("customerIdFromHeader", request.getCustomerIdFromHeader());
      map.put("customerAcctNumber", request.getCustomerAcctNumber());
      map.put("pan", request.getPan());
      map.put("merchantId", request.getMerchantId());
      map.put("merchantName", request.getMerchantName());
      map.put("transactionAmount", request.getTransactionAmount());
      map.put("transactionCurrencyCode", request.getTransactionCurrencyCode());
      map.put("mcc", request.getMcc());
      map.put("merchantCountryCode", request.getMerchantCountryCode());
      map.put("merchantState", request.getMerchantState());
      map.put("merchantCity", request.getMerchantCity());
      map.put("merchantPostalCode", request.getMerchantPostalCode());
      map.put("transactionDate", request.getTransactionDate());
      map.put("transactionTime", request.getTransactionTime());
      map.put("gmtOffset", request.getGmtOffset());

      // Campos de autenticação
      map.put("cryptogramValid", request.getCryptogramValid());
      map.put("cvv2Response", request.getCvv2Response());
      map.put("cvv2Present", request.getCvv2Present());
      map.put("pinVerifyCode", request.getPinVerifyCode());
      map.put("cvvVerifyCode", request.getCvvVerifyCode());

      // Campos de terminal
      map.put("posEntryMode", request.getPosEntryMode());
      map.put("terminalType", request.getTerminalType());
      map.put("terminalEntryCapability", request.getTerminalEntryCapability());
      map.put("customerPresent", request.getCustomerPresent());
      map.put("cardMediaType", request.getCardMediaType());

      // Campos de cartão
      map.put("cardSeqNum", request.getCardSeqNum());
      map.put("cardExpireDate", request.getCardExpireDate());
      map.put("expandedBIN", request.getExpandedBIN());

      // Campos de rede
      map.put("networkId", request.getNetworkId());
      map.put("authIndicator", request.getAuthIndicator());

      // Campos de 3DS
      map.put("eciIndicator", request.getEciIndicator());
      map.put("cavvResult", request.getCavvResult());
      map.put("tokenizationIndicator", request.getTokenizationIndicator());

      // Campos de risco
      map.put("consumerAuthenticationScore", request.getConsumerAuthenticationScore());
      map.put("availableCredit", request.getAvailableCredit());

      // Campos de device (se presentes no request)
      map.put("deviceId", request.getDeviceId());
      map.put("userAgent", request.getUserAgent());

      return map;
    }

    public static FullEnrichmentContext empty() {
      return FullEnrichmentContext.builder()
          .basicEnrichment(null)
          .velocityContext(VelocityEnrichment.VelocityContext.empty())
          .deviceContext(DeviceEnrichment.DeviceContext.empty())
          .geoContext(GeoEnrichment.GeoContext.empty())
          .customerContext(CustomerEnrichment.CustomerContext.empty())
          .cardContext(CardEnrichment.CardContext.empty())
          .authContext(AuthEnrichment.AuthContext.empty())
          .anomalyContext(AnomalyEnrichment.AnomalyContext.empty())
          .originalRequest(null)
          .enrichmentTimeMs(0)
          .build();
    }
  }

  /**
   * Enriquece uma transação com todos os contextos disponíveis.
   *
   * @param request A transação a ser enriquecida
   * @return Contexto completo de enriquecimento
   */
  public FullEnrichmentContext enrichFull(TransactionRequest request) {
    long startTime = System.currentTimeMillis();

    if (request == null) {
      log.warn("TransactionRequest é null, retornando contexto vazio");
      return FullEnrichmentContext.empty();
    }

    try {
      // Enriquecimento básico (BIN, MCC, DerivedContext)
      EnrichmentContext basicEnrichment = enrichmentService.enrich(request);

      // Enriquecimento de velocidade
      VelocityEnrichment.VelocityContext velocityContext = velocityEnrichment.enrich(request);

      // Enriquecimento de device
      DeviceEnrichment.DeviceContext deviceContext = deviceEnrichment.enrich(request);

      // Enriquecimento de geo
      GeoEnrichment.GeoContext geoContext = geoEnrichment.enrich(request);

      // Enriquecimento de customer
      CustomerEnrichment.CustomerContext customerContext = customerEnrichment.enrich(request);

      // Enriquecimento de card
      CardEnrichment.CardContext cardContext = cardEnrichment.enrich(request);

      // Enriquecimento de auth
      AuthEnrichment.AuthContext authContext = authEnrichment.enrich(request);

      // Enriquecimento de anomaly
      AnomalyEnrichment.AnomalyContext anomalyContext =
          anomalyEnrichment.enrich(request, new HashMap<>(), velocityContext);

      long enrichmentTime = System.currentTimeMillis() - startTime;

      log.debug(
          "Enriquecimento completo em {}ms: velocity.score={}, device.risk_score={}, geo.region_risk_score={}, customer.risk_score={}, card.risk_score={}, anomaly.score={}",
          enrichmentTime,
          velocityContext.getVelocityScore(),
          deviceContext.getRiskScore(),
          geoContext.getRegionRiskScore(),
          customerContext.getRiskScore(),
          cardContext.getRiskScore(),
          anomalyContext.getAnomalyScore());

      return FullEnrichmentContext.builder()
          .basicEnrichment(basicEnrichment)
          .velocityContext(velocityContext)
          .deviceContext(deviceContext)
          .geoContext(geoContext)
          .customerContext(customerContext)
          .cardContext(cardContext)
          .authContext(authContext)
          .anomalyContext(anomalyContext)
          .originalRequest(request)
          .enrichmentTimeMs(enrichmentTime)
          .build();

    } catch (Exception e) {
      log.error("Erro ao enriquecer transação: {}", e.getMessage(), e);
      return FullEnrichmentContext.builder()
          .originalRequest(request)
          .velocityContext(VelocityEnrichment.VelocityContext.empty())
          .deviceContext(DeviceEnrichment.DeviceContext.empty())
          .geoContext(GeoEnrichment.GeoContext.empty())
          .customerContext(CustomerEnrichment.CustomerContext.empty())
          .cardContext(CardEnrichment.CardContext.empty())
          .authContext(AuthEnrichment.AuthContext.empty())
          .anomalyContext(AnomalyEnrichment.AnomalyContext.empty())
          .enrichmentTimeMs(System.currentTimeMillis() - startTime)
          .build();
    }
  }

  /**
   * Enriquece uma transação com contextos específicos.
   *
   * @param request A transação
   * @param deviceData Dados do dispositivo (opcional)
   * @param ipCountry País do IP (opcional)
   * @param homeCountry País de origem do cliente (opcional)
   * @return Contexto completo de enriquecimento
   */
  public FullEnrichmentContext enrichFull(
      TransactionRequest request,
      Map<String, Object> deviceData,
      String ipCountry,
      String homeCountry) {

    long startTime = System.currentTimeMillis();

    if (request == null) {
      log.warn("TransactionRequest é null, retornando contexto vazio");
      return FullEnrichmentContext.empty();
    }

    try {
      // Enriquecimento básico
      EnrichmentContext basicEnrichment = enrichmentService.enrich(request);

      // Enriquecimento de velocidade
      VelocityEnrichment.VelocityContext velocityContext = velocityEnrichment.enrich(request);

      // Enriquecimento de device com dados adicionais
      DeviceEnrichment.DeviceContext deviceContext =
          deviceEnrichment.enrich(request, deviceData != null ? deviceData : new HashMap<>());

      // Enriquecimento de geo com dados adicionais
      GeoEnrichment.GeoContext geoContext = geoEnrichment.enrich(request, ipCountry, homeCountry);

      // Enriquecimento de customer
      CustomerEnrichment.CustomerContext customerContext = customerEnrichment.enrich(request);

      // Enriquecimento de card
      CardEnrichment.CardContext cardContext =
          cardEnrichment.enrich(request, request.getMerchantCountryCode());

      // Enriquecimento de auth
      AuthEnrichment.AuthContext authContext = authEnrichment.enrich(request);

      // Enriquecimento de anomaly
      AnomalyEnrichment.AnomalyContext anomalyContext =
          anomalyEnrichment.enrich(request, new HashMap<>(), velocityContext);

      long enrichmentTime = System.currentTimeMillis() - startTime;

      return FullEnrichmentContext.builder()
          .basicEnrichment(basicEnrichment)
          .velocityContext(velocityContext)
          .deviceContext(deviceContext)
          .geoContext(geoContext)
          .customerContext(customerContext)
          .cardContext(cardContext)
          .authContext(authContext)
          .anomalyContext(anomalyContext)
          .originalRequest(request)
          .enrichmentTimeMs(enrichmentTime)
          .build();

    } catch (Exception e) {
      log.error("Erro ao enriquecer transação: {}", e.getMessage(), e);
      return FullEnrichmentContext.builder()
          .originalRequest(request)
          .velocityContext(VelocityEnrichment.VelocityContext.empty())
          .deviceContext(DeviceEnrichment.DeviceContext.empty())
          .geoContext(GeoEnrichment.GeoContext.empty())
          .customerContext(CustomerEnrichment.CustomerContext.empty())
          .cardContext(CardEnrichment.CardContext.empty())
          .authContext(AuthEnrichment.AuthContext.empty())
          .anomalyContext(AnomalyEnrichment.AnomalyContext.empty())
          .enrichmentTimeMs(System.currentTimeMillis() - startTime)
          .build();
    }
  }

  /**
   * Retorna apenas o Map plano com todos os campos.
   *
   * <p>Atalho para enrichFull(request).toFlatMap()
   *
   * @param request A transação
   * @return Map com todos os campos derivados
   */
  public Map<String, Object> enrichToMap(TransactionRequest request) {
    return enrichFull(request).toFlatMap();
  }

  /**
   * Retorna estatísticas dos serviços de enriquecimento.
   *
   * @return Map com estatísticas
   */
  public Map<String, Object> getStats() {
    Map<String, Object> stats = new HashMap<>();
    stats.put("services", 8);
    stats.put("totalFields", 103);
    stats.put(
        "categories",
        Map.of(
            "basic", 20,
            "velocity", 25,
            "device", 20,
            "geo", 18,
            "customer", 15,
            "card", 12,
            "auth", 8,
            "anomaly", 5));
    stats.put("implementedFields", 123); // Total de campos implementados
    return stats;
  }
}
