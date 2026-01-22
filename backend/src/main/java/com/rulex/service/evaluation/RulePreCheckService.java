package com.rulex.service.evaluation;

import com.rulex.dto.TransactionRequest;
import com.rulex.dto.TriggeredRuleDTO;
import com.rulex.entity.TransactionDecision;
import com.rulex.service.BloomFilterService;
import com.rulex.service.EnrichmentService;
import java.util.List;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

/**
 * Serviço de pre-checks para regras.
 *
 * <p>Extraído de RuleEngineService para melhorar a manutenibilidade. Responsável por executar
 * verificações prévias como blocklists, whitelists e classificação de risco.
 */
@Service
@RequiredArgsConstructor
public class RulePreCheckService {

  private final BloomFilterService bloomFilterService;
  private final EnrichmentService enrichmentService;

  @Value("${rulex.engine.bloomFilter.enabled:true}")
  private boolean bloomFilterEnabled;

  /**
   * Executa pre-checks em uma transação.
   *
   * @param request a transação
   * @param triggeredRules lista para adicionar regras disparadas
   * @param scoreDetails mapa para adicionar detalhes de score
   * @return a classificação máxima encontrada nos pre-checks
   */
  public TransactionDecision.TransactionClassification runPreChecks(
      TransactionRequest request,
      List<TriggeredRuleDTO> triggeredRules,
      Map<String, Object> scoreDetails) {

    TransactionDecision.TransactionClassification max =
        TransactionDecision.TransactionClassification.APPROVED;

    if (!bloomFilterEnabled) {
      return max;
    }

    // Check PAN blocklist
    max = checkPanBlocklist(request, triggeredRules, scoreDetails, max);
    if (max == TransactionDecision.TransactionClassification.FRAUD) {
      return max;
    }

    // Check Merchant blocklist
    max = checkMerchantBlocklist(request, triggeredRules, scoreDetails, max);
    if (max == TransactionDecision.TransactionClassification.FRAUD) {
      return max;
    }

    // Check Customer blocklist
    max = checkCustomerBlocklist(request, triggeredRules, scoreDetails, max);
    if (max == TransactionDecision.TransactionClassification.FRAUD) {
      return max;
    }

    // Check Device blocklist
    max = checkDeviceBlocklist(request, triggeredRules, scoreDetails, max);
    if (max == TransactionDecision.TransactionClassification.FRAUD) {
      return max;
    }

    // Check IP blocklist
    max = checkIpBlocklist(request, triggeredRules, scoreDetails, max);
    if (max == TransactionDecision.TransactionClassification.FRAUD) {
      return max;
    }

    // Check whitelists
    max = checkWhitelists(request, triggeredRules, scoreDetails, max);

    return max;
  }

  private TransactionDecision.TransactionClassification checkPanBlocklist(
      TransactionRequest request,
      List<TriggeredRuleDTO> triggeredRules,
      Map<String, Object> scoreDetails,
      TransactionDecision.TransactionClassification current) {

    if (request.getPan() == null || request.getPan().isBlank()) {
      return current;
    }

    try {
      var r =
          bloomFilterService.isBlacklisted(
              com.rulex.entity.RuleList.EntityType.PAN, request.getPan());
      if (r.inList()) {
        triggeredRules.add(
            TriggeredRuleDTO.builder()
                .name("BLOCKLIST_PAN")
                .weight(100)
                .contribution(100)
                .detail("PAN encontrado em blacklist")
                .build());
        scoreDetails.put("BLOCKLIST_PAN", Map.of("triggered", true, "source", r.source()));
        return TransactionDecision.TransactionClassification.FRAUD;
      }
    } catch (Exception e) {
      // Log and continue
    }
    return current;
  }

  private TransactionDecision.TransactionClassification checkMerchantBlocklist(
      TransactionRequest request,
      List<TriggeredRuleDTO> triggeredRules,
      Map<String, Object> scoreDetails,
      TransactionDecision.TransactionClassification current) {

    if (request.getMerchantId() == null || request.getMerchantId().isBlank()) {
      return current;
    }

    try {
      var r =
          bloomFilterService.isBlacklisted(
              com.rulex.entity.RuleList.EntityType.MERCHANT_ID, request.getMerchantId());
      if (r.inList()) {
        triggeredRules.add(
            TriggeredRuleDTO.builder()
                .name("BLOCKLIST_MERCHANT")
                .weight(100)
                .contribution(100)
                .detail("Merchant encontrado em blacklist")
                .build());
        scoreDetails.put("BLOCKLIST_MERCHANT", Map.of("triggered", true, "source", r.source()));
        return TransactionDecision.TransactionClassification.FRAUD;
      }
    } catch (Exception e) {
      // Log and continue
    }
    return current;
  }

  private TransactionDecision.TransactionClassification checkCustomerBlocklist(
      TransactionRequest request,
      List<TriggeredRuleDTO> triggeredRules,
      Map<String, Object> scoreDetails,
      TransactionDecision.TransactionClassification current) {

    if (request.getCustomerIdFromHeader() == null || request.getCustomerIdFromHeader().isBlank()) {
      return current;
    }

    try {
      var r =
          bloomFilterService.isBlacklisted(
              com.rulex.entity.RuleList.EntityType.CUSTOMER_ID, request.getCustomerIdFromHeader());
      if (r.inList()) {
        triggeredRules.add(
            TriggeredRuleDTO.builder()
                .name("BLOCKLIST_CUSTOMER")
                .weight(100)
                .contribution(100)
                .detail("Customer encontrado em blacklist")
                .build());
        scoreDetails.put("BLOCKLIST_CUSTOMER", Map.of("triggered", true, "source", r.source()));
        return TransactionDecision.TransactionClassification.FRAUD;
      }
    } catch (Exception e) {
      // Log and continue
    }
    return current;
  }

  private TransactionDecision.TransactionClassification checkDeviceBlocklist(
      TransactionRequest request,
      List<TriggeredRuleDTO> triggeredRules,
      Map<String, Object> scoreDetails,
      TransactionDecision.TransactionClassification current) {

    if (request.getDeviceId() == null || request.getDeviceId().isBlank()) {
      return current;
    }

    try {
      var r =
          bloomFilterService.isBlacklisted(
              com.rulex.entity.RuleList.EntityType.DEVICE_ID, request.getDeviceId());
      if (r.inList()) {
        triggeredRules.add(
            TriggeredRuleDTO.builder()
                .name("BLOCKLIST_DEVICE")
                .weight(100)
                .contribution(100)
                .detail("Device encontrado em blacklist")
                .build());
        scoreDetails.put("BLOCKLIST_DEVICE", Map.of("triggered", true, "source", r.source()));
        return TransactionDecision.TransactionClassification.FRAUD;
      }
    } catch (Exception e) {
      // Log and continue
    }
    return current;
  }

  private TransactionDecision.TransactionClassification checkIpBlocklist(
      TransactionRequest request,
      List<TriggeredRuleDTO> triggeredRules,
      Map<String, Object> scoreDetails,
      TransactionDecision.TransactionClassification current) {

    if (request.getIpAddress() == null || request.getIpAddress().isBlank()) {
      return current;
    }

    try {
      var r =
          bloomFilterService.isBlacklisted(
              com.rulex.entity.RuleList.EntityType.IP, request.getIpAddress());
      if (r.inList()) {
        triggeredRules.add(
            TriggeredRuleDTO.builder()
                .name("BLOCKLIST_IP")
                .weight(100)
                .contribution(100)
                .detail("IP encontrado em blacklist")
                .build());
        scoreDetails.put("BLOCKLIST_IP", Map.of("triggered", true, "source", r.source()));
        return TransactionDecision.TransactionClassification.FRAUD;
      }
    } catch (Exception e) {
      // Log and continue
    }
    return current;
  }

  private TransactionDecision.TransactionClassification checkWhitelists(
      TransactionRequest request,
      List<TriggeredRuleDTO> triggeredRules,
      Map<String, Object> scoreDetails,
      TransactionDecision.TransactionClassification current) {

    // Check PAN whitelist
    if (request.getPan() != null && !request.getPan().isBlank()) {
      try {
        var r =
            bloomFilterService.isWhitelisted(
                com.rulex.entity.RuleList.EntityType.PAN, request.getPan());
        if (r.inList()) {
          triggeredRules.add(
              TriggeredRuleDTO.builder()
                  .name("WHITELIST_PAN")
                  .weight(-50)
                  .contribution(-50)
                  .detail("PAN encontrado em whitelist")
                  .build());
          scoreDetails.put("WHITELIST_PAN", Map.of("triggered", true, "source", r.source()));
        }
      } catch (Exception e) {
        // Log and continue
      }
    }

    // Check Customer whitelist
    if (request.getCustomerIdFromHeader() != null && !request.getCustomerIdFromHeader().isBlank()) {
      try {
        var r =
            bloomFilterService.isWhitelisted(
                com.rulex.entity.RuleList.EntityType.CUSTOMER_ID,
                request.getCustomerIdFromHeader());
        if (r.inList()) {
          triggeredRules.add(
              TriggeredRuleDTO.builder()
                  .name("WHITELIST_CUSTOMER")
                  .weight(-30)
                  .contribution(-30)
                  .detail("Customer encontrado em whitelist")
                  .build());
          scoreDetails.put("WHITELIST_CUSTOMER", Map.of("triggered", true, "source", r.source()));
        }
      } catch (Exception e) {
        // Log and continue
      }
    }

    return current;
  }

  /**
   * Verifica se o MCC é de alto risco.
   *
   * @param mcc o código MCC
   * @return true se é de alto risco
   */
  public boolean isHighRiskMcc(Integer mcc) {
    return enrichmentService.isHighRiskMcc(mcc);
  }

  /**
   * Verifica se é uma transação internacional.
   *
   * @param request a transação
   * @return true se é internacional
   */
  public boolean isInternationalTransaction(TransactionRequest request) {
    return request.getMerchantCountryCode() != null
        && !request.getMerchantCountryCode().equals("076");
  }

  /**
   * Classifica o risco baseado no score.
   *
   * @param riskScore o score de risco
   * @return a classificação
   */
  public TransactionDecision.TransactionClassification classifyRiskByScore(int riskScore) {
    if (riskScore < 30) {
      return TransactionDecision.TransactionClassification.APPROVED;
    }
    if (riskScore < 70) {
      return TransactionDecision.TransactionClassification.SUSPICIOUS;
    }
    return TransactionDecision.TransactionClassification.FRAUD;
  }
}
