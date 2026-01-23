package com.rulex.service.engine;

import com.rulex.dto.TransactionRequest;
import com.rulex.dto.TriggeredRuleDTO;
import com.rulex.entity.TransactionDecision;
import com.rulex.service.BloomFilterService;
import com.rulex.service.DerivedContext;
import com.rulex.service.GeoService;
import com.rulex.service.ImpossibleTravelService;
import com.rulex.util.PanHashUtil;
import java.util.List;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@Slf4j
public class RuleEnginePrecheckHelper {

  private final BloomFilterService bloomFilterService;
  private final ImpossibleTravelService impossibleTravelService;
  private final GeoService geoService;

  public TransactionDecision.TransactionClassification runPreChecks(
      TransactionRequest request,
      DerivedContext derivedContext,
      List<TriggeredRuleDTO> triggeredRules,
      Map<String, Object> scoreDetails,
      boolean bloomFilterEnabled,
      boolean impossibleTravelEnabled) {
    TransactionDecision.TransactionClassification max =
        TransactionDecision.TransactionClassification.APPROVED;

    if (bloomFilterEnabled) {
      try {
        if (request.getPan() != null && !request.getPan().isBlank()) {
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
        }

        if (request.getMerchantId() != null && !request.getMerchantId().isBlank()) {
          var r =
              bloomFilterService.isBlacklisted(
                  com.rulex.entity.RuleList.EntityType.MERCHANT_ID, request.getMerchantId());
          if (r.inList()) {
            triggeredRules.add(
                TriggeredRuleDTO.builder()
                    .name("BLOCKLIST_MERCHANT")
                    .weight(100)
                    .contribution(100)
                    .detail("MerchantId encontrado em blacklist")
                    .build());
            scoreDetails.put(
                "BLOCKLIST_MERCHANT", Map.of("triggered", true, "source", r.source()));
            return TransactionDecision.TransactionClassification.FRAUD;
          }
        }

        if (request.getCustomerIdFromHeader() != null
            && !request.getCustomerIdFromHeader().isBlank()) {
          var r =
              bloomFilterService.isBlacklisted(
                  com.rulex.entity.RuleList.EntityType.CUSTOMER_ID,
                  request.getCustomerIdFromHeader());
          if (r.inList()) {
            triggeredRules.add(
                TriggeredRuleDTO.builder()
                    .name("BLOCKLIST_CUSTOMER")
                    .weight(100)
                    .contribution(100)
                    .detail("CustomerId encontrado em blacklist")
                    .build());
            scoreDetails.put(
                "BLOCKLIST_CUSTOMER", Map.of("triggered", true, "source", r.source()));
            return TransactionDecision.TransactionClassification.FRAUD;
          }
        }

        // Use terminalId as a proxy for device id if present.
        if (request.getTerminalId() != null && !request.getTerminalId().isBlank()) {
          var r =
              bloomFilterService.isBlacklisted(
                  com.rulex.entity.RuleList.EntityType.DEVICE_ID, request.getTerminalId());
          if (r.inList()) {
            triggeredRules.add(
                TriggeredRuleDTO.builder()
                    .name("BLOCKLIST_DEVICE")
                    .weight(100)
                    .contribution(100)
                    .detail("DeviceId (terminalId) encontrado em blacklist")
                    .build());
            scoreDetails.put(
                "BLOCKLIST_DEVICE", Map.of("triggered", true, "source", r.source()));
            return TransactionDecision.TransactionClassification.FRAUD;
          }
        }
      } catch (Exception e) { // SEC-006 FIX
        // best-effort
        log.debug("Erro best-effort no bloom filter pre-check", e);
      }
    }

    if (impossibleTravelEnabled
        && request.getPan() != null
        && !request.getPan().isBlank()
        && derivedContext.getTransactionTimestamp() != null) {
      try {
        GeoService.GeoCoordinates coords = geoService.deriveCoordinates(request);
        if (coords != null && coords.isFound()) {
          String panHash = PanHashUtil.sha256Hex(request.getPan());
          boolean cardPresent = "Y".equals(request.getCustomerPresent());

          var analysis =
              impossibleTravelService.analyzeTravel(
                  panHash,
                  coords.getLatitude(),
                  coords.getLongitude(),
                  request.getMerchantCity(),
                  derivedContext.getNormalizedCountryCode(),
                  derivedContext.getTransactionTimestamp().toLocalDateTime(),
                  request.getExternalTransactionId(),
                  cardPresent);

          if (analysis.riskLevel() == ImpossibleTravelService.TravelRisk.IMPOSSIBLE) {
            triggeredRules.add(
                TriggeredRuleDTO.builder()
                    .name("IMPOSSIBLE_TRAVEL")
                    .weight(100)
                    .contribution(100)
                    .detail(
                        String.format(
                            "Viagem impossível: %.0fkm em %.0fmin (%.0f km/h)",
                            analysis.distanceKm(), analysis.elapsedMinutes(), analysis.speedKmh()))
                    .build());
            scoreDetails.put(
                "IMPOSSIBLE_TRAVEL",
                Map.of(
                    "triggered",
                    true,
                    "distanceKm",
                    analysis.distanceKm(),
                    "speedKmh",
                    analysis.speedKmh(),
                    "elapsedMinutes",
                    analysis.elapsedMinutes()));
            return TransactionDecision.TransactionClassification.FRAUD;
          }

          if (analysis.riskLevel() == ImpossibleTravelService.TravelRisk.HIGH) {
            triggeredRules.add(
                TriggeredRuleDTO.builder()
                    .name("SUSPICIOUS_TRAVEL")
                    .weight(60)
                    .contribution(60)
                    .detail(String.format("Viagem suspeita: %.0f km/h", analysis.speedKmh()))
                    .build());
            scoreDetails.put(
                "SUSPICIOUS_TRAVEL", Map.of("triggered", true, "speedKmh", analysis.speedKmh()));
            max = TransactionDecision.TransactionClassification.SUSPICIOUS;
          }
        }
      } catch (Exception e) { // SEC-006 FIX
        // best-effort
        log.debug("Erro best-effort no impossible travel pre-check", e);
      }
    }

    return max;
  }
}
