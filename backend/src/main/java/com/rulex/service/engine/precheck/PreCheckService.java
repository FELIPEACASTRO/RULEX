package com.rulex.service.engine.precheck;

import com.rulex.dto.TransactionRequest;
import com.rulex.dto.TriggeredRuleDTO;
import com.rulex.entity.RuleList;
import com.rulex.entity.TransactionDecision;
import com.rulex.service.BloomFilterService;
import com.rulex.service.DerivedContext;
import com.rulex.service.GeoService;
import com.rulex.service.ImpossibleTravelService;
import com.rulex.service.RedisVelocityService;
import java.util.List;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

/**
 * Serviço responsável por pré-verificações rápidas antes da avaliação de regras.
 *
 * <p>Extraído de RuleEngineService para seguir Single Responsibility Principle.
 *
 * <p>Responsabilidades:
 * <ul>
 *   <li>Verificação de blacklists via Bloom Filter</li>
 *   <li>Detecção de viagem impossível</li>
 *   <li>Verificações de velocity em Redis</li>
 *   <li>Short-circuit em caso de FRAUD detectado</li>
 * </ul>
 *
 * @author Refactoring - RULEX Team
 * @since 2.0.0
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class PreCheckService {

    private final BloomFilterService bloomFilterService;
    private final ImpossibleTravelService impossibleTravelService;
    private final GeoService geoService;
    private final RedisVelocityService redisVelocityService;

    @Value("${rulex.engine.bloomFilter.enabled:true}")
    private boolean bloomFilterEnabled;

    @Value("${rulex.engine.impossibleTravel.enabled:false}")
    private boolean impossibleTravelEnabled;

    @Value("${rulex.engine.velocity.redis.enabled:false}")
    private boolean redisVelocityEnabled;

    /**
     * Resultado das pré-verificações.
     */
    public record PreCheckResult(
        TransactionDecision.TransactionClassification classification,
        List<TriggeredRuleDTO> triggeredRules,
        Map<String, Object> scoreDetails,
        boolean shouldShortCircuit
    ) {
        public static PreCheckResult clean() {
            return new PreCheckResult(
                TransactionDecision.TransactionClassification.APPROVED,
                List.of(),
                Map.of(),
                false
            );
        }
    }

    /**
     * Executa todas as pré-verificações.
     *
     * @param request dados da transação
     * @param derivedContext contexto derivado
     * @return resultado das verificações
     */
    public PreCheckResult runPreChecks(TransactionRequest request, DerivedContext derivedContext) {
        java.util.List<TriggeredRuleDTO> triggeredRules = new java.util.ArrayList<>();
        java.util.Map<String, Object> scoreDetails = new java.util.HashMap<>();
        TransactionDecision.TransactionClassification maxClassification =
            TransactionDecision.TransactionClassification.APPROVED;

        // 1. Bloom Filter - Blacklists
        if (bloomFilterEnabled) {
            PreCheckResult bloomResult = checkBloomFilters(request, triggeredRules, scoreDetails);
            maxClassification = maxSeverity(maxClassification, bloomResult.classification());

            if (maxClassification == TransactionDecision.TransactionClassification.FRAUD) {
                return new PreCheckResult(maxClassification, triggeredRules, scoreDetails, true);
            }
        }

        // 2. Impossible Travel
        if (impossibleTravelEnabled) {
            PreCheckResult travelResult = checkImpossibleTravel(request, derivedContext, triggeredRules, scoreDetails);
            maxClassification = maxSeverity(maxClassification, travelResult.classification());

            if (maxClassification == TransactionDecision.TransactionClassification.FRAUD) {
                return new PreCheckResult(maxClassification, triggeredRules, scoreDetails, true);
            }
        }

        // 3. Redis Velocity (best-effort)
        if (redisVelocityEnabled) {
            try {
                checkRedisVelocity(request, triggeredRules, scoreDetails);
            } catch (Exception e) {
                log.warn("Redis velocity check failed (best-effort): {}", e.getMessage());
            }
        }

        return new PreCheckResult(maxClassification, triggeredRules, scoreDetails, false);
    }

    /**
     * Verifica blacklists via Bloom Filter.
     */
    private PreCheckResult checkBloomFilters(
            TransactionRequest request,
            List<TriggeredRuleDTO> triggeredRules,
            Map<String, Object> scoreDetails) {

        TransactionDecision.TransactionClassification maxClass =
            TransactionDecision.TransactionClassification.APPROVED;

        // Check PAN
        if (request.getPan() != null && !request.getPan().isBlank()) {
            try {
                var result = bloomFilterService.isBlacklisted(RuleList.EntityType.PAN, request.getPan());
                if (result.inList()) {
                    triggeredRules.add(TriggeredRuleDTO.builder()
                        .name("BLOOM_PAN_BLACKLIST")
                        .weight(100)
                        .contribution(100)
                        .detail("PAN found in blacklist: " + result.source())
                        .build());
                    scoreDetails.put("BLOOM_PAN_BLACKLIST", Map.of(
                        "triggered", true,
                        "source", result.source()
                    ));
                    maxClass = TransactionDecision.TransactionClassification.FRAUD;
                }
            } catch (Exception e) {
                log.debug("Bloom filter PAN check failed: {}", e.getMessage());
            }
        }

        // Check Device ID
        if (request.getDeviceId() != null && !request.getDeviceId().isBlank()) {
            try {
                var result = bloomFilterService.isBlacklisted(RuleList.EntityType.DEVICE_ID, request.getDeviceId());
                if (result.inList()) {
                    triggeredRules.add(TriggeredRuleDTO.builder()
                        .name("BLOOM_DEVICE_BLACKLIST")
                        .weight(100)
                        .contribution(100)
                        .detail("Device ID found in blacklist: " + result.source())
                        .build());
                    scoreDetails.put("BLOOM_DEVICE_BLACKLIST", Map.of(
                        "triggered", true,
                        "source", result.source()
                    ));
                    maxClass = TransactionDecision.TransactionClassification.FRAUD;
                }
            } catch (Exception e) {
                log.debug("Bloom filter device check failed: {}", e.getMessage());
            }
        }

        // Check IP Address
        if (request.getIpAddress() != null && !request.getIpAddress().isBlank()) {
            try {
                var result = bloomFilterService.isBlacklisted(RuleList.EntityType.IP, request.getIpAddress());
                if (result.inList()) {
                    triggeredRules.add(TriggeredRuleDTO.builder()
                        .name("BLOOM_IP_BLACKLIST")
                        .weight(80)
                        .contribution(80)
                        .detail("IP address found in blacklist: " + result.source())
                        .build());
                    scoreDetails.put("BLOOM_IP_BLACKLIST", Map.of(
                        "triggered", true,
                        "source", result.source()
                    ));
                    maxClass = maxSeverity(maxClass, TransactionDecision.TransactionClassification.SUSPICIOUS);
                }
            } catch (Exception e) {
                log.debug("Bloom filter IP check failed: {}", e.getMessage());
            }
        }

        // Check Merchant ID
        if (request.getMerchantId() != null && !request.getMerchantId().isBlank()) {
            try {
                var result = bloomFilterService.isBlacklisted(RuleList.EntityType.MERCHANT_ID, request.getMerchantId());
                if (result.inList()) {
                    triggeredRules.add(TriggeredRuleDTO.builder()
                        .name("BLOOM_MERCHANT_BLACKLIST")
                        .weight(90)
                        .contribution(90)
                        .detail("Merchant ID found in blacklist: " + result.source())
                        .build());
                    scoreDetails.put("BLOOM_MERCHANT_BLACKLIST", Map.of(
                        "triggered", true,
                        "source", result.source()
                    ));
                    maxClass = TransactionDecision.TransactionClassification.FRAUD;
                }
            } catch (Exception e) {
                log.debug("Bloom filter merchant check failed: {}", e.getMessage());
            }
        }

        return new PreCheckResult(maxClass, triggeredRules, scoreDetails,
            maxClass == TransactionDecision.TransactionClassification.FRAUD);
    }

    /**
     * Verifica viagem impossível.
     *
     * NOTA: Implementação simplificada - a verificação completa requer
     * coordenadas geográficas que não estão disponíveis no TransactionRequest.
     */
    private PreCheckResult checkImpossibleTravel(
            TransactionRequest request,
            DerivedContext derivedContext,
            List<TriggeredRuleDTO> triggeredRules,
            Map<String, Object> scoreDetails) {

        // Impossible travel check requires geo coordinates
        // This is a placeholder - full implementation would need lat/lon
        log.debug("Impossible travel check skipped - requires geo coordinates");

        return new PreCheckResult(
            TransactionDecision.TransactionClassification.APPROVED,
            triggeredRules, scoreDetails, false);
    }

    /**
     * Verifica velocity em Redis.
     */
    private void checkRedisVelocity(
            TransactionRequest request,
            List<TriggeredRuleDTO> triggeredRules,
            Map<String, Object> scoreDetails) {

        // Esta é uma verificação best-effort
        // O resultado é registrado mas não bloqueia
        try {
            redisVelocityService.recordTransaction(request);
        } catch (Exception e) {
            log.debug("Redis velocity recording failed: {}", e.getMessage());
        }
    }

    /**
     * Retorna a classificação mais severa.
     */
    private TransactionDecision.TransactionClassification maxSeverity(
            TransactionDecision.TransactionClassification a,
            TransactionDecision.TransactionClassification b) {

        int severityA = severity(a);
        int severityB = severity(b);
        return severityA >= severityB ? a : b;
    }

    /**
     * Retorna severidade numérica da classificação.
     */
    private int severity(TransactionDecision.TransactionClassification c) {
        return switch (c) {
            case FRAUD -> 3;
            case SUSPICIOUS -> 2;
            case APPROVED -> 1;
        };
    }
}
