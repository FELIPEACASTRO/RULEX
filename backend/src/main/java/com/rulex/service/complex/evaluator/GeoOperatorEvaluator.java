package com.rulex.service.complex.evaluator;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.RuleCondition.ConditionOperator;
import com.rulex.service.GeoService;
import com.rulex.service.GeoService.GeoResult;
import com.rulex.service.ImpossibleTravelService;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Map;
import java.util.Set;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Avaliador para operadores geográficos.
 *
 * <p>Operadores suportados:
 * <ul>
 *   <li>GEO_DISTANCE_LT, GEO_DISTANCE_GT - distância entre pontos</li>
 *   <li>GEO_IN_POLYGON - ponto dentro de polígono</li>
 * </ul>
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class GeoOperatorEvaluator implements OperatorEvaluator {

    private final GeoService geoService;
    private final ImpossibleTravelService impossibleTravelService;

    private static final Set<ConditionOperator> SUPPORTED = Set.of(
        ConditionOperator.GEO_DISTANCE_LT,
        ConditionOperator.GEO_DISTANCE_GT,
        ConditionOperator.GEO_IN_POLYGON
    );

    @Override
    public Set<ConditionOperator> getSupportedOperators() {
        return SUPPORTED;
    }

    @Override
    public boolean evaluate(RuleCondition condition, EvaluationContext context) {
        ConditionOperator op = condition.getOperator();
        TransactionRequest request = context.getTransactionRequest();

        log.debug("GeoOperatorEvaluator: op={}", op);

        if (request == null) {
            log.debug("TransactionRequest is null");
            return false;
        }

        try {
            return switch (op) {
                case GEO_DISTANCE_LT -> evaluateGeoDistanceLt(request, condition);
                case GEO_DISTANCE_GT -> evaluateGeoDistanceGt(request, condition);
                case GEO_IN_POLYGON -> evaluateGeoInPolygon(request, condition);
                default -> false;
            };
        } catch (Exception e) {
            log.error("Erro ao avaliar operador geográfico {}: {}", op, e.getMessage());
            return false;
        }
    }

    private boolean evaluateGeoDistanceLt(TransactionRequest request, RuleCondition condition) {
        double maxDistanceKm = parseDouble(condition.getValueSingle(), 100.0);

        // Obter coordenadas de referência do condition
        double refLat = parseDouble(condition.getValueMin(), 0.0);
        double refLon = parseDouble(condition.getValueMax(), 0.0);

        GeoResult result = geoService.evaluateDistanceLessThan(request, refLat, refLon, maxDistanceKm);
        log.debug("GEO_DISTANCE_LT: maxDistance={}, result={}", maxDistanceKm, result.isResult());
        return result.isResult();
    }

    private boolean evaluateGeoDistanceGt(TransactionRequest request, RuleCondition condition) {
        double minDistanceKm = parseDouble(condition.getValueSingle(), 100.0);

        // Obter coordenadas de referência do condition
        double refLat = parseDouble(condition.getValueMin(), 0.0);
        double refLon = parseDouble(condition.getValueMax(), 0.0);

        GeoResult result = geoService.evaluateDistanceGreaterThan(request, refLat, refLon, minDistanceKm);
        log.debug("GEO_DISTANCE_GT: minDistance={}, result={}", minDistanceKm, result.isResult());
        return result.isResult();
    }

    private boolean evaluateGeoInPolygon(TransactionRequest request, RuleCondition condition) {
        String polygonName = condition.getValueSingle();
        if (polygonName == null || polygonName.isEmpty()) {
            log.warn("GEO_IN_POLYGON: polygonName is null or empty");
            return false;
        }

        GeoResult result = geoService.evaluateInPolygon(request, polygonName);
        log.debug("GEO_IN_POLYGON: polygon={}, result={}", polygonName, result.isResult());
        return result.isResult();
    }

    private double parseDouble(String value, double defaultValue) {
        if (value == null || value.isEmpty()) {
            return defaultValue;
        }
        try {
            return Double.parseDouble(value);
        } catch (NumberFormatException e) {
            log.warn("Erro ao parsear double '{}', usando default {}", value, defaultValue);
            return defaultValue;
        }
    }

    @Override
    public String getCategory() {
        return "GEO";
    }
}
