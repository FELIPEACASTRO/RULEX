package com.rulex.service.engine.operator.strategy;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.GeoService;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Set;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Strategy para operadores de geolocalização.
 *
 * <p>Operadores suportados:
 * <ul>
 *   <li>GEO_DISTANCE_LT - distância menor que</li>
 *   <li>GEO_DISTANCE_GT - distância maior que</li>
 *   <li>GEO_IN_POLYGON - ponto dentro de polígono</li>
 * </ul>
 *
 * @author Refactoring - RULEX Team
 * @since 2.0.0
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class GeoOperatorStrategy implements OperatorStrategy {

    private static final Set<ConditionOperator> SUPPORTED = Set.of(
        ConditionOperator.GEO_DISTANCE_LT,
        ConditionOperator.GEO_DISTANCE_GT,
        ConditionOperator.GEO_IN_POLYGON
    );

    private final GeoService geoService;

    @Override
    public Set<ConditionOperator> supportedOperators() {
        return SUPPORTED;
    }

    @Override
    public String category() {
        return "GEO";
    }

    @Override
    public int priority() {
        return 150; // Prioridade mais baixa - requer cálculos
    }

    @Override
    public boolean requiresExternalServices() {
        return true;
    }

    @Override
    public boolean evaluate(Object fieldValue, RuleCondition condition, EvaluationContext context) {
        ConditionOperator operator = condition.getOperator();

        return switch (operator) {
            case GEO_DISTANCE_LT -> evaluateGeoDistanceLt(fieldValue, condition, context);
            case GEO_DISTANCE_GT -> evaluateGeoDistanceGt(fieldValue, condition, context);
            case GEO_IN_POLYGON -> evaluateGeoInPolygon(fieldValue, condition, context);
            default -> {
                log.warn("Unexpected operator in GeoOperatorStrategy: {}", operator);
                yield false;
            }
        };
    }

    /**
     * Avalia se distância entre dois pontos é menor que threshold.
     */
    private boolean evaluateGeoDistanceLt(Object fieldValue, RuleCondition condition, EvaluationContext context) {
        GeoPoint point1 = parseGeoPoint(fieldValue);
        if (point1 == null) {
            return false;
        }

        // Formato esperado: "lat,lon,distanceKm"
        String valueSingle = condition.getValueSingle();
        if (valueSingle == null) {
            log.warn("GEO_DISTANCE_LT requires 'lat,lon,distanceKm' format");
            return false;
        }

        String[] parts = valueSingle.split(",");
        if (parts.length < 3) {
            log.warn("GEO_DISTANCE_LT requires 'lat,lon,distanceKm' format");
            return false;
        }

        try {
            double lat2 = Double.parseDouble(parts[0].trim());
            double lon2 = Double.parseDouble(parts[1].trim());
            double maxDistanceKm = Double.parseDouble(parts[2].trim());

            double distance = calculateHaversineDistance(point1.lat, point1.lon, lat2, lon2);
            return distance < maxDistanceKm;
        } catch (NumberFormatException e) {
            log.warn("Invalid geo values: {}", valueSingle);
            return false;
        }
    }

    /**
     * Avalia se distância entre dois pontos é maior que threshold.
     */
    private boolean evaluateGeoDistanceGt(Object fieldValue, RuleCondition condition, EvaluationContext context) {
        GeoPoint point1 = parseGeoPoint(fieldValue);
        if (point1 == null) {
            return false;
        }

        String valueSingle = condition.getValueSingle();
        if (valueSingle == null) {
            log.warn("GEO_DISTANCE_GT requires 'lat,lon,distanceKm' format");
            return false;
        }

        String[] parts = valueSingle.split(",");
        if (parts.length < 3) {
            log.warn("GEO_DISTANCE_GT requires 'lat,lon,distanceKm' format");
            return false;
        }

        try {
            double lat2 = Double.parseDouble(parts[0].trim());
            double lon2 = Double.parseDouble(parts[1].trim());
            double minDistanceKm = Double.parseDouble(parts[2].trim());

            double distance = calculateHaversineDistance(point1.lat, point1.lon, lat2, lon2);
            return distance > minDistanceKm;
        } catch (NumberFormatException e) {
            log.warn("Invalid geo values: {}", valueSingle);
            return false;
        }
    }

    /**
     * Avalia se ponto está dentro de um polígono.
     */
    private boolean evaluateGeoInPolygon(Object fieldValue, RuleCondition condition, EvaluationContext context) {
        GeoPoint point = parseGeoPoint(fieldValue);
        if (point == null) {
            return false;
        }

        String polygonName = condition.getValueSingle();
        if (polygonName == null || polygonName.isBlank()) {
            log.warn("GEO_IN_POLYGON requires polygon name in valueSingle");
            return false;
        }

        try {
            // Usar o TransactionRequest do contexto para avaliar
            var request = context.getTransactionRequest();
            if (request != null) {
                var result = geoService.evaluateInPolygon(request, polygonName);
                return result != null && result.isSuccess() && result.isResult();
            }
            return false;
        } catch (Exception e) {
            log.warn("Error checking point in polygon {}: {}", polygonName, e.getMessage());
            return false;
        }
    }

    /**
     * Parse de ponto geográfico de vários formatos.
     */
    private GeoPoint parseGeoPoint(Object value) {
        if (value == null) {
            return null;
        }

        if (value instanceof String str) {
            // Formato "lat,lon"
            String[] parts = str.split(",");
            if (parts.length >= 2) {
                try {
                    double lat = Double.parseDouble(parts[0].trim());
                    double lon = Double.parseDouble(parts[1].trim());
                    return new GeoPoint(lat, lon);
                } catch (NumberFormatException e) {
                    return null;
                }
            }
        }

        // Tentar extrair de objeto com campos lat/lon
        try {
            var latField = value.getClass().getDeclaredField("lat");
            var lonField = value.getClass().getDeclaredField("lon");
            latField.setAccessible(true);
            lonField.setAccessible(true);
            double lat = ((Number) latField.get(value)).doubleValue();
            double lon = ((Number) lonField.get(value)).doubleValue();
            return new GeoPoint(lat, lon);
        } catch (Exception e) {
            // Tentar latitude/longitude
            try {
                var latField = value.getClass().getDeclaredField("latitude");
                var lonField = value.getClass().getDeclaredField("longitude");
                latField.setAccessible(true);
                lonField.setAccessible(true);
                double lat = ((Number) latField.get(value)).doubleValue();
                double lon = ((Number) lonField.get(value)).doubleValue();
                return new GeoPoint(lat, lon);
            } catch (Exception ex) {
                return null;
            }
        }
    }

    /**
     * Calcula distância entre dois pontos usando fórmula de Haversine.
     *
     * @return distância em quilômetros
     */
    private double calculateHaversineDistance(double lat1, double lon1, double lat2, double lon2) {
        final double R = 6371; // Raio da Terra em km

        double latDistance = Math.toRadians(lat2 - lat1);
        double lonDistance = Math.toRadians(lon2 - lon1);

        double a = Math.sin(latDistance / 2) * Math.sin(latDistance / 2)
                + Math.cos(Math.toRadians(lat1)) * Math.cos(Math.toRadians(lat2))
                * Math.sin(lonDistance / 2) * Math.sin(lonDistance / 2);

        double c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));

        return R * c;
    }

    /**
     * Representa um ponto geográfico.
     */
    private record GeoPoint(double lat, double lon) {}
}
