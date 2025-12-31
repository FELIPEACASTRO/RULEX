package com.rulex.service;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.TransactionRequest;
import com.rulex.entity.GeoPolygon;
import com.rulex.entity.GeoReference;
import com.rulex.repository.GeoPolygonRepository;
import com.rulex.repository.GeoReferenceRepository;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * Serviço de geolocalização para operadores GEO_*.
 * 
 * <p>Implementa:
 * - GEO_DISTANCE_LT/GT: Distância Haversine entre dois pontos
 * - GEO_IN_POLYGON: Verificação de ponto dentro de polígono (ray casting)
 * 
 * <p>Como o payload não pode ser alterado, as coordenadas são derivadas de:
 * - merchantCity + merchantState + merchantCountryCode → lookup na tabela geo_reference
 * - Fallback para centroid do país se cidade não encontrada
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class GeoService {

    private static final double EARTH_RADIUS_KM = 6371.0;
    
    private final GeoReferenceRepository geoReferenceRepository;
    private final GeoPolygonRepository geoPolygonRepository;
    private final ObjectMapper objectMapper;

    // Cache simples para evitar lookups repetidos
    private final Map<String, GeoCoordinates> coordinatesCache = new ConcurrentHashMap<>();
    private final Map<String, List<double[]>> polygonCache = new ConcurrentHashMap<>();

    /**
     * Coordenadas geográficas.
     */
    @Data
    @Builder
    public static class GeoCoordinates {
        private final double latitude;
        private final double longitude;
        private final String source; // CITY, STATE, COUNTRY, FALLBACK
        private final boolean found;

        public static GeoCoordinates notFound() {
            return GeoCoordinates.builder()
                    .latitude(0)
                    .longitude(0)
                    .source("NOT_FOUND")
                    .found(false)
                    .build();
        }
    }

    /**
     * Resultado de operação GEO.
     */
    @Data
    @Builder
    public static class GeoResult {
        private final boolean success;
        private final boolean result;
        private final double distance; // Para GEO_DISTANCE
        private final String errorMessage;
        private final GeoCoordinates point1;
        private final GeoCoordinates point2;
    }

    /**
     * Deriva coordenadas de uma transação.
     * Usa merchantCity, merchantState, merchantCountryCode para lookup.
     */
    public GeoCoordinates deriveCoordinates(TransactionRequest request) {
        if (request == null) {
            return GeoCoordinates.notFound();
        }

        String countryCode = request.getMerchantCountryCode();
        String city = request.getMerchantCity();
        String state = request.getMerchantState();

        if (countryCode == null || countryCode.isBlank()) {
            log.debug("merchantCountryCode ausente, não é possível derivar coordenadas");
            return GeoCoordinates.notFound();
        }

        // Gerar chave de cache
        String cacheKey = String.format("%s|%s|%s", 
                countryCode, 
                state != null ? state : "", 
                city != null ? city : "");

        return coordinatesCache.computeIfAbsent(cacheKey, k -> lookupCoordinates(countryCode, state, city));
    }

    /**
     * Busca coordenadas na tabela de referência.
     */
    private GeoCoordinates lookupCoordinates(String countryCode, String state, String city) {
        try {
            // 1. Tentar busca exata (país + estado + cidade)
            if (city != null && !city.isBlank()) {
                Optional<GeoReference> exact = geoReferenceRepository.findByLocation(
                        countryCode, state, normalizeCity(city));
                if (exact.isPresent()) {
                    GeoReference ref = exact.get();
                    return GeoCoordinates.builder()
                            .latitude(ref.getLatitude().doubleValue())
                            .longitude(ref.getLongitude().doubleValue())
                            .source("CITY")
                            .found(true)
                            .build();
                }

                // Tentar só por cidade (pode estar em outro estado)
                List<GeoReference> byCity = geoReferenceRepository.findByCityNameIgnoreCase(normalizeCity(city));
                if (!byCity.isEmpty()) {
                    GeoReference ref = byCity.get(0);
                    return GeoCoordinates.builder()
                            .latitude(ref.getLatitude().doubleValue())
                            .longitude(ref.getLongitude().doubleValue())
                            .source("CITY_FUZZY")
                            .found(true)
                            .build();
                }
            }

            // 2. Fallback para capital do estado (se tiver estado)
            if (state != null && !state.isBlank()) {
                List<GeoReference> byState = geoReferenceRepository.findByCountryCodeAndStateCode(countryCode, state);
                if (!byState.isEmpty()) {
                    // Pegar a maior cidade do estado (por população)
                    GeoReference ref = byState.stream()
                            .max((a, b) -> {
                                int popA = a.getPopulation() != null ? a.getPopulation() : 0;
                                int popB = b.getPopulation() != null ? b.getPopulation() : 0;
                                return Integer.compare(popA, popB);
                            })
                            .orElse(byState.get(0));
                    return GeoCoordinates.builder()
                            .latitude(ref.getLatitude().doubleValue())
                            .longitude(ref.getLongitude().doubleValue())
                            .source("STATE")
                            .found(true)
                            .build();
                }
            }

            // 3. Fallback para capital do país
            Optional<GeoReference> capital = geoReferenceRepository.findCapitalByCountryCode(countryCode);
            if (capital.isPresent()) {
                GeoReference ref = capital.get();
                return GeoCoordinates.builder()
                        .latitude(ref.getLatitude().doubleValue())
                        .longitude(ref.getLongitude().doubleValue())
                        .source("COUNTRY_CAPITAL")
                        .found(true)
                        .build();
            }

            // 4. Fallback para qualquer referência do país
            List<GeoReference> byCountry = geoReferenceRepository.findByCountryCode(countryCode);
            if (!byCountry.isEmpty()) {
                GeoReference ref = byCountry.get(0);
                return GeoCoordinates.builder()
                        .latitude(ref.getLatitude().doubleValue())
                        .longitude(ref.getLongitude().doubleValue())
                        .source("COUNTRY_FALLBACK")
                        .found(true)
                        .build();
            }

            log.debug("Nenhuma referência geográfica encontrada para: country={}, state={}, city={}",
                    countryCode, state, city);
            return GeoCoordinates.notFound();

        } catch (Exception e) {
            log.warn("Erro ao buscar coordenadas: {}", e.getMessage());
            return GeoCoordinates.notFound();
        }
    }

    /**
     * Normaliza nome de cidade para busca.
     */
    private String normalizeCity(String city) {
        if (city == null) return null;
        return city.toUpperCase()
                .replaceAll("[^A-Z0-9 ]", "")
                .trim();
    }

    /**
     * Calcula distância Haversine entre dois pontos em km.
     */
    public double haversineDistance(double lat1, double lon1, double lat2, double lon2) {
        double dLat = Math.toRadians(lat2 - lat1);
        double dLon = Math.toRadians(lon2 - lon1);

        double a = Math.sin(dLat / 2) * Math.sin(dLat / 2) +
                   Math.cos(Math.toRadians(lat1)) * Math.cos(Math.toRadians(lat2)) *
                   Math.sin(dLon / 2) * Math.sin(dLon / 2);

        double c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));

        return EARTH_RADIUS_KM * c;
    }

    /**
     * Avalia GEO_DISTANCE_LT: distância menor que threshold.
     * 
     * @param request Transação (para derivar coordenadas)
     * @param targetLat Latitude do ponto de referência
     * @param targetLon Longitude do ponto de referência
     * @param thresholdKm Distância máxima em km
     * @return Resultado da avaliação
     */
    public GeoResult evaluateDistanceLessThan(
            TransactionRequest request, 
            double targetLat, 
            double targetLon, 
            double thresholdKm) {
        
        GeoCoordinates coords = deriveCoordinates(request);
        if (!coords.isFound()) {
            return GeoResult.builder()
                    .success(false)
                    .result(false)
                    .errorMessage("Não foi possível derivar coordenadas da transação")
                    .point1(coords)
                    .build();
        }

        double distance = haversineDistance(coords.getLatitude(), coords.getLongitude(), targetLat, targetLon);
        boolean result = distance < thresholdKm;

        log.debug("GEO_DISTANCE_LT: ({},{}) -> ({},{}) = {:.2f}km < {:.2f}km = {}",
                coords.getLatitude(), coords.getLongitude(),
                targetLat, targetLon,
                distance, thresholdKm, result);

        return GeoResult.builder()
                .success(true)
                .result(result)
                .distance(distance)
                .point1(coords)
                .point2(GeoCoordinates.builder()
                        .latitude(targetLat)
                        .longitude(targetLon)
                        .source("TARGET")
                        .found(true)
                        .build())
                .build();
    }

    /**
     * Avalia GEO_DISTANCE_GT: distância maior que threshold.
     */
    public GeoResult evaluateDistanceGreaterThan(
            TransactionRequest request, 
            double targetLat, 
            double targetLon, 
            double thresholdKm) {
        
        GeoCoordinates coords = deriveCoordinates(request);
        if (!coords.isFound()) {
            return GeoResult.builder()
                    .success(false)
                    .result(false)
                    .errorMessage("Não foi possível derivar coordenadas da transação")
                    .point1(coords)
                    .build();
        }

        double distance = haversineDistance(coords.getLatitude(), coords.getLongitude(), targetLat, targetLon);
        boolean result = distance > thresholdKm;

        log.debug("GEO_DISTANCE_GT: ({},{}) -> ({},{}) = {:.2f}km > {:.2f}km = {}",
                coords.getLatitude(), coords.getLongitude(),
                targetLat, targetLon,
                distance, thresholdKm, result);

        return GeoResult.builder()
                .success(true)
                .result(result)
                .distance(distance)
                .point1(coords)
                .point2(GeoCoordinates.builder()
                        .latitude(targetLat)
                        .longitude(targetLon)
                        .source("TARGET")
                        .found(true)
                        .build())
                .build();
    }

    /**
     * Avalia GEO_IN_POLYGON: ponto dentro de polígono.
     * 
     * @param request Transação (para derivar coordenadas)
     * @param polygonName Nome do polígono na tabela geo_polygon
     * @return Resultado da avaliação
     */
    public GeoResult evaluateInPolygon(TransactionRequest request, String polygonName) {
        GeoCoordinates coords = deriveCoordinates(request);
        if (!coords.isFound()) {
            return GeoResult.builder()
                    .success(false)
                    .result(false)
                    .errorMessage("Não foi possível derivar coordenadas da transação")
                    .point1(coords)
                    .build();
        }

        try {
            // Buscar polígono
            Optional<GeoPolygon> polygonOpt = geoPolygonRepository.findByNameIgnoreCase(polygonName);
            if (polygonOpt.isEmpty()) {
                return GeoResult.builder()
                        .success(false)
                        .result(false)
                        .errorMessage("Polígono não encontrado: " + polygonName)
                        .point1(coords)
                        .build();
            }

            GeoPolygon polygon = polygonOpt.get();
            
            // Verificar bounding box primeiro (otimização)
            if (!isInBoundingBox(coords.getLatitude(), coords.getLongitude(), polygon)) {
                return GeoResult.builder()
                        .success(true)
                        .result(false)
                        .point1(coords)
                        .build();
            }

            // Parsear pontos do polígono
            List<double[]> points = parsePolygonPoints(polygon);
            if (points == null || points.size() < 3) {
                return GeoResult.builder()
                        .success(false)
                        .result(false)
                        .errorMessage("Polígono inválido (menos de 3 pontos)")
                        .point1(coords)
                        .build();
            }

            // Ray casting algorithm
            boolean inside = isPointInPolygon(coords.getLatitude(), coords.getLongitude(), points);

            log.debug("GEO_IN_POLYGON: ({},{}) in {} = {}",
                    coords.getLatitude(), coords.getLongitude(), polygonName, inside);

            return GeoResult.builder()
                    .success(true)
                    .result(inside)
                    .point1(coords)
                    .build();

        } catch (Exception e) {
            log.error("Erro ao avaliar GEO_IN_POLYGON: {}", e.getMessage());
            return GeoResult.builder()
                    .success(false)
                    .result(false)
                    .errorMessage("Erro ao avaliar polígono: " + e.getMessage())
                    .point1(coords)
                    .build();
        }
    }

    /**
     * Verifica se ponto está dentro do bounding box do polígono.
     */
    private boolean isInBoundingBox(double lat, double lon, GeoPolygon polygon) {
        if (polygon.getMinLat() == null || polygon.getMaxLat() == null ||
            polygon.getMinLon() == null || polygon.getMaxLon() == null) {
            return true; // Se não tem bbox, assume que pode estar dentro
        }

        return lat >= polygon.getMinLat().doubleValue() &&
               lat <= polygon.getMaxLat().doubleValue() &&
               lon >= polygon.getMinLon().doubleValue() &&
               lon <= polygon.getMaxLon().doubleValue();
    }

    /**
     * Parseia pontos do polígono do JSON.
     */
    @SuppressWarnings("unchecked")
    private List<double[]> parsePolygonPoints(GeoPolygon polygon) {
        String cacheKey = polygon.getName();
        return polygonCache.computeIfAbsent(cacheKey, k -> {
            try {
                List<List<Double>> rawPoints = objectMapper.readValue(
                        polygon.getPolygonPoints(),
                        new TypeReference<List<List<Double>>>() {});
                
                return rawPoints.stream()
                        .map(p -> new double[]{p.get(0), p.get(1)})
                        .toList();
            } catch (Exception e) {
                log.error("Erro ao parsear polígono {}: {}", polygon.getName(), e.getMessage());
                return null;
            }
        });
    }

    /**
     * Ray casting algorithm para verificar se ponto está dentro de polígono.
     * https://en.wikipedia.org/wiki/Point_in_polygon
     */
    private boolean isPointInPolygon(double lat, double lon, List<double[]> polygon) {
        int n = polygon.size();
        boolean inside = false;

        for (int i = 0, j = n - 1; i < n; j = i++) {
            double[] pi = polygon.get(i);
            double[] pj = polygon.get(j);

            double xi = pi[1], yi = pi[0]; // lon, lat
            double xj = pj[1], yj = pj[0];

            if (((yi > lat) != (yj > lat)) &&
                (lon < (xj - xi) * (lat - yi) / (yj - yi) + xi)) {
                inside = !inside;
            }
        }

        return inside;
    }

    /**
     * Limpa o cache de coordenadas.
     */
    public void clearCache() {
        coordinatesCache.clear();
        polygonCache.clear();
        log.info("Cache de geolocalização limpo");
    }

    /**
     * Retorna estatísticas do cache.
     */
    public Map<String, Integer> getCacheStats() {
        return Map.of(
                "coordinatesCache", coordinatesCache.size(),
                "polygonCache", polygonCache.size()
        );
    }
}
