package com.rulex.service;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

/**
 * Impossible Travel Detection Service.
 * 
 * <p>Detects physically impossible travel between consecutive transactions based on:
 * <ul>
 *   <li>Distance between transaction locations</li>
 *   <li>Time elapsed between transactions</li>
 *   <li>Calculated travel speed</li>
 * </ul>
 * 
 * <p>Example: If Card A transacts in São Paulo at 10:00 and in Tokyo at 10:30,
 * the required speed would be ~36,000 km/h - clearly impossible.
 * 
 * <p>Thresholds:
 * <ul>
 *   <li>Commercial flight: ~900 km/h max</li>
 *   <li>Private jet: ~1,100 km/h max</li>
 *   <li>Conservative threshold: 500 km/h (allows for some GPS variance)</li>
 *   <li>Aggressive threshold: 200 km/h (land travel only)</li>
 * </ul>
 * 
 * <p>False positive mitigation:
 * <ul>
 *   <li>Card-not-present transactions may show merchant location, not cardholder</li>
 *   <li>VPN/proxy usage can mask true location</li>
 *   <li>GPS variance can add ~50km error</li>
 *   <li>Time zone differences in transaction timestamps</li>
 * </ul>
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class ImpossibleTravelService {

    // Last known location per PAN (in-memory for performance)
    private final Map<String, LocationHistory> locationHistories = new ConcurrentHashMap<>();

    // Statistics
    private final AtomicLong totalChecks = new AtomicLong(0);
    private final AtomicLong impossibleTravelDetected = new AtomicLong(0);
    private final AtomicLong suspiciousTravelDetected = new AtomicLong(0);

    // Configuration thresholds
    private static final double IMPOSSIBLE_SPEED_KMH = 1200.0;  // Faster than any commercial travel
    private static final double SUSPICIOUS_SPEED_KMH = 500.0;   // Suspicious but possible
    private static final double HIGH_SPEED_KMH = 200.0;         // Fast land travel
    private static final Duration MIN_TIME_BETWEEN = Duration.ofMinutes(1);
    private static final Duration HISTORY_TTL = Duration.ofHours(72);
    private static final int MAX_HISTORY_PER_PAN = 20;

    // Earth radius in kilometers
    private static final double EARTH_RADIUS_KM = 6371.0;

    /**
     * Location data for a transaction.
     */
    public record Location(
            double latitude,
            double longitude,
            String city,
            String country,
            LocalDateTime timestamp,
            String transactionId,
            boolean isCardPresent
    ) {}

    /**
     * Travel analysis result.
     */
    public record TravelAnalysis(
            TravelRisk riskLevel,
            double distanceKm,
            double elapsedMinutes,
            double speedKmh,
            Location previousLocation,
            Location currentLocation,
            List<String> riskFactors,
            int riskScore
    ) {
        public static TravelAnalysis noHistory() {
            return new TravelAnalysis(
                    TravelRisk.UNKNOWN, 0, 0, 0, null, null,
                    List.of("NO_PREVIOUS_LOCATION"), 0
            );
        }
    }

    /**
     * Travel risk levels.
     */
    public enum TravelRisk {
        /** First transaction or no history */
        UNKNOWN(0),
        /** Normal travel speed */
        LOW(0),
        /** Fast but possible (airport to airport) */
        MEDIUM(30),
        /** Very fast, suspicious */
        HIGH(60),
        /** Physically impossible */
        IMPOSSIBLE(100);

        private final int score;
        TravelRisk(int score) { this.score = score; }
        public int getScore() { return score; }
    }

    /**
     * Location history for a PAN.
     */
    private static class LocationHistory {
        private final List<Location> locations = new java.util.concurrent.CopyOnWriteArrayList<>();
        
        void addLocation(Location loc) {
            locations.add(loc);
            // Trim to max size, keeping most recent
            while (locations.size() > MAX_HISTORY_PER_PAN) {
                locations.remove(0);
            }
        }
        
        Optional<Location> getLastLocation() {
            if (locations.isEmpty()) return Optional.empty();
            return Optional.of(locations.get(locations.size() - 1));
        }
        
        List<Location> getRecentLocations(int count) {
            int start = Math.max(0, locations.size() - count);
            return locations.subList(start, locations.size());
        }
        
        void cleanup(LocalDateTime cutoff) {
            locations.removeIf(loc -> loc.timestamp().isBefore(cutoff));
        }
    }

    /**
     * Analyzes travel between the last known location and current transaction.
     * 
     * @param panHash Hashed PAN
     * @param currentLat Current transaction latitude
     * @param currentLon Current transaction longitude
     * @param city Current city
     * @param country Current country
     * @param transactionTime Transaction timestamp
     * @param transactionId Transaction ID
     * @param isCardPresent Whether card was physically present
     * @return Travel analysis with risk assessment
     */
    public TravelAnalysis analyzeTravel(String panHash, double currentLat, double currentLon,
                                         String city, String country, LocalDateTime transactionTime,
                                         String transactionId, boolean isCardPresent) {
        totalChecks.incrementAndGet();

        Location currentLocation = new Location(
                currentLat, currentLon, city, country, transactionTime, transactionId, isCardPresent
        );

        // Get or create location history
        LocationHistory history = locationHistories.computeIfAbsent(panHash, k -> new LocationHistory());
        Optional<Location> lastLocationOpt = history.getLastLocation();

        // Store current location
        history.addLocation(currentLocation);

        // If no previous location, return unknown
        if (lastLocationOpt.isEmpty()) {
            return TravelAnalysis.noHistory();
        }

        Location previousLocation = lastLocationOpt.get();
        
        // Calculate distance and time
        double distanceKm = calculateDistance(
                previousLocation.latitude(), previousLocation.longitude(),
                currentLat, currentLon
        );

        Duration elapsed = Duration.between(previousLocation.timestamp(), transactionTime);
        double elapsedMinutes = elapsed.toMinutes() + (elapsed.toSecondsPart() / 60.0);

        // Avoid division by zero
        if (elapsedMinutes < 1) {
            elapsedMinutes = 1;
        }

        double speedKmh = (distanceKm / elapsedMinutes) * 60;

        // Analyze risk
        List<String> riskFactors = new java.util.ArrayList<>();
        TravelRisk riskLevel = assessRisk(speedKmh, distanceKm, elapsed, 
                previousLocation, currentLocation, riskFactors);

        // Update statistics
        if (riskLevel == TravelRisk.IMPOSSIBLE) {
            impossibleTravelDetected.incrementAndGet();
        } else if (riskLevel == TravelRisk.HIGH) {
            suspiciousTravelDetected.incrementAndGet();
        }

        log.debug("Travel analysis for {}: {} km in {} min = {} km/h, risk={}",
                maskPan(panHash), String.format("%.1f", distanceKm), 
                String.format("%.1f", elapsedMinutes),
                String.format("%.1f", speedKmh), riskLevel);

        return new TravelAnalysis(
                riskLevel,
                distanceKm,
                elapsedMinutes,
                speedKmh,
                previousLocation,
                currentLocation,
                riskFactors,
                riskLevel.getScore()
        );
    }

    /**
     * Gets travel patterns for a PAN (for investigation).
     */
    public List<Location> getTravelHistory(String panHash, int limit) {
        LocationHistory history = locationHistories.get(panHash);
        if (history == null) return List.of();
        return history.getRecentLocations(limit);
    }

    /**
     * Gets service statistics.
     */
    public Map<String, Object> getStatistics() {
        return Map.of(
                "totalChecks", totalChecks.get(),
                "impossibleTravelDetected", impossibleTravelDetected.get(),
                "suspiciousTravelDetected", suspiciousTravelDetected.get(),
                "activePans", locationHistories.size()
        );
    }

    /**
     * Checks if a specific travel pattern is possible.
     * Static utility method for one-off checks.
     */
    public static boolean isPossibleTravel(double fromLat, double fromLon,
                                            double toLat, double toLon,
                                            Duration elapsed) {
        double distanceKm = calculateDistance(fromLat, fromLon, toLat, toLon);
        double elapsedMinutes = elapsed.toMinutes();
        if (elapsedMinutes < 1) elapsedMinutes = 1;
        double speedKmh = (distanceKm / elapsedMinutes) * 60;
        return speedKmh <= IMPOSSIBLE_SPEED_KMH;
    }

    // ========== Internal Methods ==========

    private TravelRisk assessRisk(double speedKmh, double distanceKm, Duration elapsed,
                                   Location prev, Location current, List<String> riskFactors) {
        // Check minimum time threshold
        if (elapsed.compareTo(MIN_TIME_BETWEEN) < 0 && distanceKm > 10) {
            riskFactors.add("RAPID_SEQUENCE: " + elapsed.toSeconds() + "s between transactions");
        }

        // Cross-country in short time
        if (!prev.country().equals(current.country()) && elapsed.toHours() < 2) {
            riskFactors.add("CROSS_COUNTRY: " + prev.country() + " -> " + current.country() + 
                           " in " + elapsed.toMinutes() + " min");
        }

        // Cross-continent detection
        if (isCrossContinent(prev, current) && elapsed.toHours() < 6) {
            riskFactors.add("CROSS_CONTINENT: Requires minimum 6h flight");
        }

        // Card present at both locations makes impossible travel more significant
        if (prev.isCardPresent() && current.isCardPresent()) {
            riskFactors.add("BOTH_CARD_PRESENT: Physical card at both locations");
        }

        // Assess speed-based risk
        if (speedKmh > IMPOSSIBLE_SPEED_KMH) {
            riskFactors.add("IMPOSSIBLE_SPEED: " + String.format("%.0f", speedKmh) + " km/h");
            return TravelRisk.IMPOSSIBLE;
        }

        if (speedKmh > SUSPICIOUS_SPEED_KMH) {
            riskFactors.add("SUSPICIOUS_SPEED: " + String.format("%.0f", speedKmh) + " km/h");
            return TravelRisk.HIGH;
        }

        if (speedKmh > HIGH_SPEED_KMH) {
            riskFactors.add("FAST_TRAVEL: " + String.format("%.0f", speedKmh) + " km/h");
            return TravelRisk.MEDIUM;
        }

        return TravelRisk.LOW;
    }

    private boolean isCrossContinent(Location loc1, Location loc2) {
        String continent1 = getContinent(loc1.latitude(), loc1.longitude());
        String continent2 = getContinent(loc2.latitude(), loc2.longitude());
        return !continent1.equals(continent2);
    }

    private String getContinent(double lat, double lon) {
        // Simplified continent detection based on coordinates
        if (lat > 35 && lon > -25 && lon < 60) return "EUROPE";
        if (lat > -60 && lat < 15 && lon > -82 && lon < -34) return "SOUTH_AMERICA";
        if (lat > 15 && lon > -170 && lon < -50) return "NORTH_AMERICA";
        if (lat > -40 && lat < 55 && lon > 25 && lon < 180) return "ASIA";
        if (lat > -40 && lat < 37 && lon > -20 && lon < 55) return "AFRICA";
        if (lat < -10 && lon > 110 && lon < 180) return "OCEANIA";
        return "UNKNOWN";
    }

    /**
     * Calculates distance between two points using Haversine formula.
     */
    public static double calculateDistance(double lat1, double lon1, double lat2, double lon2) {
        double dLat = Math.toRadians(lat2 - lat1);
        double dLon = Math.toRadians(lon2 - lon1);
        
        double a = Math.sin(dLat / 2) * Math.sin(dLat / 2)
                 + Math.cos(Math.toRadians(lat1)) * Math.cos(Math.toRadians(lat2))
                 * Math.sin(dLon / 2) * Math.sin(dLon / 2);
        
        double c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
        
        return EARTH_RADIUS_KM * c;
    }

    private String maskPan(String panHash) {
        if (panHash == null || panHash.length() < 8) return "****";
        return panHash.substring(0, 4) + "****" + panHash.substring(panHash.length() - 4);
    }

    @Scheduled(fixedRate = 3600_000) // Every hour
    public void cleanupOldLocations() {
        LocalDateTime cutoff = LocalDateTime.now().minus(HISTORY_TTL);
        
        locationHistories.values().forEach(h -> h.cleanup(cutoff));
        locationHistories.entrySet().removeIf(e -> e.getValue().locations.isEmpty());
        
        log.info("Location history cleanup complete. Active PANs: {}", locationHistories.size());
    }
}
