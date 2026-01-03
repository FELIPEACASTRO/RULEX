package com.rulex.service;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.time.LocalDateTime;
import java.util.Base64;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

/**
 * Device Fingerprinting Service for fraud detection.
 * 
 * <p>Device fingerprinting creates a unique identifier for a device based on
 * multiple attributes that are difficult to forge. This helps detect:
 * <ul>
 *   <li>Device hijacking: Same card used from completely different device</li>
 *   <li>Device farming: Single device used with many different cards</li>
 *   <li>Device spoofing: Attempts to mask device identity</li>
 *   <li>New device risk: First-time device for a known card</li>
 * </ul>
 * 
 * <p>Fingerprint components (weighted by reliability):
 * <ol>
 *   <li>Hardware signals: Screen resolution, platform, memory, CPU cores</li>
 *   <li>Software signals: User agent, language, timezone</li>
 *   <li>Browser signals: Plugins, fonts, canvas hash, WebGL hash</li>
 *   <li>Network signals: IP prefix, connection type</li>
 * </ol>
 * 
 * <p>Similarity scoring uses Jaccard similarity and weighted component matching
 * to handle partial fingerprint matches (user updates browser, etc.).
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class DeviceFingerprintService {

    // In-memory fingerprint store (replace with database for production)
    // Key: PAN hash, Value: List of fingerprints for this card
    private final Map<String, List<DeviceFingerprint>> panFingerprints = new ConcurrentHashMap<>();
    
    // Key: Device fingerprint hash, Value: List of PANs using this device
    private final Map<String, List<String>> deviceToPans = new ConcurrentHashMap<>();

    // Statistics
    private final AtomicLong totalFingerprints = new AtomicLong(0);
    private final AtomicLong newDeviceDetections = new AtomicLong(0);
    private final AtomicLong deviceFarmDetections = new AtomicLong(0);
    private final AtomicLong spoofingAttempts = new AtomicLong(0);

    // Configuration
    private static final double SIMILARITY_THRESHOLD = 0.75; // 75% match = same device
    private static final int MAX_CARDS_PER_DEVICE = 5; // Device farming threshold
    private static final Duration FINGERPRINT_TTL = Duration.ofDays(90);
    private static final int MAX_FINGERPRINTS_PER_PAN = 10;

    /**
     * Device fingerprint data structure.
     */
    public record DeviceFingerprint(
            String hash,
            String userAgent,
            String screenResolution,
            String platform,
            String language,
            String timezone,
            Integer colorDepth,
            Integer deviceMemory,
            Integer hardwareConcurrency,
            String canvasHash,
            String webglHash,
            String audioHash,
            List<String> plugins,
            List<String> fonts,
            String ipPrefix,
            String connectionType,
            LocalDateTime firstSeen,
            LocalDateTime lastSeen,
            long transactionCount
    ) {
        public DeviceFingerprint withLastSeen(LocalDateTime time) {
            return new DeviceFingerprint(hash, userAgent, screenResolution, platform, language,
                    timezone, colorDepth, deviceMemory, hardwareConcurrency, canvasHash, webglHash,
                    audioHash, plugins, fonts, ipPrefix, connectionType, firstSeen, time, transactionCount + 1);
        }
    }

    /**
     * Fingerprint analysis result.
     */
    public record FingerprintAnalysis(
            String fingerprintHash,
            RiskLevel riskLevel,
            double similarityScore,
            boolean isNewDevice,
            boolean isDeviceFarm,
            boolean isSpoofingAttempt,
            int cardsOnDevice,
            int devicesForCard,
            String matchedFingerprintHash,
            List<String> riskFactors
    ) {}

    /**
     * Risk levels from fingerprint analysis.
     */
    public enum RiskLevel {
        LOW(0),        // Known device, normal usage
        MEDIUM(50),    // Minor changes detected
        HIGH(80),      // New device or suspicious changes
        CRITICAL(100); // Device farming or spoofing detected

        private final int score;
        RiskLevel(int score) { this.score = score; }
        public int getScore() { return score; }
    }

    /**
     * Analyzes a device fingerprint for fraud indicators.
     * 
     * @param panHash Hashed PAN (card number)
     * @param rawFingerprint Raw fingerprint data from client
     * @return Analysis result with risk factors
     */
    public FingerprintAnalysis analyzeFingerprint(String panHash, Map<String, Object> rawFingerprint) {
        DeviceFingerprint fingerprint = buildFingerprint(rawFingerprint);
        List<String> riskFactors = new java.util.ArrayList<>();

        // Check 1: Is this a new device for this card?
        boolean isNewDevice = isNewDeviceForCard(panHash, fingerprint);
        if (isNewDevice) {
            newDeviceDetections.incrementAndGet();
            riskFactors.add("NEW_DEVICE_FOR_CARD");
        }

        // Check 2: Device farming detection
        int cardsOnDevice = getCardsOnDevice(fingerprint.hash());
        boolean isDeviceFarm = cardsOnDevice >= MAX_CARDS_PER_DEVICE;
        if (isDeviceFarm) {
            deviceFarmDetections.incrementAndGet();
            riskFactors.add("DEVICE_FARMING: " + cardsOnDevice + " cards on same device");
        }

        // Check 3: Find best matching known fingerprint
        DeviceFingerprint bestMatch = findBestMatch(panHash, fingerprint);
        double similarityScore = bestMatch != null ? calculateSimilarity(fingerprint, bestMatch) : 0.0;

        // Check 4: Spoofing detection
        boolean isSpoofingAttempt = detectSpoofing(fingerprint, bestMatch, riskFactors);
        if (isSpoofingAttempt) {
            spoofingAttempts.incrementAndGet();
        }

        // Check 5: Get devices count for this card
        int devicesForCard = getDevicesForCard(panHash);
        if (devicesForCard > 3) {
            riskFactors.add("MULTIPLE_DEVICES: " + devicesForCard + " devices for this card");
        }

        // Calculate risk level
        RiskLevel riskLevel = calculateRiskLevel(isNewDevice, isDeviceFarm, isSpoofingAttempt, 
                similarityScore, devicesForCard);

        // Store fingerprint
        storeFingerprint(panHash, fingerprint);

        return new FingerprintAnalysis(
                fingerprint.hash(),
                riskLevel,
                similarityScore,
                isNewDevice,
                isDeviceFarm,
                isSpoofingAttempt,
                cardsOnDevice,
                devicesForCard,
                bestMatch != null ? bestMatch.hash() : null,
                riskFactors
        );
    }

    /**
     * Gets fingerprint statistics.
     */
    public Map<String, Object> getStatistics() {
        return Map.of(
                "totalFingerprints", totalFingerprints.get(),
                "uniquePans", panFingerprints.size(),
                "uniqueDevices", deviceToPans.size(),
                "newDeviceDetections", newDeviceDetections.get(),
                "deviceFarmDetections", deviceFarmDetections.get(),
                "spoofingAttempts", spoofingAttempts.get()
        );
    }

    /**
     * Checks if a specific device is risky.
     * 
     * @param fingerprintHash The device fingerprint hash
     * @return Risk assessment for the device
     */
    public Map<String, Object> getDeviceRisk(String fingerprintHash) {
        List<String> pans = deviceToPans.get(fingerprintHash);
        int cardCount = pans != null ? pans.size() : 0;
        
        return Map.of(
                "fingerprintHash", fingerprintHash,
                "cardCount", cardCount,
                "isDeviceFarm", cardCount >= MAX_CARDS_PER_DEVICE,
                "riskLevel", cardCount >= MAX_CARDS_PER_DEVICE ? "HIGH" : 
                            cardCount > 3 ? "MEDIUM" : "LOW"
        );
    }

    // ========== Internal Methods ==========

    private DeviceFingerprint buildFingerprint(Map<String, Object> raw) {
        String userAgent = getString(raw, "userAgent", "");
        String screenResolution = getString(raw, "screenResolution", "");
        String platform = getString(raw, "platform", "");
        String language = getString(raw, "language", "");
        String timezone = getString(raw, "timezone", "");
        Integer colorDepth = getInteger(raw, "colorDepth");
        Integer deviceMemory = getInteger(raw, "deviceMemory");
        Integer hardwareConcurrency = getInteger(raw, "hardwareConcurrency");
        String canvasHash = getString(raw, "canvasHash", "");
        String webglHash = getString(raw, "webglHash", "");
        String audioHash = getString(raw, "audioHash", "");
        
        @SuppressWarnings("unchecked")
        List<String> plugins = raw.get("plugins") instanceof List<?> ? 
                (List<String>) raw.get("plugins") : List.of();
        @SuppressWarnings("unchecked")
        List<String> fonts = raw.get("fonts") instanceof List<?> ? 
                (List<String>) raw.get("fonts") : List.of();
        
        String ipPrefix = getString(raw, "ipPrefix", "");
        String connectionType = getString(raw, "connectionType", "");

        // Generate fingerprint hash
        String hash = generateFingerprintHash(userAgent, screenResolution, platform, 
                canvasHash, webglHash, audioHash);

        LocalDateTime now = LocalDateTime.now();
        totalFingerprints.incrementAndGet();

        return new DeviceFingerprint(
                hash, userAgent, screenResolution, platform, language, timezone,
                colorDepth, deviceMemory, hardwareConcurrency, canvasHash, webglHash,
                audioHash, plugins, fonts, ipPrefix, connectionType, now, now, 1
        );
    }

    private String generateFingerprintHash(String... components) {
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            for (String component : components) {
                if (component != null && !component.isEmpty()) {
                    md.update(component.getBytes(StandardCharsets.UTF_8));
                }
            }
            return Base64.getEncoder().encodeToString(md.digest()).substring(0, 32);
        } catch (NoSuchAlgorithmException e) {
            log.error("SHA-256 not available", e);
            return String.valueOf(Objects.hash((Object[]) components));
        }
    }

    private boolean isNewDeviceForCard(String panHash, DeviceFingerprint fingerprint) {
        List<DeviceFingerprint> known = panFingerprints.get(panHash);
        if (known == null || known.isEmpty()) {
            return true;
        }

        // Check if any known fingerprint matches closely
        return known.stream()
                .noneMatch(k -> calculateSimilarity(fingerprint, k) >= SIMILARITY_THRESHOLD);
    }

    private int getCardsOnDevice(String fingerprintHash) {
        List<String> pans = deviceToPans.get(fingerprintHash);
        return pans != null ? pans.size() : 0;
    }

    private int getDevicesForCard(String panHash) {
        List<DeviceFingerprint> fingerprints = panFingerprints.get(panHash);
        return fingerprints != null ? fingerprints.size() : 0;
    }

    private DeviceFingerprint findBestMatch(String panHash, DeviceFingerprint fingerprint) {
        List<DeviceFingerprint> known = panFingerprints.get(panHash);
        if (known == null || known.isEmpty()) {
            return null;
        }

        return known.stream()
                .max(Comparator.comparingDouble(k -> calculateSimilarity(fingerprint, k)))
                .orElse(null);
    }

    /**
     * Calculates similarity between two fingerprints using weighted Jaccard.
     */
    private double calculateSimilarity(DeviceFingerprint fp1, DeviceFingerprint fp2) {
        double totalWeight = 0;
        double matchWeight = 0;

        // Hardware signals (high weight - hard to fake)
        totalWeight += 20;
        if (Objects.equals(fp1.screenResolution(), fp2.screenResolution())) matchWeight += 5;
        if (Objects.equals(fp1.platform(), fp2.platform())) matchWeight += 5;
        if (Objects.equals(fp1.deviceMemory(), fp2.deviceMemory())) matchWeight += 5;
        if (Objects.equals(fp1.hardwareConcurrency(), fp2.hardwareConcurrency())) matchWeight += 5;

        // Canvas/WebGL (very high weight - unique per device)
        totalWeight += 30;
        if (Objects.equals(fp1.canvasHash(), fp2.canvasHash())) matchWeight += 15;
        if (Objects.equals(fp1.webglHash(), fp2.webglHash())) matchWeight += 15;

        // Audio hash
        totalWeight += 10;
        if (Objects.equals(fp1.audioHash(), fp2.audioHash())) matchWeight += 10;

        // User agent (medium weight - changes with updates)
        totalWeight += 10;
        matchWeight += 10 * userAgentSimilarity(fp1.userAgent(), fp2.userAgent());

        // Timezone and language (low weight - can be changed)
        totalWeight += 10;
        if (Objects.equals(fp1.timezone(), fp2.timezone())) matchWeight += 5;
        if (Objects.equals(fp1.language(), fp2.language())) matchWeight += 5;

        // Plugins (medium weight)
        totalWeight += 10;
        matchWeight += 10 * jaccardSimilarity(fp1.plugins(), fp2.plugins());

        // Fonts (medium weight)
        totalWeight += 10;
        matchWeight += 10 * jaccardSimilarity(fp1.fonts(), fp2.fonts());

        return matchWeight / totalWeight;
    }

    private double userAgentSimilarity(String ua1, String ua2) {
        if (ua1 == null || ua2 == null) return 0;
        if (ua1.equals(ua2)) return 1.0;
        
        // Extract browser family and major version
        String browser1 = extractBrowserFamily(ua1);
        String browser2 = extractBrowserFamily(ua2);
        
        return browser1.equals(browser2) ? 0.8 : 0.0;
    }

    private String extractBrowserFamily(String userAgent) {
        if (userAgent.contains("Chrome")) return "Chrome";
        if (userAgent.contains("Firefox")) return "Firefox";
        if (userAgent.contains("Safari")) return "Safari";
        if (userAgent.contains("Edge")) return "Edge";
        return "Other";
    }

    private double jaccardSimilarity(List<String> list1, List<String> list2) {
        if (list1 == null || list2 == null || list1.isEmpty() || list2.isEmpty()) {
            return 0;
        }
        
        long intersection = list1.stream().filter(list2::contains).count();
        long union = list1.size() + list2.size() - intersection;
        
        return union > 0 ? (double) intersection / union : 0;
    }

    private boolean detectSpoofing(DeviceFingerprint current, DeviceFingerprint known, 
                                    List<String> riskFactors) {
        if (known == null) return false;
        
        boolean spoofingDetected = false;

        // Check 1: Canvas/WebGL changed but hardware same (browser automation)
        if (!Objects.equals(current.canvasHash(), known.canvasHash()) &&
            Objects.equals(current.screenResolution(), known.screenResolution()) &&
            Objects.equals(current.hardwareConcurrency(), known.hardwareConcurrency())) {
            riskFactors.add("POSSIBLE_CANVAS_SPOOFING");
            spoofingDetected = true;
        }

        // Check 2: Hardware specs changed dramatically (virtual machine)
        if (current.deviceMemory() != null && known.deviceMemory() != null) {
            if (Math.abs(current.deviceMemory() - known.deviceMemory()) > 8) {
                riskFactors.add("MEMORY_INCONSISTENCY: " + known.deviceMemory() + " -> " + current.deviceMemory());
                spoofingDetected = true;
            }
        }

        // Check 3: Platform changed but other signals same (user agent spoofing)
        if (!Objects.equals(current.platform(), known.platform()) &&
            Objects.equals(current.screenResolution(), known.screenResolution()) &&
            Objects.equals(current.canvasHash(), known.canvasHash())) {
            riskFactors.add("PLATFORM_SPOOFING: " + known.platform() + " -> " + current.platform());
            spoofingDetected = true;
        }

        // Check 4: Empty/generic fingerprint (privacy tools blocking)
        if (isGenericFingerprint(current)) {
            riskFactors.add("FINGERPRINT_BLOCKED");
            spoofingDetected = true;
        }

        return spoofingDetected;
    }

    private boolean isGenericFingerprint(DeviceFingerprint fp) {
        int emptyCount = 0;
        if (fp.canvasHash() == null || fp.canvasHash().isEmpty()) emptyCount++;
        if (fp.webglHash() == null || fp.webglHash().isEmpty()) emptyCount++;
        if (fp.audioHash() == null || fp.audioHash().isEmpty()) emptyCount++;
        if (fp.plugins() == null || fp.plugins().isEmpty()) emptyCount++;
        if (fp.fonts() == null || fp.fonts().isEmpty()) emptyCount++;
        
        return emptyCount >= 3;
    }

    private RiskLevel calculateRiskLevel(boolean isNewDevice, boolean isDeviceFarm,
                                          boolean isSpoofingAttempt, double similarityScore, int devicesForCard) {
        if (isDeviceFarm || isSpoofingAttempt) {
            return RiskLevel.CRITICAL;
        }
        
        if (isNewDevice && devicesForCard > 3) {
            return RiskLevel.HIGH;
        }
        
        if (isNewDevice) {
            return RiskLevel.HIGH;
        }
        
        if (similarityScore < 0.5) {
            return RiskLevel.HIGH;
        }
        
        if (similarityScore < SIMILARITY_THRESHOLD) {
            return RiskLevel.MEDIUM;
        }
        
        return RiskLevel.LOW;
    }

    private void storeFingerprint(String panHash, DeviceFingerprint fingerprint) {
        // Store fingerprint for PAN
        panFingerprints.compute(panHash, (k, existing) -> {
            if (existing == null) {
                return new java.util.ArrayList<>(List.of(fingerprint));
            }
            
            // Check if we already have a similar fingerprint
            for (int i = 0; i < existing.size(); i++) {
                DeviceFingerprint known = existing.get(i);
                if (calculateSimilarity(fingerprint, known) >= SIMILARITY_THRESHOLD) {
                    // Update existing
                    existing.set(i, known.withLastSeen(LocalDateTime.now()));
                    return existing;
                }
            }
            
            // Add new fingerprint (limit to MAX)
            if (existing.size() >= MAX_FINGERPRINTS_PER_PAN) {
                // Remove oldest
                existing.sort(Comparator.comparing(DeviceFingerprint::lastSeen));
                existing.remove(0);
            }
            existing.add(fingerprint);
            return existing;
        });

        // Store PAN for device
        deviceToPans.compute(fingerprint.hash(), (k, existing) -> {
            if (existing == null) {
                return new java.util.ArrayList<>(List.of(panHash));
            }
            if (!existing.contains(panHash)) {
                existing.add(panHash);
            }
            return existing;
        });
    }

    @Scheduled(fixedRate = 3600_000) // Every hour
    public void cleanupExpiredFingerprints() {
        LocalDateTime cutoff = LocalDateTime.now().minus(FINGERPRINT_TTL);
        
        panFingerprints.values().forEach(list -> 
                list.removeIf(fp -> fp.lastSeen().isBefore(cutoff)));
        
        // Remove empty entries
        panFingerprints.entrySet().removeIf(e -> e.getValue().isEmpty());
        
        log.info("Fingerprint cleanup complete. Active PANs: {}, Active devices: {}",
                panFingerprints.size(), deviceToPans.size());
    }

    private String getString(Map<String, Object> map, String key, String defaultValue) {
        Object value = map.get(key);
        return value instanceof String ? (String) value : defaultValue;
    }

    private Integer getInteger(Map<String, Object> map, String key) {
        Object value = map.get(key);
        if (value instanceof Integer) return (Integer) value;
        if (value instanceof Number) return ((Number) value).intValue();
        return null;
    }
}
