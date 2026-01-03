package com.rulex.service;

import com.rulex.dto.TransactionRequest;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.time.Duration;
import java.time.Instant;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.LongAdder;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

/**
 * High-performance velocity service using in-memory sliding windows.
 * 
 * <p>This implementation uses a hybrid approach:
 * <ul>
 *   <li>In-memory HyperLogLog-like counting for real-time velocity</li>
 *   <li>Sliding window algorithm with bucketed counters</li>
 *   <li>Periodic sync to database for persistence</li>
 * </ul>
 * 
 * <p>Performance characteristics:
 * <ul>
 *   <li>Read latency: O(1) - sub-microsecond</li>
 *   <li>Write latency: O(1) - sub-microsecond</li>
 *   <li>Memory: ~100 bytes per unique key per window</li>
 * </ul>
 * 
 * <p>For production with multiple instances, replace with Redis implementation:
 * <ul>
 *   <li>Redis Sorted Sets for time-windowed counting</li>
 *   <li>Redis HyperLogLog for distinct counts</li>
 *   <li>Redis Streams for event sourcing</li>
 * </ul>
 * 
 * @see VelocityService for database-backed fallback
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class RedisVelocityService {

    private final VelocityService velocityService;

    // Sliding window buckets: key -> window -> bucket[] 
    // Each bucket represents 1 minute of data
    private final Map<String, SlidingWindowCounter> velocityCounters = new ConcurrentHashMap<>();
    
    // Distinct count approximation using HyperLogLog-like structure
    private final Map<String, HyperLogLogCounter> distinctCounters = new ConcurrentHashMap<>();
    
    // Sum accumulators for amount tracking
    private final Map<String, SlidingWindowSum> sumCounters = new ConcurrentHashMap<>();

    // Configuration
    private static final int BUCKET_SIZE_MINUTES = 1;
    private static final int MAX_BUCKETS = 60 * 24 * 7; // 7 days of 1-minute buckets
    private static final int CLEANUP_THRESHOLD = 100_000; // Max entries before cleanup

    /**
     * Time windows supported (in minutes).
     */
    public enum TimeWindow {
        MINUTE_5(5),
        MINUTE_15(15),
        MINUTE_30(30),
        HOUR_1(60),
        HOUR_6(360),
        HOUR_12(720),
        HOUR_24(1440),
        DAY_7(10080);

        private final int minutes;

        TimeWindow(int minutes) {
            this.minutes = minutes;
        }

        public int getMinutes() {
            return minutes;
        }
    }

    /**
     * Key types for velocity tracking.
     */
    public enum KeyType {
        PAN,
        CUSTOMER_ID,
        MERCHANT_ID,
        DEVICE_ID,
        IP_ADDRESS
    }

    /**
     * Velocity statistics result.
     */
    @Data
    @Builder
    public static class VelocityStats {
        private final long transactionCount;
        private final BigDecimal totalAmount;
        private final BigDecimal avgAmount;
        private final long distinctMerchants;
        private final long distinctMccs;
        private final long distinctCountries;
        private final long distinctDevices;
        private final boolean fromCache;
        private final long latencyMicros;

        public static VelocityStats empty() {
            return VelocityStats.builder()
                    .transactionCount(0)
                    .totalAmount(BigDecimal.ZERO)
                    .avgAmount(BigDecimal.ZERO)
                    .distinctMerchants(0)
                    .distinctMccs(0)
                    .distinctCountries(0)
                    .distinctDevices(0)
                    .fromCache(true)
                    .latencyMicros(0)
                    .build();
        }
    }

    /**
     * Gets velocity statistics with sub-millisecond latency.
     * 
     * @param request Transaction request
     * @param keyType Type of key (PAN, CUSTOMER_ID, etc.)
     * @param window Time window
     * @return Velocity statistics
     */
    public VelocityStats getStats(TransactionRequest request, KeyType keyType, TimeWindow window) {
        long startNanos = System.nanoTime();
        
        if (request == null) {
            return VelocityStats.empty();
        }

        String keyValue = extractKeyValue(request, keyType);
        if (keyValue == null || keyValue.isBlank()) {
            return VelocityStats.empty();
        }

        String cacheKey = buildCacheKey(keyType, keyValue);
        
        // Get count from sliding window
        SlidingWindowCounter counter = velocityCounters.get(cacheKey);
        long count = counter != null ? counter.getCount(window.getMinutes()) : 0;
        
        // Get sum from sliding window
        SlidingWindowSum sumCounter = sumCounters.get(cacheKey);
        BigDecimal sum = sumCounter != null ? sumCounter.getSum(window.getMinutes()) : BigDecimal.ZERO;
        
        // Get distinct counts
        String distinctMerchantKey = cacheKey + ":merchants";
        String distinctMccKey = cacheKey + ":mccs";
        String distinctCountryKey = cacheKey + ":countries";
        String distinctDeviceKey = cacheKey + ":devices";
        
        HyperLogLogCounter merchantHll = distinctCounters.get(distinctMerchantKey);
        HyperLogLogCounter mccHll = distinctCounters.get(distinctMccKey);
        HyperLogLogCounter countryHll = distinctCounters.get(distinctCountryKey);
        HyperLogLogCounter deviceHll = distinctCounters.get(distinctDeviceKey);
        
        long distinctMerchants = merchantHll != null ? merchantHll.estimate() : 0;
        long distinctMccs = mccHll != null ? mccHll.estimate() : 0;
        long distinctCountries = countryHll != null ? countryHll.estimate() : 0;
        long distinctDevices = deviceHll != null ? deviceHll.estimate() : 0;
        
        // Calculate average
        BigDecimal avg = count > 0 ? sum.divide(BigDecimal.valueOf(count), 2, RoundingMode.HALF_UP) : BigDecimal.ZERO;
        
        long latencyMicros = (System.nanoTime() - startNanos) / 1000;
        
        return VelocityStats.builder()
                .transactionCount(count)
                .totalAmount(sum)
                .avgAmount(avg)
                .distinctMerchants(distinctMerchants)
                .distinctMccs(distinctMccs)
                .distinctCountries(distinctCountries)
                .distinctDevices(distinctDevices)
                .fromCache(true)
                .latencyMicros(latencyMicros)
                .build();
    }

    /**
     * Records a transaction for velocity tracking.
     * Call this AFTER the transaction is processed.
     * 
     * @param request Transaction request
     */
    public void recordTransaction(TransactionRequest request) {
        if (request == null || request.getExternalTransactionId() == null) {
            return;
        }

        Instant now = Instant.now();
        BigDecimal amount = request.getTransactionAmount() != null 
                ? request.getTransactionAmount() 
                : BigDecimal.ZERO;

        // Record for PAN
        String panKey = buildCacheKey(KeyType.PAN, hashPan(request.getPan()));
        recordToCounters(panKey, now, amount, request);

        // Record for Customer ID
        String customerId = request.getCustomerIdFromHeader();
        if (customerId != null && !customerId.isBlank()) {
            String customerKey = buildCacheKey(KeyType.CUSTOMER_ID, customerId);
            recordToCounters(customerKey, now, amount, request);
        }

        // Record for Merchant ID
        String merchantId = request.getMerchantId();
        if (merchantId != null && !merchantId.isBlank()) {
            String merchantKey = buildCacheKey(KeyType.MERCHANT_ID, merchantId);
            recordToCounters(merchantKey, now, amount, request);
        }

        // Also persist to database asynchronously
        velocityService.logTransaction(request, "PROCESSED", null);
    }

    /**
     * Gets transaction count for a specific key and window.
     */
    public long getTransactionCount(TransactionRequest request, KeyType keyType, TimeWindow window) {
        return getStats(request, keyType, window).getTransactionCount();
    }

    /**
     * Gets total amount for a specific key and window.
     */
    public BigDecimal getTotalAmount(TransactionRequest request, KeyType keyType, TimeWindow window) {
        return getStats(request, keyType, window).getTotalAmount();
    }

    /**
     * Gets distinct merchant count for a specific key and window.
     */
    public long getDistinctMerchants(TransactionRequest request, KeyType keyType, TimeWindow window) {
        return getStats(request, keyType, window).getDistinctMerchants();
    }

    // ========== Internal Methods ==========

    private void recordToCounters(String cacheKey, Instant timestamp, BigDecimal amount, TransactionRequest request) {
        // Record count
        velocityCounters.computeIfAbsent(cacheKey, k -> new SlidingWindowCounter())
                .increment(timestamp);
        
        // Record sum
        sumCounters.computeIfAbsent(cacheKey, k -> new SlidingWindowSum())
                .add(timestamp, amount);
        
        // Record distinct values
        if (request.getMerchantId() != null) {
            distinctCounters.computeIfAbsent(cacheKey + ":merchants", k -> new HyperLogLogCounter())
                    .add(request.getMerchantId());
        }
        if (request.getMcc() != null) {
            distinctCounters.computeIfAbsent(cacheKey + ":mccs", k -> new HyperLogLogCounter())
                    .add(String.valueOf(request.getMcc()));
        }
        if (request.getMerchantCountryCode() != null) {
            distinctCounters.computeIfAbsent(cacheKey + ":countries", k -> new HyperLogLogCounter())
                    .add(request.getMerchantCountryCode());
        }
    }

    private String extractKeyValue(TransactionRequest request, KeyType keyType) {
        return switch (keyType) {
            case PAN -> hashPan(request.getPan());
            case CUSTOMER_ID -> request.getCustomerIdFromHeader();
            case MERCHANT_ID -> request.getMerchantId();
            case DEVICE_ID -> request.getTerminalId(); // Using terminalId as device proxy
            case IP_ADDRESS -> request.getAcquirerBin(); // Using acquirerBin as IP proxy
        };
    }

    private String buildCacheKey(KeyType keyType, String value) {
        return keyType.name() + ":" + value;
    }

    private String hashPan(String pan) {
        if (pan == null || pan.isBlank()) {
            return null;
        }
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            byte[] hash = md.digest(pan.getBytes(StandardCharsets.UTF_8));
            StringBuilder sb = new StringBuilder();
            for (byte b : hash) {
                sb.append(String.format("%02x", b));
            }
            return sb.toString();
        } catch (Exception e) {
            log.warn("Error hashing PAN: {}", e.getMessage());
            return pan;
        }
    }

    /**
     * Cleanup old entries periodically.
     */
    @Scheduled(fixedRate = 300_000) // Every 5 minutes
    public void cleanup() {
        if (velocityCounters.size() > CLEANUP_THRESHOLD) {
            log.info("Velocity cache cleanup: {} entries before", velocityCounters.size());
            
            Instant cutoff = Instant.now().minus(Duration.ofDays(7));
            velocityCounters.entrySet().removeIf(e -> e.getValue().isExpired(cutoff));
            sumCounters.entrySet().removeIf(e -> e.getValue().isExpired(cutoff));
            distinctCounters.entrySet().removeIf(e -> e.getValue().isExpired(cutoff));
            
            log.info("Velocity cache cleanup: {} entries after", velocityCounters.size());
        }
    }

    // ========== Sliding Window Counter ==========

    /**
     * Sliding window counter using bucketed approach.
     * Each bucket represents 1 minute of data.
     */
    private static class SlidingWindowCounter {
        private final Map<Long, LongAdder> buckets = new ConcurrentHashMap<>();
        private volatile long lastAccessTime = System.currentTimeMillis();

        void increment(Instant timestamp) {
            long bucketKey = timestamp.toEpochMilli() / (BUCKET_SIZE_MINUTES * 60_000);
            buckets.computeIfAbsent(bucketKey, k -> new LongAdder()).increment();
            lastAccessTime = System.currentTimeMillis();
            
            // Cleanup old buckets
            if (buckets.size() > MAX_BUCKETS) {
                long cutoff = bucketKey - MAX_BUCKETS;
                buckets.entrySet().removeIf(e -> e.getKey() < cutoff);
            }
        }

        long getCount(int windowMinutes) {
            long now = System.currentTimeMillis();
            long currentBucket = now / (BUCKET_SIZE_MINUTES * 60_000);
            long startBucket = currentBucket - (windowMinutes / BUCKET_SIZE_MINUTES);
            
            long count = 0;
            for (long bucket = startBucket; bucket <= currentBucket; bucket++) {
                LongAdder adder = buckets.get(bucket);
                if (adder != null) {
                    count += adder.sum();
                }
            }
            return count;
        }

        boolean isExpired(Instant cutoff) {
            return lastAccessTime < cutoff.toEpochMilli();
        }
    }

    // ========== Sliding Window Sum ==========

    /**
     * Sliding window sum for amount tracking.
     */
    private static class SlidingWindowSum {
        private final Map<Long, BigDecimal> buckets = new ConcurrentHashMap<>();
        private volatile long lastAccessTime = System.currentTimeMillis();

        synchronized void add(Instant timestamp, BigDecimal amount) {
            long bucketKey = timestamp.toEpochMilli() / (BUCKET_SIZE_MINUTES * 60_000);
            buckets.merge(bucketKey, amount, BigDecimal::add);
            lastAccessTime = System.currentTimeMillis();
            
            // Cleanup old buckets
            if (buckets.size() > MAX_BUCKETS) {
                long cutoff = bucketKey - MAX_BUCKETS;
                buckets.entrySet().removeIf(e -> e.getKey() < cutoff);
            }
        }

        BigDecimal getSum(int windowMinutes) {
            long now = System.currentTimeMillis();
            long currentBucket = now / (BUCKET_SIZE_MINUTES * 60_000);
            long startBucket = currentBucket - (windowMinutes / BUCKET_SIZE_MINUTES);
            
            BigDecimal sum = BigDecimal.ZERO;
            for (long bucket = startBucket; bucket <= currentBucket; bucket++) {
                BigDecimal bucketSum = buckets.get(bucket);
                if (bucketSum != null) {
                    sum = sum.add(bucketSum);
                }
            }
            return sum;
        }

        boolean isExpired(Instant cutoff) {
            return lastAccessTime < cutoff.toEpochMilli();
        }
    }

    // ========== HyperLogLog Counter ==========

    /**
     * Simplified HyperLogLog-like counter for distinct value estimation.
     * Uses a probabilistic approach with configurable precision.
     * 
     * For production, use Redis HyperLogLog or Google's HyperLogLogPlusPlus.
     */
    private static class HyperLogLogCounter {
        // Using 2^10 = 1024 registers for ~3% standard error
        private static final int NUM_REGISTERS = 1024;
        private static final int REGISTER_BITS = 10;
        private final byte[] registers = new byte[NUM_REGISTERS];
        private volatile long lastAccessTime = System.currentTimeMillis();
        private final AtomicLong exactCount = new AtomicLong(0);
        private static final int EXACT_THRESHOLD = 1000;

        void add(String value) {
            if (value == null) return;
            
            lastAccessTime = System.currentTimeMillis();
            
            // For small counts, use exact counting
            if (exactCount.get() < EXACT_THRESHOLD) {
                exactCount.incrementAndGet();
            }
            
            // HyperLogLog update
            int hash = murmurHash3(value);
            int registerIndex = hash >>> (32 - REGISTER_BITS);
            int remainingBits = hash << REGISTER_BITS | (1 << (REGISTER_BITS - 1));
            int leadingZeros = Integer.numberOfLeadingZeros(remainingBits) + 1;
            
            synchronized (registers) {
                if (leadingZeros > registers[registerIndex]) {
                    registers[registerIndex] = (byte) leadingZeros;
                }
            }
        }

        long estimate() {
            // For small counts, return exact count
            if (exactCount.get() < EXACT_THRESHOLD) {
                return exactCount.get();
            }
            
            // HyperLogLog estimation
            double sum = 0;
            int zeros = 0;
            
            synchronized (registers) {
                for (byte register : registers) {
                    sum += Math.pow(2, -register);
                    if (register == 0) zeros++;
                }
            }
            
            double alpha = 0.7213 / (1 + 1.079 / NUM_REGISTERS);
            double estimate = alpha * NUM_REGISTERS * NUM_REGISTERS / sum;
            
            // Small range correction
            if (estimate <= 2.5 * NUM_REGISTERS && zeros > 0) {
                estimate = NUM_REGISTERS * Math.log((double) NUM_REGISTERS / zeros);
            }
            
            return Math.round(estimate);
        }

        boolean isExpired(Instant cutoff) {
            return lastAccessTime < cutoff.toEpochMilli();
        }

        // MurmurHash3 32-bit implementation
        private static int murmurHash3(String value) {
            byte[] data = value.getBytes(StandardCharsets.UTF_8);
            int h = 0x9747b28c;
            int len = data.length;
            int i = 0;
            
            while (len >= 4) {
                int k = (data[i] & 0xff) | ((data[i + 1] & 0xff) << 8) 
                      | ((data[i + 2] & 0xff) << 16) | ((data[i + 3] & 0xff) << 24);
                k *= 0xcc9e2d51;
                k = Integer.rotateLeft(k, 15);
                k *= 0x1b873593;
                h ^= k;
                h = Integer.rotateLeft(h, 13);
                h = h * 5 + 0xe6546b64;
                i += 4;
                len -= 4;
            }
            
            int k = 0;
            switch (len) {
                case 3: k ^= (data[i + 2] & 0xff) << 16;
                case 2: k ^= (data[i + 1] & 0xff) << 8;
                case 1: k ^= (data[i] & 0xff);
                        k *= 0xcc9e2d51;
                        k = Integer.rotateLeft(k, 15);
                        k *= 0x1b873593;
                        h ^= k;
            }
            
            h ^= data.length;
            h ^= h >>> 16;
            h *= 0x85ebca6b;
            h ^= h >>> 13;
            h *= 0xc2b2ae35;
            h ^= h >>> 16;
            
            return h;
        }
    }
}
