package com.rulex.service;

import com.rulex.entity.RuleList.EntityType;
import com.rulex.entity.RuleListEntry;
import com.rulex.repository.RuleListEntryRepository;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.BitSet;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * High-performance Bloom Filter service for blocklist/allowlist lookups.
 *
 * <p>A Bloom filter is a probabilistic data structure that provides:
 *
 * <ul>
 *   <li>O(1) lookup time regardless of list size
 *   <li>Space-efficient: ~10 bits per element for 1% false positive rate
 *   <li>No false negatives: if filter says "not in list", it's guaranteed
 *   <li>Possible false positives: if filter says "might be in list", verify with DB
 * </ul>
 *
 * <p>Workflow:
 *
 * <ol>
 *   <li>Check Bloom filter first (sub-microsecond)
 *   <li>If DEFINITELY_NOT_IN_LIST: return immediately (most cases)
 *   <li>If POSSIBLY_IN_LIST: verify with database query
 * </ol>
 *
 * <p>This reduces database load by 90-99% for blocklist checks.
 *
 * <p>For production with multiple instances, use Redis Bloom module or synchronized filters with
 * periodic rebuild.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class BloomFilterService {

  private final RuleListEntryRepository ruleListEntryRepository;

  // Bloom filters per entity type for blacklists
  private final Map<EntityType, BloomFilter> blacklistFilters = new ConcurrentHashMap<>();

  // Bloom filters per entity type for whitelists
  private final Map<EntityType, BloomFilter> whitelistFilters = new ConcurrentHashMap<>();

  // Bloom filters per entity type for greylists
  private final Map<EntityType, BloomFilter> greylistFilters = new ConcurrentHashMap<>();

  // Statistics
  private final AtomicLong bloomFilterHits = new AtomicLong(0);
  private final AtomicLong bloomFilterMisses = new AtomicLong(0);
  private final AtomicLong dbQueriesAvoided = new AtomicLong(0);
  private final AtomicLong falsePositives = new AtomicLong(0);

  // Configuration
  private static final int DEFAULT_EXPECTED_ELEMENTS = 1_000_000;
  private static final double DEFAULT_FALSE_POSITIVE_RATE = 0.01; // 1%
  private volatile boolean initialized = false;

  /** Result of a Bloom filter check. */
  public enum BloomResult {
    /** Value is definitely NOT in the list - no DB check needed */
    DEFINITELY_NOT_IN_LIST,
    /** Value might be in the list - DB check required */
    POSSIBLY_IN_LIST
  }

  /** Check result with metadata. */
  public record CheckResult(
      boolean inList, boolean fromBloomFilter, long latencyMicros, String source) {
    public static CheckResult notInList(boolean fromBloom, long latency) {
      return new CheckResult(false, fromBloom, latency, fromBloom ? "BLOOM_FILTER" : "DATABASE");
    }

    public static CheckResult inList(boolean fromBloom, long latency) {
      return new CheckResult(true, fromBloom, latency, fromBloom ? "BLOOM_FILTER" : "DATABASE");
    }
  }

  /**
   * Checks if a value is blacklisted using Bloom filter optimization.
   *
   * @param entityType Type of entity (PAN, MERCHANT_ID, etc.)
   * @param value Value to check
   * @return Check result with metadata
   */
  public CheckResult isBlacklisted(EntityType entityType, String value) {
    return checkList(entityType, value, blacklistFilters, true);
  }

  /**
   * Checks if a value is whitelisted using Bloom filter optimization.
   *
   * @param entityType Type of entity
   * @param value Value to check
   * @return Check result with metadata
   */
  public CheckResult isWhitelisted(EntityType entityType, String value) {
    return checkList(entityType, value, whitelistFilters, false);
  }

  /**
   * Checks if a value is greylisted using Bloom filter optimization.
   *
   * @param entityType Type of entity
   * @param value Value to check
   * @return Check result with metadata
   */
  public CheckResult isGreylisted(EntityType entityType, String value) {
    return checkList(entityType, value, greylistFilters, false);
  }

  /**
   * Adds a value to the blacklist Bloom filter. Call this when adding entries to the database.
   *
   * @param entityType Type of entity
   * @param value Value to add
   */
  public void addToBlacklist(EntityType entityType, String value) {
    BloomFilter filter =
        blacklistFilters.computeIfAbsent(
            entityType,
            k -> new BloomFilter(DEFAULT_EXPECTED_ELEMENTS, DEFAULT_FALSE_POSITIVE_RATE));
    filter.add(value);
  }

  /**
   * Adds a value to the whitelist Bloom filter.
   *
   * @param entityType Type of entity
   * @param value Value to add
   */
  public void addToWhitelist(EntityType entityType, String value) {
    BloomFilter filter =
        whitelistFilters.computeIfAbsent(
            entityType,
            k -> new BloomFilter(DEFAULT_EXPECTED_ELEMENTS, DEFAULT_FALSE_POSITIVE_RATE));
    filter.add(value);
  }

  /**
   * Adds a value to the greylist Bloom filter.
   *
   * @param entityType Type of entity
   * @param value Value to add
   */
  public void addToGreylist(EntityType entityType, String value) {
    BloomFilter filter =
        greylistFilters.computeIfAbsent(
            entityType,
            k -> new BloomFilter(DEFAULT_EXPECTED_ELEMENTS, DEFAULT_FALSE_POSITIVE_RATE));
    filter.add(value);
  }

  /**
   * Rebuilds all Bloom filters from the database. Should be called periodically or when lists
   * change significantly.
   */
  @Scheduled(fixedRate = 3600_000) // Every hour
  @Transactional(readOnly = true)
  public void rebuildFilters() {
    log.info("Rebuilding Bloom filters from database...");
    long startTime = System.currentTimeMillis();

    try {
      // Clear existing filters
      blacklistFilters.clear();
      whitelistFilters.clear();
      greylistFilters.clear();

      // Rebuild from database
      LocalDateTime now = LocalDateTime.now();

      for (EntityType entityType : EntityType.values()) {
        // Load blacklist entries
        loadEntriesForType(entityType, "BLACKLIST", blacklistFilters, now);

        // Load whitelist entries
        loadEntriesForType(entityType, "WHITELIST", whitelistFilters, now);

        // Load greylist entries
        loadEntriesForType(entityType, "GREYLIST", greylistFilters, now);
      }

      initialized = true;

      long duration = System.currentTimeMillis() - startTime;
      log.info(
          "Bloom filters rebuilt in {}ms. Blacklist entries: {}, Whitelist entries: {}, Greylist entries: {}",
          duration,
          blacklistFilters.values().stream().mapToLong(BloomFilter::getCount).sum(),
          whitelistFilters.values().stream().mapToLong(BloomFilter::getCount).sum(),
          greylistFilters.values().stream().mapToLong(BloomFilter::getCount).sum());

    } catch (Exception e) {
      log.error("Error rebuilding Bloom filters: {}", e.getMessage(), e);
    }
  }

  /** Gets statistics about Bloom filter performance. */
  public Map<String, Object> getStatistics() {
    long hits = bloomFilterHits.get();
    long misses = bloomFilterMisses.get();
    long total = hits + misses;
    double hitRate = total > 0 ? (double) hits / total * 100 : 0;

    return Map.of(
        "bloomFilterHits", hits,
        "bloomFilterMisses", misses,
        "hitRate", String.format("%.2f%%", hitRate),
        "dbQueriesAvoided", dbQueriesAvoided.get(),
        "falsePositives", falsePositives.get(),
        "blacklistFilters", blacklistFilters.size(),
        "whitelistFilters", whitelistFilters.size(),
        "initialized", initialized);
  }

  // ========== Internal Methods ==========

  private CheckResult checkList(
      EntityType entityType,
      String value,
      Map<EntityType, BloomFilter> filters,
      boolean isBlacklist) {
    long startNanos = System.nanoTime();

    if (value == null || value.isBlank()) {
      return CheckResult.notInList(true, 0);
    }

    // Initialize filters if needed
    if (!initialized) {
      rebuildFilters();
    }

    BloomFilter filter = filters.get(entityType);

    // If no filter exists, fall back to DB
    if (filter == null) {
      return checkDatabase(entityType, value, isBlacklist, startNanos);
    }

    // Check Bloom filter
    BloomResult bloomResult = filter.mightContain(value);

    if (bloomResult == BloomResult.DEFINITELY_NOT_IN_LIST) {
      // Bloom filter says definitely not in list - no DB check needed
      bloomFilterHits.incrementAndGet();
      dbQueriesAvoided.incrementAndGet();
      long latencyMicros = (System.nanoTime() - startNanos) / 1000;
      return CheckResult.notInList(true, latencyMicros);
    }

    // Bloom filter says possibly in list - verify with DB
    bloomFilterMisses.incrementAndGet();
    CheckResult dbResult = checkDatabase(entityType, value, isBlacklist, startNanos);

    // Track false positives for monitoring
    if (!dbResult.inList()) {
      falsePositives.incrementAndGet();
    }

    return dbResult;
  }

  private CheckResult checkDatabase(
      EntityType entityType, String value, boolean isBlacklist, long startNanos) {
    try {
      LocalDateTime now = LocalDateTime.now();
      boolean inList =
          isBlacklist
              ? ruleListEntryRepository.isValueBlacklisted(entityType, value, now)
              : ruleListEntryRepository.isValueWhitelisted(entityType, value, now);

      long latencyMicros = (System.nanoTime() - startNanos) / 1000;
      return inList
          ? CheckResult.inList(false, latencyMicros)
          : CheckResult.notInList(false, latencyMicros);
    } catch (Exception e) {
      log.warn("Error checking database for {} {}: {}", entityType, value, e.getMessage());
      long latencyMicros = (System.nanoTime() - startNanos) / 1000;
      return CheckResult.notInList(false, latencyMicros);
    }
  }

  private void loadEntriesForType(
      EntityType entityType,
      String listType,
      Map<EntityType, BloomFilter> filters,
      LocalDateTime now) {
    try {
      // Query database for active entries of this type
      // Note: This should be optimized with a streaming query for large datasets
      List<RuleListEntry> entries =
          switch (listType) {
            case "BLACKLIST" ->
                ruleListEntryRepository.findAll().stream()
                    .filter(e -> e.getList().getEntityType() == entityType)
                    .filter(e -> e.getList().getListType().name().equals("BLACKLIST"))
                    .filter(e -> e.getIsActive())
                    .filter(e -> e.getExpiresAt() == null || e.getExpiresAt().isAfter(now))
                    .toList();
            case "WHITELIST" ->
                ruleListEntryRepository.findAll().stream()
                    .filter(e -> e.getList().getEntityType() == entityType)
                    .filter(e -> e.getList().getListType().name().equals("WHITELIST"))
                    .filter(e -> e.getIsActive())
                    .filter(e -> e.getExpiresAt() == null || e.getExpiresAt().isAfter(now))
                    .toList();
            case "GREYLIST" ->
                ruleListEntryRepository.findAll().stream()
                    .filter(e -> e.getList().getEntityType() == entityType)
                    .filter(e -> e.getList().getListType().name().equals("GREYLIST"))
                    .filter(e -> e.getIsActive())
                    .filter(e -> e.getExpiresAt() == null || e.getExpiresAt().isAfter(now))
                    .toList();
            default -> java.util.Collections.emptyList();
          };

      if (!entries.isEmpty()) {
        BloomFilter filter =
            new BloomFilter(
                Math.max(entries.size() * 2, DEFAULT_EXPECTED_ELEMENTS),
                DEFAULT_FALSE_POSITIVE_RATE);

        for (RuleListEntry entry : entries) {
          filter.add(entry.getEntryValue());
        }

        filters.put(entityType, filter);
        log.debug("Loaded {} {} entries for entity type {}", entries.size(), listType, entityType);
      }
    } catch (Exception e) {
      log.warn("Error loading {} entries for {}: {}", listType, entityType, e.getMessage());
    }
  }

  // ========== Bloom Filter Implementation ==========

  /**
   * Space-efficient probabilistic Bloom filter.
   *
   * <p>Uses multiple hash functions (simulated via double hashing) to achieve target false positive
   * rate with minimal memory.
   */
  private static class BloomFilter {
    private final BitSet bitSet;
    private final int numBits;
    private final int numHashFunctions;
    private final AtomicLong count = new AtomicLong(0);

    /**
     * Creates a new Bloom filter.
     *
     * @param expectedElements Expected number of elements
     * @param falsePositiveRate Target false positive rate (0.0 to 1.0)
     */
    BloomFilter(int expectedElements, double falsePositiveRate) {
      // Calculate optimal size: m = -n * ln(p) / (ln(2)^2)
      this.numBits =
          (int)
              Math.ceil(
                  -expectedElements * Math.log(falsePositiveRate) / (Math.log(2) * Math.log(2)));

      // Calculate optimal number of hash functions: k = (m/n) * ln(2)
      this.numHashFunctions =
          Math.max(1, (int) Math.round((double) numBits / expectedElements * Math.log(2)));

      this.bitSet = new BitSet(numBits);

      log.debug(
          "Created Bloom filter: {} bits, {} hash functions for {} expected elements, {}% FP rate",
          numBits, numHashFunctions, expectedElements, falsePositiveRate * 100);
    }

    /** Adds a value to the filter. */
    void add(String value) {
      if (value == null) return;

      int[] hashes = getHashes(value);
      synchronized (bitSet) {
        for (int hash : hashes) {
          bitSet.set(Math.abs(hash % numBits));
        }
      }
      count.incrementAndGet();
    }

    /**
     * Checks if a value might be in the filter.
     *
     * @return DEFINITELY_NOT_IN_LIST if value is definitely not in filter, POSSIBLY_IN_LIST if
     *     value might be in filter
     */
    BloomResult mightContain(String value) {
      if (value == null) return BloomResult.DEFINITELY_NOT_IN_LIST;

      int[] hashes = getHashes(value);
      synchronized (bitSet) {
        for (int hash : hashes) {
          if (!bitSet.get(Math.abs(hash % numBits))) {
            return BloomResult.DEFINITELY_NOT_IN_LIST;
          }
        }
      }
      return BloomResult.POSSIBLY_IN_LIST;
    }

    /** Gets the number of elements added. */
    long getCount() {
      return count.get();
    }

    /** Generates multiple hash values using double hashing technique. h(i) = h1 + i * h2 */
    private int[] getHashes(String value) {
      byte[] bytes = value.getBytes(StandardCharsets.UTF_8);

      int h1 = murmurHash3(bytes, 0);
      int h2 = murmurHash3(bytes, h1);

      int[] hashes = new int[numHashFunctions];
      for (int i = 0; i < numHashFunctions; i++) {
        hashes[i] = h1 + i * h2;
      }
      return hashes;
    }

    /** MurmurHash3 32-bit implementation. */
    @SuppressWarnings("fallthrough")
    private static int murmurHash3(byte[] data, int seed) {
      int h = seed;
      int len = data.length;
      int i = 0;

      while (len >= 4) {
        int k =
            (data[i] & 0xff)
                | ((data[i + 1] & 0xff) << 8)
                | ((data[i + 2] & 0xff) << 16)
                | ((data[i + 3] & 0xff) << 24);
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
        case 3:
          k ^= (data[i + 2] & 0xff) << 16;
        case 2:
          k ^= (data[i + 1] & 0xff) << 8;
        case 1:
          k ^= (data[i] & 0xff);
          k *= 0xcc9e2d51;
          k = Integer.rotateLeft(k, 15);
          k *= 0x1b873593;
          h ^= k;
          break;
        default:
          break;
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
