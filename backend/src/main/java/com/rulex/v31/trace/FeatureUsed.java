package com.rulex.v31.trace;

/**
 * Minimal, deterministic audit-friendly record of an input/feature/function used during evaluation.
 *
 * <p>Intentionally stores only hashed/masked value representations.
 */
public record FeatureUsed(
    String source,
    String name,
    String entityKey,
    String featureVersion,
    String windowName,
    String valueHash,
    String valueMasked) {}
