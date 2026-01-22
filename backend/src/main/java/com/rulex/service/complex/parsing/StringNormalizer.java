package com.rulex.service.complex.parsing;

import java.text.Normalizer;
import java.util.Locale;

public final class StringNormalizer {

  private StringNormalizer() {}

  public static String normalizeForMatch(String value) {
    if (value == null) return "";
    String s = value.trim().toLowerCase(Locale.ROOT);
    if (s.isBlank()) return "";
    s = Normalizer.normalize(s, Normalizer.Form.NFD).replaceAll("\\p{M}", "");
    s = s.replaceAll("[^a-z0-9\\s]", " ");
    s = s.replaceAll("\\s+", " ").trim();
    return s;
  }
}
