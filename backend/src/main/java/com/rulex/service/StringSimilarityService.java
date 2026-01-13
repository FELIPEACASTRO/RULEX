package com.rulex.service;

import java.util.HashSet;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.text.similarity.CosineSimilarity;
import org.apache.commons.text.similarity.JaroWinklerSimilarity;
import org.apache.commons.text.similarity.LevenshteinDistance;
import org.springframework.stereotype.Service;

/**
 * Serviço de similaridade de strings para detecção de fraude. Implementa os operadores de Sanctions
 * & Name Matching (SANC001-SANC007).
 */
@Service
@Slf4j
public class StringSimilarityService {

  private final LevenshteinDistance levenshtein = LevenshteinDistance.getDefaultInstance();
  private final JaroWinklerSimilarity jaroWinkler = new JaroWinklerSimilarity();
  private final CosineSimilarity cosineSimilarity = new CosineSimilarity();

  /** SANC001: Levenshtein Distance - Distância de edição */
  public int levenshteinDistance(String s1, String s2) {
    if (s1 == null || s2 == null) return -1;
    return levenshtein.apply(normalize(s1), normalize(s2));
  }

  /** SANC001: Levenshtein Similarity (0-1) */
  public double levenshteinSimilarity(String s1, String s2) {
    if (s1 == null || s2 == null) return 0.0;
    String n1 = normalize(s1);
    String n2 = normalize(s2);
    int maxLen = Math.max(n1.length(), n2.length());
    if (maxLen == 0) return 1.0;
    return 1.0 - (double) levenshtein.apply(n1, n2) / maxLen;
  }

  /** SANC002: Jaro-Winkler Similarity */
  public double jaroWinklerSimilarity(String s1, String s2) {
    if (s1 == null || s2 == null) return 0.0;
    return jaroWinkler.apply(normalize(s1), normalize(s2));
  }

  /** SANC003: Soundex - Codificação fonética */
  public String soundex(String s) {
    if (s == null || s.isEmpty()) return "";

    String normalized = normalize(s).toUpperCase();
    if (normalized.isEmpty()) return "";

    StringBuilder soundex = new StringBuilder();
    soundex.append(normalized.charAt(0));

    char lastCode = getSoundexCode(normalized.charAt(0));

    for (int i = 1; i < normalized.length() && soundex.length() < 4; i++) {
      char code = getSoundexCode(normalized.charAt(i));
      if (code != '0' && code != lastCode) {
        soundex.append(code);
        lastCode = code;
      }
    }

    while (soundex.length() < 4) {
      soundex.append('0');
    }

    return soundex.toString();
  }

  private char getSoundexCode(char c) {
    return switch (Character.toUpperCase(c)) {
      case 'B', 'F', 'P', 'V' -> '1';
      case 'C', 'G', 'J', 'K', 'Q', 'S', 'X', 'Z' -> '2';
      case 'D', 'T' -> '3';
      case 'L' -> '4';
      case 'M', 'N' -> '5';
      case 'R' -> '6';
      default -> '0';
    };
  }

  /** SANC003: Soundex Similarity */
  public boolean soundexMatch(String s1, String s2) {
    return soundex(s1).equals(soundex(s2));
  }

  /** SANC004: Metaphone - Codificação fonética avançada */
  public String metaphone(String s) {
    if (s == null || s.isEmpty()) return "";

    String normalized = normalize(s).toUpperCase();
    StringBuilder result = new StringBuilder();

    int i = 0;
    while (i < normalized.length() && result.length() < 6) {
      char c = normalized.charAt(i);
      char next = i + 1 < normalized.length() ? normalized.charAt(i + 1) : ' ';

      switch (c) {
        case 'A', 'E', 'I', 'O', 'U' -> {
          if (i == 0) result.append(c);
        }
        case 'B' -> {
          if (i == normalized.length() - 1 && normalized.charAt(i - 1) == 'M') {
            // Silent B after M
          } else {
            result.append('B');
          }
        }
        case 'C' -> {
          if (next == 'H') {
            result.append('X');
            i++;
          } else if (next == 'I' || next == 'E' || next == 'Y') {
            result.append('S');
          } else {
            result.append('K');
          }
        }
        case 'D' -> {
          if (next == 'G') {
            result.append('J');
            i++;
          } else {
            result.append('T');
          }
        }
        case 'F', 'J', 'L', 'M', 'N', 'R' -> result.append(c);
        case 'G' -> {
          if (next == 'H' || next == 'N') {
            // Silent G
          } else if (next == 'I' || next == 'E' || next == 'Y') {
            result.append('J');
          } else {
            result.append('K');
          }
        }
        case 'H' -> {
          char prev = i > 0 ? normalized.charAt(i - 1) : ' ';
          if (!"AEIOU".contains(String.valueOf(prev)) && "AEIOU".contains(String.valueOf(next))) {
            result.append('H');
          }
        }
        case 'K' -> {
          if (i == 0 || normalized.charAt(i - 1) != 'C') {
            result.append('K');
          }
        }
        case 'P' -> {
          if (next == 'H') {
            result.append('F');
            i++;
          } else {
            result.append('P');
          }
        }
        case 'Q' -> result.append('K');
        case 'S' -> {
          if (next == 'H') {
            result.append('X');
            i++;
          } else {
            result.append('S');
          }
        }
        case 'T' -> {
          if (next == 'H') {
            result.append('0'); // TH sound
            i++;
          } else if (next != 'C'
              || (i + 2 < normalized.length() && normalized.charAt(i + 2) != 'H')) {
            result.append('T');
          }
        }
        case 'V' -> result.append('F');
        case 'W', 'Y' -> {
          if ("AEIOU".contains(String.valueOf(next))) {
            result.append(c);
          }
        }
        case 'X' -> result.append("KS");
        case 'Z' -> result.append('S');
      }
      i++;
    }

    return result.toString();
  }

  /** SANC005: N-Gram Similarity */
  public double ngramSimilarity(String s1, String s2, int n) {
    if (s1 == null || s2 == null) return 0.0;

    Set<String> ngrams1 = getNgrams(normalize(s1), n);
    Set<String> ngrams2 = getNgrams(normalize(s2), n);

    if (ngrams1.isEmpty() && ngrams2.isEmpty()) return 1.0;
    if (ngrams1.isEmpty() || ngrams2.isEmpty()) return 0.0;

    Set<String> intersection = new HashSet<>(ngrams1);
    intersection.retainAll(ngrams2);

    Set<String> union = new HashSet<>(ngrams1);
    union.addAll(ngrams2);

    return (double) intersection.size() / union.size();
  }

  private Set<String> getNgrams(String s, int n) {
    Set<String> ngrams = new HashSet<>();
    if (s.length() < n) {
      ngrams.add(s);
      return ngrams;
    }
    for (int i = 0; i <= s.length() - n; i++) {
      ngrams.add(s.substring(i, i + n));
    }
    return ngrams;
  }

  /** SANC006: Combined Similarity Score */
  public double combinedSimilarity(String s1, String s2) {
    if (s1 == null || s2 == null) return 0.0;

    double levenshteinScore = levenshteinSimilarity(s1, s2);
    double jaroWinklerScore = jaroWinklerSimilarity(s1, s2);
    double soundexScore = soundexMatch(s1, s2) ? 1.0 : 0.0;
    double trigramScore = ngramSimilarity(s1, s2, 3);

    // Weighted combination
    return 0.3 * levenshteinScore
        + 0.4 * jaroWinklerScore
        + 0.15 * soundexScore
        + 0.15 * trigramScore;
  }

  /** SANC007: Fuzzy Name Match */
  public NameMatchResult fuzzyNameMatch(String name1, String name2, double threshold) {
    if (name1 == null || name2 == null) {
      return new NameMatchResult(false, 0.0, "NULL_INPUT");
    }

    double similarity = combinedSimilarity(name1, name2);
    boolean isMatch = similarity >= threshold;

    String matchType;
    if (similarity >= 0.95) {
      matchType = "EXACT";
    } else if (similarity >= 0.85) {
      matchType = "STRONG";
    } else if (similarity >= 0.70) {
      matchType = "MODERATE";
    } else if (similarity >= threshold) {
      matchType = "WEAK";
    } else {
      matchType = "NO_MATCH";
    }

    return new NameMatchResult(isMatch, similarity, matchType);
  }

  /** Normaliza string para comparação */
  private String normalize(String s) {
    if (s == null) return "";
    return s.toLowerCase()
        .replaceAll("[^a-z0-9\\s]", "") // Remove caracteres especiais
        .replaceAll("\\s+", " ") // Normaliza espaços
        .trim();
  }

  /** Resultado de match de nome */
  public record NameMatchResult(boolean isMatch, double similarity, String matchType) {}

  /** Verifica se nome está em lista de sanções (simulado) */
  public boolean isInSanctionsList(
      String name, java.util.List<String> sanctionsList, double threshold) {
    if (name == null || sanctionsList == null) return false;

    for (String sanctionedName : sanctionsList) {
      if (combinedSimilarity(name, sanctionedName) >= threshold) {
        log.info(
            "Potential sanctions match: {} ~ {} (threshold: {})", name, sanctionedName, threshold);
        return true;
      }
    }
    return false;
  }
}
