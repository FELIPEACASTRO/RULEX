package com.rulex.service.complex.evaluation;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.Neo4jGraphService;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import com.rulex.service.complex.parsing.NumericParser;
import java.util.List;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class Neo4jGraphEvaluator {

  private Neo4jGraphEvaluator() {}

  public static boolean evaluateNeo4jWeaklyConnectedComponents(
      RuleCondition condition, EvaluationContext context, Neo4jGraphService neo4jGraphService) {
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    int componentId = neo4jGraphService.getWeaklyConnectedComponentId(accountId);
    int threshold = parseIntSafe(condition.getValueSingle(), 0);
    log.debug(
        "NEO4J_WCC: accountId={}, componentId={}, threshold={}", accountId, componentId, threshold);
    return componentId >= threshold;
  }

  public static boolean evaluateNeo4jDegreeCentrality(
      RuleCondition condition, EvaluationContext context, Neo4jGraphService neo4jGraphService) {
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    int degree = neo4jGraphService.getDegreeCentrality(accountId);
    int threshold = parseIntSafe(condition.getValueSingle(), 5);
    log.debug("NEO4J_DEGREE: accountId={}, degree={}, threshold={}", accountId, degree, threshold);
    return degree > threshold;
  }

  public static boolean evaluateNeo4jPagerankFraudScore(
      RuleCondition condition, EvaluationContext context, Neo4jGraphService neo4jGraphService) {
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    double score = neo4jGraphService.getPageRankScore(accountId);
    double threshold = parseDoubleSafe(condition.getValueSingle(), 0.5);
    log.debug("NEO4J_PAGERANK: accountId={}, score={}, threshold={}", accountId, score, threshold);
    return score > threshold;
  }

  public static boolean evaluateNeo4jLouvainCommunityDetection(
      RuleCondition condition, EvaluationContext context, Neo4jGraphService neo4jGraphService) {
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    int communityId = neo4jGraphService.getLouvainCommunityId(accountId);
    log.debug("NEO4J_LOUVAIN: accountId={}, communityId={}", accountId, communityId);
    return communityId >= 0;
  }

  public static boolean evaluateNeo4jPairwiseSimilarityPii(
      RuleCondition condition, EvaluationContext context, Neo4jGraphService neo4jGraphService) {
    String accountId = getAccountId(context);
    String otherAccountId = condition.getValueSingle();
    if (accountId == null || otherAccountId == null) return false;

    double similarity = neo4jGraphService.getPairwiseSimilarity(accountId, otherAccountId);
    double threshold = 0.7;
    log.debug(
        "NEO4J_PII_SIMILARITY: {} vs {}, similarity={}", accountId, otherAccountId, similarity);
    return similarity > threshold;
  }

  public static boolean evaluateNeo4jEntityResolutionSharedPii(
      RuleCondition condition, EvaluationContext context, Neo4jGraphService neo4jGraphService) {
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    List<String> sharedAccounts = neo4jGraphService.findAccountsWithSharedPii(accountId);
    int threshold = parseIntSafe(condition.getValueSingle(), 1);
    log.debug("NEO4J_SHARED_PII: accountId={}, sharedCount={}", accountId, sharedAccounts.size());
    return sharedAccounts.size() >= threshold;
  }

  public static boolean evaluateNeo4jFraudRingDetection(
      RuleCondition condition, EvaluationContext context, Neo4jGraphService neo4jGraphService) {
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    int minRingSize = parseIntSafe(condition.getValueSingle(), 3);
    boolean inRing = neo4jGraphService.isInFraudRing(accountId, minRingSize);
    log.debug("NEO4J_FRAUD_RING: accountId={}, inRing={}", accountId, inRing);
    return inRing;
  }

  public static boolean evaluateNeo4jMoneyMuleNetworkAnalysis(
      RuleCondition condition, EvaluationContext context, Neo4jGraphService neo4jGraphService) {
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    int maxHops = parseIntSafe(condition.getValueSingle(), 5);
    Map<String, Object> analysis = neo4jGraphService.analyzeMoneyMuleNetwork(accountId, maxHops);
    int suspiciousCount = (int) analysis.getOrDefault("suspiciousDestinations", 0);
    log.debug("NEO4J_MULE_NETWORK: accountId={}, suspicious={}", accountId, suspiciousCount);
    return suspiciousCount > 0;
  }

  public static boolean evaluateNeo4jCircularTransactionDetection(
      RuleCondition condition, EvaluationContext context, Neo4jGraphService neo4jGraphService) {
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    int minCycleLength = parseIntSafe(condition.getValueSingle(), 3);
    boolean hasCircular = neo4jGraphService.hasCircularTransactions(accountId, minCycleLength);
    log.debug("NEO4J_CIRCULAR: accountId={}, hasCircular={}", accountId, hasCircular);
    return hasCircular;
  }

  public static boolean evaluateNeo4jFirstPartyFraudClustering(
      RuleCondition condition, EvaluationContext context, Neo4jGraphService neo4jGraphService) {
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    int clusterId = neo4jGraphService.getFirstPartyFraudClusterId(accountId);
    log.debug("NEO4J_FPF_CLUSTER: accountId={}, clusterId={}", accountId, clusterId);
    return clusterId >= 0;
  }

  public static boolean evaluateNeo4jSecondLevelFraudsterId(
      RuleCondition condition, EvaluationContext context, Neo4jGraphService neo4jGraphService) {
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    boolean isSecondLevel = neo4jGraphService.isSecondLevelFraudster(accountId);
    log.debug("NEO4J_2ND_LEVEL: accountId={}, isSecondLevel={}", accountId, isSecondLevel);
    return isSecondLevel;
  }

  public static boolean evaluateNeo4jBetweennessCentralityMule(
      RuleCondition condition, EvaluationContext context, Neo4jGraphService neo4jGraphService) {
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    double centrality = neo4jGraphService.getBetweennessCentrality(accountId);
    double threshold = parseDoubleSafe(condition.getValueSingle(), 0.1);
    log.debug("NEO4J_BETWEENNESS: accountId={}, centrality={}", accountId, centrality);
    return centrality > threshold;
  }

  public static boolean evaluateNeo4jLabelPropagationFraudSpread(
      RuleCondition condition, EvaluationContext context, Neo4jGraphService neo4jGraphService) {
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    String label = neo4jGraphService.getFraudLabel(accountId);
    log.debug("NEO4J_LABEL_PROP: accountId={}, label={}", accountId, label);
    return "FRAUD".equalsIgnoreCase(label) || "HIGH_RISK".equalsIgnoreCase(label);
  }

  public static boolean evaluateNeo4jShortestPathAmlTracking(
      RuleCondition condition, EvaluationContext context, Neo4jGraphService neo4jGraphService) {
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    int pathLength = neo4jGraphService.getShortestPathToHighRisk(accountId);
    int threshold = parseIntSafe(condition.getValueSingle(), 3);
    log.debug("NEO4J_AML_PATH: accountId={}, pathLength={}", accountId, pathLength);
    return pathLength >= 0 && pathLength <= threshold;
  }

  public static boolean evaluateNeo4jTriangleCountCollusion(
      RuleCondition condition, EvaluationContext context, Neo4jGraphService neo4jGraphService) {
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    int triangles = neo4jGraphService.getTriangleCount(accountId);
    int threshold = parseIntSafe(condition.getValueSingle(), 2);
    log.debug("NEO4J_TRIANGLES: accountId={}, count={}", accountId, triangles);
    return triangles >= threshold;
  }

  public static boolean evaluateNeo4jNodeSimilaritySyntheticId(
      RuleCondition condition, EvaluationContext context, Neo4jGraphService neo4jGraphService) {
    String accountId = getAccountId(context);
    String otherAccountId = condition.getValueSingle();
    if (accountId == null || otherAccountId == null) return false;

    double similarity = neo4jGraphService.getNodeSimilarity(accountId, otherAccountId);
    log.debug("NEO4J_NODE_SIM: {} vs {}, similarity={}", accountId, otherAccountId, similarity);
    return similarity > 0.8;
  }

  public static boolean evaluateNeo4jGraphEmbeddingFraudPrediction(
      RuleCondition condition, EvaluationContext context, Neo4jGraphService neo4jGraphService) {
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    double probability = neo4jGraphService.getFraudProbabilityFromEmbedding(accountId);
    double threshold = parseDoubleSafe(condition.getValueSingle(), 0.7);
    log.debug("NEO4J_EMBEDDING: accountId={}, probability={}", accountId, probability);
    return probability > threshold;
  }

  /**
   * Detecta padrões temporais de motif no grafo de transações.
   *
   * <p>Motifs são padrões estruturais recorrentes em grafos temporais que podem indicar:
   * - TRIANGLE: Três contas transacionando entre si em sequência
   * - STAR: Uma conta central recebendo/enviando para múltiplas contas
   * - CHAIN: Sequência linear de transações (A→B→C→D)
   * - CYCLE: Transações que retornam à origem
   *
   * <p>Formato do valor: "pattern|threshold" ou apenas "threshold"
   * Exemplos: "TRIANGLE|3", "STAR|5", "2" (usa TRIANGLE como padrão)
   *
   * @param condition Condição com o padrão e threshold
   * @param context Contexto de avaliação
   * @param neo4jGraphService Serviço Neo4j
   * @return true se o número de motifs encontrados >= threshold
   */
  public static boolean evaluateNeo4jTemporalMotifPattern(
      RuleCondition condition, EvaluationContext context, Neo4jGraphService neo4jGraphService) {
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    // Parse do valor: "pattern|threshold" ou apenas "threshold"
    String valueSingle = condition.getValueSingle();
    String motifPattern = "TRIANGLE"; // padrão default
    int threshold = 1;

    if (valueSingle != null && !valueSingle.isBlank()) {
      String[] parts = valueSingle.split("\\|");
      if (parts.length >= 2) {
        motifPattern = parts[0].trim().toUpperCase();
        threshold = parseIntSafe(parts[1].trim(), 1);
      } else {
        // Tenta interpretar como número (threshold) ou como pattern
        try {
          threshold = Integer.parseInt(parts[0].trim());
        } catch (NumberFormatException e) {
          motifPattern = parts[0].trim().toUpperCase();
        }
      }
    }

    // Validar padrão suportado
    if (!isValidMotifPattern(motifPattern)) {
      log.warn(
          "NEO4J_TEMPORAL_MOTIF: Padrão '{}' não reconhecido. Usando TRIANGLE.", motifPattern);
      motifPattern = "TRIANGLE";
    }

    int count = neo4jGraphService.getTemporalMotifCount(accountId, motifPattern);
    log.debug(
        "NEO4J_TEMPORAL_MOTIF: accountId={}, pattern={}, count={}, threshold={}",
        accountId,
        motifPattern,
        count,
        threshold);
    return count >= threshold;
  }

  /**
   * Valida se o padrão de motif é suportado.
   */
  private static boolean isValidMotifPattern(String pattern) {
    return pattern != null
        && (pattern.equals("TRIANGLE")
            || pattern.equals("STAR")
            || pattern.equals("CHAIN")
            || pattern.equals("CYCLE")
            || pattern.equals("FAN_OUT")
            || pattern.equals("FAN_IN")
            || pattern.equals("BIPARTITE"));
  }

  private static int parseIntSafe(String value, int defaultValue) {
    return NumericParser.parseIntSafe(value, defaultValue);
  }

  private static double parseDoubleSafe(String value, double defaultValue) {
    return NumericParser.parseDoubleSafe(value, defaultValue);
  }

  private static String getAccountId(EvaluationContext context) {
    if (context == null || context.getTransactionRequest() == null) {
      return null;
    }
    TransactionRequest req = context.getTransactionRequest();
    if (req.getCustomerAcctNumber() != null) {
      return String.valueOf(req.getCustomerAcctNumber());
    }
    if (req.getCustomerIdFromHeader() != null && !req.getCustomerIdFromHeader().isBlank()) {
      return req.getCustomerIdFromHeader();
    }
    if (req.getPan() != null && !req.getPan().isBlank()) {
      return "PAN_" + req.getPan().hashCode();
    }
    return null;
  }
}
