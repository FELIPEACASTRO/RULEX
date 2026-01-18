package com.rulex.service.complex.evaluator;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.RuleCondition.ConditionOperator;
import com.rulex.service.Neo4jGraphService;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Map;
import java.util.Set;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Avaliador para operadores de grafo (Neo4j).
 *
 * <p>Operadores suportados:
 * <ul>
 *   <li>NEO4J_WEAKLY_CONNECTED_COMPONENTS - componentes fracamente conectados</li>
 *   <li>NEO4J_DEGREE_CENTRALITY - centralidade de grau</li>
 *   <li>NEO4J_PAGERANK_FRAUD_SCORE - score de fraude via PageRank</li>
 *   <li>NEO4J_LOUVAIN_COMMUNITY_DETECTION - detecção de comunidade</li>
 *   <li>NEO4J_FRAUD_RING_DETECTION - detecção de anel de fraude</li>
 *   <li>NEO4J_CIRCULAR_TRANSACTION_DETECTION - detecção de transação circular</li>
 *   <li>NEO4J_BETWEENNESS_CENTRALITY_MULE - centralidade para mules</li>
 *   <li>NEO4J_TRIANGLE_COUNT_COLLUSION - contagem de triângulos</li>
 * </ul>
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class GraphOperatorEvaluator implements OperatorEvaluator {

    private final Neo4jGraphService neo4jService;

    private static final Set<ConditionOperator> SUPPORTED = Set.of(
        ConditionOperator.NEO4J_WEAKLY_CONNECTED_COMPONENTS,
        ConditionOperator.NEO4J_DEGREE_CENTRALITY,
        ConditionOperator.NEO4J_PAGERANK_FRAUD_SCORE,
        ConditionOperator.NEO4J_LOUVAIN_COMMUNITY_DETECTION,
        ConditionOperator.NEO4J_PAIRWISE_SIMILARITY_PII,
        ConditionOperator.NEO4J_ENTITY_RESOLUTION_SHARED_PII,
        ConditionOperator.NEO4J_FRAUD_RING_DETECTION,
        ConditionOperator.NEO4J_MONEY_MULE_NETWORK_ANALYSIS,
        ConditionOperator.NEO4J_CIRCULAR_TRANSACTION_DETECTION,
        ConditionOperator.NEO4J_FIRST_PARTY_FRAUD_CLUSTERING,
        ConditionOperator.NEO4J_SECOND_LEVEL_FRAUDSTER_ID,
        ConditionOperator.NEO4J_BETWEENNESS_CENTRALITY_MULE,
        ConditionOperator.NEO4J_LABEL_PROPAGATION_FRAUD_SPREAD,
        ConditionOperator.NEO4J_SHORTEST_PATH_AML_TRACKING,
        ConditionOperator.NEO4J_TRIANGLE_COUNT_COLLUSION,
        ConditionOperator.NEO4J_NODE_SIMILARITY_SYNTHETIC_ID,
        ConditionOperator.NEO4J_GRAPH_EMBEDDING_FRAUD_PREDICTION,
        ConditionOperator.NEO4J_TEMPORAL_MOTIF_PATTERN
    );

    @Override
    public Set<ConditionOperator> getSupportedOperators() {
        return SUPPORTED;
    }

    @Override
    public boolean evaluate(RuleCondition condition, EvaluationContext context) {
        ConditionOperator op = condition.getOperator();
        TransactionRequest request = context.getTransactionRequest();

        log.debug("GraphOperatorEvaluator: op={}", op);

        if (request == null) {
            log.debug("TransactionRequest is null");
            return false;
        }

        String accountId = getAccountId(request, context);
        if (accountId == null || accountId.isEmpty()) {
            log.debug("AccountId is null or empty");
            return false;
        }

        try {
            return switch (op) {
                case NEO4J_WEAKLY_CONNECTED_COMPONENTS -> evaluateWcc(accountId, condition);
                case NEO4J_DEGREE_CENTRALITY -> evaluateDegreeCentrality(accountId, condition);
                case NEO4J_PAGERANK_FRAUD_SCORE -> evaluatePageRank(accountId, condition);
                case NEO4J_LOUVAIN_COMMUNITY_DETECTION -> evaluateLouvain(accountId, condition);
                case NEO4J_PAIRWISE_SIMILARITY_PII -> evaluatePairwiseSimilarity(accountId, condition, context);
                case NEO4J_ENTITY_RESOLUTION_SHARED_PII -> evaluateSharedPii(accountId, condition);
                case NEO4J_FRAUD_RING_DETECTION -> evaluateFraudRing(accountId, condition);
                case NEO4J_MONEY_MULE_NETWORK_ANALYSIS -> evaluateMoneyMuleNetwork(accountId, condition);
                case NEO4J_CIRCULAR_TRANSACTION_DETECTION -> evaluateCircularTransactions(accountId, condition);
                case NEO4J_FIRST_PARTY_FRAUD_CLUSTERING -> evaluateFirstPartyFraud(accountId, condition);
                case NEO4J_SECOND_LEVEL_FRAUDSTER_ID -> evaluateSecondLevelFraudster(accountId, condition);
                case NEO4J_BETWEENNESS_CENTRALITY_MULE -> evaluateBetweennessCentrality(accountId, condition);
                case NEO4J_LABEL_PROPAGATION_FRAUD_SPREAD -> evaluateFraudLabel(accountId, condition);
                case NEO4J_SHORTEST_PATH_AML_TRACKING -> evaluateShortestPath(accountId, condition);
                case NEO4J_TRIANGLE_COUNT_COLLUSION -> evaluateTriangleCount(accountId, condition);
                case NEO4J_NODE_SIMILARITY_SYNTHETIC_ID -> evaluateNodeSimilarity(accountId, condition, context);
                case NEO4J_GRAPH_EMBEDDING_FRAUD_PREDICTION -> evaluateFraudProbability(accountId, condition);
                case NEO4J_TEMPORAL_MOTIF_PATTERN -> evaluateTemporalMotif(accountId, condition);
                default -> false;
            };
        } catch (Exception e) {
            log.error("Erro ao avaliar operador de grafo {}: {}", op, e.getMessage());
            return false;
        }
    }

    private String getAccountId(TransactionRequest request, EvaluationContext context) {
        // Tentar obter do payload primeiro
        Map<String, Object> payload = context.getPayload();
        if (payload != null) {
            Object accountId = payload.get("accountId");
            if (accountId != null) return String.valueOf(accountId);

            Object customerId = payload.get("customerId");
            if (customerId != null) return String.valueOf(customerId);
        }

        // Fallback para TransactionRequest
        if (request != null && request.getCustomerIdFromHeader() != null) {
            return request.getCustomerIdFromHeader();
        }

        return null;
    }

    private boolean evaluateWcc(String accountId, RuleCondition condition) {
        int componentId = neo4jService.getWeaklyConnectedComponentId(accountId);
        int threshold = parseIntSafe(condition.getValueSingle(), 0);
        log.debug("NEO4J_WCC: componentId={}, threshold={}", componentId, threshold);
        return componentId > threshold;
    }

    private boolean evaluateDegreeCentrality(String accountId, RuleCondition condition) {
        int degree = neo4jService.getDegreeCentrality(accountId);
        int threshold = parseIntSafe(condition.getValueSingle(), 5);
        log.debug("NEO4J_DEGREE_CENTRALITY: degree={}, threshold={}", degree, threshold);
        return degree > threshold;
    }

    private boolean evaluatePageRank(String accountId, RuleCondition condition) {
        double score = neo4jService.getPageRankScore(accountId);
        double threshold = parseDoubleSafe(condition.getValueSingle(), 0.5);
        log.debug("NEO4J_PAGERANK: score={}, threshold={}", score, threshold);
        return score > threshold;
    }

    private boolean evaluateLouvain(String accountId, RuleCondition condition) {
        int communityId = neo4jService.getLouvainCommunityId(accountId);
        // Se communityId > 0, significa que faz parte de uma comunidade detectada
        log.debug("NEO4J_LOUVAIN: communityId={}", communityId);
        return communityId > 0;
    }

    private boolean evaluatePairwiseSimilarity(String accountId, RuleCondition condition, EvaluationContext context) {
        String otherAccountId = condition.getValueFieldRef();
        if (otherAccountId == null) {
            Map<String, Object> payload = context.getPayload();
            if (payload != null) {
                Object other = payload.get("targetAccountId");
                if (other != null) otherAccountId = String.valueOf(other);
            }
        }

        if (otherAccountId == null) return false;

        double similarity = neo4jService.getPairwiseSimilarity(accountId, otherAccountId);
        double threshold = parseDoubleSafe(condition.getValueSingle(), 0.7);
        log.debug("NEO4J_PAIRWISE_SIMILARITY: similarity={}, threshold={}", similarity, threshold);
        return similarity > threshold;
    }

    private boolean evaluateSharedPii(String accountId, RuleCondition condition) {
        var sharedAccounts = neo4jService.findAccountsWithSharedPii(accountId);
        int threshold = parseIntSafe(condition.getValueSingle(), 1);
        log.debug("NEO4J_SHARED_PII: sharedCount={}, threshold={}", sharedAccounts.size(), threshold);
        return sharedAccounts.size() >= threshold;
    }

    private boolean evaluateFraudRing(String accountId, RuleCondition condition) {
        int minRingSize = parseIntSafe(condition.getValueSingle(), 3);
        boolean inRing = neo4jService.isInFraudRing(accountId, minRingSize);
        log.debug("NEO4J_FRAUD_RING: inRing={}, minSize={}", inRing, minRingSize);
        return inRing;
    }

    private boolean evaluateMoneyMuleNetwork(String accountId, RuleCondition condition) {
        int maxHops = parseIntSafe(condition.getValueSingle(), 3);
        Map<String, Object> analysis = neo4jService.analyzeMoneyMuleNetwork(accountId, maxHops);

        // Verificar se há indicadores de money mule
        Object riskScore = analysis.get("riskScore");
        if (riskScore instanceof Number) {
            double score = ((Number) riskScore).doubleValue();
            double threshold = parseDoubleSafe(condition.getValueMin(), 0.5);
            log.debug("NEO4J_MONEY_MULE: score={}, threshold={}", score, threshold);
            return score > threshold;
        }

        return false;
    }

    private boolean evaluateCircularTransactions(String accountId, RuleCondition condition) {
        int minCycleLength = parseIntSafe(condition.getValueSingle(), 3);
        boolean hasCircular = neo4jService.hasCircularTransactions(accountId, minCycleLength);
        log.debug("NEO4J_CIRCULAR: hasCircular={}, minCycle={}", hasCircular, minCycleLength);
        return hasCircular;
    }

    private boolean evaluateFirstPartyFraud(String accountId, RuleCondition condition) {
        int clusterId = neo4jService.getFirstPartyFraudClusterId(accountId);
        log.debug("NEO4J_FIRST_PARTY_FRAUD: clusterId={}", clusterId);
        return clusterId > 0;
    }

    private boolean evaluateSecondLevelFraudster(String accountId, RuleCondition condition) {
        boolean isSecondLevel = neo4jService.isSecondLevelFraudster(accountId);
        log.debug("NEO4J_SECOND_LEVEL_FRAUDSTER: isSecondLevel={}", isSecondLevel);
        return isSecondLevel;
    }

    private boolean evaluateBetweennessCentrality(String accountId, RuleCondition condition) {
        double centrality = neo4jService.getBetweennessCentrality(accountId);
        double threshold = parseDoubleSafe(condition.getValueSingle(), 0.3);
        log.debug("NEO4J_BETWEENNESS: centrality={}, threshold={}", centrality, threshold);
        return centrality > threshold;
    }

    private boolean evaluateFraudLabel(String accountId, RuleCondition condition) {
        String label = neo4jService.getFraudLabel(accountId);
        String expectedLabel = condition.getValueSingle();

        if (expectedLabel == null || expectedLabel.isEmpty()) {
            // Se não especificou label, retorna true se tiver qualquer label de fraude
            log.debug("NEO4J_FRAUD_LABEL: label={}", label);
            return label != null && !label.isEmpty() && !"CLEAN".equalsIgnoreCase(label);
        }

        log.debug("NEO4J_FRAUD_LABEL: label={}, expected={}", label, expectedLabel);
        return expectedLabel.equalsIgnoreCase(label);
    }

    private boolean evaluateShortestPath(String accountId, RuleCondition condition) {
        int pathLength = neo4jService.getShortestPathToHighRisk(accountId);
        int threshold = parseIntSafe(condition.getValueSingle(), 3);
        log.debug("NEO4J_SHORTEST_PATH: pathLength={}, threshold={}", pathLength, threshold);
        return pathLength > 0 && pathLength <= threshold;
    }

    private boolean evaluateTriangleCount(String accountId, RuleCondition condition) {
        int triangles = neo4jService.getTriangleCount(accountId);
        int threshold = parseIntSafe(condition.getValueSingle(), 2);
        log.debug("NEO4J_TRIANGLE_COUNT: triangles={}, threshold={}", triangles, threshold);
        return triangles >= threshold;
    }

    private boolean evaluateNodeSimilarity(String accountId, RuleCondition condition, EvaluationContext context) {
        String otherAccountId = condition.getValueFieldRef();
        if (otherAccountId == null) {
            Map<String, Object> payload = context.getPayload();
            if (payload != null) {
                Object other = payload.get("targetAccountId");
                if (other != null) otherAccountId = String.valueOf(other);
            }
        }

        if (otherAccountId == null) return false;

        double similarity = neo4jService.getNodeSimilarity(accountId, otherAccountId);
        double threshold = parseDoubleSafe(condition.getValueSingle(), 0.8);
        log.debug("NEO4J_NODE_SIMILARITY: similarity={}, threshold={}", similarity, threshold);
        return similarity > threshold;
    }

    private boolean evaluateFraudProbability(String accountId, RuleCondition condition) {
        double probability = neo4jService.getFraudProbabilityFromEmbedding(accountId);
        double threshold = parseDoubleSafe(condition.getValueSingle(), 0.7);
        log.debug("NEO4J_FRAUD_PROBABILITY: probability={}, threshold={}", probability, threshold);
        return probability > threshold;
    }

    private boolean evaluateTemporalMotif(String accountId, RuleCondition condition) {
        String motifPattern = condition.getValueSingle();
        if (motifPattern == null) motifPattern = "BURST";

        int count = neo4jService.getTemporalMotifCount(accountId, motifPattern);
        int threshold = parseIntSafe(condition.getValueMin(), 1);
        log.debug("NEO4J_TEMPORAL_MOTIF: pattern={}, count={}, threshold={}", motifPattern, count, threshold);
        return count >= threshold;
    }

    private int parseIntSafe(String value, int defaultValue) {
        if (value == null || value.isEmpty()) return defaultValue;
        try {
            return Integer.parseInt(value);
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    private double parseDoubleSafe(String value, double defaultValue) {
        if (value == null || value.isEmpty()) return defaultValue;
        try {
            return Double.parseDouble(value);
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    @Override
    public String getCategory() {
        return "GRAPH";
    }
}
