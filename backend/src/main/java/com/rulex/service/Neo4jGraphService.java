package com.rulex.service;

import java.time.Duration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;
import org.neo4j.driver.AuthTokens;
import org.neo4j.driver.Config;
import org.neo4j.driver.Driver;
import org.neo4j.driver.GraphDatabase;
import org.neo4j.driver.Record;
import org.neo4j.driver.Result;
import org.neo4j.driver.Session;
import org.neo4j.driver.SessionConfig;
import org.neo4j.driver.TransactionConfig;
import org.springframework.stereotype.Service;

/**
 * Serviço de integração com Neo4j para detecção de fraude baseada em grafos. Implementa os
 * operadores NEO001-NEO018 para análise de fraud rings, money mules e redes de fraude.
 */
@Service
@Slf4j
public class Neo4jGraphService {

  private final Driver driver;
  private final boolean enabled;
  private volatile boolean available;
  private final String uri;
  private final long queryTimeoutMs;

  public Neo4jGraphService(
      @org.springframework.beans.factory.annotation.Value(
              "${rulex.neo4j.uri:bolt://localhost:7687}")
          String uri,
      @org.springframework.beans.factory.annotation.Value("${rulex.neo4j.username:neo4j}")
          String username,
      @org.springframework.beans.factory.annotation.Value("${rulex.neo4j.password:}")
          String password,
      @org.springframework.beans.factory.annotation.Value("${rulex.neo4j.enabled:true}")
        boolean enabled,
      @org.springframework.beans.factory.annotation.Value("${rulex.neo4j.connection-timeout-ms:2000}")
        long connectionTimeoutMs,
      @org.springframework.beans.factory.annotation.Value("${rulex.neo4j.acquire-timeout-ms:2000}")
        long acquisitionTimeoutMs,
      @org.springframework.beans.factory.annotation.Value("${rulex.neo4j.max-pool-size:30}")
        int maxPoolSize,
      @org.springframework.beans.factory.annotation.Value("${rulex.neo4j.max-connection-lifetime-ms:600000}")
        long maxConnectionLifetimeMs,
      @org.springframework.beans.factory.annotation.Value("${rulex.neo4j.query-timeout-ms:2000}")
        long queryTimeoutMs) {
    boolean resolvedEnabled = enabled;
    if (enabled && (password == null || password.isBlank())) {
      resolvedEnabled = false;
      log.warn(
          "Neo4j integration disabled: missing rulex.neo4j.password. Provide RULEX_NEO4J_PASSWORD to enable.");
    }
    this.enabled = resolvedEnabled;
    this.uri = uri;
    this.queryTimeoutMs = queryTimeoutMs;

    if (resolvedEnabled) {
      Driver tempDriver = null;
      boolean tempAvailable = false;
      try {
        Config config =
            Config.builder()
                .withConnectionTimeout(Duration.ofMillis(connectionTimeoutMs))
                .withConnectionAcquisitionTimeout(Duration.ofMillis(acquisitionTimeoutMs))
                .withMaxConnectionPoolSize(maxPoolSize)
                .withMaxConnectionLifetime(Duration.ofMillis(maxConnectionLifetimeMs))
                .build();
        tempDriver = GraphDatabase.driver(uri, AuthTokens.basic(username, password), config);
        // Testar conexão sem bloquear startup
        tempDriver.verifyConnectivity();
        tempAvailable = true;
        log.info("Neo4j driver initialized and connected: {}", uri);
      } catch (Exception e) {
        // DEGRADAÇÃO GRACIOSA: não quebrar startup, apenas marcar como indisponível
        log.warn(
            "⚠️ Neo4j não disponível na inicialização: {}. "
                + "Operadores de grafo retornarão valores conservadores. "
                + "Reconexão será tentada automaticamente.",
            e.getMessage());
        tempAvailable = false;
        // Manter o driver para tentativas futuras (se foi criado)
      }
      this.driver = tempDriver;
      this.available = tempAvailable;
    } else {
      this.driver = null;
      this.available = false;
      log.info("Neo4j integration disabled by configuration");
    }
  }

  /** NEO001: Weakly Connected Components - Detecta clusters de contas conectadas */
  public int getWeaklyConnectedComponentId(String accountId) {
    if (!enabled || driver == null) return -1;

    String query =
        """
        MATCH (a:Account {id: $accountId})
        CALL gds.wcc.stream('account-graph')
        YIELD nodeId, componentId
        WHERE gds.util.asNode(nodeId).id = $accountId
        RETURN componentId
        """;

    try (Session session = driver.session(SessionConfig.defaultConfig())) {
      Result result = run(session, query, Map.of("accountId", accountId));
      if (result.hasNext()) {
        return result.next().get("componentId").asInt();
      }
    } catch (Exception e) {
      log.debug("WCC query failed (graph may not exist): {}", e.getMessage());
    }
    return -1;
  }

  /** NEO002: Degree Centrality - Conta conexões de uma conta */
  public int getDegreeCentrality(String accountId) {
    if (!enabled || driver == null) return 0;

    String query =
        """
        MATCH (a:Account {id: $accountId})-[r]-()
        RETURN count(r) as degree
        """;

    try (Session session = driver.session()) {
      Result result = run(session, query, Map.of("accountId", accountId));
      if (result.hasNext()) {
        return result.next().get("degree").asInt();
      }
    } catch (Exception e) {
      log.debug("Degree centrality query failed: {}", e.getMessage());
    }
    return 0;
  }

  /** NEO003: PageRank Fraud Score - Score baseado em importância no grafo */
  public double getPageRankScore(String accountId) {
    if (!enabled || driver == null) return 0.0;

    String query =
        """
        MATCH (a:Account {id: $accountId})
        RETURN a.pageRankScore as score
        """;

    try (Session session = driver.session()) {
      Result result = run(session, query, Map.of("accountId", accountId));
      if (result.hasNext()) {
        org.neo4j.driver.Value score = result.next().get("score");
        return score.isNull() ? 0.0 : score.asDouble();
      }
    } catch (Exception e) {
      log.debug("PageRank query failed: {}", e.getMessage());
    }
    return 0.0;
  }

  /** NEO004: Louvain Community Detection - Detecta comunidades de fraude */
  public int getLouvainCommunityId(String accountId) {
    if (!enabled || driver == null) return -1;

    String query =
        """
        MATCH (a:Account {id: $accountId})
        RETURN a.communityId as communityId
        """;

    try (Session session = driver.session()) {
      Result result = run(session, query, Map.of("accountId", accountId));
      if (result.hasNext()) {
        org.neo4j.driver.Value communityId = result.next().get("communityId");
        return communityId.isNull() ? -1 : communityId.asInt();
      }
    } catch (Exception e) {
      log.debug("Louvain query failed: {}", e.getMessage());
    }
    return -1;
  }

  /** NEO005: Pairwise Similarity PII - Similaridade de PII entre contas */
  public double getPairwiseSimilarity(String accountId1, String accountId2) {
    if (!enabled || driver == null) return 0.0;

    String query =
        """
        MATCH (a1:Account {id: $accountId1}), (a2:Account {id: $accountId2})
        WITH a1, a2,
             CASE WHEN a1.email = a2.email THEN 1 ELSE 0 END +
             CASE WHEN a1.phone = a2.phone THEN 1 ELSE 0 END +
             CASE WHEN a1.address = a2.address THEN 1 ELSE 0 END +
             CASE WHEN a1.ssn = a2.ssn THEN 1 ELSE 0 END as matches
        RETURN matches / 4.0 as similarity
        """;

    try (Session session = driver.session()) {
      Result result =
          run(session, query, Map.of("accountId1", accountId1, "accountId2", accountId2));
      if (result.hasNext()) {
        return result.next().get("similarity").asDouble();
      }
    } catch (Exception e) {
      log.debug("Pairwise similarity query failed: {}", e.getMessage());
    }
    return 0.0;
  }

  /** NEO006: Entity Resolution Shared PII - Encontra contas com PII compartilhado */
  public List<String> findAccountsWithSharedPii(String accountId) {
    if (!enabled || driver == null) return List.of();

    String query =
        """
        MATCH (a:Account {id: $accountId})-[:SHARES_PII]-(other:Account)
        RETURN collect(other.id) as sharedAccounts
        """;

    try (Session session = driver.session()) {
      Result result = run(session, query, Map.of("accountId", accountId));
      if (result.hasNext()) {
        return result.next().get("sharedAccounts").asList(org.neo4j.driver.Value::asString);
      }
    } catch (Exception e) {
      log.debug("Shared PII query failed: {}", e.getMessage());
    }
    return List.of();
  }

  /** NEO007: Fraud Ring Detection - Detecta anéis de fraude */
  public boolean isInFraudRing(String accountId, int minRingSize) {
    if (!enabled || driver == null) return false;

    String query =
        """
        MATCH (a:Account {id: $accountId})
        MATCH path = (a)-[:TRANSFERRED_TO*2..5]-(a)
        WITH length(path) as ringSize
        WHERE ringSize >= $minRingSize
        RETURN count(*) > 0 as inRing
        """;

    try (Session session = driver.session()) {
      Result result =
          run(session, query, Map.of("accountId", accountId, "minRingSize", minRingSize));
      if (result.hasNext()) {
        return result.next().get("inRing").asBoolean();
      }
    } catch (Exception e) {
      log.debug("Fraud ring query failed: {}", e.getMessage());
    }
    return false;
  }

  /** NEO008: Money Mule Network Analysis - Analisa rede de money mules */
  public Map<String, Object> analyzeMoneyMuleNetwork(String sourceAccountId, int maxHops) {
    if (!enabled || driver == null) return Map.of();

    String query =
        """
        MATCH path = (source:Account {id: $sourceId})-[:TRANSFERRED_TO*1..%d]->(dest:Account)
        WHERE dest.riskLevel = 'HIGH' OR dest.country IN ['NG', 'PK', 'RU']
        WITH path, length(path) as hops, dest
        RETURN count(DISTINCT dest) as suspiciousDestinations,
               avg(hops) as avgHops,
               max(hops) as maxHops
        """
            .formatted(maxHops);

    try (Session session = driver.session()) {
      Result result = run(session, query, Map.of("sourceId", sourceAccountId));
      if (result.hasNext()) {
        Record record = result.next();
        Map<String, Object> analysis = new HashMap<>();
        analysis.put("suspiciousDestinations", record.get("suspiciousDestinations").asInt());
        analysis.put("avgHops", record.get("avgHops").asDouble());
        analysis.put("maxHops", record.get("maxHops").asInt());
        return analysis;
      }
    } catch (Exception e) {
      log.debug("Money mule analysis failed: {}", e.getMessage());
    }
    return Map.of();
  }

  /** NEO009: Circular Transaction Detection - Detecta transações circulares */
  public boolean hasCircularTransactions(String accountId, int minCycleLength) {
    if (!enabled || driver == null) return false;

    String query =
        """
        MATCH (a:Account {id: $accountId})
        MATCH path = (a)-[:TRANSFERRED_TO*%d..10]->(a)
        RETURN count(path) > 0 as hasCircular
        """
            .formatted(minCycleLength);

    try (Session session = driver.session()) {
      Result result = run(session, query, Map.of("accountId", accountId));
      if (result.hasNext()) {
        return result.next().get("hasCircular").asBoolean();
      }
    } catch (Exception e) {
      log.debug("Circular transaction query failed: {}", e.getMessage());
    }
    return false;
  }

  /** NEO010: First Party Fraud Clustering - Clustering de fraude de primeira parte */
  public int getFirstPartyFraudClusterId(String accountId) {
    if (!enabled || driver == null) return -1;

    String query =
        """
        MATCH (a:Account {id: $accountId})
        WHERE a.firstPartyFraudCluster IS NOT NULL
        RETURN a.firstPartyFraudCluster as clusterId
        """;

    try (Session session = driver.session()) {
      Result result = run(session, query, Map.of("accountId", accountId));
      if (result.hasNext()) {
        org.neo4j.driver.Value clusterId = result.next().get("clusterId");
        return clusterId.isNull() ? -1 : clusterId.asInt();
      }
    } catch (Exception e) {
      log.debug("First party fraud cluster query failed: {}", e.getMessage());
    }
    return -1;
  }

  /** NEO011: Second Level Fraudster Identification - Identifica fraudadores de segundo nível */
  public boolean isSecondLevelFraudster(String accountId) {
    if (!enabled || driver == null) return false;

    String query =
        """
        MATCH (a:Account {id: $accountId})-[:TRANSFERRED_TO|RECEIVED_FROM]-(known:Account)
        WHERE known.isFraudster = true
        RETURN count(known) > 0 as isSecondLevel
        """;

    try (Session session = driver.session()) {
      Result result = run(session, query, Map.of("accountId", accountId));
      if (result.hasNext()) {
        return result.next().get("isSecondLevel").asBoolean();
      }
    } catch (Exception e) {
      log.debug("Second level fraudster query failed: {}", e.getMessage());
    }
    return false;
  }

  /** NEO012: Betweenness Centrality Mule Detection - Detecta mules por centralidade */
  public double getBetweennessCentrality(String accountId) {
    if (!enabled || driver == null) return 0.0;

    String query =
        """
        MATCH (a:Account {id: $accountId})
        RETURN a.betweennessCentrality as centrality
        """;

    try (Session session = driver.session()) {
      Result result = run(session, query, Map.of("accountId", accountId));
      if (result.hasNext()) {
        org.neo4j.driver.Value centrality = result.next().get("centrality");
        return centrality.isNull() ? 0.0 : centrality.asDouble();
      }
    } catch (Exception e) {
      log.debug("Betweenness centrality query failed: {}", e.getMessage());
    }
    return 0.0;
  }

  /** NEO013: Label Propagation Fraud Spread - Propagação de labels de fraude */
  public String getFraudLabel(String accountId) {
    if (!enabled || driver == null) return "UNKNOWN";

    String query =
        """
        MATCH (a:Account {id: $accountId})
        RETURN a.fraudLabel as label
        """;

    try (Session session = driver.session()) {
      Result result = run(session, query, Map.of("accountId", accountId));
      if (result.hasNext()) {
        org.neo4j.driver.Value label = result.next().get("label");
        return label.isNull() ? "UNKNOWN" : label.asString();
      }
    } catch (Exception e) {
      log.debug("Fraud label query failed: {}", e.getMessage());
    }
    return "UNKNOWN";
  }

  /** NEO014: Shortest Path AML Tracking - Caminho mais curto para AML */
  public int getShortestPathToHighRisk(String accountId) {
    if (!enabled || driver == null) return -1;

    String query =
        """
        MATCH (source:Account {id: $accountId}), (target:Account {riskLevel: 'HIGH'})
        MATCH path = shortestPath((source)-[:TRANSFERRED_TO*]-(target))
        RETURN length(path) as pathLength
        ORDER BY pathLength
        LIMIT 1
        """;

    try (Session session = driver.session()) {
      Result result = run(session, query, Map.of("accountId", accountId));
      if (result.hasNext()) {
        return result.next().get("pathLength").asInt();
      }
    } catch (Exception e) {
      log.debug("Shortest path query failed: {}", e.getMessage());
    }
    return -1;
  }

  /** NEO015: Triangle Count Collusion Detection - Detecta colusão por triângulos */
  public int getTriangleCount(String accountId) {
    if (!enabled || driver == null) return 0;

    String query =
        """
        MATCH (a:Account {id: $accountId})-[:TRANSFERRED_TO]-(b:Account)-[:TRANSFERRED_TO]-(c:Account)-[:TRANSFERRED_TO]-(a)
        RETURN count(*) / 6 as triangles
        """;

    try (Session session = driver.session()) {
      Result result = run(session, query, Map.of("accountId", accountId));
      if (result.hasNext()) {
        return result.next().get("triangles").asInt();
      }
    } catch (Exception e) {
      log.debug("Triangle count query failed: {}", e.getMessage());
    }
    return 0;
  }

  /** NEO016: Node Similarity Synthetic ID - Similaridade de nós para ID sintético */
  public double getNodeSimilarity(String accountId1, String accountId2) {
    if (!enabled || driver == null) return 0.0;

    String query =
        """
        MATCH (a1:Account {id: $accountId1})-[:TRANSFERRED_TO]-(common)-[:TRANSFERRED_TO]-(a2:Account {id: $accountId2})
        WITH a1, a2, count(common) as intersection
        MATCH (a1)-[:TRANSFERRED_TO]-(n1)
        WITH a1, a2, intersection, count(n1) as a1Neighbors
        MATCH (a2)-[:TRANSFERRED_TO]-(n2)
        WITH intersection, a1Neighbors, count(n2) as a2Neighbors
        RETURN intersection * 1.0 / (a1Neighbors + a2Neighbors - intersection) as similarity
        """;

    try (Session session = driver.session()) {
      Result result =
          run(session, query, Map.of("accountId1", accountId1, "accountId2", accountId2));
      if (result.hasNext()) {
        org.neo4j.driver.Value similarity = result.next().get("similarity");
        return similarity.isNull() ? 0.0 : similarity.asDouble();
      }
    } catch (Exception e) {
      log.debug("Node similarity query failed: {}", e.getMessage());
    }
    return 0.0;
  }

  /** NEO017: Graph Embedding Fraud Prediction - Predição de fraude por embedding */
  public double getFraudProbabilityFromEmbedding(String accountId) {
    if (!enabled || driver == null) return 0.0;

    String query =
        """
        MATCH (a:Account {id: $accountId})
        WHERE a.fraudEmbeddingScore IS NOT NULL
        RETURN a.fraudEmbeddingScore as score
        """;

    try (Session session = driver.session()) {
      Result result = run(session, query, Map.of("accountId", accountId));
      if (result.hasNext()) {
        org.neo4j.driver.Value score = result.next().get("score");
        return score.isNull() ? 0.0 : score.asDouble();
      }
    } catch (Exception e) {
      log.debug("Fraud embedding query failed: {}", e.getMessage());
    }
    return 0.0;
  }

  /** NEO018: Temporal Motif Transaction Pattern - Padrões temporais de transação */
  public int getTemporalMotifCount(String accountId, String motifPattern) {
    if (!enabled || driver == null) return 0;

    String query =
        """
        MATCH (a:Account {id: $accountId})
        MATCH path = (a)-[:TRANSFERRED_TO*2..4]->()
        WHERE all(r IN relationships(path) WHERE r.timestamp IS NOT NULL)
        WITH path, [r IN relationships(path) | r.timestamp] as timestamps
        WHERE reduce(valid = true, i IN range(0, size(timestamps)-2) |
              valid AND timestamps[i+1] > timestamps[i])
        RETURN count(path) as motifCount
        """;

    try (Session session = driver.session()) {
      Result result = run(session, query, Map.of("accountId", accountId));
      if (result.hasNext()) {
        return result.next().get("motifCount").asInt();
      }
    } catch (Exception e) {
      log.debug("Temporal motif query failed: {}", e.getMessage());
    }
    return 0;
  }

  /** Registra uma transação no grafo */
  public void recordTransaction(
      String fromAccountId, String toAccountId, double amount, long timestamp) {
    if (!enabled || driver == null) return;

    String query =
        """
        MERGE (from:Account {id: $fromId})
        MERGE (to:Account {id: $toId})
        CREATE (from)-[:TRANSFERRED_TO {amount: $amount, timestamp: $timestamp}]->(to)
        """;

    try (Session session = driver.session()) {
      run(
        session,
        query,
        Map.of(
          "fromId",
          fromAccountId,
          "toId",
          toAccountId,
          "amount",
          amount,
          "timestamp",
          timestamp));
    } catch (Exception e) {
      log.warn("Failed to record transaction in Neo4j: {}", e.getMessage());
    }
  }

  /** Verifica se o serviço está habilitado e conectado. Tenta reconectar se estava indisponível. */
  public boolean isAvailable() {
    if (!enabled || driver == null) return false;

    // Se já está marcado como disponível, verificar se ainda está
    if (available) {
      try (Session session = driver.session()) {
        run(session, "RETURN 1", Map.of());
        return true;
      } catch (Exception e) {
        log.warn("Neo4j connection lost: {}", e.getMessage());
        available = false;
        return false;
      }
    }

    // Tentar reconectar se estava indisponível
    try (Session session = driver.session()) {
      run(session, "RETURN 1", Map.of());
      available = true;
      log.info("Neo4j reconnected successfully: {}", uri);
      return true;
    } catch (Exception e) {
      log.debug("Neo4j still unavailable: {}", e.getMessage());
      return false;
    }
  }

  /** Retorna se o serviço está habilitado na configuração. */
  public boolean isEnabled() {
    return enabled;
  }

  /** Retorna o status atual de disponibilidade (sem verificar conexão). */
  public boolean isCurrentlyAvailable() {
    return enabled && available;
  }

  /** Fecha a conexão com o Neo4j */
  public void close() {
    if (driver != null) {
      try {
        driver.close();
      } catch (Exception e) {
        log.warn("Error closing Neo4j driver: {}", e.getMessage());
      }
    }
  }

  private Result run(Session session, String query, Map<String, Object> params) {
    TransactionConfig txConfig =
        TransactionConfig.builder().withTimeout(Duration.ofMillis(queryTimeoutMs)).build();
    return session.run(query, params, txConfig);
  }
}
