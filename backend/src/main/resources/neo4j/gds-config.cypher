// =====================================================================
// RULEX Neo4j GDS (Graph Data Science) Configuration Script
// V5.0: Fraud Ring Detection Algorithms
// =====================================================================
// Execute este script APÓS o init.cypher e quando houver dados no grafo
// =====================================================================

// =====================================================================
// 1. DROP EXISTING PROJECTED GRAPHS (se existirem)
// =====================================================================

// Verificar grafos existentes
CALL gds.graph.list()
YIELD graphName
RETURN graphName;

// Dropar grafo existente (descomentar se necessário)
// CALL gds.graph.drop('fraud_detection_graph', false) YIELD graphName;

// =====================================================================
// 2. CREATE PROJECTED GRAPH FOR FRAUD DETECTION
// =====================================================================

// Projetar grafo para análise de fraude
// Inclui: Account nodes, Merchant nodes, TRANSACTED_WITH relationships
CALL gds.graph.project(
  'fraud_detection_graph',
  ['Account', 'Merchant'],
  {
    TRANSACTED_WITH: {
      type: 'TRANSACTED_WITH',
      orientation: 'UNDIRECTED',
      properties: {
        amount: {
          property: 'amount',
          defaultValue: 0.0
        },
        timestamp: {
          property: 'timestamp',
          defaultValue: 0
        }
      }
    }
  }
)
YIELD graphName, nodeCount, relationshipCount, projectMillis;

// =====================================================================
// 3. VERIFY PROJECTED GRAPH
// =====================================================================

CALL gds.graph.list('fraud_detection_graph')
YIELD graphName, nodeCount, relationshipCount, memoryUsage, creationTime
RETURN graphName, nodeCount, relationshipCount, memoryUsage;

// =====================================================================
// 4. TEST GDS ALGORITHMS (Optional - descomentar para testar)
// =====================================================================

// -- Weakly Connected Components (WCC) --
// Identifica componentes conectados (potenciais fraud rings)
// CALL gds.wcc.stream('fraud_detection_graph')
// YIELD nodeId, componentId
// RETURN gds.util.asNode(nodeId).id AS entityId, componentId
// ORDER BY componentId, entityId
// LIMIT 100;

// -- PageRank --
// Identifica nodes mais influentes na rede
// CALL gds.pageRank.stream('fraud_detection_graph')
// YIELD nodeId, score
// RETURN gds.util.asNode(nodeId).id AS entityId, score
// ORDER BY score DESC
// LIMIT 20;

// -- Louvain Community Detection --
// Detecta comunidades/clusters de contas relacionadas
// CALL gds.louvain.stream('fraud_detection_graph')
// YIELD nodeId, communityId
// RETURN gds.util.asNode(nodeId).id AS entityId, communityId
// ORDER BY communityId, entityId
// LIMIT 100;

// -- Betweenness Centrality --
// Identifica nodes que são "pontes" entre grupos
// CALL gds.betweenness.stream('fraud_detection_graph')
// YIELD nodeId, score
// RETURN gds.util.asNode(nodeId).id AS entityId, score
// ORDER BY score DESC
// LIMIT 20;

// =====================================================================
// 5. STATISTICS
// =====================================================================

// Estatísticas do grafo projetado
CALL gds.graph.list('fraud_detection_graph')
YIELD graphName, degreeDistribution
RETURN graphName, 
       degreeDistribution.min AS minDegree,
       degreeDistribution.max AS maxDegree,
       degreeDistribution.mean AS avgDegree;

// =====================================================================
// END OF GDS CONFIGURATION SCRIPT
// =====================================================================
