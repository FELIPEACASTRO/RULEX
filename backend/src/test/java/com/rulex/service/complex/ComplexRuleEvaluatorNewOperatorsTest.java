package com.rulex.service.complex;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.RuleCondition.ConditionOperator;
import com.rulex.entity.complex.RuleConditionGroup;
import com.rulex.entity.complex.RuleConditionGroup.GroupLogicOperator;
import com.rulex.service.FuzzyLogicService;
import com.rulex.service.GeoService;
import com.rulex.service.Neo4jGraphService;
import com.rulex.service.OperatorDataService;
import com.rulex.service.StatisticalAnalysisService;
import com.rulex.service.StringSimilarityService;
import com.rulex.service.VelocityService;
import com.rulex.service.VelocityServiceFacade;
import com.rulex.service.complex.evaluator.OperatorEvaluatorRegistry;
import java.math.BigDecimal;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

/**
 * Testes unitarios para operadores implementados (V28-V30). Foca em operadores que podem ser
 * testados sem dependencias externas.
 *
 * <p>Operadores testados: - IN_LIST - CONTAINS_SUSPICIOUS_KEYWORDS - IS_CRYPTO_RANSOM_AMOUNT
 *
 * @see ComplexRuleEvaluator
 */
@DisplayName("ComplexRuleEvaluator - Novos Operadores (V28-V30)")
class ComplexRuleEvaluatorNewOperatorsTest {

  private GeoService geoService;
  private VelocityService velocityService;
  private VelocityServiceFacade velocityServiceFacade;
  private OperatorDataService operatorDataService;
  private Neo4jGraphService neo4jGraphService;
  private StatisticalAnalysisService statisticalAnalysisService;
  private FuzzyLogicService fuzzyLogicService;
  private StringSimilarityService stringSimilarityService;
  private OperatorEvaluatorRegistry operatorEvaluatorRegistry;
  private ComplexRuleEvaluator evaluator;

  @BeforeEach
  void setUp() {
    geoService = Mockito.mock(GeoService.class);
    velocityService = Mockito.mock(VelocityService.class);
    velocityServiceFacade = Mockito.mock(VelocityServiceFacade.class);
    operatorDataService = Mockito.mock(OperatorDataService.class);
    neo4jGraphService = Mockito.mock(Neo4jGraphService.class);
    statisticalAnalysisService = Mockito.mock(StatisticalAnalysisService.class);
    fuzzyLogicService = Mockito.mock(FuzzyLogicService.class);
    stringSimilarityService = Mockito.mock(StringSimilarityService.class);
    // ARCH-001 FIX: Adicionado mock do OperatorEvaluatorRegistry
    operatorEvaluatorRegistry = new OperatorEvaluatorRegistry(Collections.emptyList());
    evaluator =
        new ComplexRuleEvaluator(
            geoService,
            velocityService,
            velocityServiceFacade,
            operatorDataService,
            neo4jGraphService,
            statisticalAnalysisService,
            fuzzyLogicService,
            stringSimilarityService,
            operatorEvaluatorRegistry);
  }

  // =====================================================
  // Helpers
  // =====================================================

  private RuleCondition createCondition(String field, ConditionOperator op, String value) {
    RuleCondition condition = new RuleCondition();
    condition.setFieldName(field);
    condition.setOperator(op);
    condition.setValueSingle(value);
    return condition;
  }

  private RuleConditionGroup createGroup(GroupLogicOperator logic, RuleCondition... conditions) {
    RuleConditionGroup group = new RuleConditionGroup();
    group.setLogicOperator(logic);
    group.setConditions(List.of(conditions));
    return group;
  }

  private ComplexRuleEvaluator.EvaluationContext createContext(Map<String, Object> payload) {
    return ComplexRuleEvaluator.EvaluationContext.builder().payload(payload).build();
  }

  // =====================================================
  // Testes: IN_LIST
  // =====================================================

  @Nested
  @DisplayName("IN_LIST - Verificacao de valor em lista")
  class InListTests {

    @Test
    @DisplayName("Deve retornar TRUE quando valor esta na lista")
    void inList_shouldReturnTrue_whenValueInList() {
      RuleCondition condition = createCondition("mcc", ConditionOperator.IN_LIST, "5411|5412|5413");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", "5412"));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("Deve retornar FALSE quando valor nao esta na lista")
    void inList_shouldReturnFalse_whenValueNotInList() {
      RuleCondition condition = createCondition("mcc", ConditionOperator.IN_LIST, "5411|5412|5413");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", "7995"));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isFalse();
    }

    @Test
    @DisplayName("Deve suportar lista vazia graciosamente")
    void inList_shouldHandleEmptyList() {
      RuleCondition condition = createCondition("mcc", ConditionOperator.IN_LIST, "");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", "5411"));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isFalse();
    }

    @Test
    @DisplayName("Deve ser case-insensitive para strings")
    void inList_shouldBeCaseInsensitive() {
      RuleCondition condition = createCondition("country", ConditionOperator.IN_LIST, "BR|US|UK");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("country", "br"));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("Deve suportar valores numericos na lista")
    void inList_shouldSupportNumericValues() {
      RuleCondition condition = createCondition("amount", ConditionOperator.IN_LIST, "100|200|300");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("amount", new BigDecimal("200")));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }
  }

  // =====================================================
  // Testes: CONTAINS_SUSPICIOUS_KEYWORDS
  // =====================================================

  @Nested
  @DisplayName("CONTAINS_SUSPICIOUS_KEYWORDS - Deteccao de palavras-chave")
  class ContainsSuspiciousKeywordsTests {

    @Test
    @DisplayName("Deve detectar keyword 'urgente' em descricao")
    void containsSuspiciousKeywords_shouldDetect_urgente() {
      RuleCondition condition =
          createCondition(
              "description", ConditionOperator.CONTAINS_SUSPICIOUS_KEYWORDS, "urgente|pix|agora");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("description", "Pagamento URGENTE favor confirmar"));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("Deve detectar multiplas keywords")
    void containsSuspiciousKeywords_shouldDetect_multipleKeywords() {
      RuleCondition condition =
          createCondition(
              "description",
              ConditionOperator.CONTAINS_SUSPICIOUS_KEYWORDS,
              "bitcoin|crypto|exchange");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("description", "Transfer to BITCOIN exchange wallet"));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("Deve retornar FALSE quando nenhuma keyword encontrada")
    void containsSuspiciousKeywords_shouldReturnFalse_whenNoMatch() {
      RuleCondition condition =
          createCondition(
              "description", ConditionOperator.CONTAINS_SUSPICIOUS_KEYWORDS, "ransomware|hacker");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("description", "Pagamento normal de servicos"));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isFalse();
    }

    @Test
    @DisplayName("Deve detectar keyword parcial (substring)")
    void containsSuspiciousKeywords_shouldDetect_partialMatch() {
      RuleCondition condition =
          createCondition("merchantName", ConditionOperator.CONTAINS_SUSPICIOUS_KEYWORDS, "casino");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("merchantName", "Royal Casino Online Gaming"));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("Deve ser case-insensitive")
    void containsSuspiciousKeywords_shouldBeCaseInsensitive() {
      RuleCondition condition =
          createCondition("description", ConditionOperator.CONTAINS_SUSPICIOUS_KEYWORDS, "URGENTE");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("description", "pagamento urgente necessario"));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }
  }

  // =====================================================
  // Testes: IS_CRYPTO_RANSOM_AMOUNT
  // =====================================================

  @Nested
  @DisplayName("IS_CRYPTO_RANSOM_AMOUNT - Deteccao de valores tipicos de ransom")
  class IsCryptoRansomAmountTests {

    @Test
    @DisplayName("Deve detectar valor exato de ransom comum (0.1 BTC em BRL)")
    void isCryptoRansomAmount_shouldDetect_exactRansomValue() {
      // 0.1 BTC aproximadamente R$ 35.000 (preco tipico)
      RuleCondition condition =
          createCondition("transactionAmount", ConditionOperator.IS_CRYPTO_RANSOM_AMOUNT, "BRL");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("transactionAmount", new BigDecimal("35000.00")));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("Deve detectar valor arredondado suspeito (50000)")
    void isCryptoRansomAmount_shouldDetect_roundSuspiciousValue() {
      RuleCondition condition =
          createCondition("transactionAmount", ConditionOperator.IS_CRYPTO_RANSOM_AMOUNT, "BRL");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("transactionAmount", new BigDecimal("50000.00")));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("Deve retornar FALSE para valor normal")
    void isCryptoRansomAmount_shouldReturnFalse_normalValue() {
      RuleCondition condition =
          createCondition("transactionAmount", ConditionOperator.IS_CRYPTO_RANSOM_AMOUNT, "BRL");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("transactionAmount", new BigDecimal("127.43")));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isFalse();
    }

    @Test
    @DisplayName("Deve detectar valor de ransom em USD")
    void isCryptoRansomAmount_shouldDetect_ransomValueUSD() {
      // $10,000 USD e valor comum de ransom
      RuleCondition condition =
          createCondition("transactionAmount", ConditionOperator.IS_CRYPTO_RANSOM_AMOUNT, "USD");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("transactionAmount", new BigDecimal("10000.00")));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("Deve detectar multiplos de BTC tipicos")
    void isCryptoRansomAmount_shouldDetect_btcMultiples() {
      // 0.5 BTC aproximadamente R$ 175.000
      RuleCondition condition =
          createCondition("transactionAmount", ConditionOperator.IS_CRYPTO_RANSOM_AMOUNT, "BRL");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("transactionAmount", new BigDecimal("175000.00")));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }
  }

  // =====================================================
  // Testes de Borda (Edge Cases)
  // =====================================================

  @Nested
  @DisplayName("Edge Cases - Valores nulos e invalidos")
  class EdgeCasesTests {

    @Test
    @DisplayName("IN_LIST deve lidar com valor null graciosamente")
    void inList_shouldHandle_nullValue() {
      RuleCondition condition = createCondition("mcc", ConditionOperator.IN_LIST, "5411|5412|5413");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of());

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isFalse();
    }

    @Test
    @DisplayName("Operador deve lidar com campo inexistente")
    void operator_shouldHandle_missingField() {
      RuleCondition condition =
          createCondition("nonExistentField", ConditionOperator.IN_LIST, "value1|value2");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", "5411"));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isFalse();
    }

    @Test
    @DisplayName("CONTAINS_SUSPICIOUS_KEYWORDS com campo null")
    void containsSuspiciousKeywords_shouldHandle_nullField() {
      RuleCondition condition =
          createCondition("description", ConditionOperator.CONTAINS_SUSPICIOUS_KEYWORDS, "test");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", "5411"));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isFalse();
    }

    @Test
    @DisplayName("IS_CRYPTO_RANSOM_AMOUNT com valor zero")
    void isCryptoRansomAmount_shouldHandle_zeroValue() {
      RuleCondition condition =
          createCondition("transactionAmount", ConditionOperator.IS_CRYPTO_RANSOM_AMOUNT, "BRL");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("transactionAmount", BigDecimal.ZERO));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isFalse();
    }

    @Test
    @DisplayName("IN_LIST deve tratar delimitador pipe corretamente")
    void inList_shouldHandle_pipeDelimiter() {
      RuleCondition condition =
          createCondition("status", ConditionOperator.IN_LIST, "APPROVED|PENDING|REVIEW");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("status", "PENDING"));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }
  }

  // =====================================================
  // Testes de Integracao de Operadores
  // =====================================================

  @Nested
  @DisplayName("Integracao - Multiplos operadores em grupo")
  class IntegrationTests {

    @Test
    @DisplayName("AND com IN_LIST e GT")
    void andGroup_withInListAndGt() {
      RuleCondition inListCondition =
          createCondition("mcc", ConditionOperator.IN_LIST, "5411|5412");
      RuleCondition gtCondition =
          createCondition("transactionAmount", ConditionOperator.GT, "1000");

      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, inListCondition, gtCondition);
      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("mcc", "5411", "transactionAmount", new BigDecimal("1500")));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("OR com IN_LIST e CONTAINS_SUSPICIOUS_KEYWORDS")
    void orGroup_withMultipleOperators() {
      RuleCondition inListCondition =
          createCondition("mcc", ConditionOperator.IN_LIST, "7995|7994"); // gambling
      RuleCondition keywordsCondition =
          createCondition("description", ConditionOperator.CONTAINS_SUSPICIOUS_KEYWORDS, "casino");

      RuleConditionGroup group =
          createGroup(GroupLogicOperator.OR, inListCondition, keywordsCondition);
      ComplexRuleEvaluator.EvaluationContext context =
          createContext(
              Map.of(
                  "mcc", "5411", // Not gambling
                  "description", "Royal Casino Payment"));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      // MCC nao bate, mas description contem casino
      assertThat(result.isMatched()).isTrue();
    }
  }
}
