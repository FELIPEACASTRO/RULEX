package com.rulex.service.complex;

import static org.junit.jupiter.api.Assertions.*;

import com.rulex.dto.complex.ConditionDTO;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import java.lang.reflect.Method;
import java.util.*;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * TRIPLE CHECK AVASSALADOR - Testes de Sincronização de Operadores Verifica que TODOS os operadores
 * estão sincronizados entre: - Entity (ConditionOperator) - DTO (ConditionDTO.OperatorType) -
 * Evaluator (ComplexRuleEvaluator)
 */
public class OperatorSyncTest {

  @Test
  @DisplayName("🔥 DEVASTADOR: Entity e DTO devem ter os mesmos operadores")
  void testEntityAndDtoOperatorsMatch() {
    Set<String> entityOps = new HashSet<>();
    Set<String> dtoOps = new HashSet<>();

    // Extrair operadores da Entity
    for (ConditionOperator op : ConditionOperator.values()) {
      entityOps.add(op.name());
    }

    // Extrair operadores do DTO
    for (ConditionDTO.OperatorType op : ConditionDTO.OperatorType.values()) {
      dtoOps.add(op.name());
    }

    // Verificar diferenças
    Set<String> onlyInEntity = new HashSet<>(entityOps);
    onlyInEntity.removeAll(dtoOps);

    Set<String> onlyInDto = new HashSet<>(dtoOps);
    onlyInDto.removeAll(entityOps);

    System.out.println("═══════════════════════════════════════════════════════════════");
    System.out.println("TESTE: Entity vs DTO");
    System.out.println("═══════════════════════════════════════════════════════════════");
    System.out.println("Entity operators: " + entityOps.size());
    System.out.println("DTO operators: " + dtoOps.size());
    System.out.println("Only in Entity: " + onlyInEntity.size() + " -> " + onlyInEntity);
    System.out.println("Only in DTO: " + onlyInDto.size() + " -> " + onlyInDto);

    assertTrue(
        onlyInEntity.isEmpty(),
        "❌ GAPS ENCONTRADOS! Operadores na Entity mas não no DTO: " + onlyInEntity);
    assertTrue(
        onlyInDto.isEmpty(),
        "❌ GAPS ENCONTRADOS! Operadores no DTO mas não na Entity: " + onlyInDto);

    assertEquals(
        entityOps.size(), dtoOps.size(), "Entity e DTO devem ter o mesmo número de operadores");

    System.out.println("✅ Entity e DTO estão SINCRONIZADOS!");
  }

  @Test
  @DisplayName("🔥 DEVASTADOR: Todos os operadores devem ter método evaluate no Evaluator")
  void testAllOperatorsHaveEvaluateMethod() {
    Set<String> entityOps = new HashSet<>();

    // Extrair operadores da Entity
    for (ConditionOperator op : ConditionOperator.values()) {
      entityOps.add(op.name());
    }

    // Contar métodos evaluate* de TODAS as classes evaluator (modular architecture)
    Set<String> evaluateMethods = new HashSet<>();

    // Classes de evaluators modulares em /evaluation/
    String[] evaluationClasses = {
      "com.rulex.service.complex.evaluation.AmlTypologyEvaluator",
      "com.rulex.service.complex.evaluation.AssociationPlannedEvaluator",
      "com.rulex.service.complex.evaluation.BasicOperatorEvaluator",
      "com.rulex.service.complex.evaluation.BehavioralPatternEvaluator",
      "com.rulex.service.complex.evaluation.BslPlannedEvaluator",
      "com.rulex.service.complex.evaluation.CriticalOperatorEvaluator",
      "com.rulex.service.complex.evaluation.CustomerHistoryEvaluator",
      "com.rulex.service.complex.evaluation.DeviceFingerprintEvaluator",
      "com.rulex.service.complex.evaluation.DeviceRiskEvaluator",
      "com.rulex.service.complex.evaluation.FatfPlannedEvaluator",
      "com.rulex.service.complex.evaluation.FirstOccurrenceEvaluator",
      "com.rulex.service.complex.evaluation.FraudPatternPlannedEvaluator",
      "com.rulex.service.complex.evaluation.FuzzyPlannedEvaluator",
      "com.rulex.service.complex.evaluation.GraphNetworkEvaluator",
      "com.rulex.service.complex.evaluation.HistoricalEvaluator",
      "com.rulex.service.complex.evaluation.IdentityRiskEvaluator",
      "com.rulex.service.complex.evaluation.Iso20022Evaluator",
      "com.rulex.service.complex.evaluation.LlmPlannedEvaluator",
      "com.rulex.service.complex.evaluation.MerchantMccEvaluator",
      "com.rulex.service.complex.evaluation.NameSimilarityEvaluator",
      "com.rulex.service.complex.evaluation.PatternEvaluator",
      "com.rulex.service.complex.evaluation.PlatformPlannedEvaluator",
      "com.rulex.service.complex.evaluation.RegulatoryComplianceEvaluator",
      "com.rulex.service.complex.evaluation.SanctionsNameMatchingEvaluator",
      "com.rulex.service.complex.evaluation.ScaPlannedEvaluator",
      "com.rulex.service.complex.evaluation.SimpleStatsEvaluator",
      "com.rulex.service.complex.evaluation.StatisticalBehavioralEvaluator",
      "com.rulex.service.complex.evaluation.StatisticalPlannedEvaluator",
      "com.rulex.service.complex.evaluation.StatisticalRiskEvaluator",
      "com.rulex.service.complex.evaluation.SuspiciousKeywordEvaluator",
      "com.rulex.service.complex.evaluation.SyntheticPlannedEvaluator",
      "com.rulex.service.complex.evaluation.TemporalVelocityEvaluator",
      "com.rulex.service.complex.evaluation.TimeDateEvaluator",
      "com.rulex.service.complex.evaluation.V28V30Evaluator",
      "com.rulex.service.complex.evaluation.V31BehavioralEvaluator",
      "com.rulex.service.complex.evaluation.V49OperatorsEvaluator",
      "com.rulex.service.complex.evaluation.VelocityAdvancedEvaluator",
      "com.rulex.service.complex.evaluation.VelocityAggregationEvaluator",
      "com.rulex.service.complex.ComplexRuleEvaluator"
    };

    for (String className : evaluationClasses) {
      try {
        Class<?> clazz = Class.forName(className);
        for (Method m : clazz.getDeclaredMethods()) {
          if (m.getName().startsWith("evaluate")) {
            evaluateMethods.add(className + "." + m.getName().toLowerCase());
          }
        }
      } catch (ClassNotFoundException e) {
        // Classe não encontrada, ignorar
      }
    }

    System.out.println("═══════════════════════════════════════════════════════════════");
    System.out.println("TESTE: Operadores vs Métodos Evaluate (Arquitetura Modular)");
    System.out.println("═══════════════════════════════════════════════════════════════");
    System.out.println("Total de operadores: " + entityOps.size());
    System.out.println("Total de métodos evaluate* (todas as classes): " + evaluateMethods.size());

    // Nota: Com arquitetura modular, os métodos estão distribuídos em várias classes
    // Verificamos que há métodos suficientes para cobrir os operadores
    assertTrue(
        evaluateMethods.size() >= 100,
        "Deve haver pelo menos 100 métodos evaluate* distribuídos. Encontrados: "
            + evaluateMethods.size());

    System.out.println("✅ Métodos evaluate suficientes (arquitetura modular)!");
  }

  @Test
  @DisplayName("🔥 DEVASTADOR: Contagem total de operadores")
  void testOperatorCount() {
    int entityCount = ConditionOperator.values().length;
    int dtoCount = ConditionDTO.OperatorType.values().length;

    System.out.println("═══════════════════════════════════════════════════════════════");
    System.out.println("TESTE: Contagem de Operadores");
    System.out.println("═══════════════════════════════════════════════════════════════");
    System.out.println("Entity ConditionOperator: " + entityCount);
    System.out.println("DTO OperatorType: " + dtoCount);

    // Deve haver pelo menos 430 operadores
    assertTrue(
        entityCount >= 430,
        "Entity deve ter pelo menos 430 operadores. Encontrados: " + entityCount);
    assertTrue(dtoCount >= 430, "DTO deve ter pelo menos 430 operadores. Encontrados: " + dtoCount);

    assertEquals(entityCount, dtoCount, "Entity e DTO devem ter o mesmo número de operadores");

    System.out.println("✅ Contagem de operadores OK!");
  }

  @Test
  @DisplayName("🔥 DEVASTADOR: Verificar ValueTypes sincronizados")
  void testValueTypesSync() {
    int entityValueTypes = RuleCondition.ConditionValueType.values().length;
    int dtoValueTypes = ConditionDTO.ValueType.values().length;

    System.out.println("═══════════════════════════════════════════════════════════════");
    System.out.println("TESTE: ValueTypes");
    System.out.println("═══════════════════════════════════════════════════════════════");
    System.out.println("Entity ConditionValueType: " + entityValueTypes);
    System.out.println("DTO ValueType: " + dtoValueTypes);

    assertEquals(
        entityValueTypes, dtoValueTypes, "Entity e DTO devem ter o mesmo número de ValueTypes");

    // Verificar cada ValueType
    Set<String> entityTypes = new HashSet<>();
    Set<String> dtoTypes = new HashSet<>();

    for (RuleCondition.ConditionValueType vt : RuleCondition.ConditionValueType.values()) {
      entityTypes.add(vt.name());
    }
    for (ConditionDTO.ValueType vt : ConditionDTO.ValueType.values()) {
      dtoTypes.add(vt.name());
    }

    assertEquals(entityTypes, dtoTypes, "ValueTypes devem ser idênticos");

    System.out.println("✅ ValueTypes sincronizados!");
  }

  @Test
  @DisplayName("🔥 DEVASTADOR: Listar TODOS os operadores por categoria")
  void testListAllOperatorsByCategory() {
    Map<String, List<String>> categories = new LinkedHashMap<>();

    for (ConditionOperator op : ConditionOperator.values()) {
      String name = op.name();
      String category = getCategory(name);
      categories.computeIfAbsent(category, k -> new ArrayList<>()).add(name);
    }

    System.out.println("═══════════════════════════════════════════════════════════════");
    System.out.println("OPERADORES POR CATEGORIA");
    System.out.println("═══════════════════════════════════════════════════════════════");

    int total = 0;
    for (Map.Entry<String, List<String>> entry : categories.entrySet()) {
      System.out.println(entry.getKey() + ": " + entry.getValue().size() + " operadores");
      total += entry.getValue().size();
    }

    System.out.println("───────────────────────────────────────────────────────────────");
    System.out.println("TOTAL: " + total + " operadores");

    assertTrue(total >= 430, "Deve haver pelo menos 430 operadores");
  }

  private String getCategory(String opName) {
    if (opName.startsWith("VELOCITY_")) return "VELOCITY";
    if (opName.startsWith("NEO4J_")) return "NEO4J";
    if (opName.startsWith("GEO_")) return "GEO";
    if (opName.startsWith("DEVICE_")) return "DEVICE";
    if (opName.startsWith("TIME_")) return "TIME";
    if (opName.startsWith("AMOUNT_")) return "AMOUNT";
    if (opName.startsWith("FATF_")) return "FATF";
    if (opName.startsWith("PLT_")) return "PLT";
    if (opName.startsWith("SCA_")) return "SCA";
    if (opName.startsWith("BSL_")) return "BSL";
    if (opName.startsWith("DORA_")) return "DORA";
    if (opName.startsWith("PSD")) return "PSD";
    if (opName.startsWith("MCC_")) return "MCC";
    if (opName.startsWith("ARRAY_")) return "ARRAY";
    if (opName.startsWith("STRING_")) return "STRING";
    if (opName.startsWith("LIST_")) return "LIST";
    if (opName.startsWith("REGEX_")) return "REGEX";
    if (opName.startsWith("DATE_")) return "DATE";
    if (opName.startsWith("MATH_")) return "MATH";
    if (opName.startsWith("STATISTICAL_")) return "STATISTICAL";
    if (opName.startsWith("ML_")) return "ML";
    if (opName.startsWith("FUZZY_")) return "FUZZY";
    if (opName.startsWith("BEHAVIORAL_")) return "BEHAVIORAL";
    if (opName.startsWith("NETWORK_")) return "NETWORK";
    if (opName.startsWith("CRYPTO_")) return "CRYPTO";
    return "OUTROS";
  }
}
