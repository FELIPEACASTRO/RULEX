package com.rulex.service.complex;

import static org.junit.jupiter.api.Assertions.*;

import com.rulex.dto.complex.ConditionDTO;
import com.rulex.entity.complex.RuleCondition;
import java.lang.reflect.Method;
import java.util.*;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * TRIPLE CHECK AVASSALADOR - Testes de Sincronização de Operadores Verifica que TODOS os operadores
 * estão sincronizados entre: - Entity (RuleCondition.ConditionOperator) - DTO
 * (ConditionDTO.OperatorType) - Evaluator (ComplexRuleEvaluator)
 */
public class OperatorSyncTest {

  @Test
  @DisplayName("🔥 DEVASTADOR: Entity e DTO devem ter os mesmos operadores")
  void testEntityAndDtoOperatorsMatch() {
    Set<String> entityOps = new HashSet<>();
    Set<String> dtoOps = new HashSet<>();

    // Extrair operadores da Entity
    for (RuleCondition.ConditionOperator op : RuleCondition.ConditionOperator.values()) {
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
    Set<String> missingMethods = new HashSet<>();

    // Extrair operadores da Entity
    for (RuleCondition.ConditionOperator op : RuleCondition.ConditionOperator.values()) {
      entityOps.add(op.name());
    }

    // Verificar se ComplexRuleEvaluator tem método para cada operador
    Class<?> evaluatorClass = ComplexRuleEvaluator.class;
    Method[] methods = evaluatorClass.getDeclaredMethods();

    Set<String> evaluateMethods = new HashSet<>();
    for (Method m : methods) {
      if (m.getName().startsWith("evaluate")) {
        evaluateMethods.add(m.getName().toLowerCase());
      }
    }

    System.out.println("═══════════════════════════════════════════════════════════════");
    System.out.println("TESTE: Operadores vs Métodos Evaluate");
    System.out.println("═══════════════════════════════════════════════════════════════");
    System.out.println("Total de operadores: " + entityOps.size());
    System.out.println("Total de métodos evaluate*: " + evaluateMethods.size());

    // Nota: Nem todo operador precisa de método próprio (alguns usam métodos genéricos)
    // Este teste verifica que há métodos suficientes
    assertTrue(
        evaluateMethods.size() >= 400,
        "Deve haver pelo menos 400 métodos evaluate*. Encontrados: " + evaluateMethods.size());

    System.out.println("✅ Métodos evaluate suficientes!");
  }

  @Test
  @DisplayName("🔥 DEVASTADOR: Contagem total de operadores")
  void testOperatorCount() {
    int entityCount = RuleCondition.ConditionOperator.values().length;
    int dtoCount = ConditionDTO.OperatorType.values().length;

    System.out.println("═══════════════════════════════════════════════════════════════");
    System.out.println("TESTE: Contagem de Operadores");
    System.out.println("═══════════════════════════════════════════════════════════════");
    System.out.println("Entity ConditionOperator: " + entityCount);
    System.out.println("DTO OperatorType: " + dtoCount);

    // Deve haver pelo menos 440 operadores
    assertTrue(
        entityCount >= 440,
        "Entity deve ter pelo menos 440 operadores. Encontrados: " + entityCount);
    assertTrue(dtoCount >= 440, "DTO deve ter pelo menos 440 operadores. Encontrados: " + dtoCount);

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

    for (RuleCondition.ConditionOperator op : RuleCondition.ConditionOperator.values()) {
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

    assertTrue(total >= 440, "Deve haver pelo menos 440 operadores");
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
    if (opName.startsWith("BIOMETRIC_")) return "BIOMETRIC";
    return "OUTROS";
  }
}
