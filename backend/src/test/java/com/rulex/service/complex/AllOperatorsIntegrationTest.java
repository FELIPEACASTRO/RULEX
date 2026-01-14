package com.rulex.service.complex;

import static org.junit.jupiter.api.Assertions.*;

import com.rulex.dto.complex.ConditionDTO;
import com.rulex.entity.complex.RuleCondition;
import java.io.IOException;
import java.nio.file.*;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

/**
 * 🔥 TRIPLE CHECK AVASSALADOR E DEVASTADOR 1000x 🔥 Testes integrados para verificar TODOS os
 * operadores
 */
public class AllOperatorsIntegrationTest {

  @ParameterizedTest
  @EnumSource(RuleCondition.ConditionOperator.class)
  @DisplayName("🔥 Cada operador da Entity deve existir no DTO")
  void testEachEntityOperatorExistsInDto(RuleCondition.ConditionOperator entityOp) {
    String opName = entityOp.name();
    boolean foundInDto = false;
    for (ConditionDTO.OperatorType dtoOp : ConditionDTO.OperatorType.values()) {
      if (dtoOp.name().equals(opName)) {
        foundInDto = true;
        break;
      }
    }
    assertTrue(foundInDto, "❌ OPERADOR FALTANDO NO DTO: " + opName);
  }

  @ParameterizedTest
  @EnumSource(ConditionDTO.OperatorType.class)
  @DisplayName("🔥 Cada operador do DTO deve existir na Entity")
  void testEachDtoOperatorExistsInEntity(ConditionDTO.OperatorType dtoOp) {
    String opName = dtoOp.name();
    boolean foundInEntity = false;
    for (RuleCondition.ConditionOperator entityOp : RuleCondition.ConditionOperator.values()) {
      if (entityOp.name().equals(opName)) {
        foundInEntity = true;
        break;
      }
    }
    assertTrue(foundInEntity, "❌ OPERADOR FALTANDO NA ENTITY: " + opName);
  }

  @Test
  @DisplayName("🔥 DEVASTADOR: Verificar switch case no Evaluator para cada operador")
  void testEvaluatorHasCaseForEachOperator() throws IOException {
    Path evaluatorPath =
        Paths.get("src/main/java/com/rulex/service/complex/ComplexRuleEvaluator.java");
    String content = Files.readString(evaluatorPath);

    Set<String> foundCases = new HashSet<>();

    // Regex que captura: case OPERATOR_NAME (com ou sem ->)
    Pattern casePattern = Pattern.compile("case\\s+([A-Z][A-Z0-9_]+)");
    Matcher matcher = casePattern.matcher(content);
    while (matcher.find()) {
      foundCases.add(matcher.group(1));
    }

    System.out.println("═══════════════════════════════════════════════════════════════");
    System.out.println("VERIFICAÇÃO DE CASES NO EVALUATOR");
    System.out.println("═══════════════════════════════════════════════════════════════");
    System.out.println("Cases encontrados no switch: " + foundCases.size());

    Set<String> missingCases = new HashSet<>();
    for (RuleCondition.ConditionOperator op : RuleCondition.ConditionOperator.values()) {
      if (!foundCases.contains(op.name())) {
        missingCases.add(op.name());
      }
    }

    if (!missingCases.isEmpty()) {
      System.out.println("❌ OPERADORES SEM CASE NO SWITCH (" + missingCases.size() + "):");
      for (String missing : missingCases) {
        System.out.println("   - " + missing);
      }
    } else {
      System.out.println("✅ Todos os operadores têm case no switch!");
    }

    assertTrue(
        missingCases.isEmpty(),
        "❌ GAPS ENCONTRADOS! Operadores sem case no switch: "
            + missingCases.size()
            + "\n"
            + missingCases);
  }

  @Test
  @DisplayName("🔥 DEVASTADOR: Verificar PostgreSQL migration tem todos os operadores")
  void testPostgresHasAllOperators() throws IOException {
    Path v34Path = Paths.get("src/main/resources/db/migration/V34__add_v31_plus_operators.sql");
    String content = Files.readString(v34Path);

    Set<String> pgOperators = new HashSet<>();
    Pattern pattern = Pattern.compile("ADD VALUE IF NOT EXISTS '([A-Z0-9_]+)'");
    Matcher matcher = pattern.matcher(content);
    while (matcher.find()) {
      pgOperators.add(matcher.group(1));
    }

    System.out.println("═══════════════════════════════════════════════════════════════");
    System.out.println("VERIFICAÇÃO DO POSTGRESQL");
    System.out.println("═══════════════════════════════════════════════════════════════");
    System.out.println("Operadores no V34: " + pgOperators.size());

    Set<String> missingInPg = new HashSet<>();
    for (RuleCondition.ConditionOperator op : RuleCondition.ConditionOperator.values()) {
      if (!pgOperators.contains(op.name())) {
        missingInPg.add(op.name());
      }
    }

    if (!missingInPg.isEmpty()) {
      System.out.println("❌ OPERADORES FALTANDO NO POSTGRESQL (" + missingInPg.size() + "):");
      for (String missing : missingInPg) {
        System.out.println("   - " + missing);
      }
    } else {
      System.out.println("✅ PostgreSQL tem todos os operadores!");
    }

    assertTrue(
        missingInPg.isEmpty(),
        "❌ GAPS ENCONTRADOS! Operadores faltando no PostgreSQL: "
            + missingInPg.size()
            + "\n"
            + missingInPg);
  }

  @Test
  @DisplayName("🔥 DEVASTADOR: Relatório completo de operadores")
  void testGenerateCompleteReport() {
    int entityCount = RuleCondition.ConditionOperator.values().length;
    int dtoCount = ConditionDTO.OperatorType.values().length;
    int entityValueTypes = RuleCondition.ConditionValueType.values().length;
    int dtoValueTypes = ConditionDTO.ValueType.values().length;

    System.out.println("╔══════════════════════════════════════════════════════════════╗");
    System.out.println("║  🔥 RELATÓRIO AVASSALADOR E DEVASTADOR 1000x 🔥              ║");
    System.out.println("╚══════════════════════════════════════════════════════════════╝");
    System.out.println("");
    System.out.println("┌──────────────────────────────────────────────────────────────┐");
    System.out.println("│ OPERADORES                                                   │");
    System.out.println("├──────────────────────────────────────────────────────────────┤");
    System.out.println(
        "│ Entity (ConditionOperator):  "
            + String.format("%4d", entityCount)
            + "                           │");
    System.out.println(
        "│ DTO (OperatorType):          "
            + String.format("%4d", dtoCount)
            + "                           │");
    System.out.println("├──────────────────────────────────────────────────────────────┤");
    System.out.println("│ VALUE TYPES                                                  │");
    System.out.println("├──────────────────────────────────────────────────────────────┤");
    System.out.println(
        "│ Entity (ConditionValueType): "
            + String.format("%4d", entityValueTypes)
            + "                           │");
    System.out.println(
        "│ DTO (ValueType):             "
            + String.format("%4d", dtoValueTypes)
            + "                           │");
    System.out.println("└──────────────────────────────────────────────────────────────┘");

    assertEquals(entityCount, dtoCount, "Entity e DTO devem ter o mesmo número de operadores");
    assertEquals(
        entityValueTypes, dtoValueTypes, "Entity e DTO devem ter o mesmo número de ValueTypes");

    System.out.println("");
    System.out.println("✅ TODOS OS TESTES PASSARAM!");
  }
}
