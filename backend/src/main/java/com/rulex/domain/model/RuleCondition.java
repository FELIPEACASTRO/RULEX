package com.rulex.domain.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Value;

/**
 * Domain model para Condição de Regra.
 *
 * <p>POJO puro sem anotações JPA/Spring. Representa uma condição individual de uma regra.
 *
 * <p>Exemplo: field="transactionAmount", operator="GT", value="1000"
 */
@Value
@Builder
@AllArgsConstructor
public class RuleCondition {

  /** Campo do payload a ser avaliado (ex: transactionAmount, mcc, pan) */
  String field;

  /** Operador de comparação (ex: EQ, GT, CONTAINS, IN) */
  String operator;

  /**
   * Valor para comparação.
   *
   * <p>Nota: operadores unários (IS_NULL, IS_TRUE) não precisam de valor.
   */
  String value;

  /** Cria condição de igualdade */
  public static RuleCondition eq(String field, String value) {
    return RuleCondition.builder().field(field).operator("EQ").value(value).build();
  }

  /** Cria condição maior que */
  public static RuleCondition gt(String field, String value) {
    return RuleCondition.builder().field(field).operator("GT").value(value).build();
  }

  /** Cria condição menor que */
  public static RuleCondition lt(String field, String value) {
    return RuleCondition.builder().field(field).operator("LT").value(value).build();
  }

  /** Cria condição IN (lista) */
  public static RuleCondition in(String field, String value) {
    return RuleCondition.builder().field(field).operator("IN").value(value).build();
  }

  /** Cria condição CONTAINS (string) */
  public static RuleCondition contains(String field, String value) {
    return RuleCondition.builder().field(field).operator("CONTAINS").value(value).build();
  }

  /** Cria condição IS_NULL (unária) */
  public static RuleCondition isNull(String field) {
    return RuleCondition.builder().field(field).operator("IS_NULL").value("").build();
  }

  /** Verifica se é operador unário (não precisa de valor) */
  public boolean isUnaryOperator() {
    return operator != null
        && (operator.equals("IS_NULL")
            || operator.equals("IS_NOT_NULL")
            || operator.equals("IS_TRUE")
            || operator.equals("IS_FALSE"));
  }
}
