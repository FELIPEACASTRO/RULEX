package com.rulex.application.port.in;

import com.rulex.domain.model.Rule;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

/**
 * Port de entrada para gerenciamento de regras.
 *
 * <p>Define o contrato para CRUD de regras. Implementação em application/usecase.
 */
public interface ManageRulesUseCase {

  /**
   * Cria uma nova regra.
   *
   * @param command comando com dados da regra
   * @return regra criada com ID
   */
  Rule create(CreateRuleCommand command);

  /**
   * Atualiza uma regra existente.
   *
   * @param id identificador da regra
   * @param command comando com dados atualizados
   * @return regra atualizada
   */
  Rule update(UUID id, UpdateRuleCommand command);

  /**
   * Busca regra por ID.
   *
   * @param id identificador da regra
   * @return regra encontrada ou empty
   */
  Optional<Rule> findById(UUID id);

  /**
   * Busca regra por nome.
   *
   * @param name nome da regra
   * @return regra encontrada ou empty
   */
  Optional<Rule> findByName(String name);

  /**
   * Lista todas as regras ativas.
   *
   * @return lista de regras ativas
   */
  List<Rule> findAllActive();

  /**
   * Habilita uma regra.
   *
   * @param id identificador da regra
   */
  void enable(UUID id);

  /**
   * Desabilita uma regra.
   *
   * @param id identificador da regra
   */
  void disable(UUID id);

  /**
   * Remove uma regra.
   *
   * @param id identificador da regra
   */
  void delete(UUID id);

  /** Comando para criar regra */
  record CreateRuleCommand(
      String name,
      String description,
      Rule.RuleType type,
      Integer weight,
      Integer threshold,
      String classification,
      String conditionsJson,
      String logicOperator) {}

  /** Comando para atualizar regra */
  record UpdateRuleCommand(
      String name,
      String description,
      Rule.RuleType type,
      Integer weight,
      Integer threshold,
      String classification,
      String conditionsJson,
      String logicOperator,
      Boolean enabled) {}
}
