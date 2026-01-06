package com.rulex.application.port.out;

import com.rulex.domain.model.Rule;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

/**
 * Port de persistência de regras de fraude.
 *
 * <p>Define como a camada de aplicação acessa regras. Implementação em infrastructure/persistence.
 */
public interface RulePersistencePort {

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
   * Lista todas as regras ativas ordenadas por prioridade.
   *
   * @return lista de regras ativas
   */
  List<Rule> findAllActiveOrderedByPriority();

  /**
   * Lista regras ativas por tipo.
   *
   * @param type tipo de regra (HARD_RULE, SOFT_RULE, etc.)
   * @return lista de regras do tipo especificado
   */
  List<Rule> findActiveByType(Rule.RuleType type);

  /**
   * Lista regras em shadow mode.
   *
   * @return regras em shadow/canary
   */
  List<Rule> findShadowRules();

  /**
   * Persiste ou atualiza uma regra.
   *
   * @param rule regra a persistir
   * @return regra persistida com ID
   */
  Rule save(Rule rule);

  /**
   * Remove regra por ID.
   *
   * @param id identificador da regra
   */
  void deleteById(UUID id);

  /**
   * Conta regras ativas.
   *
   * @return total de regras ativas
   */
  long countActive();
}
