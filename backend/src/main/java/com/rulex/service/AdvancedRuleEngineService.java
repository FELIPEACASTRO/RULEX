package com.rulex.service;

import com.rulex.dto.TransactionRequest;
import com.rulex.dto.TriggeredRuleDTO;
import java.util.List;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * Serviço avançado de motor de regras de fraude.
 *
 * <p>REFATORADO (GAP-FIX): Este serviço agora delega a execução das regras para o
 * DatabaseRuleExecutorService, que lê as regras do banco de dados.
 *
 * <p>As 28 regras que antes estavam hardcoded foram migradas para a tabela rule_configurations
 * através da migration V27__migrate_hardcoded_advanced_rules.sql
 *
 * <p>Mantém compatibilidade com a API existente (executeAllAdvancedRules,
 * executeAllAdvancedRulesDetailed) para não quebrar código cliente.
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class AdvancedRuleEngineService {

  private final DatabaseRuleExecutorService databaseRuleExecutorService;

  /** Categorias de regras (mantido para compatibilidade). */
  public enum RuleCategory {
    EMV_SECURITY,
    TRANSACTION_CONTEXT,
    TERMINAL_NETWORK,
    PIN_CVV_VERIFICATION,
    CUSTOM_INDICATORS,
    TEMPORAL_ADVANCED,
    UNIQUE_IDENTIFIERS,
    CURRENCY_CONVERSION,
    AUTH_SEQUENCE,
    CONTEXT_COHERENCE,
    AUTHORIZATION_CONTRADICTION,
    ACQUIRER_PATTERN
  }

  /** Resultado da avaliação de regras. */
  public enum RuleResult {
    APPROVED,
    SUSPICIOUS,
    FRAUD
  }

  /** Record com resultado da execução e regras acionadas. */
  public record AdvancedExecution(RuleResult result, List<TriggeredRuleDTO> triggeredRules) {}

  /**
   * Executa todas as regras avançadas e retorna o resultado mais severo.
   *
   * @param transaction A transação a ser avaliada
   * @return O resultado mais severo (FRAUD > SUSPICIOUS > APPROVED)
   */
  public RuleResult executeAllAdvancedRules(TransactionRequest transaction) {
    return executeAllAdvancedRulesDetailed(transaction).result();
  }

  /**
   * Executa todas as regras avançadas e retorna detalhes completos.
   *
   * <p>REFATORADO: Agora delega para o DatabaseRuleExecutorService que lê as regras do banco de
   * dados ao invés de usar lógica hardcoded.
   *
   * @param transaction A transação a ser avaliada
   * @return Resultado com classificação e lista de regras acionadas
   */
  public AdvancedExecution executeAllAdvancedRulesDetailed(TransactionRequest transaction) {
    log.info(
        "Executando regras avançadas do banco de dados para transação: {}",
        transaction.getExternalTransactionId());

    DatabaseRuleExecutorService.RuleExecution execution =
        databaseRuleExecutorService.executeAdvancedRules(transaction);

    // Converte o resultado do DatabaseRuleExecutorService para o formato esperado
    RuleResult result = mapResult(execution.result());

    return new AdvancedExecution(result, execution.triggeredRules());
  }

  /** Mapeia o resultado do DatabaseRuleExecutorService para o enum local. */
  private RuleResult mapResult(DatabaseRuleExecutorService.RuleResult dbResult) {
    return switch (dbResult) {
      case FRAUD -> RuleResult.FRAUD;
      case SUSPICIOUS -> RuleResult.SUSPICIOUS;
      case APPROVED -> RuleResult.APPROVED;
    };
  }
}
