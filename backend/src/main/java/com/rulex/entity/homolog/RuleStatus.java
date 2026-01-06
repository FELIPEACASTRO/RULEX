package com.rulex.entity.homolog;

/**
 * Status de uma regra no sistema.
 *
 * <p>Valores:
 * <ul>
 *   <li>DRAFT - Rascunho, não é executada</li>
 *   <li>PUBLISHED - Publicada, em execução normal</li>
 *   <li>DEPRECATED - Descontinuada, mantida para histórico</li>
 *   <li>ARCHIVED - Arquivada, não é executada</li>
 *   <li>TESTING - Em teste, executa mas não afeta decisão final</li>
 * </ul>
 */
public enum RuleStatus {
  /** Rascunho - regra ainda não publicada, não é executada */
  DRAFT,

  /** Publicada - regra ativa em execução normal */
  PUBLISHED,

  /** Descontinuada - mantida para histórico, não é executada */
  DEPRECATED,

  /** Arquivada - regra desativada, não é executada */
  ARCHIVED,

  /** Em teste - executa mas não afeta a decisão final */
  TESTING
}
