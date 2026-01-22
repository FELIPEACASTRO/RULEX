/**
 * Pacote de evaluators com Strategy Pattern para regras complexas.
 * 
 * <p>ARQUITETURA:
 * Este pacote contém evaluators que implementam a interface {@link OperatorEvaluator}
 * e são registrados automaticamente via Spring no {@link OperatorEvaluatorRegistry}.
 * 
 * <p>VANTAGENS:
 * - Extensibilidade: Novos operadores podem ser adicionados sem modificar o ComplexRuleEvaluator
 * - Testabilidade: Cada evaluator pode ser testado isoladamente
 * - Manutenibilidade: Código organizado por domínio/categoria de operadores
 * 
 * <p>COMO ADICIONAR NOVO EVALUATOR:
 * 1. Criar classe que implementa {@link OperatorEvaluator}
 * 2. Anotar com @Component
 * 3. Implementar getSupportedOperators() retornando os operadores suportados
 * 4. Implementar evaluate() com a lógica de avaliação
 * 
 * @see OperatorEvaluator
 * @see OperatorEvaluatorRegistry
 * @see com.rulex.service.complex.ComplexRuleEvaluator
 */
package com.rulex.service.complex.evaluator;
