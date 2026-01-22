/**
 * Pacote de evaluators estáticos para regras complexas.
 * 
 * <p>ARQUITETURA:
 * Este pacote contém evaluators com métodos ESTÁTICOS que são chamados
 * diretamente pelo ComplexRuleEvaluator. Eles são usados para operadores
 * que requerem acesso a serviços externos (VelocityService, Neo4jService, etc.)
 * 
 * <p>DIFERENÇA COM evaluator/:
 * - evaluation/ (este pacote): Métodos estáticos, chamados diretamente
 * - evaluator/: Strategy Pattern, registrados via OperatorEvaluatorRegistry
 * 
 * <p>MIGRAÇÃO FUTURA:
 * Idealmente, todos os evaluators deveriam usar o padrão Strategy do evaluator/.
 * A migração deve ser feita gradualmente para evitar regressões.
 * 
 * @see com.rulex.service.complex.evaluator.OperatorEvaluatorRegistry
 * @see com.rulex.service.complex.ComplexRuleEvaluator
 */
package com.rulex.service.complex.evaluation;
