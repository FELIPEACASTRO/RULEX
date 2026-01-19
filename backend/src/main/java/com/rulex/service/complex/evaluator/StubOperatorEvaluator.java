package com.rulex.service.complex.evaluator;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.exception.UnsupportedOperatorException;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Avaliador para operadores PLANNED (não implementados).
 *
 * <p>Este avaliador SEMPRE lança UnsupportedOperatorException quando chamado.
 * Operadores nesta lista estão declarados no enum mas ainda não foram implementados.
 *
 * <p>Para implementar um operador:
 * <ol>
 *   <li>Remova-o desta lista</li>
 *   <li>Adicione ao avaliador apropriado (ex: VelocityOperatorEvaluator)</li>
 *   <li>Implemente a lógica de avaliação</li>
 *   <li>Crie testes unitários</li>
 * </ol>
 */
@Component
@Slf4j
public class StubOperatorEvaluator implements OperatorEvaluator {

    /**
     * Lista de operadores PLANNED - declarados mas NÃO implementados.
     *
     * NOTA: Todos os 88 operadores anteriormente nesta lista foram implementados em:
     * - FATFOperatorEvaluator (28 operadores FATF)
     * - SCAOperatorEvaluator (12 operadores SCA/PSD2)
     * - BaselOperatorEvaluator (14 operadores Basel III)
     * - PlatformOperatorEvaluator (28 operadores PLT)
     * - MiningOperatorEvaluator (6 operadores Fuzzy/Mining)
     *
     * Esta lista agora está VAZIA - todos os operadores estão implementados!
     */
    private static final Set<ConditionOperator> PLANNED_OPERATORS = Set.of(
        // VAZIO - Todos os operadores foram implementados!
    );

    @Override
    public Set<ConditionOperator> getSupportedOperators() {
        return PLANNED_OPERATORS;
    }

    @Override
    public boolean evaluate(RuleCondition condition, EvaluationContext context) {
        ConditionOperator op = condition.getOperator();

        log.warn("Tentativa de usar operador PLANNED: {}. Este operador não está implementado.", op);
        log.warn("Regra que tentou usar: field={}, value={}",
            condition.getFieldName(), condition.getValueSingle());

        throw new UnsupportedOperatorException(op,
            "Este operador está planejado para implementação futura. " +
            "Consulte GET /api/operators/status para ver operadores disponíveis. " +
            "Operadores STABLE podem ser usados imediatamente.");
    }

    @Override
    public String getCategory() {
        return "PLANNED";
    }

    /**
     * Retorna o número de operadores PLANNED.
     */
    public int getPlannedCount() {
        return PLANNED_OPERATORS.size();
    }
}
