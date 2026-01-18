package com.rulex.service.complex.evaluator;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.RuleCondition.ConditionOperator;
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
     * Identificados via: grep "stub - retornando false" ComplexRuleEvaluator.java
     */
    private static final Set<ConditionOperator> PLANNED_OPERATORS = Set.of(
        // FATF (Financial Action Task Force) - 23 operadores
        ConditionOperator.FATF_BLACK_MARKET_EXCHANGE,
        ConditionOperator.FATF_CORRESPONDENT_LAYERING,
        ConditionOperator.FATF_CRYPTO_ATM_CASHOUT,
        ConditionOperator.FATF_CRYPTO_MIXING,
        ConditionOperator.FATF_HAWALA_INFORMAL,
        ConditionOperator.FATF_INSURANCE_CASH_VALUE,
        ConditionOperator.FATF_INTEGRATION_BUSINESS_INVESTMENT,
        ConditionOperator.FATF_INTEGRATION_LOAN_REPAYMENT,
        ConditionOperator.FATF_INTEGRATION_LUXURY_GOODS,
        ConditionOperator.FATF_INTEGRATION_REAL_ESTATE,
        ConditionOperator.FATF_LAYERING_CONVERTIBLE_INSTRUMENTS,
        ConditionOperator.FATF_LAYERING_OFFSHORE,
        ConditionOperator.FATF_LAYERING_RAPID_MOVEMENT,
        ConditionOperator.FATF_LAYERING_SHELL_COMPANY,
        ConditionOperator.FATF_LAYERING_WIRE_CHAINS,
        ConditionOperator.FATF_NEW_PAYMENT_EXPLOITATION,
        ConditionOperator.FATF_PEP_TRANSACTION,
        ConditionOperator.FATF_PLACEMENT_CASH_INTENSIVE,
        ConditionOperator.FATF_PLACEMENT_CASINO_GAMBLING,
        ConditionOperator.FATF_PLACEMENT_CURRENCY_EXCHANGE,
        ConditionOperator.FATF_PLACEMENT_SMURFING,
        ConditionOperator.FATF_PLACEMENT_STRUCTURING,
        ConditionOperator.FATF_ROUND_TRIPPING,
        ConditionOperator.FATF_TBML_FALSE_DESCRIPTION,
        ConditionOperator.FATF_TBML_MULTIPLE_INVOICING,
        ConditionOperator.FATF_TBML_OVER_INVOICING,
        ConditionOperator.FATF_TBML_PHANTOM_SHIPPING,
        ConditionOperator.FATF_TBML_UNDER_INVOICING,

        // PLT (Platform) - 28 operadores
        ConditionOperator.PLT_BACKTESTING_LABELING,
        ConditionOperator.PLT_BAD_ENTITY_NETWORK,
        ConditionOperator.PLT_BEHAVIORAL_PROFILING,
        ConditionOperator.PLT_BEHAVIOR_SORTED_LISTS,
        ConditionOperator.PLT_BUSINESS_RULES_SCENARIO,
        ConditionOperator.PLT_COMPROMISE_MANAGER,
        ConditionOperator.PLT_CONSORTIUM_DATA_CHECK,
        ConditionOperator.PLT_CUSTOM_RULE_BUILDER,
        ConditionOperator.PLT_DS2_RULE_ENGINE,
        ConditionOperator.PLT_IDENTITY_RESOLUTION,
        ConditionOperator.PLT_INTELLIGENCE_NETWORK,
        ConditionOperator.PLT_LINKING_VELOCITY,
        ConditionOperator.PLT_ML_FRAUD_RISK_OUTCOME,
        ConditionOperator.PLT_NETWORK_ANALYTICS,
        ConditionOperator.PLT_NETWORK_ENTITY_RESOLUTION,
        ConditionOperator.PLT_RADAR_COMPLEX_CONDITIONS,
        ConditionOperator.PLT_RADAR_INLINE_LISTS,
        ConditionOperator.PLT_RADAR_METADATA_MATCHING,
        ConditionOperator.PLT_RADAR_RULE_BACKTESTING,
        ConditionOperator.PLT_REAL_TIME_DETECTION,
        ConditionOperator.PLT_REVIEWLIST_QUEUE,
        ConditionOperator.PLT_RISK_LIST_COMPARISON,
        ConditionOperator.PLT_RISK_PROFILE_ASSIGNMENT,
        ConditionOperator.PLT_RISK_SCORE_CALCULATION,
        ConditionOperator.PLT_RULES_MODELS_HYBRID,
        ConditionOperator.PLT_SAR_AUTOMATED,
        ConditionOperator.PLT_SCENARIO_SCORECARD,
        ConditionOperator.PLT_VELOCITY_FILTERS,

        // BSL (Basel) - 14 operadores
        ConditionOperator.BSL_BUCKET_CLASSIFICATION,
        ConditionOperator.BSL_BUSINESS_INDICATOR,
        ConditionOperator.BSL_BUSINESS_INDICATOR_COMPONENT,
        ConditionOperator.BSL_CONTROL_DEFICIENCY,
        ConditionOperator.BSL_INTERNAL_LOSS_MULTIPLIER,
        ConditionOperator.BSL_KRI_MONITORING,
        ConditionOperator.BSL_LOSS_DATA_COLLECTION,
        ConditionOperator.BSL_LOSS_EVENT_REPORTING,
        ConditionOperator.BSL_LOSS_EXCLUSION_APPROVAL,
        ConditionOperator.BSL_LOSS_THRESHOLD_SETTING,
        ConditionOperator.BSL_MARGINAL_COEFFICIENT,
        ConditionOperator.BSL_RETENTION_PERIOD,
        ConditionOperator.BSL_RISK_GOVERNANCE,
        ConditionOperator.BSL_SCENARIO_ANALYSIS,

        // SCA (Strong Customer Authentication) - 12 operadores
        ConditionOperator.SCA_CHALLENGE_MANDATORY,
        ConditionOperator.SCA_CONTACTLESS_EXEMPTION,
        ConditionOperator.SCA_CORPORATE_PAYMENT,
        ConditionOperator.SCA_DYNAMIC_3DS_ROUTING,
        ConditionOperator.SCA_FRAUD_RATE_MONITORING,
        ConditionOperator.SCA_LIABILITY_SHIFT,
        ConditionOperator.SCA_LOW_VALUE_EXEMPTION,
        ConditionOperator.SCA_MERCHANT_INITIATED,
        ConditionOperator.SCA_RECURRING_TRANSACTION,
        ConditionOperator.SCA_SECURE_CORPORATE_PROTOCOL,
        ConditionOperator.SCA_TRA_EXEMPTION,
        ConditionOperator.SCA_TRUSTED_BENEFICIARY,

        // Fuzzy/ML - 3 operadores
        ConditionOperator.FUZZY_ADAPTIVE_THRESHOLD,
        ConditionOperator.FUZZY_MEMBERSHIP,

        // Data Mining - 3 operadores
        ConditionOperator.APRIORI_ASSOCIATION,
        ConditionOperator.ECLAT_ITEMSET,
        ConditionOperator.FPGROWTH_FREQUENT_PATTERNS
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
