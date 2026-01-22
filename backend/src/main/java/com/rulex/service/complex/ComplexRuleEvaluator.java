package com.rulex.service.complex;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.RuleConditionGroup;
import com.rulex.entity.complex.RuleExecutionDetail;
import com.rulex.exception.UnsupportedOperatorException;
import com.rulex.service.FuzzyLogicService;
import com.rulex.service.Neo4jGraphService;
import com.rulex.service.OperatorDataService;
import com.rulex.service.StatisticalAnalysisService;
import com.rulex.service.StringSimilarityService;
import com.rulex.service.VelocityService;
import com.rulex.service.VelocityServiceFacade;
import com.rulex.service.complex.context.GroupByExtractor;
import com.rulex.service.complex.evaluation.AmlTypologyEvaluator;
import com.rulex.service.complex.evaluation.BasicOperatorEvaluator;
import com.rulex.service.complex.evaluation.BehavioralPatternEvaluator;
import com.rulex.service.complex.evaluation.CriticalOperatorEvaluator;
import com.rulex.service.complex.evaluation.CustomerHistoryEvaluator;
import com.rulex.service.complex.evaluation.DeviceFingerprintEvaluator;
import com.rulex.service.complex.evaluation.DeviceRiskEvaluator;
import com.rulex.service.complex.evaluation.ExpectedValueFormatter;
import com.rulex.service.complex.evaluation.FirstOccurrenceEvaluator;
import com.rulex.service.complex.evaluation.GraphNetworkEvaluator;
import com.rulex.service.complex.evaluation.HistoricalEvaluator;
import com.rulex.service.complex.evaluation.IdentityRiskEvaluator;
import com.rulex.service.complex.evaluation.Iso20022Evaluator;
import com.rulex.service.complex.evaluation.MerchantAdvancedEvaluator;
import com.rulex.service.complex.evaluation.MerchantMccEvaluator;
import com.rulex.service.complex.evaluation.NameSimilarityEvaluator;
import com.rulex.service.complex.evaluation.Neo4jGraphEvaluator;
import com.rulex.service.complex.evaluation.PatternEvaluator;
import com.rulex.service.complex.evaluation.RegulatoryComplianceEvaluator;
import com.rulex.service.complex.evaluation.SanctionsNameMatchingEvaluator;
import com.rulex.service.complex.evaluation.SimpleStatsEvaluator;
import com.rulex.service.complex.evaluation.StatisticalBehavioralEvaluator;
import com.rulex.service.complex.evaluation.StatisticalRiskEvaluator;
import com.rulex.service.complex.evaluation.SuspiciousKeywordEvaluator;
import com.rulex.service.complex.evaluation.TemporalVelocityEvaluator;
import com.rulex.service.complex.evaluation.TimeDateEvaluator;
import com.rulex.service.complex.evaluation.V28V30Evaluator;
import com.rulex.service.complex.evaluation.V31BehavioralEvaluator;
import com.rulex.service.complex.evaluation.V49OperatorsEvaluator;
import com.rulex.service.complex.evaluation.VelocityAdvancedEvaluator;
import com.rulex.service.complex.evaluation.VelocityAggregationEvaluator;
import com.rulex.service.complex.evaluator.OperatorEvaluator;
import com.rulex.service.complex.evaluator.OperatorEvaluatorRegistry;
import com.rulex.service.complex.parsing.TimeWindowParser;
import java.util.*;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Motor de avaliação de regras complexas. Avalia grupos de condições aninhados com suporte a todos
 * os operadores.
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class ComplexRuleEvaluator {

  private final VelocityServiceFacade velocityServiceFacade;
  private final OperatorDataService operatorDataService;
  private final Neo4jGraphService neo4jGraphService;
  private final StatisticalAnalysisService statisticalAnalysisService;
  private final FuzzyLogicService fuzzyLogicService;
  private final StringSimilarityService stringSimilarityService;

  // ARCH-001 FIX: Integração com OperatorEvaluatorRegistry para delegação modular
  private final OperatorEvaluatorRegistry operatorEvaluatorRegistry;

  /** Resultado da avaliação de uma regra */
  @Data
  @Builder
  public static class EvaluationResult {
    private boolean matched;
    private List<RuleExecutionDetail> executionDetails;
    private long executionTimeMs;
    private String errorMessage;
  }

  /** Contexto de avaliação contendo os dados da transação */
  @Data
  @Builder
  public static class EvaluationContext {
    private Map<String, Object> payload;
    private Map<String, Object> variables;
    private UUID decisionLogId;
    private UUID ruleVersionId;
    private TransactionRequest transactionRequest; // Para operadores GEO
  }

  /** Avalia um grupo de condições contra um contexto */
  public EvaluationResult evaluate(RuleConditionGroup rootGroup, EvaluationContext context) {
    long startTime = System.currentTimeMillis();
    List<RuleExecutionDetail> details = new ArrayList<>();

    try {
      boolean result = evaluateGroup(rootGroup, context, details);

      return EvaluationResult.builder()
          .matched(result)
          .executionDetails(details)
          .executionTimeMs(System.currentTimeMillis() - startTime)
          .build();

    } catch (Exception e) {
      log.error("Erro ao avaliar regra: {}", e.getMessage(), e);
      return EvaluationResult.builder()
          .matched(false)
          .executionDetails(details)
          .executionTimeMs(System.currentTimeMillis() - startTime)
          .errorMessage(e.getMessage())
          .build();
    }
  }

  /** Avalia um grupo de condições recursivamente */
  private boolean evaluateGroup(
      RuleConditionGroup group, EvaluationContext context, List<RuleExecutionDetail> details) {
    if (group == null || !Boolean.TRUE.equals(group.getEnabled())) {
      return true; // Grupo desabilitado é considerado verdadeiro
    }

    List<Boolean> results = new ArrayList<>();

    // Avaliar condições diretas do grupo
    if (group.getConditions() != null) {
      for (RuleCondition condition : group.getConditions()) {
        if (Boolean.TRUE.equals(condition.getEnabled())) {
          boolean conditionResult = evaluateCondition(condition, context, details);
          results.add(conditionResult);
        }
      }
    }

    // Avaliar grupos filhos recursivamente
    if (group.getChildren() != null) {
      for (RuleConditionGroup child : group.getChildren()) {
        boolean childResult = evaluateGroup(child, context, details);
        results.add(childResult);
      }
    }

    // Se não há resultados, retorna verdadeiro
    if (results.isEmpty()) {
      return true;
    }

    // Aplicar operador lógico
    return applyLogicOperator(group.getLogicOperator(), results);
  }

  /** Aplica o operador lógico aos resultados */
  private boolean applyLogicOperator(
      RuleConditionGroup.GroupLogicOperator operator, List<Boolean> results) {
    if (results.isEmpty()) return true;

    return switch (operator) {
      case AND -> results.stream().allMatch(Boolean::booleanValue);
      case OR -> results.stream().anyMatch(Boolean::booleanValue);
      case NOT -> !results.get(0); // NOT aplica ao primeiro resultado
      case XOR -> results.stream().filter(Boolean::booleanValue).count() == 1;
      case NAND -> !results.stream().allMatch(Boolean::booleanValue);
      case NOR -> !results.stream().anyMatch(Boolean::booleanValue);
    };
  }

  /** Avalia uma condição individual */
  private boolean evaluateCondition(
      RuleCondition condition, EvaluationContext context, List<RuleExecutionDetail> details) {
    long startTime = System.currentTimeMillis();
    String errorMessage = null;
    boolean result = false;

    try {
      // Obter valor do campo
      Object fieldValue =
          getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);

      // Avaliar condição
      result = evaluateOperator(condition, fieldValue, context);

      // Aplicar negação se necessário
      if (Boolean.TRUE.equals(condition.getNegate())) {
        result = !result;
      }

    } catch (Exception e) {
      log.warn("Erro ao avaliar condição {}: {}", condition.getFieldName(), e.getMessage());
      errorMessage = e.getMessage();
      result = false;
    }

    // Registrar detalhe da execução
    details.add(
        RuleExecutionDetail.builder()
            .decisionLogId(context.getDecisionLogId())
            .ruleVersionId(context.getRuleVersionId())
            .conditionId(condition.getId())
            .groupId(condition.getGroup() != null ? condition.getGroup().getId() : null)
            .fieldName(condition.getFieldName())
            .fieldValue(
                String.valueOf(
                    getFieldValue(condition.getFieldName(), condition.getFieldPath(), context)))
            .operator(condition.getOperator().name())
            .expectedValue(getExpectedValueString(condition))
            .result(result)
            .executionTimeMs((int) (System.currentTimeMillis() - startTime))
            .errorMessage(errorMessage)
            .build());

    return result;
  }

  /** Obtém o valor de um campo do contexto */
  @SuppressWarnings("unchecked")
  private Object getFieldValue(String fieldName, String fieldPath, EvaluationContext context) {
    // Primeiro verificar variáveis
    if (context.getVariables() != null && context.getVariables().containsKey(fieldName)) {
      return context.getVariables().get(fieldName);
    }

    // Depois verificar payload
    if (context.getPayload() == null) {
      return null;
    }

    // Se tem path, navegar pelo objeto
    if (fieldPath != null && !fieldPath.isEmpty()) {
      String[] parts = fieldPath.split("\\.");
      Object current = context.getPayload();

      for (String part : parts) {
        if (current == null) return null;
        if (current instanceof Map) {
          current = ((Map<String, Object>) current).get(part);
        } else {
          return null;
        }
      }
      return current;
    }

    return context.getPayload().get(fieldName);
  }

  /** Avalia o operador da condição */
  private boolean evaluateOperator(
      RuleCondition condition, Object fieldValue, EvaluationContext context) {
    ConditionOperator operator = condition.getOperator();

    return switch (operator) {
        // Comparação básica
      case EQ ->
          evaluateEquals(fieldValue, condition.getValueSingle(), condition.getCaseSensitive());
      case NEQ ->
          !evaluateEquals(fieldValue, condition.getValueSingle(), condition.getCaseSensitive());
      case GT -> compareValues(fieldValue, condition.getValueSingle()) > 0;
      case GTE -> compareValues(fieldValue, condition.getValueSingle()) >= 0;
      case LT -> compareValues(fieldValue, condition.getValueSingle()) < 0;
      case LTE -> compareValues(fieldValue, condition.getValueSingle()) <= 0;

        // Listas
      case IN -> evaluateIn(fieldValue, condition.getValueArray(), condition.getCaseSensitive());
      case NOT_IN ->
          !evaluateIn(fieldValue, condition.getValueArray(), condition.getCaseSensitive());

        // Strings
      case CONTAINS ->
          evaluateContains(fieldValue, condition.getValueSingle(), condition.getCaseSensitive());
      case NOT_CONTAINS ->
          !evaluateContains(fieldValue, condition.getValueSingle(), condition.getCaseSensitive());
      case STARTS_WITH ->
          evaluateStartsWith(fieldValue, condition.getValueSingle(), condition.getCaseSensitive());
      case ENDS_WITH ->
          evaluateEndsWith(fieldValue, condition.getValueSingle(), condition.getCaseSensitive());
      case REGEX -> evaluateRegex(fieldValue, condition.getValueSingle());
      case NOT_REGEX -> !evaluateRegex(fieldValue, condition.getValueSingle());

        // Nulos
      case IS_NULL -> fieldValue == null;
      case NOT_NULL -> fieldValue != null;

        // Booleanos
      case IS_TRUE -> Boolean.TRUE.equals(toBoolean(fieldValue));
      case IS_FALSE -> Boolean.FALSE.equals(toBoolean(fieldValue));

        // Range
      case BETWEEN -> evaluateBetween(fieldValue, condition.getValueMin(), condition.getValueMax());
      case NOT_BETWEEN ->
          !evaluateBetween(fieldValue, condition.getValueMin(), condition.getValueMax());

        // Comparação entre campos
      case FIELD_EQ ->
          evaluateFieldComparison(fieldValue, condition.getValueFieldRef(), context, 0);
      case FIELD_NEQ ->
          !evaluateFieldComparison(fieldValue, condition.getValueFieldRef(), context, 0);
      case FIELD_GT ->
          evaluateFieldComparison(fieldValue, condition.getValueFieldRef(), context, 1);
      case FIELD_GTE ->
          evaluateFieldComparison(fieldValue, condition.getValueFieldRef(), context, 2);
      case FIELD_LT ->
          evaluateFieldComparison(fieldValue, condition.getValueFieldRef(), context, -1);
      case FIELD_LTE ->
          evaluateFieldComparison(fieldValue, condition.getValueFieldRef(), context, -2);

        // Data/Tempo
      case DATE_BEFORE -> evaluateDateBefore(fieldValue, condition.getValueSingle());
      case DATE_AFTER -> evaluateDateAfter(fieldValue, condition.getValueSingle());
      case DATE_BETWEEN ->
          evaluateDateBetween(fieldValue, condition.getValueMin(), condition.getValueMax());
      case TIME_BEFORE -> evaluateTimeBefore(fieldValue, condition.getValueSingle());
      case TIME_AFTER -> evaluateTimeAfter(fieldValue, condition.getValueSingle());
      case TIME_BETWEEN ->
          evaluateTimeBetween(fieldValue, condition.getValueMin(), condition.getValueMax());

        // Array
      case ARRAY_CONTAINS -> evaluateArrayContains(fieldValue, condition.getValueSingle());
      case ARRAY_NOT_CONTAINS -> !evaluateArrayContains(fieldValue, condition.getValueSingle());
      case ARRAY_SIZE_EQ -> evaluateArraySize(fieldValue, condition.getValueSingle(), 0);
      case ARRAY_SIZE_GT -> evaluateArraySize(fieldValue, condition.getValueSingle(), 1);
      case ARRAY_SIZE_LT -> evaluateArraySize(fieldValue, condition.getValueSingle(), -1);

        // Matemáticas
      case MOD_EQ ->
          evaluateModulo(fieldValue, condition.getValueSingle(), condition.getValueMin(), true);
      case MOD_NEQ ->
          evaluateModulo(fieldValue, condition.getValueSingle(), condition.getValueMin(), false);

        // Geolocalização - Implementado via GeoService
        // Coordenadas são derivadas de merchantCity/State/CountryCode
      case GEO_DISTANCE_LT -> delegateToRegistry(operator, condition, context);
      case GEO_DISTANCE_GT -> delegateToRegistry(operator, condition, context);
      case GEO_IN_POLYGON -> delegateToRegistry(operator, condition, context);

        // Velocity (agregações temporais)
      case VELOCITY_COUNT_GT -> delegateToRegistry(operator, condition, context);
      case VELOCITY_COUNT_LT -> delegateToRegistry(operator, condition, context);
      case VELOCITY_SUM_GT -> delegateToRegistry(operator, condition, context);
      case VELOCITY_SUM_LT -> delegateToRegistry(operator, condition, context);
      case VELOCITY_AVG_GT -> delegateToRegistry(operator, condition, context);
      case VELOCITY_AVG_LT -> delegateToRegistry(operator, condition, context);
      case VELOCITY_DISTINCT_GT -> delegateToRegistry(operator, condition, context);
      case VELOCITY_DISTINCT_LT -> delegateToRegistry(operator, condition, context);

        // Agregações temporais avançadas (DSL expandida)
      case SUM_LAST_N_DAYS -> evaluateSumLastNDays(condition, context);
      case COUNT_LAST_N_HOURS -> evaluateCountLastNHours(condition, context);
      case AVG_LAST_N_DAYS -> evaluateAvgLastNDays(condition, context);
      case COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS ->
          evaluateCountDistinctMerchantsLastNDays(condition, context);
      case COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS ->
          evaluateCountDistinctCountriesLastNHours(condition, context);
      case MAX_AMOUNT_LAST_N_DAYS -> evaluateMaxAmountLastNDays(condition, context);
      case MIN_AMOUNT_LAST_N_DAYS -> evaluateMinAmountLastNDays(condition, context);

        // Operadores críticos para regras de fraude avançadas
      case NOT_IN_HISTORICAL ->
          HistoricalEvaluator.evaluateNotInHistorical(condition, context, operatorDataService);
      case NAME_SIMILARITY_LT ->
          NameSimilarityEvaluator.evaluateNameSimilarityLt(condition, context);
      case GTE_PERCENT_OF_LAST_INCOMING -> evaluateGtePercentOfLastIncoming(condition, context);
      case DOMAIN_IN_LIST -> evaluateDomainInList(fieldValue, condition);
      case CHARGEBACK_RATE_GT -> evaluateChargebackRateGt(condition, context);
      case ACCOUNT_AGE_LT_MINUTES -> evaluateAccountAgeLtMinutes(condition, context);
      case IS_VOIP -> evaluateIsVoip(fieldValue);
      case COUNT_DISTINCT_PANS_LAST_N_HOURS ->
          evaluateCountDistinctPansLastNHours(condition, context);
      case COUNT_DISTINCT_ACCOUNTS -> evaluateCountDistinctAccounts(condition, context);

        // Operadores de Tempo/Data
      case IS_WEEKEND -> evaluateIsWeekend(context);
      case IS_HOLIDAY -> evaluateIsHoliday(context);
      case HOUR_BETWEEN -> evaluateHourBetween(condition, context);
      case DAY_OF_WEEK_IN -> evaluateDayOfWeekIn(condition, context);
      case GT_CURRENT_DATE -> evaluateGtCurrentDate(fieldValue);
      case LT_CURRENT_DATE -> evaluateLtCurrentDate(fieldValue);
      case EXPIRES_WITHIN_DAYS -> evaluateExpiresWithinDays(fieldValue, condition);

        // Operadores de Padrão
      case DECIMAL_PLACES_GT -> evaluateDecimalPlacesGt(fieldValue, condition);
      case PATTERN_ROUND_NUMBERS -> evaluatePatternRoundNumbers(fieldValue);
      case GT_FIELD_MULTIPLIER -> evaluateGtFieldMultiplier(condition, context);
      case PERCENTAGE_OF_FIELD -> evaluatePercentageOfField(condition, context);

        // Operadores de Velocity adicionais
      case SUM_LAST_N_HOURS -> evaluateSumLastNHours(condition, context);
      case COUNT_FAILURES_LAST_N_HOURS -> evaluateCountFailuresLastNHours(condition, context);
      case COUNT_DISTINCT_MERCHANTS_LAST_N_HOURS ->
          evaluateCountDistinctMerchantsLastNHours(condition, context);
      case TIME_SINCE_LAST_LT -> evaluateTimeSinceLastLt(condition, context);

        // Operadores de Padrão de Fraude
      case VELOCITY_SPIKE -> evaluateVelocitySpike(condition, context);
      case AMOUNT_SPIKE -> evaluateAmountSpike(condition, context);
      case PATTERN_ESCALATION -> evaluatePatternEscalation(condition, context);
      case PATTERN_SPLIT_TRANSACTION -> evaluatePatternSplitTransaction(condition, context);

        // Operadores de Histórico do Cliente
      case IN_CUSTOMER_HISTORY -> evaluateInCustomerHistory(condition, context);
      case NOT_IN_CUSTOMER_HISTORY -> !evaluateInCustomerHistory(condition, context);
      case IN_CUSTOMER_USUAL_HOURS -> evaluateInCustomerUsualHours(condition, context);
      case NOT_IN_CUSTOMER_USUAL_HOURS -> !evaluateInCustomerUsualHours(condition, context);
      case IN_CUSTOMER_CHARGEBACK_MERCHANTS ->
          evaluateInCustomerChargebackMerchants(condition, context);

        // Operadores de Primeira Ocorrência
      case IS_FIRST -> evaluateIsFirst(condition, context);
      case IS_NEW -> evaluateIsNew(condition, context);
      case DISTANCE_FROM_LAST_GT -> evaluateDistanceFromLastGt(condition, context);

        // ========== OPERADORES V28-V30 (17 novos) ==========
        // Implementados conforme Triple-Check de Especialistas

      case IN_LIST -> evaluateInList(fieldValue, condition);

      case HAS_FAILED_3DS_LAST_N_MINUTES -> evaluateHasFailed3dsLastNMinutes(condition, context);

      case COUNT_MFA_ABANDONMENTS -> evaluateCountMfaAbandonments(condition, context);

      case HAS_INCOMING_TRANSFER_LAST_N_HOURS ->
          evaluateHasIncomingTransferLastNHours(condition, context);

      case IS_IMPOSSIBLE_COMBINATION -> evaluateIsImpossibleCombination(condition, context);

      case PIX_KEY_CHANGED_LAST_N_DAYS -> evaluatePixKeyChangedLastNDays(condition, context);

      case CONTAINS_SUSPICIOUS_KEYWORDS ->
          SuspiciousKeywordEvaluator.containsSuspiciousKeywords(fieldValue, condition);

      case COUNT_CRYPTO_TXN_LAST_N_DAYS -> evaluateCountCryptoTxnLastNDays(condition, context);

      case COUNT_DISTINCT_INSTRUMENTS_LAST_N_DAYS ->
          evaluateCountDistinctInstrumentsLastNDays(condition, context);

      case COUNT_DISTINCT_PAYERS_LAST_N_DAYS ->
          evaluateCountDistinctPayersLastNDays(condition, context);

      case COUNT_DISTINCT_USER_AGENTS_LAST_N_HOURS ->
          evaluateCountDistinctUserAgentsLastNHours(condition, context);

      case COUNT_LAST_N_DAYS -> evaluateCountLastNDays(condition, context);

      case COUNT_MFA_DENIALS_LAST_N_HOURS -> evaluateCountMfaDenialsLastNHours(condition, context);

      case DAYS_SINCE_LAST_ACTIVITY -> evaluateDaysSinceLastActivity(condition, context);

      case DEVICE_CHANGED_IN_SESSION -> evaluateDeviceChangedInSession(condition, context);

      case IS_CRYPTO_RANSOM_AMOUNT -> evaluateIsCryptoRansomAmount(condition, context);

      case OUTFLOW_RATE_LAST_N_DAYS -> evaluateOutflowRateLastNDays(condition, context);

        // ========== OPERADORES V31+ (82 novos) ==========
        // CATEGORIA A: Velocity Avançado (10)
      case VELOCITY_CROSS_CHANNEL ->
          VelocityAdvancedEvaluator.evaluateVelocityCrossChannel(condition, context);
      case VELOCITY_ROLLING_WINDOW ->
          VelocityAdvancedEvaluator.evaluateVelocityRollingWindow(
              condition, context, velocityServiceFacade);
      case VELOCITY_PERCENTILE ->
          VelocityAdvancedEvaluator.evaluateVelocityPercentile(
              condition, context, velocityServiceFacade);
      case VELOCITY_RATIO_GT ->
          VelocityAdvancedEvaluator.evaluateVelocityRatioGt(
              condition, context, velocityServiceFacade);
      case VELOCITY_TREND -> VelocityAdvancedEvaluator.evaluateVelocityTrend(condition, context);
      case COUNT_UNIQUE_BENEFICIARIES_LAST_N_DAYS ->
          VelocityAdvancedEvaluator.evaluateCountUniqueBeneficiariesLastNDays(condition, context);
      case COUNT_UNIQUE_IPS_LAST_N_HOURS ->
          VelocityAdvancedEvaluator.evaluateCountUniqueIpsLastNHours(condition, context);
      case SUM_BY_CHANNEL_LAST_N_DAYS ->
          VelocityAdvancedEvaluator.evaluateSumByChannelLastNDays(condition, context);
      case AVG_INTERVAL_BETWEEN_TXN ->
          VelocityAdvancedEvaluator.evaluateAvgIntervalBetweenTxn(condition, context);
      case VELOCITY_ACCELERATION ->
          VelocityAdvancedEvaluator.evaluateVelocityAcceleration(condition, context);

        // CATEGORIA B: Behavioral Rules (8)
      case DORMANCY_REVIVAL -> evaluateDormancyRevival(condition, context);
      case AMOUNT_DEVIATION_FROM_AVG -> evaluateAmountDeviationFromAvg(condition, context);
      case TIME_DEVIATION_FROM_USUAL -> evaluateTimeDeviationFromUsual(condition, context);
      case MERCHANT_DEVIATION -> evaluateMerchantDeviation(condition, context);
      case MICRO_TRANSACTION_TEST -> evaluateMicroTransactionTest(condition, context);
      case LOCATION_DEVIATION -> evaluateLocationDeviation(condition, context);
      case CHANNEL_SWITCH_PATTERN -> evaluateChannelSwitchPattern(condition, context);
      case BENEFICIARY_REUSE_PATTERN -> evaluateBeneficiaryReusePattern(condition, context);

        // CATEGORIA C: Graph/Network (8)
      case FAN_OUT_COUNT -> GraphNetworkEvaluator.evaluateFanOutCount(condition, context);
      case FAN_IN_COUNT -> GraphNetworkEvaluator.evaluateFanInCount(condition, context);
      case SHARED_DEVICE_COUNT ->
          GraphNetworkEvaluator.evaluateSharedDeviceCount(condition, context);
      case SHARED_IP_COUNT -> GraphNetworkEvaluator.evaluateSharedIpCount(condition, context);
      case ACCOUNT_LINK_DEPTH -> GraphNetworkEvaluator.evaluateAccountLinkDepth(condition, context);
      case CIRCULAR_TRANSFER_DETECTION ->
          GraphNetworkEvaluator.evaluateCircularTransferDetection(condition, context);
      case RAPID_MULTI_HOP -> GraphNetworkEvaluator.evaluateRapidMultiHop(condition, context);
      case BENEFICIARY_CONCENTRATION ->
          GraphNetworkEvaluator.evaluateBeneficiaryConcentration(condition, context);

        // CATEGORIA D: Sanctions & Name Matching (7)
      case OFAC_LIST_CHECK ->
          SanctionsNameMatchingEvaluator.evaluateOfacListCheck(fieldValue, condition);
      case PEP_LIST_CHECK ->
          SanctionsNameMatchingEvaluator.evaluatePepListCheck(fieldValue, condition);
      case ADVERSE_MEDIA_CHECK ->
          SanctionsNameMatchingEvaluator.evaluateAdverseMediaCheck(fieldValue, condition);
      case SANCTIONS_COUNTRY_CHECK ->
          SanctionsNameMatchingEvaluator.evaluateSanctionsCountryCheck(fieldValue, condition);
      case HIGH_RISK_JURISDICTION ->
          SanctionsNameMatchingEvaluator.evaluateHighRiskJurisdiction(fieldValue, condition);
      case NAME_TRANSLITERATION_MATCH ->
          SanctionsNameMatchingEvaluator.evaluateNameTransliterationMatch(condition, context);
      case ALIAS_DETECTION ->
          SanctionsNameMatchingEvaluator.evaluateAliasDetection(condition, context);

        // CATEGORIA E: Synthetic ID Detection (8)
      case CPF_SSN_VALIDATION ->
          IdentityRiskEvaluator.evaluateCpfSsnValidation(fieldValue, condition);
      case PHONE_CARRIER_CHECK ->
          IdentityRiskEvaluator.evaluatePhoneCarrierCheck(fieldValue, condition);
      case EMAIL_DOMAIN_AGE -> IdentityRiskEvaluator.evaluateEmailDomainAge(fieldValue, condition);
      case ADDRESS_VERIFICATION ->
          IdentityRiskEvaluator.evaluateAddressVerification(condition, context);
      case IDENTITY_VELOCITY -> IdentityRiskEvaluator.evaluateIdentityVelocity(condition, context);
      case DEVICE_ACCOUNT_RATIO ->
          IdentityRiskEvaluator.evaluateDeviceAccountRatio(condition, context);
      case EMAIL_PHONE_MISMATCH ->
          IdentityRiskEvaluator.evaluateEmailPhoneMismatch(condition, context);
      case CREDIT_FILE_THIN -> IdentityRiskEvaluator.evaluateCreditFileThin(condition, context);

        // CATEGORIA F: AML Typology (8)
      case STRUCTURING_DETECTION ->
          AmlTypologyEvaluator.evaluateStructuringDetection(condition, context);
      case LAYERING_PATTERN -> AmlTypologyEvaluator.evaluateLayeringPattern(condition, context);
      case RAPID_MOVEMENT -> AmlTypologyEvaluator.evaluateRapidMovement(condition, context);
      case INTEGRATION_PATTERN ->
          AmlTypologyEvaluator.evaluateIntegrationPattern(condition, context);
      case CASH_INTENSIVE_RATIO ->
          AmlTypologyEvaluator.evaluateCashIntensiveRatio(condition, context);
      case UNUSUAL_BUSINESS_PATTERN ->
          AmlTypologyEvaluator.evaluateUnusualBusinessPattern(condition, context);
      case SHELL_COMPANY_INDICATOR ->
          AmlTypologyEvaluator.evaluateShellCompanyIndicator(condition, context);
      case TRADE_BASED_ML_INDICATOR ->
          AmlTypologyEvaluator.evaluateTradeBasedMlIndicator(condition, context);

        // CATEGORIA G: Regulatory (8)
      case SCA_EXEMPTION_TRA ->
          RegulatoryComplianceEvaluator.evaluateScaExemptionTra(condition, context);
      case SCA_EXEMPTION_LOW_VALUE ->
          RegulatoryComplianceEvaluator.evaluateScaExemptionLowValue(condition, context);
      case SCA_EXEMPTION_TRUSTED_BENEFICIARY ->
          RegulatoryComplianceEvaluator.evaluateScaExemptionTrustedBeneficiary(condition, context);
      case SCA_EXEMPTION_RECURRING ->
          RegulatoryComplianceEvaluator.evaluateScaExemptionRecurring(condition, context);
      case PSD3_COP_NAME_MATCH ->
          RegulatoryComplianceEvaluator.evaluatePsd3CopNameMatch(condition, context);
      case DORA_INCIDENT_SEVERITY ->
          RegulatoryComplianceEvaluator.evaluateDoraIncidentSeverity(condition, context);
      case EIDAS_ASSURANCE_LEVEL ->
          RegulatoryComplianceEvaluator.evaluateEidasAssuranceLevel(condition, context);
      case GDPR_DATA_RETENTION_CHECK ->
          RegulatoryComplianceEvaluator.evaluateGdprDataRetentionCheck(condition, context);

        // CATEGORIA H: Device (7)
      case DEVICE_JAILBREAK_ROOTED ->
          DeviceRiskEvaluator.evaluateDeviceJailbreakRooted(condition, context);
      case EMULATOR_DETECTION -> DeviceRiskEvaluator.evaluateEmulatorDetection(condition, context);
      case VPN_PROXY_DETECTION -> DeviceRiskEvaluator.evaluateVpnProxyDetection(condition, context);
      case TOR_EXIT_NODE -> DeviceRiskEvaluator.evaluateTorExitNode(fieldValue, condition);
      case BROWSER_INCONSISTENCY ->
          DeviceRiskEvaluator.evaluateBrowserInconsistency(condition, context);
      case TIMEZONE_MISMATCH -> DeviceRiskEvaluator.evaluateTimezoneMismatch(condition, context);
      case LANGUAGE_MISMATCH -> DeviceRiskEvaluator.evaluateLanguageMismatch(condition, context);

        // CATEGORIA I: Merchant & MCC (7)
      case MCC_HIGH_RISK -> MerchantMccEvaluator.evaluateMccHighRisk(fieldValue, condition);
      case MCC_GAMBLING -> MerchantMccEvaluator.evaluateMccGambling(fieldValue, condition);
      case MCC_CRYPTO -> MerchantMccEvaluator.evaluateMccCrypto(fieldValue, condition);
      case MERCHANT_FIRST_SEEN ->
          MerchantMccEvaluator.evaluateMerchantFirstSeen(condition, context);
      case MERCHANT_COUNTRY_MISMATCH ->
          MerchantMccEvaluator.evaluateMerchantCountryMismatch(condition, context);
      case MERCHANT_CATEGORY_CHANGE ->
          MerchantMccEvaluator.evaluateMerchantCategoryChange(condition, context);
      case MERCHANT_VELOCITY_SPIKE ->
          MerchantMccEvaluator.evaluateMerchantVelocitySpike(
              condition, context, velocityServiceFacade);

        // CATEGORIA J: ISO 20022 (6)
      case PACS008_FIELD_VALIDATION ->
          Iso20022Evaluator.evaluatePacs008FieldValidation(condition, context);
      case REMITTANCE_INFO_ANALYSIS ->
          Iso20022Evaluator.evaluateRemittanceInfoAnalysis(condition, context);
      case PURPOSE_CODE_MISMATCH ->
          Iso20022Evaluator.evaluatePurposeCodeMismatch(condition, context);
      case UETR_DUPLICATE_CHECK -> Iso20022Evaluator.evaluateUetrDuplicateCheck(condition, context);
      case CREDITOR_NAME_VALIDATION ->
          Iso20022Evaluator.evaluateCreditorNameValidation(condition, context);
      case STRUCTURED_ADDRESS_CHECK ->
          Iso20022Evaluator.evaluateStructuredAddressCheck(condition, context);

        // CATEGORIA K: Estatísticos Simples (5)
      case BENFORD_LAW_DEVIATION ->
          SimpleStatsEvaluator.evaluateBenfordLawDeviation(condition, context);
      case Z_SCORE_GT ->
          StatisticalRiskEvaluator.evaluateZScoreGt(condition, context, velocityServiceFacade);
      case STANDARD_DEVIATION_GT ->
          StatisticalRiskEvaluator.evaluateStandardDeviationGt(
              condition, context, velocityServiceFacade);
      case PERCENTILE_GT -> StatisticalRiskEvaluator.evaluatePercentileGt(condition, context);
      case COEFFICIENT_VARIATION_GT ->
          StatisticalRiskEvaluator.evaluateCoefficientVariationGt(
              condition, context, velocityServiceFacade);

        // ========== OPERADORES V4.0 PHASE 1 (40 novos) - Velocity + Device ==========
        // CATEGORIA L: Transaction Count Velocity Avançado (12)
      case TRANSACTION_COUNT_PER_CARD_HOUR ->
          VelocityAdvancedEvaluator.evaluateTransactionCountPerCardHour(
              condition, context, velocityServiceFacade);
      case TRANSACTION_COUNT_PER_IP_HOUR ->
          VelocityAdvancedEvaluator.evaluateTransactionCountPerIpHour(
              condition, context, velocityServiceFacade);
      case TRANSACTION_COUNT_PER_DEVICE_DAY ->
          VelocityAdvancedEvaluator.evaluateTransactionCountPerDeviceDay(
              condition, context, velocityServiceFacade);
      case TRANSACTION_COUNT_PER_MERCHANT_HOUR ->
          VelocityAdvancedEvaluator.evaluateTransactionCountPerMerchantHour(
              condition, context, velocityServiceFacade);
      case TRANSACTION_COUNT_PER_CUSTOMER_HOUR ->
          VelocityAdvancedEvaluator.evaluateTransactionCountPerCustomerHour(
              condition, context, velocityServiceFacade);
      case UNIQUE_CARD_COUNT_PER_IP_HOUR ->
          VelocityAdvancedEvaluator.evaluateUniqueCardCountPerIpHour(
              condition, context, velocityServiceFacade);
      case UNIQUE_MERCHANT_COUNT_PER_CARD_DAY ->
          VelocityAdvancedEvaluator.evaluateUniqueMerchantCountPerCardDay(
              condition, context, velocityServiceFacade);
      case TRANSACTION_ATTEMPT_COUNT_PER_CARD ->
          VelocityAdvancedEvaluator.evaluateTransactionAttemptCountPerCard(
              condition, context, velocityServiceFacade);
      case CVV_FAILURE_VELOCITY ->
          VelocityAdvancedEvaluator.evaluateCvvFailureVelocity(
              condition, context, velocityServiceFacade);
      case ADDRESS_CHANGE_VELOCITY ->
          VelocityAdvancedEvaluator.evaluateAddressChangeVelocity(condition, context);
      case BENEFICIARY_ADD_VELOCITY ->
          VelocityAdvancedEvaluator.evaluateBeneficiaryAddVelocity(condition, context);
      case CARD_ADD_VELOCITY ->
          VelocityAdvancedEvaluator.evaluateCardAddVelocity(condition, context);

        // CATEGORIA M: Amount Velocity Avançado (10)
      case AMOUNT_SUM_PER_CARD_HOUR ->
          VelocityAdvancedEvaluator.evaluateAmountSumPerCardHour(
              condition, context, velocityServiceFacade);
      case AMOUNT_SUM_PER_CUSTOMER_DAY ->
          VelocityAdvancedEvaluator.evaluateAmountSumPerCustomerDay(
              condition, context, velocityServiceFacade);
      case AVG_TRANSACTION_SPIKE ->
          VelocityAdvancedEvaluator.evaluateAvgTransactionSpike(
              condition, context, velocityServiceFacade);
      case LARGE_AMOUNT_FREQUENCY ->
          VelocityAdvancedEvaluator.evaluateLargeAmountFrequency(condition, context);
      case SMALL_AMOUNT_VELOCITY ->
          VelocityAdvancedEvaluator.evaluateSmallAmountVelocity(condition, context);
      case ROUND_AMOUNT_FREQUENCY ->
          VelocityAdvancedEvaluator.evaluateRoundAmountFrequency(condition, context);
      case SEQUENTIAL_AMOUNT_PATTERN ->
          VelocityAdvancedEvaluator.evaluateSequentialAmountPattern(condition, context);
      case AMOUNT_VARIANCE_ANOMALY ->
          VelocityAdvancedEvaluator.evaluateAmountVarianceAnomaly(
              condition, context, velocityServiceFacade);
      case DAILY_LIMIT_PROXIMITY ->
          VelocityAdvancedEvaluator.evaluateDailyLimitProximity(condition, context);
      case WEEKLY_LIMIT_PROXIMITY ->
          VelocityAdvancedEvaluator.evaluateWeeklyLimitProximity(condition, context);

        // CATEGORIA N: Temporal Velocity Avançado (8)
      case TIME_BETWEEN_CONSECUTIVE_TX ->
          TemporalVelocityEvaluator.evaluateTimeBetweenConsecutiveTx(condition, context);
      case TRANSACTION_FREQUENCY_ANOMALY ->
          TemporalVelocityEvaluator.evaluateTransactionFrequencyAnomaly(condition, context);
      case TIME_OF_DAY_ANOMALY ->
          TemporalVelocityEvaluator.evaluateTimeOfDayAnomaly(condition, context);
      case DORMANCY_ALERT_VELOCITY ->
          TemporalVelocityEvaluator.evaluateDormancyAlertVelocity(condition, context);
      case WEEKEND_VS_WEEKDAY_PATTERN ->
          TemporalVelocityEvaluator.evaluateWeekendVsWeekdayPattern(condition, context);
      case HOLIDAY_TRANSACTION_SPIKE ->
          TemporalVelocityEvaluator.evaluateHolidayTransactionSpike(condition, context);
      case NIGHTTIME_TRANSACTION_RATIO ->
          TemporalVelocityEvaluator.evaluateNighttimeTransactionRatio(condition, context);
      case BUSINESS_HOURS_DEVIATION ->
          TemporalVelocityEvaluator.evaluateBusinessHoursDeviation(condition, context);

        // CATEGORIA O: Device Fingerprint Avançado (10)
      case DEVICE_TRUST_SCORE ->
          DeviceFingerprintEvaluator.evaluateDeviceTrustScore(condition, context);
      case CANVAS_FINGERPRINT_MISMATCH ->
          DeviceFingerprintEvaluator.evaluateCanvasFingerprintMismatch(condition, context);
      case WEBGL_FINGERPRINT_ANOMALY ->
          DeviceFingerprintEvaluator.evaluateWebglFingerprintAnomaly(condition, context);
      case AUDIO_FINGERPRINT_NEW ->
          DeviceFingerprintEvaluator.evaluateAudioFingerprintNew(condition, context);
      case FONTS_FINGERPRINT_ANOMALY ->
          DeviceFingerprintEvaluator.evaluateFontsFingerprintAnomaly(condition, context);
      case SCREEN_RESOLUTION_CHANGE ->
          DeviceFingerprintEvaluator.evaluateScreenResolutionChange(condition, context);
      case BATTERY_LEVEL_ANOMALY ->
          DeviceFingerprintEvaluator.evaluateBatteryLevelAnomaly(condition, context);
      case HARDWARE_CONCURRENCY_MISMATCH ->
          DeviceFingerprintEvaluator.evaluateHardwareConcurrencyMismatch(condition, context);
      case TOUCH_SUPPORT_INCONSISTENCY ->
          DeviceFingerprintEvaluator.evaluateTouchSupportInconsistency(condition, context);
      case DEVICE_MEMORY_ANOMALY ->
          DeviceFingerprintEvaluator.evaluateDeviceMemoryAnomaly(condition, context);

        // ========== OPERADORES V4.0 PHASE 1B (25 novos) - Behavioral ==========
        // CATEGORIA P: Behavioral Patterns (15)
      case BEHAVIORAL_BASELINE_DEVIATION ->
          BehavioralPatternEvaluator.evaluateBehavioralBaselineDeviation(condition, context);
      case SPENDING_CATEGORY_SHIFT ->
          BehavioralPatternEvaluator.evaluateSpendingCategoryShift(condition, context);
      case TRANSACTION_SIZE_ESCALATION ->
          BehavioralPatternEvaluator.evaluateTransactionSizeEscalation(condition, context);
      case FREQUENCY_PATTERN_CHANGE ->
          BehavioralPatternEvaluator.evaluateFrequencyPatternChange(condition, context);
      case TIME_PREFERENCE_SHIFT ->
          BehavioralPatternEvaluator.evaluateTimePreferenceShift(condition, context);
      case CHANNEL_USAGE_ANOMALY ->
          BehavioralPatternEvaluator.evaluateChannelUsageAnomaly(condition, context);
      case PAYMENT_METHOD_SWITCH ->
          BehavioralPatternEvaluator.evaluatePaymentMethodSwitch(condition, context);
      case RECIPIENT_DIVERSITY_CHANGE ->
          BehavioralPatternEvaluator.evaluateRecipientDiversityChange(condition, context);
      case GEOGRAPHIC_BEHAVIOR_SHIFT ->
          BehavioralPatternEvaluator.evaluateGeographicBehaviorShift(condition, context);
      case SESSION_BEHAVIOR_ANOMALY ->
          BehavioralPatternEvaluator.evaluateSessionBehaviorAnomaly(condition, context);
      case LOGIN_PATTERN_DEVIATION ->
          BehavioralPatternEvaluator.evaluateLoginPatternDeviation(condition, context);
      case NAVIGATION_PATTERN_ANOMALY ->
          BehavioralPatternEvaluator.evaluateNavigationPatternAnomaly(condition, context);
      case TRANSACTION_TIMING_CLUSTER ->
          BehavioralPatternEvaluator.evaluateTransactionTimingCluster(condition, context);
      case AMOUNT_ROUNDING_BEHAVIOR ->
          BehavioralPatternEvaluator.evaluateAmountRoundingBehavior(condition, context);
      case SPLIT_PAYMENT_PATTERN ->
          BehavioralPatternEvaluator.evaluateSplitPaymentPattern(condition, context);

        // CATEGORIA Q: Statistical Behavioral (10)
      case CHI_SQUARE_DISTRIBUTION_TEST ->
          StatisticalBehavioralEvaluator.evaluateChiSquareDistributionTest(condition, context);
      case KOLMOGOROV_SMIRNOV_TEST ->
          StatisticalBehavioralEvaluator.evaluateKolmogorovSmirnovTest(condition, context);
      case ANDERSON_DARLING_TEST ->
          StatisticalBehavioralEvaluator.evaluateAndersonDarlingTest(condition, context);
      case T_TEST_AMOUNT_DEVIATION ->
          StatisticalBehavioralEvaluator.evaluateTTestAmountDeviation(condition, context);
      case MANN_WHITNEY_U_TEST ->
          StatisticalBehavioralEvaluator.evaluateMannWhitneyUTest(condition, context);
      case CORRELATION_ANOMALY ->
          StatisticalBehavioralEvaluator.evaluateCorrelationAnomaly(condition, context);
      case REGRESSION_RESIDUAL_OUTLIER ->
          StatisticalBehavioralEvaluator.evaluateRegressionResidualOutlier(condition, context);
      case VARIANCE_RATIO_TEST ->
          StatisticalBehavioralEvaluator.evaluateVarianceRatioTest(condition, context);
      case ENTROPY_SCORE_ANOMALY ->
          StatisticalBehavioralEvaluator.evaluateEntropyScoreAnomaly(condition, context);
      case SKEWNESS_KURTOSIS_ANOMALY ->
          SimpleStatsEvaluator.evaluateSkewnessKurtosisAnomaly(condition, context);

        // ========== OPERADORES V4.0 PHASE 1C (18 novos) - MCC & Merchant ==========
        // CATEGORIA R: MCC & Merchant Advanced (18)
      case MCC_CATEGORY_VELOCITY ->
          MerchantAdvancedEvaluator.evaluateMccCategoryVelocity(condition, context);
      case MCC_SPENDING_LIMIT_CHECK ->
          MerchantAdvancedEvaluator.evaluateMccSpendingLimitCheck(condition, context);
      case MCC_CROSS_CATEGORY_PATTERN ->
          MerchantAdvancedEvaluator.evaluateMccCrossCategoryPattern(condition, context);
      case MERCHANT_REPUTATION_SCORE ->
          MerchantAdvancedEvaluator.evaluateMerchantReputationScore(condition, context);
      case MERCHANT_AGE_CHECK ->
          MerchantAdvancedEvaluator.evaluateMerchantAgeCheck(condition, context);
      case MERCHANT_TRANSACTION_VOLUME ->
          MerchantAdvancedEvaluator.evaluateMerchantTransactionVolume(condition, context);
      case MERCHANT_CHARGEBACK_HISTORY ->
          MerchantAdvancedEvaluator.evaluateMerchantChargebackHistory(condition, context);
      case MERCHANT_FRAUD_RATE_CHECK ->
          MerchantAdvancedEvaluator.evaluateMerchantFraudRateCheck(condition, context);
      case MERCHANT_GEOGRAPHIC_SPREAD ->
          MerchantAdvancedEvaluator.evaluateMerchantGeographicSpread(condition, context);
      case MERCHANT_CUSTOMER_CONCENTRATION ->
          MerchantAdvancedEvaluator.evaluateMerchantCustomerConcentration(condition, context);
      case MERCHANT_AMOUNT_DISTRIBUTION ->
          MerchantAdvancedEvaluator.evaluateMerchantAmountDistribution(condition, context);
      case MERCHANT_TIME_PATTERN ->
          MerchantAdvancedEvaluator.evaluateMerchantTimePattern(condition, context);
      case MERCHANT_DEVICE_DIVERSITY ->
          MerchantAdvancedEvaluator.evaluateMerchantDeviceDiversity(condition, context);
      case MERCHANT_REFUND_RATIO ->
          MerchantAdvancedEvaluator.evaluateMerchantRefundRatio(condition, context);
      case MERCHANT_NEW_CUSTOMER_RATIO ->
          MerchantAdvancedEvaluator.evaluateMerchantNewCustomerRatio(condition, context);
      case MERCHANT_DORMANT_REACTIVATION ->
          MerchantAdvancedEvaluator.evaluateMerchantDormantReactivation(condition, context);
      case MERCHANT_CROSS_BORDER_RATIO ->
          MerchantAdvancedEvaluator.evaluateMerchantCrossBorderRatio(condition, context);
      case MERCHANT_HIGH_VALUE_FREQUENCY ->
          MerchantAdvancedEvaluator.evaluateMerchantHighValueFrequency(condition, context);

        // ========== CATEGORIA S: FATF AML Typologies (28) ==========
        // ========== CATEGORIA T: PSD2 SCA Exemptions (12) ==========
        // ========== CATEGORIA U: Platform Best Practices (28) ==========
        // ========== Basel III Operational Risk (BSL001-BSL014) ==========
        // ========== Rule Mining Determinístico (APRIORI, FPGROWTH, ECLAT) ==========
        // ========== Fuzzy Logic (FUZZY001, FUZZY002) ==========
        // ========== LLM & Generative AI Fraud Detection (LLM001-LLM012) ==========

        // ========== Neo4j Graph Fraud Detection (NEO001-NEO018) ==========
      case NEO4J_WEAKLY_CONNECTED_COMPONENTS ->
          Neo4jGraphEvaluator.evaluateNeo4jWeaklyConnectedComponents(
              condition, context, neo4jGraphService);
      case NEO4J_DEGREE_CENTRALITY ->
          Neo4jGraphEvaluator.evaluateNeo4jDegreeCentrality(condition, context, neo4jGraphService);
      case NEO4J_PAGERANK_FRAUD_SCORE ->
          Neo4jGraphEvaluator.evaluateNeo4jPagerankFraudScore(
              condition, context, neo4jGraphService);
      case NEO4J_LOUVAIN_COMMUNITY_DETECTION ->
          Neo4jGraphEvaluator.evaluateNeo4jLouvainCommunityDetection(
              condition, context, neo4jGraphService);
      case NEO4J_PAIRWISE_SIMILARITY_PII ->
          Neo4jGraphEvaluator.evaluateNeo4jPairwiseSimilarityPii(
              condition, context, neo4jGraphService);
      case NEO4J_ENTITY_RESOLUTION_SHARED_PII ->
          Neo4jGraphEvaluator.evaluateNeo4jEntityResolutionSharedPii(
              condition, context, neo4jGraphService);
      case NEO4J_FRAUD_RING_DETECTION ->
          Neo4jGraphEvaluator.evaluateNeo4jFraudRingDetection(
              condition, context, neo4jGraphService);
      case NEO4J_MONEY_MULE_NETWORK_ANALYSIS ->
          Neo4jGraphEvaluator.evaluateNeo4jMoneyMuleNetworkAnalysis(
              condition, context, neo4jGraphService);
      case NEO4J_CIRCULAR_TRANSACTION_DETECTION ->
          Neo4jGraphEvaluator.evaluateNeo4jCircularTransactionDetection(
              condition, context, neo4jGraphService);
      case NEO4J_FIRST_PARTY_FRAUD_CLUSTERING ->
          Neo4jGraphEvaluator.evaluateNeo4jFirstPartyFraudClustering(
              condition, context, neo4jGraphService);
      case NEO4J_SECOND_LEVEL_FRAUDSTER_ID ->
          Neo4jGraphEvaluator.evaluateNeo4jSecondLevelFraudsterId(
              condition, context, neo4jGraphService);
      case NEO4J_BETWEENNESS_CENTRALITY_MULE ->
          Neo4jGraphEvaluator.evaluateNeo4jBetweennessCentralityMule(
              condition, context, neo4jGraphService);
      case NEO4J_LABEL_PROPAGATION_FRAUD_SPREAD ->
          Neo4jGraphEvaluator.evaluateNeo4jLabelPropagationFraudSpread(
              condition, context, neo4jGraphService);
      case NEO4J_SHORTEST_PATH_AML_TRACKING ->
          Neo4jGraphEvaluator.evaluateNeo4jShortestPathAmlTracking(
              condition, context, neo4jGraphService);
      case NEO4J_TRIANGLE_COUNT_COLLUSION ->
          Neo4jGraphEvaluator.evaluateNeo4jTriangleCountCollusion(
              condition, context, neo4jGraphService);
      case NEO4J_NODE_SIMILARITY_SYNTHETIC_ID ->
          Neo4jGraphEvaluator.evaluateNeo4jNodeSimilaritySyntheticId(
              condition, context, neo4jGraphService);
      case NEO4J_GRAPH_EMBEDDING_FRAUD_PREDICTION ->
          Neo4jGraphEvaluator.evaluateNeo4jGraphEmbeddingFraudPrediction(
              condition, context, neo4jGraphService);
      case NEO4J_TEMPORAL_MOTIF_PATTERN ->
          Neo4jGraphEvaluator.evaluateNeo4jTemporalMotifPattern(
              condition, context, neo4jGraphService);

        // ========== Synthetic Identity Detection (SYN001-SYN015) ==========

        // ========== Estatísticos Avançados Pure Rules (STAT001-STAT015) ==========

        // ========== Fraud Patterns & Market Operators (Phase 7) ==========

        // ========== OPERADORES SINCRONIZADOS V49 ==========
        // Operadores lógicos (delegados para grupos)

        // Operadores de anomalia

        // Operadores de dispositivo/sessão

        // Operadores de fraude de cartão

        // Operadores de transferência

        // Operadores de validação

        // Operadores de contexto/classificação

      default -> delegateToRegistry(operator, condition, context);
    };
  }

  /**
   * ARCH-001 FIX: Delega avaliação para OperatorEvaluatorRegistry. Isso permite adicionar novos
   * operadores sem modificar o switch principal.
   */
  private boolean delegateToRegistry(
      ConditionOperator operator, RuleCondition condition, EvaluationContext context) {
    try {
      OperatorEvaluator evaluator = operatorEvaluatorRegistry.getEvaluator(operator);
      log.debug("Delegando operador {} para {}", operator, evaluator.getClass().getSimpleName());
      return evaluator.evaluate(condition, context);
    } catch (UnsupportedOperatorException e) {
      log.error("Operador não suportado: {} - nem no switch nem no registry", operator);
      throw e;
    } catch (Exception e) {
      log.error("Erro ao delegar operador {} para registry: {}", operator, e.getMessage());
      throw new UnsupportedOperatorException(
          operator, "Erro ao avaliar operador via registry: " + e.getMessage());
    }
  }

  // ========== Métodos auxiliares de avaliação ==========

  private Boolean toBoolean(Object value) {
    if (value == null) return null;
    if (value instanceof Boolean) return (Boolean) value;
    String str = String.valueOf(value).toLowerCase();
    return "true".equals(str) || "1".equals(str) || "yes".equals(str);
  }

  private String getExpectedValueString(RuleCondition condition) {
    return ExpectedValueFormatter.format(condition);
  }

  private boolean evaluateEquals(Object fieldValue, String expected, Boolean caseSensitive) {
    return BasicOperatorEvaluator.evaluateEquals(fieldValue, expected, caseSensitive);
  }

  private int compareValues(Object fieldValue, String expected) {
    return BasicOperatorEvaluator.compareValues(fieldValue, expected);
  }

  private boolean evaluateIn(Object fieldValue, List<String> values, Boolean caseSensitive) {
    return BasicOperatorEvaluator.evaluateIn(fieldValue, values, caseSensitive);
  }

  /** Avalia IN_LIST com suporte a valueArray ou valueSingle com delimitador pipe. */
  private boolean evaluateInList(Object fieldValue, RuleCondition condition) {
    return BasicOperatorEvaluator.evaluateInList(fieldValue, condition);
  }

  private boolean evaluateContains(Object fieldValue, String substring, Boolean caseSensitive) {
    return BasicOperatorEvaluator.evaluateContains(fieldValue, substring, caseSensitive);
  }

  private boolean evaluateStartsWith(Object fieldValue, String prefix, Boolean caseSensitive) {
    return BasicOperatorEvaluator.evaluateStartsWith(fieldValue, prefix, caseSensitive);
  }

  private boolean evaluateEndsWith(Object fieldValue, String suffix, Boolean caseSensitive) {
    return BasicOperatorEvaluator.evaluateEndsWith(fieldValue, suffix, caseSensitive);
  }

  private boolean evaluateRegex(Object fieldValue, String pattern) {
    return BasicOperatorEvaluator.evaluateRegex(fieldValue, pattern);
  }

  private boolean evaluateBetween(Object fieldValue, String min, String max) {
    return BasicOperatorEvaluator.evaluateBetween(fieldValue, min, max);
  }

  private boolean evaluateFieldComparison(
      Object fieldValue, String otherFieldName, EvaluationContext context, int comparison) {
    return BasicOperatorEvaluator.evaluateFieldComparison(
        fieldValue, otherFieldName, context, comparison);
  }

  private boolean evaluateDateBefore(Object fieldValue, String dateStr) {
    return BasicOperatorEvaluator.evaluateDateBefore(fieldValue, dateStr);
  }

  private boolean evaluateDateAfter(Object fieldValue, String dateStr) {
    return BasicOperatorEvaluator.evaluateDateAfter(fieldValue, dateStr);
  }

  private boolean evaluateDateBetween(Object fieldValue, String minDate, String maxDate) {
    return BasicOperatorEvaluator.evaluateDateBetween(fieldValue, minDate, maxDate);
  }

  private boolean evaluateTimeBefore(Object fieldValue, String timeStr) {
    return BasicOperatorEvaluator.evaluateTimeBefore(fieldValue, timeStr);
  }

  private boolean evaluateTimeAfter(Object fieldValue, String timeStr) {
    return BasicOperatorEvaluator.evaluateTimeAfter(fieldValue, timeStr);
  }

  private boolean evaluateTimeBetween(Object fieldValue, String minTime, String maxTime) {
    return BasicOperatorEvaluator.evaluateTimeBetween(fieldValue, minTime, maxTime);
  }

  @SuppressWarnings("unchecked")
  private boolean evaluateArrayContains(Object fieldValue, String element) {
    return BasicOperatorEvaluator.evaluateArrayContains(fieldValue, element);
  }

  @SuppressWarnings("unchecked")
  private boolean evaluateArraySize(Object fieldValue, String expectedSize, int comparison) {
    return BasicOperatorEvaluator.evaluateArraySize(fieldValue, expectedSize, comparison);
  }

  private boolean evaluateModulo(
      Object fieldValue, String divisor, String remainder, boolean equals) {
    return BasicOperatorEvaluator.evaluateModulo(fieldValue, divisor, remainder, equals);
  }

  // ========== Métodos utilitários ==========

  // ========== Agregações Temporais Avançadas (DSL Expandida) ==========

  private boolean evaluateSumLastNDays(RuleCondition condition, EvaluationContext context) {
    return VelocityAggregationEvaluator.evaluateSumLastNDays(
        condition, context, velocityServiceFacade);
  }

  private boolean evaluateCountLastNHours(RuleCondition condition, EvaluationContext context) {
    return VelocityAggregationEvaluator.evaluateCountLastNHours(
        condition, context, velocityServiceFacade);
  }

  private boolean evaluateAvgLastNDays(RuleCondition condition, EvaluationContext context) {
    return VelocityAggregationEvaluator.evaluateAvgLastNDays(
        condition, context, velocityServiceFacade);
  }

  private boolean evaluateCountDistinctMerchantsLastNDays(
      RuleCondition condition, EvaluationContext context) {
    return VelocityAggregationEvaluator.evaluateCountDistinctMerchantsLastNDays(
        condition, context, velocityServiceFacade);
  }

  private boolean evaluateCountDistinctCountriesLastNHours(
      RuleCondition condition, EvaluationContext context) {
    return VelocityAggregationEvaluator.evaluateCountDistinctCountriesLastNHours(
        condition, context, velocityServiceFacade);
  }

  private boolean evaluateMaxAmountLastNDays(RuleCondition condition, EvaluationContext context) {
    return VelocityAggregationEvaluator.evaluateMaxAmountLastNDays(
        condition, context, velocityServiceFacade);
  }

  private boolean evaluateMinAmountLastNDays(RuleCondition condition, EvaluationContext context) {
    return VelocityAggregationEvaluator.evaluateMinAmountLastNDays(
        condition, context, velocityServiceFacade);
  }

  // ========== Métodos Auxiliares para Agregações Avançadas ==========

  /** Converte dias para TimeWindow mais próxima */
  private VelocityService.TimeWindow parseTimeWindowFromDays(int days) {
    return TimeWindowParser.fromDays(days);
  }

  /** Converte horas para TimeWindow mais próxima */
  private VelocityService.TimeWindow parseTimeWindowFromHours(int hours) {
    return TimeWindowParser.fromHours(hours);
  }

  /** Extrai o campo de agrupamento do contexto (ex: cardNumber, accountId) */
  private String extractGroupByFromContext(EvaluationContext context) {
    return GroupByExtractor.extractGroupBy(context);
  }

  // ========== Operadores Críticos para Regras de Fraude Avançadas ==========

  /**
   * NOT_IN_HISTORICAL: Verifica se o valor NÃO está no histórico do cliente. Formato:
   * "sourceField:targetField:days" (ex: "customerAcctNumber:beneficiaryId:90") Retorna true se o
   * beneficiário NUNCA foi usado antes pelo cliente nos últimos N dias.
   */
  /**
   * NAME_SIMILARITY_LT: Verifica se a similaridade entre dois nomes é menor que o threshold.
   * Formato: "otherField:threshold" (ex: "shippingName:50") Usa algoritmo de Levenshtein para
   * calcular similaridade.
   */

  /**
   * GTE_PERCENT_OF_LAST_INCOMING: Verifica se o valor é >= X% do último valor recebido. Formato:
   * "percentage" (ex: "80") Usado para detectar saques que drenam a conta após depósito.
   */
  private boolean evaluateGtePercentOfLastIncoming(
      RuleCondition condition, EvaluationContext context) {
    return CriticalOperatorEvaluator.evaluateGtePercentOfLastIncoming(
        condition, context, operatorDataService);
  }

  /**
   * DOMAIN_IN_LIST: Verifica se o domínio do email está em uma lista. Formato:
   * "domain1,domain2,domain3" (ex: "guerrillamail.com,10minutemail.com")
   */
  private boolean evaluateDomainInList(Object fieldValue, RuleCondition condition) {
    return CriticalOperatorEvaluator.evaluateDomainInList(fieldValue, condition);
  }

  /**
   * CHARGEBACK_RATE_GT: Verifica se a taxa de chargeback do merchant é maior que o threshold.
   * Formato: "rate:days" (ex: "2:7" = taxa > 2% nos últimos 7 dias)
   */
  private boolean evaluateChargebackRateGt(RuleCondition condition, EvaluationContext context) {
    return CriticalOperatorEvaluator.evaluateChargebackRateGt(
        condition, context, operatorDataService);
  }

  /**
   * ACCOUNT_AGE_LT_MINUTES: Verifica se a conta tem menos de N minutos de idade. Formato: "minutes"
   * (ex: "10")
   */
  private boolean evaluateAccountAgeLtMinutes(RuleCondition condition, EvaluationContext context) {
    return CriticalOperatorEvaluator.evaluateAccountAgeLtMinutes(
        condition, context, operatorDataService);
  }

  /**
   * IS_VOIP: Verifica se o número de telefone é VoIP. Retorna true se o telefone for identificado
   * como VoIP.
   */
  private boolean evaluateIsVoip(Object fieldValue) {
    return CriticalOperatorEvaluator.evaluateIsVoip(fieldValue, operatorDataService);
  }

  /**
   * COUNT_DISTINCT_PANS_LAST_N_HOURS: Conta PANs distintos associados ao campo nas últimas N horas.
   * Formato: "threshold:hours" (ex: "5:1" = mais de 5 PANs distintos na última hora)
   */
  private boolean evaluateCountDistinctPansLastNHours(
      RuleCondition condition, EvaluationContext context) {
    return VelocityAggregationEvaluator.evaluateCountDistinctPansLastNHours(
        condition, context, velocityServiceFacade);
  }

  /**
   * COUNT_DISTINCT_ACCOUNTS: Conta contas distintas associadas ao campo. Formato: "threshold" (ex:
   * "3" = mais de 3 contas distintas)
   */
  private boolean evaluateCountDistinctAccounts(
      RuleCondition condition, EvaluationContext context) {
    return VelocityAggregationEvaluator.evaluateCountDistinctAccounts(
        condition, context, velocityServiceFacade);
  }

  // ========== Operadores de Tempo/Data ==========

  /** IS_WEEKEND: Verifica se a transação é em fim de semana */
  private boolean evaluateIsWeekend(EvaluationContext context) {
    return TimeDateEvaluator.evaluateIsWeekend(context);
  }

  /** IS_HOLIDAY: Verifica se a transação é em feriado */
  private boolean evaluateIsHoliday(EvaluationContext context) {
    return TimeDateEvaluator.evaluateIsHoliday(context, operatorDataService);
  }

  /** HOUR_BETWEEN: Verifica se a hora está entre dois valores. Formato: "startHour:endHour" */
  private boolean evaluateHourBetween(RuleCondition condition, EvaluationContext context) {
    return TimeDateEvaluator.evaluateHourBetween(condition, context);
  }

  /** DAY_OF_WEEK_IN: Verifica se o dia da semana está na lista. Formato: "1,2,3" (1=Segunda) */
  private boolean evaluateDayOfWeekIn(RuleCondition condition, EvaluationContext context) {
    return TimeDateEvaluator.evaluateDayOfWeekIn(condition, context);
  }

  /** GT_CURRENT_DATE: Verifica se a data é maior que a data atual (futuro) */
  private boolean evaluateGtCurrentDate(Object fieldValue) {
    return TimeDateEvaluator.evaluateGtCurrentDate(fieldValue);
  }

  /** LT_CURRENT_DATE: Verifica se a data é menor que a data atual (passado) */
  private boolean evaluateLtCurrentDate(Object fieldValue) {
    return TimeDateEvaluator.evaluateLtCurrentDate(fieldValue);
  }

  /** EXPIRES_WITHIN_DAYS: Verifica se expira em N dias. Formato: "days" */
  private boolean evaluateExpiresWithinDays(Object fieldValue, RuleCondition condition) {
    return TimeDateEvaluator.evaluateExpiresWithinDays(fieldValue, condition);
  }

  // ========== Operadores de Padrão ==========

  /** DECIMAL_PLACES_GT: Verifica se o número tem mais de N casas decimais */
  private boolean evaluateDecimalPlacesGt(Object fieldValue, RuleCondition condition) {
    return PatternEvaluator.evaluateDecimalPlacesGt(fieldValue, condition);
  }

  /** PATTERN_ROUND_NUMBERS: Verifica se o valor é um número redondo (múltiplo de 100, 500, 1000) */
  private boolean evaluatePatternRoundNumbers(Object fieldValue) {
    return PatternEvaluator.evaluatePatternRoundNumbers(fieldValue);
  }

  /** GT_FIELD_MULTIPLIER: Verifica se campo > outro * fator. Formato: "otherField:multiplier" */
  private boolean evaluateGtFieldMultiplier(RuleCondition condition, EvaluationContext context) {
    return PatternEvaluator.evaluateGtFieldMultiplier(condition, context);
  }

  /**
   * PERCENTAGE_OF_FIELD: Calcula se campo é X% de outro. Formato: "otherField:percentage:operator"
   */
  private boolean evaluatePercentageOfField(RuleCondition condition, EvaluationContext context) {
    return PatternEvaluator.evaluatePercentageOfField(condition, context);
  }

  // ========== Operadores de Velocity Adicionais ==========

  /** SUM_LAST_N_HOURS: Soma nas últimas N horas. Formato: "hours:threshold:operator" */
  private boolean evaluateSumLastNHours(RuleCondition condition, EvaluationContext context) {
    return VelocityAggregationEvaluator.evaluateSumLastNHours(
        condition, context, velocityServiceFacade);
  }

  /** COUNT_FAILURES_LAST_N_HOURS: Contagem de falhas nas últimas N horas */
  private boolean evaluateCountFailuresLastNHours(
      RuleCondition condition, EvaluationContext context) {
    return VelocityAggregationEvaluator.evaluateCountFailuresLastNHours(
        condition, context, operatorDataService, velocityServiceFacade);
  }

  /** COUNT_DISTINCT_MERCHANTS_LAST_N_HOURS: Merchants distintos nas últimas N horas */
  private boolean evaluateCountDistinctMerchantsLastNHours(
      RuleCondition condition, EvaluationContext context) {
    return VelocityAggregationEvaluator.evaluateCountDistinctMerchantsLastNHours(
        condition, context, velocityServiceFacade);
  }

  /** TIME_SINCE_LAST_LT: Tempo desde última transação menor que N minutos */
  private boolean evaluateTimeSinceLastLt(RuleCondition condition, EvaluationContext context) {
    return VelocityAggregationEvaluator.evaluateTimeSinceLastLt(
        condition, context, operatorDataService, velocityServiceFacade);
  }

  // ========== Operadores de Padrão de Fraude ==========

  /** VELOCITY_SPIKE: Detecta spike de velocidade comparado com média histórica */
  private boolean evaluateVelocitySpike(RuleCondition condition, EvaluationContext context) {
    return VelocityAggregationEvaluator.evaluateVelocitySpike(
        condition, context, velocityServiceFacade);
  }

  /** AMOUNT_SPIKE: Detecta spike de valor comparado com média histórica */
  private boolean evaluateAmountSpike(RuleCondition condition, EvaluationContext context) {
    return VelocityAggregationEvaluator.evaluateAmountSpike(
        condition, context, velocityServiceFacade);
  }

  /** PATTERN_ESCALATION: Detecta padrão de valores crescentes (escada) */
  private boolean evaluatePatternEscalation(RuleCondition condition, EvaluationContext context) {
    return VelocityAggregationEvaluator.evaluatePatternEscalation(
        condition, context, velocityServiceFacade);
  }

  /**
   * PATTERN_SPLIT_TRANSACTION: Detecta padrão de split de transação. Múltiplas transações pequenas
   * em curto período que somam um valor maior. Formato: "maxMinutes:minTransactions:totalThreshold"
   */
  private boolean evaluatePatternSplitTransaction(
      RuleCondition condition, EvaluationContext context) {
    return VelocityAggregationEvaluator.evaluatePatternSplitTransaction(
        condition, context, velocityServiceFacade);
  }

  // ========== Operadores de Histórico do Cliente ==========

  /**
   * IN_CUSTOMER_HISTORY: Verifica se o valor do campo está no histórico do cliente. Formato:
   * "fieldToCheck:days" (ex: "merchantId:90" - merchant usado nos últimos 90 dias)
   */
  private boolean evaluateInCustomerHistory(RuleCondition condition, EvaluationContext context) {
    return CustomerHistoryEvaluator.evaluateInCustomerHistory(
        condition, context, velocityServiceFacade);
  }

  /**
   * IN_CUSTOMER_USUAL_HOURS: Verifica se a transação está no horário habitual do cliente. Analisa o
   * padrão de horários das transações anteriores.
   */
  private boolean evaluateInCustomerUsualHours(RuleCondition condition, EvaluationContext context) {
    return CustomerHistoryEvaluator.evaluateInCustomerUsualHours(condition, context);
  }

  private boolean evaluateInCustomerChargebackMerchants(
      RuleCondition condition, EvaluationContext context) {
    return CustomerHistoryEvaluator.evaluateInCustomerChargebackMerchants(
        condition, context, operatorDataService);
  }

  // ========== Operadores de Primeira Ocorrência ==========

  /**
   * IS_FIRST: Verifica se é a primeira ocorrência do valor para o cliente. Formato: "fieldToCheck"
   * (ex: "merchantId" - primeiro uso deste merchant)
   */
  private boolean evaluateIsFirst(RuleCondition condition, EvaluationContext context) {
    return FirstOccurrenceEvaluator.evaluateIsFirst(condition, context, velocityServiceFacade);
  }

  /**
   * IS_NEW: Verifica se o valor é "novo" (visto pela primeira vez recentemente). Formato: "maxDays"
   * (ex: "7" - visto pela primeira vez nos últimos 7 dias)
   */
  private boolean evaluateIsNew(RuleCondition condition, EvaluationContext context) {
    return FirstOccurrenceEvaluator.evaluateIsNew(condition, context, velocityServiceFacade);
  }

  /**
   * DISTANCE_FROM_LAST_GT: Verifica se a distância da última transação é maior que N km. Formato:
   * "distanceKm" (ex: "500" - mais de 500km da última transação)
   */
  private boolean evaluateDistanceFromLastGt(RuleCondition condition, EvaluationContext context) {
    return FirstOccurrenceEvaluator.evaluateDistanceFromLastGt(
        condition, context, velocityServiceFacade);
  }

  // ========== OPERADORES V28-V30 (17 novos métodos) ==========
  // Implementados conforme recomendação do Triple-Check de Especialistas

  /**
   * HAS_FAILED_3DS_LAST_N_MINUTES: Verifica se houve falha 3DS nos últimos N minutos. Formato
   * valueSingle: "minutes" (ex: "30")
   */
  private boolean evaluateHasFailed3dsLastNMinutes(
      RuleCondition condition, EvaluationContext context) {
    return V28V30Evaluator.evaluateHasFailed3dsLastNMinutes(condition, context);
  }

  /**
   * COUNT_MFA_ABANDONMENTS: Contagem de abandonos de MFA. Formato valueSingle: "threshold:hours"
   * (ex: "3:24")
   */
  private boolean evaluateCountMfaAbandonments(RuleCondition condition, EvaluationContext context) {
    return V28V30Evaluator.evaluateCountMfaAbandonments(condition, context);
  }

  /**
   * HAS_INCOMING_TRANSFER_LAST_N_HOURS: Verifica se houve transferência de entrada. Formato
   * valueSingle: "hours" (ex: "24")
   */
  private boolean evaluateHasIncomingTransferLastNHours(
      RuleCondition condition, EvaluationContext context) {
    return V28V30Evaluator.evaluateHasIncomingTransferLastNHours(condition, context);
  }

  /**
   * IS_IMPOSSIBLE_COMBINATION: Verifica combinações impossíveis de dados. Formato valueSingle: tipo
   * de combinação (ex: "age_corporate", "country_currency")
   */
  private boolean evaluateIsImpossibleCombination(
      RuleCondition condition, EvaluationContext context) {
    return V28V30Evaluator.evaluateIsImpossibleCombination(condition, context);
  }

  /**
   * PIX_KEY_CHANGED_LAST_N_DAYS: Verifica se chave PIX foi alterada recentemente. Formato
   * valueSingle: "days" (ex: "7")
   */
  private boolean evaluatePixKeyChangedLastNDays(
      RuleCondition condition, EvaluationContext context) {
    return V28V30Evaluator.evaluatePixKeyChangedLastNDays(condition, context);
  }

  /**
   * CONTAINS_SUSPICIOUS_KEYWORDS: Detecta palavras suspeitas em texto. Formato valueArray: lista de
   * keywords, ou usa lista padrão
   */
  /**
   * COUNT_CRYPTO_TXN_LAST_N_DAYS: Conta transações crypto nos últimos N dias. Formato valueSingle:
   * "threshold|days" (ex: "5|30")
   */
  private boolean evaluateCountCryptoTxnLastNDays(
      RuleCondition condition, EvaluationContext context) {
    return V28V30Evaluator.evaluateCountCryptoTxnLastNDays(condition, context);
  }

  /**
   * COUNT_DISTINCT_INSTRUMENTS_LAST_N_DAYS: Instrumentos distintos nos últimos N dias. Formato
   * valueSingle: "threshold|days" (ex: "10|30")
   */
  private boolean evaluateCountDistinctInstrumentsLastNDays(
      RuleCondition condition, EvaluationContext context) {
    return V28V30Evaluator.evaluateCountDistinctInstrumentsLastNDays(condition, context);
  }

  /**
   * COUNT_DISTINCT_PAYERS_LAST_N_DAYS: Pagadores distintos nos últimos N dias. Formato valueSingle:
   * "threshold|days" (ex: "5|7")
   */
  private boolean evaluateCountDistinctPayersLastNDays(
      RuleCondition condition, EvaluationContext context) {
    return V28V30Evaluator.evaluateCountDistinctPayersLastNDays(condition, context);
  }

  /**
   * COUNT_DISTINCT_USER_AGENTS_LAST_N_HOURS: User agents distintos nas últimas N horas. Formato
   * valueSingle: "threshold|hours" (ex: "5|24")
   */
  private boolean evaluateCountDistinctUserAgentsLastNHours(
      RuleCondition condition, EvaluationContext context) {
    return V28V30Evaluator.evaluateCountDistinctUserAgentsLastNHours(condition, context);
  }

  /**
   * COUNT_LAST_N_DAYS: Contagem genérica nos últimos N dias. Formato valueSingle: "threshold|days"
   * ou "keyType|threshold|days"
   */
  private boolean evaluateCountLastNDays(RuleCondition condition, EvaluationContext context) {
    return VelocityAggregationEvaluator.evaluateCountLastNDays(
        condition, context, velocityServiceFacade);
  }

  /**
   * COUNT_MFA_DENIALS_LAST_N_HOURS: Negações de MFA nas últimas N horas. Formato valueSingle:
   * "threshold:hours" (ex: "3:24")
   */
  private boolean evaluateCountMfaDenialsLastNHours(
      RuleCondition condition, EvaluationContext context) {
    return V28V30Evaluator.evaluateCountMfaDenialsLastNHours(condition, context);
  }

  /**
   * DAYS_SINCE_LAST_ACTIVITY: Dias desde última atividade. Formato valueSingle:
   * "threshold|operator" (ex: "30|GT" = mais de 30 dias)
   */
  private boolean evaluateDaysSinceLastActivity(
      RuleCondition condition, EvaluationContext context) {
    return V28V30Evaluator.evaluateDaysSinceLastActivity(condition, context);
  }

  /** DEVICE_CHANGED_IN_SESSION: Verifica se device mudou durante a sessão. */
  private boolean evaluateDeviceChangedInSession(
      RuleCondition condition, EvaluationContext context) {
    return V28V30Evaluator.evaluateDeviceChangedInSession(condition, context);
  }

  /**
   * IS_CRYPTO_RANSOM_AMOUNT: Detecta valores típicos de ransomware. Verifica se o valor está
   * próximo de quantias típicas de ransom.
   */
  private boolean evaluateIsCryptoRansomAmount(RuleCondition condition, EvaluationContext context) {
    return V28V30Evaluator.evaluateIsCryptoRansomAmount(condition, context);
  }

  /**
   * OUTFLOW_RATE_LAST_N_DAYS: Taxa de saída nos últimos N dias. Formato valueSingle:
   * "threshold|days" (threshold em percentual)
   */
  private boolean evaluateOutflowRateLastNDays(RuleCondition condition, EvaluationContext context) {
    return V28V30Evaluator.evaluateOutflowRateLastNDays(condition, context);
  }

  // ========== OPERADORES V31+ IMPLEMENTADOS - CATEGORIAS A-K ==========

  // --- CATEGORIA B: Behavioral Rules (8) ---

  /**
   * DORMANCY_REVIVAL: Detecta conta reativada após período de dormência. Formato valueSingle:
   * "dormancyDays|txThreshold" (ex: "90|3")
   */
  private boolean evaluateDormancyRevival(RuleCondition condition, EvaluationContext context) {
    return V31BehavioralEvaluator.evaluateDormancyRevival(condition, context);
  }

  /**
   * AMOUNT_DEVIATION_FROM_AVG: Desvio do valor médio histórico. Formato valueSingle:
   * "deviationMultiplier" (ex: "3.0" = 3x desvio padrão)
   */
  private boolean evaluateAmountDeviationFromAvg(
      RuleCondition condition, EvaluationContext context) {
    return V31BehavioralEvaluator.evaluateAmountDeviationFromAvg(
        condition, context, velocityServiceFacade);
  }

  /**
   * TIME_DEVIATION_FROM_USUAL: Desvio do horário usual de transações. Formato valueSingle:
   * "hoursDeviation" (ex: "4" = 4 horas fora do padrão)
   */
  private boolean evaluateTimeDeviationFromUsual(
      RuleCondition condition, EvaluationContext context) {
    return V31BehavioralEvaluator.evaluateTimeDeviationFromUsual(condition, context);
  }

  /**
   * MERCHANT_DEVIATION: Detecta merchant fora do padrão usual do cliente. Formato valueSingle:
   * "true" (verifica se merchant é novo/incomum)
   */
  private boolean evaluateMerchantDeviation(RuleCondition condition, EvaluationContext context) {
    return V31BehavioralEvaluator.evaluateMerchantDeviation(condition, context);
  }

  /**
   * MICRO_TRANSACTION_TEST: Detecta teste com micro-transação. Formato valueSingle:
   * "maxAmount|count" (ex: "1.00|3")
   */
  private boolean evaluateMicroTransactionTest(RuleCondition condition, EvaluationContext context) {
    return V31BehavioralEvaluator.evaluateMicroTransactionTest(condition, context);
  }

  /**
   * LOCATION_DEVIATION: Detecta localização fora do padrão. Formato valueSingle: "kmThreshold" (ex:
   * "100" = 100km da localização usual)
   */
  private boolean evaluateLocationDeviation(RuleCondition condition, EvaluationContext context) {
    return V31BehavioralEvaluator.evaluateLocationDeviation(condition, context);
  }

  /**
   * CHANNEL_SWITCH_PATTERN: Detecta troca suspeita de canal. Formato valueSingle:
   * "switchCount|hours" (ex: "3|1")
   */
  private boolean evaluateChannelSwitchPattern(RuleCondition condition, EvaluationContext context) {
    return V31BehavioralEvaluator.evaluateChannelSwitchPattern(condition, context);
  }

  /**
   * BENEFICIARY_REUSE_PATTERN: Detecta padrão de reutilização de beneficiário. Formato valueSingle:
   * "reuseThreshold" (ex: "5" = mesmo beneficiário 5x)
   */
  private boolean evaluateBeneficiaryReusePattern(
      RuleCondition condition, EvaluationContext context) {
    return V31BehavioralEvaluator.evaluateBeneficiaryReusePattern(condition, context);
  }

  // ========== OPERADORES V4.0 PHASE 1 - VELOCITY + DEVICE (40 novos) ==========

  // --- CATEGORIA L: Transaction Count Velocity Avançado (12) ---

  // ========== PLANNED OPERATORS - Basel SL (Operational Risk) (lançam
  // UnsupportedOperatorException) ==========

  // ========== LLM & Generative AI Fraud Detection (LLM001-LLM012) ==========

  // ========== Neo4j Graph Fraud Detection (NEO001-NEO018) ==========

  // ========== OPERADORES SINCRONIZADOS V49 - IMPLEMENTAÇÕES ==========

  // Operadores lógicos
  private boolean evaluateLogicalAnd(RuleCondition condition, EvaluationContext context) {
    return V49OperatorsEvaluator.evaluateLogicalAnd(condition, context);
  }

  private boolean evaluateLogicalOr(RuleCondition condition, EvaluationContext context) {
    return V49OperatorsEvaluator.evaluateLogicalOr(condition, context);
  }

  private boolean evaluateLogicalNot(RuleCondition condition, EvaluationContext context) {
    return V49OperatorsEvaluator.evaluateLogicalNot(condition, context);
  }

  private boolean evaluateLogicalXor(RuleCondition condition, EvaluationContext context) {
    return V49OperatorsEvaluator.evaluateLogicalXor(condition, context);
  }

  private boolean evaluateLogicalNand(RuleCondition condition, EvaluationContext context) {
    return V49OperatorsEvaluator.evaluateLogicalNand(condition, context);
  }

  private boolean evaluateLogicalNor(RuleCondition condition, EvaluationContext context) {
    return V49OperatorsEvaluator.evaluateLogicalNor(condition, context);
  }

  // Operadores de anomalia
  private boolean evaluateAmountAnomalyOp(RuleCondition condition, EvaluationContext context) {
    return V49OperatorsEvaluator.evaluateAmountAnomalyOp(condition, context);
  }

  private boolean evaluateTimeAnomalyOp(RuleCondition condition, EvaluationContext context) {
    return V49OperatorsEvaluator.evaluateTimeAnomalyOp(condition, context);
  }

  private boolean evaluateVelocityAnomalyOp(RuleCondition condition, EvaluationContext context) {
    return V49OperatorsEvaluator.evaluateVelocityAnomalyOp(condition, context);
  }

  private boolean evaluateMccAnomalyOp(RuleCondition condition, EvaluationContext context) {
    return V49OperatorsEvaluator.evaluateMccAnomalyOp(condition, context);
  }

  private boolean evaluateMerchantAnomalyOp(RuleCondition condition, EvaluationContext context) {
    return V49OperatorsEvaluator.evaluateMerchantAnomalyOp(condition, context);
  }

  // Operadores de dispositivo/sessão
  private boolean evaluateIsNewDeviceOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluateIsNewDeviceOp(fieldValue);
  }

  private boolean evaluateIsNewLocationOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluateIsNewLocationOp(fieldValue);
  }

  private boolean evaluateDeviceFingerprintMismatchOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluateDeviceFingerprintMismatchOp(fieldValue);
  }

  private boolean evaluateSessionDurationLtOp(Object fieldValue, RuleCondition condition) {
    return V49OperatorsEvaluator.evaluateSessionDurationLtOp(fieldValue, condition);
  }

  private boolean evaluateClickVelocityGtOp(Object fieldValue, RuleCondition condition) {
    return V49OperatorsEvaluator.evaluateClickVelocityGtOp(fieldValue, condition);
  }

  private boolean evaluateMouseMovementAnomalyOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluateMouseMovementAnomalyOp(fieldValue);
  }

  private boolean evaluateTypingSpeedAnomalyOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluateTypingSpeedAnomalyOp(fieldValue);
  }

  private boolean evaluateUserAgentSuspiciousOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluateUserAgentSuspiciousOp(fieldValue);
  }

  // Operadores de fraude de cartão
  private boolean evaluateExpiredCardOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluateExpiredCardOp(fieldValue);
  }

  private boolean evaluateCardCaptureFraudOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluateCardCaptureFraudOp(fieldValue);
  }

  private boolean evaluatePinCvvLimitExceededOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluatePinCvvLimitExceededOp(fieldValue);
  }

  private boolean evaluateOfflinePinFailedOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluateOfflinePinFailedOp(fieldValue);
  }

  private boolean evaluateEmvSecurityCheckOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluateEmvSecurityCheckOp(fieldValue);
  }

  private boolean evaluateEcommerceNoAvsOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluateEcommerceNoAvsOp(fieldValue);
  }

  private boolean evaluatePosSecurityMissingOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluatePosSecurityMissingOp(fieldValue);
  }

  private boolean evaluateTerminalVerificationFailedOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluateTerminalVerificationFailedOp(fieldValue);
  }

  private boolean evaluateSuspiciousTerminalOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluateSuspiciousTerminalOp(fieldValue);
  }

  private boolean evaluateUnusualCardMediaOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluateUnusualCardMediaOp(fieldValue);
  }

  // Operadores de transferência
  private boolean evaluateTransferAmountGtOp(Object fieldValue, RuleCondition condition) {
    return V49OperatorsEvaluator.evaluateTransferAmountGtOp(fieldValue, condition);
  }

  private boolean evaluateTransferVelocityGtOp(Object fieldValue, RuleCondition condition) {
    return V49OperatorsEvaluator.evaluateTransferVelocityGtOp(fieldValue, condition);
  }

  private boolean evaluateRecipientInWatchlistOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluateRecipientInWatchlistOp(fieldValue);
  }

  private boolean evaluateRecipientIsNewOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluateRecipientIsNewOp(fieldValue);
  }

  // Operadores de validação
  private boolean evaluateAddressMismatchOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluateAddressMismatchOp(fieldValue);
  }

  private boolean evaluatePhoneCountryMismatchOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluatePhoneCountryMismatchOp(fieldValue);
  }

  private boolean evaluateEmailDomainAgeLtDaysOp(Object fieldValue, RuleCondition condition) {
    return V49OperatorsEvaluator.evaluateEmailDomainAgeLtDaysOp(fieldValue, condition);
  }

  private boolean evaluateAccountAgeLtDaysOp(Object fieldValue, RuleCondition condition) {
    return V49OperatorsEvaluator.evaluateAccountAgeLtDaysOp(fieldValue, condition);
  }

  // Operadores de contexto/classificação
  private boolean evaluateContextOp(
      Object fieldValue, RuleCondition condition, EvaluationContext context) {
    return V49OperatorsEvaluator.evaluateContextOp(fieldValue, condition, context);
  }

  private boolean evaluateFraudOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluateFraudOp(fieldValue);
  }

  private boolean evaluateSecurityOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluateSecurityOp(fieldValue);
  }

  private boolean evaluateSuspiciousOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluateSuspiciousOp(fieldValue);
  }

  private boolean evaluateSuspiciousTransactionTypeOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluateSuspiciousTransactionTypeOp(fieldValue);
  }

  private boolean evaluateVelocityOp(Object fieldValue, RuleCondition condition) {
    return V49OperatorsEvaluator.evaluateVelocityOp(fieldValue, condition);
  }

  private boolean evaluateRoundAmountOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluateRoundAmountOp(fieldValue);
  }

  private boolean evaluateImpossibleTravelOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluateImpossibleTravelOp(fieldValue);
  }

  private boolean evaluateNotInListOp(Object fieldValue, RuleCondition condition) {
    return V49OperatorsEvaluator.evaluateNotInListOp(fieldValue, condition);
  }

  private boolean evaluateCaptchaFailedOp(Object fieldValue) {
    return V49OperatorsEvaluator.evaluateCaptchaFailedOp(fieldValue);
  }

  private boolean evaluateCountDistinctCountriesLastNDaysOp(
      RuleCondition condition, EvaluationContext context) {
    return V49OperatorsEvaluator.evaluateCountDistinctCountriesLastNDaysOp(condition, context);
  }
}
