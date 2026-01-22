package com.rulex.service.complex;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleConditionGroup;
import com.rulex.entity.complex.RuleExecutionDetail;
import com.rulex.exception.UnsupportedOperatorException;
import com.rulex.service.FuzzyLogicService;
import com.rulex.service.GeoService;
import com.rulex.service.Neo4jGraphService;
import com.rulex.service.OperatorDataService;
import com.rulex.service.StatisticalAnalysisService;
import com.rulex.service.StringSimilarityService;
import com.rulex.service.VelocityService;
import com.rulex.service.VelocityServiceFacade;
import com.rulex.service.complex.evaluation.AmlTypologyEvaluator;
import com.rulex.service.complex.evaluation.AssociationPlannedEvaluator;
import com.rulex.service.complex.evaluation.BslPlannedEvaluator;
import com.rulex.service.complex.evaluation.BehavioralPatternEvaluator;
import com.rulex.service.complex.evaluation.DeviceFingerprintEvaluator;
import com.rulex.service.complex.evaluation.DeviceRiskEvaluator;
import com.rulex.service.complex.evaluation.FatfPlannedEvaluator;
import com.rulex.service.complex.evaluation.FraudPatternPlannedEvaluator;
import com.rulex.service.complex.evaluation.FuzzyPlannedEvaluator;
import com.rulex.service.complex.evaluation.GraphNetworkEvaluator;
import com.rulex.service.complex.evaluation.IdentityRiskEvaluator;
import com.rulex.service.complex.evaluation.Iso20022Evaluator;
import com.rulex.service.complex.evaluation.LlmPlannedEvaluator;
import com.rulex.service.complex.evaluation.MerchantAdvancedEvaluator;
import com.rulex.service.complex.evaluation.NameSimilarityEvaluator;
import com.rulex.service.complex.evaluation.Neo4jGraphEvaluator;
import com.rulex.service.complex.evaluation.ScaPlannedEvaluator;
import com.rulex.service.complex.evaluation.RegulatoryComplianceEvaluator;
import com.rulex.service.complex.evaluation.SanctionsNameMatchingEvaluator;
import com.rulex.service.complex.evaluation.StatisticalBehavioralEvaluator;
import com.rulex.service.complex.evaluation.StatisticalRiskEvaluator;
import com.rulex.service.complex.evaluation.StatisticalPlannedEvaluator;
import com.rulex.service.complex.evaluation.SyntheticPlannedEvaluator;
import com.rulex.service.complex.evaluation.SuspiciousKeywordEvaluator;
import com.rulex.service.complex.evaluation.HistoricalEvaluator;
import com.rulex.service.complex.evaluation.VelocityAggregationEvaluator;
import com.rulex.service.complex.evaluation.VelocityAdvancedEvaluator;
import com.rulex.service.complex.evaluator.OperatorEvaluator;
import com.rulex.service.complex.evaluator.OperatorEvaluatorRegistry;
import com.rulex.service.complex.evaluation.PlatformPlannedEvaluator;
import com.rulex.service.complex.evaluation.TemporalVelocityEvaluator;
import com.rulex.util.RegexValidator;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.Normalizer;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.regex.Pattern;
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

  private final GeoService geoService;
  private final VelocityService velocityService;
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
      case AND -> results.stream().allMatch(r -> r);
      case OR -> results.stream().anyMatch(r -> r);
      case NOT -> !results.get(0); // NOT aplica ao primeiro resultado
      case XOR -> results.stream().filter(r -> r).count() == 1;
      case NAND -> !results.stream().allMatch(r -> r);
      case NOR -> !results.stream().anyMatch(r -> r);
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
      case GEO_DISTANCE_LT -> evaluateGeoDistanceLt(condition, context);
      case GEO_DISTANCE_GT -> evaluateGeoDistanceGt(condition, context);
      case GEO_IN_POLYGON -> evaluateGeoInPolygon(condition, context);

        // Velocity (agregações temporais)
      case VELOCITY_COUNT_GT -> evaluateVelocityCount(condition, context, true);
      case VELOCITY_COUNT_LT -> evaluateVelocityCount(condition, context, false);
      case VELOCITY_SUM_GT -> evaluateVelocitySum(condition, context, true);
      case VELOCITY_SUM_LT -> evaluateVelocitySum(condition, context, false);
      case VELOCITY_AVG_GT -> evaluateVelocityAvg(condition, context, true);
      case VELOCITY_AVG_LT -> evaluateVelocityAvg(condition, context, false);
      case VELOCITY_DISTINCT_GT -> evaluateVelocityDistinct(condition, context, true);
      case VELOCITY_DISTINCT_LT -> evaluateVelocityDistinct(condition, context, false);

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
        case NAME_SIMILARITY_LT -> NameSimilarityEvaluator.evaluateNameSimilarityLt(condition, context);
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
          VelocityAdvancedEvaluator.evaluateVelocityRatioGt(condition, context, velocityServiceFacade);
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
        case SHARED_DEVICE_COUNT -> GraphNetworkEvaluator.evaluateSharedDeviceCount(condition, context);
        case SHARED_IP_COUNT -> GraphNetworkEvaluator.evaluateSharedIpCount(condition, context);
        case ACCOUNT_LINK_DEPTH -> GraphNetworkEvaluator.evaluateAccountLinkDepth(condition, context);
        case CIRCULAR_TRANSFER_DETECTION ->
          GraphNetworkEvaluator.evaluateCircularTransferDetection(condition, context);
        case RAPID_MULTI_HOP -> GraphNetworkEvaluator.evaluateRapidMultiHop(condition, context);
        case BENEFICIARY_CONCENTRATION ->
          GraphNetworkEvaluator.evaluateBeneficiaryConcentration(condition, context);

        // CATEGORIA D: Sanctions & Name Matching (7)
        case OFAC_LIST_CHECK -> SanctionsNameMatchingEvaluator.evaluateOfacListCheck(fieldValue, condition);
        case PEP_LIST_CHECK -> SanctionsNameMatchingEvaluator.evaluatePepListCheck(fieldValue, condition);
        case ADVERSE_MEDIA_CHECK ->
          SanctionsNameMatchingEvaluator.evaluateAdverseMediaCheck(fieldValue, condition);
        case SANCTIONS_COUNTRY_CHECK ->
          SanctionsNameMatchingEvaluator.evaluateSanctionsCountryCheck(fieldValue, condition);
        case HIGH_RISK_JURISDICTION ->
          SanctionsNameMatchingEvaluator.evaluateHighRiskJurisdiction(fieldValue, condition);
        case NAME_TRANSLITERATION_MATCH ->
          SanctionsNameMatchingEvaluator.evaluateNameTransliterationMatch(condition, context);
        case ALIAS_DETECTION -> SanctionsNameMatchingEvaluator.evaluateAliasDetection(condition, context);

        // CATEGORIA E: Synthetic ID Detection (8)
      case CPF_SSN_VALIDATION -> IdentityRiskEvaluator.evaluateCpfSsnValidation(fieldValue, condition);
      case PHONE_CARRIER_CHECK -> IdentityRiskEvaluator.evaluatePhoneCarrierCheck(fieldValue, condition);
      case EMAIL_DOMAIN_AGE -> IdentityRiskEvaluator.evaluateEmailDomainAge(fieldValue, condition);
      case ADDRESS_VERIFICATION -> IdentityRiskEvaluator.evaluateAddressVerification(condition, context);
      case IDENTITY_VELOCITY -> IdentityRiskEvaluator.evaluateIdentityVelocity(condition, context);
      case DEVICE_ACCOUNT_RATIO -> IdentityRiskEvaluator.evaluateDeviceAccountRatio(condition, context);
      case EMAIL_PHONE_MISMATCH -> IdentityRiskEvaluator.evaluateEmailPhoneMismatch(condition, context);
      case CREDIT_FILE_THIN -> IdentityRiskEvaluator.evaluateCreditFileThin(condition, context);

        // CATEGORIA F: AML Typology (8)
        case STRUCTURING_DETECTION ->
          AmlTypologyEvaluator.evaluateStructuringDetection(condition, context);
        case LAYERING_PATTERN -> AmlTypologyEvaluator.evaluateLayeringPattern(condition, context);
        case RAPID_MOVEMENT -> AmlTypologyEvaluator.evaluateRapidMovement(condition, context);
        case INTEGRATION_PATTERN -> AmlTypologyEvaluator.evaluateIntegrationPattern(condition, context);
        case CASH_INTENSIVE_RATIO -> AmlTypologyEvaluator.evaluateCashIntensiveRatio(condition, context);
        case UNUSUAL_BUSINESS_PATTERN ->
          AmlTypologyEvaluator.evaluateUnusualBusinessPattern(condition, context);
        case SHELL_COMPANY_INDICATOR ->
          AmlTypologyEvaluator.evaluateShellCompanyIndicator(condition, context);
        case TRADE_BASED_ML_INDICATOR ->
          AmlTypologyEvaluator.evaluateTradeBasedMlIndicator(condition, context);

        // CATEGORIA G: Regulatory (8)
        case SCA_EXEMPTION_TRA -> RegulatoryComplianceEvaluator.evaluateScaExemptionTra(condition, context);
        case SCA_EXEMPTION_LOW_VALUE ->
          RegulatoryComplianceEvaluator.evaluateScaExemptionLowValue(condition, context);
        case SCA_EXEMPTION_TRUSTED_BENEFICIARY ->
          RegulatoryComplianceEvaluator.evaluateScaExemptionTrustedBeneficiary(condition, context);
        case SCA_EXEMPTION_RECURRING ->
          RegulatoryComplianceEvaluator.evaluateScaExemptionRecurring(condition, context);
        case PSD3_COP_NAME_MATCH -> RegulatoryComplianceEvaluator.evaluatePsd3CopNameMatch(condition, context);
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
      case MCC_HIGH_RISK -> evaluateMccHighRisk(fieldValue, condition);
      case MCC_GAMBLING -> evaluateMccGambling(fieldValue, condition);
      case MCC_CRYPTO -> evaluateMccCrypto(fieldValue, condition);
      case MERCHANT_FIRST_SEEN -> evaluateMerchantFirstSeen(condition, context);
      case MERCHANT_COUNTRY_MISMATCH -> evaluateMerchantCountryMismatch(condition, context);
      case MERCHANT_CATEGORY_CHANGE -> evaluateMerchantCategoryChange(condition, context);
      case MERCHANT_VELOCITY_SPIKE -> evaluateMerchantVelocitySpike(condition, context);

        // CATEGORIA J: ISO 20022 (6)
      case PACS008_FIELD_VALIDATION -> Iso20022Evaluator.evaluatePacs008FieldValidation(condition, context);
      case REMITTANCE_INFO_ANALYSIS -> Iso20022Evaluator.evaluateRemittanceInfoAnalysis(condition, context);
      case PURPOSE_CODE_MISMATCH -> Iso20022Evaluator.evaluatePurposeCodeMismatch(condition, context);
      case UETR_DUPLICATE_CHECK -> Iso20022Evaluator.evaluateUetrDuplicateCheck(condition, context);
      case CREDITOR_NAME_VALIDATION -> Iso20022Evaluator.evaluateCreditorNameValidation(condition, context);
      case STRUCTURED_ADDRESS_CHECK -> Iso20022Evaluator.evaluateStructuredAddressCheck(condition, context);

        // CATEGORIA K: Estatísticos Simples (5)
      case BENFORD_LAW_DEVIATION -> evaluateBenfordLawDeviation(condition, context);
        case Z_SCORE_GT ->
          StatisticalRiskEvaluator.evaluateZScoreGt(condition, context, velocityServiceFacade);
        case STANDARD_DEVIATION_GT ->
          StatisticalRiskEvaluator.evaluateStandardDeviationGt(condition, context, velocityServiceFacade);
        case PERCENTILE_GT -> StatisticalRiskEvaluator.evaluatePercentileGt(condition, context);
        case COEFFICIENT_VARIATION_GT ->
          StatisticalRiskEvaluator.evaluateCoefficientVariationGt(condition, context, velocityServiceFacade);

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
        case CARD_ADD_VELOCITY -> VelocityAdvancedEvaluator.evaluateCardAddVelocity(condition, context);

        // CATEGORIA M: Amount Velocity Avançado (10)
        case AMOUNT_SUM_PER_CARD_HOUR ->
          VelocityAdvancedEvaluator.evaluateAmountSumPerCardHour(
            condition, context, velocityServiceFacade);
        case AMOUNT_SUM_PER_CUSTOMER_DAY ->
          VelocityAdvancedEvaluator.evaluateAmountSumPerCustomerDay(
            condition, context, velocityServiceFacade);
        case AVG_TRANSACTION_SPIKE ->
          VelocityAdvancedEvaluator.evaluateAvgTransactionSpike(condition, context);
        case LARGE_AMOUNT_FREQUENCY ->
          VelocityAdvancedEvaluator.evaluateLargeAmountFrequency(condition, context);
        case SMALL_AMOUNT_VELOCITY -> VelocityAdvancedEvaluator.evaluateSmallAmountVelocity(condition, context);
        case ROUND_AMOUNT_FREQUENCY ->
          VelocityAdvancedEvaluator.evaluateRoundAmountFrequency(condition, context);
        case SEQUENTIAL_AMOUNT_PATTERN ->
          VelocityAdvancedEvaluator.evaluateSequentialAmountPattern(condition, context);
        case AMOUNT_VARIANCE_ANOMALY ->
          VelocityAdvancedEvaluator.evaluateAmountVarianceAnomaly(condition, context);
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
        case DEVICE_TRUST_SCORE -> DeviceFingerprintEvaluator.evaluateDeviceTrustScore(condition, context);
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
          StatisticalBehavioralEvaluator.evaluateSkewnessKurtosisAnomaly(condition, context);

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
        case MERCHANT_AGE_CHECK -> MerchantAdvancedEvaluator.evaluateMerchantAgeCheck(condition, context);
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
        case FATF_PLACEMENT_CASH_INTENSIVE ->
          FatfPlannedEvaluator.evaluateFatfPlacementCashIntensive(condition, context);
        case FATF_PLACEMENT_STRUCTURING ->
          FatfPlannedEvaluator.evaluateFatfPlacementStructuring(condition, context);
        case FATF_PLACEMENT_SMURFING ->
          FatfPlannedEvaluator.evaluateFatfPlacementSmurfing(condition, context);
        case FATF_PLACEMENT_CURRENCY_EXCHANGE ->
          FatfPlannedEvaluator.evaluateFatfPlacementCurrencyExchange(condition, context);
        case FATF_PLACEMENT_CASINO_GAMBLING ->
          FatfPlannedEvaluator.evaluateFatfPlacementCasinoGambling(condition, context);
        case FATF_LAYERING_RAPID_MOVEMENT ->
          FatfPlannedEvaluator.evaluateFatfLayeringRapidMovement(condition, context);
        case FATF_LAYERING_SHELL_COMPANY ->
          FatfPlannedEvaluator.evaluateFatfLayeringShellCompany(condition, context);
        case FATF_LAYERING_OFFSHORE ->
          FatfPlannedEvaluator.evaluateFatfLayeringOffshore(condition, context);
        case FATF_LAYERING_WIRE_CHAINS ->
          FatfPlannedEvaluator.evaluateFatfLayeringWireChains(condition, context);
        case FATF_LAYERING_CONVERTIBLE_INSTRUMENTS ->
          FatfPlannedEvaluator.evaluateFatfLayeringConvertibleInstruments(condition, context);
        case FATF_INTEGRATION_REAL_ESTATE ->
          FatfPlannedEvaluator.evaluateFatfIntegrationRealEstate(condition, context);
        case FATF_INTEGRATION_LUXURY_GOODS ->
          FatfPlannedEvaluator.evaluateFatfIntegrationLuxuryGoods(condition, context);
        case FATF_INTEGRATION_BUSINESS_INVESTMENT ->
          FatfPlannedEvaluator.evaluateFatfIntegrationBusinessInvestment(condition, context);
        case FATF_INTEGRATION_LOAN_REPAYMENT ->
          FatfPlannedEvaluator.evaluateFatfIntegrationLoanRepayment(condition, context);
        case FATF_TBML_OVER_INVOICING ->
          FatfPlannedEvaluator.evaluateFatfTbmlOverInvoicing(condition, context);
        case FATF_TBML_UNDER_INVOICING ->
          FatfPlannedEvaluator.evaluateFatfTbmlUnderInvoicing(condition, context);
        case FATF_TBML_PHANTOM_SHIPPING ->
          FatfPlannedEvaluator.evaluateFatfTbmlPhantomShipping(condition, context);
        case FATF_TBML_MULTIPLE_INVOICING ->
          FatfPlannedEvaluator.evaluateFatfTbmlMultipleInvoicing(condition, context);
        case FATF_TBML_FALSE_DESCRIPTION ->
          FatfPlannedEvaluator.evaluateFatfTbmlFalseDescription(condition, context);
        case FATF_HAWALA_INFORMAL ->
          FatfPlannedEvaluator.evaluateFatfHawalaInformal(condition, context);
        case FATF_NEW_PAYMENT_EXPLOITATION ->
          FatfPlannedEvaluator.evaluateFatfNewPaymentExploitation(condition, context);
        case FATF_CRYPTO_MIXING ->
          FatfPlannedEvaluator.evaluateFatfCryptoMixing(condition, context);
        case FATF_CRYPTO_ATM_CASHOUT ->
          FatfPlannedEvaluator.evaluateFatfCryptoAtmCashout(condition, context);
        case FATF_PEP_TRANSACTION ->
          FatfPlannedEvaluator.evaluateFatfPepTransaction(condition, context);
        case FATF_CORRESPONDENT_LAYERING ->
          FatfPlannedEvaluator.evaluateFatfCorrespondentLayering(condition, context);
        case FATF_ROUND_TRIPPING ->
          FatfPlannedEvaluator.evaluateFatfRoundTripping(condition, context);
        case FATF_BLACK_MARKET_EXCHANGE ->
          FatfPlannedEvaluator.evaluateFatfBlackMarketExchange(condition, context);
        case FATF_INSURANCE_CASH_VALUE ->
          FatfPlannedEvaluator.evaluateFatfInsuranceCashValue(condition, context);

        // ========== CATEGORIA T: PSD2 SCA Exemptions (12) ==========
        case SCA_LOW_VALUE_EXEMPTION ->
          ScaPlannedEvaluator.evaluateScaLowValueExemption(condition, context);
        case SCA_CONTACTLESS_EXEMPTION ->
          ScaPlannedEvaluator.evaluateScaContactlessExemption(condition, context);
        case SCA_TRA_EXEMPTION -> ScaPlannedEvaluator.evaluateScaTraExemption(condition, context);
        case SCA_TRUSTED_BENEFICIARY ->
          ScaPlannedEvaluator.evaluateScaTrustedBeneficiary(condition, context);
        case SCA_RECURRING_TRANSACTION ->
          ScaPlannedEvaluator.evaluateScaRecurringTransaction(condition, context);
        case SCA_MERCHANT_INITIATED ->
          ScaPlannedEvaluator.evaluateScaMerchantInitiated(condition, context);
        case SCA_CORPORATE_PAYMENT ->
          ScaPlannedEvaluator.evaluateScaCorporatePayment(condition, context);
        case SCA_SECURE_CORPORATE_PROTOCOL ->
          ScaPlannedEvaluator.evaluateScaSecureCorporateProtocol(condition, context);
        case SCA_LIABILITY_SHIFT -> ScaPlannedEvaluator.evaluateScaLiabilityShift(condition, context);
        case SCA_DYNAMIC_3DS_ROUTING ->
          ScaPlannedEvaluator.evaluateScaDynamic3dsRouting(condition, context);
        case SCA_FRAUD_RATE_MONITORING ->
          ScaPlannedEvaluator.evaluateScaFraudRateMonitoring(condition, context);
        case SCA_CHALLENGE_MANDATORY ->
          ScaPlannedEvaluator.evaluateScaChallengeMandatory(condition, context);

        // ========== CATEGORIA U: Platform Best Practices (28) ==========
        case PLT_BEHAVIOR_SORTED_LISTS ->
          PlatformPlannedEvaluator.evaluatePltBehaviorSortedLists(condition, context);
        case PLT_BUSINESS_RULES_SCENARIO ->
          PlatformPlannedEvaluator.evaluatePltBusinessRulesScenario(condition, context);
        case PLT_IDENTITY_RESOLUTION ->
          PlatformPlannedEvaluator.evaluatePltIdentityResolution(condition, context);
        case PLT_COMPROMISE_MANAGER ->
          PlatformPlannedEvaluator.evaluatePltCompromiseManager(condition, context);
        case PLT_INTELLIGENCE_NETWORK ->
          PlatformPlannedEvaluator.evaluatePltIntelligenceNetwork(condition, context);
        case PLT_RULES_MODELS_HYBRID ->
          PlatformPlannedEvaluator.evaluatePltRulesModelsHybrid(condition, context);
        case PLT_BEHAVIORAL_PROFILING ->
          PlatformPlannedEvaluator.evaluatePltBehavioralProfiling(condition, context);
        case PLT_NETWORK_ANALYTICS ->
          PlatformPlannedEvaluator.evaluatePltNetworkAnalytics(condition, context);
        case PLT_SAR_AUTOMATED -> PlatformPlannedEvaluator.evaluatePltSarAutomated(condition, context);
        case PLT_DS2_RULE_ENGINE -> PlatformPlannedEvaluator.evaluatePltDs2RuleEngine(condition, context);
        case PLT_REAL_TIME_DETECTION ->
          PlatformPlannedEvaluator.evaluatePltRealTimeDetection(condition, context);
        case PLT_NETWORK_ENTITY_RESOLUTION ->
          PlatformPlannedEvaluator.evaluatePltNetworkEntityResolution(condition, context);
        case PLT_SCENARIO_SCORECARD ->
          PlatformPlannedEvaluator.evaluatePltScenarioScorecard(condition, context);
        case PLT_RADAR_RULE_BACKTESTING ->
          PlatformPlannedEvaluator.evaluatePltRadarRuleBacktesting(condition, context);
        case PLT_RADAR_METADATA_MATCHING ->
          PlatformPlannedEvaluator.evaluatePltRadarMetadataMatching(condition, context);
        case PLT_RADAR_INLINE_LISTS ->
          PlatformPlannedEvaluator.evaluatePltRadarInlineLists(condition, context);
        case PLT_RADAR_COMPLEX_CONDITIONS ->
          PlatformPlannedEvaluator.evaluatePltRadarComplexConditions(condition, context);
        case PLT_RISK_PROFILE_ASSIGNMENT ->
          PlatformPlannedEvaluator.evaluatePltRiskProfileAssignment(condition, context);
        case PLT_CUSTOM_RULE_BUILDER ->
          PlatformPlannedEvaluator.evaluatePltCustomRuleBuilder(condition, context);
        case PLT_RISK_LIST_COMPARISON ->
          PlatformPlannedEvaluator.evaluatePltRiskListComparison(condition, context);
        case PLT_BACKTESTING_LABELING ->
          PlatformPlannedEvaluator.evaluatePltBacktestingLabeling(condition, context);
        case PLT_ML_FRAUD_RISK_OUTCOME ->
          PlatformPlannedEvaluator.evaluatePltMlFraudRiskOutcome(condition, context);
        case PLT_RISK_SCORE_CALCULATION ->
          PlatformPlannedEvaluator.evaluatePltRiskScoreCalculation(condition, context);
        case PLT_VELOCITY_FILTERS ->
          PlatformPlannedEvaluator.evaluatePltVelocityFilters(condition, context);
        case PLT_LINKING_VELOCITY ->
          PlatformPlannedEvaluator.evaluatePltLinkingVelocity(condition, context);
        case PLT_BAD_ENTITY_NETWORK ->
          PlatformPlannedEvaluator.evaluatePltBadEntityNetwork(condition, context);
        case PLT_REVIEWLIST_QUEUE ->
          PlatformPlannedEvaluator.evaluatePltReviewlistQueue(condition, context);
        case PLT_CONSORTIUM_DATA_CHECK ->
          PlatformPlannedEvaluator.evaluatePltConsortiumDataCheck(condition, context);

        // ========== Basel III Operational Risk (BSL001-BSL014) ==========
        case BSL_BUSINESS_INDICATOR ->
          BslPlannedEvaluator.evaluateBslBusinessIndicator(condition, context);
        case BSL_BUSINESS_INDICATOR_COMPONENT ->
          BslPlannedEvaluator.evaluateBslBusinessIndicatorComponent(condition, context);
        case BSL_INTERNAL_LOSS_MULTIPLIER ->
          BslPlannedEvaluator.evaluateBslInternalLossMultiplier(condition, context);
        case BSL_LOSS_DATA_COLLECTION ->
          BslPlannedEvaluator.evaluateBslLossDataCollection(condition, context);
        case BSL_LOSS_EXCLUSION_APPROVAL ->
          BslPlannedEvaluator.evaluateBslLossExclusionApproval(condition, context);
        case BSL_BUCKET_CLASSIFICATION ->
          BslPlannedEvaluator.evaluateBslBucketClassification(condition, context);
        case BSL_MARGINAL_COEFFICIENT ->
          BslPlannedEvaluator.evaluateBslMarginalCoefficient(condition, context);
        case BSL_LOSS_THRESHOLD_SETTING ->
          BslPlannedEvaluator.evaluateBslLossThresholdSetting(condition, context);
        case BSL_RETENTION_PERIOD ->
          BslPlannedEvaluator.evaluateBslRetentionPeriod(condition, context);
        case BSL_RISK_GOVERNANCE ->
          BslPlannedEvaluator.evaluateBslRiskGovernance(condition, context);
        case BSL_LOSS_EVENT_REPORTING ->
          BslPlannedEvaluator.evaluateBslLossEventReporting(condition, context);
        case BSL_CONTROL_DEFICIENCY ->
          BslPlannedEvaluator.evaluateBslControlDeficiency(condition, context);
        case BSL_KRI_MONITORING ->
          BslPlannedEvaluator.evaluateBslKriMonitoring(condition, context);
        case BSL_SCENARIO_ANALYSIS ->
          BslPlannedEvaluator.evaluateBslScenarioAnalysis(condition, context);

        // ========== Rule Mining Determinístico (APRIORI, FPGROWTH, ECLAT) ==========
        case APRIORI_ASSOCIATION ->
          AssociationPlannedEvaluator.evaluateAprioriAssociation(condition, context);
        case FPGROWTH_FREQUENT_PATTERNS ->
          AssociationPlannedEvaluator.evaluateFpgrowthFrequentPatterns(condition, context);
        case ECLAT_ITEMSET -> AssociationPlannedEvaluator.evaluateEclatItemset(condition, context);

        // ========== Fuzzy Logic (FUZZY001, FUZZY002) ==========
        case FUZZY_MEMBERSHIP -> FuzzyPlannedEvaluator.evaluateFuzzyMembership(condition, context);
        case FUZZY_ADAPTIVE_THRESHOLD ->
          FuzzyPlannedEvaluator.evaluateFuzzyAdaptiveThreshold(condition, context);

        // ========== LLM & Generative AI Fraud Detection (LLM001-LLM012) ==========
        case LLM_TRANSACTION_DESCRIPTION_ANALYSIS ->
          LlmPlannedEvaluator.evaluateLlmTransactionDescriptionAnalysis(condition, context);
        case LLM_GENERATIVE_RULE_SYNTHESIS ->
          LlmPlannedEvaluator.evaluateLlmGenerativeRuleSynthesis(condition, context);
        case LLM_ANOMALY_EXPLANATION_GENERATION ->
          LlmPlannedEvaluator.evaluateLlmAnomalyExplanationGeneration(condition, context);
        case LLM_CHATBOT_FRAUD_DETECTION ->
          LlmPlannedEvaluator.evaluateLlmChatbotFraudDetection(condition, context);
        case LLM_DEEPFAKE_VOICE_DETECTION ->
          LlmPlannedEvaluator.evaluateLlmDeepfakeVoiceDetection(condition, context);
        case LLM_SYNTHETIC_IMAGE_DETECTION ->
          LlmPlannedEvaluator.evaluateLlmSyntheticImageDetection(condition, context);
        case LLM_EMAIL_PHISHING_ANALYSIS ->
          LlmPlannedEvaluator.evaluateLlmEmailPhishingAnalysis(condition, context);
        case LLM_SOCIAL_ENGINEERING_CLASSIFICATION ->
          LlmPlannedEvaluator.evaluateLlmSocialEngineeringClassification(condition, context);
        case LLM_FRAUD_ALERT_PRIORITIZATION ->
          LlmPlannedEvaluator.evaluateLlmFraudAlertPrioritization(condition, context);
        case LLM_MULTI_MODAL_FRAUD_DETECTION ->
          LlmPlannedEvaluator.evaluateLlmMultiModalFraudDetection(condition, context);
        case LLM_ADVERSARIAL_ATTACK_RESISTANCE ->
          LlmPlannedEvaluator.evaluateLlmAdversarialAttackResistance(condition, context);
        case LLM_FRAUD_PATTERN_AUTODISCOVERY ->
          LlmPlannedEvaluator.evaluateLlmFraudPatternAutodiscovery(condition, context);

        // ========== Neo4j Graph Fraud Detection (NEO001-NEO018) ==========
        case NEO4J_WEAKLY_CONNECTED_COMPONENTS ->
          Neo4jGraphEvaluator.evaluateNeo4jWeaklyConnectedComponents(
            condition, context, neo4jGraphService);
        case NEO4J_DEGREE_CENTRALITY ->
          Neo4jGraphEvaluator.evaluateNeo4jDegreeCentrality(
            condition, context, neo4jGraphService);
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
        case BIOMETRIC_KEYSTROKE_DYNAMICS ->
          SyntheticPlannedEvaluator.evaluateBiometricKeystrokeDynamics(condition, context);
        case BIOMETRIC_MOUSE_MOVEMENT ->
          SyntheticPlannedEvaluator.evaluateBiometricMouseMovement(condition, context);
        case BIOMETRIC_SCROLL_VELOCITY ->
          SyntheticPlannedEvaluator.evaluateBiometricScrollVelocity(condition, context);
        case DEVICE_FINGERPRINT_CONSISTENCY_CHECK ->
          SyntheticPlannedEvaluator.evaluateDeviceFingerprintConsistencyCheck(condition, context);
        case ECBSV_SSN_VALIDATION ->
          SyntheticPlannedEvaluator.evaluateEcbsvSsnValidation(condition, context);
        case SYNTHETIC_FRAUD_SCORE ->
          SyntheticPlannedEvaluator.evaluateSyntheticFraudScore(condition, context);
        case INJECTION_ATTACK_DETECTION ->
          SyntheticPlannedEvaluator.evaluateInjectionAttackDetection(condition, context);
        case LIVENESS_DETECTION_FACIAL ->
          SyntheticPlannedEvaluator.evaluateLivenessDetectionFacial(condition, context);
        case LIVENESS_DETECTION_VOICE ->
          SyntheticPlannedEvaluator.evaluateLivenessDetectionVoice(condition, context);
        case ANTI_DETECT_BROWSER_DETECTION ->
          SyntheticPlannedEvaluator.evaluateAntiDetectBrowserDetection(condition, context);
        case DOCUMENT_FORGERY_DETECTION ->
          SyntheticPlannedEvaluator.evaluateDocumentForgeryDetection(condition, context);
        case FACE_TO_ID_PHOTO_MATCHING ->
          SyntheticPlannedEvaluator.evaluateFaceToIdPhotoMatching(condition, context);
        case ADAPTIVE_BEHAVIORAL_ANALYTICS ->
          SyntheticPlannedEvaluator.evaluateAdaptiveBehavioralAnalytics(condition, context);
        case SYNTHETIC_ID_LABEL_CORRECTION ->
          SyntheticPlannedEvaluator.evaluateSyntheticIdLabelCorrection(condition, context);
        case MULTI_LAYERED_SYNTHETIC_ID_CONTROLS ->
          SyntheticPlannedEvaluator.evaluateMultiLayeredSyntheticIdControls(condition, context);

        // ========== Estatísticos Avançados Pure Rules (STAT001-STAT015) ==========
        case STAT_KRUSKAL_WALLIS_TEST ->
          StatisticalPlannedEvaluator.evaluateStatKruskalWallisTest(condition, context);
        case STAT_ANOVA_F_TEST ->
          StatisticalPlannedEvaluator.evaluateStatAnovaFTest(condition, context);
        case STAT_ISOLATION_FOREST_SCORE ->
          StatisticalPlannedEvaluator.evaluateStatIsolationForestScore(condition, context);
        case STAT_LOCAL_OUTLIER_FACTOR ->
          StatisticalPlannedEvaluator.evaluateStatLocalOutlierFactor(condition, context);
        case STAT_ONE_CLASS_SVM_BOUNDARY ->
          StatisticalPlannedEvaluator.evaluateStatOneClassSvmBoundary(condition, context);
        case STAT_KMEANS_CLUSTER_DISTANCE ->
          StatisticalPlannedEvaluator.evaluateStatKmeansClusterDistance(condition, context);
        case STAT_DBSCAN_NOISE_DETECTION ->
          StatisticalPlannedEvaluator.evaluateStatDbscanNoiseDetection(condition, context);
        case STAT_GMM_PROBABILITY ->
          StatisticalPlannedEvaluator.evaluateStatGmmProbability(condition, context);
        case STAT_MAHALANOBIS_DISTANCE ->
          StatisticalPlannedEvaluator.evaluateStatMahalanobisDistance(condition, context);
        case STAT_GRUBBS_TEST ->
          StatisticalPlannedEvaluator.evaluateStatGrubbsTest(condition, context);
        case STAT_DIXON_Q_TEST ->
          StatisticalPlannedEvaluator.evaluateStatDixonQTest(condition, context);
        case STAT_SHAPIRO_WILK_TEST ->
          StatisticalPlannedEvaluator.evaluateStatShapiroWilkTest(condition, context);
        case STAT_LEVENE_TEST -> StatisticalPlannedEvaluator.evaluateStatLeveneTest(condition, context);
        case STAT_WELCH_T_TEST ->
          StatisticalPlannedEvaluator.evaluateStatWelchTTest(condition, context);
        case STAT_BOOTSTRAP_CONFIDENCE_INTERVAL ->
          StatisticalPlannedEvaluator.evaluateStatBootstrapConfidenceInterval(condition, context);

        // ========== Fraud Patterns & Market Operators (Phase 7) ==========
        case CARD_TESTING_RING_DETECTION ->
          FraudPatternPlannedEvaluator.evaluateCardTestingRingDetection(condition, context);
        case BUST_OUT_PATTERN_DETECTION ->
          FraudPatternPlannedEvaluator.evaluateBustOutPatternDetection(condition, context);
        case CIRCULAR_PAYMENT_DETECTION ->
          FraudPatternPlannedEvaluator.evaluateCircularPaymentDetection(condition, context);
        case ACCOUNT_TAKEOVER_PATTERN ->
          FraudPatternPlannedEvaluator.evaluateAccountTakeoverPattern(condition, context);
        case SYNTHETIC_IDENTITY_RING ->
          FraudPatternPlannedEvaluator.evaluateSyntheticIdentityRing(condition, context);
        case CROSS_BORDER_VELOCITY ->
          FraudPatternPlannedEvaluator.evaluateCrossBorderVelocity(condition, context);
        case CORRESPONDENT_ANOMALY ->
          FraudPatternPlannedEvaluator.evaluateCorrespondentAnomaly(condition, context);
        case NESTED_CORRESPONDENT_CHECK ->
          FraudPatternPlannedEvaluator.evaluateNestedCorrespondentCheck(condition, context);
        case SHELL_BANK_INDICATOR ->
          FraudPatternPlannedEvaluator.evaluateShellBankIndicator(condition, context);
        case HIGH_RISK_CORRIDOR_CHECK ->
          FraudPatternPlannedEvaluator.evaluateHighRiskCorridorCheck(condition, context);
        case SEGMENT_OF_ONE_PROFILING ->
          FraudPatternPlannedEvaluator.evaluateSegmentOfOneProfiling(condition, context);
        case ADAPTIVE_PARAMETRIC_THRESHOLD ->
          FraudPatternPlannedEvaluator.evaluateAdaptiveParametricThreshold(condition, context);
        case REAL_TIME_RISK_SCORING ->
          FraudPatternPlannedEvaluator.evaluateRealTimeRiskScoring(condition, context);
        case CONSORTIUM_NEGATIVE_FILE_CHECK ->
          FraudPatternPlannedEvaluator.evaluateConsortiumNegativeFileCheck(condition, context);
        case PEER_GROUP_DEVIATION_SCORE ->
          FraudPatternPlannedEvaluator.evaluatePeerGroupDeviationScore(condition, context);
        case MICRO_DEPOSIT_VELOCITY ->
          FraudPatternPlannedEvaluator.evaluateMicroDepositVelocity(condition, context);
        case RAPID_SUCCESSION_PATTERN ->
          FraudPatternPlannedEvaluator.evaluateRapidSuccessionPattern(condition, context);
        case SPLIT_TRANSACTION_DETECTION ->
          FraudPatternPlannedEvaluator.evaluateSplitTransactionDetection(condition, context);
        case ROUND_TRIP_DETECTION ->
          FraudPatternPlannedEvaluator.evaluateRoundTripDetection(condition, context);
        case LAYERED_TRANSFER_PATTERN ->
          FraudPatternPlannedEvaluator.evaluateLayeredTransferPattern(condition, context);
        case APP_FRAUD_DETECTION ->
          FraudPatternPlannedEvaluator.evaluateAppFraudDetection(condition, context);
        case ROMANCE_SCAM_INDICATOR ->
          FraudPatternPlannedEvaluator.evaluateRomanceScamIndicator(condition, context);
        case INVESTMENT_SCAM_PATTERN ->
          FraudPatternPlannedEvaluator.evaluateInvestmentScamPattern(condition, context);
        case CRYPTO_PUMP_DUMP_DETECTION ->
          FraudPatternPlannedEvaluator.evaluateCryptoPumpDumpDetection(condition, context);
        case PIG_BUTCHERING_INDICATOR ->
          FraudPatternPlannedEvaluator.evaluatePigButcheringIndicator(condition, context);

        // ========== OPERADORES SINCRONIZADOS V49 ==========
        // Operadores lógicos (delegados para grupos)
      case AND -> evaluateLogicalAnd(condition, context);
      case OR -> evaluateLogicalOr(condition, context);
      case NOT -> evaluateLogicalNot(condition, context);
      case XOR -> evaluateLogicalXor(condition, context);
      case NAND -> evaluateLogicalNand(condition, context);
      case NOR -> evaluateLogicalNor(condition, context);

        // Operadores de anomalia
      case AMOUNT_ANOMALY -> evaluateAmountAnomalyOp(condition, context);
      case TIME_ANOMALY -> evaluateTimeAnomalyOp(condition, context);
      case VELOCITY_ANOMALY -> evaluateVelocityAnomalyOp(condition, context);
      case MCC_ANOMALY -> evaluateMccAnomalyOp(condition, context);
      case MERCHANT_ANOMALY -> evaluateMerchantAnomalyOp(condition, context);

        // Operadores de dispositivo/sessão
      case IS_NEW_DEVICE -> evaluateIsNewDeviceOp(fieldValue);
      case IS_NEW_LOCATION -> evaluateIsNewLocationOp(fieldValue);
      case DEVICE_FINGERPRINT_MISMATCH -> evaluateDeviceFingerprintMismatchOp(fieldValue);
      case SESSION_DURATION_LT -> evaluateSessionDurationLtOp(fieldValue, condition);
      case CLICK_VELOCITY_GT -> evaluateClickVelocityGtOp(fieldValue, condition);
      case MOUSE_MOVEMENT_ANOMALY -> evaluateMouseMovementAnomalyOp(fieldValue);
      case TYPING_SPEED_ANOMALY -> evaluateTypingSpeedAnomalyOp(fieldValue);
      case USER_AGENT_SUSPICIOUS -> evaluateUserAgentSuspiciousOp(fieldValue);

        // Operadores de fraude de cartão
      case EXPIRED_CARD -> evaluateExpiredCardOp(fieldValue);
      case CARD_CAPTURE_FRAUD -> evaluateCardCaptureFraudOp(fieldValue);
      case PIN_CVV_LIMIT_EXCEEDED -> evaluatePinCvvLimitExceededOp(fieldValue);
      case OFFLINE_PIN_FAILED -> evaluateOfflinePinFailedOp(fieldValue);
      case EMV_SECURITY_CHECK -> evaluateEmvSecurityCheckOp(fieldValue);
      case ECOMMERCE_NO_AVS -> evaluateEcommerceNoAvsOp(fieldValue);
      case POS_SECURITY_MISSING -> evaluatePosSecurityMissingOp(fieldValue);
      case TERMINAL_VERIFICATION_FAILED -> evaluateTerminalVerificationFailedOp(fieldValue);
      case SUSPICIOUS_TERMINAL -> evaluateSuspiciousTerminalOp(fieldValue);
      case UNUSUAL_CARD_MEDIA -> evaluateUnusualCardMediaOp(fieldValue);

        // Operadores de transferência
      case TRANSFER_AMOUNT_GT -> evaluateTransferAmountGtOp(fieldValue, condition);
      case TRANSFER_VELOCITY_GT -> evaluateTransferVelocityGtOp(fieldValue, condition);
      case RECIPIENT_IN_WATCHLIST -> evaluateRecipientInWatchlistOp(fieldValue);
      case RECIPIENT_IS_NEW -> evaluateRecipientIsNewOp(fieldValue);

        // Operadores de validação
      case ADDRESS_MISMATCH -> evaluateAddressMismatchOp(fieldValue);
      case PHONE_COUNTRY_MISMATCH -> evaluatePhoneCountryMismatchOp(fieldValue);
      case EMAIL_DOMAIN_AGE_LT_DAYS -> evaluateEmailDomainAgeLtDaysOp(fieldValue, condition);
      case NAME_SIMILARITY_GT -> evaluateNameSimilarityGtOp(fieldValue, condition);
      case ACCOUNT_AGE_LT_DAYS -> evaluateAccountAgeLtDaysOp(fieldValue, condition);

        // Operadores de contexto/classificação
      case CONTEXT -> evaluateContextOp(fieldValue, condition, context);
      case FRAUD -> evaluateFraudOp(fieldValue);
      case SECURITY -> evaluateSecurityOp(fieldValue);
      case SUSPICIOUS -> evaluateSuspiciousOp(fieldValue);
      case SUSPICIOUS_TRANSACTION_TYPE -> evaluateSuspiciousTransactionTypeOp(fieldValue);
      case VELOCITY -> evaluateVelocityOp(fieldValue, condition);
      case ROUND_AMOUNT -> evaluateRoundAmountOp(fieldValue);
      case IMPOSSIBLE_TRAVEL -> evaluateImpossibleTravelOp(fieldValue);
      case NOT_IN_LIST -> evaluateNotInListOp(fieldValue, condition);
      case CAPTCHA_FAILED -> evaluateCaptchaFailedOp(fieldValue);
      case COUNT_DISTINCT_COUNTRIES_LAST_N_DAYS -> evaluateCountDistinctCountriesLastNDaysOp(condition, context);

      default -> delegateToRegistry(operator, condition, context);
    };
  }

  /**
   * ARCH-001 FIX: Delega avaliação para OperatorEvaluatorRegistry.
   * Isso permite adicionar novos operadores sem modificar o switch principal.
   */
  private boolean delegateToRegistry(ConditionOperator operator, RuleCondition condition, EvaluationContext context) {
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
          operator,
          "Erro ao avaliar operador via registry: " + e.getMessage());
    }
  }

  // ========== Métodos auxiliares de avaliação ==========

  private boolean evaluateEquals(Object fieldValue, String expected, Boolean caseSensitive) {
    if (fieldValue == null && expected == null) return true;
    if (fieldValue == null || expected == null) return false;

    String fieldStr = String.valueOf(fieldValue);
    if (Boolean.FALSE.equals(caseSensitive)) {
      return fieldStr.equalsIgnoreCase(expected);
    }
    return fieldStr.equals(expected);
  }

  private int compareValues(Object fieldValue, String expected) {
    if (fieldValue == null || expected == null) return 0;

    try {
      BigDecimal fieldNum = new BigDecimal(String.valueOf(fieldValue));
      BigDecimal expectedNum = new BigDecimal(expected);
      return fieldNum.compareTo(expectedNum);
    } catch (NumberFormatException e) {
      return String.valueOf(fieldValue).compareTo(expected);
    }
  }

  private boolean evaluateIn(Object fieldValue, List<String> values, Boolean caseSensitive) {
    if (fieldValue == null || values == null) return false;

    String fieldStr = String.valueOf(fieldValue);
    for (String value : values) {
      if (Boolean.FALSE.equals(caseSensitive)) {
        if (fieldStr.equalsIgnoreCase(value)) return true;
      } else {
        if (fieldStr.equals(value)) return true;
      }
    }
    return false;
  }

  /** Avalia IN_LIST com suporte a valueArray ou valueSingle com delimitador pipe. */
  private boolean evaluateInList(Object fieldValue, RuleCondition condition) {
    if (fieldValue == null) return false;

    List<String> values;
    if (condition.getValueArray() != null && !condition.getValueArray().isEmpty()) {
      values = condition.getValueArray();
    } else if (condition.getValueSingle() != null && !condition.getValueSingle().isEmpty()) {
      // Suporte a valueSingle com delimitador pipe
      values = java.util.Arrays.asList(condition.getValueSingle().split("\\|"));
    } else {
      return false;
    }

    String fieldStr = String.valueOf(fieldValue);
    Boolean caseSensitive = condition.getCaseSensitive();

    for (String value : values) {
      String trimmedValue = value.trim();
      if (Boolean.FALSE.equals(caseSensitive)) {
        if (fieldStr.equalsIgnoreCase(trimmedValue)) return true;
      } else {
        // Default: case-insensitive for IN_LIST
        if (fieldStr.equalsIgnoreCase(trimmedValue)) return true;
      }
    }
    return false;
  }

  private boolean evaluateContains(Object fieldValue, String substring, Boolean caseSensitive) {
    if (fieldValue == null || substring == null) return false;

    String fieldStr = String.valueOf(fieldValue);
    if (Boolean.FALSE.equals(caseSensitive)) {
      return fieldStr.toLowerCase().contains(substring.toLowerCase());
    }
    return fieldStr.contains(substring);
  }

  private boolean evaluateStartsWith(Object fieldValue, String prefix, Boolean caseSensitive) {
    if (fieldValue == null || prefix == null) return false;

    String fieldStr = String.valueOf(fieldValue);
    if (Boolean.FALSE.equals(caseSensitive)) {
      return fieldStr.toLowerCase().startsWith(prefix.toLowerCase());
    }
    return fieldStr.startsWith(prefix);
  }

  private boolean evaluateEndsWith(Object fieldValue, String suffix, Boolean caseSensitive) {
    if (fieldValue == null || suffix == null) return false;

    String fieldStr = String.valueOf(fieldValue);
    if (Boolean.FALSE.equals(caseSensitive)) {
      return fieldStr.toLowerCase().endsWith(suffix.toLowerCase());
    }
    return fieldStr.endsWith(suffix);
  }

  private boolean evaluateRegex(Object fieldValue, String pattern) {
    if (fieldValue == null || pattern == null) return false;

    // Validar pattern contra ReDoS antes de executar
    RegexValidator.ValidationResult validation = RegexValidator.validate(pattern);
    if (!validation.valid()) {
      log.warn("Regex rejeitada por segurança: {} - {}", pattern, validation.errorMessage());
      return false;
    }

    try {
      // GAP-C FIX: Usar safeMatches que valida pattern + aplica timeout
      return RegexValidator.safeMatches(pattern, String.valueOf(fieldValue));
    } catch (Exception e) {
      log.warn("Erro ao avaliar regex '{}': {}", pattern, e.getMessage());
      return false;
    }
  }

  private boolean evaluateBetween(Object fieldValue, String min, String max) {
    if (fieldValue == null || min == null || max == null) return false;

    try {
      BigDecimal value = new BigDecimal(String.valueOf(fieldValue));
      BigDecimal minVal = new BigDecimal(min);
      BigDecimal maxVal = new BigDecimal(max);
      return value.compareTo(minVal) >= 0 && value.compareTo(maxVal) <= 0;
    } catch (NumberFormatException e) {
      return false;
    }
  }

  private boolean evaluateFieldComparison(
      Object fieldValue, String otherFieldName, EvaluationContext context, int comparison) {
    Object otherValue = getFieldValue(otherFieldName, null, context);
    int result = compareValues(fieldValue, String.valueOf(otherValue));

    return switch (comparison) {
      case 0 -> result == 0; // EQ
      case 1 -> result > 0; // GT
      case 2 -> result >= 0; // GTE
      case -1 -> result < 0; // LT
      case -2 -> result <= 0; // LTE
      default -> false;
    };
  }

  private boolean evaluateDateBefore(Object fieldValue, String dateStr) {
    try {
      LocalDate fieldDate = parseDate(fieldValue);
      LocalDate compareDate = LocalDate.parse(dateStr);
      return fieldDate != null && fieldDate.isBefore(compareDate);
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateDateAfter(Object fieldValue, String dateStr) {
    try {
      LocalDate fieldDate = parseDate(fieldValue);
      LocalDate compareDate = LocalDate.parse(dateStr);
      return fieldDate != null && fieldDate.isAfter(compareDate);
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateDateBetween(Object fieldValue, String minDate, String maxDate) {
    try {
      LocalDate fieldDate = parseDate(fieldValue);
      LocalDate min = LocalDate.parse(minDate);
      LocalDate max = LocalDate.parse(maxDate);
      return fieldDate != null && !fieldDate.isBefore(min) && !fieldDate.isAfter(max);
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateTimeBefore(Object fieldValue, String timeStr) {
    try {
      LocalTime fieldTime = parseTime(fieldValue);
      LocalTime compareTime = LocalTime.parse(timeStr);
      return fieldTime != null && fieldTime.isBefore(compareTime);
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateTimeAfter(Object fieldValue, String timeStr) {
    try {
      LocalTime fieldTime = parseTime(fieldValue);
      LocalTime compareTime = LocalTime.parse(timeStr);
      return fieldTime != null && fieldTime.isAfter(compareTime);
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateTimeBetween(Object fieldValue, String minTime, String maxTime) {
    try {
      LocalTime fieldTime = parseTime(fieldValue);
      LocalTime min = LocalTime.parse(minTime);
      LocalTime max = LocalTime.parse(maxTime);
      return fieldTime != null && !fieldTime.isBefore(min) && !fieldTime.isAfter(max);
    } catch (Exception e) {
      return false;
    }
  }

  @SuppressWarnings("unchecked")
  private boolean evaluateArrayContains(Object fieldValue, String element) {
    if (fieldValue == null) return false;

    if (fieldValue instanceof List) {
      return ((List<Object>) fieldValue)
          .stream().anyMatch(item -> String.valueOf(item).equals(element));
    }
    return false;
  }

  @SuppressWarnings("unchecked")
  private boolean evaluateArraySize(Object fieldValue, String expectedSize, int comparison) {
    if (fieldValue == null || expectedSize == null) return false;

    int size = 0;
    if (fieldValue instanceof List) {
      size = ((List<Object>) fieldValue).size();
    } else if (fieldValue.getClass().isArray()) {
      size = java.lang.reflect.Array.getLength(fieldValue);
    } else {
      return false;
    }

    int expected = Integer.parseInt(expectedSize);
    return switch (comparison) {
      case 0 -> size == expected;
      case 1 -> size > expected;
      case -1 -> size < expected;
      default -> false;
    };
  }

  private boolean evaluateModulo(
      Object fieldValue, String divisor, String remainder, boolean equals) {
    try {
      long value = Long.parseLong(String.valueOf(fieldValue));
      long div = Long.parseLong(divisor);
      long rem = Long.parseLong(remainder);
      boolean result = (value % div) == rem;
      return equals ? result : !result;
    } catch (Exception e) {
      return false;
    }
  }

  // ========== Métodos utilitários ==========

  private LocalDate parseDate(Object value) {
    return DateTimeParser.parseDate(value);
  }

  private LocalTime parseTime(Object value) {
    return DateTimeParser.parseTime(value);
  }

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
    try {
      int percentage = Integer.parseInt(condition.getValueSingle().trim());
      Object fieldValue = getFieldValue(condition.getFieldName(), null, context);

      if (fieldValue == null) {
        return false;
      }

      BigDecimal currentAmount = new BigDecimal(String.valueOf(fieldValue));

      // Obter ID do cliente
      Object customerIdObj = getFieldValue("customerIdFromHeader", null, context);
      if (customerIdObj == null) {
        customerIdObj = getFieldValue("customerId", null, context);
      }

      BigDecimal lastIncoming = BigDecimal.valueOf(1000); // Default fallback

      if (customerIdObj != null) {
        String customerId = customerIdObj.toString();
        Optional<BigDecimal> lastIncomingOpt =
            operatorDataService.getLastIncomingAmount(customerId);
        if (lastIncomingOpt.isPresent()) {
          lastIncoming = lastIncomingOpt.get();
        }
      }

      if (lastIncoming.compareTo(BigDecimal.ZERO) == 0) {
        return false; // Evitar divisão por zero
      }

      BigDecimal threshold =
          lastIncoming
              .multiply(BigDecimal.valueOf(percentage))
              .divide(BigDecimal.valueOf(100), RoundingMode.HALF_UP);
      boolean result = currentAmount.compareTo(threshold) >= 0;

      log.debug(
          "GTE_PERCENT_OF_LAST_INCOMING: current={}, lastIncoming={}, percentage={}%, threshold={}, result={}",
          currentAmount, lastIncoming, percentage, threshold, result);
      return result;
    } catch (Exception e) {
      log.error("Erro ao avaliar GTE_PERCENT_OF_LAST_INCOMING: {}", e.getMessage());
      return false;
    }
  }

  /**
   * DOMAIN_IN_LIST: Verifica se o domínio do email está em uma lista. Formato:
   * "domain1,domain2,domain3" (ex: "guerrillamail.com,10minutemail.com")
   */
  private boolean evaluateDomainInList(Object fieldValue, RuleCondition condition) {
    if (fieldValue == null || condition.getValueSingle() == null) {
      return false;
    }

    String email = String.valueOf(fieldValue).toLowerCase().trim();
    int atIndex = email.indexOf('@');
    if (atIndex < 0 || atIndex >= email.length() - 1) {
      return false;
    }

    String domain = email.substring(atIndex + 1);
    String[] blockedDomains = condition.getValueSingle().toLowerCase().split(",");

    for (String blocked : blockedDomains) {
      if (domain.equals(blocked.trim())) {
        return true;
      }
    }
    return false;
  }

  /**
   * CHARGEBACK_RATE_GT: Verifica se a taxa de chargeback do merchant é maior que o threshold.
   * Formato: "rate:days" (ex: "2:7" = taxa > 2% nos últimos 7 dias)
   */
  private boolean evaluateChargebackRateGt(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 2) {
        log.warn("Formato inválido para CHARGEBACK_RATE_GT. Esperado: rate:days");
        return false;
      }

      double rateThreshold = Double.parseDouble(parts[0].trim());
      int days = Integer.parseInt(parts[1].trim());

      Object merchantIdObj = getFieldValue(condition.getFieldName(), null, context);
      if (merchantIdObj == null) {
        return false;
      }
      String merchantId = merchantIdObj.toString();

      // Consulta real à taxa de chargeback do merchant
      BigDecimal threshold = BigDecimal.valueOf(rateThreshold / 100.0); // Converter % para decimal
      boolean result = operatorDataService.hasHighChargebackRate(merchantId, threshold);

      log.debug(
          "CHARGEBACK_RATE_GT: merchantId={}, threshold={}%, days={}, result={}",
          merchantId, rateThreshold, days, result);
      return result;
    } catch (Exception e) {
      log.error("Erro ao avaliar CHARGEBACK_RATE_GT: {}", e.getMessage());
      return false;
    }
  }

  /**
   * ACCOUNT_AGE_LT_MINUTES: Verifica se a conta tem menos de N minutos de idade. Formato: "minutes"
   * (ex: "10")
   */
  private boolean evaluateAccountAgeLtMinutes(RuleCondition condition, EvaluationContext context) {
    try {
      int thresholdMinutes = Integer.parseInt(condition.getValueSingle().trim());

      // Obter ID do cliente
      Object customerIdObj = getFieldValue("customerIdFromHeader", null, context);
      if (customerIdObj == null) {
        customerIdObj = getFieldValue("customerId", null, context);
      }

      if (customerIdObj != null) {
        String customerId = customerIdObj.toString();
        long accountAgeMinutes = operatorDataService.getAccountAgeInMinutes(customerId);

        if (accountAgeMinutes >= 0) {
          boolean result = accountAgeMinutes < thresholdMinutes;
          log.debug(
              "ACCOUNT_AGE_LT_MINUTES: customerId={}, age={}min, threshold={}min, result={}",
              customerId,
              accountAgeMinutes,
              thresholdMinutes,
              result);
          return result;
        }
      }

      // Fallback: verificar campo de data de criação no payload
      Object createdAt = getFieldValue("accountCreatedAt", null, context);
      if (createdAt == null) {
        createdAt = getFieldValue("customerCreatedAt", null, context);
      }

      if (createdAt != null) {
        java.time.OffsetDateTime createdDateTime;
        if (createdAt instanceof java.time.OffsetDateTime) {
          createdDateTime = (java.time.OffsetDateTime) createdAt;
        } else if (createdAt instanceof String) {
          createdDateTime = java.time.OffsetDateTime.parse((String) createdAt);
        } else {
          return false;
        }

        long ageMinutes =
            java.time.Duration.between(createdDateTime, java.time.OffsetDateTime.now()).toMinutes();
        boolean result = ageMinutes < thresholdMinutes;
        log.debug(
            "ACCOUNT_AGE_LT_MINUTES: age={}min, threshold={}min, result={}",
            ageMinutes,
            thresholdMinutes,
            result);
        return result;
      }

      // Se não há informação de idade da conta, assumir conta antiga (segura)
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar ACCOUNT_AGE_LT_MINUTES: {}", e.getMessage());
      return false;
    }
  }

  /**
   * IS_VOIP: Verifica se o número de telefone é VoIP. Retorna true se o telefone for identificado
   * como VoIP.
   */
  private boolean evaluateIsVoip(Object fieldValue) {
    if (fieldValue == null) {
      return false;
    }

    String phone = String.valueOf(fieldValue).replaceAll("[^0-9]", "");

    // Verificar na tabela de ranges VoIP
    String countryCode = "55"; // Brasil por padrão
    if (phone.startsWith("55")) {
      phone = phone.substring(2);
    }

    // Consultar banco de dados de VoIP
    if (operatorDataService.isVoipNumber(phone, countryCode)) {
      return true;
    }

    // Fallback: Lista de prefixos conhecidos de VoIP no Brasil
    String[] voipPrefixes = {
      "0800", "0300", "0303", "0500", "0900", "4000", "4003", "4004", "4020", "4062"
    };
    for (String prefix : voipPrefixes) {
      if (phone.startsWith(prefix)) {
        return true;
      }
    }

    return false;
  }

  /**
   * COUNT_DISTINCT_PANS_LAST_N_HOURS: Conta PANs distintos associados ao campo nas últimas N horas.
   * Formato: "threshold:hours" (ex: "5:1" = mais de 5 PANs distintos na última hora)
   */
  private boolean evaluateCountDistinctPansLastNHours(
      RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 2) {
        log.warn(
            "Formato inválido para COUNT_DISTINCT_PANS_LAST_N_HOURS. Esperado: threshold:hours");
        return false;
      }

      int threshold = Integer.parseInt(parts[0].trim());
      int hours = Integer.parseInt(parts[1].trim());

      Object keyValue = getFieldValue(condition.getFieldName(), null, context);
      if (keyValue == null) {
        return false;
      }

      // Usar VelocityService para obter contagem de PANs distintos
      VelocityService.TimeWindow window = parseTimeWindowFromHours(hours);

      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(), VelocityService.KeyType.PAN, window);
        // Usar distinctMerchants como proxy para PANs distintos (simplificação)
        return stats.getDistinctMerchants() > threshold;
      }

      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_DISTINCT_PANS_LAST_N_HOURS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * COUNT_DISTINCT_ACCOUNTS: Conta contas distintas associadas ao campo. Formato: "threshold" (ex:
   * "3" = mais de 3 contas distintas)
   */
  private boolean evaluateCountDistinctAccounts(
      RuleCondition condition, EvaluationContext context) {
    try {
      int threshold = Integer.parseInt(condition.getValueSingle().trim());

      Object keyValue = getFieldValue(condition.getFieldName(), null, context);
      if (keyValue == null) {
        return false;
      }

      // Usar VelocityService para obter contagem de contas distintas
      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(),
                VelocityService.KeyType.PAN,
                VelocityService.TimeWindow.DAY_30);
        // Usar distinctCountries como proxy para contas distintas (simplificação)
        return stats.getDistinctCountries() > threshold;
      }

      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_DISTINCT_ACCOUNTS: {}", e.getMessage());
      return false;
    }
  }

  // ========== Operadores de Tempo/Data ==========

  /** IS_WEEKEND: Verifica se a transação é em fim de semana */
  private boolean evaluateIsWeekend(EvaluationContext context) {
    try {
      Object dateValue = getFieldValue("transactionDate", null, context);
      if (dateValue == null) {
        return false;
      }
      LocalDate date = parseDate(dateValue);
      if (date == null) {
        return false;
      }
      java.time.DayOfWeek dow = date.getDayOfWeek();
      return dow == java.time.DayOfWeek.SATURDAY || dow == java.time.DayOfWeek.SUNDAY;
    } catch (Exception e) {
      log.error("Erro ao avaliar IS_WEEKEND: {}", e.getMessage());
      return false;
    }
  }

  /** IS_HOLIDAY: Verifica se a transação é em feriado */
  private boolean evaluateIsHoliday(EvaluationContext context) {
    try {
      // Obter data da transação
      Object dateValue = getFieldValue("transactionDate", null, context);
      LocalDate transactionDate;

      if (dateValue instanceof LocalDate) {
        transactionDate = (LocalDate) dateValue;
      } else if (dateValue instanceof LocalDateTime) {
        transactionDate = ((LocalDateTime) dateValue).toLocalDate();
      } else if (dateValue instanceof String) {
        transactionDate = LocalDate.parse((String) dateValue);
      } else {
        transactionDate = LocalDate.now();
      }

      // Obter país da transação
      Object countryValue = getFieldValue("merchantCountryCode", null, context);
      String countryCode = countryValue != null ? countryValue.toString() : "BRA";

      return operatorDataService.isHoliday(transactionDate, countryCode);
    } catch (Exception e) {
      log.warn("Erro ao verificar feriado: {}", e.getMessage());
      return false;
    }
  }

  /** HOUR_BETWEEN: Verifica se a hora está entre dois valores. Formato: "startHour:endHour" */
  private boolean evaluateHourBetween(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 2) {
        return false;
      }
      int startHour = Integer.parseInt(parts[0].trim());
      int endHour = Integer.parseInt(parts[1].trim());

      Object timeValue = getFieldValue("transactionTime", null, context);
      if (timeValue == null) {
        return false;
      }

      int hour;
      if (timeValue instanceof Integer) {
        // Formato HHMMSS
        hour = ((Integer) timeValue) / 10000;
      } else {
        LocalTime time = parseTime(timeValue);
        if (time == null) return false;
        hour = time.getHour();
      }

      if (startHour <= endHour) {
        return hour >= startHour && hour <= endHour;
      } else {
        // Atravessa meia-noite (ex: 22:00 - 06:00)
        return hour >= startHour || hour <= endHour;
      }
    } catch (Exception e) {
      log.error("Erro ao avaliar HOUR_BETWEEN: {}", e.getMessage());
      return false;
    }
  }

  /** DAY_OF_WEEK_IN: Verifica se o dia da semana está na lista. Formato: "1,2,3" (1=Segunda) */
  private boolean evaluateDayOfWeekIn(RuleCondition condition, EvaluationContext context) {
    try {
      Object dateValue = getFieldValue("transactionDate", null, context);
      if (dateValue == null) {
        return false;
      }
      LocalDate date = parseDate(dateValue);
      if (date == null) {
        return false;
      }

      int dayOfWeek = date.getDayOfWeek().getValue(); // 1=Monday, 7=Sunday
      String[] days = condition.getValueSingle().split(",");
      for (String day : days) {
        if (Integer.parseInt(day.trim()) == dayOfWeek) {
          return true;
        }
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar DAY_OF_WEEK_IN: {}", e.getMessage());
      return false;
    }
  }

  /** GT_CURRENT_DATE: Verifica se a data é maior que a data atual (futuro) */
  private boolean evaluateGtCurrentDate(Object fieldValue) {
    try {
      LocalDate date = parseDate(fieldValue);
      return date != null && date.isAfter(LocalDate.now());
    } catch (Exception e) {
      return false;
    }
  }

  /** LT_CURRENT_DATE: Verifica se a data é menor que a data atual (passado) */
  private boolean evaluateLtCurrentDate(Object fieldValue) {
    try {
      LocalDate date = parseDate(fieldValue);
      return date != null && date.isBefore(LocalDate.now());
    } catch (Exception e) {
      return false;
    }
  }

  /** EXPIRES_WITHIN_DAYS: Verifica se expira em N dias. Formato: "days" */
  private boolean evaluateExpiresWithinDays(Object fieldValue, RuleCondition condition) {
    try {
      int days = Integer.parseInt(condition.getValueSingle().trim());
      LocalDate expiryDate = parseDate(fieldValue);
      if (expiryDate == null) {
        return false;
      }
      LocalDate threshold = LocalDate.now().plusDays(days);
      return !expiryDate.isAfter(threshold);
    } catch (Exception e) {
      log.error("Erro ao avaliar EXPIRES_WITHIN_DAYS: {}", e.getMessage());
      return false;
    }
  }

  // ========== Operadores de Padrão ==========

  /** DECIMAL_PLACES_GT: Verifica se o número tem mais de N casas decimais */
  private boolean evaluateDecimalPlacesGt(Object fieldValue, RuleCondition condition) {
    try {
      int threshold = Integer.parseInt(condition.getValueSingle().trim());
      String valueStr = String.valueOf(fieldValue);
      int dotIndex = valueStr.indexOf('.');
      if (dotIndex < 0) {
        return 0 > threshold;
      }
      int decimalPlaces = valueStr.length() - dotIndex - 1;
      return decimalPlaces > threshold;
    } catch (Exception e) {
      return false;
    }
  }

  /** PATTERN_ROUND_NUMBERS: Verifica se o valor é um número redondo (múltiplo de 100, 500, 1000) */
  private boolean evaluatePatternRoundNumbers(Object fieldValue) {
    try {
      BigDecimal value = new BigDecimal(String.valueOf(fieldValue));
      // Verificar se é múltiplo de 100
      return value.remainder(BigDecimal.valueOf(100)).compareTo(BigDecimal.ZERO) == 0;
    } catch (Exception e) {
      return false;
    }
  }

  /** GT_FIELD_MULTIPLIER: Verifica se campo > outro * fator. Formato: "otherField:multiplier" */
  private boolean evaluateGtFieldMultiplier(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 2) {
        return false;
      }
      String otherField = parts[0].trim();
      BigDecimal multiplier = new BigDecimal(parts[1].trim());

      Object fieldValue = getFieldValue(condition.getFieldName(), null, context);
      Object otherValue = getFieldValue(otherField, null, context);

      if (fieldValue == null || otherValue == null) {
        return false;
      }

      BigDecimal value = new BigDecimal(String.valueOf(fieldValue));
      BigDecimal other = new BigDecimal(String.valueOf(otherValue));
      BigDecimal threshold = other.multiply(multiplier);

      return value.compareTo(threshold) > 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar GT_FIELD_MULTIPLIER: {}", e.getMessage());
      return false;
    }
  }

  /**
   * PERCENTAGE_OF_FIELD: Calcula se campo é X% de outro. Formato: "otherField:percentage:operator"
   */
  private boolean evaluatePercentageOfField(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 2) {
        return false;
      }
      String otherField = parts[0].trim();
      BigDecimal percentage = new BigDecimal(parts[1].trim());

      Object fieldValue = getFieldValue(condition.getFieldName(), null, context);
      Object otherValue = getFieldValue(otherField, null, context);

      if (fieldValue == null || otherValue == null) {
        return false;
      }

      BigDecimal value = new BigDecimal(String.valueOf(fieldValue));
      BigDecimal other = new BigDecimal(String.valueOf(otherValue));

      if (other.compareTo(BigDecimal.ZERO) == 0) {
        return false;
      }

      BigDecimal actualPercentage =
          value.multiply(BigDecimal.valueOf(100)).divide(other, 2, java.math.RoundingMode.HALF_UP);
      return actualPercentage.compareTo(percentage) >= 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar PERCENTAGE_OF_FIELD: {}", e.getMessage());
      return false;
    }
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
    try {
      String[] parts = condition.getValueSingle().split(":");
      String fieldToCheck = parts.length > 0 ? parts[0].trim() : condition.getFieldName();
      int days = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 90;

      Object fieldValue = getFieldValue(fieldToCheck, null, context);
      if (fieldValue == null) {
        return false;
      }

      // Usar VelocityService para verificar histórico
      if (context.getTransactionRequest() != null) {
        VelocityService.TimeWindow window = parseTimeWindowFromDays(days);
        VelocityService.VelocityStats stats =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(), VelocityService.KeyType.PAN, window);

        // Se há transações no período, assumir que o campo está no histórico
        // Para uma implementação completa, seria necessário consultar o histórico real
        return stats.getTransactionCount() > 0;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar IN_CUSTOMER_HISTORY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * IN_CUSTOMER_USUAL_HOURS: Verifica se a transação está no horário habitual do cliente. Analisa o
   * padrão de horários das transações anteriores.
   */
  private boolean evaluateInCustomerUsualHours(RuleCondition condition, EvaluationContext context) {
    try {
      // Obter hora da transação atual
      Object timeValue = getFieldValue("transactionTime", null, context);
      if (timeValue == null) {
        return true; // Se não há hora, assumir horário normal
      }

      int currentHour;
      if (timeValue instanceof Integer) {
        currentHour = ((Integer) timeValue) / 10000;
      } else {
        LocalTime time = parseTime(timeValue);
        if (time == null) return true;
        currentHour = time.getHour();
      }

      // Definir horários "normais" como 6h-23h
      // Para uma implementação completa, seria necessário analisar o histórico do cliente
    } catch (Exception e) {
      log.error("Erro ao avaliar IN_CUSTOMER_CHARGEBACK_MERCHANTS: {}", e.getMessage());
      return false;
    }
  }

  // ========== Operadores de Primeira Ocorrência ==========

  /**
   * IS_FIRST: Verifica se é a primeira ocorrência do valor para o cliente. Formato: "fieldToCheck"
   * (ex: "merchantId" - primeiro uso deste merchant)
   */
  private boolean evaluateIsFirst(RuleCondition condition, EvaluationContext context) {
    try {
      String fieldToCheck =
          condition.getValueSingle() != null
              ? condition.getValueSingle().trim()
              : condition.getFieldName();

      Object fieldValue = getFieldValue(fieldToCheck, null, context);
      if (fieldValue == null) {
        return false;
      }

      // Usar VelocityService para verificar se há histórico
      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(),
                VelocityService.KeyType.PAN,
                VelocityService.TimeWindow.DAY_30);

        // Se não há transações anteriores, é a primeira
        return stats.getTransactionCount() == 0;
      }
      return true; // Se não há contexto, assumir primeira
    } catch (Exception e) {
      log.error("Erro ao avaliar IS_FIRST: {}", e.getMessage());
      return false;
    }
  }

  /**
   * IS_NEW: Verifica se o valor é "novo" (visto pela primeira vez recentemente). Formato: "maxDays"
   * (ex: "7" - visto pela primeira vez nos últimos 7 dias)
   */
  private boolean evaluateIsNew(RuleCondition condition, EvaluationContext context) {
    try {
      int maxDays =
          condition.getValueSingle() != null
              ? Integer.parseInt(condition.getValueSingle().trim())
              : 7;

      Object fieldValue = getFieldValue(condition.getFieldName(), null, context);
      if (fieldValue == null) {
        return false;
      }

      // Verificar se há histórico além do período especificado
      if (context.getTransactionRequest() != null) {
        // Verificar histórico de 30 dias
        VelocityService.VelocityStats stats30 =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(),
                VelocityService.KeyType.PAN,
                VelocityService.TimeWindow.DAY_30);

        // Verificar histórico do período especificado
        VelocityService.TimeWindow window = parseTimeWindowFromDays(maxDays);
        VelocityService.VelocityStats statsRecent =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(), VelocityService.KeyType.PAN, window);

        // É "novo" se só aparece no período recente, não no histórico mais longo
        return statsRecent.getTransactionCount() > 0
            && stats30.getTransactionCount() <= statsRecent.getTransactionCount();
      }
      return true;
    } catch (Exception e) {
      log.error("Erro ao avaliar IS_NEW: {}", e.getMessage());
      return false;
    }
  }

  /**
   * DISTANCE_FROM_LAST_GT: Verifica se a distância da última transação é maior que N km. Formato:
   * "distanceKm" (ex: "500" - mais de 500km da última transação)
   */
  private boolean evaluateDistanceFromLastGt(RuleCondition condition, EvaluationContext context) {
    try {
      double thresholdKm = Double.parseDouble(condition.getValueSingle().trim());

      if (context.getTransactionRequest() == null) {
        return false;
      }

      // Obter localização atual da transação
      String currentCountry = context.getTransactionRequest().getMerchantCountryCode();
      String currentCity = context.getTransactionRequest().getMerchantCity();

      if (currentCountry == null && currentCity == null) {
        return false;
      }

      // Para uma implementação completa, seria necessário:
      // 1. Consultar a última transação do cliente
      // 2. Obter as coordenadas de ambas as localizações
      // 3. Calcular a distância usando a fórmula de Haversine

      // Por enquanto, usar heurística baseada em país
      // Se o país mudou, assumir distância > threshold
      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(),
                VelocityService.KeyType.PAN,
                VelocityService.TimeWindow.HOUR_24);

        // Se há transações em múltiplos países nas últimas 24h, pode indicar distância grande
        return stats.getDistinctCountries() > 1;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar DISTANCE_FROM_LAST_GT: {}", e.getMessage());
      return false;
    }
  }

  // ========== OPERADORES V28-V30 (17 novos métodos) ==========
  // Implementados conforme recomendação do Triple-Check de Especialistas

  /**
   * Helper seguro para parsing de inteiros. Evita NumberFormatException retornando valor default.
   */
  private int parseIntSafe(String value, int defaultValue) {
    return NumericParser.parseIntSafe(value, defaultValue);
  }

  /** Helper seguro para parsing de doubles. */
  private double parseDoubleSafe(String value, double defaultValue) {
    return NumericParser.parseDoubleSafe(value, defaultValue);
  }

  /** Obtém o ID da conta do contexto da transação. */
  private String getAccountId(EvaluationContext context) {
    if (context == null || context.getTransactionRequest() == null) {
      return null;
    }
    TransactionRequest req = context.getTransactionRequest();
    // Prioridade: customerAcctNumber > customerIdFromHeader > pan (hash)
    if (req.getCustomerAcctNumber() != null) {
      return String.valueOf(req.getCustomerAcctNumber());
    }
    if (req.getCustomerIdFromHeader() != null && !req.getCustomerIdFromHeader().isBlank()) {
      return req.getCustomerIdFromHeader();
    }
    if (req.getPan() != null && !req.getPan().isBlank()) {
      return "PAN_" + req.getPan().hashCode();
    }
    return null;
  }

  /**
   * HAS_FAILED_3DS_LAST_N_MINUTES: Verifica se houve falha 3DS nos últimos N minutos. Formato
   * valueSingle: "minutes" (ex: "30")
   */
  private boolean evaluateHasFailed3dsLastNMinutes(
      RuleCondition condition, EvaluationContext context) {
    try {
      int minutes = parseIntSafe(condition.getValueSingle(), 30);
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      // Verificar campos de AuthEnrichment
      Boolean failed3ds = (Boolean) payload.get("auth_has_failed_3ds_recently");
      Integer lastFailureMinutes = (Integer) payload.get("auth_last_3ds_failure_minutes_ago");

      if (Boolean.TRUE.equals(failed3ds) && lastFailureMinutes != null) {
        return lastFailureMinutes <= minutes;
      }

      // Fallback: verificar campo derivado genérico
      Boolean hasRecentFailure = (Boolean) payload.get("has_3ds_failure_" + minutes + "min");
      return Boolean.TRUE.equals(hasRecentFailure);
    } catch (Exception e) {
      log.error("Erro ao avaliar HAS_FAILED_3DS_LAST_N_MINUTES: {}", e.getMessage());
      return false;
    }
  }

  /**
   * COUNT_MFA_ABANDONMENTS: Contagem de abandonos de MFA. Formato valueSingle: "threshold:hours"
   * (ex: "3:24")
   */
  private boolean evaluateCountMfaAbandonments(RuleCondition condition, EvaluationContext context) {
    try {
      String valueSingle = condition.getValueSingle();
      if (valueSingle == null || valueSingle.isBlank()) {
        return false;
      }

      String[] parts = valueSingle.split(":");
      int threshold = parseIntSafe(parts[0], 3);
      int hours = parts.length > 1 ? parseIntSafe(parts[1], 24) : 24;

      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Integer count = (Integer) payload.get("auth_mfa_abandonment_count_" + hours + "h");
      if (count == null) {
        count = (Integer) payload.get("mfa_abandonment_count");
      }

      return count != null && count >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_MFA_ABANDONMENTS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * HAS_INCOMING_TRANSFER_LAST_N_HOURS: Verifica se houve transferência de entrada. Formato
   * valueSingle: "hours" (ex: "24")
   */
  private boolean evaluateHasIncomingTransferLastNHours(
      RuleCondition condition, EvaluationContext context) {
    try {
      int hours = parseIntSafe(condition.getValueSingle(), 24);
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Boolean hasIncoming = (Boolean) payload.get("velocity_has_incoming_transfer_" + hours + "h");
      if (hasIncoming == null) {
        hasIncoming = (Boolean) payload.get("has_incoming_transfer_last_" + hours + "h");
      }

      return Boolean.TRUE.equals(hasIncoming);
    } catch (Exception e) {
      log.error("Erro ao avaliar HAS_INCOMING_TRANSFER_LAST_N_HOURS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * IS_IMPOSSIBLE_COMBINATION: Verifica combinações impossíveis de dados. Formato valueSingle: tipo
   * de combinação (ex: "age_corporate", "country_currency")
   */
  private boolean evaluateIsImpossibleCombination(
      RuleCondition condition, EvaluationContext context) {
    try {
      String combinationType = condition.getValueSingle();
      if (combinationType == null || combinationType.isBlank()) {
        return false;
      }

      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      return switch (combinationType.toLowerCase().trim()) {
        case "age_corporate" -> {
          Object ageObj = payload.get("customer_age");
          String accountType = (String) payload.get("account_type");
          int age = ageObj instanceof Number ? ((Number) ageObj).intValue() : -1;
          yield age >= 0 && age < 18 && "CORPORATE".equalsIgnoreCase(accountType);
        }
        case "country_currency" -> {
          String country = (String) payload.get("merchantCountryCode");
          String currency = (String) payload.get("transactionCurrencyCode");
          yield !isValidCurrencyForCountry(country, currency);
        }
        case "deceased_active" -> {
          Boolean isDeceased = (Boolean) payload.get("customer_is_deceased");
          yield Boolean.TRUE.equals(isDeceased);
        }
        case "minor_high_value" -> {
          Object ageObj = payload.get("customer_age");
          Object amountObj = payload.get("transactionAmount");
          int age = ageObj instanceof Number ? ((Number) ageObj).intValue() : 99;
          BigDecimal amount = getBigDecimal(amountObj);
          yield age < 18 && amount != null && amount.compareTo(new BigDecimal("5000")) > 0;
        }
        default -> false;
      };
    } catch (Exception e) {
      log.error("Erro ao avaliar IS_IMPOSSIBLE_COMBINATION: {}", e.getMessage());
      return false;
    }
  }

  /** Verifica se a moeda é válida para o país. */
  private boolean isValidCurrencyForCountry(String country, String currency) {
    if (country == null || currency == null) return true;

    Map<String, List<String>> validCurrencies =
        Map.of(
            "BR", List.of("BRL"),
            "US", List.of("USD"),
            "GB", List.of("GBP"),
            "DE", List.of("EUR"),
            "FR", List.of("EUR"),
            "IT", List.of("EUR"),
            "ES", List.of("EUR"),
            "PT", List.of("EUR"),
            "JP", List.of("JPY"),
            "CN", List.of("CNY", "CNH"));

    List<String> valid = validCurrencies.get(country.toUpperCase());
    return valid == null || valid.contains(currency.toUpperCase());
  }

  /**
   * PIX_KEY_CHANGED_LAST_N_DAYS: Verifica se chave PIX foi alterada recentemente. Formato
   * valueSingle: "days" (ex: "7")
   */
  private boolean evaluatePixKeyChangedLastNDays(
      RuleCondition condition, EvaluationContext context) {
    try {
      int days = parseIntSafe(condition.getValueSingle(), 7);
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Integer daysSinceChange = (Integer) payload.get("customer_pix_key_changed_days_ago");
      if (daysSinceChange == null) {
        daysSinceChange = (Integer) payload.get("pix_key_age_days");
      }

      return daysSinceChange != null && daysSinceChange <= days;
    } catch (Exception e) {
      log.error("Erro ao avaliar PIX_KEY_CHANGED_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
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
    try {
      String valueSingle = condition.getValueSingle();
      if (valueSingle == null || valueSingle.isBlank()) {
        return false;
      }

      String[] parts = valueSingle.split("\\|");
      int threshold = parseIntSafe(parts[0], 5);
      int days = parts.length > 1 ? parseIntSafe(parts[1], 30) : 30;

      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object countObj = payload.get("velocity_crypto_txn_count_" + days + "d");
      if (countObj == null) {
        countObj = payload.get("crypto_transaction_count");
      }

      long count = countObj instanceof Number ? ((Number) countObj).longValue() : 0;
      return count >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_CRYPTO_TXN_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * COUNT_DISTINCT_INSTRUMENTS_LAST_N_DAYS: Instrumentos distintos nos últimos N dias. Formato
   * valueSingle: "threshold|days" (ex: "10|30")
   */
  private boolean evaluateCountDistinctInstrumentsLastNDays(
      RuleCondition condition, EvaluationContext context) {
    try {
      String valueSingle = condition.getValueSingle();
      if (valueSingle == null || valueSingle.isBlank()) {
        return false;
      }

      String[] parts = valueSingle.split("\\|");
      int threshold = parseIntSafe(parts[0], 10);
      int days = parts.length > 1 ? parseIntSafe(parts[1], 30) : 30;

      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object countObj = payload.get("velocity_distinct_instruments_" + days + "d");
      if (countObj == null) {
        countObj = payload.get("distinct_payment_instruments");
      }

      long count = countObj instanceof Number ? ((Number) countObj).longValue() : 0;
      return count >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_DISTINCT_INSTRUMENTS_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * COUNT_DISTINCT_PAYERS_LAST_N_DAYS: Pagadores distintos nos últimos N dias. Formato valueSingle:
   * "threshold|days" (ex: "5|7")
   */
  private boolean evaluateCountDistinctPayersLastNDays(
      RuleCondition condition, EvaluationContext context) {
    try {
      String valueSingle = condition.getValueSingle();
      if (valueSingle == null || valueSingle.isBlank()) {
        return false;
      }

      String[] parts = valueSingle.split("\\|");
      int threshold = parseIntSafe(parts[0], 5);
      int days = parts.length > 1 ? parseIntSafe(parts[1], 7) : 7;

      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object countObj = payload.get("velocity_distinct_payers_" + days + "d");
      if (countObj == null) {
        countObj = payload.get("distinct_payers_count");
      }

      long count = countObj instanceof Number ? ((Number) countObj).longValue() : 0;
      return count >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_DISTINCT_PAYERS_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * COUNT_DISTINCT_USER_AGENTS_LAST_N_HOURS: User agents distintos nas últimas N horas. Formato
   * valueSingle: "threshold|hours" (ex: "5|24")
   */
  private boolean evaluateCountDistinctUserAgentsLastNHours(
      RuleCondition condition, EvaluationContext context) {
    try {
      String valueSingle = condition.getValueSingle();
      if (valueSingle == null || valueSingle.isBlank()) {
        return false;
      }

      String[] parts = valueSingle.split("\\|");
      int threshold = parseIntSafe(parts[0], 5);
      int hours = parts.length > 1 ? parseIntSafe(parts[1], 24) : 24;

      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object countObj = payload.get("device_distinct_user_agents_" + hours + "h");
      if (countObj == null) {
        countObj = payload.get("distinct_user_agents");
      }

      long count = countObj instanceof Number ? ((Number) countObj).longValue() : 0;
      return count >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_DISTINCT_USER_AGENTS_LAST_N_HOURS: {}", e.getMessage());
      return false;
    }
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
    try {
      String valueSingle = condition.getValueSingle();
      if (valueSingle == null || valueSingle.isBlank()) {
        return false;
      }

      String[] parts = valueSingle.split(":");
      int threshold = parseIntSafe(parts[0], 3);
      int hours = parts.length > 1 ? parseIntSafe(parts[1], 24) : 24;

      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Integer count = (Integer) payload.get("auth_mfa_denial_count_" + hours + "h");
      if (count == null) {
        count = (Integer) payload.get("mfa_denial_count");
      }

      return count != null && count >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_MFA_DENIALS_LAST_N_HOURS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * DAYS_SINCE_LAST_ACTIVITY: Dias desde última atividade. Formato valueSingle:
   * "threshold|operator" (ex: "30|GT" = mais de 30 dias)
   */
  private boolean evaluateDaysSinceLastActivity(
      RuleCondition condition, EvaluationContext context) {
    try {
      String valueSingle = condition.getValueSingle();
      if (valueSingle == null || valueSingle.isBlank()) {
        return false;
      }

      String[] parts = valueSingle.split("\\|");
      int threshold = parseIntSafe(parts[0], 30);
      String operator = parts.length > 1 ? parts[1].toUpperCase() : "GT";

      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object daysObj = payload.get("customer_days_since_last_activity");
      if (daysObj == null) {
        daysObj = payload.get("days_since_last_transaction");
      }

      if (daysObj == null) return false;
      int days = daysObj instanceof Number ? ((Number) daysObj).intValue() : -1;
      if (days < 0) return false;

      return switch (operator) {
        case "GT" -> days > threshold;
        case "GTE" -> days >= threshold;
        case "LT" -> days < threshold;
        case "LTE" -> days <= threshold;
        case "EQ" -> days == threshold;
        default -> days > threshold;
      };
    } catch (Exception e) {
      log.error("Erro ao avaliar DAYS_SINCE_LAST_ACTIVITY: {}", e.getMessage());
      return false;
    }
  }

  /** DEVICE_CHANGED_IN_SESSION: Verifica se device mudou durante a sessão. */
  private boolean evaluateDeviceChangedInSession(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Boolean changed = (Boolean) payload.get("device_changed_in_session");
      if (changed == null) {
        changed = (Boolean) payload.get("session_device_changed");
      }

      return Boolean.TRUE.equals(changed);
    } catch (Exception e) {
      log.error("Erro ao avaliar DEVICE_CHANGED_IN_SESSION: {}", e.getMessage());
      return false;
    }
  }

  /**
   * IS_CRYPTO_RANSOM_AMOUNT: Detecta valores típicos de ransomware. Verifica se o valor está
   * próximo de quantias típicas de ransom.
   */
  private boolean evaluateIsCryptoRansomAmount(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object amountObj = payload.get("transactionAmount");
      if (amountObj == null) {
        amountObj = payload.get("amount");
      }

      BigDecimal amount = getBigDecimal(amountObj);
      if (amount == null || amount.compareTo(BigDecimal.ZERO) <= 0) return false;

      // Valores tipicos de ransom (em BRL ou USD)
      // Convertidos de valores comuns em BTC: 0.05, 0.1, 0.25, 0.5, 1, 2, 5 BTC
      // Considerando BTC entre R$ 300.000 - R$ 400.000
      List<BigDecimal> typicalRansomAmounts =
          List.of(
              new BigDecimal("500"), // ~0.001 BTC
              new BigDecimal("1000"), // ~0.003 BTC
              new BigDecimal("2500"), // ~0.007 BTC
              new BigDecimal("5000"), // ~0.015 BTC
              new BigDecimal("10000"), // ~0.03 BTC
              new BigDecimal("15000"), // ~0.05 BTC
              new BigDecimal("25000"), // ~0.08 BTC
              new BigDecimal("35000"), // ~0.1 BTC
              new BigDecimal("50000"), // ~0.15 BTC
              new BigDecimal("75000"), // ~0.25 BTC
              new BigDecimal("100000"), // ~0.3 BTC
              new BigDecimal("150000"), // ~0.5 BTC
              new BigDecimal("175000"), // ~0.5 BTC
              new BigDecimal("200000"), // ~0.6 BTC
              new BigDecimal("350000"), // ~1 BTC
              new BigDecimal("500000") // ~1.5 BTC
              );

      // Verificar se valor esta proximo (±10%) de valores tipicos
      for (BigDecimal typical : typicalRansomAmounts) {
        BigDecimal lower = typical.multiply(new BigDecimal("0.9"));
        BigDecimal upper = typical.multiply(new BigDecimal("1.1"));
        if (amount.compareTo(lower) >= 0 && amount.compareTo(upper) <= 0) {
          return true;
        }
      }

      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar IS_CRYPTO_RANSOM_AMOUNT: {}", e.getMessage());
      return false;
    }
  }

  /**
   * OUTFLOW_RATE_LAST_N_DAYS: Taxa de saída nos últimos N dias. Formato valueSingle:
   * "threshold|days" (threshold em percentual)
   */
  private boolean evaluateOutflowRateLastNDays(RuleCondition condition, EvaluationContext context) {
    try {
      String valueSingle = condition.getValueSingle();
      if (valueSingle == null || valueSingle.isBlank()) {
        return false;
      }

      String[] parts = valueSingle.split("\\|");
      double threshold = parseDoubleSafe(parts[0], 80.0);
      int days = parts.length > 1 ? parseIntSafe(parts[1], 30) : 30;

      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object rateObj = payload.get("velocity_outflow_rate_" + days + "d");
      if (rateObj == null) {
        rateObj = payload.get("outflow_rate_percentage");
      }

      double rate = rateObj instanceof Number ? ((Number) rateObj).doubleValue() : 0.0;
      return rate >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar OUTFLOW_RATE_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  /** Converte objeto para BigDecimal de forma segura. */
  private BigDecimal getBigDecimal(Object value) {
    if (value == null) return null;
    if (value instanceof BigDecimal) return (BigDecimal) value;
    if (value instanceof Number) return BigDecimal.valueOf(((Number) value).doubleValue());
    try {
      return new BigDecimal(String.valueOf(value));
    } catch (NumberFormatException e) {
      return null;
    }
  }

  // ========== OPERADORES V31+ IMPLEMENTADOS - CATEGORIAS A-K ==========


  // --- CATEGORIA B: Behavioral Rules (8) ---

  /**
   * DORMANCY_REVIVAL: Detecta conta reativada após período de dormência. Formato valueSingle:
   * "dormancyDays|txThreshold" (ex: "90|3")
   */
  private boolean evaluateDormancyRevival(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int dormancyDays = Integer.parseInt(parts[0].trim());
      int txThreshold = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 3;

      Object lastActivityObj = payload.get("days_since_last_activity");
      if (lastActivityObj == null) lastActivityObj = payload.get("daysSinceLastActivity");

      Object recentTxObj = payload.get("recent_tx_count");
      if (recentTxObj == null) recentTxObj = payload.get("recentTxCount");

      int daysSinceActivity =
          lastActivityObj instanceof Number ? ((Number) lastActivityObj).intValue() : 0;
      int recentTxCount = recentTxObj instanceof Number ? ((Number) recentTxObj).intValue() : 0;

      return daysSinceActivity > dormancyDays && recentTxCount >= txThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar DORMANCY_REVIVAL: {}", e.getMessage());
      return false;
    }
  }

  /**
   * AMOUNT_DEVIATION_FROM_AVG: Desvio do valor médio histórico. Formato valueSingle:
   * "deviationMultiplier" (ex: "3.0" = 3x desvio padrão)
   */
  private boolean evaluateAmountDeviationFromAvg(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getTransactionRequest() == null) return false;

      double multiplier = Double.parseDouble(condition.getValueSingle().trim());

      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(),
              VelocityService.KeyType.PAN,
              VelocityService.TimeWindow.HOUR_24);

      BigDecimal avg = stats.getAvgAmount();
      BigDecimal stdDev = stats.getStdDevAmount();
      BigDecimal txAmount = context.getTransactionRequest().getTransactionAmount();

      if (avg == null || stdDev == null || stdDev.compareTo(BigDecimal.ZERO) == 0) return false;

      BigDecimal deviation = txAmount.subtract(avg).abs();
      BigDecimal threshold = stdDev.multiply(BigDecimal.valueOf(multiplier));

      return deviation.compareTo(threshold) > 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar AMOUNT_DEVIATION_FROM_AVG: {}", e.getMessage());
      return false;
    }
  }

  /**
   * TIME_DEVIATION_FROM_USUAL: Desvio do horário usual de transações. Formato valueSingle:
   * "hoursDeviation" (ex: "4" = 4 horas fora do padrão)
   */
  private boolean evaluateTimeDeviationFromUsual(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int hoursThreshold = Integer.parseInt(condition.getValueSingle().trim());

      Object deviationObj = payload.get("time_deviation_hours");
      if (deviationObj == null) deviationObj = payload.get("timeDeviationHours");

      double deviation =
          deviationObj instanceof Number ? ((Number) deviationObj).doubleValue() : 0.0;
      return Math.abs(deviation) > hoursThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar TIME_DEVIATION_FROM_USUAL: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_DEVIATION: Detecta merchant fora do padrão usual do cliente. Formato valueSingle:
   * "true" (verifica se merchant é novo/incomum)
   */
  private boolean evaluateMerchantDeviation(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object deviationObj = payload.get("merchant_is_unusual");
      if (deviationObj == null) deviationObj = payload.get("merchantIsUnusual");
      if (deviationObj == null) deviationObj = payload.get("new_merchant");

      return Boolean.TRUE.equals(deviationObj)
          || "true".equalsIgnoreCase(String.valueOf(deviationObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_DEVIATION: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MICRO_TRANSACTION_TEST: Detecta teste com micro-transação. Formato valueSingle:
   * "maxAmount|count" (ex: "1.00|3")
   */
  private boolean evaluateMicroTransactionTest(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getTransactionRequest() == null) return false;
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      double maxAmount = Double.parseDouble(parts[0].trim());
      int countThreshold = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 2;

      BigDecimal txAmount = context.getTransactionRequest().getTransactionAmount();

      Object microCountObj = payload.get("micro_tx_count");
      if (microCountObj == null) microCountObj = payload.get("microTxCount");

      int microCount = microCountObj instanceof Number ? ((Number) microCountObj).intValue() : 0;

      boolean isMicro = txAmount.doubleValue() <= maxAmount;
      return isMicro && microCount >= countThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar MICRO_TRANSACTION_TEST: {}", e.getMessage());
      return false;
    }
  }

  /**
   * LOCATION_DEVIATION: Detecta localização fora do padrão. Formato valueSingle: "kmThreshold" (ex:
   * "100" = 100km da localização usual)
   */
  private boolean evaluateLocationDeviation(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double kmThreshold = Double.parseDouble(condition.getValueSingle().trim());

      Object distanceObj = payload.get("distance_from_usual_km");
      if (distanceObj == null) distanceObj = payload.get("distanceFromUsualKm");

      double distance = distanceObj instanceof Number ? ((Number) distanceObj).doubleValue() : 0.0;
      return distance > kmThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar LOCATION_DEVIATION: {}", e.getMessage());
      return false;
    }
  }

  /**
   * CHANNEL_SWITCH_PATTERN: Detecta troca suspeita de canal. Formato valueSingle:
   * "switchCount|hours" (ex: "3|1")
   */
  private boolean evaluateChannelSwitchPattern(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int switchThreshold = Integer.parseInt(parts[0].trim());

      Object switchCountObj = payload.get("channel_switch_count");
      if (switchCountObj == null) switchCountObj = payload.get("channelSwitchCount");

      int switchCount = switchCountObj instanceof Number ? ((Number) switchCountObj).intValue() : 0;
      return switchCount >= switchThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar CHANNEL_SWITCH_PATTERN: {}", e.getMessage());
      return false;
    }
  }

  /**
   * BENEFICIARY_REUSE_PATTERN: Detecta padrão de reutilização de beneficiário. Formato valueSingle:
   * "reuseThreshold" (ex: "5" = mesmo beneficiário 5x)
   */
  private boolean evaluateBeneficiaryReusePattern(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int reuseThreshold = Integer.parseInt(condition.getValueSingle().trim());

      Object reuseCountObj = payload.get("beneficiary_reuse_count");
      if (reuseCountObj == null) reuseCountObj = payload.get("beneficiaryReuseCount");

      int reuseCount = reuseCountObj instanceof Number ? ((Number) reuseCountObj).intValue() : 0;
      return reuseCount >= reuseThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar BENEFICIARY_REUSE_PATTERN: {}", e.getMessage());
      return false;
    }
  }

  // ========== OPERADORES V4.0 PHASE 1 - VELOCITY + DEVICE (40 novos) ==========

  // --- CATEGORIA L: Transaction Count Velocity Avançado (12) ---

  // ========== PLANNED OPERATORS - Basel SL (Operational Risk) (lançam UnsupportedOperatorException) ==========

  // ========== LLM & Generative AI Fraud Detection (LLM001-LLM012) ==========

  // ========== Neo4j Graph Fraud Detection (NEO001-NEO018) ==========

  // ========== OPERADORES SINCRONIZADOS V49 - IMPLEMENTAÇÕES ==========

  // Operadores lógicos
  private boolean evaluateLogicalAnd(RuleCondition condition, EvaluationContext context) {
    Object fieldValue = getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
    boolean a = Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
    boolean b = Boolean.parseBoolean(condition.getValueSingle());
    return a && b;
  }

  private boolean evaluateLogicalOr(RuleCondition condition, EvaluationContext context) {
    Object fieldValue = getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
    boolean a = Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
    boolean b = Boolean.parseBoolean(condition.getValueSingle());
    return a || b;
  }

  private boolean evaluateLogicalNot(RuleCondition condition, EvaluationContext context) {
    Object fieldValue = getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
    return !Boolean.TRUE.equals(fieldValue) && !"true".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateLogicalXor(RuleCondition condition, EvaluationContext context) {
    Object fieldValue = getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
    boolean a = Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
    boolean b = Boolean.parseBoolean(condition.getValueSingle());
    return a ^ b;
  }

  private boolean evaluateLogicalNand(RuleCondition condition, EvaluationContext context) {
    return !evaluateLogicalAnd(condition, context);
  }

  private boolean evaluateLogicalNor(RuleCondition condition, EvaluationContext context) {
    return !evaluateLogicalOr(condition, context);
  }

  // Operadores de anomalia
  private boolean evaluateAmountAnomalyOp(RuleCondition condition, EvaluationContext context) {
    try {
      Object fieldValue = getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
      double amount = Double.parseDouble(String.valueOf(fieldValue));
      Map<String, Object> payload = context.getPayload();
      double avgAmount = payload != null && payload.containsKey("avgAmount")
          ? Double.parseDouble(payload.get("avgAmount").toString()) : 0;
      double stdDev = payload != null && payload.containsKey("amountStdDev")
          ? Double.parseDouble(payload.get("amountStdDev").toString()) : 1;
      double threshold = condition.getValueSingle() != null
          ? Double.parseDouble(condition.getValueSingle()) : 3.0;
      return Math.abs(amount - avgAmount) > threshold * stdDev;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateTimeAnomalyOp(RuleCondition condition, EvaluationContext context) {
    try {
      Object fieldValue = getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
      int hour = Integer.parseInt(String.valueOf(fieldValue));
      return hour >= 2 && hour <= 5; // Horário suspeito: 2h-5h
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateVelocityAnomalyOp(RuleCondition condition, EvaluationContext context) {
    try {
      Object fieldValue = getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
      double velocity = Double.parseDouble(String.valueOf(fieldValue));
      Map<String, Object> payload = context.getPayload();
      double avgVelocity = payload != null && payload.containsKey("avgVelocity")
          ? Double.parseDouble(payload.get("avgVelocity").toString()) : 0;
      double stdDev = payload != null && payload.containsKey("velocityStdDev")
          ? Double.parseDouble(payload.get("velocityStdDev").toString()) : 1;
      double threshold = condition.getValueSingle() != null
          ? Double.parseDouble(condition.getValueSingle()) : 2.0;
      return Math.abs(velocity - avgVelocity) > threshold * stdDev;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateMccAnomalyOp(RuleCondition condition, EvaluationContext context) {
    Object fieldValue = getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "ANOMALY".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateMerchantAnomalyOp(RuleCondition condition, EvaluationContext context) {
    Object fieldValue = getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "ANOMALY".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  // Operadores de dispositivo/sessão
  private boolean evaluateIsNewDeviceOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "NEW".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateIsNewLocationOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "NEW".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateDeviceFingerprintMismatchOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "MISMATCH".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateSessionDurationLtOp(Object fieldValue, RuleCondition condition) {
    try {
      double duration = Double.parseDouble(String.valueOf(fieldValue));
      double threshold = Double.parseDouble(condition.getValueSingle());
      return duration < threshold;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateClickVelocityGtOp(Object fieldValue, RuleCondition condition) {
    try {
      double velocity = Double.parseDouble(String.valueOf(fieldValue));
      double threshold = Double.parseDouble(condition.getValueSingle());
      return velocity > threshold;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateMouseMovementAnomalyOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "ANOMALY".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateTypingSpeedAnomalyOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "ANOMALY".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateUserAgentSuspiciousOp(Object fieldValue) {
    if (fieldValue == null) return false;
    String userAgent = String.valueOf(fieldValue).toLowerCase();
    return userAgent.contains("bot") || userAgent.contains("crawler") || userAgent.contains("spider")
        || userAgent.contains("headless") || userAgent.contains("phantom");
  }

  // Operadores de fraude de cartão
  private boolean evaluateExpiredCardOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "EXPIRED".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateCardCaptureFraudOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluatePinCvvLimitExceededOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "EXCEEDED".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateOfflinePinFailedOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "FAILED".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateEmvSecurityCheckOp(Object fieldValue) {
    return Boolean.FALSE.equals(fieldValue) || "false".equalsIgnoreCase(String.valueOf(fieldValue))
        || "FAILED".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateEcommerceNoAvsOp(Object fieldValue) {
    return fieldValue == null || Boolean.TRUE.equals(fieldValue)
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "NO_AVS".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluatePosSecurityMissingOp(Object fieldValue) {
    return fieldValue == null || Boolean.TRUE.equals(fieldValue)
        || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "MISSING".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateTerminalVerificationFailedOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "FAILED".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateSuspiciousTerminalOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "SUSPICIOUS".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateUnusualCardMediaOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "UNUSUAL".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  // Operadores de transferência
  private boolean evaluateTransferAmountGtOp(Object fieldValue, RuleCondition condition) {
    try {
      double amount = Double.parseDouble(String.valueOf(fieldValue));
      double threshold = Double.parseDouble(condition.getValueSingle());
      return amount > threshold;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateTransferVelocityGtOp(Object fieldValue, RuleCondition condition) {
    try {
      double velocity = Double.parseDouble(String.valueOf(fieldValue));
      double threshold = Double.parseDouble(condition.getValueSingle());
      return velocity > threshold;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateRecipientInWatchlistOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "WATCHLIST".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateRecipientIsNewOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "NEW".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  // Operadores de validação
  private boolean evaluateAddressMismatchOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "MISMATCH".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluatePhoneCountryMismatchOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "MISMATCH".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateEmailDomainAgeLtDaysOp(Object fieldValue, RuleCondition condition) {
    try {
      double domainAgeDays = Double.parseDouble(String.valueOf(fieldValue));
      double threshold = Double.parseDouble(condition.getValueSingle());
      return domainAgeDays < threshold;
    } catch (Exception e) {
      return false;
    }
  }


  private boolean evaluateAccountAgeLtDaysOp(Object fieldValue, RuleCondition condition) {
    try {
      double accountAgeDays = Double.parseDouble(String.valueOf(fieldValue));
      double threshold = Double.parseDouble(condition.getValueSingle());
      return accountAgeDays < threshold;
    } catch (Exception e) {
      return false;
    }
  }

  // Operadores de contexto/classificação
  private boolean evaluateContextOp(Object fieldValue, RuleCondition condition, EvaluationContext context) {
    if (condition.getValueSingle() == null || context.getPayload() == null) return false;
    Object contextValue = context.getPayload().get(condition.getValueSingle());
    return fieldValue != null && fieldValue.equals(contextValue);
  }

  private boolean evaluateFraudOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "FRAUD".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateSecurityOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateSuspiciousOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "SUSPICIOUS".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateSuspiciousTransactionTypeOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "SUSPICIOUS".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateVelocityOp(Object fieldValue, RuleCondition condition) {
    try {
      double velocity = Double.parseDouble(String.valueOf(fieldValue));
      double threshold = Double.parseDouble(condition.getValueSingle());
      return velocity > threshold;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateRoundAmountOp(Object fieldValue) {
    try {
      double amount = Double.parseDouble(String.valueOf(fieldValue));
      return amount == Math.round(amount) && amount % 100 == 0;
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateImpossibleTravelOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "IMPOSSIBLE".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateNotInListOp(Object fieldValue, RuleCondition condition) {
    if (fieldValue == null) return true;
    if (condition.getValueArray() == null || condition.getValueArray().isEmpty()) return true;
    String fieldStr = String.valueOf(fieldValue);
    return !condition.getValueArray().contains(fieldStr);
  }

  private boolean evaluateCaptchaFailedOp(Object fieldValue) {
    return Boolean.TRUE.equals(fieldValue) || "true".equalsIgnoreCase(String.valueOf(fieldValue))
        || "FAILED".equalsIgnoreCase(String.valueOf(fieldValue));
  }

  private boolean evaluateCountDistinctCountriesLastNDaysOp(RuleCondition condition, EvaluationContext context) {
    try {
      Object fieldValue = getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
      int count = Integer.parseInt(String.valueOf(fieldValue));
      int threshold = Integer.parseInt(condition.getValueSingle());
      return count > threshold;
    } catch (Exception e) {
      return false;
    }
  }
}
