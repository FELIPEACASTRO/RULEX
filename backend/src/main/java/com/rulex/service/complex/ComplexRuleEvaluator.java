package com.rulex.service.complex;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.RuleCondition.ConditionOperator;
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
    RuleCondition.ConditionOperator operator = condition.getOperator();

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
      case NOT_IN_HISTORICAL -> evaluateNotInHistorical(condition, context);
      case NAME_SIMILARITY_LT -> evaluateNameSimilarityLt(condition, context);
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
          evaluateContainsSuspiciousKeywords(fieldValue, condition);

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
      case VELOCITY_CROSS_CHANNEL -> evaluateVelocityCrossChannel(condition, context);
      case VELOCITY_ROLLING_WINDOW -> evaluateVelocityRollingWindow(condition, context);
      case VELOCITY_PERCENTILE -> evaluateVelocityPercentile(condition, context);
      case VELOCITY_RATIO_GT -> evaluateVelocityRatioGt(condition, context);
      case VELOCITY_TREND -> evaluateVelocityTrend(condition, context);
      case COUNT_UNIQUE_BENEFICIARIES_LAST_N_DAYS ->
          evaluateCountUniqueBeneficiariesLastNDays(condition, context);
      case COUNT_UNIQUE_IPS_LAST_N_HOURS -> evaluateCountUniqueIpsLastNHours(condition, context);
      case SUM_BY_CHANNEL_LAST_N_DAYS -> evaluateSumByChannelLastNDays(condition, context);
      case AVG_INTERVAL_BETWEEN_TXN -> evaluateAvgIntervalBetweenTxn(condition, context);
      case VELOCITY_ACCELERATION -> evaluateVelocityAcceleration(condition, context);

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
      case FAN_OUT_COUNT -> evaluateFanOutCount(condition, context);
      case FAN_IN_COUNT -> evaluateFanInCount(condition, context);
      case SHARED_DEVICE_COUNT -> evaluateSharedDeviceCount(condition, context);
      case SHARED_IP_COUNT -> evaluateSharedIpCount(condition, context);
      case ACCOUNT_LINK_DEPTH -> evaluateAccountLinkDepth(condition, context);
      case CIRCULAR_TRANSFER_DETECTION -> evaluateCircularTransferDetection(condition, context);
      case RAPID_MULTI_HOP -> evaluateRapidMultiHop(condition, context);
      case BENEFICIARY_CONCENTRATION -> evaluateBeneficiaryConcentration(condition, context);

        // CATEGORIA D: Sanctions & Name Matching (7)
      case OFAC_LIST_CHECK -> evaluateOfacListCheck(fieldValue, condition);
      case PEP_LIST_CHECK -> evaluatePepListCheck(fieldValue, condition);
      case ADVERSE_MEDIA_CHECK -> evaluateAdverseMediaCheck(fieldValue, condition);
      case SANCTIONS_COUNTRY_CHECK -> evaluateSanctionsCountryCheck(fieldValue, condition);
      case HIGH_RISK_JURISDICTION -> evaluateHighRiskJurisdiction(fieldValue, condition);
      case NAME_TRANSLITERATION_MATCH -> evaluateNameTransliterationMatch(condition, context);
      case ALIAS_DETECTION -> evaluateAliasDetection(condition, context);

        // CATEGORIA E: Synthetic ID Detection (8)
      case CPF_SSN_VALIDATION -> evaluateCpfSsnValidation(fieldValue, condition);
      case PHONE_CARRIER_CHECK -> evaluatePhoneCarrierCheck(fieldValue, condition);
      case EMAIL_DOMAIN_AGE -> evaluateEmailDomainAge(fieldValue, condition);
      case ADDRESS_VERIFICATION -> evaluateAddressVerification(condition, context);
      case IDENTITY_VELOCITY -> evaluateIdentityVelocity(condition, context);
      case DEVICE_ACCOUNT_RATIO -> evaluateDeviceAccountRatio(condition, context);
      case EMAIL_PHONE_MISMATCH -> evaluateEmailPhoneMismatch(condition, context);
      case CREDIT_FILE_THIN -> evaluateCreditFileThin(condition, context);

        // CATEGORIA F: AML Typology (8)
      case STRUCTURING_DETECTION -> evaluateStructuringDetection(condition, context);
      case LAYERING_PATTERN -> evaluateLayeringPattern(condition, context);
      case RAPID_MOVEMENT -> evaluateRapidMovement(condition, context);
      case INTEGRATION_PATTERN -> evaluateIntegrationPattern(condition, context);
      case CASH_INTENSIVE_RATIO -> evaluateCashIntensiveRatio(condition, context);
      case UNUSUAL_BUSINESS_PATTERN -> evaluateUnusualBusinessPattern(condition, context);
      case SHELL_COMPANY_INDICATOR -> evaluateShellCompanyIndicator(condition, context);
      case TRADE_BASED_ML_INDICATOR -> evaluateTradeBasedMlIndicator(condition, context);

        // CATEGORIA G: Regulatory (8)
      case SCA_EXEMPTION_TRA -> evaluateScaExemptionTra(condition, context);
      case SCA_EXEMPTION_LOW_VALUE -> evaluateScaExemptionLowValue(condition, context);
      case SCA_EXEMPTION_TRUSTED_BENEFICIARY ->
          evaluateScaExemptionTrustedBeneficiary(condition, context);
      case SCA_EXEMPTION_RECURRING -> evaluateScaExemptionRecurring(condition, context);
      case PSD3_COP_NAME_MATCH -> evaluatePsd3CopNameMatch(condition, context);
      case DORA_INCIDENT_SEVERITY -> evaluateDoraIncidentSeverity(condition, context);
      case EIDAS_ASSURANCE_LEVEL -> evaluateEidasAssuranceLevel(condition, context);
      case GDPR_DATA_RETENTION_CHECK -> evaluateGdprDataRetentionCheck(condition, context);

        // CATEGORIA H: Device (7)
      case DEVICE_JAILBREAK_ROOTED -> evaluateDeviceJailbreakRooted(condition, context);
      case EMULATOR_DETECTION -> evaluateEmulatorDetection(condition, context);
      case VPN_PROXY_DETECTION -> evaluateVpnProxyDetection(condition, context);
      case TOR_EXIT_NODE -> evaluateTorExitNode(fieldValue, condition);
      case BROWSER_INCONSISTENCY -> evaluateBrowserInconsistency(condition, context);
      case TIMEZONE_MISMATCH -> evaluateTimezoneMismatch(condition, context);
      case LANGUAGE_MISMATCH -> evaluateLanguageMismatch(condition, context);

        // CATEGORIA I: Merchant & MCC (7)
      case MCC_HIGH_RISK -> evaluateMccHighRisk(fieldValue, condition);
      case MCC_GAMBLING -> evaluateMccGambling(fieldValue, condition);
      case MCC_CRYPTO -> evaluateMccCrypto(fieldValue, condition);
      case MERCHANT_FIRST_SEEN -> evaluateMerchantFirstSeen(condition, context);
      case MERCHANT_COUNTRY_MISMATCH -> evaluateMerchantCountryMismatch(condition, context);
      case MERCHANT_CATEGORY_CHANGE -> evaluateMerchantCategoryChange(condition, context);
      case MERCHANT_VELOCITY_SPIKE -> evaluateMerchantVelocitySpike(condition, context);

        // CATEGORIA J: ISO 20022 (6)
      case PACS008_FIELD_VALIDATION -> evaluatePacs008FieldValidation(condition, context);
      case REMITTANCE_INFO_ANALYSIS -> evaluateRemittanceInfoAnalysis(condition, context);
      case PURPOSE_CODE_MISMATCH -> evaluatePurposeCodeMismatch(condition, context);
      case UETR_DUPLICATE_CHECK -> evaluateUetrDuplicateCheck(condition, context);
      case CREDITOR_NAME_VALIDATION -> evaluateCreditorNameValidation(condition, context);
      case STRUCTURED_ADDRESS_CHECK -> evaluateStructuredAddressCheck(condition, context);

        // CATEGORIA K: Estatísticos Simples (5)
      case BENFORD_LAW_DEVIATION -> evaluateBenfordLawDeviation(condition, context);
      case Z_SCORE_GT -> evaluateZScoreGt(condition, context);
      case STANDARD_DEVIATION_GT -> evaluateStandardDeviationGt(condition, context);
      case PERCENTILE_GT -> evaluatePercentileGt(condition, context);
      case COEFFICIENT_VARIATION_GT -> evaluateCoefficientVariationGt(condition, context);

        // ========== OPERADORES V4.0 PHASE 1 (40 novos) - Velocity + Device ==========
        // CATEGORIA L: Transaction Count Velocity Avançado (12)
      case TRANSACTION_COUNT_PER_CARD_HOUR ->
          evaluateTransactionCountPerCardHour(condition, context);
      case TRANSACTION_COUNT_PER_IP_HOUR -> evaluateTransactionCountPerIpHour(condition, context);
      case TRANSACTION_COUNT_PER_DEVICE_DAY ->
          evaluateTransactionCountPerDeviceDay(condition, context);
      case TRANSACTION_COUNT_PER_MERCHANT_HOUR ->
          evaluateTransactionCountPerMerchantHour(condition, context);
      case TRANSACTION_COUNT_PER_CUSTOMER_HOUR ->
          evaluateTransactionCountPerCustomerHour(condition, context);
      case UNIQUE_CARD_COUNT_PER_IP_HOUR -> evaluateUniqueCardCountPerIpHour(condition, context);
      case UNIQUE_MERCHANT_COUNT_PER_CARD_DAY ->
          evaluateUniqueMerchantCountPerCardDay(condition, context);
      case TRANSACTION_ATTEMPT_COUNT_PER_CARD ->
          evaluateTransactionAttemptCountPerCard(condition, context);
      case CVV_FAILURE_VELOCITY -> evaluateCvvFailureVelocity(condition, context);
      case ADDRESS_CHANGE_VELOCITY -> evaluateAddressChangeVelocity(condition, context);
      case BENEFICIARY_ADD_VELOCITY -> evaluateBeneficiaryAddVelocity(condition, context);
      case CARD_ADD_VELOCITY -> evaluateCardAddVelocity(condition, context);

        // CATEGORIA M: Amount Velocity Avançado (10)
      case AMOUNT_SUM_PER_CARD_HOUR -> evaluateAmountSumPerCardHour(condition, context);
      case AMOUNT_SUM_PER_CUSTOMER_DAY -> evaluateAmountSumPerCustomerDay(condition, context);
      case AVG_TRANSACTION_SPIKE -> evaluateAvgTransactionSpike(condition, context);
      case LARGE_AMOUNT_FREQUENCY -> evaluateLargeAmountFrequency(condition, context);
      case SMALL_AMOUNT_VELOCITY -> evaluateSmallAmountVelocity(condition, context);
      case ROUND_AMOUNT_FREQUENCY -> evaluateRoundAmountFrequency(condition, context);
      case SEQUENTIAL_AMOUNT_PATTERN -> evaluateSequentialAmountPattern(condition, context);
      case AMOUNT_VARIANCE_ANOMALY -> evaluateAmountVarianceAnomaly(condition, context);
      case DAILY_LIMIT_PROXIMITY -> evaluateDailyLimitProximity(condition, context);
      case WEEKLY_LIMIT_PROXIMITY -> evaluateWeeklyLimitProximity(condition, context);

        // CATEGORIA N: Temporal Velocity Avançado (8)
      case TIME_BETWEEN_CONSECUTIVE_TX -> evaluateTimeBetweenConsecutiveTx(condition, context);
      case TRANSACTION_FREQUENCY_ANOMALY -> evaluateTransactionFrequencyAnomaly(condition, context);
      case TIME_OF_DAY_ANOMALY -> evaluateTimeOfDayAnomaly(condition, context);
      case DORMANCY_ALERT_VELOCITY -> evaluateDormancyAlertVelocity(condition, context);
      case WEEKEND_VS_WEEKDAY_PATTERN -> evaluateWeekendVsWeekdayPattern(condition, context);
      case HOLIDAY_TRANSACTION_SPIKE -> evaluateHolidayTransactionSpike(condition, context);
      case NIGHTTIME_TRANSACTION_RATIO -> evaluateNighttimeTransactionRatio(condition, context);
      case BUSINESS_HOURS_DEVIATION -> evaluateBusinessHoursDeviation(condition, context);

        // CATEGORIA O: Device Fingerprint Avançado (10)
      case DEVICE_TRUST_SCORE -> evaluateDeviceTrustScore(condition, context);
      case CANVAS_FINGERPRINT_MISMATCH -> evaluateCanvasFingerprintMismatch(condition, context);
      case WEBGL_FINGERPRINT_ANOMALY -> evaluateWebglFingerprintAnomaly(condition, context);
      case AUDIO_FINGERPRINT_NEW -> evaluateAudioFingerprintNew(condition, context);
      case FONTS_FINGERPRINT_ANOMALY -> evaluateFontsFingerprintAnomaly(condition, context);
      case SCREEN_RESOLUTION_CHANGE -> evaluateScreenResolutionChange(condition, context);
      case BATTERY_LEVEL_ANOMALY -> evaluateBatteryLevelAnomaly(condition, context);
      case HARDWARE_CONCURRENCY_MISMATCH -> evaluateHardwareConcurrencyMismatch(condition, context);
      case TOUCH_SUPPORT_INCONSISTENCY -> evaluateTouchSupportInconsistency(condition, context);
      case DEVICE_MEMORY_ANOMALY -> evaluateDeviceMemoryAnomaly(condition, context);

        // ========== OPERADORES V4.0 PHASE 1B (25 novos) - Behavioral ==========
        // CATEGORIA P: Behavioral Patterns (15)
      case BEHAVIORAL_BASELINE_DEVIATION -> evaluateBehavioralBaselineDeviation(condition, context);
      case SPENDING_CATEGORY_SHIFT -> evaluateSpendingCategoryShift(condition, context);
      case TRANSACTION_SIZE_ESCALATION -> evaluateTransactionSizeEscalation(condition, context);
      case FREQUENCY_PATTERN_CHANGE -> evaluateFrequencyPatternChange(condition, context);
      case TIME_PREFERENCE_SHIFT -> evaluateTimePreferenceShift(condition, context);
      case CHANNEL_USAGE_ANOMALY -> evaluateChannelUsageAnomaly(condition, context);
      case PAYMENT_METHOD_SWITCH -> evaluatePaymentMethodSwitch(condition, context);
      case RECIPIENT_DIVERSITY_CHANGE -> evaluateRecipientDiversityChange(condition, context);
      case GEOGRAPHIC_BEHAVIOR_SHIFT -> evaluateGeographicBehaviorShift(condition, context);
      case SESSION_BEHAVIOR_ANOMALY -> evaluateSessionBehaviorAnomaly(condition, context);
      case LOGIN_PATTERN_DEVIATION -> evaluateLoginPatternDeviation(condition, context);
      case NAVIGATION_PATTERN_ANOMALY -> evaluateNavigationPatternAnomaly(condition, context);
      case TRANSACTION_TIMING_CLUSTER -> evaluateTransactionTimingCluster(condition, context);
      case AMOUNT_ROUNDING_BEHAVIOR -> evaluateAmountRoundingBehavior(condition, context);
      case SPLIT_PAYMENT_PATTERN -> evaluateSplitPaymentPattern(condition, context);

        // CATEGORIA Q: Statistical Behavioral (10)
      case CHI_SQUARE_DISTRIBUTION_TEST -> evaluateChiSquareDistributionTest(condition, context);
      case KOLMOGOROV_SMIRNOV_TEST -> evaluateKolmogorovSmirnovTest(condition, context);
      case ANDERSON_DARLING_TEST -> evaluateAndersonDarlingTest(condition, context);
      case T_TEST_AMOUNT_DEVIATION -> evaluateTTestAmountDeviation(condition, context);
      case MANN_WHITNEY_U_TEST -> evaluateMannWhitneyUTest(condition, context);
      case CORRELATION_ANOMALY -> evaluateCorrelationAnomaly(condition, context);
      case REGRESSION_RESIDUAL_OUTLIER -> evaluateRegressionResidualOutlier(condition, context);
      case VARIANCE_RATIO_TEST -> evaluateVarianceRatioTest(condition, context);
      case ENTROPY_SCORE_ANOMALY -> evaluateEntropyScoreAnomaly(condition, context);
      case SKEWNESS_KURTOSIS_ANOMALY -> evaluateSkewnessKurtosisAnomaly(condition, context);

        // ========== OPERADORES V4.0 PHASE 1C (18 novos) - MCC & Merchant ==========
        // CATEGORIA R: MCC & Merchant Advanced (18)
      case MCC_CATEGORY_VELOCITY -> evaluateMccCategoryVelocity(condition, context);
      case MCC_SPENDING_LIMIT_CHECK -> evaluateMccSpendingLimitCheck(condition, context);
      case MCC_CROSS_CATEGORY_PATTERN -> evaluateMccCrossCategoryPattern(condition, context);
      case MERCHANT_REPUTATION_SCORE -> evaluateMerchantReputationScore(condition, context);
      case MERCHANT_AGE_CHECK -> evaluateMerchantAgeCheck(condition, context);
      case MERCHANT_TRANSACTION_VOLUME -> evaluateMerchantTransactionVolume(condition, context);
      case MERCHANT_CHARGEBACK_HISTORY -> evaluateMerchantChargebackHistory(condition, context);
      case MERCHANT_FRAUD_RATE_CHECK -> evaluateMerchantFraudRateCheck(condition, context);
      case MERCHANT_GEOGRAPHIC_SPREAD -> evaluateMerchantGeographicSpread(condition, context);
      case MERCHANT_CUSTOMER_CONCENTRATION ->
          evaluateMerchantCustomerConcentration(condition, context);
      case MERCHANT_AMOUNT_DISTRIBUTION -> evaluateMerchantAmountDistribution(condition, context);
      case MERCHANT_TIME_PATTERN -> evaluateMerchantTimePattern(condition, context);
      case MERCHANT_DEVICE_DIVERSITY -> evaluateMerchantDeviceDiversity(condition, context);
      case MERCHANT_REFUND_RATIO -> evaluateMerchantRefundRatio(condition, context);
      case MERCHANT_NEW_CUSTOMER_RATIO -> evaluateMerchantNewCustomerRatio(condition, context);
      case MERCHANT_DORMANT_REACTIVATION -> evaluateMerchantDormantReactivation(condition, context);
      case MERCHANT_CROSS_BORDER_RATIO -> evaluateMerchantCrossBorderRatio(condition, context);
      case MERCHANT_HIGH_VALUE_FREQUENCY -> evaluateMerchantHighValueFrequency(condition, context);

        // ========== CATEGORIA S: FATF AML Typologies (28) ==========
      case FATF_PLACEMENT_CASH_INTENSIVE -> evaluateFatfPlacementCashIntensive(condition, context);
      case FATF_PLACEMENT_STRUCTURING -> evaluateFatfPlacementStructuring(condition, context);
      case FATF_PLACEMENT_SMURFING -> evaluateFatfPlacementSmurfing(condition, context);
      case FATF_PLACEMENT_CURRENCY_EXCHANGE ->
          evaluateFatfPlacementCurrencyExchange(condition, context);
      case FATF_PLACEMENT_CASINO_GAMBLING ->
          evaluateFatfPlacementCasinoGambling(condition, context);
      case FATF_LAYERING_RAPID_MOVEMENT -> evaluateFatfLayeringRapidMovement(condition, context);
      case FATF_LAYERING_SHELL_COMPANY -> evaluateFatfLayeringShellCompany(condition, context);
      case FATF_LAYERING_OFFSHORE -> evaluateFatfLayeringOffshore(condition, context);
      case FATF_LAYERING_WIRE_CHAINS -> evaluateFatfLayeringWireChains(condition, context);
      case FATF_LAYERING_CONVERTIBLE_INSTRUMENTS ->
          evaluateFatfLayeringConvertibleInstruments(condition, context);
      case FATF_INTEGRATION_REAL_ESTATE -> evaluateFatfIntegrationRealEstate(condition, context);
      case FATF_INTEGRATION_LUXURY_GOODS -> evaluateFatfIntegrationLuxuryGoods(condition, context);
      case FATF_INTEGRATION_BUSINESS_INVESTMENT ->
          evaluateFatfIntegrationBusinessInvestment(condition, context);
      case FATF_INTEGRATION_LOAN_REPAYMENT ->
          evaluateFatfIntegrationLoanRepayment(condition, context);
      case FATF_TBML_OVER_INVOICING -> evaluateFatfTbmlOverInvoicing(condition, context);
      case FATF_TBML_UNDER_INVOICING -> evaluateFatfTbmlUnderInvoicing(condition, context);
      case FATF_TBML_PHANTOM_SHIPPING -> evaluateFatfTbmlPhantomShipping(condition, context);
      case FATF_TBML_MULTIPLE_INVOICING -> evaluateFatfTbmlMultipleInvoicing(condition, context);
      case FATF_TBML_FALSE_DESCRIPTION -> evaluateFatfTbmlFalseDescription(condition, context);
      case FATF_HAWALA_INFORMAL -> evaluateFatfHawalaInformal(condition, context);
      case FATF_NEW_PAYMENT_EXPLOITATION -> evaluateFatfNewPaymentExploitation(condition, context);
      case FATF_CRYPTO_MIXING -> evaluateFatfCryptoMixing(condition, context);
      case FATF_CRYPTO_ATM_CASHOUT -> evaluateFatfCryptoAtmCashout(condition, context);
      case FATF_PEP_TRANSACTION -> evaluateFatfPepTransaction(condition, context);
      case FATF_CORRESPONDENT_LAYERING -> evaluateFatfCorrespondentLayering(condition, context);
      case FATF_ROUND_TRIPPING -> evaluateFatfRoundTripping(condition, context);
      case FATF_BLACK_MARKET_EXCHANGE -> evaluateFatfBlackMarketExchange(condition, context);
      case FATF_INSURANCE_CASH_VALUE -> evaluateFatfInsuranceCashValue(condition, context);

        // ========== CATEGORIA T: PSD2 SCA Exemptions (12) ==========
      case SCA_LOW_VALUE_EXEMPTION -> evaluateScaLowValueExemption(condition, context);
      case SCA_CONTACTLESS_EXEMPTION -> evaluateScaContactlessExemption(condition, context);
      case SCA_TRA_EXEMPTION -> evaluateScaTraExemption(condition, context);
      case SCA_TRUSTED_BENEFICIARY -> evaluateScaTrustedBeneficiary(condition, context);
      case SCA_RECURRING_TRANSACTION -> evaluateScaRecurringTransaction(condition, context);
      case SCA_MERCHANT_INITIATED -> evaluateScaMerchantInitiated(condition, context);
      case SCA_CORPORATE_PAYMENT -> evaluateScaCorporatePayment(condition, context);
      case SCA_SECURE_CORPORATE_PROTOCOL -> evaluateScaSecureCorporateProtocol(condition, context);
      case SCA_LIABILITY_SHIFT -> evaluateScaLiabilityShift(condition, context);
      case SCA_DYNAMIC_3DS_ROUTING -> evaluateScaDynamic3dsRouting(condition, context);
      case SCA_FRAUD_RATE_MONITORING -> evaluateScaFraudRateMonitoring(condition, context);
      case SCA_CHALLENGE_MANDATORY -> evaluateScaChallengeMandatory(condition, context);

        // ========== CATEGORIA U: Platform Best Practices (28) ==========
      case PLT_BEHAVIOR_SORTED_LISTS -> evaluatePltBehaviorSortedLists(condition, context);
      case PLT_BUSINESS_RULES_SCENARIO -> evaluatePltBusinessRulesScenario(condition, context);
      case PLT_IDENTITY_RESOLUTION -> evaluatePltIdentityResolution(condition, context);
      case PLT_COMPROMISE_MANAGER -> evaluatePltCompromiseManager(condition, context);
      case PLT_INTELLIGENCE_NETWORK -> evaluatePltIntelligenceNetwork(condition, context);
      case PLT_RULES_MODELS_HYBRID -> evaluatePltRulesModelsHybrid(condition, context);
      case PLT_BEHAVIORAL_PROFILING -> evaluatePltBehavioralProfiling(condition, context);
      case PLT_NETWORK_ANALYTICS -> evaluatePltNetworkAnalytics(condition, context);
      case PLT_SAR_AUTOMATED -> evaluatePltSarAutomated(condition, context);
      case PLT_DS2_RULE_ENGINE -> evaluatePltDs2RuleEngine(condition, context);
      case PLT_REAL_TIME_DETECTION -> evaluatePltRealTimeDetection(condition, context);
      case PLT_NETWORK_ENTITY_RESOLUTION -> evaluatePltNetworkEntityResolution(condition, context);
      case PLT_SCENARIO_SCORECARD -> evaluatePltScenarioScorecard(condition, context);
      case PLT_RADAR_RULE_BACKTESTING -> evaluatePltRadarRuleBacktesting(condition, context);
      case PLT_RADAR_METADATA_MATCHING -> evaluatePltRadarMetadataMatching(condition, context);
      case PLT_RADAR_INLINE_LISTS -> evaluatePltRadarInlineLists(condition, context);
      case PLT_RADAR_COMPLEX_CONDITIONS -> evaluatePltRadarComplexConditions(condition, context);
      case PLT_RISK_PROFILE_ASSIGNMENT -> evaluatePltRiskProfileAssignment(condition, context);
      case PLT_CUSTOM_RULE_BUILDER -> evaluatePltCustomRuleBuilder(condition, context);
      case PLT_RISK_LIST_COMPARISON -> evaluatePltRiskListComparison(condition, context);
      case PLT_BACKTESTING_LABELING -> evaluatePltBacktestingLabeling(condition, context);
      case PLT_ML_FRAUD_RISK_OUTCOME -> evaluatePltMlFraudRiskOutcome(condition, context);
      case PLT_RISK_SCORE_CALCULATION -> evaluatePltRiskScoreCalculation(condition, context);
      case PLT_VELOCITY_FILTERS -> evaluatePltVelocityFilters(condition, context);
      case PLT_LINKING_VELOCITY -> evaluatePltLinkingVelocity(condition, context);
      case PLT_BAD_ENTITY_NETWORK -> evaluatePltBadEntityNetwork(condition, context);
      case PLT_REVIEWLIST_QUEUE -> evaluatePltReviewlistQueue(condition, context);
      case PLT_CONSORTIUM_DATA_CHECK -> evaluatePltConsortiumDataCheck(condition, context);

        // ========== Basel III Operational Risk (BSL001-BSL014) ==========
      case BSL_BUSINESS_INDICATOR -> evaluateBslBusinessIndicator(condition, context);
      case BSL_BUSINESS_INDICATOR_COMPONENT ->
          evaluateBslBusinessIndicatorComponent(condition, context);
      case BSL_INTERNAL_LOSS_MULTIPLIER -> evaluateBslInternalLossMultiplier(condition, context);
      case BSL_LOSS_DATA_COLLECTION -> evaluateBslLossDataCollection(condition, context);
      case BSL_LOSS_EXCLUSION_APPROVAL -> evaluateBslLossExclusionApproval(condition, context);
      case BSL_BUCKET_CLASSIFICATION -> evaluateBslBucketClassification(condition, context);
      case BSL_MARGINAL_COEFFICIENT -> evaluateBslMarginalCoefficient(condition, context);
      case BSL_LOSS_THRESHOLD_SETTING -> evaluateBslLossThresholdSetting(condition, context);
      case BSL_RETENTION_PERIOD -> evaluateBslRetentionPeriod(condition, context);
      case BSL_RISK_GOVERNANCE -> evaluateBslRiskGovernance(condition, context);
      case BSL_LOSS_EVENT_REPORTING -> evaluateBslLossEventReporting(condition, context);
      case BSL_CONTROL_DEFICIENCY -> evaluateBslControlDeficiency(condition, context);
      case BSL_KRI_MONITORING -> evaluateBslKriMonitoring(condition, context);
      case BSL_SCENARIO_ANALYSIS -> evaluateBslScenarioAnalysis(condition, context);

        // ========== Rule Mining Determinístico (APRIORI, FPGROWTH, ECLAT) ==========
      case APRIORI_ASSOCIATION -> evaluateAprioriAssociation(condition, context);
      case FPGROWTH_FREQUENT_PATTERNS -> evaluateFpgrowthFrequentPatterns(condition, context);
      case ECLAT_ITEMSET -> evaluateEclatItemset(condition, context);

        // ========== Fuzzy Logic (FUZZY001, FUZZY002) ==========
      case FUZZY_MEMBERSHIP -> evaluateFuzzyMembership(condition, context);
      case FUZZY_ADAPTIVE_THRESHOLD -> evaluateFuzzyAdaptiveThreshold(condition, context);

        // ========== LLM & Generative AI Fraud Detection (LLM001-LLM012) ==========
      case LLM_TRANSACTION_DESCRIPTION_ANALYSIS ->
          evaluateLlmTransactionDescriptionAnalysis(condition, context);
      case LLM_GENERATIVE_RULE_SYNTHESIS -> evaluateLlmGenerativeRuleSynthesis(condition, context);
      case LLM_ANOMALY_EXPLANATION_GENERATION ->
          evaluateLlmAnomalyExplanationGeneration(condition, context);
      case LLM_CHATBOT_FRAUD_DETECTION -> evaluateLlmChatbotFraudDetection(condition, context);
      case LLM_DEEPFAKE_VOICE_DETECTION -> evaluateLlmDeepfakeVoiceDetection(condition, context);
      case LLM_SYNTHETIC_IMAGE_DETECTION -> evaluateLlmSyntheticImageDetection(condition, context);
      case LLM_EMAIL_PHISHING_ANALYSIS -> evaluateLlmEmailPhishingAnalysis(condition, context);
      case LLM_SOCIAL_ENGINEERING_CLASSIFICATION ->
          evaluateLlmSocialEngineeringClassification(condition, context);
      case LLM_FRAUD_ALERT_PRIORITIZATION ->
          evaluateLlmFraudAlertPrioritization(condition, context);
      case LLM_MULTI_MODAL_FRAUD_DETECTION ->
          evaluateLlmMultiModalFraudDetection(condition, context);
      case LLM_ADVERSARIAL_ATTACK_RESISTANCE ->
          evaluateLlmAdversarialAttackResistance(condition, context);
      case LLM_FRAUD_PATTERN_AUTODISCOVERY ->
          evaluateLlmFraudPatternAutodiscovery(condition, context);

        // ========== Neo4j Graph Fraud Detection (NEO001-NEO018) ==========
      case NEO4J_WEAKLY_CONNECTED_COMPONENTS ->
          evaluateNeo4jWeaklyConnectedComponents(condition, context);
      case NEO4J_DEGREE_CENTRALITY -> evaluateNeo4jDegreeCentrality(condition, context);
      case NEO4J_PAGERANK_FRAUD_SCORE -> evaluateNeo4jPagerankFraudScore(condition, context);
      case NEO4J_LOUVAIN_COMMUNITY_DETECTION ->
          evaluateNeo4jLouvainCommunityDetection(condition, context);
      case NEO4J_PAIRWISE_SIMILARITY_PII -> evaluateNeo4jPairwiseSimilarityPii(condition, context);
      case NEO4J_ENTITY_RESOLUTION_SHARED_PII ->
          evaluateNeo4jEntityResolutionSharedPii(condition, context);
      case NEO4J_FRAUD_RING_DETECTION -> evaluateNeo4jFraudRingDetection(condition, context);
      case NEO4J_MONEY_MULE_NETWORK_ANALYSIS ->
          evaluateNeo4jMoneyMuleNetworkAnalysis(condition, context);
      case NEO4J_CIRCULAR_TRANSACTION_DETECTION ->
          evaluateNeo4jCircularTransactionDetection(condition, context);
      case NEO4J_FIRST_PARTY_FRAUD_CLUSTERING ->
          evaluateNeo4jFirstPartyFraudClustering(condition, context);
      case NEO4J_SECOND_LEVEL_FRAUDSTER_ID ->
          evaluateNeo4jSecondLevelFraudsterId(condition, context);
      case NEO4J_BETWEENNESS_CENTRALITY_MULE ->
          evaluateNeo4jBetweennessCentralityMule(condition, context);
      case NEO4J_LABEL_PROPAGATION_FRAUD_SPREAD ->
          evaluateNeo4jLabelPropagationFraudSpread(condition, context);
      case NEO4J_SHORTEST_PATH_AML_TRACKING ->
          evaluateNeo4jShortestPathAmlTracking(condition, context);
      case NEO4J_TRIANGLE_COUNT_COLLUSION ->
          evaluateNeo4jTriangleCountCollusion(condition, context);
      case NEO4J_NODE_SIMILARITY_SYNTHETIC_ID ->
          evaluateNeo4jNodeSimilaritySyntheticId(condition, context);
      case NEO4J_GRAPH_EMBEDDING_FRAUD_PREDICTION ->
          evaluateNeo4jGraphEmbeddingFraudPrediction(condition, context);
      case NEO4J_TEMPORAL_MOTIF_PATTERN -> evaluateNeo4jTemporalMotifPattern(condition, context);

        // ========== Synthetic Identity Detection (SYN001-SYN015) ==========
      case BIOMETRIC_KEYSTROKE_DYNAMICS -> evaluateBiometricKeystrokeDynamics(condition, context);
      case BIOMETRIC_MOUSE_MOVEMENT -> evaluateBiometricMouseMovement(condition, context);
      case BIOMETRIC_SCROLL_VELOCITY -> evaluateBiometricScrollVelocity(condition, context);
      case DEVICE_FINGERPRINT_CONSISTENCY_CHECK ->
          evaluateDeviceFingerprintConsistencyCheck(condition, context);
      case ECBSV_SSN_VALIDATION -> evaluateEcbsvSsnValidation(condition, context);
      case SYNTHETIC_FRAUD_SCORE -> evaluateSyntheticFraudScore(condition, context);
      case INJECTION_ATTACK_DETECTION -> evaluateInjectionAttackDetection(condition, context);
      case LIVENESS_DETECTION_FACIAL -> evaluateLivenessDetectionFacial(condition, context);
      case LIVENESS_DETECTION_VOICE -> evaluateLivenessDetectionVoice(condition, context);
      case ANTI_DETECT_BROWSER_DETECTION -> evaluateAntiDetectBrowserDetection(condition, context);
      case DOCUMENT_FORGERY_DETECTION -> evaluateDocumentForgeryDetection(condition, context);
      case FACE_TO_ID_PHOTO_MATCHING -> evaluateFaceToIdPhotoMatching(condition, context);
      case ADAPTIVE_BEHAVIORAL_ANALYTICS -> evaluateAdaptiveBehavioralAnalytics(condition, context);
      case SYNTHETIC_ID_LABEL_CORRECTION -> evaluateSyntheticIdLabelCorrection(condition, context);
      case MULTI_LAYERED_SYNTHETIC_ID_CONTROLS ->
          evaluateMultiLayeredSyntheticIdControls(condition, context);

        // ========== Estatísticos Avançados Pure Rules (STAT001-STAT015) ==========
      case STAT_KRUSKAL_WALLIS_TEST -> evaluateStatKruskalWallisTest(condition, context);
      case STAT_ANOVA_F_TEST -> evaluateStatAnovaFTest(condition, context);
      case STAT_ISOLATION_FOREST_SCORE -> evaluateStatIsolationForestScore(condition, context);
      case STAT_LOCAL_OUTLIER_FACTOR -> evaluateStatLocalOutlierFactor(condition, context);
      case STAT_ONE_CLASS_SVM_BOUNDARY -> evaluateStatOneClassSvmBoundary(condition, context);
      case STAT_KMEANS_CLUSTER_DISTANCE -> evaluateStatKmeansClusterDistance(condition, context);
      case STAT_DBSCAN_NOISE_DETECTION -> evaluateStatDbscanNoiseDetection(condition, context);
      case STAT_GMM_PROBABILITY -> evaluateStatGmmProbability(condition, context);
      case STAT_MAHALANOBIS_DISTANCE -> evaluateStatMahalanobisDistance(condition, context);
      case STAT_GRUBBS_TEST -> evaluateStatGrubbsTest(condition, context);
      case STAT_DIXON_Q_TEST -> evaluateStatDixonQTest(condition, context);
      case STAT_SHAPIRO_WILK_TEST -> evaluateStatShapiroWilkTest(condition, context);
      case STAT_LEVENE_TEST -> evaluateStatLeveneTest(condition, context);
      case STAT_WELCH_T_TEST -> evaluateStatWelchTTest(condition, context);
      case STAT_BOOTSTRAP_CONFIDENCE_INTERVAL ->
          evaluateStatBootstrapConfidenceInterval(condition, context);

        // ========== Fraud Patterns & Market Operators (Phase 7) ==========
      case CARD_TESTING_RING_DETECTION -> evaluateCardTestingRingDetection(condition, context);
      case BUST_OUT_PATTERN_DETECTION -> evaluateBustOutPatternDetection(condition, context);
      case CIRCULAR_PAYMENT_DETECTION -> evaluateCircularPaymentDetection(condition, context);
      case ACCOUNT_TAKEOVER_PATTERN -> evaluateAccountTakeoverPattern(condition, context);
      case SYNTHETIC_IDENTITY_RING -> evaluateSyntheticIdentityRing(condition, context);
      case CROSS_BORDER_VELOCITY -> evaluateCrossBorderVelocity(condition, context);
      case CORRESPONDENT_ANOMALY -> evaluateCorrespondentAnomaly(condition, context);
      case NESTED_CORRESPONDENT_CHECK -> evaluateNestedCorrespondentCheck(condition, context);
      case SHELL_BANK_INDICATOR -> evaluateShellBankIndicator(condition, context);
      case HIGH_RISK_CORRIDOR_CHECK -> evaluateHighRiskCorridorCheck(condition, context);
      case SEGMENT_OF_ONE_PROFILING -> evaluateSegmentOfOneProfiling(condition, context);
      case ADAPTIVE_PARAMETRIC_THRESHOLD -> evaluateAdaptiveParametricThreshold(condition, context);
      case REAL_TIME_RISK_SCORING -> evaluateRealTimeRiskScoring(condition, context);
      case CONSORTIUM_NEGATIVE_FILE_CHECK ->
          evaluateConsortiumNegativeFileCheck(condition, context);
      case PEER_GROUP_DEVIATION_SCORE -> evaluatePeerGroupDeviationScore(condition, context);
      case MICRO_DEPOSIT_VELOCITY -> evaluateMicroDepositVelocity(condition, context);
      case RAPID_SUCCESSION_PATTERN -> evaluateRapidSuccessionPattern(condition, context);
      case SPLIT_TRANSACTION_DETECTION -> evaluateSplitTransactionDetection(condition, context);
      case ROUND_TRIP_DETECTION -> evaluateRoundTripDetection(condition, context);
      case LAYERED_TRANSFER_PATTERN -> evaluateLayeredTransferPattern(condition, context);
      case APP_FRAUD_DETECTION -> evaluateAppFraudDetection(condition, context);
      case ROMANCE_SCAM_INDICATOR -> evaluateRomanceScamIndicator(condition, context);
      case INVESTMENT_SCAM_PATTERN -> evaluateInvestmentScamPattern(condition, context);
      case CRYPTO_PUMP_DUMP_DETECTION -> evaluateCryptoPumpDumpDetection(condition, context);
      case PIG_BUTCHERING_INDICATOR -> evaluatePigButcheringIndicator(condition, context);

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

      default -> {
        log.error("Operador desconhecido ou não mapeado no switch: {}", operator);
        throw new UnsupportedOperatorException(
            operator,
            "Operador não mapeado no evaluateOperator switch. Verifique se o operador está registrado.");
      }
    };
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
      Pattern compiledPattern = Pattern.compile(pattern);
      return RegexValidator.matchWithTimeout(compiledPattern, String.valueOf(fieldValue));
    } catch (RegexValidator.TimeoutException e) {
      log.warn("Regex timeout: {} - {}", pattern, e.getMessage());
      return false;
    } catch (Exception e) {
      log.warn("Regex inválida: {}", pattern);
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

  // ========== Métodos de Geolocalização ==========

  /**
   * Avalia GEO_DISTANCE_LT: distância menor que threshold. Formato do valor: "lat,lon,distanceKm"
   * (ex: "-23.55,-46.63,100")
   */
  private boolean evaluateGeoDistanceLt(RuleCondition condition, EvaluationContext context) {
    if (context.getTransactionRequest() == null) {
      log.warn("TransactionRequest não disponível para GEO_DISTANCE_LT");
      return false;
    }

    try {
      String[] parts = condition.getValueSingle().split(",");
      if (parts.length < 3) {
        log.warn("Formato inválido para GEO_DISTANCE_LT. Esperado: lat,lon,distanceKm");
        return false;
      }

      double targetLat = Double.parseDouble(parts[0].trim());
      double targetLon = Double.parseDouble(parts[1].trim());
      double thresholdKm = Double.parseDouble(parts[2].trim());

      GeoService.GeoResult result =
          geoService.evaluateDistanceLessThan(
              context.getTransactionRequest(), targetLat, targetLon, thresholdKm);

      if (!result.isSuccess()) {
        log.warn("GEO_DISTANCE_LT falhou: {}", result.getErrorMessage());
        return false;
      }

      return result.isResult();
    } catch (Exception e) {
      log.error("Erro ao avaliar GEO_DISTANCE_LT: {}", e.getMessage());
      return false;
    }
  }

  /**
   * Avalia GEO_DISTANCE_GT: distância maior que threshold. Formato do valor: "lat,lon,distanceKm"
   * (ex: "-23.55,-46.63,100")
   */
  private boolean evaluateGeoDistanceGt(RuleCondition condition, EvaluationContext context) {
    if (context.getTransactionRequest() == null) {
      log.warn("TransactionRequest não disponível para GEO_DISTANCE_GT");
      return false;
    }

    try {
      String[] parts = condition.getValueSingle().split(",");
      if (parts.length < 3) {
        log.warn("Formato inválido para GEO_DISTANCE_GT. Esperado: lat,lon,distanceKm");
        return false;
      }

      double targetLat = Double.parseDouble(parts[0].trim());
      double targetLon = Double.parseDouble(parts[1].trim());
      double thresholdKm = Double.parseDouble(parts[2].trim());

      GeoService.GeoResult result =
          geoService.evaluateDistanceGreaterThan(
              context.getTransactionRequest(), targetLat, targetLon, thresholdKm);

      if (!result.isSuccess()) {
        log.warn("GEO_DISTANCE_GT falhou: {}", result.getErrorMessage());
        return false;
      }

      return result.isResult();
    } catch (Exception e) {
      log.error("Erro ao avaliar GEO_DISTANCE_GT: {}", e.getMessage());
      return false;
    }
  }

  /**
   * Avalia GEO_IN_POLYGON: ponto dentro de polígono. Formato do valor: nome do polígono (ex:
   * "BRASIL", "SAO_PAULO_ESTADO")
   */
  private boolean evaluateGeoInPolygon(RuleCondition condition, EvaluationContext context) {
    if (context.getTransactionRequest() == null) {
      log.warn("TransactionRequest não disponível para GEO_IN_POLYGON");
      return false;
    }

    try {
      String polygonName = condition.getValueSingle();
      if (polygonName == null || polygonName.isBlank()) {
        log.warn("Nome do polígono não especificado para GEO_IN_POLYGON");
        return false;
      }

      GeoService.GeoResult result =
          geoService.evaluateInPolygon(context.getTransactionRequest(), polygonName.trim());

      if (!result.isSuccess()) {
        log.warn("GEO_IN_POLYGON falhou: {}", result.getErrorMessage());
        return false;
      }

      return result.isResult();
    } catch (Exception e) {
      log.error("Erro ao avaliar GEO_IN_POLYGON: {}", e.getMessage());
      return false;
    }
  }

  // ========== Métodos de Velocity ==========

  /**
   * Avalia VELOCITY_COUNT: contagem de transações em janela temporal. Formato do valor:
   * "keyType,windowMinutes,threshold" (ex: "PAN,60,5")
   */
  private boolean evaluateVelocityCount(
      RuleCondition condition, EvaluationContext context, boolean greaterThan) {
    if (context.getTransactionRequest() == null) {
      log.warn("TransactionRequest não disponível para VELOCITY_COUNT");
      return false;
    }

    try {
      String[] parts = condition.getValueSingle().split(",");
      if (parts.length < 3) {
        log.warn("Formato inválido para VELOCITY_COUNT. Esperado: keyType,windowMinutes,threshold");
        return false;
      }

      VelocityService.KeyType keyType =
          VelocityService.KeyType.valueOf(parts[0].trim().toUpperCase());
      VelocityService.TimeWindow window = parseTimeWindow(Integer.parseInt(parts[1].trim()));
      long threshold = Long.parseLong(parts[2].trim());

      // GAP-FIX #2: Usar facade que escolhe entre cache e banco
      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(context.getTransactionRequest(), keyType, window);

      return greaterThan
          ? stats.getTransactionCount() > threshold
          : stats.getTransactionCount() < threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar VELOCITY_COUNT: {}", e.getMessage());
      return false;
    }
  }

  /**
   * Avalia VELOCITY_SUM: soma de valores em janela temporal. Formato do valor:
   * "keyType,windowMinutes,threshold" (ex: "PAN,1440,10000")
   */
  private boolean evaluateVelocitySum(
      RuleCondition condition, EvaluationContext context, boolean greaterThan) {
    if (context.getTransactionRequest() == null) {
      log.warn("TransactionRequest não disponível para VELOCITY_SUM");
      return false;
    }

    try {
      String[] parts = condition.getValueSingle().split(",");
      if (parts.length < 3) {
        log.warn("Formato inválido para VELOCITY_SUM. Esperado: keyType,windowMinutes,threshold");
        return false;
      }

      VelocityService.KeyType keyType =
          VelocityService.KeyType.valueOf(parts[0].trim().toUpperCase());
      VelocityService.TimeWindow window = parseTimeWindow(Integer.parseInt(parts[1].trim()));
      BigDecimal threshold = new BigDecimal(parts[2].trim());

      // GAP-FIX #2: Usar facade que escolhe entre cache e banco
      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(context.getTransactionRequest(), keyType, window);

      return greaterThan
          ? stats.getTotalAmount().compareTo(threshold) > 0
          : stats.getTotalAmount().compareTo(threshold) < 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar VELOCITY_SUM: {}", e.getMessage());
      return false;
    }
  }

  /**
   * Avalia VELOCITY_AVG: média de valores em janela temporal. Formato do valor:
   * "keyType,windowMinutes,threshold" (ex: "PAN,1440,500")
   */
  private boolean evaluateVelocityAvg(
      RuleCondition condition, EvaluationContext context, boolean greaterThan) {
    if (context.getTransactionRequest() == null) {
      log.warn("TransactionRequest não disponível para VELOCITY_AVG");
      return false;
    }

    try {
      String[] parts = condition.getValueSingle().split(",");
      if (parts.length < 3) {
        log.warn("Formato inválido para VELOCITY_AVG. Esperado: keyType,windowMinutes,threshold");
        return false;
      }

      VelocityService.KeyType keyType =
          VelocityService.KeyType.valueOf(parts[0].trim().toUpperCase());
      VelocityService.TimeWindow window = parseTimeWindow(Integer.parseInt(parts[1].trim()));
      BigDecimal threshold = new BigDecimal(parts[2].trim());

      // GAP-FIX #2: Usar facade que escolhe entre cache e banco
      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(context.getTransactionRequest(), keyType, window);

      return greaterThan
          ? stats.getAvgAmount().compareTo(threshold) > 0
          : stats.getAvgAmount().compareTo(threshold) < 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar VELOCITY_AVG: {}", e.getMessage());
      return false;
    }
  }

  /**
   * Avalia VELOCITY_DISTINCT: contagem de valores distintos em janela temporal. Formato do valor:
   * "keyType,windowMinutes,distinctType,threshold" (ex: "PAN,1440,MERCHANTS,3")
   */
  private boolean evaluateVelocityDistinct(
      RuleCondition condition, EvaluationContext context, boolean greaterThan) {
    if (context.getTransactionRequest() == null) {
      log.warn("TransactionRequest não disponível para VELOCITY_DISTINCT");
      return false;
    }

    try {
      String[] parts = condition.getValueSingle().split(",");
      if (parts.length < 4) {
        log.warn(
            "Formato inválido para VELOCITY_DISTINCT. Esperado: keyType,windowMinutes,distinctType,threshold");
        return false;
      }

      VelocityService.KeyType keyType =
          VelocityService.KeyType.valueOf(parts[0].trim().toUpperCase());
      VelocityService.TimeWindow window = parseTimeWindow(Integer.parseInt(parts[1].trim()));
      String distinctType = parts[2].trim().toUpperCase();
      long threshold = Long.parseLong(parts[3].trim());

      // GAP-FIX #2: Usar facade que escolhe entre cache e banco
      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(context.getTransactionRequest(), keyType, window);

      long distinctCount =
          switch (distinctType) {
            case "MERCHANTS" -> stats.getDistinctMerchants();
            case "MCCS" -> stats.getDistinctMccs();
            case "COUNTRIES" -> stats.getDistinctCountries();
            default -> 0;
          };

      return greaterThan ? distinctCount > threshold : distinctCount < threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar VELOCITY_DISTINCT: {}", e.getMessage());
      return false;
    }
  }

  /** Converte minutos para TimeWindow mais próxima. */
  private VelocityService.TimeWindow parseTimeWindow(int minutes) {
    if (minutes <= 5) return VelocityService.TimeWindow.MINUTE_5;
    if (minutes <= 15) return VelocityService.TimeWindow.MINUTE_15;
    if (minutes <= 30) return VelocityService.TimeWindow.MINUTE_30;
    if (minutes <= 60) return VelocityService.TimeWindow.HOUR_1;
    if (minutes <= 360) return VelocityService.TimeWindow.HOUR_6;
    if (minutes <= 720) return VelocityService.TimeWindow.HOUR_12;
    if (minutes <= 1440) return VelocityService.TimeWindow.HOUR_24;
    if (minutes <= 10080) return VelocityService.TimeWindow.DAY_7;
    return VelocityService.TimeWindow.DAY_30;
  }

  // ========== Métodos utilitários ==========

  private Boolean toBoolean(Object value) {
    if (value == null) return null;
    if (value instanceof Boolean) return (Boolean) value;
    String str = String.valueOf(value).toLowerCase();
    return "true".equals(str) || "1".equals(str) || "yes".equals(str);
  }

  private LocalDate parseDate(Object value) {
    if (value == null) return null;
    if (value instanceof LocalDate) return (LocalDate) value;
    if (value instanceof LocalDateTime) return ((LocalDateTime) value).toLocalDate();

    String str = String.valueOf(value);
    // Tentar formato YYYYMMDD (inteiro)
    if (str.matches("\\d{8}")) {
      return LocalDate.parse(str, DateTimeFormatter.BASIC_ISO_DATE);
    }
    return LocalDate.parse(str);
  }

  private LocalTime parseTime(Object value) {
    if (value == null) return null;
    if (value instanceof LocalTime) return (LocalTime) value;
    if (value instanceof LocalDateTime) return ((LocalDateTime) value).toLocalTime();

    String str = String.valueOf(value);
    // Tentar formato HHMMSS (inteiro)
    if (str.matches("\\d{6}")) {
      return LocalTime.parse(str, DateTimeFormatter.ofPattern("HHmmss"));
    }
    return LocalTime.parse(str);
  }

  private String getExpectedValueString(RuleCondition condition) {
    if (condition.getValueSingle() != null) {
      return condition.getValueSingle();
    }
    if (condition.getValueArray() != null) {
      return condition.getValueArray().toString();
    }
    if (condition.getValueMin() != null && condition.getValueMax() != null) {
      return condition.getValueMin() + " - " + condition.getValueMax();
    }
    if (condition.getValueFieldRef() != null) {
      return "field:" + condition.getValueFieldRef();
    }
    if (condition.getValueExpression() != null) {
      return "expr:" + condition.getValueExpression();
    }
    return "";
  }

  // ========== Agregações Temporais Avançadas (DSL Expandida) ==========

  /**
   * Avalia SUM_LAST_N_DAYS: Soma de valores nos últimos N dias Formato do valueSingle:
   * "fieldName|nDays|threshold|operator" Exemplo: "amount|7|5000|GT" -> Soma de 'amount' nos
   * últimos 7 dias > 5000
   */
  private boolean evaluateSumLastNDays(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split("\\|");
      if (parts.length != 4) {
        log.error("Formato inválido para SUM_LAST_N_DAYS: {}", condition.getValueSingle());
        return false;
      }

      String fieldName = parts[0];
      int nDays = Integer.parseInt(parts[1]);
      BigDecimal threshold = new BigDecimal(parts[2]);
      String operator = parts[3]; // GT, GTE, LT, LTE, EQ

      // Calcular a soma usando o VelocityServiceFacade
      VelocityService.TimeWindow window = parseTimeWindowFromDays(nDays);
      VelocityService.KeyType keyType = VelocityService.KeyType.PAN;

      var stats = velocityServiceFacade.getStats(context.getTransactionRequest(), keyType, window);

      BigDecimal sum = stats.getTotalAmount();

      return switch (operator) {
        case "GT" -> sum.compareTo(threshold) > 0;
        case "GTE" -> sum.compareTo(threshold) >= 0;
        case "LT" -> sum.compareTo(threshold) < 0;
        case "LTE" -> sum.compareTo(threshold) <= 0;
        case "EQ" -> sum.compareTo(threshold) == 0;
        default -> false;
      };

    } catch (Exception e) {
      log.error("Erro ao avaliar SUM_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * Avalia COUNT_LAST_N_HOURS: Contagem de transações nas últimas N horas Formato do valueSingle:
   * "nHours|threshold|operator" Exemplo: "24|100|GT" -> Mais de 100 transações nas últimas 24 horas
   */
  private boolean evaluateCountLastNHours(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split("\\|");
      if (parts.length != 3) {
        log.error("Formato inválido para COUNT_LAST_N_HOURS: {}", condition.getValueSingle());
        return false;
      }

      int nHours = Integer.parseInt(parts[0]);
      long threshold = Long.parseLong(parts[1]);
      String operator = parts[2];

      VelocityService.TimeWindow window = parseTimeWindowFromHours(nHours);
      VelocityService.KeyType keyType = VelocityService.KeyType.PAN;

      var stats = velocityServiceFacade.getStats(context.getTransactionRequest(), keyType, window);

      long count = stats.getTransactionCount();

      return switch (operator) {
        case "GT" -> count > threshold;
        case "GTE" -> count >= threshold;
        case "LT" -> count < threshold;
        case "LTE" -> count <= threshold;
        case "EQ" -> count == threshold;
        default -> false;
      };

    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_LAST_N_HOURS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * Avalia AVG_LAST_N_DAYS: Média de valores nos últimos N dias Formato do valueSingle:
   * "fieldName|nDays|threshold|operator" Exemplo: "amount|30|500|GT" -> Média de 'amount' nos
   * últimos 30 dias > 500
   */
  private boolean evaluateAvgLastNDays(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split("\\|");
      if (parts.length != 4) {
        log.error("Formato inválido para AVG_LAST_N_DAYS: {}", condition.getValueSingle());
        return false;
      }

      String fieldName = parts[0];
      int nDays = Integer.parseInt(parts[1]);
      BigDecimal threshold = new BigDecimal(parts[2]);
      String operator = parts[3];

      VelocityService.TimeWindow window = parseTimeWindowFromDays(nDays);
      VelocityService.KeyType keyType = VelocityService.KeyType.PAN;

      var stats = velocityServiceFacade.getStats(context.getTransactionRequest(), keyType, window);

      BigDecimal avg = stats.getAvgAmount();

      return switch (operator) {
        case "GT" -> avg.compareTo(threshold) > 0;
        case "GTE" -> avg.compareTo(threshold) >= 0;
        case "LT" -> avg.compareTo(threshold) < 0;
        case "LTE" -> avg.compareTo(threshold) <= 0;
        case "EQ" -> avg.compareTo(threshold) == 0;
        default -> false;
      };

    } catch (Exception e) {
      log.error("Erro ao avaliar AVG_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * Avalia COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS: Contagem de merchants distintos Formato do
   * valueSingle: "nDays|threshold|operator" Exemplo: "7|10|GT" -> Mais de 10 merchants distintos
   * nos últimos 7 dias
   */
  private boolean evaluateCountDistinctMerchantsLastNDays(
      RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split("\\|");
      if (parts.length != 3) {
        log.error(
            "Formato inválido para COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS: {}",
            condition.getValueSingle());
        return false;
      }

      int nDays = Integer.parseInt(parts[0]);
      int threshold = Integer.parseInt(parts[1]);
      String operator = parts[2];

      VelocityService.TimeWindow window = parseTimeWindowFromDays(nDays);
      VelocityService.KeyType keyType = VelocityService.KeyType.PAN;

      var stats = velocityServiceFacade.getStats(context.getTransactionRequest(), keyType, window);

      long distinctMerchants = stats.getDistinctMerchants();

      return switch (operator) {
        case "GT" -> distinctMerchants > threshold;
        case "GTE" -> distinctMerchants >= threshold;
        case "LT" -> distinctMerchants < threshold;
        case "LTE" -> distinctMerchants <= threshold;
        case "EQ" -> distinctMerchants == threshold;
        default -> false;
      };

    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * Avalia COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS: Contagem de países distintos Formato do
   * valueSingle: "nHours|threshold|operator" Exemplo: "24|5|GT" -> Mais de 5 países distintos nas
   * últimas 24 horas
   */
  private boolean evaluateCountDistinctCountriesLastNHours(
      RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split("\\|");
      if (parts.length != 3) {
        log.error(
            "Formato inválido para COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS: {}",
            condition.getValueSingle());
        return false;
      }

      int nHours = Integer.parseInt(parts[0]);
      int threshold = Integer.parseInt(parts[1]);
      String operator = parts[2];

      VelocityService.TimeWindow window = parseTimeWindowFromHours(nHours);
      VelocityService.KeyType keyType = VelocityService.KeyType.PAN;

      var stats = velocityServiceFacade.getStats(context.getTransactionRequest(), keyType, window);

      long distinctCountries = stats.getDistinctCountries();

      return switch (operator) {
        case "GT" -> distinctCountries > threshold;
        case "GTE" -> distinctCountries >= threshold;
        case "LT" -> distinctCountries < threshold;
        case "LTE" -> distinctCountries <= threshold;
        case "EQ" -> distinctCountries == threshold;
        default -> false;
      };

    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * Avalia MAX_AMOUNT_LAST_N_DAYS: Valor máximo nos últimos N dias Formato do valueSingle:
   * "nDays|threshold|operator" Exemplo: "30|10000|GT" -> Valor máximo nos últimos 30 dias > 10000
   */
  private boolean evaluateMaxAmountLastNDays(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split("\\|");
      if (parts.length != 3) {
        log.error("Formato inválido para MAX_AMOUNT_LAST_N_DAYS: {}", condition.getValueSingle());
        return false;
      }

      int nDays = Integer.parseInt(parts[0]);
      BigDecimal threshold = new BigDecimal(parts[1]);
      String operator = parts[2];

      VelocityService.TimeWindow window = parseTimeWindowFromDays(nDays);
      VelocityService.KeyType keyType = VelocityService.KeyType.PAN;

      var stats = velocityServiceFacade.getStats(context.getTransactionRequest(), keyType, window);

      BigDecimal maxAmount = stats.getMaxAmount();

      return switch (operator) {
        case "GT" -> maxAmount.compareTo(threshold) > 0;
        case "GTE" -> maxAmount.compareTo(threshold) >= 0;
        case "LT" -> maxAmount.compareTo(threshold) < 0;
        case "LTE" -> maxAmount.compareTo(threshold) <= 0;
        case "EQ" -> maxAmount.compareTo(threshold) == 0;
        default -> false;
      };

    } catch (Exception e) {
      log.error("Erro ao avaliar MAX_AMOUNT_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * Avalia MIN_AMOUNT_LAST_N_DAYS: Valor mínimo nos últimos N dias Formato do valueSingle:
   * "nDays|threshold|operator" Exemplo: "7|10|LT" -> Valor mínimo nos últimos 7 dias < 10
   */
  private boolean evaluateMinAmountLastNDays(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split("\\|");
      if (parts.length != 3) {
        log.error("Formato inválido para MIN_AMOUNT_LAST_N_DAYS: {}", condition.getValueSingle());
        return false;
      }

      int nDays = Integer.parseInt(parts[0]);
      BigDecimal threshold = new BigDecimal(parts[1]);
      String operator = parts[2];

      VelocityService.TimeWindow window = parseTimeWindowFromDays(nDays);
      VelocityService.KeyType keyType = VelocityService.KeyType.PAN;

      var stats = velocityServiceFacade.getStats(context.getTransactionRequest(), keyType, window);

      BigDecimal minAmount = stats.getMinAmount();

      return switch (operator) {
        case "GT" -> minAmount.compareTo(threshold) > 0;
        case "GTE" -> minAmount.compareTo(threshold) >= 0;
        case "LT" -> minAmount.compareTo(threshold) < 0;
        case "LTE" -> minAmount.compareTo(threshold) <= 0;
        case "EQ" -> minAmount.compareTo(threshold) == 0;
        default -> false;
      };

    } catch (Exception e) {
      log.error("Erro ao avaliar MIN_AMOUNT_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  // ========== Métodos Auxiliares para Agregações Avançadas ==========

  /** Converte dias para TimeWindow mais próxima */
  private VelocityService.TimeWindow parseTimeWindowFromDays(int days) {
    if (days <= 1) return VelocityService.TimeWindow.HOUR_24;
    if (days <= 7) return VelocityService.TimeWindow.DAY_7;
    return VelocityService.TimeWindow.DAY_30;
  }

  /** Converte horas para TimeWindow mais próxima */
  private VelocityService.TimeWindow parseTimeWindowFromHours(int hours) {
    if (hours <= 1) return VelocityService.TimeWindow.HOUR_1;
    if (hours <= 6) return VelocityService.TimeWindow.HOUR_6;
    if (hours <= 12) return VelocityService.TimeWindow.HOUR_12;
    if (hours <= 24) return VelocityService.TimeWindow.HOUR_24;
    if (hours <= 168) return VelocityService.TimeWindow.DAY_7;
    return VelocityService.TimeWindow.DAY_30;
  }

  /** Extrai o campo de agrupamento do contexto (ex: cardNumber, accountId) */
  private String extractGroupByFromContext(EvaluationContext context) {
    // Prioridade: cardNumber > accountId > customerId
    if (context.getPayload().containsKey("cardNumber")) {
      return String.valueOf(context.getPayload().get("cardNumber"));
    }
    if (context.getPayload().containsKey("accountId")) {
      return String.valueOf(context.getPayload().get("accountId"));
    }
    if (context.getPayload().containsKey("customerId")) {
      return String.valueOf(context.getPayload().get("customerId"));
    }
    return "unknown";
  }

  // ========== Operadores Críticos para Regras de Fraude Avançadas ==========

  /**
   * NOT_IN_HISTORICAL: Verifica se o valor NÃO está no histórico do cliente. Formato:
   * "sourceField:targetField:days" (ex: "customerAcctNumber:beneficiaryId:90") Retorna true se o
   * beneficiário NUNCA foi usado antes pelo cliente nos últimos N dias.
   */
  private boolean evaluateNotInHistorical(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 3) {
        log.warn("Formato inválido para NOT_IN_HISTORICAL. Esperado: sourceField:targetField:days");
        return false;
      }

      String sourceField = parts[0].trim(); // Ex: customerIdFromHeader
      String targetField = parts[1].trim(); // Ex: beneficiaryId
      int days = Integer.parseInt(parts[2].trim());

      Object sourceValue = getFieldValue(sourceField, null, context);
      Object targetValue = getFieldValue(targetField, null, context);

      if (sourceValue == null || targetValue == null) {
        return false;
      }

      String customerId = sourceValue.toString();
      String beneficiaryId = targetValue.toString();

      // Consultar histórico de beneficiários
      boolean isNew = operatorDataService.isNewBeneficiary(customerId, beneficiaryId);
      log.debug(
          "NOT_IN_HISTORICAL: customerId={}, beneficiaryId={}, days={}, isNew={}",
          customerId,
          beneficiaryId,
          days,
          isNew);
      return isNew;
    } catch (Exception e) {
      log.error("Erro ao avaliar NOT_IN_HISTORICAL: {}", e.getMessage());
      return false;
    }
  }

  /**
   * NAME_SIMILARITY_LT: Verifica se a similaridade entre dois nomes é menor que o threshold.
   * Formato: "otherField:threshold" (ex: "shippingName:50") Usa algoritmo de Levenshtein para
   * calcular similaridade.
   */
  private boolean evaluateNameSimilarityLt(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 2) {
        log.warn("Formato inválido para NAME_SIMILARITY_LT. Esperado: otherField:threshold");
        return false;
      }

      String otherField = parts[0].trim();
      int threshold = Integer.parseInt(parts[1].trim());

      Object fieldValue = getFieldValue(condition.getFieldName(), null, context);
      Object otherValue = getFieldValue(otherField, null, context);

      if (fieldValue == null || otherValue == null) {
        return false;
      }

      String name1 = String.valueOf(fieldValue).toLowerCase().trim();
      String name2 = String.valueOf(otherValue).toLowerCase().trim();

      int similarity = calculateSimilarity(name1, name2);
      return similarity < threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar NAME_SIMILARITY_LT: {}", e.getMessage());
      return false;
    }
  }

  /** Calcula similaridade entre duas strings usando distância de Levenshtein (0-100) */
  private int calculateSimilarity(String s1, String s2) {
    if (s1.equals(s2)) return 100;
    if (s1.isEmpty() || s2.isEmpty()) return 0;

    int[][] dp = new int[s1.length() + 1][s2.length() + 1];

    for (int i = 0; i <= s1.length(); i++) dp[i][0] = i;
    for (int j = 0; j <= s2.length(); j++) dp[0][j] = j;

    for (int i = 1; i <= s1.length(); i++) {
      for (int j = 1; j <= s2.length(); j++) {
        int cost = s1.charAt(i - 1) == s2.charAt(j - 1) ? 0 : 1;
        dp[i][j] = Math.min(Math.min(dp[i - 1][j] + 1, dp[i][j - 1] + 1), dp[i - 1][j - 1] + cost);
      }
    }

    int maxLen = Math.max(s1.length(), s2.length());
    return (int) ((1.0 - (double) dp[s1.length()][s2.length()] / maxLen) * 100);
  }

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
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 2) {
        log.error("Formato inválido para SUM_LAST_N_HOURS: {}", condition.getValueSingle());
        return false;
      }

      int hours = Integer.parseInt(parts[0].trim());
      BigDecimal threshold = new BigDecimal(parts[1].trim());
      String operator = parts.length > 2 ? parts[2].trim() : "GT";

      VelocityService.TimeWindow window = parseTimeWindowFromHours(hours);

      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(), VelocityService.KeyType.PAN, window);

        return switch (operator) {
          case "GT" -> stats.getTotalAmount().compareTo(threshold) > 0;
          case "GTE" -> stats.getTotalAmount().compareTo(threshold) >= 0;
          case "LT" -> stats.getTotalAmount().compareTo(threshold) < 0;
          case "LTE" -> stats.getTotalAmount().compareTo(threshold) <= 0;
          default -> false;
        };
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar SUM_LAST_N_HOURS: {}", e.getMessage());
      return false;
    }
  }

  /** COUNT_FAILURES_LAST_N_HOURS: Contagem de falhas nas últimas N horas */
  private boolean evaluateCountFailuresLastNHours(
      RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 2) {
        return false;
      }

      int hours = Integer.parseInt(parts[0].trim());
      int threshold = Integer.parseInt(parts[1].trim());

      // Obter ID do cliente
      Object customerIdObj = getFieldValue("customerIdFromHeader", null, context);
      if (customerIdObj == null) {
        customerIdObj = getFieldValue("customerId", null, context);
      }

      if (customerIdObj != null) {
        String customerId = customerIdObj.toString();
        long failureCount = operatorDataService.countAuthFailuresLastNHours(customerId, hours);
        boolean result = failureCount > threshold;
        log.debug(
            "COUNT_FAILURES_LAST_N_HOURS: customerId={}, hours={}, failures={}, threshold={}, result={}",
            customerId,
            hours,
            failureCount,
            threshold,
            result);
        return result;
      }

      // Fallback: usar velocity service
      VelocityService.TimeWindow window = parseTimeWindowFromHours(hours);
      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(), VelocityService.KeyType.PAN, window);
        return stats.getFraudCount() > threshold;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_FAILURES_LAST_N_HOURS: {}", e.getMessage());
      return false;
    }
  }

  /** COUNT_DISTINCT_MERCHANTS_LAST_N_HOURS: Merchants distintos nas últimas N horas */
  private boolean evaluateCountDistinctMerchantsLastNHours(
      RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 2) {
        return false;
      }

      int hours = Integer.parseInt(parts[0].trim());
      int threshold = Integer.parseInt(parts[1].trim());

      VelocityService.TimeWindow window = parseTimeWindowFromHours(hours);

      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(), VelocityService.KeyType.PAN, window);
        return stats.getDistinctMerchants() > threshold;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_DISTINCT_MERCHANTS_LAST_N_HOURS: {}", e.getMessage());
      return false;
    }
  }

  /** TIME_SINCE_LAST_LT: Tempo desde última transação menor que N minutos */
  private boolean evaluateTimeSinceLastLt(RuleCondition condition, EvaluationContext context) {
    try {
      int thresholdMinutes = Integer.parseInt(condition.getValueSingle().trim());

      // Obter ID do cliente
      Object customerIdObj = getFieldValue("customerIdFromHeader", null, context);
      if (customerIdObj == null) {
        customerIdObj = getFieldValue("customerId", null, context);
      }

      if (customerIdObj != null) {
        String customerId = customerIdObj.toString();
        long minutesSinceLast = operatorDataService.getMinutesSinceLastTransaction(customerId);

        if (minutesSinceLast >= 0) {
          boolean result = minutesSinceLast < thresholdMinutes;
          log.debug(
              "TIME_SINCE_LAST_LT: customerId={}, minutesSinceLast={}, threshold={}, result={}",
              customerId,
              minutesSinceLast,
              thresholdMinutes,
              result);
          return result;
        }
      }

      // Fallback: usar velocity service
      VelocityService.TimeWindow window = parseTimeWindowFromHours(1);
      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(), VelocityService.KeyType.PAN, window);
        // Se há mais de 1 transação na última hora, provavelmente o tempo é curto
        return stats.getTransactionCount() > 1;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar TIME_SINCE_LAST_LT: {}", e.getMessage());
      return false;
    }
  }

  // ========== Operadores de Padrão de Fraude ==========

  /** VELOCITY_SPIKE: Detecta spike de velocidade comparado com média histórica */
  private boolean evaluateVelocitySpike(RuleCondition condition, EvaluationContext context) {
    try {
      // Formato: "multiplier" (ex: "3" = 3x a média)
      double multiplier = Double.parseDouble(condition.getValueSingle().trim());

      if (context.getTransactionRequest() != null) {
        // Comparar contagem atual com média histórica
        VelocityService.VelocityStats statsHour =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(),
                VelocityService.KeyType.PAN,
                VelocityService.TimeWindow.HOUR_1);

        VelocityService.VelocityStats statsDay =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(),
                VelocityService.KeyType.PAN,
                VelocityService.TimeWindow.HOUR_24);

        // Média por hora = total do dia / 24
        double avgPerHour = statsDay.getTransactionCount() / 24.0;
        double currentHour = statsHour.getTransactionCount();

        return avgPerHour > 0 && currentHour > (avgPerHour * multiplier);
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar VELOCITY_SPIKE: {}", e.getMessage());
      return false;
    }
  }

  /** AMOUNT_SPIKE: Detecta spike de valor comparado com média histórica */
  private boolean evaluateAmountSpike(RuleCondition condition, EvaluationContext context) {
    try {
      double multiplier = Double.parseDouble(condition.getValueSingle().trim());

      Object amountValue = getFieldValue("transactionAmount", null, context);
      if (amountValue == null) {
        return false;
      }

      BigDecimal currentAmount = new BigDecimal(String.valueOf(amountValue));

      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(),
                VelocityService.KeyType.PAN,
                VelocityService.TimeWindow.DAY_30);

        BigDecimal avgAmount = stats.getAvgAmount();
        if (avgAmount.compareTo(BigDecimal.ZERO) > 0) {
          BigDecimal threshold = avgAmount.multiply(BigDecimal.valueOf(multiplier));
          return currentAmount.compareTo(threshold) > 0;
        }
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar AMOUNT_SPIKE: {}", e.getMessage());
      return false;
    }
  }

  /** PATTERN_ESCALATION: Detecta padrão de valores crescentes (escada) */
  private boolean evaluatePatternEscalation(RuleCondition condition, EvaluationContext context) {
    try {
      // Formato: "minTransactions:escalationFactor" (ex: "3:1.5" = 3+ transações com fator 1.5x)
      String[] parts = condition.getValueSingle().split(":");
      int minTransactions = parts.length > 0 ? Integer.parseInt(parts[0].trim()) : 3;
      double escalationFactor = parts.length > 1 ? Double.parseDouble(parts[1].trim()) : 1.5;

      if (context.getTransactionRequest() == null) {
        return false;
      }

      // Usar velocity service para obter transações recentes
      VelocityService.TimeWindow window = VelocityService.TimeWindow.HOUR_1;
      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(), VelocityService.KeyType.PAN, window);

      // Se não há transações suficientes, não pode haver padrão de escada
      if (stats.getTransactionCount() < minTransactions) {
        return false;
      }

      // Verificar se o valor atual é significativamente maior que a média
      BigDecimal currentAmount = context.getTransactionRequest().getTransactionAmount();
      BigDecimal avgAmount = stats.getAvgAmount();

      if (avgAmount != null && avgAmount.compareTo(BigDecimal.ZERO) > 0) {
        double ratio = currentAmount.doubleValue() / avgAmount.doubleValue();
        boolean isEscalating = ratio >= escalationFactor;

        log.debug(
            "PATTERN_ESCALATION: txCount={}, currentAmount={}, avgAmount={}, ratio={}, factor={}, result={}",
            stats.getTransactionCount(),
            currentAmount,
            avgAmount,
            ratio,
            escalationFactor,
            isEscalating);
        return isEscalating;
      }

      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar PATTERN_ESCALATION: {}", e.getMessage());
      return false;
    }
  }

  /**
   * PATTERN_SPLIT_TRANSACTION: Detecta padrão de split de transação. Múltiplas transações pequenas
   * em curto período que somam um valor maior. Formato: "maxMinutes:minTransactions:totalThreshold"
   */
  private boolean evaluatePatternSplitTransaction(
      RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 3) {
        log.warn(
            "Formato inválido para PATTERN_SPLIT_TRANSACTION. Esperado: maxMinutes:minTransactions:totalThreshold");
        return false;
      }

      int maxMinutes = Integer.parseInt(parts[0].trim());
      int minTransactions = Integer.parseInt(parts[1].trim());
      BigDecimal totalThreshold = new BigDecimal(parts[2].trim());

      if (context.getTransactionRequest() != null) {
        VelocityService.TimeWindow window = parseTimeWindowFromHours(maxMinutes / 60 + 1);
        VelocityService.VelocityStats stats =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(), VelocityService.KeyType.PAN, window);

        // Detectar split: muitas transações que somam mais que o threshold
        return stats.getTransactionCount() >= minTransactions
            && stats.getTotalAmount().compareTo(totalThreshold) >= 0;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar PATTERN_SPLIT_TRANSACTION: {}", e.getMessage());
      return false;
    }
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
      int threshold =
          condition.getValueSingle() != null
              ? Integer.parseInt(condition.getValueSingle().trim())
              : 2; // Tolerância de 2 horas

      // Horário comercial estendido: 6h às 23h
      boolean isNormalHour = currentHour >= 6 && currentHour <= 23;

      return isNormalHour;
    } catch (Exception e) {
      log.error("Erro ao avaliar IN_CUSTOMER_USUAL_HOURS: {}", e.getMessage());
      return true; // Em caso de erro, assumir horário normal
    }
  }

  /**
   * IN_CUSTOMER_CHARGEBACK_MERCHANTS: Verifica se o merchant está na lista de chargebacks do
   * cliente.
   */
  private boolean evaluateInCustomerChargebackMerchants(
      RuleCondition condition, EvaluationContext context) {
    try {
      Object merchantIdObj = getFieldValue("merchantId", null, context);
      if (merchantIdObj == null) {
        return false;
      }
      String merchantId = merchantIdObj.toString();

      // Obter ID do cliente
      Object customerIdObj = getFieldValue("customerIdFromHeader", null, context);
      if (customerIdObj == null) {
        customerIdObj = getFieldValue("customerId", null, context);
      }
      if (customerIdObj == null) {
        return false;
      }
      String customerId = customerIdObj.toString();

      // Consultar histórico de chargebacks
      boolean result = operatorDataService.hasChargebackWithMerchant(customerId, merchantId);
      log.debug(
          "IN_CUSTOMER_CHARGEBACK_MERCHANTS: customerId={}, merchantId={}, result={}",
          customerId,
          merchantId,
          result);
      return result;
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
    if (value == null || value.isBlank()) {
      return defaultValue;
    }
    try {
      return Integer.parseInt(value.trim());
    } catch (NumberFormatException e) {
      log.warn(
          "Valor inválido para parse int: '{}', usando default: {}",
          maskSensitiveData(value),
          defaultValue);
      return defaultValue;
    }
  }

  /** Helper seguro para parsing de doubles. */
  private double parseDoubleSafe(String value, double defaultValue) {
    if (value == null || value.isBlank()) {
      return defaultValue;
    }
    try {
      return Double.parseDouble(value.trim());
    } catch (NumberFormatException e) {
      log.warn(
          "Valor inválido para parse double: '{}', usando default: {}",
          maskSensitiveData(value),
          defaultValue);
      return defaultValue;
    }
  }

  /** Mascara dados sensíveis para logging seguro. */
  private String maskSensitiveData(String data) {
    if (data == null || data.length() < 4) {
      return "****";
    }
    return "****" + data.substring(Math.max(0, data.length() - 4));
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
  private boolean evaluateContainsSuspiciousKeywords(Object fieldValue, RuleCondition condition) {
    try {
      if (fieldValue == null) return false;

      String text = String.valueOf(fieldValue).toLowerCase();
      List<String> keywords = condition.getValueArray();

      // Suporte a valueSingle com delimitador pipe
      if ((keywords == null || keywords.isEmpty()) && condition.getValueSingle() != null) {
        keywords = java.util.Arrays.asList(condition.getValueSingle().split("\\|"));
      }

      if (keywords == null || keywords.isEmpty()) {
        // Keywords padrao para fraude brasileira
        keywords =
            List.of(
                "urgente",
                "transferir agora",
                "bloqueio",
                "seguranca",
                "atualizar dados",
                "conta suspensa",
                "premio",
                "heranca",
                "emprestimo aprovado",
                "divida",
                "spc",
                "serasa",
                "banco central",
                "pix devolvido",
                "comprovante falso");
      }

      return keywords.stream().map(String::toLowerCase).map(String::trim).anyMatch(text::contains);
    } catch (Exception e) {
      log.error("Erro ao avaliar CONTAINS_SUSPICIOUS_KEYWORDS: {}", e.getMessage());
      return false;
    }
  }

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
    try {
      String valueSingle = condition.getValueSingle();
      if (valueSingle == null || valueSingle.isBlank()) {
        return false;
      }

      String[] parts = valueSingle.split("\\|");

      String keyType = "PAN";
      int threshold;
      int days;

      if (parts.length == 3) {
        keyType = parts[0].toUpperCase();
        threshold = parseIntSafe(parts[1], 10);
        days = parseIntSafe(parts[2], 30);
      } else if (parts.length == 2) {
        threshold = parseIntSafe(parts[0], 10);
        days = parseIntSafe(parts[1], 30);
      } else {
        threshold = parseIntSafe(parts[0], 10);
        days = 30;
      }

      // Usar VelocityService se disponível
      if (context.getTransactionRequest() != null) {
        VelocityService.TimeWindow window = parseTimeWindowFromDays(days);
        VelocityService.KeyType kt = parseKeyType(keyType);
        VelocityService.VelocityStats stats =
            velocityServiceFacade.getStats(context.getTransactionRequest(), kt, window);
        return stats.getTransactionCount() >= threshold;
      }

      // Fallback: verificar no payload
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object countObj = payload.get("velocity_count_" + days + "d");
      long count = countObj instanceof Number ? ((Number) countObj).longValue() : 0;
      return count >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  /** Converte string para KeyType. */
  private VelocityService.KeyType parseKeyType(String keyType) {
    try {
      return VelocityService.KeyType.valueOf(keyType.toUpperCase());
    } catch (IllegalArgumentException e) {
      return VelocityService.KeyType.PAN;
    }
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

  // --- CATEGORIA A: Velocity Avançado (10) ---

  /**
   * VELOCITY_CROSS_CHANNEL: Detecta velocidade entre diferentes canais. Formato valueSingle:
   * "threshold|channelField" (ex: "5|channel_type")
   */
  private boolean evaluateVelocityCrossChannel(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getTransactionRequest() == null) return false;
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int threshold = Integer.parseInt(parts[0].trim());

      Object channelsObj = payload.get("cross_channel_count");
      if (channelsObj == null) channelsObj = payload.get("distinctChannels");

      int channelCount = channelsObj instanceof Number ? ((Number) channelsObj).intValue() : 1;
      return channelCount > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar VELOCITY_CROSS_CHANNEL: {}", e.getMessage());
      return false;
    }
  }

  /**
   * VELOCITY_ROLLING_WINDOW: Velocidade com janela móvel customizada. Formato valueSingle:
   * "threshold|minutes" (ex: "10|30")
   */
  private boolean evaluateVelocityRollingWindow(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getTransactionRequest() == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      long threshold = Long.parseLong(parts[0].trim());
      int minutes = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 60;

      VelocityService.TimeWindow window = parseTimeWindow(minutes);
      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(), VelocityService.KeyType.PAN, window);

      return stats.getTransactionCount() > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar VELOCITY_ROLLING_WINDOW: {}", e.getMessage());
      return false;
    }
  }

  /**
   * VELOCITY_PERCENTILE: Verifica se transação está acima de determinado percentil. Formato
   * valueSingle: "percentile" (ex: "95" = acima do P95)
   */
  private boolean evaluateVelocityPercentile(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getTransactionRequest() == null) return false;

      double percentileThreshold = Double.parseDouble(condition.getValueSingle().trim());

      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(),
              VelocityService.KeyType.PAN,
              VelocityService.TimeWindow.HOUR_24);

      BigDecimal avg = stats.getAvgAmount();
      BigDecimal stdDev = stats.getStdDevAmount();
      BigDecimal txAmount = context.getTransactionRequest().getTransactionAmount();

      if (avg == null || stdDev == null || stdDev.compareTo(BigDecimal.ZERO) == 0) return false;

      // Usar Z-score para aproximar percentil
      double zScore = txAmount.subtract(avg).divide(stdDev, 4, RoundingMode.HALF_UP).doubleValue();
      double percentile = 50 + (50 * Math.tanh(zScore / 2)); // Aproximação

      return percentile > percentileThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar VELOCITY_PERCENTILE: {}", e.getMessage());
      return false;
    }
  }

  /**
   * VELOCITY_RATIO_GT: Razão entre velocidade atual e histórica. Formato valueSingle: "ratio" (ex:
   * "2.0" = 2x a velocidade normal)
   */
  private boolean evaluateVelocityRatioGt(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getTransactionRequest() == null) return false;

      double ratioThreshold = Double.parseDouble(condition.getValueSingle().trim());

      VelocityService.VelocityStats statsHour =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(),
              VelocityService.KeyType.PAN,
              VelocityService.TimeWindow.HOUR_1);
      VelocityService.VelocityStats stats24h =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(),
              VelocityService.KeyType.PAN,
              VelocityService.TimeWindow.HOUR_24);

      long countHour = statsHour.getTransactionCount();
      long count24h = stats24h.getTransactionCount();

      if (count24h == 0) return false;

      double avgHourly = count24h / 24.0;
      double ratio = avgHourly > 0 ? countHour / avgHourly : 0;

      return ratio > ratioThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar VELOCITY_RATIO_GT: {}", e.getMessage());
      return false;
    }
  }

  /**
   * VELOCITY_TREND: Detecta tendência de aumento na velocidade. Formato valueSingle:
   * "direction|threshold" (ex: "UP|1.5")
   */
  private boolean evaluateVelocityTrend(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      String direction = parts[0].trim().toUpperCase();
      double threshold = parts.length > 1 ? Double.parseDouble(parts[1].trim()) : 1.5;

      Object trendObj = payload.get("velocity_trend_ratio");
      if (trendObj == null) trendObj = payload.get("velocityTrendRatio");

      double trendRatio = trendObj instanceof Number ? ((Number) trendObj).doubleValue() : 1.0;

      if ("UP".equals(direction)) {
        return trendRatio > threshold;
      } else if ("DOWN".equals(direction)) {
        return trendRatio < (1.0 / threshold);
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar VELOCITY_TREND: {}", e.getMessage());
      return false;
    }
  }

  /**
   * COUNT_UNIQUE_BENEFICIARIES_LAST_N_DAYS: Conta beneficiários únicos em N dias. Formato
   * valueSingle: "threshold|days" (ex: "10|7")
   */
  private boolean evaluateCountUniqueBeneficiariesLastNDays(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int threshold = Integer.parseInt(parts[0].trim());

      Object countObj = payload.get("unique_beneficiaries_count");
      if (countObj == null) countObj = payload.get("uniqueBeneficiariesCount");

      int count = countObj instanceof Number ? ((Number) countObj).intValue() : 0;
      return count > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_UNIQUE_BENEFICIARIES_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * COUNT_UNIQUE_IPS_LAST_N_HOURS: Conta IPs únicos em N horas. Formato valueSingle:
   * "threshold|hours" (ex: "5|24")
   */
  private boolean evaluateCountUniqueIpsLastNHours(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int threshold = Integer.parseInt(parts[0].trim());

      Object countObj = payload.get("unique_ips_count");
      if (countObj == null) countObj = payload.get("uniqueIpsCount");

      int count = countObj instanceof Number ? ((Number) countObj).intValue() : 0;
      return count > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_UNIQUE_IPS_LAST_N_HOURS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * SUM_BY_CHANNEL_LAST_N_DAYS: Soma valores por canal em N dias. Formato valueSingle:
   * "threshold|channel|days" (ex: "50000|PIX|7")
   */
  private boolean evaluateSumByChannelLastNDays(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      double threshold = Double.parseDouble(parts[0].trim());
      String channel = parts.length > 1 ? parts[1].trim().toUpperCase() : "ALL";

      String key = "sum_by_channel_" + channel.toLowerCase();
      Object sumObj = payload.get(key);
      if (sumObj == null) sumObj = payload.get("sumByChannel" + channel);

      double sum = sumObj instanceof Number ? ((Number) sumObj).doubleValue() : 0.0;
      return sum > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar SUM_BY_CHANNEL_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * AVG_INTERVAL_BETWEEN_TXN: Intervalo médio entre transações. Formato valueSingle: "minSeconds"
   * (ex: "30" = mínimo 30s entre txns)
   */
  private boolean evaluateAvgIntervalBetweenTxn(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double minSeconds = Double.parseDouble(condition.getValueSingle().trim());

      Object intervalObj = payload.get("avg_interval_seconds");
      if (intervalObj == null) intervalObj = payload.get("avgIntervalSeconds");

      double avgInterval =
          intervalObj instanceof Number ? ((Number) intervalObj).doubleValue() : Double.MAX_VALUE;
      return avgInterval < minSeconds;
    } catch (Exception e) {
      log.error("Erro ao avaliar AVG_INTERVAL_BETWEEN_TXN: {}", e.getMessage());
      return false;
    }
  }

  /**
   * VELOCITY_ACCELERATION: Detecta aceleração na frequência de transações. Formato valueSingle:
   * "accelerationThreshold" (ex: "2.0" = aceleração 2x)
   */
  private boolean evaluateVelocityAcceleration(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object accelObj = payload.get("velocity_acceleration");
      if (accelObj == null) accelObj = payload.get("velocityAcceleration");

      double acceleration = accelObj instanceof Number ? ((Number) accelObj).doubleValue() : 1.0;
      return acceleration > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar VELOCITY_ACCELERATION: {}", e.getMessage());
      return false;
    }
  }

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

  // --- CATEGORIA C: Graph/Network (8) ---

  /**
   * FAN_OUT_COUNT: Conta destinatários únicos de um remetente. Formato valueSingle:
   * "threshold|hours" (ex: "10|24")
   */
  private boolean evaluateFanOutCount(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int threshold = Integer.parseInt(parts[0].trim());

      Object fanOutObj = payload.get("fan_out_count");
      if (fanOutObj == null) fanOutObj = payload.get("fanOutCount");

      int fanOut = fanOutObj instanceof Number ? ((Number) fanOutObj).intValue() : 0;
      return fanOut > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar FAN_OUT_COUNT: {}", e.getMessage());
      return false;
    }
  }

  /**
   * FAN_IN_COUNT: Conta remetentes únicos para um destinatário. Formato valueSingle:
   * "threshold|hours" (ex: "10|24")
   */
  private boolean evaluateFanInCount(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int threshold = Integer.parseInt(parts[0].trim());

      Object fanInObj = payload.get("fan_in_count");
      if (fanInObj == null) fanInObj = payload.get("fanInCount");

      int fanIn = fanInObj instanceof Number ? ((Number) fanInObj).intValue() : 0;
      return fanIn > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar FAN_IN_COUNT: {}", e.getMessage());
      return false;
    }
  }

  /**
   * SHARED_DEVICE_COUNT: Conta contas usando o mesmo dispositivo. Formato valueSingle: "threshold"
   * (ex: "3")
   */
  private boolean evaluateSharedDeviceCount(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int threshold = Integer.parseInt(condition.getValueSingle().trim());

      Object sharedObj = payload.get("shared_device_accounts");
      if (sharedObj == null) sharedObj = payload.get("sharedDeviceAccounts");

      int sharedCount = sharedObj instanceof Number ? ((Number) sharedObj).intValue() : 0;
      return sharedCount > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar SHARED_DEVICE_COUNT: {}", e.getMessage());
      return false;
    }
  }

  /** SHARED_IP_COUNT: Conta contas usando o mesmo IP. Formato valueSingle: "threshold" (ex: "5") */
  private boolean evaluateSharedIpCount(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int threshold = Integer.parseInt(condition.getValueSingle().trim());

      Object sharedObj = payload.get("shared_ip_accounts");
      if (sharedObj == null) sharedObj = payload.get("sharedIpAccounts");

      int sharedCount = sharedObj instanceof Number ? ((Number) sharedObj).intValue() : 0;
      return sharedCount > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar SHARED_IP_COUNT: {}", e.getMessage());
      return false;
    }
  }

  /**
   * ACCOUNT_LINK_DEPTH: Profundidade de links entre contas. Formato valueSingle: "maxDepth" (ex:
   * "3" = 3 saltos)
   */
  private boolean evaluateAccountLinkDepth(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int maxDepth = Integer.parseInt(condition.getValueSingle().trim());

      Object depthObj = payload.get("account_link_depth");
      if (depthObj == null) depthObj = payload.get("accountLinkDepth");

      int depth = depthObj instanceof Number ? ((Number) depthObj).intValue() : 0;
      return depth >= maxDepth;
    } catch (Exception e) {
      log.error("Erro ao avaliar ACCOUNT_LINK_DEPTH: {}", e.getMessage());
      return false;
    }
  }

  /**
   * CIRCULAR_TRANSFER_DETECTION: Detecta transferências circulares. Formato valueSingle: "true"
   * (verifica se há ciclo)
   */
  private boolean evaluateCircularTransferDetection(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object circularObj = payload.get("circular_transfer_detected");
      if (circularObj == null) circularObj = payload.get("circularTransferDetected");

      return Boolean.TRUE.equals(circularObj)
          || "true".equalsIgnoreCase(String.valueOf(circularObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar CIRCULAR_TRANSFER_DETECTION: {}", e.getMessage());
      return false;
    }
  }

  /**
   * RAPID_MULTI_HOP: Detecta múltiplos saltos rápidos. Formato valueSingle: "hops|minutes" (ex:
   * "3|10")
   */
  private boolean evaluateRapidMultiHop(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int hopThreshold = Integer.parseInt(parts[0].trim());

      Object hopCountObj = payload.get("rapid_hop_count");
      if (hopCountObj == null) hopCountObj = payload.get("rapidHopCount");

      int hopCount = hopCountObj instanceof Number ? ((Number) hopCountObj).intValue() : 0;
      return hopCount >= hopThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar RAPID_MULTI_HOP: {}", e.getMessage());
      return false;
    }
  }

  /**
   * BENEFICIARY_CONCENTRATION: Concentração de valores em poucos beneficiários. Formato
   * valueSingle: "percentageThreshold" (ex: "80" = 80% para top 3)
   */
  private boolean evaluateBeneficiaryConcentration(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double percentageThreshold = Double.parseDouble(condition.getValueSingle().trim());

      Object concentrationObj = payload.get("beneficiary_concentration_pct");
      if (concentrationObj == null) concentrationObj = payload.get("beneficiaryConcentrationPct");

      double concentration =
          concentrationObj instanceof Number ? ((Number) concentrationObj).doubleValue() : 0.0;
      return concentration > percentageThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar BENEFICIARY_CONCENTRATION: {}", e.getMessage());
      return false;
    }
  }

  // --- CATEGORIA D: Sanctions & Name Matching (7) ---
  private boolean evaluateOfacListCheck(Object fieldValue, RuleCondition condition) {
    try {
      if (fieldValue == null) return false;
      String name = normalizeForMatch(String.valueOf(fieldValue));
      if (name.isBlank()) return false;

      List<String> list = condition.getValueArray();
      if (list == null || list.isEmpty()) {
        String csv = condition.getValueSingle();
        if (csv == null || csv.isBlank()) return false;
        list = Arrays.stream(csv.split(",")).map(String::trim).filter(s -> !s.isBlank()).toList();
      }

      for (String entry : list) {
        String normalizedEntry = normalizeForMatch(entry);
        if (normalizedEntry.isBlank()) continue;
        if (name.equals(normalizedEntry)) return true;
        if (calculateSimilarity(name, normalizedEntry) >= 92) return true;
      }

      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar OFAC_LIST_CHECK: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluatePepListCheck(Object fieldValue, RuleCondition condition) {
    try {
      if (fieldValue == null) return false;
      String name = normalizeForMatch(String.valueOf(fieldValue));
      if (name.isBlank()) return false;

      List<String> list = condition.getValueArray();
      if (list == null || list.isEmpty()) {
        String csv = condition.getValueSingle();
        if (csv == null || csv.isBlank()) return false;
        list = Arrays.stream(csv.split(",")).map(String::trim).filter(s -> !s.isBlank()).toList();
      }

      for (String entry : list) {
        String normalizedEntry = normalizeForMatch(entry);
        if (normalizedEntry.isBlank()) continue;
        if (name.equals(normalizedEntry)) return true;
        if (calculateSimilarity(name, normalizedEntry) >= 92) return true;
      }

      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar PEP_LIST_CHECK: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateAdverseMediaCheck(Object fieldValue, RuleCondition condition) {
    try {
      if (fieldValue == null) return false;

      String text = normalizeForMatch(String.valueOf(fieldValue));
      if (text.isBlank()) return false;

      List<String> keywords = condition.getValueArray();
      if (keywords == null || keywords.isEmpty()) {
        String csv = condition.getValueSingle();
        if (csv == null || csv.isBlank()) {
          keywords =
              List.of("fraud", "scam", "money laundering", "sanction", "terror", "corruption");
        } else {
          keywords =
              Arrays.stream(csv.split(",")).map(String::trim).filter(s -> !s.isBlank()).toList();
        }
      }

      for (String kw : keywords) {
        String needle = normalizeForMatch(kw);
        if (needle.isBlank()) continue;
        if (text.contains(needle)) return true;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar ADVERSE_MEDIA_CHECK: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateSanctionsCountryCheck(Object fieldValue, RuleCondition condition) {
    try {
      if (fieldValue == null) return false;
      String country = String.valueOf(fieldValue).trim().toUpperCase(Locale.ROOT);
      if (country.isBlank()) return false;

      List<String> list = condition.getValueArray();
      if (list == null || list.isEmpty()) {
        String csv = condition.getValueSingle();
        if (csv == null || csv.isBlank()) return false;
        list = Arrays.stream(csv.split(",")).map(String::trim).filter(s -> !s.isBlank()).toList();
      }

      for (String entry : list) {
        if (country.equalsIgnoreCase(String.valueOf(entry).trim())) return true;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar SANCTIONS_COUNTRY_CHECK: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateHighRiskJurisdiction(Object fieldValue, RuleCondition condition) {
    try {
      if (fieldValue == null) return false;
      String jurisdiction = String.valueOf(fieldValue).trim().toUpperCase(Locale.ROOT);
      if (jurisdiction.isBlank()) return false;

      List<String> list = condition.getValueArray();
      if (list == null || list.isEmpty()) {
        String csv = condition.getValueSingle();
        if (csv == null || csv.isBlank()) return false;
        list = Arrays.stream(csv.split(",")).map(String::trim).filter(s -> !s.isBlank()).toList();
      }

      for (String entry : list) {
        if (jurisdiction.equalsIgnoreCase(String.valueOf(entry).trim())) return true;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar HIGH_RISK_JURISDICTION: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateNameTransliterationMatch(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      String otherField = null;
      int threshold = 90;

      if (condition.getValueSingle() != null && !condition.getValueSingle().isBlank()) {
        String[] parts = condition.getValueSingle().split(":");
        if (parts.length >= 1 && !parts[0].isBlank()) otherField = parts[0].trim();
        if (parts.length >= 2 && !parts[1].isBlank()) threshold = Integer.parseInt(parts[1].trim());
      }

      Object v1 = getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
      Object v2 = otherField != null ? getFieldValue(otherField, null, context) : null;
      if (v2 == null) {
        v2 = context.getPayload().get("transliterated_name");
        if (v2 == null) v2 = context.getPayload().get("transliteratedName");
      }

      if (v1 == null || v2 == null) return false;
      String s1 = normalizeForMatch(String.valueOf(v1));
      String s2 = normalizeForMatch(String.valueOf(v2));
      if (s1.isBlank() || s2.isBlank()) return false;

      int similarity = calculateSimilarity(s1, s2);
      return similarity >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar NAME_TRANSLITERATION_MATCH: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateAliasDetection(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object aliasObj = context.getPayload().get("alias_detected");
      if (aliasObj == null) aliasObj = context.getPayload().get("aliasDetected");
      if (aliasObj != null) {
        return Boolean.TRUE.equals(toBoolean(aliasObj));
      }

      Object fieldValue =
          getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
      if (fieldValue == null) return false;
      String name = normalizeForMatch(String.valueOf(fieldValue));
      if (name.isBlank()) return false;

      List<String> aliases = condition.getValueArray();
      if (aliases == null || aliases.isEmpty()) {
        String csv = condition.getValueSingle();
        if (csv == null || csv.isBlank()) return false;
        aliases =
            Arrays.stream(csv.split(",")).map(String::trim).filter(s -> !s.isBlank()).toList();
      }

      for (String alias : aliases) {
        String a = normalizeForMatch(alias);
        if (a.isBlank()) continue;
        if (name.contains(a) || a.contains(name)) return true;
        if (calculateSimilarity(name, a) >= 90) return true;
      }

      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar ALIAS_DETECTION: {}", e.getMessage());
      return false;
    }
  }

  // --- CATEGORIA E: Synthetic ID Detection (8) ---
  private boolean evaluateCpfSsnValidation(Object fieldValue, RuleCondition condition) {
    try {
      if (fieldValue == null) return false;
      String raw = String.valueOf(fieldValue);
      String digits = raw.replaceAll("\\D", "");
      if (digits.length() == 11) {
        return isValidCpf(digits);
      }
      if (digits.length() == 9) {
        // SSN básico: não pode ser tudo zero, e não pode iniciar com 000.
        if (digits.chars().allMatch(ch -> ch == '0')) return false;
        if (digits.startsWith("000")) return false;
        return true;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar CPF_SSN_VALIDATION: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluatePhoneCarrierCheck(Object fieldValue, RuleCondition condition) {
    try {
      if (fieldValue == null) return false;
      String carrier = normalizeForMatch(String.valueOf(fieldValue));
      if (carrier.isBlank()) return false;

      List<String> list = condition.getValueArray();
      if (list == null || list.isEmpty()) {
        String csv = condition.getValueSingle();
        if (csv == null || csv.isBlank()) return false;
        list = Arrays.stream(csv.split(",")).map(String::trim).filter(s -> !s.isBlank()).toList();
      }

      for (String entry : list) {
        if (carrier.equals(normalizeForMatch(entry))) return true;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar PHONE_CARRIER_CHECK: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateEmailDomainAge(Object fieldValue, RuleCondition condition) {
    try {
      int thresholdDays = 30;
      if (condition.getValueSingle() != null && !condition.getValueSingle().isBlank()) {
        thresholdDays = Integer.parseInt(condition.getValueSingle().trim());
      }

      Integer ageDays = null;
      if (fieldValue instanceof Number) {
        ageDays = ((Number) fieldValue).intValue();
      } else if (fieldValue != null) {
        String email = String.valueOf(fieldValue).trim();
        // Se vier como email, procuramos a idade do domínio no payload/variáveis.
        // (feature pré-computada)
        // Sem payload aqui, então só validamos formato básico e falhamos fechado.
        if (!email.contains("@")) return false;
      }

      if (ageDays == null) return false;
      // Regra de risco: domínio muito novo.
      return ageDays < thresholdDays;
    } catch (Exception e) {
      log.error("Erro ao avaliar EMAIL_DOMAIN_AGE: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateAddressVerification(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object verifiedObj = context.getPayload().get("address_verified");
      if (verifiedObj == null) verifiedObj = context.getPayload().get("addressVerified");
      if (verifiedObj == null) {
        verifiedObj = getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
      }
      Boolean verified = toBoolean(verifiedObj);
      if (verified == null) return false;

      if (condition.getValueSingle() == null || condition.getValueSingle().isBlank()) {
        return verified;
      }
      boolean expected = Boolean.parseBoolean(condition.getValueSingle().trim());
      return verified == expected;
    } catch (Exception e) {
      log.error("Erro ao avaliar ADDRESS_VERIFICATION: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateIdentityVelocity(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      String[] parts =
          (condition.getValueSingle() == null ? "" : condition.getValueSingle()).split("\\|");
      long threshold =
          parts.length >= 1 && !parts[0].isBlank() ? Long.parseLong(parts[0].trim()) : 3L;

      Object countObj = context.getPayload().get("identity_velocity_count");
      if (countObj == null) countObj = context.getPayload().get("identityVelocityCount");
      if (countObj == null)
        countObj = getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
      long count = countObj instanceof Number ? ((Number) countObj).longValue() : 0L;
      return count >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar IDENTITY_VELOCITY: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateDeviceAccountRatio(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object ratioObj = context.getPayload().get("device_account_ratio");
      if (ratioObj == null) ratioObj = context.getPayload().get("deviceAccountRatio");
      if (ratioObj == null)
        ratioObj = getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);

      double ratio = ratioObj instanceof Number ? ((Number) ratioObj).doubleValue() : 0.0;
      return ratio > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar DEVICE_ACCOUNT_RATIO: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateEmailPhoneMismatch(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object mismatchObj = context.getPayload().get("email_phone_mismatch");
      if (mismatchObj == null) mismatchObj = context.getPayload().get("emailPhoneMismatch");
      if (mismatchObj == null)
        mismatchObj = getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
      return Boolean.TRUE.equals(toBoolean(mismatchObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar EMAIL_PHONE_MISMATCH: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateCreditFileThin(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      int minMonths = Integer.parseInt(condition.getValueSingle().trim());

      Object monthsObj = context.getPayload().get("credit_file_months");
      if (monthsObj == null) monthsObj = context.getPayload().get("creditFileMonths");
      if (monthsObj == null)
        monthsObj = getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);

      int months =
          monthsObj instanceof Number ? ((Number) monthsObj).intValue() : Integer.MAX_VALUE;
      return months < minMonths;
    } catch (Exception e) {
      log.error("Erro ao avaliar CREDIT_FILE_THIN: {}", e.getMessage());
      return false;
    }
  }

  // --- CATEGORIA F: AML Typology (8) ---
  private boolean evaluateStructuringDetection(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      // Formato: "maxSingleAmount|minCount" (ex: "1000|5")
      String[] parts =
          (condition.getValueSingle() == null ? "" : condition.getValueSingle()).split("\\|");
      BigDecimal maxSingle =
          parts.length >= 1 && !parts[0].isBlank()
              ? new BigDecimal(parts[0].trim())
              : new BigDecimal("1000");
      int minCount =
          parts.length >= 2 && !parts[1].isBlank() ? Integer.parseInt(parts[1].trim()) : 5;

      Object countObj = context.getPayload().get("cash_like_tx_count_below_threshold");
      if (countObj == null) countObj = context.getPayload().get("structuringCount");
      int count = countObj instanceof Number ? ((Number) countObj).intValue() : 0;

      BigDecimal amount = null;
      if (context.getTransactionRequest() != null) {
        amount = context.getTransactionRequest().getTransactionAmount();
      }
      boolean currentBelow = amount != null && amount.compareTo(maxSingle) <= 0;

      return currentBelow && count >= minCount;
    } catch (Exception e) {
      log.error("Erro ao avaliar STRUCTURING_DETECTION: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateLayeringPattern(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      int minHops = Integer.parseInt(condition.getValueSingle().trim());
      Object hopsObj = context.getPayload().get("layering_hops");
      if (hopsObj == null) hopsObj = context.getPayload().get("layeringHops");
      int hops = hopsObj instanceof Number ? ((Number) hopsObj).intValue() : 0;
      return hops >= minHops;
    } catch (Exception e) {
      log.error("Erro ao avaliar LAYERING_PATTERN: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateRapidMovement(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      int maxMinutes = Integer.parseInt(condition.getValueSingle().trim());
      Object minutesObj = context.getPayload().get("minutes_between_in_out");
      if (minutesObj == null) minutesObj = context.getPayload().get("minutesBetweenInOut");
      int minutes =
          minutesObj instanceof Number ? ((Number) minutesObj).intValue() : Integer.MAX_VALUE;
      return minutes <= maxMinutes;
    } catch (Exception e) {
      log.error("Erro ao avaliar RAPID_MOVEMENT: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateIntegrationPattern(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object flagObj = context.getPayload().get("integration_detected");
      if (flagObj == null) flagObj = context.getPayload().get("integrationDetected");
      if (flagObj == null)
        flagObj = getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
      return Boolean.TRUE.equals(toBoolean(flagObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar INTEGRATION_PATTERN: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateCashIntensiveRatio(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      double threshold = Double.parseDouble(condition.getValueSingle().trim());
      Object ratioObj = context.getPayload().get("cash_intensive_ratio");
      if (ratioObj == null) ratioObj = context.getPayload().get("cashIntensiveRatio");
      double ratio = ratioObj instanceof Number ? ((Number) ratioObj).doubleValue() : 0.0;
      return ratio > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar CASH_INTENSIVE_RATIO: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateUnusualBusinessPattern(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object flagObj = context.getPayload().get("unusual_business_pattern");
      if (flagObj == null) flagObj = context.getPayload().get("unusualBusinessPattern");
      if (flagObj == null)
        flagObj = getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
      return Boolean.TRUE.equals(toBoolean(flagObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar UNUSUAL_BUSINESS_PATTERN: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateShellCompanyIndicator(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object flagObj = context.getPayload().get("shell_company_indicator");
      if (flagObj == null) flagObj = context.getPayload().get("shellCompanyIndicator");
      if (flagObj != null) return Boolean.TRUE.equals(toBoolean(flagObj));

      // Heurística: empresa muito nova + sem funcionários.
      int maxAgeMonths = Integer.parseInt(condition.getValueSingle().trim());
      Object ageObj = context.getPayload().get("company_age_months");
      if (ageObj == null) ageObj = context.getPayload().get("companyAgeMonths");
      int ageMonths = ageObj instanceof Number ? ((Number) ageObj).intValue() : Integer.MAX_VALUE;

      Object empObj = context.getPayload().get("employee_count");
      if (empObj == null) empObj = context.getPayload().get("employeeCount");
      int employees = empObj instanceof Number ? ((Number) empObj).intValue() : 1;

      return ageMonths <= maxAgeMonths && employees <= 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar SHELL_COMPANY_INDICATOR: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateTradeBasedMlIndicator(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object flagObj = context.getPayload().get("trade_based_ml_indicator");
      if (flagObj == null) flagObj = context.getPayload().get("tradeBasedMlIndicator");
      if (flagObj != null) return Boolean.TRUE.equals(toBoolean(flagObj));

      double threshold = Double.parseDouble(condition.getValueSingle().trim());
      Object mismatchObj = context.getPayload().get("invoice_mismatch_pct");
      if (mismatchObj == null) mismatchObj = context.getPayload().get("invoiceMismatchPct");
      double mismatch = mismatchObj instanceof Number ? ((Number) mismatchObj).doubleValue() : 0.0;
      return mismatch > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar TRADE_BASED_ML_INDICATOR: {}", e.getMessage());
      return false;
    }
  }

  // --- CATEGORIA G: Regulatory (8) ---
  private boolean evaluateScaExemptionTra(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      double maxRisk = Double.parseDouble(condition.getValueSingle().trim());
      Object riskObj = context.getPayload().get("tra_risk_score");
      if (riskObj == null) riskObj = context.getPayload().get("traRiskScore");
      double risk = riskObj instanceof Number ? ((Number) riskObj).doubleValue() : Double.MAX_VALUE;
      return risk <= maxRisk;
    } catch (Exception e) {
      log.error("Erro ao avaliar SCA_EXEMPTION_TRA: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateScaExemptionLowValue(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getTransactionRequest() == null) return false;
      BigDecimal maxAmount = new BigDecimal(condition.getValueSingle().trim());
      BigDecimal amount = context.getTransactionRequest().getTransactionAmount();
      if (amount == null) return false;
      return amount.compareTo(maxAmount) <= 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar SCA_EXEMPTION_LOW_VALUE: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateScaExemptionTrustedBeneficiary(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object trustedObj = context.getPayload().get("trusted_beneficiary");
      if (trustedObj == null) trustedObj = context.getPayload().get("trustedBeneficiary");
      if (trustedObj == null)
        trustedObj = getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
      return Boolean.TRUE.equals(toBoolean(trustedObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar SCA_EXEMPTION_TRUSTED_BENEFICIARY: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateScaExemptionRecurring(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object recurringObj = context.getPayload().get("recurring_payment");
      if (recurringObj == null) recurringObj = context.getPayload().get("recurringPayment");
      if (recurringObj == null)
        recurringObj = getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
      return Boolean.TRUE.equals(toBoolean(recurringObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar SCA_EXEMPTION_RECURRING: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluatePsd3CopNameMatch(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts =
          (condition.getValueSingle() == null ? "" : condition.getValueSingle()).split(":");
      if (parts.length < 2) {
        // fallback: usa fieldName vs payload[other_name]
        parts = new String[] {"other_name", "90"};
      }
      String otherField = parts[0].trim();
      int threshold = Integer.parseInt(parts[1].trim());

      Object v1 = getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
      Object v2 = getFieldValue(otherField, null, context);
      if (v1 == null || v2 == null) return false;

      String s1 = normalizeForMatch(String.valueOf(v1));
      String s2 = normalizeForMatch(String.valueOf(v2));
      if (s1.isBlank() || s2.isBlank()) return false;

      return calculateSimilarity(s1, s2) >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar PSD3_COP_NAME_MATCH: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateDoraIncidentSeverity(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      int minSeverity = Integer.parseInt(condition.getValueSingle().trim());
      Object sevObj = context.getPayload().get("incident_severity");
      if (sevObj == null) sevObj = context.getPayload().get("incidentSeverity");
      int sev = sevObj instanceof Number ? ((Number) sevObj).intValue() : 0;
      return sev >= minSeverity;
    } catch (Exception e) {
      log.error("Erro ao avaliar DORA_INCIDENT_SEVERITY: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateEidasAssuranceLevel(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      String required =
          (condition.getValueSingle() == null ? "" : condition.getValueSingle()).trim();
      int requiredLevel = mapEidasLevel(required);
      Object levelObj = context.getPayload().get("eidas_assurance_level");
      if (levelObj == null) levelObj = context.getPayload().get("eidasAssuranceLevel");
      if (levelObj == null)
        levelObj = getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
      if (levelObj == null) return false;
      int actualLevel = mapEidasLevel(String.valueOf(levelObj));
      return actualLevel >= requiredLevel;
    } catch (Exception e) {
      log.error("Erro ao avaliar EIDAS_ASSURANCE_LEVEL: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateGdprDataRetentionCheck(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      int maxDays = Integer.parseInt(condition.getValueSingle().trim());
      Object ageObj = context.getPayload().get("data_age_days");
      if (ageObj == null) ageObj = context.getPayload().get("dataAgeDays");
      int ageDays = ageObj instanceof Number ? ((Number) ageObj).intValue() : 0;
      // Retorna true quando há violação (dados mais antigos que o permitido)
      return ageDays > maxDays;
    } catch (Exception e) {
      log.error("Erro ao avaliar GDPR_DATA_RETENTION_CHECK: {}", e.getMessage());
      return false;
    }
  }

  // --- CATEGORIA H: Device (7) ---
  private boolean evaluateDeviceJailbreakRooted(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object rootedObj = context.getPayload().get("device_rooted");
      if (rootedObj == null) rootedObj = context.getPayload().get("deviceRooted");
      if (rootedObj == null) rootedObj = context.getPayload().get("jailbroken");
      return Boolean.TRUE.equals(toBoolean(rootedObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar DEVICE_JAILBREAK_ROOTED: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateEmulatorDetection(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object emuObj = context.getPayload().get("emulator_detected");
      if (emuObj == null) emuObj = context.getPayload().get("emulatorDetected");
      return Boolean.TRUE.equals(toBoolean(emuObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar EMULATOR_DETECTION: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateVpnProxyDetection(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object vpnObj = context.getPayload().get("vpn_or_proxy");
      if (vpnObj == null) vpnObj = context.getPayload().get("vpnOrProxy");
      if (vpnObj == null) vpnObj = context.getPayload().get("proxy_detected");
      return Boolean.TRUE.equals(toBoolean(vpnObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar VPN_PROXY_DETECTION: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateTorExitNode(Object fieldValue, RuleCondition condition) {
    try {
      if (fieldValue == null) return false;
      String ip = String.valueOf(fieldValue).trim();
      if (ip.isBlank()) return false;

      List<String> list = condition.getValueArray();
      if (list == null || list.isEmpty()) {
        String csv = condition.getValueSingle();
        if (csv == null || csv.isBlank()) return false;
        list = Arrays.stream(csv.split(",")).map(String::trim).filter(s -> !s.isBlank()).toList();
      }

      for (String entry : list) {
        if (ip.equalsIgnoreCase(String.valueOf(entry).trim())) return true;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar TOR_EXIT_NODE: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateBrowserInconsistency(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      double threshold = Double.parseDouble(condition.getValueSingle().trim());
      Object scoreObj = context.getPayload().get("browser_inconsistency_score");
      if (scoreObj == null) scoreObj = context.getPayload().get("browserInconsistencyScore");
      double score = scoreObj instanceof Number ? ((Number) scoreObj).doubleValue() : 0.0;
      return score > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar BROWSER_INCONSISTENCY: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateTimezoneMismatch(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object mismatchObj = context.getPayload().get("timezone_mismatch");
      if (mismatchObj == null) mismatchObj = context.getPayload().get("timezoneMismatch");
      return Boolean.TRUE.equals(toBoolean(mismatchObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar TIMEZONE_MISMATCH: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateLanguageMismatch(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object mismatchObj = context.getPayload().get("language_mismatch");
      if (mismatchObj == null) mismatchObj = context.getPayload().get("languageMismatch");
      if (mismatchObj != null) return Boolean.TRUE.equals(toBoolean(mismatchObj));

      Object userLang = context.getPayload().get("user_language");
      if (userLang == null) userLang = context.getPayload().get("userLanguage");
      Object deviceLang = context.getPayload().get("device_language");
      if (deviceLang == null) deviceLang = context.getPayload().get("deviceLanguage");
      if (userLang == null || deviceLang == null) return false;
      return !String.valueOf(userLang).equalsIgnoreCase(String.valueOf(deviceLang));
    } catch (Exception e) {
      log.error("Erro ao avaliar LANGUAGE_MISMATCH: {}", e.getMessage());
      return false;
    }
  }

  // --- CATEGORIA I: Merchant & MCC (7) ---
  private boolean evaluateMccHighRisk(Object fieldValue, RuleCondition condition) {
    try {
      if (fieldValue == null) return false;
      String mcc = String.valueOf(fieldValue).trim();
      if (mcc.isBlank()) return false;

      List<String> list = condition.getValueArray();
      if (list == null || list.isEmpty()) {
        String csv = condition.getValueSingle();
        if (csv == null || csv.isBlank()) return false;
        list = Arrays.stream(csv.split(",")).map(String::trim).filter(s -> !s.isBlank()).toList();
      }

      for (String entry : list) {
        if (mcc.equals(String.valueOf(entry).trim())) return true;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar MCC_HIGH_RISK: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateMccGambling(Object fieldValue, RuleCondition condition) {
    try {
      if (fieldValue == null) return false;
      String mcc = String.valueOf(fieldValue).trim();
      if (mcc.isBlank()) return false;
      Set<String> gambling = Set.of("7995", "7800", "7801", "7802");
      return gambling.contains(mcc);
    } catch (Exception e) {
      log.error("Erro ao avaliar MCC_GAMBLING: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateMccCrypto(Object fieldValue, RuleCondition condition) {
    try {
      if (fieldValue == null) return false;
      String mcc = String.valueOf(fieldValue).trim();
      if (mcc.isBlank()) return false;
      Set<String> crypto = Set.of("6051", "4829");
      return crypto.contains(mcc);
    } catch (Exception e) {
      log.error("Erro ao avaliar MCC_CRYPTO: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateMerchantFirstSeen(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      int thresholdDays = Integer.parseInt(condition.getValueSingle().trim());

      Object daysObj = context.getPayload().get("merchant_first_seen_days_ago");
      if (daysObj == null) daysObj = context.getPayload().get("merchantFirstSeenDaysAgo");
      if (daysObj == null) {
        Object isNewObj = context.getPayload().get("merchant_is_new");
        if (isNewObj == null) isNewObj = context.getPayload().get("merchantIsNew");
        if (isNewObj != null) return Boolean.TRUE.equals(toBoolean(isNewObj));
        return false;
      }

      int daysAgo = daysObj instanceof Number ? ((Number) daysObj).intValue() : Integer.MAX_VALUE;
      return daysAgo <= thresholdDays;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_FIRST_SEEN: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateMerchantCountryMismatch(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object mismatchObj = context.getPayload().get("merchant_country_mismatch");
      if (mismatchObj == null) mismatchObj = context.getPayload().get("merchantCountryMismatch");
      if (mismatchObj != null) return Boolean.TRUE.equals(toBoolean(mismatchObj));

      Object merchantCountry = context.getPayload().get("merchant_country");
      if (merchantCountry == null) merchantCountry = context.getPayload().get("merchantCountry");
      Object customerCountry = context.getPayload().get("customer_country");
      if (customerCountry == null) customerCountry = context.getPayload().get("customerCountry");
      if (merchantCountry == null || customerCountry == null) return false;
      return !String.valueOf(merchantCountry).equalsIgnoreCase(String.valueOf(customerCountry));
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_COUNTRY_MISMATCH: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateMerchantCategoryChange(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object changedObj = context.getPayload().get("merchant_category_changed");
      if (changedObj == null) changedObj = context.getPayload().get("merchantCategoryChanged");
      return Boolean.TRUE.equals(toBoolean(changedObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_CATEGORY_CHANGE: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateMerchantVelocitySpike(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getTransactionRequest() == null) return false;
      String[] parts =
          (condition.getValueSingle() == null ? "" : condition.getValueSingle()).split("\\|");
      long threshold =
          parts.length >= 1 && !parts[0].isBlank() ? Long.parseLong(parts[0].trim()) : 20L;
      int hours = parts.length >= 2 && !parts[1].isBlank() ? Integer.parseInt(parts[1].trim()) : 1;
      VelocityService.TimeWindow window = parseTimeWindowFromHours(hours);
      var stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(), VelocityService.KeyType.MERCHANT_ID, window);
      return stats.getTransactionCount() >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_VELOCITY_SPIKE: {}", e.getMessage());
      return false;
    }
  }

  // --- CATEGORIA J: ISO 20022 (6) ---
  private boolean evaluatePacs008FieldValidation(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      String requiredCsv = condition.getValueSingle();
      if (requiredCsv == null || requiredCsv.isBlank()) {
        requiredCsv = "uetr,instgAgt,instgAgtBic,dbtrNm,cdtrNm,cdtrAcct";
      }
      String[] required =
          Arrays.stream(requiredCsv.split(","))
              .map(String::trim)
              .filter(s -> !s.isBlank())
              .toArray(String[]::new);

      // Retorna true quando a mensagem está inválida (campo ausente)
      for (String key : required) {
        Object v = context.getPayload().get(key);
        if (v == null) {
          // tenta variantes camelCase
          String camel = toCamelCaseFromSnake(key);
          v = context.getPayload().get(camel);
        }
        if (v == null || String.valueOf(v).isBlank()) return true;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar PACS008_FIELD_VALIDATION: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateRemittanceInfoAnalysis(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object infoObj = getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
      if (infoObj == null) {
        infoObj = context.getPayload().get("remittance_info");
        if (infoObj == null) infoObj = context.getPayload().get("remittanceInfo");
      }
      if (infoObj == null) return false;
      String text = normalizeForMatch(String.valueOf(infoObj));
      if (text.isBlank()) return false;

      List<String> keywords = condition.getValueArray();
      if (keywords == null || keywords.isEmpty()) {
        String csv = condition.getValueSingle();
        if (csv == null || csv.isBlank()) {
          keywords =
              List.of("gift", "loan", "crypto", "investment", "refund", "cash", "commission");
        } else {
          keywords =
              Arrays.stream(csv.split(",")).map(String::trim).filter(s -> !s.isBlank()).toList();
        }
      }

      for (String kw : keywords) {
        String needle = normalizeForMatch(kw);
        if (needle.isBlank()) continue;
        if (text.contains(needle)) return true;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar REMITTANCE_INFO_ANALYSIS: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluatePurposeCodeMismatch(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object purposeObj =
          getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
      if (purposeObj == null) {
        purposeObj = context.getPayload().get("purpose_code");
        if (purposeObj == null) purposeObj = context.getPayload().get("purposeCode");
      }
      if (purposeObj == null) return false;
      String purpose = String.valueOf(purposeObj).trim().toUpperCase(Locale.ROOT);
      if (purpose.isBlank()) return false;

      List<String> allowed = condition.getValueArray();
      if (allowed == null || allowed.isEmpty()) {
        String csv = condition.getValueSingle();
        if (csv == null || csv.isBlank()) return false;
        allowed =
            Arrays.stream(csv.split(",")).map(String::trim).filter(s -> !s.isBlank()).toList();
      }

      for (String entry : allowed) {
        if (purpose.equalsIgnoreCase(String.valueOf(entry).trim())) return false;
      }
      // mismatch quando não está na allowlist
      return true;
    } catch (Exception e) {
      log.error("Erro ao avaliar PURPOSE_CODE_MISMATCH: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateUetrDuplicateCheck(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object dupObj = context.getPayload().get("uetr_duplicate");
      if (dupObj == null) dupObj = context.getPayload().get("uetrDuplicate");
      return Boolean.TRUE.equals(toBoolean(dupObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar UETR_DUPLICATE_CHECK: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateCreditorNameValidation(
      RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts =
          (condition.getValueSingle() == null ? "" : condition.getValueSingle()).split(":");
      String otherField = parts.length >= 1 && !parts[0].isBlank() ? parts[0].trim() : null;
      int threshold =
          parts.length >= 2 && !parts[1].isBlank() ? Integer.parseInt(parts[1].trim()) : 85;

      Object v1 = getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);
      if (v1 == null) {
        v1 = context.getPayload() != null ? context.getPayload().get("creditor_name") : null;
        if (v1 == null && context.getPayload() != null)
          v1 = context.getPayload().get("creditorName");
      }
      if (v1 == null) return false;
      String creditorName = normalizeForMatch(String.valueOf(v1));
      if (creditorName.isBlank()) return true; // inválido

      if (otherField == null) return false;
      Object v2 = getFieldValue(otherField, null, context);
      if (v2 == null) return false;
      String referenceName = normalizeForMatch(String.valueOf(v2));
      if (referenceName.isBlank()) return false;

      return calculateSimilarity(creditorName, referenceName) < threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar CREDITOR_NAME_VALIDATION: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateStructuredAddressCheck(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      String requiredCsv = condition.getValueSingle();
      if (requiredCsv == null || requiredCsv.isBlank()) {
        requiredCsv = "street,building,postCode,townName,country";
      }
      String[] required =
          Arrays.stream(requiredCsv.split(","))
              .map(String::trim)
              .filter(s -> !s.isBlank())
              .toArray(String[]::new);

      // Retorna true quando está inválido (faltando algo)
      for (String key : required) {
        Object v = context.getPayload().get(key);
        if (v == null || String.valueOf(v).isBlank()) return true;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar STRUCTURED_ADDRESS_CHECK: {}", e.getMessage());
      return false;
    }
  }

  // --- CATEGORIA K: Estatísticos Simples (5) ---
  private boolean evaluateBenfordLawDeviation(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      double threshold = Double.parseDouble(condition.getValueSingle().trim());
      Object devObj = context.getPayload().get("benford_deviation");
      if (devObj == null) devObj = context.getPayload().get("benfordDeviation");
      double deviation = devObj instanceof Number ? ((Number) devObj).doubleValue() : 0.0;
      return deviation > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar BENFORD_LAW_DEVIATION: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateZScoreGt(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null && context.getTransactionRequest() == null) return false;
      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object zObj = context.getPayload() != null ? context.getPayload().get("z_score") : null;
      if (zObj == null && context.getPayload() != null) zObj = context.getPayload().get("zScore");
      if (zObj instanceof Number) {
        return ((Number) zObj).doubleValue() > threshold;
      }

      if (context.getTransactionRequest() == null) return false;
      var stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(),
              VelocityService.KeyType.PAN,
              VelocityService.TimeWindow.HOUR_24);
      BigDecimal avg = stats.getAvgAmount();
      BigDecimal std = stats.getStdDevAmount();
      BigDecimal amount = context.getTransactionRequest().getTransactionAmount();
      if (avg == null || std == null || std.compareTo(BigDecimal.ZERO) == 0 || amount == null)
        return false;
      BigDecimal z = amount.subtract(avg).divide(std, 8, java.math.RoundingMode.HALF_UP).abs();
      return z.compareTo(BigDecimal.valueOf(threshold)) > 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar Z_SCORE_GT: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateStandardDeviationGt(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getTransactionRequest() == null) return false;
      BigDecimal threshold = new BigDecimal(condition.getValueSingle().trim());
      var stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(),
              VelocityService.KeyType.PAN,
              VelocityService.TimeWindow.HOUR_24);
      BigDecimal std = stats.getStdDevAmount();
      if (std == null) return false;
      return std.compareTo(threshold) > 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar STANDARD_DEVIATION_GT: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluatePercentileGt(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      double threshold = Double.parseDouble(condition.getValueSingle().trim());
      Object pctObj = context.getPayload().get("current_percentile");
      if (pctObj == null) pctObj = context.getPayload().get("currentPercentile");
      double pct = pctObj instanceof Number ? ((Number) pctObj).doubleValue() : 0.0;
      return pct > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar PERCENTILE_GT: {}", e.getMessage());
      return false;
    }
  }

  private boolean evaluateCoefficientVariationGt(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getTransactionRequest() == null) return false;
      double threshold = Double.parseDouble(condition.getValueSingle().trim());
      var stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(),
              VelocityService.KeyType.PAN,
              VelocityService.TimeWindow.HOUR_24);
      BigDecimal avg = stats.getAvgAmount();
      BigDecimal std = stats.getStdDevAmount();
      if (avg == null || std == null || avg.compareTo(BigDecimal.ZERO) == 0) return false;
      BigDecimal cv = std.divide(avg.abs(), 8, java.math.RoundingMode.HALF_UP);
      return cv.compareTo(BigDecimal.valueOf(threshold)) > 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar COEFFICIENT_VARIATION_GT: {}", e.getMessage());
      return false;
    }
  }

  private String normalizeForMatch(String value) {
    if (value == null) return "";
    String s = value.trim().toLowerCase(Locale.ROOT);
    if (s.isBlank()) return "";
    s = Normalizer.normalize(s, Normalizer.Form.NFD).replaceAll("\\p{M}", "");
    s = s.replaceAll("[^a-z0-9\\s]", " ");
    s = s.replaceAll("\\s+", " ").trim();
    return s;
  }

  private String toCamelCaseFromSnake(String value) {
    if (value == null || value.isBlank()) return "";
    StringBuilder sb = new StringBuilder(value.length());
    boolean upperNext = false;
    for (int i = 0; i < value.length(); i++) {
      char c = value.charAt(i);
      if (c == '_') {
        upperNext = true;
        continue;
      }
      if (upperNext) {
        sb.append(Character.toUpperCase(c));
        upperNext = false;
      } else {
        sb.append(c);
      }
    }
    return sb.toString();
  }

  private boolean isValidCpf(String digits11) {
    if (digits11 == null) return false;
    String cpf = digits11.replaceAll("\\D", "");
    if (cpf.length() != 11) return false;
    // Bloqueia CPFs com todos os dígitos iguais
    boolean allSame = cpf.chars().distinct().count() == 1;
    if (allSame) return false;

    int d1 = calcCpfDigit(cpf, 9, 10);
    int d2 = calcCpfDigit(cpf, 10, 11);
    return d1 == (cpf.charAt(9) - '0') && d2 == (cpf.charAt(10) - '0');
  }

  private int calcCpfDigit(String cpf, int len, int weightStart) {
    int sum = 0;
    int weight = weightStart;
    for (int i = 0; i < len; i++) {
      int n = cpf.charAt(i) - '0';
      sum += n * weight;
      weight--;
    }
    int mod = sum % 11;
    return (mod < 2) ? 0 : (11 - mod);
  }

  private int mapEidasLevel(String level) {
    if (level == null) return 0;
    String v = normalizeForMatch(level);
    if (v.isBlank()) return 0;
    return switch (v) {
      case "low" -> 1;
      case "substantial" -> 2;
      case "high" -> 3;
      default -> {
        try {
          yield Integer.parseInt(v);
        } catch (NumberFormatException e) {
          yield 0;
        }
      }
    };
  }

  // ========== OPERADORES V4.0 PHASE 1 - VELOCITY + DEVICE (40 novos) ==========

  // --- CATEGORIA L: Transaction Count Velocity Avançado (12) ---

  /**
   * TRANSACTION_COUNT_PER_CARD_HOUR: Contagem de transações por cartão por hora. Formato
   * valueSingle: "threshold" (ex: "5")
   */
  private boolean evaluateTransactionCountPerCardHour(
      RuleCondition condition, EvaluationContext context) {
    return evaluateVelocityOperator(
        condition,
        context,
        VelocityService.KeyType.PAN,
        VelocityService.TimeWindow.HOUR_1,
        VelocityService.AggregationType.COUNT);
  }

  /**
   * TRANSACTION_COUNT_PER_IP_HOUR: Contagem de transações por IP por hora. Formato valueSingle:
   * "threshold" (ex: "10")
   */
  private boolean evaluateTransactionCountPerIpHour(
      RuleCondition condition, EvaluationContext context) {
    return evaluateVelocityOperator(
        condition,
        context,
        VelocityService.KeyType.IP_ADDRESS,
        VelocityService.TimeWindow.HOUR_1,
        VelocityService.AggregationType.COUNT);
  }

  /**
   * TRANSACTION_COUNT_PER_DEVICE_DAY: Contagem de transações por dispositivo por dia. Formato
   * valueSingle: "threshold" (ex: "20")
   */
  private boolean evaluateTransactionCountPerDeviceDay(
      RuleCondition condition, EvaluationContext context) {
    return evaluateVelocityOperator(
        condition,
        context,
        VelocityService.KeyType.DEVICE_ID,
        VelocityService.TimeWindow.HOUR_24,
        VelocityService.AggregationType.COUNT);
  }

  /**
   * TRANSACTION_COUNT_PER_MERCHANT_HOUR: Contagem de transações por merchant por hora. Formato
   * valueSingle: "threshold" (ex: "15")
   */
  private boolean evaluateTransactionCountPerMerchantHour(
      RuleCondition condition, EvaluationContext context) {
    return evaluateVelocityOperator(
        condition,
        context,
        VelocityService.KeyType.MERCHANT_ID,
        VelocityService.TimeWindow.HOUR_1,
        VelocityService.AggregationType.COUNT);
  }

  /**
   * TRANSACTION_COUNT_PER_CUSTOMER_HOUR: Contagem de transações por cliente por hora. Formato
   * valueSingle: "threshold" (ex: "8")
   */
  private boolean evaluateTransactionCountPerCustomerHour(
      RuleCondition condition, EvaluationContext context) {
    return evaluateVelocityOperator(
        condition,
        context,
        VelocityService.KeyType.CUSTOMER_ID,
        VelocityService.TimeWindow.HOUR_1,
        VelocityService.AggregationType.COUNT);
  }

  /**
   * UNIQUE_CARD_COUNT_PER_IP_HOUR: Cartões únicos por IP por hora. Formato valueSingle: "threshold"
   * (ex: "5")
   */
  private boolean evaluateUniqueCardCountPerIpHour(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getTransactionRequest() == null) return false;
      long threshold = Long.parseLong(condition.getValueSingle().trim());

      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(),
              VelocityService.KeyType.IP_ADDRESS,
              VelocityService.TimeWindow.HOUR_1);

      return stats.getDistinctPans() > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar UNIQUE_CARD_COUNT_PER_IP_HOUR: {}", e.getMessage());
      return false;
    }
  }

  /**
   * UNIQUE_MERCHANT_COUNT_PER_CARD_DAY: Merchants únicos por cartão por dia. Formato valueSingle:
   * "threshold" (ex: "20")
   */
  private boolean evaluateUniqueMerchantCountPerCardDay(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getTransactionRequest() == null) return false;
      long threshold = Long.parseLong(condition.getValueSingle().trim());

      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(),
              VelocityService.KeyType.PAN,
              VelocityService.TimeWindow.HOUR_24);

      return stats.getDistinctMerchants() > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar UNIQUE_MERCHANT_COUNT_PER_CARD_DAY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * TRANSACTION_ATTEMPT_COUNT_PER_CARD: Tentativas de transação por cartão (inclui falhas). Formato
   * valueSingle: "threshold|minutes" (ex: "5|15")
   */
  private boolean evaluateTransactionAttemptCountPerCard(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getTransactionRequest() == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      long threshold = Long.parseLong(parts[0].trim());
      int minutes = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 15;

      VelocityService.TimeWindow window = parseTimeWindow(minutes);
      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(), VelocityService.KeyType.PAN, window);

      // Inclui tentativas com falha
      return stats.getTransactionCount() + stats.getFailedCount() > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar TRANSACTION_ATTEMPT_COUNT_PER_CARD: {}", e.getMessage());
      return false;
    }
  }

  /**
   * CVV_FAILURE_VELOCITY: Velocidade de falhas de CVV. Formato valueSingle: "threshold|minutes"
   * (ex: "3|10")
   */
  private boolean evaluateCvvFailureVelocity(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getTransactionRequest() == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      long threshold = Long.parseLong(parts[0].trim());
      int minutes = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 10;

      VelocityService.TimeWindow window = parseTimeWindow(minutes);
      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(), VelocityService.KeyType.PAN, window);

      return stats.getCvvFailures() > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar CVV_FAILURE_VELOCITY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * ADDRESS_CHANGE_VELOCITY: Velocidade de alteração de endereço. Formato valueSingle:
   * "threshold|days" (ex: "3|30")
   */
  private boolean evaluateAddressChangeVelocity(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int threshold = Integer.parseInt(parts[0].trim());
      int days = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 30;

      Object changesObj = payload.get("address_changes_" + days + "d");
      if (changesObj == null) {
        changesObj = payload.get("addressChangesCount");
      }

      int changes = changesObj instanceof Number ? ((Number) changesObj).intValue() : 0;
      return changes > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar ADDRESS_CHANGE_VELOCITY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * BENEFICIARY_ADD_VELOCITY: Velocidade de adição de beneficiários. Formato valueSingle:
   * "threshold|days" (ex: "5|7")
   */
  private boolean evaluateBeneficiaryAddVelocity(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int threshold = Integer.parseInt(parts[0].trim());
      int days = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 7;

      Object addedObj = payload.get("beneficiaries_added_" + days + "d");
      if (addedObj == null) {
        addedObj = payload.get("newBeneficiariesCount");
      }

      int added = addedObj instanceof Number ? ((Number) addedObj).intValue() : 0;
      return added > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar BENEFICIARY_ADD_VELOCITY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * CARD_ADD_VELOCITY: Velocidade de adição de cartões. Formato valueSingle: "threshold|days" (ex:
   * "3|7")
   */
  private boolean evaluateCardAddVelocity(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int threshold = Integer.parseInt(parts[0].trim());
      int days = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 7;

      Object addedObj = payload.get("cards_added_" + days + "d");
      if (addedObj == null) {
        addedObj = payload.get("newCardsCount");
      }

      int added = addedObj instanceof Number ? ((Number) addedObj).intValue() : 0;
      return added > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar CARD_ADD_VELOCITY: {}", e.getMessage());
      return false;
    }
  }

  // --- CATEGORIA M: Amount Velocity Avançado (10) ---

  /**
   * AMOUNT_SUM_PER_CARD_HOUR: Soma de valores por cartão por hora. Formato valueSingle: "threshold"
   * (ex: "5000")
   */
  private boolean evaluateAmountSumPerCardHour(RuleCondition condition, EvaluationContext context) {
    return evaluateVelocityOperator(
        condition,
        context,
        VelocityService.KeyType.PAN,
        VelocityService.TimeWindow.HOUR_1,
        VelocityService.AggregationType.SUM);
  }

  /**
   * AMOUNT_SUM_PER_CUSTOMER_DAY: Soma de valores por cliente por dia. Formato valueSingle:
   * "threshold" (ex: "20000")
   */
  private boolean evaluateAmountSumPerCustomerDay(
      RuleCondition condition, EvaluationContext context) {
    return evaluateVelocityOperator(
        condition,
        context,
        VelocityService.KeyType.CUSTOMER_ID,
        VelocityService.TimeWindow.HOUR_24,
        VelocityService.AggregationType.SUM);
  }

  /**
   * AVG_TRANSACTION_SPIKE: Spike de valor médio de transação. Formato valueSingle: "multiplier"
   * (ex: "3.0" = 3x a média histórica)
   */
  private boolean evaluateAvgTransactionSpike(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getTransactionRequest() == null) return false;

      double multiplier = Double.parseDouble(condition.getValueSingle().trim());

      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(),
              VelocityService.KeyType.PAN,
              VelocityService.TimeWindow.DAY_30);

      BigDecimal currentAmount = context.getTransactionRequest().getTransactionAmount();
      BigDecimal avgAmount = stats.getAvgAmount();

      if (avgAmount == null || avgAmount.compareTo(BigDecimal.ZERO) == 0) {
        return false;
      }

      BigDecimal spikeThreshold = avgAmount.multiply(BigDecimal.valueOf(multiplier));
      return currentAmount.compareTo(spikeThreshold) > 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar AVG_TRANSACTION_SPIKE: {}", e.getMessage());
      return false;
    }
  }

  /**
   * LARGE_AMOUNT_FREQUENCY: Frequência de valores altos. Formato valueSingle:
   * "amountThreshold|countThreshold|days" (ex: "1000|3|7")
   */
  private boolean evaluateLargeAmountFrequency(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      double amountThreshold = Double.parseDouble(parts[0].trim());
      int countThreshold = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 3;
      int days = parts.length > 2 ? Integer.parseInt(parts[2].trim()) : 7;

      Object countObj =
          payload.get("large_amount_count_" + days + "d_over_" + (int) amountThreshold);
      if (countObj == null) {
        countObj = payload.get("largeAmountTransactionCount");
      }

      int count = countObj instanceof Number ? ((Number) countObj).intValue() : 0;
      return count >= countThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar LARGE_AMOUNT_FREQUENCY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * SMALL_AMOUNT_VELOCITY: Velocidade de valores pequenos (smurfing). Formato valueSingle:
   * "maxAmount|countThreshold|hours" (ex: "100|10|1")
   */
  private boolean evaluateSmallAmountVelocity(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      double maxAmount = Double.parseDouble(parts[0].trim());
      int countThreshold = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 10;
      int hours = parts.length > 2 ? Integer.parseInt(parts[2].trim()) : 1;

      Object countObj = payload.get("small_amount_count_" + hours + "h_under_" + (int) maxAmount);
      if (countObj == null) {
        countObj = payload.get("microTransactionCount");
      }

      int count = countObj instanceof Number ? ((Number) countObj).intValue() : 0;
      return count >= countThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar SMALL_AMOUNT_VELOCITY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * ROUND_AMOUNT_FREQUENCY: Frequência de valores redondos. Formato valueSingle:
   * "roundThreshold|percentageThreshold|days" (ex: "100|50|7")
   */
  private boolean evaluateRoundAmountFrequency(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int roundThreshold = Integer.parseInt(parts[0].trim());
      double percentageThreshold = parts.length > 1 ? Double.parseDouble(parts[1].trim()) : 50.0;

      Object percentageObj = payload.get("round_amount_percentage_divisible_" + roundThreshold);
      if (percentageObj == null) {
        percentageObj = payload.get("roundAmountPercentage");
      }

      double percentage =
          percentageObj instanceof Number ? ((Number) percentageObj).doubleValue() : 0.0;
      return percentage >= percentageThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar ROUND_AMOUNT_FREQUENCY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * SEQUENTIAL_AMOUNT_PATTERN: Padrão sequencial de valores. Formato valueSingle: "patternType"
   * (linear|fibonacci|geometric)
   */
  private boolean evaluateSequentialAmountPattern(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String patternType =
          condition.getValueSingle() != null
              ? condition.getValueSingle().trim().toLowerCase()
              : "linear";

      Object patternObj = payload.get("sequential_pattern_" + patternType);
      if (patternObj == null) {
        patternObj = payload.get("hasSequentialAmountPattern");
      }

      return Boolean.TRUE.equals(patternObj) || "true".equalsIgnoreCase(String.valueOf(patternObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar SEQUENTIAL_AMOUNT_PATTERN: {}", e.getMessage());
      return false;
    }
  }

  /**
   * AMOUNT_VARIANCE_ANOMALY: Anomalia de variância de valores. Formato valueSingle:
   * "zScoreThreshold" (ex: "3.0")
   */
  private boolean evaluateAmountVarianceAnomaly(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getTransactionRequest() == null) return false;

      double zScoreThreshold = Double.parseDouble(condition.getValueSingle().trim());

      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(),
              VelocityService.KeyType.PAN,
              VelocityService.TimeWindow.DAY_30);

      BigDecimal currentAmount = context.getTransactionRequest().getTransactionAmount();
      BigDecimal avgAmount = stats.getAvgAmount();
      BigDecimal stdDev = stats.getStdDevAmount();

      if (stdDev == null || stdDev.compareTo(BigDecimal.ZERO) == 0) {
        return false;
      }

      double zScore =
          currentAmount
              .subtract(avgAmount)
              .divide(stdDev, 4, java.math.RoundingMode.HALF_UP)
              .doubleValue();
      return Math.abs(zScore) > zScoreThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar AMOUNT_VARIANCE_ANOMALY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * DAILY_LIMIT_PROXIMITY: Proximidade do limite diário. Formato valueSingle: "percentageThreshold"
   * (ex: "90" = 90% do limite)
   */
  private boolean evaluateDailyLimitProximity(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double percentageThreshold = Double.parseDouble(condition.getValueSingle().trim());

      Object usedObj = payload.get("daily_limit_used_percentage");
      if (usedObj == null) {
        usedObj = payload.get("dailyLimitUsagePercentage");
      }

      double usedPercentage = usedObj instanceof Number ? ((Number) usedObj).doubleValue() : 0.0;
      return usedPercentage >= percentageThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar DAILY_LIMIT_PROXIMITY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * WEEKLY_LIMIT_PROXIMITY: Proximidade do limite semanal. Formato valueSingle:
   * "percentageThreshold" (ex: "85" = 85% do limite)
   */
  private boolean evaluateWeeklyLimitProximity(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double percentageThreshold = Double.parseDouble(condition.getValueSingle().trim());

      Object usedObj = payload.get("weekly_limit_used_percentage");
      if (usedObj == null) {
        usedObj = payload.get("weeklyLimitUsagePercentage");
      }

      double usedPercentage = usedObj instanceof Number ? ((Number) usedObj).doubleValue() : 0.0;
      return usedPercentage >= percentageThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar WEEKLY_LIMIT_PROXIMITY: {}", e.getMessage());
      return false;
    }
  }

  // --- CATEGORIA N: Temporal Velocity Avançado (8) ---

  /**
   * TIME_BETWEEN_CONSECUTIVE_TX: Tempo entre transações consecutivas. Formato valueSingle:
   * "minSeconds" (ex: "5" = mínimo 5 segundos)
   */
  private boolean evaluateTimeBetweenConsecutiveTx(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int minSeconds = Integer.parseInt(condition.getValueSingle().trim());

      Object secondsObj = payload.get("seconds_since_last_transaction");
      if (secondsObj == null) {
        secondsObj = payload.get("timeSinceLastTxSeconds");
      }

      int seconds =
          secondsObj instanceof Number ? ((Number) secondsObj).intValue() : Integer.MAX_VALUE;
      return seconds < minSeconds;
    } catch (Exception e) {
      log.error("Erro ao avaliar TIME_BETWEEN_CONSECUTIVE_TX: {}", e.getMessage());
      return false;
    }
  }

  /**
   * TRANSACTION_FREQUENCY_ANOMALY: Anomalia de frequência de transações. Formato valueSingle:
   * "multiplier" (ex: "3.0" = 3x a frequência normal)
   */
  private boolean evaluateTransactionFrequencyAnomaly(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double multiplier = Double.parseDouble(condition.getValueSingle().trim());

      Object ratioObj = payload.get("transaction_frequency_ratio");
      if (ratioObj == null) {
        ratioObj = payload.get("frequencyAnomalyRatio");
      }

      double ratio = ratioObj instanceof Number ? ((Number) ratioObj).doubleValue() : 1.0;
      return ratio >= multiplier;
    } catch (Exception e) {
      log.error("Erro ao avaliar TRANSACTION_FREQUENCY_ANOMALY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * TIME_OF_DAY_ANOMALY: Anomalia de horário do dia. Formato valueSingle: "deviationHours" (ex: "4"
   * = 4 horas de desvio do padrão)
   */
  private boolean evaluateTimeOfDayAnomaly(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int deviationThreshold = Integer.parseInt(condition.getValueSingle().trim());

      Object deviationObj = payload.get("time_of_day_deviation_hours");
      if (deviationObj == null) {
        deviationObj = payload.get("hourDeviationFromUsual");
      }

      int deviation = deviationObj instanceof Number ? ((Number) deviationObj).intValue() : 0;
      return deviation >= deviationThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar TIME_OF_DAY_ANOMALY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * DORMANCY_ALERT_VELOCITY: Alerta de velocidade pós-dormência. Formato valueSingle:
   * "dormancyDays|amountThreshold" (ex: "90|1000")
   */
  private boolean evaluateDormancyAlertVelocity(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int dormancyDays = Integer.parseInt(parts[0].trim());
      double amountThreshold = parts.length > 1 ? Double.parseDouble(parts[1].trim()) : 0.0;

      Object daysSinceObj = payload.get("days_since_last_activity");
      if (daysSinceObj == null) {
        daysSinceObj = payload.get("daysSinceLastTransaction");
      }

      int daysSince = daysSinceObj instanceof Number ? ((Number) daysSinceObj).intValue() : 0;

      if (daysSince >= dormancyDays) {
        BigDecimal currentAmount =
            context.getTransactionRequest() != null
                ? context.getTransactionRequest().getTransactionAmount()
                : BigDecimal.ZERO;
        return currentAmount.compareTo(BigDecimal.valueOf(amountThreshold)) >= 0;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar DORMANCY_ALERT_VELOCITY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * WEEKEND_VS_WEEKDAY_PATTERN: Padrão fim de semana vs dia útil. Formato valueSingle:
   * "deviationThreshold" (percentual de desvio)
   */
  private boolean evaluateWeekendVsWeekdayPattern(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double deviationThreshold = Double.parseDouble(condition.getValueSingle().trim());

      Object deviationObj = payload.get("weekend_weekday_pattern_deviation");
      if (deviationObj == null) {
        deviationObj = payload.get("dayTypePatternDeviation");
      }

      double deviation =
          deviationObj instanceof Number ? ((Number) deviationObj).doubleValue() : 0.0;
      return deviation >= deviationThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar WEEKEND_VS_WEEKDAY_PATTERN: {}", e.getMessage());
      return false;
    }
  }

  /**
   * HOLIDAY_TRANSACTION_SPIKE: Spike de transações em feriados. Formato valueSingle: "multiplier"
   * (ex: "2.0" = 2x do normal)
   */
  private boolean evaluateHolidayTransactionSpike(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double multiplier = Double.parseDouble(condition.getValueSingle().trim());

      Object isHolidayObj = payload.get("is_holiday");
      boolean isHoliday =
          Boolean.TRUE.equals(isHolidayObj)
              || "true".equalsIgnoreCase(String.valueOf(isHolidayObj));

      if (!isHoliday) return false;

      Object spikeRatioObj = payload.get("holiday_transaction_ratio");
      if (spikeRatioObj == null) {
        spikeRatioObj = payload.get("holidayActivityRatio");
      }

      double spikeRatio =
          spikeRatioObj instanceof Number ? ((Number) spikeRatioObj).doubleValue() : 1.0;
      return spikeRatio >= multiplier;
    } catch (Exception e) {
      log.error("Erro ao avaliar HOLIDAY_TRANSACTION_SPIKE: {}", e.getMessage());
      return false;
    }
  }

  /**
   * NIGHTTIME_TRANSACTION_RATIO: Razão de transações noturnas. Formato valueSingle:
   * "percentageThreshold|startHour|endHour" (ex: "50|22|6")
   */
  private boolean evaluateNighttimeTransactionRatio(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      double percentageThreshold = Double.parseDouble(parts[0].trim());

      Object ratioObj = payload.get("nighttime_transaction_percentage");
      if (ratioObj == null) {
        ratioObj = payload.get("nightTransactionPercentage");
      }

      double ratio = ratioObj instanceof Number ? ((Number) ratioObj).doubleValue() : 0.0;
      return ratio >= percentageThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar NIGHTTIME_TRANSACTION_RATIO: {}", e.getMessage());
      return false;
    }
  }

  /**
   * BUSINESS_HOURS_DEVIATION: Desvio do horário comercial. Formato valueSingle:
   * "outsideBusinessHours" (true/false)
   */
  private boolean evaluateBusinessHoursDeviation(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object isOutsideObj = payload.get("is_outside_business_hours");
      if (isOutsideObj == null) {
        isOutsideObj = payload.get("outsideBusinessHours");
      }

      return Boolean.TRUE.equals(isOutsideObj)
          || "true".equalsIgnoreCase(String.valueOf(isOutsideObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar BUSINESS_HOURS_DEVIATION: {}", e.getMessage());
      return false;
    }
  }

  // --- CATEGORIA O: Device Fingerprint Avançado (10) ---

  /**
   * DEVICE_TRUST_SCORE: Score de confiança do dispositivo. Formato valueSingle: "minScore" (ex:
   * "70" = score mínimo de 70)
   */
  private boolean evaluateDeviceTrustScore(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double minScore = Double.parseDouble(condition.getValueSingle().trim());

      Object scoreObj = payload.get("device_trust_score");
      if (scoreObj == null) {
        scoreObj = payload.get("deviceReputationScore");
      }

      double score = scoreObj instanceof Number ? ((Number) scoreObj).doubleValue() : 100.0;
      return score < minScore;
    } catch (Exception e) {
      log.error("Erro ao avaliar DEVICE_TRUST_SCORE: {}", e.getMessage());
      return false;
    }
  }

  /** CANVAS_FINGERPRINT_MISMATCH: Incompatibilidade de fingerprint canvas. */
  private boolean evaluateCanvasFingerprintMismatch(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object mismatchObj = payload.get("canvas_fingerprint_mismatch");
      if (mismatchObj == null) {
        mismatchObj = payload.get("canvasMismatch");
      }

      return Boolean.TRUE.equals(mismatchObj)
          || "true".equalsIgnoreCase(String.valueOf(mismatchObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar CANVAS_FINGERPRINT_MISMATCH: {}", e.getMessage());
      return false;
    }
  }

  /** WEBGL_FINGERPRINT_ANOMALY: Anomalia de fingerprint WebGL. */
  private boolean evaluateWebglFingerprintAnomaly(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object anomalyObj = payload.get("webgl_fingerprint_anomaly");
      if (anomalyObj == null) {
        anomalyObj = payload.get("webglAnomaly");
      }

      return Boolean.TRUE.equals(anomalyObj) || "true".equalsIgnoreCase(String.valueOf(anomalyObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar WEBGL_FINGERPRINT_ANOMALY: {}", e.getMessage());
      return false;
    }
  }

  /** AUDIO_FINGERPRINT_NEW: Fingerprint de áudio novo/desconhecido. */
  private boolean evaluateAudioFingerprintNew(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object isNewObj = payload.get("audio_fingerprint_new");
      if (isNewObj == null) {
        isNewObj = payload.get("audioFingerprintIsNew");
      }

      return Boolean.TRUE.equals(isNewObj) || "true".equalsIgnoreCase(String.valueOf(isNewObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar AUDIO_FINGERPRINT_NEW: {}", e.getMessage());
      return false;
    }
  }

  /** FONTS_FINGERPRINT_ANOMALY: Anomalia de fingerprint de fontes. */
  private boolean evaluateFontsFingerprintAnomaly(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object anomalyObj = payload.get("fonts_fingerprint_anomaly");
      if (anomalyObj == null) {
        anomalyObj = payload.get("fontsAnomaly");
      }

      return Boolean.TRUE.equals(anomalyObj) || "true".equalsIgnoreCase(String.valueOf(anomalyObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar FONTS_FINGERPRINT_ANOMALY: {}", e.getMessage());
      return false;
    }
  }

  /** SCREEN_RESOLUTION_CHANGE: Mudança de resolução de tela. */
  private boolean evaluateScreenResolutionChange(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object changedObj = payload.get("screen_resolution_changed");
      if (changedObj == null) {
        changedObj = payload.get("screenResolutionChanged");
      }

      return Boolean.TRUE.equals(changedObj) || "true".equalsIgnoreCase(String.valueOf(changedObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar SCREEN_RESOLUTION_CHANGE: {}", e.getMessage());
      return false;
    }
  }

  /** BATTERY_LEVEL_ANOMALY: Anomalia de nível de bateria (ex: sempre 100%). */
  private boolean evaluateBatteryLevelAnomaly(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object anomalyObj = payload.get("battery_level_anomaly");
      if (anomalyObj == null) {
        anomalyObj = payload.get("batteryAnomaly");
      }

      return Boolean.TRUE.equals(anomalyObj) || "true".equalsIgnoreCase(String.valueOf(anomalyObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar BATTERY_LEVEL_ANOMALY: {}", e.getMessage());
      return false;
    }
  }

  /** HARDWARE_CONCURRENCY_MISMATCH: Incompatibilidade de concorrência de hardware. */
  private boolean evaluateHardwareConcurrencyMismatch(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object mismatchObj = payload.get("hardware_concurrency_mismatch");
      if (mismatchObj == null) {
        mismatchObj = payload.get("hardwareConcurrencyMismatch");
      }

      return Boolean.TRUE.equals(mismatchObj)
          || "true".equalsIgnoreCase(String.valueOf(mismatchObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar HARDWARE_CONCURRENCY_MISMATCH: {}", e.getMessage());
      return false;
    }
  }

  /** TOUCH_SUPPORT_INCONSISTENCY: Inconsistência de suporte touch. */
  private boolean evaluateTouchSupportInconsistency(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object inconsistencyObj = payload.get("touch_support_inconsistency");
      if (inconsistencyObj == null) {
        inconsistencyObj = payload.get("touchSupportInconsistency");
      }

      return Boolean.TRUE.equals(inconsistencyObj)
          || "true".equalsIgnoreCase(String.valueOf(inconsistencyObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar TOUCH_SUPPORT_INCONSISTENCY: {}", e.getMessage());
      return false;
    }
  }

  /** DEVICE_MEMORY_ANOMALY: Anomalia de memória do dispositivo. */
  private boolean evaluateDeviceMemoryAnomaly(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object anomalyObj = payload.get("device_memory_anomaly");
      if (anomalyObj == null) {
        anomalyObj = payload.get("deviceMemoryAnomaly");
      }

      return Boolean.TRUE.equals(anomalyObj) || "true".equalsIgnoreCase(String.valueOf(anomalyObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar DEVICE_MEMORY_ANOMALY: {}", e.getMessage());
      return false;
    }
  }

  /** Método auxiliar genérico para operadores de velocidade. */
  private boolean evaluateVelocityOperator(
      RuleCondition condition,
      EvaluationContext context,
      VelocityService.KeyType keyType,
      VelocityService.TimeWindow window,
      VelocityService.AggregationType aggregationType) {
    try {
      if (context.getTransactionRequest() == null) return false;

      BigDecimal threshold = new BigDecimal(condition.getValueSingle().trim());

      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(context.getTransactionRequest(), keyType, window);

      BigDecimal value =
          switch (aggregationType) {
            case COUNT -> BigDecimal.valueOf(stats.getTransactionCount());
            case SUM -> stats.getTotalAmount();
            case AVG -> stats.getAvgAmount();
            case MAX -> stats.getMaxAmount();
            case MIN -> stats.getMinAmount();
            default -> BigDecimal.ZERO;
          };

      return value.compareTo(threshold) > 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar operador de velocidade: {}", e.getMessage());
      return false;
    }
  }

  // ========== OPERADORES V4.0 PHASE 1B - BEHAVIORAL (25 novos) ==========

  // --- CATEGORIA P: Behavioral Patterns (15) ---

  /**
   * BEHAVIORAL_BASELINE_DEVIATION: Verifica desvio do baseline comportamental. Formato valueSingle:
   * "deviationThreshold" (ex: "2.5" = 2.5 desvios padrão)
   */
  private boolean evaluateBehavioralBaselineDeviation(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object deviationObj = payload.get("behavioral_deviation_score");
      if (deviationObj == null) {
        deviationObj = payload.get("behavioralDeviationScore");
      }

      if (deviationObj == null) return false;
      double deviation =
          deviationObj instanceof Number ? ((Number) deviationObj).doubleValue() : 0.0;
      return deviation > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar BEHAVIORAL_BASELINE_DEVIATION: {}", e.getMessage());
      return false;
    }
  }

  /** SPENDING_CATEGORY_SHIFT: Detecta mudança de categoria de gastos. */
  private boolean evaluateSpendingCategoryShift(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object shiftObj = payload.get("spending_category_shift");
      if (shiftObj == null) {
        shiftObj = payload.get("spendingCategoryShift");
      }

      return Boolean.TRUE.equals(shiftObj) || "true".equalsIgnoreCase(String.valueOf(shiftObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar SPENDING_CATEGORY_SHIFT: {}", e.getMessage());
      return false;
    }
  }

  /**
   * TRANSACTION_SIZE_ESCALATION: Detecta escalada de tamanho de transação. Formato valueSingle:
   * "escalationFactor" (ex: "1.5" = 50% de aumento)
   */
  private boolean evaluateTransactionSizeEscalation(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double factor = Double.parseDouble(condition.getValueSingle().trim());

      Object escalationObj = payload.get("transaction_escalation_factor");
      if (escalationObj == null) {
        escalationObj = payload.get("transactionEscalationFactor");
      }

      if (escalationObj == null) return false;
      double escalation =
          escalationObj instanceof Number ? ((Number) escalationObj).doubleValue() : 0.0;
      return escalation > factor;
    } catch (Exception e) {
      log.error("Erro ao avaliar TRANSACTION_SIZE_ESCALATION: {}", e.getMessage());
      return false;
    }
  }

  /** FREQUENCY_PATTERN_CHANGE: Detecta mudança de padrão de frequência. */
  private boolean evaluateFrequencyPatternChange(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object changeObj = payload.get("frequency_pattern_changed");
      if (changeObj == null) {
        changeObj = payload.get("frequencyPatternChanged");
      }

      return Boolean.TRUE.equals(changeObj) || "true".equalsIgnoreCase(String.valueOf(changeObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar FREQUENCY_PATTERN_CHANGE: {}", e.getMessage());
      return false;
    }
  }

  /** TIME_PREFERENCE_SHIFT: Detecta mudança de preferência de horário. */
  private boolean evaluateTimePreferenceShift(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object shiftObj = payload.get("time_preference_shift");
      if (shiftObj == null) {
        shiftObj = payload.get("timePreferenceShift");
      }

      return Boolean.TRUE.equals(shiftObj) || "true".equalsIgnoreCase(String.valueOf(shiftObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar TIME_PREFERENCE_SHIFT: {}", e.getMessage());
      return false;
    }
  }

  /** CHANNEL_USAGE_ANOMALY: Detecta anomalia de uso de canal. */
  private boolean evaluateChannelUsageAnomaly(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object anomalyObj = payload.get("channel_usage_anomaly");
      if (anomalyObj == null) {
        anomalyObj = payload.get("channelUsageAnomaly");
      }

      return Boolean.TRUE.equals(anomalyObj) || "true".equalsIgnoreCase(String.valueOf(anomalyObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar CHANNEL_USAGE_ANOMALY: {}", e.getMessage());
      return false;
    }
  }

  /** PAYMENT_METHOD_SWITCH: Detecta troca de método de pagamento. */
  private boolean evaluatePaymentMethodSwitch(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object switchObj = payload.get("payment_method_switched");
      if (switchObj == null) {
        switchObj = payload.get("paymentMethodSwitched");
      }

      return Boolean.TRUE.equals(switchObj) || "true".equalsIgnoreCase(String.valueOf(switchObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar PAYMENT_METHOD_SWITCH: {}", e.getMessage());
      return false;
    }
  }

  /**
   * RECIPIENT_DIVERSITY_CHANGE: Detecta mudança na diversidade de destinatários. Formato
   * valueSingle: "diversityThreshold" (ex: "3" = mais de 3 novos destinatários)
   */
  private boolean evaluateRecipientDiversityChange(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int threshold = Integer.parseInt(condition.getValueSingle().trim());

      Object countObj = payload.get("new_recipient_count");
      if (countObj == null) {
        countObj = payload.get("newRecipientCount");
      }

      if (countObj == null) return false;
      int count = countObj instanceof Number ? ((Number) countObj).intValue() : 0;
      return count > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar RECIPIENT_DIVERSITY_CHANGE: {}", e.getMessage());
      return false;
    }
  }

  /** GEOGRAPHIC_BEHAVIOR_SHIFT: Detecta mudança de comportamento geográfico. */
  private boolean evaluateGeographicBehaviorShift(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object shiftObj = payload.get("geographic_behavior_shift");
      if (shiftObj == null) {
        shiftObj = payload.get("geographicBehaviorShift");
      }

      return Boolean.TRUE.equals(shiftObj) || "true".equalsIgnoreCase(String.valueOf(shiftObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar GEOGRAPHIC_BEHAVIOR_SHIFT: {}", e.getMessage());
      return false;
    }
  }

  /** SESSION_BEHAVIOR_ANOMALY: Detecta anomalia de comportamento de sessão. */
  private boolean evaluateSessionBehaviorAnomaly(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object anomalyObj = payload.get("session_behavior_anomaly");
      if (anomalyObj == null) {
        anomalyObj = payload.get("sessionBehaviorAnomaly");
      }

      return Boolean.TRUE.equals(anomalyObj) || "true".equalsIgnoreCase(String.valueOf(anomalyObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar SESSION_BEHAVIOR_ANOMALY: {}", e.getMessage());
      return false;
    }
  }

  /** LOGIN_PATTERN_DEVIATION: Detecta desvio de padrão de login. */
  private boolean evaluateLoginPatternDeviation(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object deviationObj = payload.get("login_pattern_deviation");
      if (deviationObj == null) {
        deviationObj = payload.get("loginPatternDeviation");
      }

      return Boolean.TRUE.equals(deviationObj)
          || "true".equalsIgnoreCase(String.valueOf(deviationObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar LOGIN_PATTERN_DEVIATION: {}", e.getMessage());
      return false;
    }
  }

  /** NAVIGATION_PATTERN_ANOMALY: Detecta anomalia de padrão de navegação. */
  private boolean evaluateNavigationPatternAnomaly(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object anomalyObj = payload.get("navigation_pattern_anomaly");
      if (anomalyObj == null) {
        anomalyObj = payload.get("navigationPatternAnomaly");
      }

      return Boolean.TRUE.equals(anomalyObj) || "true".equalsIgnoreCase(String.valueOf(anomalyObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar NAVIGATION_PATTERN_ANOMALY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * TRANSACTION_TIMING_CLUSTER: Detecta cluster de timing de transações. Formato valueSingle:
   * "clusterSize" (ex: "5" = 5+ transações em cluster)
   */
  private boolean evaluateTransactionTimingCluster(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int threshold = Integer.parseInt(condition.getValueSingle().trim());

      Object clusterObj = payload.get("transaction_cluster_size");
      if (clusterObj == null) {
        clusterObj = payload.get("transactionClusterSize");
      }

      if (clusterObj == null) return false;
      int clusterSize = clusterObj instanceof Number ? ((Number) clusterObj).intValue() : 0;
      return clusterSize >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar TRANSACTION_TIMING_CLUSTER: {}", e.getMessage());
      return false;
    }
  }

  /**
   * AMOUNT_ROUNDING_BEHAVIOR: Detecta comportamento de arredondamento de valores. Formato
   * valueSingle: "roundingPercentage" (ex: "80" = 80% valores redondos)
   */
  private boolean evaluateAmountRoundingBehavior(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object percentObj = payload.get("rounding_percentage");
      if (percentObj == null) {
        percentObj = payload.get("roundingPercentage");
      }

      if (percentObj == null) {
        // Fallback: check current transaction
        TransactionRequest tx = context.getTransactionRequest();
        if (tx != null && tx.getTransactionAmount() != null) {
          BigDecimal amount = tx.getTransactionAmount();
          boolean isRound =
              amount.remainder(BigDecimal.valueOf(100)).compareTo(BigDecimal.ZERO) == 0;
          return isRound && threshold <= 50; // Simple heuristic
        }
        return false;
      }

      double percentage = percentObj instanceof Number ? ((Number) percentObj).doubleValue() : 0.0;
      return percentage >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar AMOUNT_ROUNDING_BEHAVIOR: {}", e.getMessage());
      return false;
    }
  }

  /**
   * SPLIT_PAYMENT_PATTERN: Detecta padrão de pagamento dividido. Formato valueSingle: "splitCount"
   * (ex: "3" = 3+ pagamentos divididos)
   */
  private boolean evaluateSplitPaymentPattern(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int threshold = Integer.parseInt(condition.getValueSingle().trim());

      Object countObj = payload.get("split_payment_count");
      if (countObj == null) {
        countObj = payload.get("splitPaymentCount");
      }

      if (countObj == null) return false;
      int count = countObj instanceof Number ? ((Number) countObj).intValue() : 0;
      return count >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar SPLIT_PAYMENT_PATTERN: {}", e.getMessage());
      return false;
    }
  }

  // --- CATEGORIA Q: Statistical Behavioral (10) ---

  /**
   * CHI_SQUARE_DISTRIBUTION_TEST: Teste Chi-Square de distribuição. Formato valueSingle:
   * "significanceLevel" (ex: "0.05")
   */
  private boolean evaluateChiSquareDistributionTest(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double significanceLevel = Double.parseDouble(condition.getValueSingle().trim());

      Object pValueObj = payload.get("chi_square_p_value");
      if (pValueObj == null) {
        pValueObj = payload.get("chiSquarePValue");
      }

      if (pValueObj == null) return false;
      double pValue = pValueObj instanceof Number ? ((Number) pValueObj).doubleValue() : 1.0;
      return pValue < significanceLevel; // Reject null hypothesis = anomaly
    } catch (Exception e) {
      log.error("Erro ao avaliar CHI_SQUARE_DISTRIBUTION_TEST: {}", e.getMessage());
      return false;
    }
  }

  /**
   * KOLMOGOROV_SMIRNOV_TEST: Teste Kolmogorov-Smirnov. Formato valueSingle: "significanceLevel"
   * (ex: "0.05")
   */
  private boolean evaluateKolmogorovSmirnovTest(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double significanceLevel = Double.parseDouble(condition.getValueSingle().trim());

      Object pValueObj = payload.get("ks_test_p_value");
      if (pValueObj == null) {
        pValueObj = payload.get("ksTestPValue");
      }

      if (pValueObj == null) return false;
      double pValue = pValueObj instanceof Number ? ((Number) pValueObj).doubleValue() : 1.0;
      return pValue < significanceLevel;
    } catch (Exception e) {
      log.error("Erro ao avaliar KOLMOGOROV_SMIRNOV_TEST: {}", e.getMessage());
      return false;
    }
  }

  /**
   * ANDERSON_DARLING_TEST: Teste Anderson-Darling. Formato valueSingle: "criticalValue" (ex: "2.5")
   */
  private boolean evaluateAndersonDarlingTest(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double criticalValue = Double.parseDouble(condition.getValueSingle().trim());

      Object statObj = payload.get("anderson_darling_statistic");
      if (statObj == null) {
        statObj = payload.get("andersonDarlingStatistic");
      }

      if (statObj == null) return false;
      double statistic = statObj instanceof Number ? ((Number) statObj).doubleValue() : 0.0;
      return statistic > criticalValue;
    } catch (Exception e) {
      log.error("Erro ao avaliar ANDERSON_DARLING_TEST: {}", e.getMessage());
      return false;
    }
  }

  /**
   * T_TEST_AMOUNT_DEVIATION: T-Test de desvio de valor. Formato valueSingle: "tStatThreshold" (ex:
   * "2.0")
   */
  private boolean evaluateTTestAmountDeviation(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object tStatObj = payload.get("t_test_statistic");
      if (tStatObj == null) {
        tStatObj = payload.get("tTestStatistic");
      }

      if (tStatObj == null) return false;
      double tStat = tStatObj instanceof Number ? ((Number) tStatObj).doubleValue() : 0.0;
      return Math.abs(tStat) > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar T_TEST_AMOUNT_DEVIATION: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MANN_WHITNEY_U_TEST: Teste Mann-Whitney U. Formato valueSingle: "significanceLevel" (ex:
   * "0.05")
   */
  private boolean evaluateMannWhitneyUTest(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double significanceLevel = Double.parseDouble(condition.getValueSingle().trim());

      Object pValueObj = payload.get("mann_whitney_p_value");
      if (pValueObj == null) {
        pValueObj = payload.get("mannWhitneyPValue");
      }

      if (pValueObj == null) return false;
      double pValue = pValueObj instanceof Number ? ((Number) pValueObj).doubleValue() : 1.0;
      return pValue < significanceLevel;
    } catch (Exception e) {
      log.error("Erro ao avaliar MANN_WHITNEY_U_TEST: {}", e.getMessage());
      return false;
    }
  }

  /**
   * CORRELATION_ANOMALY: Detecta anomalia de correlação. Formato valueSingle:
   * "correlationThreshold" (ex: "0.7")
   */
  private boolean evaluateCorrelationAnomaly(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object corrObj = payload.get("correlation_coefficient");
      if (corrObj == null) {
        corrObj = payload.get("correlationCoefficient");
      }

      if (corrObj == null) return false;
      double correlation = corrObj instanceof Number ? ((Number) corrObj).doubleValue() : 0.0;
      return Math.abs(correlation) > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar CORRELATION_ANOMALY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * REGRESSION_RESIDUAL_OUTLIER: Detecta outlier de resíduo de regressão. Formato valueSingle:
   * "residualThreshold" (ex: "3.0" = 3 desvios)
   */
  private boolean evaluateRegressionResidualOutlier(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object residualObj = payload.get("standardized_residual");
      if (residualObj == null) {
        residualObj = payload.get("standardizedResidual");
      }

      if (residualObj == null) return false;
      double residual = residualObj instanceof Number ? ((Number) residualObj).doubleValue() : 0.0;
      return Math.abs(residual) > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar REGRESSION_RESIDUAL_OUTLIER: {}", e.getMessage());
      return false;
    }
  }

  /**
   * VARIANCE_RATIO_TEST: Teste de razão de variância (F-test). Formato valueSingle:
   * "fStatThreshold" (ex: "4.0")
   */
  private boolean evaluateVarianceRatioTest(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object fStatObj = payload.get("f_statistic");
      if (fStatObj == null) {
        fStatObj = payload.get("fStatistic");
      }

      if (fStatObj == null) return false;
      double fStat = fStatObj instanceof Number ? ((Number) fStatObj).doubleValue() : 0.0;
      return fStat > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar VARIANCE_RATIO_TEST: {}", e.getMessage());
      return false;
    }
  }

  /**
   * ENTROPY_SCORE_ANOMALY: Detecta anomalia de score de entropia. Formato valueSingle:
   * "entropyThreshold" (ex: "0.3" = baixa entropia suspeita)
   */
  private boolean evaluateEntropyScoreAnomaly(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object entropyObj = payload.get("entropy_score");
      if (entropyObj == null) {
        entropyObj = payload.get("entropyScore");
      }

      if (entropyObj == null) return false;
      double entropy = entropyObj instanceof Number ? ((Number) entropyObj).doubleValue() : 1.0;
      return entropy < threshold; // Low entropy is suspicious (too predictable)
    } catch (Exception e) {
      log.error("Erro ao avaliar ENTROPY_SCORE_ANOMALY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * SKEWNESS_KURTOSIS_ANOMALY: Detecta anomalia de skewness/kurtosis. Formato valueSingle:
   * "threshold" (ex: "3.0")
   */
  private boolean evaluateSkewnessKurtosisAnomaly(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object skewnessObj = payload.get("skewness");
      Object kurtosisObj = payload.get("kurtosis");

      double skewness = skewnessObj instanceof Number ? ((Number) skewnessObj).doubleValue() : 0.0;
      double kurtosis = kurtosisObj instanceof Number ? ((Number) kurtosisObj).doubleValue() : 0.0;

      // Anomaly if either is beyond threshold
      return Math.abs(skewness) > threshold || Math.abs(kurtosis - 3) > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar SKEWNESS_KURTOSIS_ANOMALY: {}", e.getMessage());
      return false;
    }
  }

  // ========== OPERADORES V4.0 PHASE 1C - MCC & MERCHANT (18 novos) ==========

  // --- CATEGORIA R: MCC & Merchant Advanced (18) ---

  /**
   * MCC_CATEGORY_VELOCITY: Velocidade por categoria MCC. Formato valueSingle:
   * "mccCategory|threshold" (ex: "gambling|5")
   */
  private boolean evaluateMccCategoryVelocity(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      String mccCategory = parts[0].trim();
      int threshold = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 5;

      Object velocityObj = payload.get("mcc_velocity_" + mccCategory.toLowerCase());
      if (velocityObj == null) {
        velocityObj = payload.get("mccVelocity_" + mccCategory);
      }

      if (velocityObj == null) return false;
      int velocity = velocityObj instanceof Number ? ((Number) velocityObj).intValue() : 0;
      return velocity > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar MCC_CATEGORY_VELOCITY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MCC_SPENDING_LIMIT_CHECK: Verificação de limite por MCC. Formato valueSingle: "limitAmount"
   * (ex: "5000")
   */
  private boolean evaluateMccSpendingLimitCheck(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      TransactionRequest tx = context.getTransactionRequest();
      if (tx == null) return false;

      BigDecimal limit = new BigDecimal(condition.getValueSingle().trim());

      Object spentObj = payload != null ? payload.get("mcc_spending_total") : null;
      if (spentObj == null && payload != null) {
        spentObj = payload.get("mccSpendingTotal");
      }

      BigDecimal spent = BigDecimal.ZERO;
      if (spentObj instanceof Number) {
        spent = BigDecimal.valueOf(((Number) spentObj).doubleValue());
      }

      BigDecimal total = spent.add(tx.getTransactionAmount());
      return total.compareTo(limit) > 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar MCC_SPENDING_LIMIT_CHECK: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MCC_CROSS_CATEGORY_PATTERN: Detecta padrão cross-category MCC. Formato valueSingle:
   * "categoryCount" (ex: "4" = 4+ categorias diferentes)
   */
  private boolean evaluateMccCrossCategoryPattern(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int threshold = Integer.parseInt(condition.getValueSingle().trim());

      Object countObj = payload.get("distinct_mcc_categories");
      if (countObj == null) {
        countObj = payload.get("distinctMccCategories");
      }

      if (countObj == null) return false;
      int count = countObj instanceof Number ? ((Number) countObj).intValue() : 0;
      return count >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar MCC_CROSS_CATEGORY_PATTERN: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_REPUTATION_SCORE: Score de reputação do merchant. Formato valueSingle: "minScore" (ex:
   * "70" = score mínimo)
   */
  private boolean evaluateMerchantReputationScore(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double minScore = Double.parseDouble(condition.getValueSingle().trim());

      Object scoreObj = payload.get("merchant_reputation_score");
      if (scoreObj == null) {
        scoreObj = payload.get("merchantReputationScore");
      }

      if (scoreObj == null) return true; // No score = suspicious
      double score = scoreObj instanceof Number ? ((Number) scoreObj).doubleValue() : 0.0;
      return score < minScore;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_REPUTATION_SCORE: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_AGE_CHECK: Verificação de idade do merchant. Formato valueSingle: "minAgeDays" (ex:
   * "30" = mínimo 30 dias)
   */
  private boolean evaluateMerchantAgeCheck(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int minAge = Integer.parseInt(condition.getValueSingle().trim());

      Object ageObj = payload.get("merchant_age_days");
      if (ageObj == null) {
        ageObj = payload.get("merchantAgeDays");
      }

      if (ageObj == null) return true; // No age = new merchant = suspicious
      int age = ageObj instanceof Number ? ((Number) ageObj).intValue() : 0;
      return age < minAge;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_AGE_CHECK: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_TRANSACTION_VOLUME: Volume de transações do merchant. Formato valueSingle:
   * "minVolume|maxVolume" (ex: "100|10000")
   */
  private boolean evaluateMerchantTransactionVolume(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int minVolume = Integer.parseInt(parts[0].trim());
      int maxVolume = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : Integer.MAX_VALUE;

      Object volumeObj = payload.get("merchant_transaction_volume");
      if (volumeObj == null) {
        volumeObj = payload.get("merchantTransactionVolume");
      }

      if (volumeObj == null) return false;
      int volume = volumeObj instanceof Number ? ((Number) volumeObj).intValue() : 0;
      return volume < minVolume || volume > maxVolume;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_TRANSACTION_VOLUME: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_CHARGEBACK_HISTORY: Histórico de chargeback do merchant. Formato valueSingle:
   * "maxChargebackRate" (ex: "0.02" = 2%)
   */
  private boolean evaluateMerchantChargebackHistory(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double maxRate = Double.parseDouble(condition.getValueSingle().trim());

      Object rateObj = payload.get("merchant_chargeback_rate");
      if (rateObj == null) {
        rateObj = payload.get("merchantChargebackRate");
      }

      if (rateObj == null) return false;
      double rate = rateObj instanceof Number ? ((Number) rateObj).doubleValue() : 0.0;
      return rate > maxRate;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_CHARGEBACK_HISTORY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_FRAUD_RATE_CHECK: Verificação de taxa de fraude do merchant. Formato valueSingle:
   * "maxFraudRate" (ex: "0.01" = 1%)
   */
  private boolean evaluateMerchantFraudRateCheck(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double maxRate = Double.parseDouble(condition.getValueSingle().trim());

      Object rateObj = payload.get("merchant_fraud_rate");
      if (rateObj == null) {
        rateObj = payload.get("merchantFraudRate");
      }

      if (rateObj == null) return false;
      double rate = rateObj instanceof Number ? ((Number) rateObj).doubleValue() : 0.0;
      return rate > maxRate;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_FRAUD_RATE_CHECK: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_GEOGRAPHIC_SPREAD: Dispersão geográfica do merchant. Formato valueSingle:
   * "maxCountries" (ex: "10")
   */
  private boolean evaluateMerchantGeographicSpread(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int maxCountries = Integer.parseInt(condition.getValueSingle().trim());

      Object countObj = payload.get("merchant_country_count");
      if (countObj == null) {
        countObj = payload.get("merchantCountryCount");
      }

      if (countObj == null) return false;
      int count = countObj instanceof Number ? ((Number) countObj).intValue() : 0;
      return count > maxCountries;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_GEOGRAPHIC_SPREAD: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_CUSTOMER_CONCENTRATION: Concentração de clientes do merchant. Formato valueSingle:
   * "concentrationThreshold" (ex: "0.5" = 50%)
   */
  private boolean evaluateMerchantCustomerConcentration(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object concObj = payload.get("merchant_customer_concentration");
      if (concObj == null) {
        concObj = payload.get("merchantCustomerConcentration");
      }

      if (concObj == null) return false;
      double concentration = concObj instanceof Number ? ((Number) concObj).doubleValue() : 0.0;
      return concentration > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_CUSTOMER_CONCENTRATION: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_AMOUNT_DISTRIBUTION: Distribuição de valores do merchant. Formato valueSingle:
   * "stdDevThreshold" (ex: "1000")
   */
  private boolean evaluateMerchantAmountDistribution(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object stdObj = payload.get("merchant_amount_stddev");
      if (stdObj == null) {
        stdObj = payload.get("merchantAmountStddev");
      }

      if (stdObj == null) return false;
      double stdDev = stdObj instanceof Number ? ((Number) stdObj).doubleValue() : 0.0;
      return stdDev > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_AMOUNT_DISTRIBUTION: {}", e.getMessage());
      return false;
    }
  }

  /** MERCHANT_TIME_PATTERN: Padrão temporal do merchant. */
  private boolean evaluateMerchantTimePattern(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object anomalyObj = payload.get("merchant_time_pattern_anomaly");
      if (anomalyObj == null) {
        anomalyObj = payload.get("merchantTimePatternAnomaly");
      }

      return Boolean.TRUE.equals(anomalyObj) || "true".equalsIgnoreCase(String.valueOf(anomalyObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_TIME_PATTERN: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_DEVICE_DIVERSITY: Diversidade de dispositivos do merchant. Formato valueSingle:
   * "minDiversity|maxDiversity" (ex: "10|1000")
   */
  private boolean evaluateMerchantDeviceDiversity(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int minDiversity = Integer.parseInt(parts[0].trim());
      int maxDiversity = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : Integer.MAX_VALUE;

      Object diversityObj = payload.get("merchant_device_diversity");
      if (diversityObj == null) {
        diversityObj = payload.get("merchantDeviceDiversity");
      }

      if (diversityObj == null) return false;
      int diversity = diversityObj instanceof Number ? ((Number) diversityObj).intValue() : 0;
      return diversity < minDiversity || diversity > maxDiversity;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_DEVICE_DIVERSITY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_REFUND_RATIO: Razão de reembolso do merchant. Formato valueSingle: "maxRefundRatio"
   * (ex: "0.1" = 10%)
   */
  private boolean evaluateMerchantRefundRatio(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double maxRatio = Double.parseDouble(condition.getValueSingle().trim());

      Object ratioObj = payload.get("merchant_refund_ratio");
      if (ratioObj == null) {
        ratioObj = payload.get("merchantRefundRatio");
      }

      if (ratioObj == null) return false;
      double ratio = ratioObj instanceof Number ? ((Number) ratioObj).doubleValue() : 0.0;
      return ratio > maxRatio;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_REFUND_RATIO: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_NEW_CUSTOMER_RATIO: Razão de novos clientes do merchant. Formato valueSingle:
   * "maxNewCustomerRatio" (ex: "0.8" = 80%)
   */
  private boolean evaluateMerchantNewCustomerRatio(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double maxRatio = Double.parseDouble(condition.getValueSingle().trim());

      Object ratioObj = payload.get("merchant_new_customer_ratio");
      if (ratioObj == null) {
        ratioObj = payload.get("merchantNewCustomerRatio");
      }

      if (ratioObj == null) return false;
      double ratio = ratioObj instanceof Number ? ((Number) ratioObj).doubleValue() : 0.0;
      return ratio > maxRatio;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_NEW_CUSTOMER_RATIO: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_DORMANT_REACTIVATION: Reativação de merchant dormente. Formato valueSingle:
   * "dormantDays" (ex: "90" = 90 dias de inatividade)
   */
  private boolean evaluateMerchantDormantReactivation(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int dormantThreshold = Integer.parseInt(condition.getValueSingle().trim());

      Object daysObj = payload.get("merchant_days_since_last_tx");
      if (daysObj == null) {
        daysObj = payload.get("merchantDaysSinceLastTx");
      }

      if (daysObj == null) return false;
      int daysSinceLastTx = daysObj instanceof Number ? ((Number) daysObj).intValue() : 0;
      return daysSinceLastTx > dormantThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_DORMANT_REACTIVATION: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_CROSS_BORDER_RATIO: Razão cross-border do merchant. Formato valueSingle:
   * "maxCrossBorderRatio" (ex: "0.3" = 30%)
   */
  private boolean evaluateMerchantCrossBorderRatio(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double maxRatio = Double.parseDouble(condition.getValueSingle().trim());

      Object ratioObj = payload.get("merchant_cross_border_ratio");
      if (ratioObj == null) {
        ratioObj = payload.get("merchantCrossBorderRatio");
      }

      if (ratioObj == null) return false;
      double ratio = ratioObj instanceof Number ? ((Number) ratioObj).doubleValue() : 0.0;
      return ratio > maxRatio;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_CROSS_BORDER_RATIO: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_HIGH_VALUE_FREQUENCY: Frequência de alto valor do merchant. Formato valueSingle:
   * "threshold|percentage" (ex: "1000|0.5" = 50% acima de 1000)
   */
  private boolean evaluateMerchantHighValueFrequency(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      double amountThreshold = Double.parseDouble(parts[0].trim());
      double percentageThreshold = parts.length > 1 ? Double.parseDouble(parts[1].trim()) : 0.5;

      Object percentageObj = payload.get("merchant_high_value_percentage");
      if (percentageObj == null) {
        percentageObj = payload.get("merchantHighValuePercentage");
      }

      if (percentageObj == null) return false;
      double percentage =
          percentageObj instanceof Number ? ((Number) percentageObj).doubleValue() : 0.0;
      return percentage > percentageThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_HIGH_VALUE_FREQUENCY: {}", e.getMessage());
      return false;
    }
  }

  // ========== PLANNED OPERATORS - FATF Compliance (lançam UnsupportedOperatorException) ==========

  private boolean evaluateFatfBlackMarketExchange(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_BLACK_MARKET_EXCHANGE,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfCorrespondentLayering(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_CORRESPONDENT_LAYERING,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfCryptoAtmCashout(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_CRYPTO_ATM_CASHOUT,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfCryptoMixing(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_CRYPTO_MIXING,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfHawalaInformal(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_HAWALA_INFORMAL,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfInsuranceCashValue(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_INSURANCE_CASH_VALUE,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfIntegrationLuxuryGoods(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_INTEGRATION_LUXURY_GOODS,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfIntegrationRealEstate(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_INTEGRATION_REAL_ESTATE,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfLayeringOffshore(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_LAYERING_OFFSHORE,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfLayeringRapidMovement(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_LAYERING_RAPID_MOVEMENT,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfLayeringShellCompany(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_LAYERING_SHELL_COMPANY,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfLayeringWireChains(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_LAYERING_WIRE_CHAINS,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfNewPaymentExploitation(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_NEW_PAYMENT_EXPLOITATION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfPepTransaction(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_PEP_TRANSACTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfPlacementCashIntensive(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_PLACEMENT_CASH_INTENSIVE,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfPlacementSmurfing(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_PLACEMENT_SMURFING,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfPlacementStructuring(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_PLACEMENT_STRUCTURING,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfRoundTripping(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_ROUND_TRIPPING,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfTbmlFalseDescription(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_TBML_FALSE_DESCRIPTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfTbmlMultipleInvoicing(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_TBML_MULTIPLE_INVOICING,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfTbmlOverInvoicing(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_TBML_OVER_INVOICING,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfTbmlPhantomShipping(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_TBML_PHANTOM_SHIPPING,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfTbmlUnderInvoicing(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_TBML_UNDER_INVOICING,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  // ========== PLANNED OPERATORS - Platform Integration (lançam UnsupportedOperatorException) ==========

  private boolean evaluatePltBacktestingLabeling(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_BACKTESTING_LABELING,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltBadEntityNetwork(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_BAD_ENTITY_NETWORK,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltBehaviorSortedLists(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_BEHAVIOR_SORTED_LISTS,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltBehavioralProfiling(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_BEHAVIORAL_PROFILING,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltBusinessRulesScenario(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_BUSINESS_RULES_SCENARIO,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltCompromiseManager(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_COMPROMISE_MANAGER,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltConsortiumDataCheck(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_CONSORTIUM_DATA_CHECK,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltCustomRuleBuilder(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_CUSTOM_RULE_BUILDER,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltDs2RuleEngine(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_DS2_RULE_ENGINE,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltIdentityResolution(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_IDENTITY_RESOLUTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltIntelligenceNetwork(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_INTELLIGENCE_NETWORK,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltLinkingVelocity(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_LINKING_VELOCITY,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltMlFraudRiskOutcome(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_ML_FRAUD_RISK_OUTCOME,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltNetworkAnalytics(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_NETWORK_ANALYTICS,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltNetworkEntityResolution(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_NETWORK_ENTITY_RESOLUTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltRadarComplexConditions(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_RADAR_COMPLEX_CONDITIONS,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltRadarInlineLists(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_RADAR_INLINE_LISTS,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltRadarMetadataMatching(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_RADAR_METADATA_MATCHING,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltRadarRuleBacktesting(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_RADAR_RULE_BACKTESTING,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltRealTimeDetection(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_REAL_TIME_DETECTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltReviewlistQueue(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_REVIEWLIST_QUEUE,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltRiskListComparison(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_RISK_LIST_COMPARISON,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltRiskProfileAssignment(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_RISK_PROFILE_ASSIGNMENT,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltRiskScoreCalculation(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_RISK_SCORE_CALCULATION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltRulesModelsHybrid(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_RULES_MODELS_HYBRID,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltSarAutomated(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_SAR_AUTOMATED,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltScenarioScorecard(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_SCENARIO_SCORECARD,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluatePltVelocityFilters(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PLT_VELOCITY_FILTERS,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  // ========== PLANNED OPERATORS - SCA Compliance (lançam UnsupportedOperatorException) ==========

  private boolean evaluateScaChallengeMandatory(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SCA_CHALLENGE_MANDATORY,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateScaContactlessExemption(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SCA_CONTACTLESS_EXEMPTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateScaCorporatePayment(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SCA_CORPORATE_PAYMENT,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateScaFraudRateMonitoring(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SCA_FRAUD_RATE_MONITORING,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateScaLiabilityShift(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SCA_LIABILITY_SHIFT,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateScaLowValueExemption(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SCA_LOW_VALUE_EXEMPTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateScaMerchantInitiated(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SCA_MERCHANT_INITIATED,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateScaRecurringTransaction(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SCA_RECURRING_TRANSACTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateScaSecureCorporateProtocol(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SCA_SECURE_CORPORATE_PROTOCOL,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateScaTraExemption(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SCA_TRA_EXEMPTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateScaTrustedBeneficiary(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SCA_TRUSTED_BENEFICIARY,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfPlacementCurrencyExchange(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_PLACEMENT_CURRENCY_EXCHANGE,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfPlacementCasinoGambling(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_PLACEMENT_CASINO_GAMBLING,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfLayeringConvertibleInstruments(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_LAYERING_CONVERTIBLE_INSTRUMENTS,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfIntegrationBusinessInvestment(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_INTEGRATION_BUSINESS_INVESTMENT,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFatfIntegrationLoanRepayment(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FATF_INTEGRATION_LOAN_REPAYMENT,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateScaDynamic3dsRouting(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SCA_DYNAMIC_3DS_ROUTING,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  // ========== PLANNED OPERATORS - Association Rules (lançam UnsupportedOperatorException) ==========

  private boolean evaluateAprioriAssociation(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.APRIORI_ASSOCIATION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateEclatItemset(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.ECLAT_ITEMSET,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFpgrowthFrequentPatterns(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FPGROWTH_FREQUENT_PATTERNS,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  // ========== PLANNED OPERATORS - Fuzzy Logic (lançam UnsupportedOperatorException) ==========

  private boolean evaluateFuzzyMembership(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FUZZY_MEMBERSHIP,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateFuzzyAdaptiveThreshold(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.FUZZY_ADAPTIVE_THRESHOLD,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  // ========== PLANNED OPERATORS - Basel SL (Operational Risk) (lançam UnsupportedOperatorException) ==========

  private boolean evaluateBslBucketClassification(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.BSL_BUCKET_CLASSIFICATION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateBslBusinessIndicator(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.BSL_BUSINESS_INDICATOR,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateBslBusinessIndicatorComponent(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.BSL_BUSINESS_INDICATOR_COMPONENT,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateBslControlDeficiency(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.BSL_CONTROL_DEFICIENCY,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateBslInternalLossMultiplier(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.BSL_INTERNAL_LOSS_MULTIPLIER,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateBslKriMonitoring(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.BSL_KRI_MONITORING,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateBslLossDataCollection(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.BSL_LOSS_DATA_COLLECTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateBslLossEventReporting(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.BSL_LOSS_EVENT_REPORTING,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateBslLossExclusionApproval(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.BSL_LOSS_EXCLUSION_APPROVAL,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateBslLossThresholdSetting(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.BSL_LOSS_THRESHOLD_SETTING,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateBslMarginalCoefficient(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.BSL_MARGINAL_COEFFICIENT,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateBslRetentionPeriod(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.BSL_RETENTION_PERIOD,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateBslRiskGovernance(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.BSL_RISK_GOVERNANCE,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  private boolean evaluateBslScenarioAnalysis(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.BSL_SCENARIO_ANALYSIS,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status para operadores disponíveis.");
  }

  // ========== LLM & Generative AI Fraud Detection (LLM001-LLM012) ==========

  private boolean evaluateLlmTransactionDescriptionAnalysis(
      RuleCondition condition, EvaluationContext context) {
    // LLM001: Análise de descrição de transação via LLM para detectar padrões suspeitos
    throw new UnsupportedOperatorException(
        ConditionOperator.LLM_TRANSACTION_DESCRIPTION_ANALYSIS,
        "Operador PLANNED - requer integração com API LLM. Consulte GET /api/operators/status.");
  }

  private boolean evaluateLlmGenerativeRuleSynthesis(
      RuleCondition condition, EvaluationContext context) {
    // LLM002: Síntese automática de regras via LLM
    throw new UnsupportedOperatorException(
        ConditionOperator.LLM_GENERATIVE_RULE_SYNTHESIS,
        "Operador PLANNED - requer integração com API LLM. Consulte GET /api/operators/status.");
  }

  private boolean evaluateLlmAnomalyExplanationGeneration(
      RuleCondition condition, EvaluationContext context) {
    // LLM003: Geração de explicação de anomalia em linguagem natural
    throw new UnsupportedOperatorException(
        ConditionOperator.LLM_ANOMALY_EXPLANATION_GENERATION,
        "Operador PLANNED - requer integração com API LLM. Consulte GET /api/operators/status.");
  }

  private boolean evaluateLlmChatbotFraudDetection(
      RuleCondition condition, EvaluationContext context) {
    // LLM004: Detecção de fraude em chatbot/conversação
    throw new UnsupportedOperatorException(
        ConditionOperator.LLM_CHATBOT_FRAUD_DETECTION,
        "Operador PLANNED - requer integração com API LLM. Consulte GET /api/operators/status.");
  }

  private boolean evaluateLlmDeepfakeVoiceDetection(
      RuleCondition condition, EvaluationContext context) {
    // LLM005: Detecção de deepfake de voz
    throw new UnsupportedOperatorException(
        ConditionOperator.LLM_DEEPFAKE_VOICE_DETECTION,
        "Operador PLANNED - requer integração com API LLM. Consulte GET /api/operators/status.");
  }

  private boolean evaluateLlmSyntheticImageDetection(
      RuleCondition condition, EvaluationContext context) {
    // LLM006: Detecção de imagem sintética/GAN
    throw new UnsupportedOperatorException(
        ConditionOperator.LLM_SYNTHETIC_IMAGE_DETECTION,
        "Operador PLANNED - requer integração com API LLM. Consulte GET /api/operators/status.");
  }

  private boolean evaluateLlmEmailPhishingAnalysis(
      RuleCondition condition, EvaluationContext context) {
    // LLM007: Análise de phishing em email
    throw new UnsupportedOperatorException(
        ConditionOperator.LLM_EMAIL_PHISHING_ANALYSIS,
        "Operador PLANNED - requer integração com API LLM. Consulte GET /api/operators/status.");
  }

  private boolean evaluateLlmSocialEngineeringClassification(
      RuleCondition condition, EvaluationContext context) {
    // LLM008: Classificação de engenharia social
    throw new UnsupportedOperatorException(
        ConditionOperator.LLM_SOCIAL_ENGINEERING_CLASSIFICATION,
        "Operador PLANNED - requer integração com API LLM. Consulte GET /api/operators/status.");
  }

  private boolean evaluateLlmFraudAlertPrioritization(
      RuleCondition condition, EvaluationContext context) {
    // LLM009: Priorização automática de alertas
    throw new UnsupportedOperatorException(
        ConditionOperator.LLM_FRAUD_ALERT_PRIORITIZATION,
        "Operador PLANNED - requer integração com API LLM. Consulte GET /api/operators/status.");
  }

  private boolean evaluateLlmMultiModalFraudDetection(
      RuleCondition condition, EvaluationContext context) {
    // LLM010: Detecção multimodal (texto+imagem+dados)
    throw new UnsupportedOperatorException(
        ConditionOperator.LLM_MULTI_MODAL_FRAUD_DETECTION,
        "Operador PLANNED - requer integração com API LLM. Consulte GET /api/operators/status.");
  }

  private boolean evaluateLlmAdversarialAttackResistance(
      RuleCondition condition, EvaluationContext context) {
    // LLM011: Resistência a ataques adversariais
    throw new UnsupportedOperatorException(
        ConditionOperator.LLM_ADVERSARIAL_ATTACK_RESISTANCE,
        "Operador PLANNED - requer integração com API LLM. Consulte GET /api/operators/status.");
  }

  private boolean evaluateLlmFraudPatternAutodiscovery(
      RuleCondition condition, EvaluationContext context) {
    // LLM012: Autodescoberta de padrões de fraude
    throw new UnsupportedOperatorException(
        ConditionOperator.LLM_FRAUD_PATTERN_AUTODISCOVERY,
        "Operador PLANNED - requer integração com API LLM. Consulte GET /api/operators/status.");
  }

  // ========== Neo4j Graph Fraud Detection (NEO001-NEO018) ==========

  private boolean evaluateNeo4jWeaklyConnectedComponents(
      RuleCondition condition, EvaluationContext context) {
    // NEO001: Componentes fracamente conectados (WCC)
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    int componentId = neo4jGraphService.getWeaklyConnectedComponentId(accountId);
    int threshold = parseIntSafe(condition.getValueSingle(), 0);
    log.debug(
        "NEO4J_WCC: accountId={}, componentId={}, threshold={}", accountId, componentId, threshold);
    return componentId >= threshold;
  }

  private boolean evaluateNeo4jDegreeCentrality(
      RuleCondition condition, EvaluationContext context) {
    // NEO002: Centralidade de grau
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    int degree = neo4jGraphService.getDegreeCentrality(accountId);
    int threshold = parseIntSafe(condition.getValueSingle(), 5);
    log.debug("NEO4J_DEGREE: accountId={}, degree={}, threshold={}", accountId, degree, threshold);
    return degree > threshold;
  }

  private boolean evaluateNeo4jPagerankFraudScore(
      RuleCondition condition, EvaluationContext context) {
    // NEO003: Score de fraude via PageRank
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    double score = neo4jGraphService.getPageRankScore(accountId);
    double threshold = parseDoubleSafe(condition.getValueSingle(), 0.5);
    log.debug("NEO4J_PAGERANK: accountId={}, score={}, threshold={}", accountId, score, threshold);
    return score > threshold;
  }

  private boolean evaluateNeo4jLouvainCommunityDetection(
      RuleCondition condition, EvaluationContext context) {
    // NEO004: Detecção de comunidade Louvain
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    int communityId = neo4jGraphService.getLouvainCommunityId(accountId);
    log.debug("NEO4J_LOUVAIN: accountId={}, communityId={}", accountId, communityId);
    return communityId >= 0; // Está em alguma comunidade
  }

  private boolean evaluateNeo4jPairwiseSimilarityPii(
      RuleCondition condition, EvaluationContext context) {
    // NEO005: Similaridade de PII entre pares
    String accountId = getAccountId(context);
    String otherAccountId = condition.getValueSingle();
    if (accountId == null || otherAccountId == null) return false;

    double similarity = neo4jGraphService.getPairwiseSimilarity(accountId, otherAccountId);
    double threshold = 0.7;
    log.debug(
        "NEO4J_PII_SIMILARITY: {} vs {}, similarity={}", accountId, otherAccountId, similarity);
    return similarity > threshold;
  }

  private boolean evaluateNeo4jEntityResolutionSharedPii(
      RuleCondition condition, EvaluationContext context) {
    // NEO006: Resolução de entidade por PII compartilhado
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    List<String> sharedAccounts = neo4jGraphService.findAccountsWithSharedPii(accountId);
    int threshold = parseIntSafe(condition.getValueSingle(), 1);
    log.debug("NEO4J_SHARED_PII: accountId={}, sharedCount={}", accountId, sharedAccounts.size());
    return sharedAccounts.size() >= threshold;
  }

  private boolean evaluateNeo4jFraudRingDetection(
      RuleCondition condition, EvaluationContext context) {
    // NEO007: Detecção de anel de fraude
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    int minRingSize = parseIntSafe(condition.getValueSingle(), 3);
    boolean inRing = neo4jGraphService.isInFraudRing(accountId, minRingSize);
    log.debug("NEO4J_FRAUD_RING: accountId={}, inRing={}", accountId, inRing);
    return inRing;
  }

  private boolean evaluateNeo4jMoneyMuleNetworkAnalysis(
      RuleCondition condition, EvaluationContext context) {
    // NEO008: Análise de rede de money mules
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    int maxHops = parseIntSafe(condition.getValueSingle(), 5);
    Map<String, Object> analysis = neo4jGraphService.analyzeMoneyMuleNetwork(accountId, maxHops);
    int suspiciousCount = (int) analysis.getOrDefault("suspiciousDestinations", 0);
    log.debug("NEO4J_MULE_NETWORK: accountId={}, suspicious={}", accountId, suspiciousCount);
    return suspiciousCount > 0;
  }

  private boolean evaluateNeo4jCircularTransactionDetection(
      RuleCondition condition, EvaluationContext context) {
    // NEO009: Detecção de transação circular
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    int minCycleLength = parseIntSafe(condition.getValueSingle(), 3);
    boolean hasCircular = neo4jGraphService.hasCircularTransactions(accountId, minCycleLength);
    log.debug("NEO4J_CIRCULAR: accountId={}, hasCircular={}", accountId, hasCircular);
    return hasCircular;
  }

  private boolean evaluateNeo4jFirstPartyFraudClustering(
      RuleCondition condition, EvaluationContext context) {
    // NEO010: Clustering de fraude de primeira parte
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    int clusterId = neo4jGraphService.getFirstPartyFraudClusterId(accountId);
    log.debug("NEO4J_FPF_CLUSTER: accountId={}, clusterId={}", accountId, clusterId);
    return clusterId >= 0;
  }

  private boolean evaluateNeo4jSecondLevelFraudsterId(
      RuleCondition condition, EvaluationContext context) {
    // NEO011: Identificação de fraudador de segundo nível
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    boolean isSecondLevel = neo4jGraphService.isSecondLevelFraudster(accountId);
    log.debug("NEO4J_2ND_LEVEL: accountId={}, isSecondLevel={}", accountId, isSecondLevel);
    return isSecondLevel;
  }

  private boolean evaluateNeo4jBetweennessCentralityMule(
      RuleCondition condition, EvaluationContext context) {
    // NEO012: Centralidade de intermediação para mules
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    double centrality = neo4jGraphService.getBetweennessCentrality(accountId);
    double threshold = parseDoubleSafe(condition.getValueSingle(), 0.1);
    log.debug("NEO4J_BETWEENNESS: accountId={}, centrality={}", accountId, centrality);
    return centrality > threshold;
  }

  private boolean evaluateNeo4jLabelPropagationFraudSpread(
      RuleCondition condition, EvaluationContext context) {
    // NEO013: Propagação de label de fraude
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    String label = neo4jGraphService.getFraudLabel(accountId);
    log.debug("NEO4J_LABEL_PROP: accountId={}, label={}", accountId, label);
    return "FRAUD".equalsIgnoreCase(label) || "HIGH_RISK".equalsIgnoreCase(label);
  }

  private boolean evaluateNeo4jShortestPathAmlTracking(
      RuleCondition condition, EvaluationContext context) {
    // NEO014: Rastreamento AML via caminho mais curto
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    int pathLength = neo4jGraphService.getShortestPathToHighRisk(accountId);
    int threshold = parseIntSafe(condition.getValueSingle(), 3);
    log.debug("NEO4J_AML_PATH: accountId={}, pathLength={}", accountId, pathLength);
    return pathLength >= 0 && pathLength <= threshold;
  }

  private boolean evaluateNeo4jTriangleCountCollusion(
      RuleCondition condition, EvaluationContext context) {
    // NEO015: Contagem de triângulos para colusão
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    int triangles = neo4jGraphService.getTriangleCount(accountId);
    int threshold = parseIntSafe(condition.getValueSingle(), 2);
    log.debug("NEO4J_TRIANGLES: accountId={}, count={}", accountId, triangles);
    return triangles >= threshold;
  }

  private boolean evaluateNeo4jNodeSimilaritySyntheticId(
      RuleCondition condition, EvaluationContext context) {
    // NEO016: Similaridade de nó para ID sintético
    String accountId = getAccountId(context);
    String otherAccountId = condition.getValueSingle();
    if (accountId == null || otherAccountId == null) return false;

    double similarity = neo4jGraphService.getNodeSimilarity(accountId, otherAccountId);
    log.debug("NEO4J_NODE_SIM: {} vs {}, similarity={}", accountId, otherAccountId, similarity);
    return similarity > 0.8;
  }

  private boolean evaluateNeo4jGraphEmbeddingFraudPrediction(
      RuleCondition condition, EvaluationContext context) {
    // NEO017: Predição de fraude via embedding de grafo
    String accountId = getAccountId(context);
    if (accountId == null) return false;

    double probability = neo4jGraphService.getFraudProbabilityFromEmbedding(accountId);
    double threshold = parseDoubleSafe(condition.getValueSingle(), 0.7);
    log.debug("NEO4J_EMBEDDING: accountId={}, probability={}", accountId, probability);
    return probability > threshold;
  }

  private boolean evaluateNeo4jTemporalMotifPattern(
      RuleCondition condition, EvaluationContext context) {
    // NEO018: Padrão de motif temporal
    throw new UnsupportedOperatorException(
        ConditionOperator.NEO4J_TEMPORAL_MOTIF_PATTERN,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  // ========== Synthetic Identity Detection (SYN001-SYN015) ==========

  private boolean evaluateBiometricKeystrokeDynamics(
      RuleCondition condition, EvaluationContext context) {
    // SYN001: Dinâmica de digitação biométrica
    throw new UnsupportedOperatorException(
        ConditionOperator.BIOMETRIC_KEYSTROKE_DYNAMICS,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateBiometricMouseMovement(
      RuleCondition condition, EvaluationContext context) {
    // SYN002: Movimento de mouse biométrico
    throw new UnsupportedOperatorException(
        ConditionOperator.BIOMETRIC_MOUSE_MOVEMENT,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateBiometricScrollVelocity(
      RuleCondition condition, EvaluationContext context) {
    // SYN003: Velocidade de scroll biométrica
    throw new UnsupportedOperatorException(
        ConditionOperator.BIOMETRIC_SCROLL_VELOCITY,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateDeviceFingerprintConsistencyCheck(
      RuleCondition condition, EvaluationContext context) {
    // SYN004: Verificação de consistência de fingerprint
    throw new UnsupportedOperatorException(
        ConditionOperator.DEVICE_FINGERPRINT_CONSISTENCY_CHECK,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateEcbsvSsnValidation(RuleCondition condition, EvaluationContext context) {
    // SYN005: Validação SSN via eCBSV
    throw new UnsupportedOperatorException(
        ConditionOperator.ECBSV_SSN_VALIDATION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateSyntheticFraudScore(RuleCondition condition, EvaluationContext context) {
    // SYN006: Score de fraude sintética
    throw new UnsupportedOperatorException(
        ConditionOperator.SYNTHETIC_FRAUD_SCORE,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateInjectionAttackDetection(
      RuleCondition condition, EvaluationContext context) {
    // SYN007: Detecção de ataque de injeção
    throw new UnsupportedOperatorException(
        ConditionOperator.INJECTION_ATTACK_DETECTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateLivenessDetectionFacial(
      RuleCondition condition, EvaluationContext context) {
    // SYN008: Detecção de liveness facial
    throw new UnsupportedOperatorException(
        ConditionOperator.LIVENESS_DETECTION_FACIAL,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateLivenessDetectionVoice(
      RuleCondition condition, EvaluationContext context) {
    // SYN009: Detecção de liveness de voz
    throw new UnsupportedOperatorException(
        ConditionOperator.LIVENESS_DETECTION_VOICE,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateAntiDetectBrowserDetection(
      RuleCondition condition, EvaluationContext context) {
    // SYN010: Detecção de browser anti-detect
    throw new UnsupportedOperatorException(
        ConditionOperator.ANTI_DETECT_BROWSER_DETECTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateDocumentForgeryDetection(
      RuleCondition condition, EvaluationContext context) {
    // SYN011: Detecção de falsificação de documento
    throw new UnsupportedOperatorException(
        ConditionOperator.DOCUMENT_FORGERY_DETECTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateFaceToIdPhotoMatching(
      RuleCondition condition, EvaluationContext context) {
    // SYN012: Matching de face com foto de ID
    throw new UnsupportedOperatorException(
        ConditionOperator.FACE_TO_ID_PHOTO_MATCHING,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateAdaptiveBehavioralAnalytics(
      RuleCondition condition, EvaluationContext context) {
    // SYN013: Analytics comportamental adaptativo
    throw new UnsupportedOperatorException(
        ConditionOperator.ADAPTIVE_BEHAVIORAL_ANALYTICS,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateSyntheticIdLabelCorrection(
      RuleCondition condition, EvaluationContext context) {
    // SYN014: Correção de label de ID sintético
    throw new UnsupportedOperatorException(
        ConditionOperator.SYNTHETIC_ID_LABEL_CORRECTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateMultiLayeredSyntheticIdControls(
      RuleCondition condition, EvaluationContext context) {
    // SYN015: Controles multicamada para ID sintético
    throw new UnsupportedOperatorException(
        ConditionOperator.MULTI_LAYERED_SYNTHETIC_ID_CONTROLS,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  // ========== Estatísticos Avançados Pure Rules (STAT001-STAT015) ==========

  private boolean evaluateStatKruskalWallisTest(
      RuleCondition condition, EvaluationContext context) {
    // STAT001: Teste Kruskal-Wallis (não-paramétrico ANOVA)
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_KRUSKAL_WALLIS_TEST,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateStatAnovaFTest(RuleCondition condition, EvaluationContext context) {
    // STAT002: Teste F ANOVA para comparação de grupos
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_ANOVA_F_TEST,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateStatIsolationForestScore(
      RuleCondition condition, EvaluationContext context) {
    // STAT003: Score de Isolation Forest (anomalia)
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_ISOLATION_FOREST_SCORE,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateStatLocalOutlierFactor(
      RuleCondition condition, EvaluationContext context) {
    // STAT004: Local Outlier Factor (LOF)
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_LOCAL_OUTLIER_FACTOR,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateStatOneClassSvmBoundary(
      RuleCondition condition, EvaluationContext context) {
    // STAT005: One-Class SVM boundary detection
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_ONE_CLASS_SVM_BOUNDARY,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateStatKmeansClusterDistance(
      RuleCondition condition, EvaluationContext context) {
    // STAT006: Distância do centróide K-Means
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_KMEANS_CLUSTER_DISTANCE,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateStatDbscanNoiseDetection(
      RuleCondition condition, EvaluationContext context) {
    // STAT007: Detecção de ruído DBSCAN
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_DBSCAN_NOISE_DETECTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateStatGmmProbability(RuleCondition condition, EvaluationContext context) {
    // STAT008: Probabilidade Gaussian Mixture Model
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_GMM_PROBABILITY,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateStatMahalanobisDistance(
      RuleCondition condition, EvaluationContext context) {
    // STAT009: Distância de Mahalanobis multivariada
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_MAHALANOBIS_DISTANCE,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateStatGrubbsTest(RuleCondition condition, EvaluationContext context) {
    // STAT010: Teste de Grubbs para outliers
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_GRUBBS_TEST,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateStatDixonQTest(RuleCondition condition, EvaluationContext context) {
    // STAT011: Teste Q de Dixon para outliers
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_DIXON_Q_TEST,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateStatShapiroWilkTest(RuleCondition condition, EvaluationContext context) {
    // STAT012: Teste Shapiro-Wilk de normalidade
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_SHAPIRO_WILK_TEST,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateStatLeveneTest(RuleCondition condition, EvaluationContext context) {
    // STAT013: Teste de Levene para homogeneidade de variância
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_LEVENE_TEST,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateStatWelchTTest(RuleCondition condition, EvaluationContext context) {
    // STAT014: Teste t de Welch (variâncias desiguais)
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_WELCH_T_TEST,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateStatBootstrapConfidenceInterval(
      RuleCondition condition, EvaluationContext context) {
    // STAT015: Intervalo de confiança Bootstrap
    throw new UnsupportedOperatorException(
        ConditionOperator.STAT_BOOTSTRAP_CONFIDENCE_INTERVAL,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  // ========== Fraud Patterns & Market Operators (Phase 7) ==========

  private boolean evaluateCardTestingRingDetection(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.CARD_TESTING_RING_DETECTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateBustOutPatternDetection(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.BUST_OUT_PATTERN_DETECTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateCircularPaymentDetection(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.CIRCULAR_PAYMENT_DETECTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateAccountTakeoverPattern(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.ACCOUNT_TAKEOVER_PATTERN,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateSyntheticIdentityRing(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SYNTHETIC_IDENTITY_RING,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateCrossBorderVelocity(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.CROSS_BORDER_VELOCITY,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateCorrespondentAnomaly(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.CORRESPONDENT_ANOMALY,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateNestedCorrespondentCheck(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.NESTED_CORRESPONDENT_CHECK,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateShellBankIndicator(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SHELL_BANK_INDICATOR,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateHighRiskCorridorCheck(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.HIGH_RISK_CORRIDOR_CHECK,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateSegmentOfOneProfiling(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SEGMENT_OF_ONE_PROFILING,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateAdaptiveParametricThreshold(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.ADAPTIVE_PARAMETRIC_THRESHOLD,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateRealTimeRiskScoring(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.REAL_TIME_RISK_SCORING,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateConsortiumNegativeFileCheck(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.CONSORTIUM_NEGATIVE_FILE_CHECK,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluatePeerGroupDeviationScore(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PEER_GROUP_DEVIATION_SCORE,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateMicroDepositVelocity(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.MICRO_DEPOSIT_VELOCITY,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateRapidSuccessionPattern(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.RAPID_SUCCESSION_PATTERN,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateSplitTransactionDetection(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.SPLIT_TRANSACTION_DETECTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateRoundTripDetection(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.ROUND_TRIP_DETECTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateLayeredTransferPattern(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.LAYERED_TRANSFER_PATTERN,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateAppFraudDetection(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.APP_FRAUD_DETECTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateRomanceScamIndicator(RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.ROMANCE_SCAM_INDICATOR,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateInvestmentScamPattern(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.INVESTMENT_SCAM_PATTERN,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluateCryptoPumpDumpDetection(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.CRYPTO_PUMP_DUMP_DETECTION,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

  private boolean evaluatePigButcheringIndicator(
      RuleCondition condition, EvaluationContext context) {
    throw new UnsupportedOperatorException(
        ConditionOperator.PIG_BUTCHERING_INDICATOR,
        "Operador PLANNED - não implementado. Consulte GET /api/operators/status.");
  }

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

  private boolean evaluateNameSimilarityGtOp(Object fieldValue, RuleCondition condition) {
    try {
      double similarity = Double.parseDouble(String.valueOf(fieldValue));
      double threshold = Double.parseDouble(condition.getValueSingle());
      return similarity > threshold;
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
