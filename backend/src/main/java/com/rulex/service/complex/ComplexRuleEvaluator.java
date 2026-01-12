package com.rulex.service.complex;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.RuleConditionGroup;
import com.rulex.entity.complex.RuleExecutionDetail;
import com.rulex.service.GeoService;
import com.rulex.service.VelocityService;
import com.rulex.service.VelocityServiceFacade;
import com.rulex.util.RegexValidator;
import java.math.BigDecimal;
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

      case HAS_INCOMING_TRANSFER_LAST_N_HOURS -> evaluateHasIncomingTransferLastNHours(condition, context);

      case IS_IMPOSSIBLE_COMBINATION -> evaluateIsImpossibleCombination(condition, context);

      case PIX_KEY_CHANGED_LAST_N_DAYS -> evaluatePixKeyChangedLastNDays(condition, context);

      case CONTAINS_SUSPICIOUS_KEYWORDS -> evaluateContainsSuspiciousKeywords(fieldValue, condition);

      case COUNT_CRYPTO_TXN_LAST_N_DAYS -> evaluateCountCryptoTxnLastNDays(condition, context);

      case COUNT_DISTINCT_INSTRUMENTS_LAST_N_DAYS -> evaluateCountDistinctInstrumentsLastNDays(condition, context);

      case COUNT_DISTINCT_PAYERS_LAST_N_DAYS -> evaluateCountDistinctPayersLastNDays(condition, context);

      case COUNT_DISTINCT_USER_AGENTS_LAST_N_HOURS -> evaluateCountDistinctUserAgentsLastNHours(condition, context);

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
      case COUNT_UNIQUE_BENEFICIARIES_LAST_N_DAYS -> evaluateCountUniqueBeneficiariesLastNDays(condition, context);
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
      case SCA_EXEMPTION_TRUSTED_BENEFICIARY -> evaluateScaExemptionTrustedBeneficiary(condition, context);
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
      case TRANSACTION_COUNT_PER_CARD_HOUR -> evaluateTransactionCountPerCardHour(condition, context);
      case TRANSACTION_COUNT_PER_IP_HOUR -> evaluateTransactionCountPerIpHour(condition, context);
      case TRANSACTION_COUNT_PER_DEVICE_DAY -> evaluateTransactionCountPerDeviceDay(condition, context);
      case TRANSACTION_COUNT_PER_MERCHANT_HOUR -> evaluateTransactionCountPerMerchantHour(condition, context);
      case TRANSACTION_COUNT_PER_CUSTOMER_HOUR -> evaluateTransactionCountPerCustomerHour(condition, context);
      case UNIQUE_CARD_COUNT_PER_IP_HOUR -> evaluateUniqueCardCountPerIpHour(condition, context);
      case UNIQUE_MERCHANT_COUNT_PER_CARD_DAY -> evaluateUniqueMerchantCountPerCardDay(condition, context);
      case TRANSACTION_ATTEMPT_COUNT_PER_CARD -> evaluateTransactionAttemptCountPerCard(condition, context);
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
      case MERCHANT_CUSTOMER_CONCENTRATION -> evaluateMerchantCustomerConcentration(condition, context);
      case MERCHANT_AMOUNT_DISTRIBUTION -> evaluateMerchantAmountDistribution(condition, context);
      case MERCHANT_TIME_PATTERN -> evaluateMerchantTimePattern(condition, context);
      case MERCHANT_DEVICE_DIVERSITY -> evaluateMerchantDeviceDiversity(condition, context);
      case MERCHANT_REFUND_RATIO -> evaluateMerchantRefundRatio(condition, context);
      case MERCHANT_NEW_CUSTOMER_RATIO -> evaluateMerchantNewCustomerRatio(condition, context);
      case MERCHANT_DORMANT_REACTIVATION -> evaluateMerchantDormantReactivation(condition, context);
      case MERCHANT_CROSS_BORDER_RATIO -> evaluateMerchantCrossBorderRatio(condition, context);
      case MERCHANT_HIGH_VALUE_FREQUENCY -> evaluateMerchantHighValueFrequency(condition, context);

      default -> {
        log.warn("Operador não implementado: {}", operator);
        yield false;
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

  /**
   * Avalia IN_LIST com suporte a valueArray ou valueSingle com delimitador pipe.
   */
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

      String sourceField = parts[0].trim();
      String targetField = parts[1].trim();
      int days = Integer.parseInt(parts[2].trim());

      Object sourceValue = getFieldValue(sourceField, null, context);
      Object targetValue = getFieldValue(targetField, null, context);

      if (sourceValue == null || targetValue == null) {
        return false;
      }

      // Consulta ao VelocityService para verificar histórico
      // Por enquanto, retorna true (beneficiário novo) se não houver dados históricos
      // TODO: Implementar consulta real ao histórico de transações
      log.debug("NOT_IN_HISTORICAL: source={}, target={}, days={}", sourceValue, targetValue, days);
      return true; // Placeholder - assumir novo beneficiário
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

      // TODO: Implementar consulta ao último depósito/crédito do cliente
      // Por enquanto, usa um valor placeholder
      BigDecimal lastIncoming = BigDecimal.valueOf(1000); // Placeholder

      BigDecimal threshold =
          lastIncoming.multiply(BigDecimal.valueOf(percentage)).divide(BigDecimal.valueOf(100));
      return currentAmount.compareTo(threshold) >= 0;
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

      Object merchantId = getFieldValue(condition.getFieldName(), null, context);
      if (merchantId == null) {
        return false;
      }

      // TODO: Implementar consulta real à taxa de chargeback do merchant
      // Por enquanto, retorna false (merchant OK)
      log.debug(
          "CHARGEBACK_RATE_GT: merchantId={}, threshold={}%, days={}",
          merchantId, rateThreshold, days);
      return false; // Placeholder
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
      int minutes = Integer.parseInt(condition.getValueSingle().trim());

      // Verificar se há campo de data de criação da conta no contexto
      Object createdAt = getFieldValue("accountCreatedAt", null, context);
      if (createdAt == null) {
        createdAt = getFieldValue("customerCreatedAt", null, context);
      }

      if (createdAt == null) {
        // Se não há informação de idade da conta, assumir conta antiga (segura)
        return false;
      }

      // TODO: Implementar parsing da data e cálculo de idade
      // Por enquanto, retorna false (conta antiga)
      log.debug("ACCOUNT_AGE_LT_MINUTES: minutes={}, createdAt={}", minutes, createdAt);
      return false; // Placeholder
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

    // Lista de prefixos conhecidos de VoIP no Brasil
    String[] voipPrefixes = {"0800", "0300", "0500", "0900"};
    for (String prefix : voipPrefixes) {
      if (phone.startsWith(prefix)) {
        return true;
      }
    }

    // TODO: Integrar com serviço externo de verificação de VoIP (ex: Twilio Lookup)
    // Por enquanto, usa heurística básica
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

  /** IS_HOLIDAY: Verifica se a transação é em feriado (placeholder) */
  private boolean evaluateIsHoliday(EvaluationContext context) {
    // TODO: Implementar consulta a tabela de feriados
    // Por enquanto, retorna false
    return false;
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

      // TODO: Implementar consulta real a falhas
      // Por enquanto, usa contagem de transações suspeitas como proxy
      VelocityService.TimeWindow window = parseTimeWindowFromHours(hours);

      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats =
            velocityServiceFacade.getStats(
                context.getTransactionRequest(), VelocityService.KeyType.PAN, window);
        // Usar suspicious_count como proxy para falhas
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
      int minutes = Integer.parseInt(condition.getValueSingle().trim());

      // TODO: Implementar consulta real ao timestamp da última transação
      // Por enquanto, verifica se há muitas transações na janela de tempo
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
    // TODO: Implementar detecção de padrão de escada
    // Requer análise de histórico de transações recentes
    // Por enquanto, retorna false
    log.debug("PATTERN_ESCALATION não implementado completamente");
    return false;
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
      Object merchantId = getFieldValue("merchantId", null, context);
      if (merchantId == null) {
        return false;
      }

      // TODO: Implementar consulta real à tabela de chargebacks
      // Por enquanto, retorna false (merchant não tem chargeback)
      log.debug("IN_CUSTOMER_CHARGEBACK_MERCHANTS: merchantId={}", merchantId);
      return false;
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
   * Helper seguro para parsing de inteiros.
   * Evita NumberFormatException retornando valor default.
   */
  private int parseIntSafe(String value, int defaultValue) {
    if (value == null || value.isBlank()) {
      return defaultValue;
    }
    try {
      return Integer.parseInt(value.trim());
    } catch (NumberFormatException e) {
      log.warn("Valor inválido para parse int: '{}', usando default: {}", 
          maskSensitiveData(value), defaultValue);
      return defaultValue;
    }
  }

  /**
   * Helper seguro para parsing de doubles.
   */
  private double parseDoubleSafe(String value, double defaultValue) {
    if (value == null || value.isBlank()) {
      return defaultValue;
    }
    try {
      return Double.parseDouble(value.trim());
    } catch (NumberFormatException e) {
      log.warn("Valor inválido para parse double: '{}', usando default: {}", 
          maskSensitiveData(value), defaultValue);
      return defaultValue;
    }
  }

  /**
   * Mascara dados sensíveis para logging seguro.
   */
  private String maskSensitiveData(String data) {
    if (data == null || data.length() < 4) {
      return "****";
    }
    return "****" + data.substring(Math.max(0, data.length() - 4));
  }

  /**
   * HAS_FAILED_3DS_LAST_N_MINUTES: Verifica se houve falha 3DS nos últimos N minutos.
   * Formato valueSingle: "minutes" (ex: "30")
   */
  private boolean evaluateHasFailed3dsLastNMinutes(RuleCondition condition, EvaluationContext context) {
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
   * COUNT_MFA_ABANDONMENTS: Contagem de abandonos de MFA.
   * Formato valueSingle: "threshold:hours" (ex: "3:24")
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
   * HAS_INCOMING_TRANSFER_LAST_N_HOURS: Verifica se houve transferência de entrada.
   * Formato valueSingle: "hours" (ex: "24")
   */
  private boolean evaluateHasIncomingTransferLastNHours(RuleCondition condition, EvaluationContext context) {
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
   * IS_IMPOSSIBLE_COMBINATION: Verifica combinações impossíveis de dados.
   * Formato valueSingle: tipo de combinação (ex: "age_corporate", "country_currency")
   */
  private boolean evaluateIsImpossibleCombination(RuleCondition condition, EvaluationContext context) {
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

  /**
   * Verifica se a moeda é válida para o país.
   */
  private boolean isValidCurrencyForCountry(String country, String currency) {
    if (country == null || currency == null) return true;
    
    Map<String, List<String>> validCurrencies = Map.of(
      "BR", List.of("BRL"),
      "US", List.of("USD"),
      "GB", List.of("GBP"),
      "DE", List.of("EUR"),
      "FR", List.of("EUR"),
      "IT", List.of("EUR"),
      "ES", List.of("EUR"),
      "PT", List.of("EUR"),
      "JP", List.of("JPY"),
      "CN", List.of("CNY", "CNH")
    );
    
    List<String> valid = validCurrencies.get(country.toUpperCase());
    return valid == null || valid.contains(currency.toUpperCase());
  }

  /**
   * PIX_KEY_CHANGED_LAST_N_DAYS: Verifica se chave PIX foi alterada recentemente.
   * Formato valueSingle: "days" (ex: "7")
   */
  private boolean evaluatePixKeyChangedLastNDays(RuleCondition condition, EvaluationContext context) {
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
   * CONTAINS_SUSPICIOUS_KEYWORDS: Detecta palavras suspeitas em texto.
   * Formato valueArray: lista de keywords, ou usa lista padrão
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
        keywords = List.of(
          "urgente", "transferir agora", "bloqueio", "seguranca",
          "atualizar dados", "conta suspensa", "premio", "heranca",
          "emprestimo aprovado", "divida", "spc", "serasa",
          "banco central", "pix devolvido", "comprovante falso"
        );
      }

      return keywords.stream()
          .map(String::toLowerCase)
          .map(String::trim)
          .anyMatch(text::contains);
    } catch (Exception e) {
      log.error("Erro ao avaliar CONTAINS_SUSPICIOUS_KEYWORDS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * COUNT_CRYPTO_TXN_LAST_N_DAYS: Conta transações crypto nos últimos N dias.
   * Formato valueSingle: "threshold|days" (ex: "5|30")
   */
  private boolean evaluateCountCryptoTxnLastNDays(RuleCondition condition, EvaluationContext context) {
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
   * COUNT_DISTINCT_INSTRUMENTS_LAST_N_DAYS: Instrumentos distintos nos últimos N dias.
   * Formato valueSingle: "threshold|days" (ex: "10|30")
   */
  private boolean evaluateCountDistinctInstrumentsLastNDays(RuleCondition condition, EvaluationContext context) {
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
   * COUNT_DISTINCT_PAYERS_LAST_N_DAYS: Pagadores distintos nos últimos N dias.
   * Formato valueSingle: "threshold|days" (ex: "5|7")
   */
  private boolean evaluateCountDistinctPayersLastNDays(RuleCondition condition, EvaluationContext context) {
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
   * COUNT_DISTINCT_USER_AGENTS_LAST_N_HOURS: User agents distintos nas últimas N horas.
   * Formato valueSingle: "threshold|hours" (ex: "5|24")
   */
  private boolean evaluateCountDistinctUserAgentsLastNHours(RuleCondition condition, EvaluationContext context) {
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
   * COUNT_LAST_N_DAYS: Contagem genérica nos últimos N dias.
   * Formato valueSingle: "threshold|days" ou "keyType|threshold|days"
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
        VelocityService.VelocityStats stats = velocityServiceFacade.getStats(
            context.getTransactionRequest(), kt, window);
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

  /**
   * Converte string para KeyType.
   */
  private VelocityService.KeyType parseKeyType(String keyType) {
    try {
      return VelocityService.KeyType.valueOf(keyType.toUpperCase());
    } catch (IllegalArgumentException e) {
      return VelocityService.KeyType.PAN;
    }
  }

  /**
   * COUNT_MFA_DENIALS_LAST_N_HOURS: Negações de MFA nas últimas N horas.
   * Formato valueSingle: "threshold:hours" (ex: "3:24")
   */
  private boolean evaluateCountMfaDenialsLastNHours(RuleCondition condition, EvaluationContext context) {
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
   * DAYS_SINCE_LAST_ACTIVITY: Dias desde última atividade.
   * Formato valueSingle: "threshold|operator" (ex: "30|GT" = mais de 30 dias)
   */
  private boolean evaluateDaysSinceLastActivity(RuleCondition condition, EvaluationContext context) {
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

  /**
   * DEVICE_CHANGED_IN_SESSION: Verifica se device mudou durante a sessão.
   */
  private boolean evaluateDeviceChangedInSession(RuleCondition condition, EvaluationContext context) {
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
   * IS_CRYPTO_RANSOM_AMOUNT: Detecta valores típicos de ransomware.
   * Verifica se o valor está próximo de quantias típicas de ransom.
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
      List<BigDecimal> typicalRansomAmounts = List.of(
          new BigDecimal("500"),       // ~0.001 BTC
          new BigDecimal("1000"),      // ~0.003 BTC
          new BigDecimal("2500"),      // ~0.007 BTC
          new BigDecimal("5000"),      // ~0.015 BTC
          new BigDecimal("10000"),     // ~0.03 BTC
          new BigDecimal("15000"),     // ~0.05 BTC
          new BigDecimal("25000"),     // ~0.08 BTC
          new BigDecimal("35000"),     // ~0.1 BTC
          new BigDecimal("50000"),     // ~0.15 BTC
          new BigDecimal("75000"),     // ~0.25 BTC
          new BigDecimal("100000"),    // ~0.3 BTC
          new BigDecimal("150000"),    // ~0.5 BTC
          new BigDecimal("175000"),    // ~0.5 BTC
          new BigDecimal("200000"),    // ~0.6 BTC
          new BigDecimal("350000"),    // ~1 BTC
          new BigDecimal("500000")     // ~1.5 BTC
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
   * OUTFLOW_RATE_LAST_N_DAYS: Taxa de saída nos últimos N dias.
   * Formato valueSingle: "threshold|days" (threshold em percentual)
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

  /**
   * Converte objeto para BigDecimal de forma segura.
   */
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

  // ========== OPERADORES V31+ (82 stubs) - CATEGORIAS A-K ==========

  // --- CATEGORIA A: Velocity Avançado (10) ---
  private boolean evaluateVelocityCrossChannel(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ VELOCITY_CROSS_CHANNEL: implementação pendente");
    return false;
  }

  private boolean evaluateVelocityRollingWindow(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ VELOCITY_ROLLING_WINDOW: implementação pendente");
    return false;
  }

  private boolean evaluateVelocityPercentile(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ VELOCITY_PERCENTILE: implementação pendente");
    return false;
  }

  private boolean evaluateVelocityRatioGt(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ VELOCITY_RATIO_GT: implementação pendente");
    return false;
  }

  private boolean evaluateVelocityTrend(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ VELOCITY_TREND: implementação pendente");
    return false;
  }

  private boolean evaluateCountUniqueBeneficiariesLastNDays(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ COUNT_UNIQUE_BENEFICIARIES_LAST_N_DAYS: implementação pendente");
    return false;
  }

  private boolean evaluateCountUniqueIpsLastNHours(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ COUNT_UNIQUE_IPS_LAST_N_HOURS: implementação pendente");
    return false;
  }

  private boolean evaluateSumByChannelLastNDays(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ SUM_BY_CHANNEL_LAST_N_DAYS: implementação pendente");
    return false;
  }

  private boolean evaluateAvgIntervalBetweenTxn(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ AVG_INTERVAL_BETWEEN_TXN: implementação pendente");
    return false;
  }

  private boolean evaluateVelocityAcceleration(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ VELOCITY_ACCELERATION: implementação pendente");
    return false;
  }

  // --- CATEGORIA B: Behavioral Rules (8) ---
  private boolean evaluateDormancyRevival(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ DORMANCY_REVIVAL: implementação pendente");
    return false;
  }

  private boolean evaluateAmountDeviationFromAvg(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ AMOUNT_DEVIATION_FROM_AVG: implementação pendente");
    return false;
  }

  private boolean evaluateTimeDeviationFromUsual(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ TIME_DEVIATION_FROM_USUAL: implementação pendente");
    return false;
  }

  private boolean evaluateMerchantDeviation(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ MERCHANT_DEVIATION: implementação pendente");
    return false;
  }

  private boolean evaluateMicroTransactionTest(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ MICRO_TRANSACTION_TEST: implementação pendente");
    return false;
  }

  private boolean evaluateLocationDeviation(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ LOCATION_DEVIATION: implementação pendente");
    return false;
  }

  private boolean evaluateChannelSwitchPattern(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ CHANNEL_SWITCH_PATTERN: implementação pendente");
    return false;
  }

  private boolean evaluateBeneficiaryReusePattern(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ BENEFICIARY_REUSE_PATTERN: implementação pendente");
    return false;
  }

  // --- CATEGORIA C: Graph/Network (8) ---
  private boolean evaluateFanOutCount(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ FAN_OUT_COUNT: implementação pendente");
    return false;
  }

  private boolean evaluateFanInCount(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ FAN_IN_COUNT: implementação pendente");
    return false;
  }

  private boolean evaluateSharedDeviceCount(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ SHARED_DEVICE_COUNT: implementação pendente");
    return false;
  }

  private boolean evaluateSharedIpCount(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ SHARED_IP_COUNT: implementação pendente");
    return false;
  }

  private boolean evaluateAccountLinkDepth(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ ACCOUNT_LINK_DEPTH: implementação pendente");
    return false;
  }

  private boolean evaluateCircularTransferDetection(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ CIRCULAR_TRANSFER_DETECTION: implementação pendente");
    return false;
  }

  private boolean evaluateRapidMultiHop(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ RAPID_MULTI_HOP: implementação pendente");
    return false;
  }

  private boolean evaluateBeneficiaryConcentration(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ BENEFICIARY_CONCENTRATION: implementação pendente");
    return false;
  }

  // --- CATEGORIA D: Sanctions & Name Matching (7) ---
  private boolean evaluateOfacListCheck(Object fieldValue, RuleCondition condition) {
    log.warn("Operador V31+ OFAC_LIST_CHECK: implementação pendente");
    return false;
  }

  private boolean evaluatePepListCheck(Object fieldValue, RuleCondition condition) {
    log.warn("Operador V31+ PEP_LIST_CHECK: implementação pendente");
    return false;
  }

  private boolean evaluateAdverseMediaCheck(Object fieldValue, RuleCondition condition) {
    log.warn("Operador V31+ ADVERSE_MEDIA_CHECK: implementação pendente");
    return false;
  }

  private boolean evaluateSanctionsCountryCheck(Object fieldValue, RuleCondition condition) {
    log.warn("Operador V31+ SANCTIONS_COUNTRY_CHECK: implementação pendente");
    return false;
  }

  private boolean evaluateHighRiskJurisdiction(Object fieldValue, RuleCondition condition) {
    log.warn("Operador V31+ HIGH_RISK_JURISDICTION: implementação pendente");
    return false;
  }

  private boolean evaluateNameTransliterationMatch(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ NAME_TRANSLITERATION_MATCH: implementação pendente");
    return false;
  }

  private boolean evaluateAliasDetection(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ ALIAS_DETECTION: implementação pendente");
    return false;
  }

  // --- CATEGORIA E: Synthetic ID Detection (8) ---
  private boolean evaluateCpfSsnValidation(Object fieldValue, RuleCondition condition) {
    log.warn("Operador V31+ CPF_SSN_VALIDATION: implementação pendente");
    return false;
  }

  private boolean evaluatePhoneCarrierCheck(Object fieldValue, RuleCondition condition) {
    log.warn("Operador V31+ PHONE_CARRIER_CHECK: implementação pendente");
    return false;
  }

  private boolean evaluateEmailDomainAge(Object fieldValue, RuleCondition condition) {
    log.warn("Operador V31+ EMAIL_DOMAIN_AGE: implementação pendente");
    return false;
  }

  private boolean evaluateAddressVerification(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ ADDRESS_VERIFICATION: implementação pendente");
    return false;
  }

  private boolean evaluateIdentityVelocity(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ IDENTITY_VELOCITY: implementação pendente");
    return false;
  }

  private boolean evaluateDeviceAccountRatio(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ DEVICE_ACCOUNT_RATIO: implementação pendente");
    return false;
  }

  private boolean evaluateEmailPhoneMismatch(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ EMAIL_PHONE_MISMATCH: implementação pendente");
    return false;
  }

  private boolean evaluateCreditFileThin(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ CREDIT_FILE_THIN: implementação pendente");
    return false;
  }

  // --- CATEGORIA F: AML Typology (8) ---
  private boolean evaluateStructuringDetection(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ STRUCTURING_DETECTION: implementação pendente");
    return false;
  }

  private boolean evaluateLayeringPattern(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ LAYERING_PATTERN: implementação pendente");
    return false;
  }

  private boolean evaluateRapidMovement(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ RAPID_MOVEMENT: implementação pendente");
    return false;
  }

  private boolean evaluateIntegrationPattern(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ INTEGRATION_PATTERN: implementação pendente");
    return false;
  }

  private boolean evaluateCashIntensiveRatio(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ CASH_INTENSIVE_RATIO: implementação pendente");
    return false;
  }

  private boolean evaluateUnusualBusinessPattern(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ UNUSUAL_BUSINESS_PATTERN: implementação pendente");
    return false;
  }

  private boolean evaluateShellCompanyIndicator(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ SHELL_COMPANY_INDICATOR: implementação pendente");
    return false;
  }

  private boolean evaluateTradeBasedMlIndicator(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ TRADE_BASED_ML_INDICATOR: implementação pendente");
    return false;
  }

  // --- CATEGORIA G: Regulatory (8) ---
  private boolean evaluateScaExemptionTra(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ SCA_EXEMPTION_TRA: implementação pendente");
    return false;
  }

  private boolean evaluateScaExemptionLowValue(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ SCA_EXEMPTION_LOW_VALUE: implementação pendente");
    return false;
  }

  private boolean evaluateScaExemptionTrustedBeneficiary(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ SCA_EXEMPTION_TRUSTED_BENEFICIARY: implementação pendente");
    return false;
  }

  private boolean evaluateScaExemptionRecurring(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ SCA_EXEMPTION_RECURRING: implementação pendente");
    return false;
  }

  private boolean evaluatePsd3CopNameMatch(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ PSD3_COP_NAME_MATCH: implementação pendente");
    return false;
  }

  private boolean evaluateDoraIncidentSeverity(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ DORA_INCIDENT_SEVERITY: implementação pendente");
    return false;
  }

  private boolean evaluateEidasAssuranceLevel(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ EIDAS_ASSURANCE_LEVEL: implementação pendente");
    return false;
  }

  private boolean evaluateGdprDataRetentionCheck(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ GDPR_DATA_RETENTION_CHECK: implementação pendente");
    return false;
  }

  // --- CATEGORIA H: Device (7) ---
  private boolean evaluateDeviceJailbreakRooted(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ DEVICE_JAILBREAK_ROOTED: implementação pendente");
    return false;
  }

  private boolean evaluateEmulatorDetection(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ EMULATOR_DETECTION: implementação pendente");
    return false;
  }

  private boolean evaluateVpnProxyDetection(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ VPN_PROXY_DETECTION: implementação pendente");
    return false;
  }

  private boolean evaluateTorExitNode(Object fieldValue, RuleCondition condition) {
    log.warn("Operador V31+ TOR_EXIT_NODE: implementação pendente");
    return false;
  }

  private boolean evaluateBrowserInconsistency(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ BROWSER_INCONSISTENCY: implementação pendente");
    return false;
  }

  private boolean evaluateTimezoneMismatch(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ TIMEZONE_MISMATCH: implementação pendente");
    return false;
  }

  private boolean evaluateLanguageMismatch(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ LANGUAGE_MISMATCH: implementação pendente");
    return false;
  }

  // --- CATEGORIA I: Merchant & MCC (7) ---
  private boolean evaluateMccHighRisk(Object fieldValue, RuleCondition condition) {
    log.warn("Operador V31+ MCC_HIGH_RISK: implementação pendente");
    return false;
  }

  private boolean evaluateMccGambling(Object fieldValue, RuleCondition condition) {
    log.warn("Operador V31+ MCC_GAMBLING: implementação pendente");
    return false;
  }

  private boolean evaluateMccCrypto(Object fieldValue, RuleCondition condition) {
    log.warn("Operador V31+ MCC_CRYPTO: implementação pendente");
    return false;
  }

  private boolean evaluateMerchantFirstSeen(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ MERCHANT_FIRST_SEEN: implementação pendente");
    return false;
  }

  private boolean evaluateMerchantCountryMismatch(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ MERCHANT_COUNTRY_MISMATCH: implementação pendente");
    return false;
  }

  private boolean evaluateMerchantCategoryChange(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ MERCHANT_CATEGORY_CHANGE: implementação pendente");
    return false;
  }

  private boolean evaluateMerchantVelocitySpike(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ MERCHANT_VELOCITY_SPIKE: implementação pendente");
    return false;
  }

  // --- CATEGORIA J: ISO 20022 (6) ---
  private boolean evaluatePacs008FieldValidation(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ PACS008_FIELD_VALIDATION: implementação pendente");
    return false;
  }

  private boolean evaluateRemittanceInfoAnalysis(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ REMITTANCE_INFO_ANALYSIS: implementação pendente");
    return false;
  }

  private boolean evaluatePurposeCodeMismatch(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ PURPOSE_CODE_MISMATCH: implementação pendente");
    return false;
  }

  private boolean evaluateUetrDuplicateCheck(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ UETR_DUPLICATE_CHECK: implementação pendente");
    return false;
  }

  private boolean evaluateCreditorNameValidation(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ CREDITOR_NAME_VALIDATION: implementação pendente");
    return false;
  }

  private boolean evaluateStructuredAddressCheck(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ STRUCTURED_ADDRESS_CHECK: implementação pendente");
    return false;
  }

  // --- CATEGORIA K: Estatísticos Simples (5) ---
  private boolean evaluateBenfordLawDeviation(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ BENFORD_LAW_DEVIATION: implementação pendente");
    return false;
  }

  private boolean evaluateZScoreGt(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ Z_SCORE_GT: implementação pendente");
    return false;
  }

  private boolean evaluateStandardDeviationGt(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ STANDARD_DEVIATION_GT: implementação pendente");
    return false;
  }

  private boolean evaluatePercentileGt(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ PERCENTILE_GT: implementação pendente");
    return false;
  }

  private boolean evaluateCoefficientVariationGt(RuleCondition condition, EvaluationContext context) {
    log.warn("Operador V31+ COEFFICIENT_VARIATION_GT: implementação pendente");
    return false;
  }

  // ========== OPERADORES V4.0 PHASE 1 - VELOCITY + DEVICE (40 novos) ==========

  // --- CATEGORIA L: Transaction Count Velocity Avançado (12) ---

  /**
   * TRANSACTION_COUNT_PER_CARD_HOUR: Contagem de transações por cartão por hora.
   * Formato valueSingle: "threshold" (ex: "5")
   */
  private boolean evaluateTransactionCountPerCardHour(RuleCondition condition, EvaluationContext context) {
    return evaluateVelocityOperator(condition, context, VelocityService.KeyType.PAN, 
        VelocityService.TimeWindow.HOUR_1, VelocityService.AggregationType.COUNT);
  }

  /**
   * TRANSACTION_COUNT_PER_IP_HOUR: Contagem de transações por IP por hora.
   * Formato valueSingle: "threshold" (ex: "10")
   */
  private boolean evaluateTransactionCountPerIpHour(RuleCondition condition, EvaluationContext context) {
    return evaluateVelocityOperator(condition, context, VelocityService.KeyType.IP_ADDRESS, 
        VelocityService.TimeWindow.HOUR_1, VelocityService.AggregationType.COUNT);
  }

  /**
   * TRANSACTION_COUNT_PER_DEVICE_DAY: Contagem de transações por dispositivo por dia.
   * Formato valueSingle: "threshold" (ex: "20")
   */
  private boolean evaluateTransactionCountPerDeviceDay(RuleCondition condition, EvaluationContext context) {
    return evaluateVelocityOperator(condition, context, VelocityService.KeyType.DEVICE_ID, 
        VelocityService.TimeWindow.HOUR_24, VelocityService.AggregationType.COUNT);
  }

  /**
   * TRANSACTION_COUNT_PER_MERCHANT_HOUR: Contagem de transações por merchant por hora.
   * Formato valueSingle: "threshold" (ex: "15")
   */
  private boolean evaluateTransactionCountPerMerchantHour(RuleCondition condition, EvaluationContext context) {
    return evaluateVelocityOperator(condition, context, VelocityService.KeyType.MERCHANT_ID, 
        VelocityService.TimeWindow.HOUR_1, VelocityService.AggregationType.COUNT);
  }

  /**
   * TRANSACTION_COUNT_PER_CUSTOMER_HOUR: Contagem de transações por cliente por hora.
   * Formato valueSingle: "threshold" (ex: "8")
   */
  private boolean evaluateTransactionCountPerCustomerHour(RuleCondition condition, EvaluationContext context) {
    return evaluateVelocityOperator(condition, context, VelocityService.KeyType.CUSTOMER_ID, 
        VelocityService.TimeWindow.HOUR_1, VelocityService.AggregationType.COUNT);
  }

  /**
   * UNIQUE_CARD_COUNT_PER_IP_HOUR: Cartões únicos por IP por hora.
   * Formato valueSingle: "threshold" (ex: "5")
   */
  private boolean evaluateUniqueCardCountPerIpHour(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getTransactionRequest() == null) return false;
      long threshold = Long.parseLong(condition.getValueSingle().trim());
      
      VelocityService.VelocityStats stats = velocityServiceFacade.getStats(
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
   * UNIQUE_MERCHANT_COUNT_PER_CARD_DAY: Merchants únicos por cartão por dia.
   * Formato valueSingle: "threshold" (ex: "20")
   */
  private boolean evaluateUniqueMerchantCountPerCardDay(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getTransactionRequest() == null) return false;
      long threshold = Long.parseLong(condition.getValueSingle().trim());
      
      VelocityService.VelocityStats stats = velocityServiceFacade.getStats(
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
   * TRANSACTION_ATTEMPT_COUNT_PER_CARD: Tentativas de transação por cartão (inclui falhas).
   * Formato valueSingle: "threshold|minutes" (ex: "5|15")
   */
  private boolean evaluateTransactionAttemptCountPerCard(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getTransactionRequest() == null) return false;
      
      String[] parts = condition.getValueSingle().split("\\|");
      long threshold = Long.parseLong(parts[0].trim());
      int minutes = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 15;
      
      VelocityService.TimeWindow window = parseTimeWindow(minutes);
      VelocityService.VelocityStats stats = velocityServiceFacade.getStats(
          context.getTransactionRequest(), 
          VelocityService.KeyType.PAN, 
          window);
      
      // Inclui tentativas com falha
      return stats.getTransactionCount() + stats.getFailedCount() > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar TRANSACTION_ATTEMPT_COUNT_PER_CARD: {}", e.getMessage());
      return false;
    }
  }

  /**
   * CVV_FAILURE_VELOCITY: Velocidade de falhas de CVV.
   * Formato valueSingle: "threshold|minutes" (ex: "3|10")
   */
  private boolean evaluateCvvFailureVelocity(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getTransactionRequest() == null) return false;
      
      String[] parts = condition.getValueSingle().split("\\|");
      long threshold = Long.parseLong(parts[0].trim());
      int minutes = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 10;
      
      VelocityService.TimeWindow window = parseTimeWindow(minutes);
      VelocityService.VelocityStats stats = velocityServiceFacade.getStats(
          context.getTransactionRequest(), 
          VelocityService.KeyType.PAN, 
          window);
      
      return stats.getCvvFailures() > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar CVV_FAILURE_VELOCITY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * ADDRESS_CHANGE_VELOCITY: Velocidade de alteração de endereço.
   * Formato valueSingle: "threshold|days" (ex: "3|30")
   */
  private boolean evaluateAddressChangeVelocity(RuleCondition condition, EvaluationContext context) {
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
   * BENEFICIARY_ADD_VELOCITY: Velocidade de adição de beneficiários.
   * Formato valueSingle: "threshold|days" (ex: "5|7")
   */
  private boolean evaluateBeneficiaryAddVelocity(RuleCondition condition, EvaluationContext context) {
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
   * CARD_ADD_VELOCITY: Velocidade de adição de cartões.
   * Formato valueSingle: "threshold|days" (ex: "3|7")
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
   * AMOUNT_SUM_PER_CARD_HOUR: Soma de valores por cartão por hora.
   * Formato valueSingle: "threshold" (ex: "5000")
   */
  private boolean evaluateAmountSumPerCardHour(RuleCondition condition, EvaluationContext context) {
    return evaluateVelocityOperator(condition, context, VelocityService.KeyType.PAN, 
        VelocityService.TimeWindow.HOUR_1, VelocityService.AggregationType.SUM);
  }

  /**
   * AMOUNT_SUM_PER_CUSTOMER_DAY: Soma de valores por cliente por dia.
   * Formato valueSingle: "threshold" (ex: "20000")
   */
  private boolean evaluateAmountSumPerCustomerDay(RuleCondition condition, EvaluationContext context) {
    return evaluateVelocityOperator(condition, context, VelocityService.KeyType.CUSTOMER_ID, 
        VelocityService.TimeWindow.HOUR_24, VelocityService.AggregationType.SUM);
  }

  /**
   * AVG_TRANSACTION_SPIKE: Spike de valor médio de transação.
   * Formato valueSingle: "multiplier" (ex: "3.0" = 3x a média histórica)
   */
  private boolean evaluateAvgTransactionSpike(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getTransactionRequest() == null) return false;
      
      double multiplier = Double.parseDouble(condition.getValueSingle().trim());
      
      VelocityService.VelocityStats stats = velocityServiceFacade.getStats(
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
   * LARGE_AMOUNT_FREQUENCY: Frequência de valores altos.
   * Formato valueSingle: "amountThreshold|countThreshold|days" (ex: "1000|3|7")
   */
  private boolean evaluateLargeAmountFrequency(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;
      
      String[] parts = condition.getValueSingle().split("\\|");
      double amountThreshold = Double.parseDouble(parts[0].trim());
      int countThreshold = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 3;
      int days = parts.length > 2 ? Integer.parseInt(parts[2].trim()) : 7;
      
      Object countObj = payload.get("large_amount_count_" + days + "d_over_" + (int) amountThreshold);
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
   * SMALL_AMOUNT_VELOCITY: Velocidade de valores pequenos (smurfing).
   * Formato valueSingle: "maxAmount|countThreshold|hours" (ex: "100|10|1")
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
   * ROUND_AMOUNT_FREQUENCY: Frequência de valores redondos.
   * Formato valueSingle: "roundThreshold|percentageThreshold|days" (ex: "100|50|7")
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
      
      double percentage = percentageObj instanceof Number ? ((Number) percentageObj).doubleValue() : 0.0;
      return percentage >= percentageThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar ROUND_AMOUNT_FREQUENCY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * SEQUENTIAL_AMOUNT_PATTERN: Padrão sequencial de valores.
   * Formato valueSingle: "patternType" (linear|fibonacci|geometric)
   */
  private boolean evaluateSequentialAmountPattern(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;
      
      String patternType = condition.getValueSingle() != null ? 
          condition.getValueSingle().trim().toLowerCase() : "linear";
      
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
   * AMOUNT_VARIANCE_ANOMALY: Anomalia de variância de valores.
   * Formato valueSingle: "zScoreThreshold" (ex: "3.0")
   */
  private boolean evaluateAmountVarianceAnomaly(RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getTransactionRequest() == null) return false;
      
      double zScoreThreshold = Double.parseDouble(condition.getValueSingle().trim());
      
      VelocityService.VelocityStats stats = velocityServiceFacade.getStats(
          context.getTransactionRequest(), 
          VelocityService.KeyType.PAN, 
          VelocityService.TimeWindow.DAY_30);
      
      BigDecimal currentAmount = context.getTransactionRequest().getTransactionAmount();
      BigDecimal avgAmount = stats.getAvgAmount();
      BigDecimal stdDev = stats.getStdDevAmount();
      
      if (stdDev == null || stdDev.compareTo(BigDecimal.ZERO) == 0) {
        return false;
      }
      
      double zScore = currentAmount.subtract(avgAmount).divide(stdDev, 4, java.math.RoundingMode.HALF_UP).doubleValue();
      return Math.abs(zScore) > zScoreThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar AMOUNT_VARIANCE_ANOMALY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * DAILY_LIMIT_PROXIMITY: Proximidade do limite diário.
   * Formato valueSingle: "percentageThreshold" (ex: "90" = 90% do limite)
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
   * WEEKLY_LIMIT_PROXIMITY: Proximidade do limite semanal.
   * Formato valueSingle: "percentageThreshold" (ex: "85" = 85% do limite)
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
   * TIME_BETWEEN_CONSECUTIVE_TX: Tempo entre transações consecutivas.
   * Formato valueSingle: "minSeconds" (ex: "5" = mínimo 5 segundos)
   */
  private boolean evaluateTimeBetweenConsecutiveTx(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;
      
      int minSeconds = Integer.parseInt(condition.getValueSingle().trim());
      
      Object secondsObj = payload.get("seconds_since_last_transaction");
      if (secondsObj == null) {
        secondsObj = payload.get("timeSinceLastTxSeconds");
      }
      
      int seconds = secondsObj instanceof Number ? ((Number) secondsObj).intValue() : Integer.MAX_VALUE;
      return seconds < minSeconds;
    } catch (Exception e) {
      log.error("Erro ao avaliar TIME_BETWEEN_CONSECUTIVE_TX: {}", e.getMessage());
      return false;
    }
  }

  /**
   * TRANSACTION_FREQUENCY_ANOMALY: Anomalia de frequência de transações.
   * Formato valueSingle: "multiplier" (ex: "3.0" = 3x a frequência normal)
   */
  private boolean evaluateTransactionFrequencyAnomaly(RuleCondition condition, EvaluationContext context) {
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
   * TIME_OF_DAY_ANOMALY: Anomalia de horário do dia.
   * Formato valueSingle: "deviationHours" (ex: "4" = 4 horas de desvio do padrão)
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
   * DORMANCY_ALERT_VELOCITY: Alerta de velocidade pós-dormência.
   * Formato valueSingle: "dormancyDays|amountThreshold" (ex: "90|1000")
   */
  private boolean evaluateDormancyAlertVelocity(RuleCondition condition, EvaluationContext context) {
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
        BigDecimal currentAmount = context.getTransactionRequest() != null ? 
            context.getTransactionRequest().getTransactionAmount() : BigDecimal.ZERO;
        return currentAmount.compareTo(BigDecimal.valueOf(amountThreshold)) >= 0;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar DORMANCY_ALERT_VELOCITY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * WEEKEND_VS_WEEKDAY_PATTERN: Padrão fim de semana vs dia útil.
   * Formato valueSingle: "deviationThreshold" (percentual de desvio)
   */
  private boolean evaluateWeekendVsWeekdayPattern(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;
      
      double deviationThreshold = Double.parseDouble(condition.getValueSingle().trim());
      
      Object deviationObj = payload.get("weekend_weekday_pattern_deviation");
      if (deviationObj == null) {
        deviationObj = payload.get("dayTypePatternDeviation");
      }
      
      double deviation = deviationObj instanceof Number ? ((Number) deviationObj).doubleValue() : 0.0;
      return deviation >= deviationThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar WEEKEND_VS_WEEKDAY_PATTERN: {}", e.getMessage());
      return false;
    }
  }

  /**
   * HOLIDAY_TRANSACTION_SPIKE: Spike de transações em feriados.
   * Formato valueSingle: "multiplier" (ex: "2.0" = 2x do normal)
   */
  private boolean evaluateHolidayTransactionSpike(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;
      
      double multiplier = Double.parseDouble(condition.getValueSingle().trim());
      
      Object isHolidayObj = payload.get("is_holiday");
      boolean isHoliday = Boolean.TRUE.equals(isHolidayObj) || "true".equalsIgnoreCase(String.valueOf(isHolidayObj));
      
      if (!isHoliday) return false;
      
      Object spikeRatioObj = payload.get("holiday_transaction_ratio");
      if (spikeRatioObj == null) {
        spikeRatioObj = payload.get("holidayActivityRatio");
      }
      
      double spikeRatio = spikeRatioObj instanceof Number ? ((Number) spikeRatioObj).doubleValue() : 1.0;
      return spikeRatio >= multiplier;
    } catch (Exception e) {
      log.error("Erro ao avaliar HOLIDAY_TRANSACTION_SPIKE: {}", e.getMessage());
      return false;
    }
  }

  /**
   * NIGHTTIME_TRANSACTION_RATIO: Razão de transações noturnas.
   * Formato valueSingle: "percentageThreshold|startHour|endHour" (ex: "50|22|6")
   */
  private boolean evaluateNighttimeTransactionRatio(RuleCondition condition, EvaluationContext context) {
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
   * BUSINESS_HOURS_DEVIATION: Desvio do horário comercial.
   * Formato valueSingle: "outsideBusinessHours" (true/false)
   */
  private boolean evaluateBusinessHoursDeviation(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;
      
      Object isOutsideObj = payload.get("is_outside_business_hours");
      if (isOutsideObj == null) {
        isOutsideObj = payload.get("outsideBusinessHours");
      }
      
      return Boolean.TRUE.equals(isOutsideObj) || "true".equalsIgnoreCase(String.valueOf(isOutsideObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar BUSINESS_HOURS_DEVIATION: {}", e.getMessage());
      return false;
    }
  }

  // --- CATEGORIA O: Device Fingerprint Avançado (10) ---

  /**
   * DEVICE_TRUST_SCORE: Score de confiança do dispositivo.
   * Formato valueSingle: "minScore" (ex: "70" = score mínimo de 70)
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

  /**
   * CANVAS_FINGERPRINT_MISMATCH: Incompatibilidade de fingerprint canvas.
   */
  private boolean evaluateCanvasFingerprintMismatch(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;
      
      Object mismatchObj = payload.get("canvas_fingerprint_mismatch");
      if (mismatchObj == null) {
        mismatchObj = payload.get("canvasMismatch");
      }
      
      return Boolean.TRUE.equals(mismatchObj) || "true".equalsIgnoreCase(String.valueOf(mismatchObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar CANVAS_FINGERPRINT_MISMATCH: {}", e.getMessage());
      return false;
    }
  }

  /**
   * WEBGL_FINGERPRINT_ANOMALY: Anomalia de fingerprint WebGL.
   */
  private boolean evaluateWebglFingerprintAnomaly(RuleCondition condition, EvaluationContext context) {
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

  /**
   * AUDIO_FINGERPRINT_NEW: Fingerprint de áudio novo/desconhecido.
   */
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

  /**
   * FONTS_FINGERPRINT_ANOMALY: Anomalia de fingerprint de fontes.
   */
  private boolean evaluateFontsFingerprintAnomaly(RuleCondition condition, EvaluationContext context) {
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

  /**
   * SCREEN_RESOLUTION_CHANGE: Mudança de resolução de tela.
   */
  private boolean evaluateScreenResolutionChange(RuleCondition condition, EvaluationContext context) {
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

  /**
   * BATTERY_LEVEL_ANOMALY: Anomalia de nível de bateria (ex: sempre 100%).
   */
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

  /**
   * HARDWARE_CONCURRENCY_MISMATCH: Incompatibilidade de concorrência de hardware.
   */
  private boolean evaluateHardwareConcurrencyMismatch(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;
      
      Object mismatchObj = payload.get("hardware_concurrency_mismatch");
      if (mismatchObj == null) {
        mismatchObj = payload.get("hardwareConcurrencyMismatch");
      }
      
      return Boolean.TRUE.equals(mismatchObj) || "true".equalsIgnoreCase(String.valueOf(mismatchObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar HARDWARE_CONCURRENCY_MISMATCH: {}", e.getMessage());
      return false;
    }
  }

  /**
   * TOUCH_SUPPORT_INCONSISTENCY: Inconsistência de suporte touch.
   */
  private boolean evaluateTouchSupportInconsistency(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;
      
      Object inconsistencyObj = payload.get("touch_support_inconsistency");
      if (inconsistencyObj == null) {
        inconsistencyObj = payload.get("touchSupportInconsistency");
      }
      
      return Boolean.TRUE.equals(inconsistencyObj) || "true".equalsIgnoreCase(String.valueOf(inconsistencyObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar TOUCH_SUPPORT_INCONSISTENCY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * DEVICE_MEMORY_ANOMALY: Anomalia de memória do dispositivo.
   */
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

  /**
   * Método auxiliar genérico para operadores de velocidade.
   */
  private boolean evaluateVelocityOperator(
      RuleCondition condition, 
      EvaluationContext context,
      VelocityService.KeyType keyType,
      VelocityService.TimeWindow window,
      VelocityService.AggregationType aggregationType) {
    try {
      if (context.getTransactionRequest() == null) return false;
      
      BigDecimal threshold = new BigDecimal(condition.getValueSingle().trim());
      
      VelocityService.VelocityStats stats = velocityServiceFacade.getStats(
          context.getTransactionRequest(), keyType, window);
      
      BigDecimal value = switch (aggregationType) {
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
   * BEHAVIORAL_BASELINE_DEVIATION: Verifica desvio do baseline comportamental.
   * Formato valueSingle: "deviationThreshold" (ex: "2.5" = 2.5 desvios padrão)
   */
  private boolean evaluateBehavioralBaselineDeviation(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;
      
      double threshold = Double.parseDouble(condition.getValueSingle().trim());
      
      Object deviationObj = payload.get("behavioral_deviation_score");
      if (deviationObj == null) {
        deviationObj = payload.get("behavioralDeviationScore");
      }
      
      if (deviationObj == null) return false;
      double deviation = deviationObj instanceof Number ? ((Number) deviationObj).doubleValue() : 0.0;
      return deviation > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar BEHAVIORAL_BASELINE_DEVIATION: {}", e.getMessage());
      return false;
    }
  }

  /**
   * SPENDING_CATEGORY_SHIFT: Detecta mudança de categoria de gastos.
   */
  private boolean evaluateSpendingCategoryShift(RuleCondition condition, EvaluationContext context) {
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
   * TRANSACTION_SIZE_ESCALATION: Detecta escalada de tamanho de transação.
   * Formato valueSingle: "escalationFactor" (ex: "1.5" = 50% de aumento)
   */
  private boolean evaluateTransactionSizeEscalation(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;
      
      double factor = Double.parseDouble(condition.getValueSingle().trim());
      
      Object escalationObj = payload.get("transaction_escalation_factor");
      if (escalationObj == null) {
        escalationObj = payload.get("transactionEscalationFactor");
      }
      
      if (escalationObj == null) return false;
      double escalation = escalationObj instanceof Number ? ((Number) escalationObj).doubleValue() : 0.0;
      return escalation > factor;
    } catch (Exception e) {
      log.error("Erro ao avaliar TRANSACTION_SIZE_ESCALATION: {}", e.getMessage());
      return false;
    }
  }

  /**
   * FREQUENCY_PATTERN_CHANGE: Detecta mudança de padrão de frequência.
   */
  private boolean evaluateFrequencyPatternChange(RuleCondition condition, EvaluationContext context) {
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

  /**
   * TIME_PREFERENCE_SHIFT: Detecta mudança de preferência de horário.
   */
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

  /**
   * CHANNEL_USAGE_ANOMALY: Detecta anomalia de uso de canal.
   */
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

  /**
   * PAYMENT_METHOD_SWITCH: Detecta troca de método de pagamento.
   */
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
   * RECIPIENT_DIVERSITY_CHANGE: Detecta mudança na diversidade de destinatários.
   * Formato valueSingle: "diversityThreshold" (ex: "3" = mais de 3 novos destinatários)
   */
  private boolean evaluateRecipientDiversityChange(RuleCondition condition, EvaluationContext context) {
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

  /**
   * GEOGRAPHIC_BEHAVIOR_SHIFT: Detecta mudança de comportamento geográfico.
   */
  private boolean evaluateGeographicBehaviorShift(RuleCondition condition, EvaluationContext context) {
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

  /**
   * SESSION_BEHAVIOR_ANOMALY: Detecta anomalia de comportamento de sessão.
   */
  private boolean evaluateSessionBehaviorAnomaly(RuleCondition condition, EvaluationContext context) {
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

  /**
   * LOGIN_PATTERN_DEVIATION: Detecta desvio de padrão de login.
   */
  private boolean evaluateLoginPatternDeviation(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;
      
      Object deviationObj = payload.get("login_pattern_deviation");
      if (deviationObj == null) {
        deviationObj = payload.get("loginPatternDeviation");
      }
      
      return Boolean.TRUE.equals(deviationObj) || "true".equalsIgnoreCase(String.valueOf(deviationObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar LOGIN_PATTERN_DEVIATION: {}", e.getMessage());
      return false;
    }
  }

  /**
   * NAVIGATION_PATTERN_ANOMALY: Detecta anomalia de padrão de navegação.
   */
  private boolean evaluateNavigationPatternAnomaly(RuleCondition condition, EvaluationContext context) {
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
   * TRANSACTION_TIMING_CLUSTER: Detecta cluster de timing de transações.
   * Formato valueSingle: "clusterSize" (ex: "5" = 5+ transações em cluster)
   */
  private boolean evaluateTransactionTimingCluster(RuleCondition condition, EvaluationContext context) {
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
   * AMOUNT_ROUNDING_BEHAVIOR: Detecta comportamento de arredondamento de valores.
   * Formato valueSingle: "roundingPercentage" (ex: "80" = 80% valores redondos)
   */
  private boolean evaluateAmountRoundingBehavior(RuleCondition condition, EvaluationContext context) {
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
          boolean isRound = amount.remainder(BigDecimal.valueOf(100)).compareTo(BigDecimal.ZERO) == 0;
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
   * SPLIT_PAYMENT_PATTERN: Detecta padrão de pagamento dividido.
   * Formato valueSingle: "splitCount" (ex: "3" = 3+ pagamentos divididos)
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
   * CHI_SQUARE_DISTRIBUTION_TEST: Teste Chi-Square de distribuição.
   * Formato valueSingle: "significanceLevel" (ex: "0.05")
   */
  private boolean evaluateChiSquareDistributionTest(RuleCondition condition, EvaluationContext context) {
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
   * KOLMOGOROV_SMIRNOV_TEST: Teste Kolmogorov-Smirnov.
   * Formato valueSingle: "significanceLevel" (ex: "0.05")
   */
  private boolean evaluateKolmogorovSmirnovTest(RuleCondition condition, EvaluationContext context) {
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
   * ANDERSON_DARLING_TEST: Teste Anderson-Darling.
   * Formato valueSingle: "criticalValue" (ex: "2.5")
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
   * T_TEST_AMOUNT_DEVIATION: T-Test de desvio de valor.
   * Formato valueSingle: "tStatThreshold" (ex: "2.0")
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
   * MANN_WHITNEY_U_TEST: Teste Mann-Whitney U.
   * Formato valueSingle: "significanceLevel" (ex: "0.05")
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
   * CORRELATION_ANOMALY: Detecta anomalia de correlação.
   * Formato valueSingle: "correlationThreshold" (ex: "0.7")
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
   * REGRESSION_RESIDUAL_OUTLIER: Detecta outlier de resíduo de regressão.
   * Formato valueSingle: "residualThreshold" (ex: "3.0" = 3 desvios)
   */
  private boolean evaluateRegressionResidualOutlier(RuleCondition condition, EvaluationContext context) {
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
   * VARIANCE_RATIO_TEST: Teste de razão de variância (F-test).
   * Formato valueSingle: "fStatThreshold" (ex: "4.0")
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
   * ENTROPY_SCORE_ANOMALY: Detecta anomalia de score de entropia.
   * Formato valueSingle: "entropyThreshold" (ex: "0.3" = baixa entropia suspeita)
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
   * SKEWNESS_KURTOSIS_ANOMALY: Detecta anomalia de skewness/kurtosis.
   * Formato valueSingle: "threshold" (ex: "3.0")
   */
  private boolean evaluateSkewnessKurtosisAnomaly(RuleCondition condition, EvaluationContext context) {
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
   * MCC_CATEGORY_VELOCITY: Velocidade por categoria MCC.
   * Formato valueSingle: "mccCategory|threshold" (ex: "gambling|5")
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
   * MCC_SPENDING_LIMIT_CHECK: Verificação de limite por MCC.
   * Formato valueSingle: "limitAmount" (ex: "5000")
   */
  private boolean evaluateMccSpendingLimitCheck(RuleCondition condition, EvaluationContext context) {
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
   * MCC_CROSS_CATEGORY_PATTERN: Detecta padrão cross-category MCC.
   * Formato valueSingle: "categoryCount" (ex: "4" = 4+ categorias diferentes)
   */
  private boolean evaluateMccCrossCategoryPattern(RuleCondition condition, EvaluationContext context) {
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
   * MERCHANT_REPUTATION_SCORE: Score de reputação do merchant.
   * Formato valueSingle: "minScore" (ex: "70" = score mínimo)
   */
  private boolean evaluateMerchantReputationScore(RuleCondition condition, EvaluationContext context) {
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
   * MERCHANT_AGE_CHECK: Verificação de idade do merchant.
   * Formato valueSingle: "minAgeDays" (ex: "30" = mínimo 30 dias)
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
   * MERCHANT_TRANSACTION_VOLUME: Volume de transações do merchant.
   * Formato valueSingle: "minVolume|maxVolume" (ex: "100|10000")
   */
  private boolean evaluateMerchantTransactionVolume(RuleCondition condition, EvaluationContext context) {
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
   * MERCHANT_CHARGEBACK_HISTORY: Histórico de chargeback do merchant.
   * Formato valueSingle: "maxChargebackRate" (ex: "0.02" = 2%)
   */
  private boolean evaluateMerchantChargebackHistory(RuleCondition condition, EvaluationContext context) {
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
   * MERCHANT_FRAUD_RATE_CHECK: Verificação de taxa de fraude do merchant.
   * Formato valueSingle: "maxFraudRate" (ex: "0.01" = 1%)
   */
  private boolean evaluateMerchantFraudRateCheck(RuleCondition condition, EvaluationContext context) {
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
   * MERCHANT_GEOGRAPHIC_SPREAD: Dispersão geográfica do merchant.
   * Formato valueSingle: "maxCountries" (ex: "10")
   */
  private boolean evaluateMerchantGeographicSpread(RuleCondition condition, EvaluationContext context) {
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
   * MERCHANT_CUSTOMER_CONCENTRATION: Concentração de clientes do merchant.
   * Formato valueSingle: "concentrationThreshold" (ex: "0.5" = 50%)
   */
  private boolean evaluateMerchantCustomerConcentration(RuleCondition condition, EvaluationContext context) {
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
   * MERCHANT_AMOUNT_DISTRIBUTION: Distribuição de valores do merchant.
   * Formato valueSingle: "stdDevThreshold" (ex: "1000")
   */
  private boolean evaluateMerchantAmountDistribution(RuleCondition condition, EvaluationContext context) {
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

  /**
   * MERCHANT_TIME_PATTERN: Padrão temporal do merchant.
   */
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
   * MERCHANT_DEVICE_DIVERSITY: Diversidade de dispositivos do merchant.
   * Formato valueSingle: "minDiversity|maxDiversity" (ex: "10|1000")
   */
  private boolean evaluateMerchantDeviceDiversity(RuleCondition condition, EvaluationContext context) {
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
   * MERCHANT_REFUND_RATIO: Razão de reembolso do merchant.
   * Formato valueSingle: "maxRefundRatio" (ex: "0.1" = 10%)
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
   * MERCHANT_NEW_CUSTOMER_RATIO: Razão de novos clientes do merchant.
   * Formato valueSingle: "maxNewCustomerRatio" (ex: "0.8" = 80%)
   */
  private boolean evaluateMerchantNewCustomerRatio(RuleCondition condition, EvaluationContext context) {
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
   * MERCHANT_DORMANT_REACTIVATION: Reativação de merchant dormente.
   * Formato valueSingle: "dormantDays" (ex: "90" = 90 dias de inatividade)
   */
  private boolean evaluateMerchantDormantReactivation(RuleCondition condition, EvaluationContext context) {
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
   * MERCHANT_CROSS_BORDER_RATIO: Razão cross-border do merchant.
   * Formato valueSingle: "maxCrossBorderRatio" (ex: "0.3" = 30%)
   */
  private boolean evaluateMerchantCrossBorderRatio(RuleCondition condition, EvaluationContext context) {
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
   * MERCHANT_HIGH_VALUE_FREQUENCY: Frequência de alto valor do merchant.
   * Formato valueSingle: "threshold|percentage" (ex: "1000|0.5" = 50% acima de 1000)
   */
  private boolean evaluateMerchantHighValueFrequency(RuleCondition condition, EvaluationContext context) {
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
      double percentage = percentageObj instanceof Number ? ((Number) percentageObj).doubleValue() : 0.0;
      return percentage > percentageThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_HIGH_VALUE_FREQUENCY: {}", e.getMessage());
      return false;
    }
  }
}
