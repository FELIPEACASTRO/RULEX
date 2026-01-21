package com.rulex.service.complex.evaluator;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Avaliador para operadores diversos/miscelâneos.
 *
 * <p>Operadores suportados:
 * <ul>
 *   <li>IS_VOIP - número é VoIP</li>
 *   <li>IS_CRYPTO_RANSOM_AMOUNT - valor típico de ransomware</li>
 *   <li>IS_IMPOSSIBLE_COMBINATION - combinação impossível de dados</li>
 *   <li>IS_HOLIDAY - data é feriado</li>
 *   <li>HAS_FAILED_3DS_LAST_N_MINUTES - falha 3DS recente</li>
 *   <li>HAS_INCOMING_TRANSFER_LAST_N_HOURS - transferência recebida</li>
 *   <li>GTE_PERCENT_OF_LAST_INCOMING - percentual da última entrada</li>
 *   <li>OUTFLOW_RATE_LAST_N_DAYS - taxa de saída</li>
 *   <li>PIX_KEY_CHANGED_LAST_N_DAYS - chave PIX alterada</li>
 *   <li>NAME_SIMILARITY_LT - similaridade de nome menor que</li>
 *   <li>NAME_TRANSLITERATION_MATCH - match de transliteração</li>
 * </ul>
 */
@Component
@Slf4j
public class MiscOperatorEvaluator implements OperatorEvaluator {

    private static final Set<ConditionOperator> SUPPORTED = Set.of(
        ConditionOperator.IS_VOIP,
        ConditionOperator.IS_CRYPTO_RANSOM_AMOUNT,
        ConditionOperator.IS_IMPOSSIBLE_COMBINATION,
        ConditionOperator.IS_HOLIDAY,
        ConditionOperator.HAS_FAILED_3DS_LAST_N_MINUTES,
        ConditionOperator.HAS_INCOMING_TRANSFER_LAST_N_HOURS,
        ConditionOperator.GTE_PERCENT_OF_LAST_INCOMING,
        ConditionOperator.OUTFLOW_RATE_LAST_N_DAYS,
        ConditionOperator.PIX_KEY_CHANGED_LAST_N_DAYS,
        ConditionOperator.NAME_SIMILARITY_LT,
        ConditionOperator.NAME_TRANSLITERATION_MATCH
    );

    // Prefixos de VoIP conhecidos (Brasil)
    private static final Set<String> VOIP_PREFIXES = Set.of(
        "0300", "0303", "0500", "0800", "0900", "4000", "4003", "4004"
    );

    // Valores típicos de ransomware em BTC (convertidos para BRL aproximado)
    private static final Set<BigDecimal> RANSOM_AMOUNTS = Set.of(
        new BigDecimal("500"),
        new BigDecimal("1000"),
        new BigDecimal("2000"),
        new BigDecimal("5000"),
        new BigDecimal("10000"),
        new BigDecimal("50000"),
        new BigDecimal("100000")
    );

    // Feriados nacionais brasileiros (mês-dia)
    private static final Set<String> BRAZILIAN_HOLIDAYS = Set.of(
        "01-01", // Ano Novo
        "04-21", // Tiradentes
        "05-01", // Dia do Trabalho
        "09-07", // Independência
        "10-12", // Nossa Senhora Aparecida
        "11-02", // Finados
        "11-15", // Proclamação da República
        "12-25"  // Natal
    );

    @Override
    public Set<ConditionOperator> getSupportedOperators() {
        return SUPPORTED;
    }

    @Override
    public boolean evaluate(RuleCondition condition, EvaluationContext context) {
        ConditionOperator op = condition.getOperator();
        
        log.debug("MiscOperatorEvaluator: op={}, field={}", op, condition.getFieldName());

        return switch (op) {
            case IS_VOIP -> evaluateIsVoip(condition, context);
            case IS_CRYPTO_RANSOM_AMOUNT -> evaluateIsCryptoRansomAmount(condition, context);
            case IS_IMPOSSIBLE_COMBINATION -> evaluateIsImpossibleCombination(condition, context);
            case IS_HOLIDAY -> evaluateIsHoliday(condition, context);
            case HAS_FAILED_3DS_LAST_N_MINUTES -> evaluateHasFailed3ds(condition, context);
            case HAS_INCOMING_TRANSFER_LAST_N_HOURS -> evaluateHasIncomingTransfer(condition, context);
            case GTE_PERCENT_OF_LAST_INCOMING -> evaluateGtePercentOfLastIncoming(condition, context);
            case OUTFLOW_RATE_LAST_N_DAYS -> evaluateOutflowRate(condition, context);
            case PIX_KEY_CHANGED_LAST_N_DAYS -> evaluatePixKeyChanged(condition, context);
            case NAME_SIMILARITY_LT -> evaluateNameSimilarityLt(condition, context);
            case NAME_TRANSLITERATION_MATCH -> evaluateNameTransliterationMatch(condition, context);
            default -> false;
        };
    }

    private Object getFieldValue(EvaluationContext context, String fieldName) {
        if (context == null || fieldName == null) {
            return null;
        }

        Map<String, Object> payload = context.getPayload();
        if (payload != null && payload.containsKey(fieldName)) {
            return payload.get(fieldName);
        }

        if (context.getTransactionRequest() != null) {
            try {
                var request = context.getTransactionRequest();
                var field = request.getClass().getDeclaredField(fieldName);
                field.setAccessible(true);
                return field.get(request);
            } catch (Exception e) {
                log.trace("Campo {} não encontrado no TransactionRequest", fieldName);
            }
        }

        return null;
    }

    /**
     * IS_VOIP - verifica se o número de telefone é VoIP.
     */
    private boolean evaluateIsVoip(RuleCondition condition, EvaluationContext context) {
        Object phoneValue = getFieldValue(context, condition.getFieldName());
        if (phoneValue == null) {
            phoneValue = getFieldValue(context, "phone");
        }
        if (phoneValue == null) {
            phoneValue = getFieldValue(context, "phoneNumber");
        }

        if (phoneValue == null) {
            return false;
        }

        String phone = String.valueOf(phoneValue).replaceAll("[^0-9]", "");
        
        // Verificar prefixos VoIP
        for (String prefix : VOIP_PREFIXES) {
            if (phone.startsWith(prefix)) {
                log.debug("IS_VOIP: telefone {} tem prefixo VoIP {}", phone, prefix);
                return true;
            }
        }

        // Verificar flag no payload
        Map<String, Object> payload = context.getPayload();
        if (payload != null) {
            if (getBooleanValue(payload, "isVoip") || getBooleanValue(payload, "is_voip")) {
                return true;
            }
        }

        return false;
    }

    /**
     * IS_CRYPTO_RANSOM_AMOUNT - verifica se o valor é típico de ransomware.
     */
    private boolean evaluateIsCryptoRansomAmount(RuleCondition condition, EvaluationContext context) {
        Object amountValue = getFieldValue(context, condition.getFieldName());
        if (amountValue == null) {
            amountValue = getFieldValue(context, "amount");
        }

        if (amountValue == null) {
            return false;
        }

        BigDecimal amount;
        try {
            amount = new BigDecimal(String.valueOf(amountValue));
        } catch (NumberFormatException e) {
            return false;
        }

        // Verificar se é valor redondo típico de ransomware
        for (BigDecimal ransomAmount : RANSOM_AMOUNTS) {
            // Tolerância de 5%
            BigDecimal tolerance = ransomAmount.multiply(new BigDecimal("0.05"));
            BigDecimal min = ransomAmount.subtract(tolerance);
            BigDecimal max = ransomAmount.add(tolerance);
            
            if (amount.compareTo(min) >= 0 && amount.compareTo(max) <= 0) {
                log.debug("IS_CRYPTO_RANSOM_AMOUNT: valor {} próximo de {}", amount, ransomAmount);
                return true;
            }
        }

        return false;
    }

    /**
     * IS_IMPOSSIBLE_COMBINATION - verifica combinações impossíveis de dados.
     */
    private boolean evaluateIsImpossibleCombination(RuleCondition condition, EvaluationContext context) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) {
            return false;
        }

        // Verificar combinações impossíveis comuns
        
        // 1. País do cartão diferente do país do IP
        String cardCountry = getStringValue(payload, "cardCountry");
        String ipCountry = getStringValue(payload, "ipCountry");
        if (cardCountry != null && ipCountry != null && !cardCountry.equalsIgnoreCase(ipCountry)) {
            // Verificar se não é viagem conhecida
            if (!getBooleanValue(payload, "isTraveling") && !getBooleanValue(payload, "knownTraveler")) {
                log.debug("IS_IMPOSSIBLE_COMBINATION: cardCountry={} != ipCountry={}", cardCountry, ipCountry);
                return true;
            }
        }

        // 2. Dispositivo mobile mas user agent de desktop
        boolean isMobile = getBooleanValue(payload, "isMobileDevice");
        String userAgent = getStringValue(payload, "userAgent");
        if (isMobile && userAgent != null && !userAgent.toLowerCase().contains("mobile")) {
            log.debug("IS_IMPOSSIBLE_COMBINATION: mobile device com user agent desktop");
            return true;
        }

        // 3. Horário impossível (ex: 3AM em dia útil para transação comercial grande)
        Object hourValue = payload.get("transactionHour");
        Object amountValue = payload.get("amount");
        if (hourValue != null && amountValue != null) {
            int hour = parseIntSafe(String.valueOf(hourValue), 12);
            BigDecimal amount = parseBigDecimalSafe(String.valueOf(amountValue), BigDecimal.ZERO);
            
            if (hour >= 1 && hour <= 5 && amount.compareTo(new BigDecimal("10000")) > 0) {
                if (!getBooleanValue(payload, "isInternational")) {
                    log.debug("IS_IMPOSSIBLE_COMBINATION: transação grande às {}h", hour);
                    return true;
                }
            }
        }

        return false;
    }

    /**
     * IS_HOLIDAY - verifica se a data é feriado.
     */
    private boolean evaluateIsHoliday(RuleCondition condition, EvaluationContext context) {
        Map<String, Object> payload = context.getPayload();
        
        LocalDate date = LocalDate.now();
        
        // Tentar obter data do payload
        if (payload != null) {
            Object dateValue = payload.get("transactionDate");
            if (dateValue instanceof LocalDate ld) {
                date = ld;
            } else if (dateValue instanceof LocalDateTime ldt) {
                date = ldt.toLocalDate();
            }
        }

        String monthDay = String.format("%02d-%02d", date.getMonthValue(), date.getDayOfMonth());
        
        if (BRAZILIAN_HOLIDAYS.contains(monthDay)) {
            log.debug("IS_HOLIDAY: {} é feriado", monthDay);
            return true;
        }

        // Verificar fim de semana
        DayOfWeek dayOfWeek = date.getDayOfWeek();
        if (dayOfWeek == DayOfWeek.SATURDAY || dayOfWeek == DayOfWeek.SUNDAY) {
            // Alguns contextos consideram fim de semana como "feriado"
            String includeWeekend = condition.getValueSingle();
            if ("true".equalsIgnoreCase(includeWeekend) || "1".equals(includeWeekend)) {
                return true;
            }
        }

        return false;
    }

    /**
     * HAS_FAILED_3DS_LAST_N_MINUTES - verifica se houve falha 3DS recente.
     */
    private boolean evaluateHasFailed3ds(RuleCondition condition, EvaluationContext context) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) {
            return false;
        }

        // Verificar flag direta
        if (getBooleanValue(payload, "hasFailed3ds") || getBooleanValue(payload, "has_failed_3ds")) {
            return true;
        }

        // Verificar contagem
        Object failCount = payload.get("failed3dsCount");
        if (failCount == null) {
            failCount = payload.get("failed_3ds_count");
        }

        if (failCount != null) {
            int count = parseIntSafe(String.valueOf(failCount), 0);
            return count > 0;
        }

        return false;
    }

    /**
     * HAS_INCOMING_TRANSFER_LAST_N_HOURS - verifica se houve transferência recebida.
     */
    private boolean evaluateHasIncomingTransfer(RuleCondition condition, EvaluationContext context) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) {
            return false;
        }

        // Verificar flag direta
        if (getBooleanValue(payload, "hasIncomingTransfer") || getBooleanValue(payload, "has_incoming_transfer")) {
            return true;
        }

        // Verificar valor de entrada
        Object incomingAmount = payload.get("lastIncomingAmount");
        if (incomingAmount == null) {
            incomingAmount = payload.get("last_incoming_amount");
        }

        if (incomingAmount != null) {
            BigDecimal amount = parseBigDecimalSafe(String.valueOf(incomingAmount), BigDecimal.ZERO);
            return amount.compareTo(BigDecimal.ZERO) > 0;
        }

        return false;
    }

    /**
     * GTE_PERCENT_OF_LAST_INCOMING - valor >= X% da última entrada.
     */
    private boolean evaluateGtePercentOfLastIncoming(RuleCondition condition, EvaluationContext context) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) {
            return false;
        }

        // Obter valor da transação atual
        Object currentAmount = payload.get("amount");
        if (currentAmount == null) {
            return false;
        }

        // Obter valor da última entrada
        Object lastIncoming = payload.get("lastIncomingAmount");
        if (lastIncoming == null) {
            lastIncoming = payload.get("last_incoming_amount");
        }
        if (lastIncoming == null) {
            return false;
        }

        BigDecimal current = parseBigDecimalSafe(String.valueOf(currentAmount), BigDecimal.ZERO);
        BigDecimal incoming = parseBigDecimalSafe(String.valueOf(lastIncoming), BigDecimal.ZERO);

        if (incoming.compareTo(BigDecimal.ZERO) <= 0) {
            return false;
        }

        // Calcular percentual
        BigDecimal percent = current.divide(incoming, 4, java.math.RoundingMode.HALF_UP)
            .multiply(new BigDecimal("100"));

        BigDecimal threshold = parseBigDecimalSafe(condition.getValueSingle(), new BigDecimal("80"));
        
        log.debug("GTE_PERCENT_OF_LAST_INCOMING: percent={}, threshold={}", percent, threshold);
        return percent.compareTo(threshold) >= 0;
    }

    /**
     * OUTFLOW_RATE_LAST_N_DAYS - taxa de saída nos últimos N dias.
     */
    private boolean evaluateOutflowRate(RuleCondition condition, EvaluationContext context) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) {
            return false;
        }

        Object outflowRate = payload.get("outflowRate");
        if (outflowRate == null) {
            outflowRate = payload.get("outflow_rate");
        }

        if (outflowRate == null) {
            return false;
        }

        BigDecimal rate = parseBigDecimalSafe(String.valueOf(outflowRate), BigDecimal.ZERO);
        BigDecimal threshold = parseBigDecimalSafe(condition.getValueSingle(), new BigDecimal("80"));
        
        log.debug("OUTFLOW_RATE_LAST_N_DAYS: rate={}, threshold={}", rate, threshold);
        return rate.compareTo(threshold) > 0;
    }

    /**
     * PIX_KEY_CHANGED_LAST_N_DAYS - chave PIX alterada recentemente.
     */
    private boolean evaluatePixKeyChanged(RuleCondition condition, EvaluationContext context) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) {
            return false;
        }

        // Verificar flag direta
        if (getBooleanValue(payload, "pixKeyChanged") || getBooleanValue(payload, "pix_key_changed")) {
            return true;
        }

        // Verificar dias desde alteração
        Object daysSinceChange = payload.get("daysSincePixKeyChange");
        if (daysSinceChange == null) {
            daysSinceChange = payload.get("days_since_pix_key_change");
        }

        if (daysSinceChange != null) {
            int days = parseIntSafe(String.valueOf(daysSinceChange), Integer.MAX_VALUE);
            int threshold = parseIntSafe(condition.getValueSingle(), 7);
            return days <= threshold;
        }

        return false;
    }

    /**
     * NAME_SIMILARITY_LT - similaridade de nome menor que threshold.
     */
    private boolean evaluateNameSimilarityLt(RuleCondition condition, EvaluationContext context) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) {
            return false;
        }

        // Obter nomes para comparar
        String name1 = getStringValue(payload, "accountHolderName");
        String name2 = getStringValue(payload, "beneficiaryName");
        
        if (name1 == null || name2 == null) {
            name1 = getStringValue(payload, "name1");
            name2 = getStringValue(payload, "name2");
        }

        if (name1 == null || name2 == null) {
            return false;
        }

        // Calcular similaridade (Levenshtein normalizado)
        double similarity = calculateSimilarity(name1, name2);
        double threshold = parseDoubleSafe(condition.getValueSingle(), 0.8);
        
        log.debug("NAME_SIMILARITY_LT: similarity={}, threshold={}", similarity, threshold);
        return similarity < threshold;
    }

    /**
     * NAME_TRANSLITERATION_MATCH - verifica se nomes são equivalentes via transliteração.
     */
    private boolean evaluateNameTransliterationMatch(RuleCondition condition, EvaluationContext context) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) {
            return false;
        }

        String name1 = getStringValue(payload, "name1");
        String name2 = getStringValue(payload, "name2");

        if (name1 == null || name2 == null) {
            return false;
        }

        // Normalizar nomes (remover acentos, converter para minúsculas)
        String normalized1 = normalizeForComparison(name1);
        String normalized2 = normalizeForComparison(name2);

        return normalized1.equals(normalized2);
    }

    // Métodos auxiliares

    private boolean getBooleanValue(Map<String, Object> payload, String key) {
        Object value = payload.get(key);
        if (value == null) return false;
        if (value instanceof Boolean b) return b;
        String str = String.valueOf(value).toLowerCase();
        return "true".equals(str) || "1".equals(str) || "yes".equals(str);
    }

    private String getStringValue(Map<String, Object> payload, String key) {
        Object value = payload.get(key);
        return value != null ? String.valueOf(value) : null;
    }

    private int parseIntSafe(String value, int defaultValue) {
        try {
            return Integer.parseInt(value);
        } catch (Exception e) {
            return defaultValue;
        }
    }

    private double parseDoubleSafe(String value, double defaultValue) {
        try {
            return Double.parseDouble(value);
        } catch (Exception e) {
            return defaultValue;
        }
    }

    private BigDecimal parseBigDecimalSafe(String value, BigDecimal defaultValue) {
        try {
            return new BigDecimal(value);
        } catch (Exception e) {
            return defaultValue;
        }
    }

    private double calculateSimilarity(String s1, String s2) {
        if (s1 == null || s2 == null) return 0.0;
        
        String str1 = s1.toLowerCase().trim();
        String str2 = s2.toLowerCase().trim();
        
        if (str1.equals(str2)) return 1.0;
        if (str1.isEmpty() || str2.isEmpty()) return 0.0;

        int distance = levenshteinDistance(str1, str2);
        int maxLength = Math.max(str1.length(), str2.length());
        
        return 1.0 - ((double) distance / maxLength);
    }

    private int levenshteinDistance(String s1, String s2) {
        int[][] dp = new int[s1.length() + 1][s2.length() + 1];

        for (int i = 0; i <= s1.length(); i++) {
            for (int j = 0; j <= s2.length(); j++) {
                if (i == 0) {
                    dp[i][j] = j;
                } else if (j == 0) {
                    dp[i][j] = i;
                } else {
                    int cost = s1.charAt(i - 1) == s2.charAt(j - 1) ? 0 : 1;
                    dp[i][j] = Math.min(
                        Math.min(dp[i - 1][j] + 1, dp[i][j - 1] + 1),
                        dp[i - 1][j - 1] + cost
                    );
                }
            }
        }

        return dp[s1.length()][s2.length()];
    }

    private String normalizeForComparison(String name) {
        if (name == null) return "";
        
        // Remover acentos
        String normalized = java.text.Normalizer.normalize(name, java.text.Normalizer.Form.NFD)
            .replaceAll("[\\p{InCombiningDiacriticalMarks}]", "");
        
        // Converter para minúsculas e remover espaços extras
        return normalized.toLowerCase().replaceAll("\\s+", " ").trim();
    }

    @Override
    public String getCategory() {
        return "MISC";
    }
}
