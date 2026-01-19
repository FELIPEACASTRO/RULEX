package com.rulex.service.complex.evaluator;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.RuleCondition.ConditionOperator;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.List;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Avaliador para operadores de lista e pertencimento.
 *
 * <p>Operadores suportados:
 * <ul>
 *   <li>IN_LIST - valor está em uma lista</li>
 *   <li>NOT_IN_HISTORICAL - valor não está no histórico</li>
 *   <li>NOT_IN_CUSTOMER_HISTORY - valor não está no histórico do cliente</li>
 *   <li>NOT_IN_CUSTOMER_USUAL_HOURS - transação fora do horário usual</li>
 *   <li>DOMAIN_IN_LIST - domínio está em lista</li>
 *   <li>CONTAINS_SUSPICIOUS_KEYWORDS - contém palavras suspeitas</li>
 * </ul>
 */
@Component
@Slf4j
public class ListOperatorEvaluator implements OperatorEvaluator {

    private static final Set<ConditionOperator> SUPPORTED = Set.of(
        ConditionOperator.IN_LIST,
        ConditionOperator.NOT_IN_HISTORICAL,
        ConditionOperator.NOT_IN_CUSTOMER_HISTORY,
        ConditionOperator.NOT_IN_CUSTOMER_USUAL_HOURS,
        ConditionOperator.DOMAIN_IN_LIST,
        ConditionOperator.CONTAINS_SUSPICIOUS_KEYWORDS
    );

    // Lista de palavras suspeitas para detecção de fraude
    private static final Set<String> SUSPICIOUS_KEYWORDS = Set.of(
        "urgente", "urgent", "transferir", "transfer", "pix", "agora", "now",
        "premio", "prize", "ganhou", "won", "loteria", "lottery", "heranca",
        "inheritance", "milhoes", "millions", "deposito", "deposit", "conta",
        "account", "senha", "password", "codigo", "code", "verificacao",
        "verification", "confirmar", "confirm", "atualizar", "update",
        "bloqueado", "blocked", "suspenso", "suspended", "cancelar", "cancel"
    );

    // Domínios suspeitos conhecidos
    private static final Set<String> SUSPICIOUS_DOMAINS = Set.of(
        "tempmail.com", "guerrillamail.com", "10minutemail.com", "mailinator.com",
        "throwaway.email", "fakeinbox.com", "trashmail.com", "temp-mail.org"
    );

    @Override
    public Set<ConditionOperator> getSupportedOperators() {
        return SUPPORTED;
    }

    @Override
    public boolean evaluate(RuleCondition condition, EvaluationContext context) {
        ConditionOperator op = condition.getOperator();
        String fieldName = condition.getFieldName();
        Object fieldValue = getFieldValue(context, fieldName);

        log.debug("ListOperatorEvaluator: op={}, field={}, value={}", op, fieldName, fieldValue);

        return switch (op) {
            case IN_LIST -> evaluateInList(fieldValue, condition.getValueArray(), condition.getCaseSensitive());
            case NOT_IN_HISTORICAL -> evaluateNotInHistorical(fieldValue, condition, context);
            case NOT_IN_CUSTOMER_HISTORY -> evaluateNotInCustomerHistory(fieldValue, condition, context);
            case NOT_IN_CUSTOMER_USUAL_HOURS -> evaluateNotInCustomerUsualHours(context);
            case DOMAIN_IN_LIST -> evaluateDomainInList(fieldValue, condition.getValueArray());
            case CONTAINS_SUSPICIOUS_KEYWORDS -> evaluateContainsSuspiciousKeywords(fieldValue, condition);
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
     * IN_LIST - verifica se o valor está em uma lista fornecida.
     * Alias para IN do BasicComparisonEvaluator.
     */
    private boolean evaluateInList(Object fieldValue, List<String> values, Boolean caseSensitive) {
        if (fieldValue == null || values == null || values.isEmpty()) {
            return false;
        }

        String fieldStr = String.valueOf(fieldValue);
        boolean isCaseSensitive = caseSensitive != null && caseSensitive;

        for (String value : values) {
            if (isCaseSensitive) {
                if (fieldStr.equals(value)) return true;
            } else {
                if (fieldStr.equalsIgnoreCase(value)) return true;
            }
        }
        return false;
    }

    /**
     * NOT_IN_HISTORICAL - verifica se o valor não está no histórico geral.
     * Útil para detectar novos merchants, novos países, etc.
     */
    private boolean evaluateNotInHistorical(Object fieldValue, RuleCondition condition, EvaluationContext context) {
        if (fieldValue == null) {
            return true; // Valor nulo não está no histórico
        }

        // Verificar se há histórico no contexto
        Map<String, Object> payload = context.getPayload();
        if (payload == null) {
            return true;
        }

        // Tentar obter histórico do payload
        String historyKey = condition.getFieldName() + "_history";
        Object history = payload.get(historyKey);
        
        if (history == null) {
            // Também tentar com "historical_" prefix
            history = payload.get("historical_" + condition.getFieldName());
        }

        if (history instanceof List<?> historyList) {
            String fieldStr = String.valueOf(fieldValue);
            return historyList.stream()
                .map(String::valueOf)
                .noneMatch(h -> h.equalsIgnoreCase(fieldStr));
        }

        // Se não há histórico, considera como "não está no histórico"
        log.debug("NOT_IN_HISTORICAL: histórico não encontrado para {}", condition.getFieldName());
        return true;
    }

    /**
     * NOT_IN_CUSTOMER_HISTORY - verifica se o valor não está no histórico do cliente específico.
     */
    private boolean evaluateNotInCustomerHistory(Object fieldValue, RuleCondition condition, EvaluationContext context) {
        if (fieldValue == null) {
            return true;
        }

        Map<String, Object> payload = context.getPayload();
        if (payload == null) {
            return true;
        }

        // Tentar obter histórico do cliente
        Object customerHistory = payload.get("customer_history");
        if (customerHistory == null) {
            customerHistory = payload.get("customerHistory");
        }

        if (customerHistory instanceof Map<?, ?> historyMap) {
            Object fieldHistory = historyMap.get(condition.getFieldName());
            if (fieldHistory instanceof List<?> historyList) {
                String fieldStr = String.valueOf(fieldValue);
                return historyList.stream()
                    .map(String::valueOf)
                    .noneMatch(h -> h.equalsIgnoreCase(fieldStr));
            }
        }

        return true;
    }

    /**
     * NOT_IN_CUSTOMER_USUAL_HOURS - verifica se a transação está fora do horário usual do cliente.
     */
    private boolean evaluateNotInCustomerUsualHours(EvaluationContext context) {
        Map<String, Object> payload = context.getPayload();
        if (payload == null) {
            return false;
        }

        // Obter hora da transação
        Object transactionHour = payload.get("transactionHour");
        if (transactionHour == null) {
            transactionHour = payload.get("hour");
        }

        // Obter horários usuais do cliente
        Object usualHours = payload.get("customer_usual_hours");
        if (usualHours == null) {
            usualHours = payload.get("usualHours");
        }

        if (transactionHour == null || usualHours == null) {
            return false;
        }

        int hour;
        try {
            hour = Integer.parseInt(String.valueOf(transactionHour));
        } catch (NumberFormatException e) {
            return false;
        }

        if (usualHours instanceof List<?> hoursList) {
            return hoursList.stream()
                .map(h -> Integer.parseInt(String.valueOf(h)))
                .noneMatch(h -> h == hour);
        }

        return false;
    }

    /**
     * DOMAIN_IN_LIST - verifica se o domínio do email está em uma lista.
     */
    private boolean evaluateDomainInList(Object fieldValue, List<String> domains) {
        if (fieldValue == null) {
            return false;
        }

        String email = String.valueOf(fieldValue);
        String domain = extractDomain(email);

        if (domain == null) {
            return false;
        }

        // Se não há lista fornecida, usar lista de domínios suspeitos padrão
        Set<String> domainList = (domains != null && !domains.isEmpty()) 
            ? Set.copyOf(domains) 
            : SUSPICIOUS_DOMAINS;

        return domainList.stream()
            .anyMatch(d -> d.equalsIgnoreCase(domain));
    }

    /**
     * CONTAINS_SUSPICIOUS_KEYWORDS - verifica se o texto contém palavras suspeitas.
     */
    private boolean evaluateContainsSuspiciousKeywords(Object fieldValue, RuleCondition condition) {
        if (fieldValue == null) {
            return false;
        }

        String text = String.valueOf(fieldValue).toLowerCase();

        // Usar lista customizada se fornecida, senão usar padrão
        Set<String> keywords = (condition.getValueArray() != null && !condition.getValueArray().isEmpty())
            ? Set.copyOf(condition.getValueArray())
            : SUSPICIOUS_KEYWORDS;

        for (String keyword : keywords) {
            if (text.contains(keyword.toLowerCase())) {
                log.debug("CONTAINS_SUSPICIOUS_KEYWORDS: encontrou '{}' em '{}'", keyword, text);
                return true;
            }
        }

        return false;
    }

    private String extractDomain(String email) {
        if (email == null || !email.contains("@")) {
            return null;
        }
        int atIndex = email.lastIndexOf('@');
        if (atIndex < 0 || atIndex >= email.length() - 1) {
            return null;
        }
        return email.substring(atIndex + 1).toLowerCase();
    }

    @Override
    public String getCategory() {
        return "LIST";
    }
}
