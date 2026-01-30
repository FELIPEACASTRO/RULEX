package com.rulex.service.complex;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.RuleContextVariable;
import com.rulex.repository.TransactionRepository;
import com.rulex.service.VelocityService;
import com.rulex.service.WebhookClient;
import com.rulex.service.WebhookRetryService;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

/**
 * Serviço para resolução de variáveis de contexto via LOOKUP, AGGREGATION e EXTERNAL_SERVICE.
 * Implementa as operações que antes estavam como stubs.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class ContextVariableResolver {

  private final TransactionRepository transactionRepository;
  private final VelocityService velocityService;
  private final WebhookClient webhookClient;
  private final WebhookRetryService webhookRetryService;
  private final JdbcTemplate jdbcTemplate;
  private final ObjectMapper objectMapper;

  /**
   * Resolve uma variável do tipo LOOKUP.
   * Busca um valor em uma tabela de referência ou lista.
   *
   * @param var Definição da variável
   * @param payload Payload da transação
   * @param variables Variáveis já resolvidas
   * @return Valor encontrado ou null
   */
  @SuppressWarnings("unchecked")
  public Object resolveLookup(RuleContextVariable var, Map<String, Object> payload, Map<String, Object> variables) {
    Map<String, Object> config = var.getSourceConfig();
    if (config == null) {
      log.warn("Configuração de LOOKUP ausente para variável: {}", var.getName());
      return null;
    }

    String lookupType = (String) config.get("type");
    if (lookupType == null) {
      lookupType = "table"; // Default
    }

    try {
      return switch (lookupType.toLowerCase()) {
        case "table" -> resolveLookupTable(config, payload, variables);
        case "list" -> resolveLookupList(config, payload, variables);
        case "velocity" -> resolveLookupVelocity(config, payload, variables);
        default -> {
          log.warn("Tipo de LOOKUP desconhecido: {}", lookupType);
          yield null;
        }
      };
    } catch (Exception e) {
      log.error("Erro ao resolver LOOKUP {}: {}", var.getName(), e.getMessage());
      return null;
    }
  }

  /**
   * Resolve uma variável do tipo AGGREGATION.
   * Executa uma agregação sobre dados históricos.
   *
   * @param var Definição da variável
   * @param payload Payload da transação
   * @param variables Variáveis já resolvidas
   * @return Resultado da agregação
   */
  @SuppressWarnings("unchecked")
  public Object resolveAggregation(RuleContextVariable var, Map<String, Object> payload, Map<String, Object> variables) {
    Map<String, Object> config = var.getSourceConfig();
    if (config == null) {
      log.warn("Configuração de AGGREGATION ausente para variável: {}", var.getName());
      return null;
    }

    String aggType = (String) config.get("function");
    if (aggType == null) {
      aggType = "count";
    }

    String field = (String) config.get("field");
    String groupBy = (String) config.get("groupBy");
    Integer periodHours = config.get("periodHours") != null
        ? Integer.parseInt(String.valueOf(config.get("periodHours")))
        : 24;

    try {
      // Obter o valor do groupBy do payload
      String groupByValue = resolveValue(groupBy, payload, variables);
      if (groupByValue == null) {
        log.debug("Valor de groupBy não encontrado para: {}", groupBy);
        return getDefaultAggregationValue(aggType);
      }

      LocalDateTime since = LocalDateTime.now().minusHours(periodHours);

      return switch (aggType.toLowerCase()) {
        case "count" -> resolveCountAggregation(groupBy, groupByValue, since);
        case "sum" -> resolveSumAggregation(groupBy, groupByValue, field, since);
        case "avg" -> resolveAvgAggregation(groupBy, groupByValue, field, since);
        case "max" -> resolveMaxAggregation(groupBy, groupByValue, field, since);
        case "min" -> resolveMinAggregation(groupBy, groupByValue, field, since);
        default -> {
          log.warn("Tipo de AGGREGATION desconhecido: {}", aggType);
          yield getDefaultAggregationValue(aggType);
        }
      };
    } catch (Exception e) {
      log.error("Erro ao resolver AGGREGATION {}: {}", var.getName(), e.getMessage());
      return getDefaultAggregationValue(aggType);
    }
  }

  /**
   * Resolve uma variável do tipo EXTERNAL_SERVICE.
   * Chama um serviço externo via HTTP.
   *
   * @param var Definição da variável
   * @param payload Payload da transação
   * @param variables Variáveis já resolvidas
   * @return Resposta do serviço externo
   */
  @SuppressWarnings("unchecked")
  public Object resolveExternalService(RuleContextVariable var, Map<String, Object> payload, Map<String, Object> variables) {
    Map<String, Object> config = var.getSourceConfig();
    if (config == null) {
      log.warn("Configuração de EXTERNAL_SERVICE ausente para variável: {}", var.getName());
      return null;
    }

    String url = (String) config.get("url");
    String method = (String) config.get("method");
    String responseField = (String) config.get("responseField");
    Map<String, String> headers = (Map<String, String>) config.get("headers");

    if (url == null || url.isBlank()) {
      log.warn("URL não configurada para EXTERNAL_SERVICE: {}", var.getName());
      return null;
    }

    // Substituir placeholders na URL
    url = substituirPlaceholders(url, payload, variables);

    try {
      WebhookClient.WebhookResult result;
      String resolvedMethod = method != null ? method.trim().toUpperCase() : "GET";
      Map<String, Object> bodyToSend = null;

      if ("POST".equalsIgnoreCase(resolvedMethod)) {
        Map<String, Object> body = (Map<String, Object>) config.get("body");
        if (body == null) {
          body = new HashMap<>(payload);
        }
        bodyToSend = body;
        result = webhookClient.callWebhook(url, body, headers);
      } else {
        result = webhookClient.callExternalService(url, headers);
      }

      if (!result.isSuccess()) {
        log.warn("Serviço externo falhou para {}: {}", var.getName(), result.getErrorMessage());
        webhookRetryService.recordFailure(
            url,
            bodyToSend != null ? bodyToSend : new HashMap<>(),
            headers,
            resolvedMethod,
          var.getRuleVersionId(),
            resolveTransactionId(payload),
            resolveWebhookError(result),
            result.getStatusCode());
        return null;
      }

      // Parsear resposta e extrair campo específico se configurado
      if (responseField != null && !responseField.isBlank()) {
        Map<String, Object> response = objectMapper.readValue(result.getResponseBody(), Map.class);
        return extractNestedValue(responseField, response);
      }

      return result.getResponseBody();
    } catch (Exception e) {
      log.error("Erro ao chamar EXTERNAL_SERVICE {}: {}", var.getName(), e.getMessage());
      return null;
    }
  }

  // ========== Métodos auxiliares de LOOKUP ==========

  private Object resolveLookupTable(Map<String, Object> config, Map<String, Object> payload, Map<String, Object> variables) {
    String table = (String) config.get("table");
    String keyField = (String) config.get("keyField");
    String valueField = (String) config.get("valueField");
    String keyValue = resolveValue((String) config.get("key"), payload, variables);

    if (table == null || keyField == null || keyValue == null) {
      return null;
    }

    String safeTable = sanitizeIdentifier(table);
    String safeKeyField = sanitizeIdentifier(keyField);
    if (!isSafeIdentifier(table, safeTable) || !isSafeIdentifier(keyField, safeKeyField)) {
      log.warn("LOOKUP table/key inválidos: table={}, keyField={}", table, keyField);
      return null;
    }

    String safeValueField = null;
    if (valueField == null || valueField.isBlank()) {
      safeValueField = "*";
    } else if ("*".equals(valueField)) {
      safeValueField = "*";
    } else {
      safeValueField = sanitizeIdentifier(valueField);
      if (!isSafeIdentifier(valueField, safeValueField)) {
        log.warn("LOOKUP valueField inválido: {}", valueField);
        return null;
      }
    }

    String sql = String.format(
        "SELECT %s FROM %s WHERE %s = ? LIMIT 1",
        safeValueField,
        safeTable,
        safeKeyField);

    try {
      List<Map<String, Object>> results = jdbcTemplate.queryForList(sql, keyValue);
      if (results.isEmpty()) {
        return null;
      }
      Map<String, Object> row = results.get(0);
      return valueField != null ? row.get(valueField) : row;
    } catch (Exception e) {
      log.debug("LOOKUP table falhou: {}", e.getMessage());
      return null;
    }
  }

  private String resolveTransactionId(Map<String, Object> payload) {
    if (payload == null || payload.isEmpty()) {
      return null;
    }

    Object value = payload.get("transactionId");
    if (value == null) {
      value = payload.get("externalTransactionId");
    }
    if (value == null) {
      value = payload.get("transaction_id");
    }
    if (value == null) {
      value = payload.get("id");
    }

    return value != null ? String.valueOf(value) : null;
  }

  private String resolveWebhookError(WebhookClient.WebhookResult result) {
    if (result == null) {
      return "External service failed";
    }
    if (result.getErrorMessage() != null && !result.getErrorMessage().isBlank()) {
      return result.getErrorMessage();
    }
    if (result.getStatusCode() > 0) {
      return "HTTP " + result.getStatusCode();
    }
    return "External service failed";
  }

  private Object resolveLookupList(Map<String, Object> config, Map<String, Object> payload, Map<String, Object> variables) {
    String listName = (String) config.get("listName");
    String keyValue = resolveValue((String) config.get("key"), payload, variables);

    if (listName == null || keyValue == null) {
      return null;
    }

    String sql = """
        SELECT e.entry_value
        FROM rule_list_entries e
        JOIN rule_lists l ON e.list_id = l.id
        WHERE l.list_name = ? AND e.entry_value = ? AND l.is_active = true
        LIMIT 1
        """;

    try {
      List<Map<String, Object>> results = jdbcTemplate.queryForList(sql, listName, keyValue);
      return !results.isEmpty(); // Retorna true se encontrou na lista
    } catch (Exception e) {
      log.debug("LOOKUP list falhou: {}", e.getMessage());
      return false;
    }
  }

  private Object resolveLookupVelocity(Map<String, Object> config, Map<String, Object> payload, Map<String, Object> variables) {
    String dimension = (String) config.get("dimension");
    String keyValue = resolveValue((String) config.get("key"), payload, variables);
    Integer windowSeconds = config.get("windowSeconds") != null
        ? Integer.parseInt(String.valueOf(config.get("windowSeconds")))
        : 3600;

    if (dimension == null || keyValue == null) {
      return null;
    }

    try {
      // Mapear dimensão para KeyType do VelocityService
      VelocityService.KeyType keyType = switch (dimension.toLowerCase()) {
        case "pan" -> VelocityService.KeyType.PAN;
        case "customer_id", "customerid", "customer" -> VelocityService.KeyType.CUSTOMER_ID;
        case "merchant_id", "merchantid", "merchant" -> VelocityService.KeyType.MERCHANT_ID;
        case "ip_address", "ip" -> VelocityService.KeyType.IP_ADDRESS;
        case "device_id", "device" -> VelocityService.KeyType.DEVICE_ID;
        default -> VelocityService.KeyType.CUSTOMER_ID;
      };

      // Mapear windowSeconds para TimeWindow
      VelocityService.TimeWindow window;
      if (windowSeconds <= 5 * 60) {
        window = VelocityService.TimeWindow.MINUTE_5;
      } else if (windowSeconds <= 15 * 60) {
        window = VelocityService.TimeWindow.MINUTE_15;
      } else if (windowSeconds <= 30 * 60) {
        window = VelocityService.TimeWindow.MINUTE_30;
      } else if (windowSeconds <= 60 * 60) {
        window = VelocityService.TimeWindow.HOUR_1;
      } else if (windowSeconds <= 6 * 60 * 60) {
        window = VelocityService.TimeWindow.HOUR_6;
      } else if (windowSeconds <= 12 * 60 * 60) {
        window = VelocityService.TimeWindow.HOUR_12;
      } else if (windowSeconds <= 24 * 60 * 60) {
        window = VelocityService.TimeWindow.HOUR_24;
      } else if (windowSeconds <= 7 * 24 * 60 * 60) {
        window = VelocityService.TimeWindow.DAY_7;
      } else {
        window = VelocityService.TimeWindow.DAY_30;
      }

      // Criar TransactionRequest mínimo para consulta
      TransactionRequest tempRequest = TransactionRequest.builder()
          .customerIdFromHeader(dimension.contains("customer") ? keyValue : "system")
          .pan(dimension.contains("pan") ? keyValue : "0000")
          .merchantId(dimension.contains("merchant") ? keyValue : null)
          .build();

      return velocityService.getAggregation(tempRequest, keyType, window, VelocityService.AggregationType.COUNT);
    } catch (Exception e) {
      log.debug("LOOKUP velocity falhou: {}", e.getMessage());
      return 0L;
    }
  }

  // ========== Métodos auxiliares de AGGREGATION ==========

  private Object resolveCountAggregation(String groupBy, String groupByValue, LocalDateTime since) {
    return switch (groupBy) {
      case "customerIdFromHeader", "customer_id" ->
          transactionRepository.countTransactionsByCustomerSince(groupByValue, since);
      case "merchantId", "merchant_id" ->
          transactionRepository.countTransactionsByMerchantSince(groupByValue, since);
      default -> {
        // Query genérica
        String sql = String.format(
            "SELECT COUNT(*) FROM transactions WHERE %s = ? AND created_at >= ?",
            sanitizeIdentifier(groupBy));
        yield jdbcTemplate.queryForObject(sql, Long.class, groupByValue, since);
      }
    };
  }

  private Object resolveSumAggregation(String groupBy, String groupByValue, String field, LocalDateTime since) {
    if (field == null) {
      field = "transaction_amount";
    }
    return switch (groupBy) {
      case "customerIdFromHeader", "customer_id" ->
          transactionRepository.sumAmountByCustomerSince(groupByValue, since);
      case "merchantId", "merchant_id" ->
          transactionRepository.sumAmountByMerchantSince(groupByValue, since);
      default -> {
        String safeGroupBy = sanitizeIdentifier(groupBy);
        String safeField = sanitizeIdentifier(field);
        if (!isSafeIdentifier(groupBy, safeGroupBy) || !isSafeIdentifier(field, safeField)) {
          log.warn("AGGREGATION sum inválido: groupBy={}, field={}", groupBy, field);
          yield BigDecimal.ZERO;
        }
        String sql = String.format(
            "SELECT COALESCE(SUM(%s), 0) FROM transactions WHERE %s = ? AND created_at >= ?",
            safeField,
            safeGroupBy);
        yield jdbcTemplate.queryForObject(sql, BigDecimal.class, groupByValue, since);
      }
    };
  }

  private Object resolveAvgAggregation(String groupBy, String groupByValue, String field, LocalDateTime since) {
    if (field == null) {
      field = "transaction_amount";
    }
    String safeGroupBy = sanitizeIdentifier(groupBy);
    String safeField = sanitizeIdentifier(field);
    if (!isSafeIdentifier(groupBy, safeGroupBy) || !isSafeIdentifier(field, safeField)) {
      log.warn("AGGREGATION avg inválido: groupBy={}, field={}", groupBy, field);
      return BigDecimal.ZERO;
    }
    String sql = String.format(
        "SELECT COALESCE(AVG(%s), 0) FROM transactions WHERE %s = ? AND created_at >= ?",
        safeField,
        safeGroupBy);
    return jdbcTemplate.queryForObject(sql, BigDecimal.class, groupByValue, since);
  }

  private Object resolveMaxAggregation(String groupBy, String groupByValue, String field, LocalDateTime since) {
    if (field == null) {
      field = "transaction_amount";
    }
    String safeGroupBy = sanitizeIdentifier(groupBy);
    String safeField = sanitizeIdentifier(field);
    if (!isSafeIdentifier(groupBy, safeGroupBy) || !isSafeIdentifier(field, safeField)) {
      log.warn("AGGREGATION max inválido: groupBy={}, field={}", groupBy, field);
      return null;
    }
    String sql = String.format(
        "SELECT MAX(%s) FROM transactions WHERE %s = ? AND created_at >= ?",
        safeField,
        safeGroupBy);
    return jdbcTemplate.queryForObject(sql, Object.class, groupByValue, since);
  }

  private Object resolveMinAggregation(String groupBy, String groupByValue, String field, LocalDateTime since) {
    if (field == null) {
      field = "transaction_amount";
    }
    String safeGroupBy = sanitizeIdentifier(groupBy);
    String safeField = sanitizeIdentifier(field);
    if (!isSafeIdentifier(groupBy, safeGroupBy) || !isSafeIdentifier(field, safeField)) {
      log.warn("AGGREGATION min inválido: groupBy={}, field={}", groupBy, field);
      return null;
    }
    String sql = String.format(
        "SELECT MIN(%s) FROM transactions WHERE %s = ? AND created_at >= ?",
        safeField,
        safeGroupBy);
    return jdbcTemplate.queryForObject(sql, Object.class, groupByValue, since);
  }

  // ========== Métodos utilitários ==========

  private String resolveValue(String path, Map<String, Object> payload, Map<String, Object> variables) {
    if (path == null) {
      return null;
    }

    // Primeiro tentar nas variáveis
    if (variables.containsKey(path)) {
      Object val = variables.get(path);
      return val != null ? String.valueOf(val) : null;
    }

    // Depois no payload (suporta nested paths)
    Object val = extractNestedValue(path, payload);
    return val != null ? String.valueOf(val) : null;
  }

  @SuppressWarnings("unchecked")
  private Object extractNestedValue(String path, Map<String, Object> data) {
    if (path == null || data == null) {
      return null;
    }

    String[] parts = path.split("\\.");
    Object current = data;

    for (String part : parts) {
      if (current == null) {
        return null;
      }
      if (current instanceof Map) {
        current = ((Map<String, Object>) current).get(part);
      } else {
        return null;
      }
    }
    return current;
  }

  private String substituirPlaceholders(String template, Map<String, Object> payload, Map<String, Object> variables) {
    String result = template;

    // Substituir ${variavel} por valores
    for (Map.Entry<String, Object> entry : variables.entrySet()) {
      result = result.replace("${" + entry.getKey() + "}",
          entry.getValue() != null ? String.valueOf(entry.getValue()) : "");
    }

    for (Map.Entry<String, Object> entry : payload.entrySet()) {
      result = result.replace("${" + entry.getKey() + "}",
          entry.getValue() != null ? String.valueOf(entry.getValue()) : "");
    }

    return result;
  }

  private Object getDefaultAggregationValue(String aggType) {
    return switch (aggType.toLowerCase()) {
      case "count" -> 0L;
      case "sum", "avg" -> BigDecimal.ZERO;
      default -> null;
    };
  }

  private String sanitizeIdentifier(String identifier) {
    // Prevenir SQL injection - apenas permitir caracteres alfanuméricos e underscore
    if (identifier == null) {
      return null;
    }
    return identifier.replaceAll("[^a-zA-Z0-9_]", "");
  }

  private boolean isSafeIdentifier(String original, String sanitized) {
    if (original == null || sanitized == null) {
      return false;
    }
    if (sanitized.isBlank()) {
      return false;
    }
    return original.equals(sanitized);
  }
}
