package com.rulex.service.enrichment;

import com.rulex.dto.TransactionRequest;
import com.rulex.repository.VelocityTransactionLogRepository;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Enriquecimento de dados do cliente para regras de fraude.
 *
 * <p>Consolida todos os campos de cliente em um formato fácil de usar pelo motor de regras. Campos
 * disponíveis:
 *
 * <ul>
 *   <li>customer.is_first_transaction - Se é a primeira transação do cliente
 *   <li>customer.account_age_days - Dias desde criação da conta
 *   <li>customer.total_transactions - Total de transações históricas
 *   <li>customer.total_amount - Soma de todas transações
 *   <li>customer.avg_amount - Média histórica de valores
 *   <li>customer.max_amount - Maior transação histórica
 *   <li>customer.last_transaction_days - Dias desde última transação
 *   <li>customer.chargeback_count - Número de chargebacks
 *   <li>customer.chargeback_rate - Taxa de chargeback
 *   <li>customer.fraud_flag - Flag de fraude anterior
 *   <li>customer.risk_score - Score de risco do cliente
 *   <li>customer.kyc_verified - KYC verificado
 *   <li>customer.cpf_blocked - CPF em lista de bloqueio
 *   <li>customer.email_domain_risk - Risco do domínio do email
 *   <li>customer.phone_verified - Telefone verificado
 * </ul>
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class CustomerEnrichment {

  private final VelocityTransactionLogRepository transactionLogRepository;

  // Domínios de email de alto risco
  private static final Set<String> HIGH_RISK_EMAIL_DOMAINS =
      Set.of(
          "tempmail.com",
          "throwaway.com",
          "guerrillamail.com",
          "10minutemail.com",
          "mailinator.com",
          "yopmail.com",
          "fakeinbox.com",
          "trashmail.com",
          "dispostable.com",
          "getnada.com");

  // Domínios de email de risco médio (gratuitos populares)
  private static final Set<String> MEDIUM_RISK_EMAIL_DOMAINS =
      Set.of("gmail.com", "yahoo.com", "hotmail.com", "outlook.com", "aol.com", "protonmail.com");

  /** Resultado do enriquecimento de cliente. */
  @Data
  @Builder
  public static class CustomerContext {
    // Status da transação
    private final boolean isFirstTransaction;
    private final int accountAgeDays;

    // Histórico de transações
    private final long totalTransactions;
    private final BigDecimal totalAmount;
    private final BigDecimal avgAmount;
    private final BigDecimal maxAmount;
    private final BigDecimal minAmount;
    private final int lastTransactionDays;
    private final int lastTransactionHours;

    // Fraude e chargeback
    private final int chargebackCount;
    private final double chargebackRate;
    private final boolean hasFraudHistory;
    private final int fraudCount;

    // Verificação
    private final boolean kycVerified;
    private final boolean phoneVerified;
    private final boolean emailVerified;
    private final boolean cpfBlocked;

    // Email
    private final String emailDomain;
    private final int emailDomainRisk;
    private final boolean isDisposableEmail;

    // Score
    private final int riskScore;
    private final int trustScore;

    /** Converte para Map para uso no evaluator. */
    public Map<String, Object> toMap() {
      Map<String, Object> map = new HashMap<>();

      // Status
      map.put("customer.is_first_transaction", isFirstTransaction);
      map.put("customer.account_age_days", accountAgeDays);

      // Aliases
      map.put("isFirstTransaction", isFirstTransaction);
      map.put("accountAgeDays", accountAgeDays);

      // Histórico
      map.put("customer.total_transactions", totalTransactions);
      map.put("customer.total_amount", totalAmount);
      map.put("customer.avg_amount", avgAmount);
      map.put("customer.max_amount", maxAmount);
      map.put("customer.min_amount", minAmount);
      map.put("customer.last_transaction_days", lastTransactionDays);
      map.put("customer.last_transaction_hours", lastTransactionHours);

      // Aliases
      map.put("customerTotalTransactions", totalTransactions);
      map.put("customerAvgAmount", avgAmount);

      // Fraude
      map.put("customer.chargeback_count", chargebackCount);
      map.put("customer.chargeback_rate", chargebackRate);
      map.put("customer.has_fraud_history", hasFraudHistory);
      map.put("customer.fraud_count", fraudCount);

      // Aliases
      map.put("chargebackCount", chargebackCount);
      map.put("hasFraudHistory", hasFraudHistory);

      // Verificação
      map.put("customer.kyc_verified", kycVerified);
      map.put("customer.phone_verified", phoneVerified);
      map.put("customer.email_verified", emailVerified);
      map.put("customer.cpf_blocked", cpfBlocked);

      // Aliases
      map.put("kycVerified", kycVerified);
      map.put("cpfBlocked", cpfBlocked);

      // Email
      map.put("customer.email_domain", emailDomain);
      map.put("customer.email_domain_risk", emailDomainRisk);
      map.put("customer.is_disposable_email", isDisposableEmail);

      // Score
      map.put("customer.risk_score", riskScore);
      map.put("customer.trust_score", trustScore);

      // Aliases
      map.put("customerRiskScore", riskScore);

      return map;
    }

    public static CustomerContext empty() {
      return CustomerContext.builder()
          .isFirstTransaction(true)
          .accountAgeDays(0)
          .totalTransactions(0)
          .totalAmount(BigDecimal.ZERO)
          .avgAmount(BigDecimal.ZERO)
          .maxAmount(BigDecimal.ZERO)
          .minAmount(BigDecimal.ZERO)
          .lastTransactionDays(0)
          .lastTransactionHours(0)
          .chargebackCount(0)
          .chargebackRate(0.0)
          .hasFraudHistory(false)
          .fraudCount(0)
          .kycVerified(false)
          .phoneVerified(false)
          .emailVerified(false)
          .cpfBlocked(false)
          .emailDomain(null)
          .emailDomainRisk(0)
          .isDisposableEmail(false)
          .riskScore(50)
          .trustScore(50)
          .build();
    }
  }

  /**
   * Enriquece uma transação com dados do cliente.
   *
   * @param request A transação a ser enriquecida
   * @param customerData Dados adicionais do cliente (opcional)
   * @return Contexto de cliente com todos os campos calculados
   */
  public CustomerContext enrich(TransactionRequest request, Map<String, Object> customerData) {
    if (request == null) {
      log.debug("TransactionRequest é null, retornando contexto vazio");
      return CustomerContext.empty();
    }

    try {
      String customerId = request.getCustomerIdFromHeader();

      // Buscar histórico do cliente
      CustomerHistory history = getCustomerHistory(customerId);

      // Extrair dados adicionais
      String email = getStringFromMap(customerData, "email", null);
      boolean kycVerified = getBooleanFromMap(customerData, "kycVerified", false);
      boolean phoneVerified = getBooleanFromMap(customerData, "phoneVerified", false);
      boolean emailVerified = getBooleanFromMap(customerData, "emailVerified", false);
      boolean cpfBlocked = getBooleanFromMap(customerData, "cpfBlocked", false);
      Integer accountAgeDays = getIntegerFromMap(customerData, "accountAgeDays", 0);

      // Analisar email
      String emailDomain = extractEmailDomain(email);
      int emailDomainRisk = calculateEmailDomainRisk(emailDomain);
      boolean isDisposableEmail = isDisposableEmailDomain(emailDomain);

      // Calcular scores
      int riskScore =
          calculateRiskScore(
              history, kycVerified, phoneVerified, cpfBlocked, emailDomainRisk, accountAgeDays);
      int trustScore = 100 - riskScore;

      return CustomerContext.builder()
          .isFirstTransaction(history.totalTransactions == 0)
          .accountAgeDays(accountAgeDays)
          .totalTransactions(history.totalTransactions)
          .totalAmount(history.totalAmount)
          .avgAmount(history.avgAmount)
          .maxAmount(history.maxAmount)
          .minAmount(history.minAmount)
          .lastTransactionDays(history.daysSinceLastTransaction)
          .lastTransactionHours(history.hoursSinceLastTransaction)
          .chargebackCount(history.chargebackCount)
          .chargebackRate(history.chargebackRate)
          .hasFraudHistory(history.fraudCount > 0)
          .fraudCount(history.fraudCount)
          .kycVerified(kycVerified)
          .phoneVerified(phoneVerified)
          .emailVerified(emailVerified)
          .cpfBlocked(cpfBlocked)
          .emailDomain(emailDomain)
          .emailDomainRisk(emailDomainRisk)
          .isDisposableEmail(isDisposableEmail)
          .riskScore(riskScore)
          .trustScore(trustScore)
          .build();

    } catch (Exception e) {
      log.warn("Erro ao enriquecer customer: {}", e.getMessage());
      return CustomerContext.empty();
    }
  }

  /** Versão simplificada sem dados adicionais. */
  public CustomerContext enrich(TransactionRequest request) {
    return enrich(request, new HashMap<>());
  }

  /** Histórico do cliente. */
  private static class CustomerHistory {
    long totalTransactions = 0;
    BigDecimal totalAmount = BigDecimal.ZERO;
    BigDecimal avgAmount = BigDecimal.ZERO;
    BigDecimal maxAmount = BigDecimal.ZERO;
    BigDecimal minAmount = BigDecimal.ZERO;
    int daysSinceLastTransaction = 0;
    int hoursSinceLastTransaction = 0;
    int chargebackCount = 0;
    double chargebackRate = 0.0;
    int fraudCount = 0;
  }

  /** Busca histórico do cliente no banco. */
  private CustomerHistory getCustomerHistory(String customerId) {
    CustomerHistory history = new CustomerHistory();

    if (customerId == null || customerId.isBlank()) {
      return history;
    }

    try {
      OffsetDateTime since = OffsetDateTime.now(ZoneOffset.UTC).minusYears(2);

      // Buscar contagem total
      history.totalTransactions =
          transactionLogRepository.countByCustomerIdSince(customerId, since);

      if (history.totalTransactions > 0) {
        // Buscar soma e média
        BigDecimal sum = transactionLogRepository.sumAmountByCustomerIdSince(customerId, since);
        history.totalAmount = sum != null ? sum : BigDecimal.ZERO;
        history.avgAmount =
            history.totalAmount.divide(
                BigDecimal.valueOf(history.totalTransactions), 2, RoundingMode.HALF_UP);
      }

      // Buscar última atividade do cliente
      Optional<OffsetDateTime> lastActivityOpt =
          transactionLogRepository.findLastActivityByCustomerId(customerId);
      if (lastActivityOpt.isPresent()) {
        OffsetDateTime lastActivity = lastActivityOpt.get();
        OffsetDateTime now = OffsetDateTime.now(ZoneOffset.UTC);
        history.daysSinceLastTransaction =
            (int) java.time.temporal.ChronoUnit.DAYS.between(lastActivity, now);
        history.hoursSinceLastTransaction =
            (int) java.time.temporal.ChronoUnit.HOURS.between(lastActivity, now);
      }

    } catch (Exception e) {
      log.debug("Erro ao buscar histórico do cliente: {}", e.getMessage());
    }

    return history;
  }

  /** Extrai domínio do email. */
  private String extractEmailDomain(String email) {
    if (email == null || !email.contains("@")) {
      return null;
    }
    return email.substring(email.indexOf("@") + 1).toLowerCase();
  }

  /** Calcula risco do domínio do email (0-100). */
  private int calculateEmailDomainRisk(String domain) {
    if (domain == null) return 50;

    if (HIGH_RISK_EMAIL_DOMAINS.contains(domain)) return 100;
    if (MEDIUM_RISK_EMAIL_DOMAINS.contains(domain)) return 30;

    // Domínios corporativos são mais confiáveis
    if (domain.endsWith(".gov") || domain.endsWith(".edu")) return 10;
    if (domain.endsWith(".com.br") || domain.endsWith(".co.uk")) return 20;

    return 40; // Domínio desconhecido
  }

  /** Verifica se é email descartável. */
  private boolean isDisposableEmailDomain(String domain) {
    return domain != null && HIGH_RISK_EMAIL_DOMAINS.contains(domain);
  }

  /** Calcula score de risco do cliente. */
  private int calculateRiskScore(
      CustomerHistory history,
      boolean kycVerified,
      boolean phoneVerified,
      boolean cpfBlocked,
      int emailDomainRisk,
      int accountAgeDays) {

    int score = 50; // Base

    // CPF bloqueado é crítico
    if (cpfBlocked) return 100;

    // Histórico de fraude
    if (history.fraudCount > 0) {
      score += 30;
    }

    // Chargeback
    if (history.chargebackRate > 0.1) {
      score += 20;
    } else if (history.chargebackRate > 0.05) {
      score += 10;
    }

    // Primeira transação
    if (history.totalTransactions == 0) {
      score += 15;
    }

    // Conta nova
    if (accountAgeDays < 7) {
      score += 15;
    } else if (accountAgeDays < 30) {
      score += 10;
    } else if (accountAgeDays > 365) {
      score -= 10;
    }

    // Verificações reduzem risco
    if (kycVerified) score -= 15;
    if (phoneVerified) score -= 10;

    // Email de risco
    score += (emailDomainRisk - 50) / 5;

    return Math.max(0, Math.min(100, score));
  }

  // Helpers
  private boolean getBooleanFromMap(Map<String, Object> map, String key, boolean defaultValue) {
    if (map == null) return defaultValue;
    Object value = map.get(key);
    if (value instanceof Boolean) return (Boolean) value;
    if (value instanceof String) return Boolean.parseBoolean((String) value);
    return defaultValue;
  }

  private String getStringFromMap(Map<String, Object> map, String key, String defaultValue) {
    if (map == null) return defaultValue;
    Object value = map.get(key);
    return value instanceof String ? (String) value : defaultValue;
  }

  private Integer getIntegerFromMap(Map<String, Object> map, String key, Integer defaultValue) {
    if (map == null) return defaultValue;
    Object value = map.get(key);
    if (value instanceof Integer) return (Integer) value;
    if (value instanceof Number) return ((Number) value).intValue();
    return defaultValue;
  }
}
