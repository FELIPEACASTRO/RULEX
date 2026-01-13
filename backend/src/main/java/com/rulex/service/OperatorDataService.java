package com.rulex.service;

import com.rulex.repository.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.Optional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * Serviço centralizado para fornecer dados aos operadores do ComplexRuleEvaluator. Resolve os gaps
 * identificados no Triple Check.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class OperatorDataService {

  private final HolidayRepository holidayRepository;
  private final MerchantChargebackRepository merchantChargebackRepository;
  private final CustomerChargebackHistoryRepository customerChargebackHistoryRepository;
  private final CustomerIncomingTransferRepository customerIncomingTransferRepository;
  private final AuthenticationFailureRepository authenticationFailureRepository;
  private final CustomerLastTransactionRepository customerLastTransactionRepository;
  private final VoipPhoneRangeRepository voipPhoneRangeRepository;
  private final CustomerAccountInfoRepository customerAccountInfoRepository;
  private final CustomerBeneficiaryHistoryRepository customerBeneficiaryHistoryRepository;

  // ==================== IS_HOLIDAY ====================

  /**
   * Verifica se uma data é feriado.
   *
   * @param date Data a verificar
   * @param countryCode Código do país (default: BRA)
   * @return true se for feriado
   */
  public boolean isHoliday(LocalDate date, String countryCode) {
    if (date == null) {
      return false;
    }
    String country = countryCode != null ? countryCode : "BRA";
    try {
      return holidayRepository.isHoliday(date, country);
    } catch (Exception e) {
      log.warn("Erro ao verificar feriado: {}", e.getMessage());
      return false;
    }
  }

  public boolean isBankHoliday(LocalDate date, String countryCode) {
    if (date == null) {
      return false;
    }
    String country = countryCode != null ? countryCode : "BRA";
    try {
      return holidayRepository.isBankHoliday(date, country);
    } catch (Exception e) {
      log.warn("Erro ao verificar feriado bancário: {}", e.getMessage());
      return false;
    }
  }

  // ==================== CHARGEBACK_RATE_GT ====================

  /**
   * Obtém a taxa de chargeback de um merchant.
   *
   * @param merchantId ID do merchant
   * @return Taxa de chargeback (0-1)
   */
  public BigDecimal getChargebackRate(String merchantId) {
    if (merchantId == null || merchantId.isBlank()) {
      return BigDecimal.ZERO;
    }
    try {
      return merchantChargebackRepository
          .findChargebackRateByMerchantId(merchantId)
          .orElse(BigDecimal.ZERO);
    } catch (Exception e) {
      log.warn("Erro ao obter taxa de chargeback: {}", e.getMessage());
      return BigDecimal.ZERO;
    }
  }

  public boolean hasHighChargebackRate(String merchantId, BigDecimal threshold) {
    if (merchantId == null || threshold == null) {
      return false;
    }
    try {
      return merchantChargebackRepository.hasHighChargebackRate(merchantId, threshold);
    } catch (Exception e) {
      log.warn("Erro ao verificar taxa de chargeback: {}", e.getMessage());
      return false;
    }
  }

  // ==================== IN_CUSTOMER_CHARGEBACK_MERCHANTS ====================

  /**
   * Verifica se o cliente já teve chargeback com o merchant.
   *
   * @param customerId ID do cliente
   * @param merchantId ID do merchant
   * @return true se já houve chargeback
   */
  public boolean hasChargebackWithMerchant(String customerId, String merchantId) {
    if (customerId == null || merchantId == null) {
      return false;
    }
    try {
      return customerChargebackHistoryRepository.hasChargebackWithMerchant(customerId, merchantId);
    } catch (Exception e) {
      log.warn("Erro ao verificar histórico de chargeback: {}", e.getMessage());
      return false;
    }
  }

  public List<String> getMerchantsWithChargeback(String customerId) {
    if (customerId == null) {
      return List.of();
    }
    try {
      return customerChargebackHistoryRepository.findMerchantIdsWithChargebackByCustomer(
          customerId);
    } catch (Exception e) {
      log.warn("Erro ao obter merchants com chargeback: {}", e.getMessage());
      return List.of();
    }
  }

  // ==================== GTE_PERCENT_OF_LAST_INCOMING ====================

  /**
   * Obtém o valor da última transferência recebida pelo cliente.
   *
   * @param customerId ID do cliente
   * @return Valor da última transferência ou empty
   */
  public Optional<BigDecimal> getLastIncomingAmount(String customerId) {
    if (customerId == null) {
      return Optional.empty();
    }
    try {
      return customerIncomingTransferRepository.findLastIncomingAmount(customerId);
    } catch (Exception e) {
      log.warn("Erro ao obter última transferência: {}", e.getMessage());
      return Optional.empty();
    }
  }

  // ==================== COUNT_FAILURES_LAST_N_HOURS ====================

  /**
   * Conta falhas de autenticação nas últimas N horas.
   *
   * @param customerId ID do cliente
   * @param hours Número de horas
   * @return Contagem de falhas
   */
  public long countAuthFailuresLastNHours(String customerId, int hours) {
    if (customerId == null || hours <= 0) {
      return 0;
    }
    try {
      OffsetDateTime since = OffsetDateTime.now().minusHours(hours);
      return authenticationFailureRepository.countFailuresSince(customerId, since);
    } catch (Exception e) {
      log.warn("Erro ao contar falhas de autenticação: {}", e.getMessage());
      return 0;
    }
  }

  public long countAuthFailuresByCardLastNHours(String cardHash, int hours) {
    if (cardHash == null || hours <= 0) {
      return 0;
    }
    try {
      OffsetDateTime since = OffsetDateTime.now().minusHours(hours);
      return authenticationFailureRepository.countFailuresByCardSince(cardHash, since);
    } catch (Exception e) {
      log.warn("Erro ao contar falhas por cartão: {}", e.getMessage());
      return 0;
    }
  }

  // ==================== TIME_SINCE_LAST_LT ====================

  /**
   * Obtém o timestamp da última transação do cliente.
   *
   * @param customerId ID do cliente
   * @return Timestamp da última transação ou empty
   */
  public Optional<OffsetDateTime> getLastTransactionDate(String customerId) {
    if (customerId == null) {
      return Optional.empty();
    }
    try {
      return customerLastTransactionRepository.findLastTransactionDateByCustomerId(customerId);
    } catch (Exception e) {
      log.warn("Erro ao obter última transação: {}", e.getMessage());
      return Optional.empty();
    }
  }

  /**
   * Calcula minutos desde a última transação.
   *
   * @param customerId ID do cliente
   * @return Minutos desde última transação ou -1 se não houver histórico
   */
  public long getMinutesSinceLastTransaction(String customerId) {
    return getLastTransactionDate(customerId)
        .map(
            lastDate -> {
              long seconds =
                  java.time.Duration.between(lastDate, OffsetDateTime.now()).getSeconds();
              return seconds / 60;
            })
        .orElse(-1L);
  }

  // ==================== IS_VOIP ====================

  /**
   * Verifica se um número de telefone é VoIP.
   *
   * @param phoneNumber Número de telefone
   * @param countryCode Código do país
   * @return true se for VoIP
   */
  public boolean isVoipNumber(String phoneNumber, String countryCode) {
    if (phoneNumber == null || phoneNumber.isBlank()) {
      return false;
    }
    String country = countryCode != null ? countryCode : "55";
    String cleanPhone = phoneNumber.replaceAll("[^0-9]", "");

    try {
      return voipPhoneRangeRepository.isVoipNumber(country, cleanPhone);
    } catch (Exception e) {
      log.warn("Erro ao verificar VoIP: {}", e.getMessage());
      return false;
    }
  }

  // ==================== ACCOUNT_AGE_LT_MINUTES ====================

  /**
   * Obtém a idade da conta em minutos.
   *
   * @param customerId ID do cliente
   * @return Idade em minutos ou -1 se não encontrar
   */
  public long getAccountAgeInMinutes(String customerId) {
    if (customerId == null) {
      return -1;
    }
    try {
      return customerAccountInfoRepository.getAccountAgeInMinutes(customerId).orElse(-1L);
    } catch (Exception e) {
      log.warn("Erro ao obter idade da conta: {}", e.getMessage());
      return -1;
    }
  }

  // ==================== NOT_IN_HISTORICAL ====================

  /**
   * Verifica se um beneficiário é novo para o cliente.
   *
   * @param customerId ID do cliente
   * @param beneficiaryId ID do beneficiário
   * @return true se for novo (não está no histórico)
   */
  public boolean isNewBeneficiary(String customerId, String beneficiaryId) {
    if (customerId == null || beneficiaryId == null) {
      return true; // Se não temos dados, assumir novo
    }
    try {
      return !customerBeneficiaryHistoryRepository.existsBeneficiaryForCustomer(
          customerId, beneficiaryId);
    } catch (Exception e) {
      log.warn("Erro ao verificar histórico de beneficiário: {}", e.getMessage());
      return true;
    }
  }

  public boolean isTrustedBeneficiary(String customerId, String beneficiaryId) {
    if (customerId == null || beneficiaryId == null) {
      return false;
    }
    try {
      return customerBeneficiaryHistoryRepository.isTrustedBeneficiary(customerId, beneficiaryId);
    } catch (Exception e) {
      log.warn("Erro ao verificar beneficiário confiável: {}", e.getMessage());
      return false;
    }
  }
}
