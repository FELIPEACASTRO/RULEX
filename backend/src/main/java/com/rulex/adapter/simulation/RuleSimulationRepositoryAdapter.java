package com.rulex.adapter.simulation;

import com.rulex.core.simulation.port.RuleSimulationRepositoryPort;
import com.rulex.entity.RuleConfiguration;
import com.rulex.entity.Transaction;
import com.rulex.repository.RuleConfigurationRepository;
import com.rulex.repository.TransactionRepository;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Component;

@Component
public class RuleSimulationRepositoryAdapter implements RuleSimulationRepositoryPort {

  private final RuleConfigurationRepository ruleConfigRepository;
  private final TransactionRepository transactionRepository;

  public RuleSimulationRepositoryAdapter(
      RuleConfigurationRepository ruleConfigRepository,
      TransactionRepository transactionRepository) {
    this.ruleConfigRepository = ruleConfigRepository;
    this.transactionRepository = transactionRepository;
  }

  @Override
  public Optional<RuleConfiguration> findRuleById(Long ruleId) {
    return ruleConfigRepository.findById(ruleId);
  }

  @Override
  public Page<Transaction> findTransactionsByFilters(
      LocalDateTime startDate, LocalDateTime endDate, Pageable pageable) {
    return transactionRepository.findByFilters(
        null,
        null,
        null,
        null,
        null,
        startDate,
        endDate,
        pageable);
  }

  @Override
  public List<Transaction> toList(Page<Transaction> page) {
    return page.getContent();
  }
}
