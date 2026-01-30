package com.rulex.core.simulation.port;

import com.rulex.entity.RuleConfiguration;
import com.rulex.entity.Transaction;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface RuleSimulationRepositoryPort {

  Optional<RuleConfiguration> findRuleById(Long ruleId);

  Page<Transaction> findTransactionsByFilters(
      LocalDateTime startDate, LocalDateTime endDate, Pageable pageable);

  List<Transaction> toList(Page<Transaction> page);
}
