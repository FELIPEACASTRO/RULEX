package com.rulex.repository;

import com.rulex.entity.RuleConfigurationHistory;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface RuleConfigurationHistoryRepository
    extends JpaRepository<RuleConfigurationHistory, Long> {

  List<RuleConfigurationHistory> findByRuleIdOrderByCreatedAtDesc(Long ruleId);
}
