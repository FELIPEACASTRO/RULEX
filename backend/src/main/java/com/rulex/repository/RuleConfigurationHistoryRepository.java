package com.rulex.repository;

import com.rulex.entity.RuleConfigurationHistory;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface RuleConfigurationHistoryRepository extends JpaRepository<RuleConfigurationHistory, Long> {

    List<RuleConfigurationHistory> findByRuleIdOrderByCreatedAtDesc(Long ruleId);
}
