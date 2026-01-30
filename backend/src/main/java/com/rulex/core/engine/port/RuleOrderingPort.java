package com.rulex.core.engine.port;

import com.rulex.entity.RuleConfiguration;
import java.util.List;

public interface RuleOrderingPort {

  List<RuleConfiguration> getOptimizedRuleOrder();

  void recordExecution(String ruleName, long elapsedNanos, boolean triggered);
}
