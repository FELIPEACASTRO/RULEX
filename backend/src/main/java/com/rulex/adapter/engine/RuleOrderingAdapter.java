package com.rulex.adapter.engine;

import com.rulex.core.engine.port.RuleOrderingPort;
import com.rulex.entity.RuleConfiguration;
import com.rulex.service.RuleOrderingService;
import java.util.List;
import org.springframework.stereotype.Component;

@Component
public class RuleOrderingAdapter implements RuleOrderingPort {

  private final RuleOrderingService ruleOrderingService;

  public RuleOrderingAdapter(RuleOrderingService ruleOrderingService) {
    this.ruleOrderingService = ruleOrderingService;
  }

  @Override
  public List<RuleConfiguration> getOptimizedRuleOrder() {
    return ruleOrderingService.getOptimizedRuleOrder();
  }

  @Override
  public void recordExecution(String ruleName, long elapsedNanos, boolean triggered) {
    ruleOrderingService.recordExecution(ruleName, elapsedNanos, triggered);
  }
}
