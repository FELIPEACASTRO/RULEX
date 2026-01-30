package com.rulex.adapter.engine;

import com.rulex.core.engine.port.RuleEngineGraphPort;
import com.rulex.service.Neo4jGraphService;
import org.springframework.stereotype.Component;

@Component
public class RuleEngineGraphAdapter implements RuleEngineGraphPort {

  private final Neo4jGraphService graphService;

  public RuleEngineGraphAdapter(Neo4jGraphService graphService) {
    this.graphService = graphService;
  }

  @Override
  public boolean isCurrentlyAvailable() {
    return graphService.isCurrentlyAvailable();
  }

  @Override
  public void recordTransaction(String fromAccount, String toAccount, double amount, long timestamp) {
    graphService.recordTransaction(fromAccount, toAccount, amount, timestamp);
  }
}
