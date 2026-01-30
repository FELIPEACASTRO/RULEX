package com.rulex.core.engine.port;

public interface RuleEngineGraphPort {

  boolean isCurrentlyAvailable();

  void recordTransaction(String fromAccount, String toAccount, double amount, long timestamp);
}
