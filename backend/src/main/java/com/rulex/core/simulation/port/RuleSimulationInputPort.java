package com.rulex.core.simulation.port;

import com.rulex.core.simulation.usecase.RuleSimulationUseCase.BacktestResult;
import com.rulex.core.simulation.usecase.RuleSimulationUseCase.ComparisonResult;
import com.rulex.core.simulation.usecase.RuleSimulationUseCase.SimulationResult;
import com.rulex.dto.RuleConfigurationDTO;
import com.rulex.dto.TransactionRequest;
import java.time.LocalDateTime;
import java.util.List;

public interface RuleSimulationInputPort {

  SimulationResult simulateRule(RuleConfigurationDTO rule, TransactionRequest testPayload);

  BacktestResult backtestRule(
      Long ruleId, LocalDateTime startDate, LocalDateTime endDate, int sampleSize);

  ComparisonResult compareRules(
      RuleConfigurationDTO ruleA,
      RuleConfigurationDTO ruleB,
      List<TransactionRequest> testPayloads);
}
