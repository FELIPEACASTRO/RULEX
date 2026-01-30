package com.rulex.service;

import com.rulex.core.simulation.port.RuleSimulationInputPort;
import com.rulex.core.simulation.port.RuleSimulationRepositoryPort;
import com.rulex.core.simulation.port.RuleSimulationSerializerPort;
import com.rulex.core.simulation.usecase.RuleSimulationUseCase;
import com.rulex.core.simulation.usecase.RuleSimulationUseCase.BacktestResult;
import com.rulex.core.simulation.usecase.RuleSimulationUseCase.ComparisonResult;
import com.rulex.core.simulation.usecase.RuleSimulationUseCase.SimulationResult;
import com.rulex.dto.RuleConfigurationDTO;
import com.rulex.dto.TransactionRequest;
import java.time.LocalDateTime;
import java.util.List;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** Adapter Spring para simulação e teste de regras. */
@Service
@Transactional(readOnly = true)
public class RuleSimulationService implements RuleSimulationInputPort {

  private final RuleSimulationUseCase useCase;

  public RuleSimulationService(
      RuleSimulationRepositoryPort repositoryPort, RuleSimulationSerializerPort serializerPort) {
    this.useCase = new RuleSimulationUseCase(repositoryPort, serializerPort);
  }

  @Override
  public SimulationResult simulateRule(RuleConfigurationDTO rule, TransactionRequest testPayload) {
    return useCase.simulateRule(rule, testPayload);
  }

  @Override
  public BacktestResult backtestRule(
      Long ruleId, LocalDateTime startDate, LocalDateTime endDate, int sampleSize) {
    return useCase.backtestRule(ruleId, startDate, endDate, sampleSize);
  }

  @Override
  public ComparisonResult compareRules(
      RuleConfigurationDTO ruleA,
      RuleConfigurationDTO ruleB,
      List<TransactionRequest> testPayloads) {
    return useCase.compareRules(ruleA, ruleB, testPayloads);
  }
}

