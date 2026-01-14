package com.rulex.homolog.adapter;

import com.rulex.entity.homolog.RuleSetVersionItemEntity;
import com.rulex.homolog.port.RuleSetVersionItemPersistencePort;
import com.rulex.repository.homolog.RuleSetVersionItemRepository;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import org.springframework.stereotype.Component;

@Component
public class RuleSetVersionItemPersistenceAdapter implements RuleSetVersionItemPersistencePort {

  private final RuleSetVersionItemRepository ruleSetVersionItemRepository;

  public RuleSetVersionItemPersistenceAdapter(
      RuleSetVersionItemRepository ruleSetVersionItemRepository) {
    this.ruleSetVersionItemRepository = ruleSetVersionItemRepository;
  }

  @Override
  public RuleSetVersionItemEntity save(RuleSetVersionItemEntity item) {
    RuleSetVersionItemEntity entity = Objects.requireNonNull(item, "item");
    return Objects.requireNonNull(ruleSetVersionItemRepository.save(entity));
  }

  @Override
  public List<RuleSetVersionItemEntity> findByRuleSetVersionIdOrderBySortOrderAsc(
      UUID ruleSetVersionId) {
    return ruleSetVersionItemRepository.findByRuleSetVersionIdOrderBySortOrderAsc(ruleSetVersionId);
  }
}
