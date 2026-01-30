package com.rulex.adapter.engine;

import com.rulex.core.engine.port.RuleEngineEnrichmentPort;
import com.rulex.dto.TransactionRequest;
import com.rulex.service.enrichment.TransactionEnrichmentFacade;
import org.springframework.stereotype.Component;

@Component
public class RuleEngineEnrichmentAdapter implements RuleEngineEnrichmentPort {

  private final TransactionEnrichmentFacade enrichmentFacade;

  public RuleEngineEnrichmentAdapter(TransactionEnrichmentFacade enrichmentFacade) {
    this.enrichmentFacade = enrichmentFacade;
  }

  @Override
  public TransactionEnrichmentFacade.FullEnrichmentContext enrichFull(TransactionRequest request) {
    return enrichmentFacade.enrichFull(request);
  }
}
