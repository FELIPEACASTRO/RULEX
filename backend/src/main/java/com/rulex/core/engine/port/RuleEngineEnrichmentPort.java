package com.rulex.core.engine.port;

import com.rulex.dto.TransactionRequest;
import com.rulex.service.enrichment.TransactionEnrichmentFacade;

public interface RuleEngineEnrichmentPort {

  TransactionEnrichmentFacade.FullEnrichmentContext enrichFull(TransactionRequest request);
}
