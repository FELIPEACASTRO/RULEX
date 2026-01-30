package com.rulex.core.transaction.port;

import com.rulex.dto.TriggeredRuleDTO;
import java.util.List;

public interface TriggeredRuleParserPort {
  List<TriggeredRuleDTO> parseTriggeredRules(String rulesApplied);
}
