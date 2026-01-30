package com.rulex.core.approval.port;

import com.rulex.dto.RuleConfigurationDTO;

public interface RuleApprovalSerializerPort {

  String serialize(RuleConfigurationDTO dto);

  RuleConfigurationDTO deserialize(String json);
}
