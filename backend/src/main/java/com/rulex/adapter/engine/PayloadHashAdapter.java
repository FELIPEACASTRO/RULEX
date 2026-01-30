package com.rulex.adapter.engine;

import com.rulex.core.engine.port.PayloadHashPort;
import com.rulex.service.PayloadHashService;
import org.springframework.stereotype.Component;

@Component
public class PayloadHashAdapter implements PayloadHashPort {

  private final PayloadHashService payloadHashService;

  public PayloadHashAdapter(PayloadHashService payloadHashService) {
    this.payloadHashService = payloadHashService;
  }

  @Override
  public String sha256Hex(byte[] payload) {
    return payloadHashService.sha256Hex(payload);
  }
}
