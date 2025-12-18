package com.rulex.homolog.adapter;

import com.rulex.dto.TransactionRequest;
import com.rulex.homolog.PayloadSanitizer;
import com.rulex.homolog.port.PayloadSanitizerPort;
import java.util.Map;
import org.springframework.stereotype.Component;

@Component
public class PayloadSanitizerAdapter implements PayloadSanitizerPort {

  private final PayloadSanitizer sanitizer;

  public PayloadSanitizerAdapter(PayloadSanitizer sanitizer) {
    this.sanitizer = sanitizer;
  }

  @Override
  public Map<String, Object> sanitize(TransactionRequest payload) {
    return sanitizer.sanitize(payload);
  }
}
