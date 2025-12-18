package com.rulex.homolog.port;

import com.rulex.dto.TransactionRequest;
import java.util.Map;

public interface PayloadSanitizerPort {
  Map<String, Object> sanitize(TransactionRequest payload);
}
