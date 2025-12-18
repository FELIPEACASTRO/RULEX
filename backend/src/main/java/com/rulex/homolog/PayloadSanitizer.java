package com.rulex.homolog;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.TransactionRequest;
import java.util.LinkedHashMap;
import java.util.Map;
import org.springframework.stereotype.Component;

@Component
public class PayloadSanitizer {

  private final ObjectMapper objectMapper;

  public PayloadSanitizer(ObjectMapper objectMapper) {
    this.objectMapper = objectMapper;
  }

  public Map<String, Object> sanitize(TransactionRequest payload) {
    Map<String, Object> map =
        objectMapper.convertValue(payload, new TypeReference<LinkedHashMap<String, Object>>() {});
    Object pan = map.get("pan");
    if (pan instanceof String s) {
      map.put("pan", maskPan(s));
    }
    return map;
  }

  private String maskPan(String pan) {
    String digits = pan.replaceAll("\\s+", "");
    if (digits.length() < 10) {
      return "****";
    }
    String first6 = digits.substring(0, 6);
    String last4 = digits.substring(digits.length() - 4);
    return first6 + "******" + last4;
  }
}
