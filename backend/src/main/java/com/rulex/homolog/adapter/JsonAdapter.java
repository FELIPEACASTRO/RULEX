package com.rulex.homolog.adapter;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.homolog.port.JsonPort;
import org.springframework.stereotype.Component;

@Component
public class JsonAdapter implements JsonPort {

  private final ObjectMapper objectMapper;

  public JsonAdapter(ObjectMapper objectMapper) {
    this.objectMapper = objectMapper;
  }

  @Override
  public String write(Object value) {
    try {
      return objectMapper.writeValueAsString(value);
    } catch (Exception e) {
      throw new IllegalArgumentException("Falha ao serializar JSON", e);
    }
  }
}
