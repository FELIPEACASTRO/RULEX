package com.rulex.v31.field;

import java.util.List;
import org.springframework.stereotype.Service;

@Service
public class FieldDictionaryService {

  private final FieldDictionaryRepository repository;

  public FieldDictionaryService(FieldDictionaryRepository repository) {
    this.repository = repository;
  }

  public List<FieldDictionaryEntity> list(String workflow, String recordType, String portfolio) {
    return repository.findByFilters(
        nullIfBlank(workflow), nullIfBlank(recordType), nullIfBlank(portfolio));
  }

  private String nullIfBlank(String s) {
    if (s == null || s.isBlank()) {
      return null;
    }
    return s;
  }
}
