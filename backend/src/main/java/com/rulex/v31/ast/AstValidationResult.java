package com.rulex.v31.ast;

import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class AstValidationResult {
  private boolean valid;
  private List<AstValidationError> errors;
}
