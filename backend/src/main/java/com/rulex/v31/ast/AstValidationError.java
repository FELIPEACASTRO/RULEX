package com.rulex.v31.ast;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class AstValidationError {
  private String path;
  private String message;
}
