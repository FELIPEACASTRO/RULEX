/**
 * OPERATOR_SPECS_BACKEND_ONLY
 *
 * Filtra o catálogo para conter APENAS operadores existentes no backend.
 * Garante sincronização entre Front, Back e Banco.
 */

import { BACKEND_OPERATORS } from "./generated/backendOperators.generated";
import { OPERATOR_SPECS_COMPLETE } from "./operatorSpecsIndex";
import type { OperatorSpec } from "./operatorSpecs";

export const OPERATOR_SPECS_BACKEND_ONLY: Record<string, OperatorSpec> =
  Object.fromEntries(
    BACKEND_OPERATORS.map((op) => [op.name, OPERATOR_SPECS_COMPLETE[op.name]])
  );

export const BACKEND_OPERATOR_NAMES = BACKEND_OPERATORS.map((op) => op.name);

export const BACKEND_SPEC_STATS = {
  backendCount: BACKEND_OPERATOR_NAMES.length,
  specCount: Object.keys(OPERATOR_SPECS_BACKEND_ONLY).length,
  missingSpecs: BACKEND_OPERATOR_NAMES.filter(
    (name) => !OPERATOR_SPECS_BACKEND_ONLY[name]
  )
};

if (process.env.NODE_ENV === "development") {
  if (BACKEND_SPEC_STATS.missingSpecs.length > 0) {
    // eslint-disable-next-line no-console
    console.warn("⚠️ Missing specs for backend operators:", BACKEND_SPEC_STATS);
  }
}
