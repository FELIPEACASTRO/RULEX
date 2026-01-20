#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

COMPLEX_EVALUATOR="$ROOT_DIR/backend/src/main/java/com/rulex/service/complex/ComplexRuleEvaluator.java"
EVALUATOR_DIR="$ROOT_DIR/backend/src/main/java/com/rulex/service/complex/evaluator"

TMP_DIR="${TMPDIR:-/tmp}/rulex-operator-audit"
mkdir -p "$TMP_DIR"

SWITCH_CASES="$TMP_DIR/complex_switch_cases.txt"
EVALUATOR_OPS="$TMP_DIR/evaluator_operator_refs.txt"
EVALUATOR_UNIQUE="$TMP_DIR/evaluator_operator_unique.txt"

rg -n "^\s*case\s+[A-Z0-9_]+\s*->" "$COMPLEX_EVALUATOR" > "$SWITCH_CASES"
rg -n "ConditionOperator\.[A-Z0-9_]+" "$EVALUATOR_DIR" > "$EVALUATOR_OPS"

sed 's/.*case\s\+//' "$SWITCH_CASES" | sed 's/\s*->.*//' | sort | uniq > "$TMP_DIR/complex_case_unique.txt"

sed 's/.*ConditionOperator\.//' "$EVALUATOR_OPS" | sed 's/[^A-Z0-9_].*$//' | sort | uniq > "$EVALUATOR_UNIQUE"

printf "ComplexRuleEvaluator cases: %s\n" "$(wc -l < "$SWITCH_CASES")"
printf "ComplexRuleEvaluator unique cases: %s\n" "$(wc -l < "$TMP_DIR/complex_case_unique.txt")"
printf "Evaluator operator references: %s\n" "$(wc -l < "$EVALUATOR_OPS")"
printf "Evaluator unique operators: %s\n" "$(wc -l < "$EVALUATOR_UNIQUE")"

printf "\nOperators in switch AND in evaluators (duplicates):\n"
comm -12 "$TMP_DIR/complex_case_unique.txt" "$EVALUATOR_UNIQUE" | tee "$TMP_DIR/duplicate_ops.txt" >/dev/null
printf "Total duplicates: %s\n" "$(wc -l < "$TMP_DIR/duplicate_ops.txt")"

printf "\nOperators only in switch (needs migration):\n"
comm -23 "$TMP_DIR/complex_case_unique.txt" "$EVALUATOR_UNIQUE" | tee "$TMP_DIR/switch_only_ops.txt" >/dev/null
printf "Total switch-only: %s\n" "$(wc -l < "$TMP_DIR/switch_only_ops.txt")"

printf "\nOperators only in evaluators (not in switch):\n"
comm -13 "$TMP_DIR/complex_case_unique.txt" "$EVALUATOR_UNIQUE" | tee "$TMP_DIR/evaluator_only_ops.txt" >/dev/null
printf "Total evaluator-only: %s\n" "$(wc -l < "$TMP_DIR/evaluator_only_ops.txt")"
