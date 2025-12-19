package com.rulex.v31.execlog;

/** Matches Postgres enum execution_event_type. */
public enum ExecutionEventType {
  EVALUATE,
  SIMULATE,
  ANTI_TAMPER,
  IDEMPOTENT_REPLAY
}
