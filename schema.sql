CREATE TABLE
  IF NOT EXISTS block (
    hash BLOB PRIMARY KEY,
    block_no UNSIGNED INTEGER,
    slot_no UNSIGNED INTEGER NOT NULL
  ) WITHOUT ROWID;

CREATE INDEX IF NOT EXISTS idx_block_slot_no ON block (slot_no);

CREATE TABLE
  IF NOT EXISTS execution_context (
    context_id INTEGER PRIMARY KEY,
    block_hash BLOB NOT NULL REFERENCES block (hash),
    cost_model_params_id INTEGER NOT NULL REFERENCES cost_model_params (params_id),
    transaction_hash BLOB NOT NULL,
    target_script_hash BLOB NOT NULL,
    target_script_name TEXT,
    shadow_script_hash BLOB NOT NULL,
    shadow_script_name TEXT,
    ledger_language SMALLINT NOT NULL,
    major_protocol_version SMALLINT NOT NULL,
    datum BLOB,
    redeemer BLOB,
    script_context BLOB NOT NULL,
    exec_budget_max_cpu INTEGER NOT NULL,
    exec_budget_max_mem INTEGER NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  );

CREATE INDEX IF NOT EXISTS idx_execution_context_block_hash ON execution_context (block_hash);

CREATE INDEX IF NOT EXISTS idx_execution_context_created_at ON execution_context (created_at ASC);

CREATE TABLE
  IF NOT EXISTS cost_model_params (
    params_id INTEGER PRIMARY KEY,
    params BLOB NOT NULL,
    UNIQUE (params)
  );

CREATE TABLE
  IF NOT EXISTS execution_event (
    event_id INTEGER PRIMARY KEY,
    context_id INTEGER NOT NULL REFERENCES execution_context (context_id),
    trace_logs TEXT NOT NULL,
    eval_error TEXT,
    exec_budget_cpu INTEGER NOT NULL,
    exec_budget_mem INTEGER NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  );

CREATE INDEX IF NOT EXISTS idx_execution_event_created_at ON execution_event (created_at ASC);

CREATE TABLE
  IF NOT EXISTS rollback_event (
    event_id INTEGER PRIMARY KEY,
    block_hash BLOB NOT NULL REFERENCES block (hash),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  );

CREATE INDEX IF NOT EXISTS idx_rollback_event_block_hash ON rollback_event (block_hash);

CREATE INDEX IF NOT EXISTS idx_rollback_event_created_at ON rollback_event (created_at ASC);

CREATE TABLE
  IF NOT EXISTS rollback_block (
    event_id INTEGER NOT NULL REFERENCES rollback_event (event_id),
    block_hash BLOB NOT NULL REFERENCES block (hash)
  );

CREATE INDEX IF NOT EXISTS idx_rollback_block_event ON rollback_block (event_id);

CREATE INDEX IF NOT EXISTS idx_rollback_block_hash ON rollback_block (block_hash);

CREATE TABLE
  IF NOT EXISTS selection_event (
    event_id INTEGER PRIMARY KEY,
    block_hash BLOB NOT NULL REFERENCES block (hash),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  );

CREATE INDEX IF NOT EXISTS idx_selection_event_block_hash ON selection_event (block_hash);

CREATE INDEX IF NOT EXISTS idx_selection_event_created_at ON selection_event (created_at ASC)
-- Note: final statement should not have a semi-colon