CREATE TABLE IF NOT EXISTS block (
  block_id INTEGER PRIMARY KEY,

  block_no UNSIGNED INTEGER NOT NULL,
  slot_no UNSIGNED INTEGER NOT NULL,

  hash BLOB NOT NULL,
  UNIQUE (block_no, slot_no, hash)
);

CREATE TABLE IF NOT EXISTS execution_context (
  context_id INTEGER PRIMARY KEY,

  block_id INTEGER NOT NULL REFERENCES block (block_id),
  cost_model_params_id INTEGER NOT NULL REFERENCES cost_model_params (params_id),

  transaction_hash BLOB NOT NULL,
  script_hash BLOB NOT NULL,
  script_name TEXT,

  ledger_language SMALLINT NOT NULL, 
  major_protocol_version SMALLINT NOT NULL,
  datum BLOB,
  redeemer BLOB,
  script_context BLOB NOT NULL,

  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS cost_model_params (
  params_id INTEGER PRIMARY KEY,
  params BLOB NOT NULL,
  UNIQUE (params)
);

CREATE TABLE IF NOT EXISTS execution_event (
  execution_id INTEGER PRIMARY KEY, 
  context_id INTEGER NOT NULL REFERENCES execution_context (context_id),

  trace_logs TEXT NOT NULL,
  eval_error TEXT,
  exec_budget_cpu INTEGER NOT NULL,
  exec_budget_mem INTEGER NOT NULL,

  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS cancellation_event (
  block_id INTEGER NOT NULL REFERENCES block (block_id),
  script_hash BLOB NOT NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS selection_event (
  block_id INTEGER NOT NULL REFERENCES block (block_id),
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
)
