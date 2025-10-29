-- Migration 019: Create arrivy_task_attachments table
-- Stores metadata for attachments/photos uploaded with task status updates
-- Files are stored in Arrivy's CDN; we cache metadata locally for performance

CREATE TABLE IF NOT EXISTS arrivy_task_attachments (
  id SERIAL PRIMARY KEY,
  arrivy_task_id BIGINT NOT NULL,
  arrivy_status_id BIGINT NOT NULL,
  file_id BIGINT NOT NULL UNIQUE,  -- Arrivy's unique file identifier
  file_path TEXT NOT NULL,          -- URL/path to file in Arrivy CDN
  filename TEXT NOT NULL,            -- Original filename
  uploaded_by TEXT,                  -- Name of person who uploaded
  uploaded_at TIMESTAMPTZ NOT NULL,  -- When file was uploaded
  created_at TIMESTAMPTZ DEFAULT NOW(),
  updated_at TIMESTAMPTZ DEFAULT NOW(),

  -- Foreign key to main task table
  CONSTRAINT fk_arrivy_task
    FOREIGN KEY (arrivy_task_id)
    REFERENCES arrivy_tasks(arrivy_task_id)
    ON DELETE CASCADE
);

-- Index for fast lookups by task (most common query pattern)
CREATE INDEX IF NOT EXISTS idx_arrivy_task_attachments_task_id
  ON arrivy_task_attachments(arrivy_task_id);

-- Index for lookups by status (for linking to specific status updates)
CREATE INDEX IF NOT EXISTS idx_arrivy_task_attachments_status_id
  ON arrivy_task_attachments(arrivy_status_id);

-- Index for duplicate detection during sync
CREATE INDEX IF NOT EXISTS idx_arrivy_task_attachments_file_id
  ON arrivy_task_attachments(file_id);

-- Index for sorting by upload time
CREATE INDEX IF NOT EXISTS idx_arrivy_task_attachments_uploaded_at
  ON arrivy_task_attachments(uploaded_at DESC);

-- Comment for documentation
COMMENT ON TABLE arrivy_task_attachments IS 'Cached metadata for attachments/photos from Arrivy task status updates. Actual files hosted by Arrivy.';
COMMENT ON COLUMN arrivy_task_attachments.file_path IS 'URL to file in Arrivy CDN (e.g., https://app.arrivy.com/files/...)';
COMMENT ON COLUMN arrivy_task_attachments.file_id IS 'Arrivy unique file identifier - ensures no duplicates';
