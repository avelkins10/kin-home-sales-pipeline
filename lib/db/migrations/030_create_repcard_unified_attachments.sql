-- Migration 030: Create Unified Attachment Views
-- This provides a single interface to query all attachments (customer + appointment)
-- and enables easier attachment counting and attribution

-- Create unified view of all RepCard attachments
CREATE OR REPLACE VIEW repcard_all_attachments AS
SELECT
  ca.id,
  ca.repcard_attachment_id,
  ca.repcard_customer_id as entity_id,
  'customer' as attachment_source,
  ca.file_name,
  ca.file_url,
  ca.file_type,
  ca.file_size,
  ca.uploaded_by_user_id,
  ca.uploaded_at,
  ca.created_at,
  ca.synced_at,
  -- Link to customer info
  c.setter_user_id as attributed_user_id,
  c.office_id,
  u.name as attributed_user_name,
  u.email as attributed_user_email
FROM repcard_customer_attachments ca
LEFT JOIN repcard_customers c ON c.repcard_customer_id::text = ca.repcard_customer_id::text
LEFT JOIN users u ON u.repcard_user_id = c.setter_user_id

UNION ALL

SELECT
  aa.id,
  aa.repcard_attachment_id,
  aa.repcard_appointment_id as entity_id,
  'appointment' as attachment_source,
  aa.file_name,
  aa.file_url,
  aa.file_type,
  aa.file_size,
  aa.uploaded_by_user_id,
  aa.uploaded_at,
  aa.created_at,
  aa.synced_at,
  -- Link to appointment setter (primary attribution)
  a.setter_user_id as attributed_user_id,
  a.office_id,
  u.name as attributed_user_name,
  u.email as attributed_user_email
FROM repcard_appointment_attachments aa
LEFT JOIN repcard_appointments a ON a.repcard_appointment_id::text = aa.repcard_appointment_id::text
LEFT JOIN users u ON u.repcard_user_id = a.setter_user_id;

-- Create view for customer attachment counts
CREATE OR REPLACE VIEW repcard_customer_attachment_summary AS
SELECT
  c.repcard_customer_id,
  c.id as customer_table_id,
  c.setter_user_id,
  -- Customer attachments
  COUNT(DISTINCT ca.id) as customer_attachment_count,
  -- Appointment attachments (all appointments for this customer)
  COUNT(DISTINCT aa.id) as appointment_attachment_count,
  -- Total attachments
  COUNT(DISTINCT ca.id) + COUNT(DISTINCT aa.id) as total_attachment_count,
  -- Has any attachments
  (COUNT(DISTINCT ca.id) + COUNT(DISTINCT aa.id)) > 0 as has_attachments,
  -- First and last attachment dates
  MIN(LEAST(ca.uploaded_at, aa.uploaded_at)) as first_attachment_date,
  MAX(GREATEST(ca.uploaded_at, aa.uploaded_at)) as last_attachment_date
FROM repcard_customers c
LEFT JOIN repcard_customer_attachments ca
  ON ca.repcard_customer_id::text = c.repcard_customer_id::text
LEFT JOIN repcard_appointments a
  ON a.repcard_customer_id::text = c.repcard_customer_id::text
LEFT JOIN repcard_appointment_attachments aa
  ON aa.repcard_appointment_id::text = a.repcard_appointment_id::text
GROUP BY c.repcard_customer_id, c.id, c.setter_user_id;

-- Create view for user attachment statistics
CREATE OR REPLACE VIEW repcard_user_attachment_stats AS
SELECT
  u.repcard_user_id,
  u.id as app_user_id,
  u.name as user_name,
  u.email as user_email,
  u.role as user_role,
  -- Count customers with attachments
  COUNT(DISTINCT CASE WHEN cas.has_attachments THEN c.repcard_customer_id END) as customers_with_attachments,
  COUNT(DISTINCT c.repcard_customer_id) as total_customers,
  -- Attachment rate
  CASE
    WHEN COUNT(DISTINCT c.repcard_customer_id) > 0
    THEN ROUND(
      100.0 * COUNT(DISTINCT CASE WHEN cas.has_attachments THEN c.repcard_customer_id END) /
      COUNT(DISTINCT c.repcard_customer_id)::decimal,
      2
    )
    ELSE 0
  END as attachment_rate_percentage,
  -- Total attachments uploaded
  COALESCE(SUM(cas.total_attachment_count), 0) as total_attachments_uploaded,
  -- Average attachments per customer
  CASE
    WHEN COUNT(DISTINCT c.repcard_customer_id) > 0
    THEN ROUND(
      COALESCE(SUM(cas.total_attachment_count), 0)::decimal /
      COUNT(DISTINCT c.repcard_customer_id)::decimal,
      2
    )
    ELSE 0
  END as avg_attachments_per_customer
FROM users u
LEFT JOIN repcard_customers c ON c.setter_user_id = u.repcard_user_id
LEFT JOIN repcard_customer_attachment_summary cas ON cas.repcard_customer_id::text = c.repcard_customer_id::text
WHERE u.repcard_user_id IS NOT NULL
GROUP BY u.repcard_user_id, u.id, u.name, u.email, u.role;

-- Create materialized view for faster queries (refresh periodically)
CREATE MATERIALIZED VIEW IF NOT EXISTS repcard_attachment_stats_snapshot AS
SELECT
  DATE(NOW()) as snapshot_date,
  COUNT(DISTINCT CASE WHEN attachment_source = 'customer' THEN id END) as total_customer_attachments,
  COUNT(DISTINCT CASE WHEN attachment_source = 'appointment' THEN id END) as total_appointment_attachments,
  COUNT(DISTINCT id) as total_attachments,
  COUNT(DISTINCT attributed_user_id) as users_with_attachments,
  AVG(file_size) as avg_file_size,
  COUNT(DISTINCT CASE WHEN file_type ILIKE '%image%' OR file_type ILIKE '%jpg%' OR file_type ILIKE '%png%' THEN id END) as image_count,
  COUNT(DISTINCT CASE WHEN file_type ILIKE '%pdf%' THEN id END) as pdf_count,
  MIN(uploaded_at) as earliest_attachment,
  MAX(uploaded_at) as latest_attachment
FROM repcard_all_attachments;

-- Create index on materialized view
CREATE UNIQUE INDEX IF NOT EXISTS idx_repcard_attachment_stats_snapshot_date
ON repcard_attachment_stats_snapshot(snapshot_date);

-- Create function to refresh attachment stats
CREATE OR REPLACE FUNCTION refresh_repcard_attachment_stats()
RETURNS void AS $$
BEGIN
  REFRESH MATERIALIZED VIEW CONCURRENTLY repcard_attachment_stats_snapshot;
END;
$$ LANGUAGE plpgsql;

-- Add comments
COMMENT ON VIEW repcard_all_attachments IS 'Unified view of all attachments from both customers and appointments';
COMMENT ON VIEW repcard_customer_attachment_summary IS 'Summary of attachment counts per customer including appointment attachments';
COMMENT ON VIEW repcard_user_attachment_stats IS 'Attachment statistics per user (attachment rate, total count, etc)';
COMMENT ON MATERIALIZED VIEW repcard_attachment_stats_snapshot IS 'Cached attachment statistics for performance - refresh daily';
COMMENT ON FUNCTION refresh_repcard_attachment_stats() IS 'Refreshes the materialized view of attachment statistics';
