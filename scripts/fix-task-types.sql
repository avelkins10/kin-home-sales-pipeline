-- Fix task types for existing Arrivy tasks
-- This updates all tasks to use the correct task type based on their template names

-- Update tasks with "Site Survey" template
UPDATE arrivy_tasks
SET task_type = 'Surveys - Site Survey',
    synced_at = NOW()
WHERE (template_id IS NOT NULL OR extra_fields IS NOT NULL)
  AND (
    template_id::text ILIKE '%survey%'
    OR extra_fields->>'task_type' ILIKE '%survey%'
    OR extra_fields->>'template' ILIKE '%site survey%'
  )
  AND task_type != 'Surveys - Site Survey';

-- Update tasks with "Install" template
UPDATE arrivy_tasks
SET task_type = 'Installations - General',
    synced_at = NOW()
WHERE (template_id IS NOT NULL OR extra_fields IS NOT NULL)
  AND (
    template_id::text ILIKE '%install%'
    OR extra_fields->>'task_type' ILIKE '%install%'
    OR extra_fields->>'template' ILIKE '%install%'
  )
  AND task_type NOT LIKE 'Installations -%';

-- Update tasks with "Inspection" template
UPDATE arrivy_tasks
SET task_type = 'Inspections - General',
    synced_at = NOW()
WHERE (template_id IS NOT NULL OR extra_fields IS NOT NULL)
  AND (
    template_id::text ILIKE '%inspection%'
    OR extra_fields->>'task_type' ILIKE '%inspection%'
    OR extra_fields->>'template' ILIKE '%inspection%'
  )
  AND task_type NOT LIKE 'Inspections -%';

-- Show results
SELECT
  task_type,
  COUNT(*) as count
FROM arrivy_tasks
GROUP BY task_type
ORDER BY count DESC;
