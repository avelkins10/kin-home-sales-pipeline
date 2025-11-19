-- Link Users to RepCard by Email
-- 
-- This script links users in the users table to RepCard users
-- by matching email addresses. It only updates users where
-- repcard_user_id is currently NULL.
--
-- Usage:
--   psql "$DATABASE_URL" -f scripts/link-users-to-repcard.sql
--
-- Or connect to database and run:
--   \i scripts/link-users-to-repcard.sql

BEGIN;

-- Show current linking status
SELECT 
  'Before Linking' as status,
  COUNT(*) FILTER (WHERE repcard_user_id IS NOT NULL) as linked_users,
  COUNT(*) FILTER (WHERE repcard_user_id IS NULL) as unlinked_users,
  COUNT(*) as total_users
FROM users;

-- Link users by matching email addresses (case-insensitive)
UPDATE users u
SET repcard_user_id = ru.repcard_user_id::text
FROM repcard_users ru
WHERE LOWER(TRIM(u.email)) = LOWER(TRIM(ru.email))
  AND u.repcard_user_id IS NULL
  AND ru.repcard_user_id IS NOT NULL;

-- Show results
SELECT 
  'After Linking' as status,
  COUNT(*) FILTER (WHERE repcard_user_id IS NOT NULL) as linked_users,
  COUNT(*) FILTER (WHERE repcard_user_id IS NULL) as unlinked_users,
  COUNT(*) as total_users
FROM users;

-- Show sample of linked users
SELECT 
  u.id,
  u.name,
  u.email,
  u.role,
  u.repcard_user_id,
  ru.first_name || ' ' || ru.last_name as repcard_name
FROM users u
INNER JOIN repcard_users ru ON u.repcard_user_id::text = ru.repcard_user_id::text
LIMIT 10;

-- Show users that couldn't be linked (for manual review)
SELECT 
  u.id,
  u.name,
  u.email,
  u.role,
  'No matching RepCard user found' as reason
FROM users u
WHERE u.repcard_user_id IS NULL
  AND u.email IS NOT NULL
  AND u.email != ''
LIMIT 20;

COMMIT;

