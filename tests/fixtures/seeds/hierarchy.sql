-- Test seed data for user hierarchies and office assignments
-- This file provides test data for authorization testing

-- Insert test users with different roles
INSERT INTO users (id, email, role, name, is_active, created_at) VALUES
  -- Office Leader A
  ('user-office-leader-a', 'office.leader.a@test.com', 'office_leader', 'Office Leader A', true, NOW()),
  
  -- Team Lead B
  ('user-team-lead-b', 'team.lead.b@test.com', 'team_lead', 'Team Lead B', true, NOW()),
  
  -- Reps C and D (managed by Team Lead B)
  ('user-rep-c', 'rep.c@test.com', 'closer', 'Rep C', true, NOW()),
  ('user-rep-d', 'rep.d@test.com', 'setter', 'Rep D', true, NOW()),
  
  -- Reps E and F (not managed by anyone)
  ('user-rep-e', 'rep.e@test.com', 'closer', 'Rep E', true, NOW()),
  ('user-rep-f', 'rep.f@test.com', 'setter', 'Rep F', true, NOW()),
  
  -- Admin user
  ('user-admin', 'admin@test.com', 'super_admin', 'Admin User', true, NOW()),
  
  -- Regional user
  ('user-regional', 'regional@test.com', 'regional', 'Regional User', true, NOW()),
  
  -- Inactive user (should not affect queries)
  ('user-inactive', 'inactive@test.com', 'closer', 'Inactive User', false, NOW())
ON CONFLICT (id) DO UPDATE SET
  email = EXCLUDED.email,
  role = EXCLUDED.role,
  name = EXCLUDED.name,
  is_active = EXCLUDED.is_active;

-- Insert office assignments for Office Leader A
INSERT INTO office_assignments (user_id, office_name, access_level, assigned_at) VALUES
  ('user-office-leader-a', 'Office A', 'manage', NOW()),
  ('user-office-leader-a', 'Office B', 'manage', NOW())
ON CONFLICT (user_id, office_name) DO UPDATE SET
  access_level = EXCLUDED.access_level,
  assigned_at = EXCLUDED.assigned_at;

-- Insert user hierarchies (Team Lead B manages Reps C and D)
INSERT INTO user_hierarchies (manager_id, user_id, created_at) VALUES
  ('user-team-lead-b', 'user-rep-c', NOW()),
  ('user-team-lead-b', 'user-rep-d', NOW())
ON CONFLICT (manager_id, user_id) DO NOTHING;

-- Insert additional test data for edge cases
INSERT INTO users (id, email, role, name, is_active, created_at) VALUES
  -- User with special characters in email
  ('user-special', 'user.name+tag@test.com', 'closer', 'Special User', true, NOW()),
  
  -- User with single quote in email (for SQL injection testing)
  ('user-quote', 'user''name@test.com', 'setter', 'Quote User', true, NOW()),
  
  -- User with no email (edge case)
  ('user-no-email', NULL, 'closer', 'No Email User', true, NOW())
ON CONFLICT (id) DO UPDATE SET
  email = EXCLUDED.email,
  role = EXCLUDED.role,
  name = EXCLUDED.name,
  is_active = EXCLUDED.is_active;

-- Insert office assignments for additional test cases
INSERT INTO office_assignments (user_id, office_name, access_level, assigned_at) VALUES
  -- Office Leader with single office
  ('user-office-leader-a', 'Office C', 'view', NOW()),
  
  -- Office Leader with many offices (for performance testing)
  ('user-office-leader-a', 'Office D', 'admin', NOW()),
  ('user-office-leader-a', 'Office E', 'manage', NOW()),
  ('user-office-leader-a', 'Office F', 'view', NOW())
ON CONFLICT (user_id, office_name) DO UPDATE SET
  access_level = EXCLUDED.access_level,
  assigned_at = EXCLUDED.assigned_at;

-- Insert additional user hierarchies for testing
INSERT INTO user_hierarchies (manager_id, user_id, created_at) VALUES
  -- Team Lead managing users with special email characters
  ('user-team-lead-b', 'user-special', NOW()),
  ('user-team-lead-b', 'user-quote', NOW())
ON CONFLICT (manager_id, user_id) DO NOTHING;

-- Insert test data for performance testing
INSERT INTO users (id, email, role, name, is_active, created_at) VALUES
  -- Multiple users for team lead performance testing
  ('user-perf-1', 'perf1@test.com', 'closer', 'Perf User 1', true, NOW()),
  ('user-perf-2', 'perf2@test.com', 'setter', 'Perf User 2', true, NOW()),
  ('user-perf-3', 'perf3@test.com', 'closer', 'Perf User 3', true, NOW()),
  ('user-perf-4', 'perf4@test.com', 'setter', 'Perf User 4', true, NOW()),
  ('user-perf-5', 'perf5@test.com', 'closer', 'Perf User 5', true, NOW())
ON CONFLICT (id) DO UPDATE SET
  email = EXCLUDED.email,
  role = EXCLUDED.role,
  name = EXCLUDED.name,
  is_active = EXCLUDED.is_active;

-- Insert performance test hierarchies
INSERT INTO user_hierarchies (manager_id, user_id, created_at) VALUES
  ('user-team-lead-b', 'user-perf-1', NOW()),
  ('user-team-lead-b', 'user-perf-2', NOW()),
  ('user-team-lead-b', 'user-perf-3', NOW()),
  ('user-team-lead-b', 'user-perf-4', NOW()),
  ('user-team-lead-b', 'user-perf-5', NOW())
ON CONFLICT (manager_id, user_id) DO NOTHING;

-- Create a second team lead for testing multiple team leads
INSERT INTO users (id, email, role, name, is_active, created_at) VALUES
  ('user-team-lead-2', 'team.lead.2@test.com', 'team_lead', 'Team Lead 2', true, NOW())
ON CONFLICT (id) DO UPDATE SET
  email = EXCLUDED.email,
  role = EXCLUDED.role,
  name = EXCLUDED.name,
  is_active = EXCLUDED.is_active;

-- Insert hierarchies for second team lead
INSERT INTO user_hierarchies (manager_id, user_id, created_at) VALUES
  ('user-team-lead-2', 'user-rep-e', NOW()),
  ('user-team-lead-2', 'user-rep-f', NOW())
ON CONFLICT (manager_id, user_id) DO NOTHING;

-- Insert test data for edge cases
INSERT INTO users (id, email, role, name, is_active, created_at) VALUES
  -- User with empty email
  ('user-empty-email', '', 'closer', 'Empty Email User', true, NOW()),
  
  -- User with whitespace email
  ('user-whitespace-email', '   ', 'setter', 'Whitespace Email User', true, NOW())
ON CONFLICT (id) DO UPDATE SET
  email = EXCLUDED.email,
  role = EXCLUDED.role,
  name = EXCLUDED.name,
  is_active = EXCLUDED.is_active;

-- Insert test data for office assignments edge cases
INSERT INTO office_assignments (user_id, office_name, access_level, assigned_at) VALUES
  -- Office assignment with empty office name
  ('user-office-leader-a', '', 'view', NOW()),
  
  -- Office assignment with whitespace office name
  ('user-office-leader-a', '   ', 'view', NOW())
ON CONFLICT (user_id, office_name) DO UPDATE SET
  access_level = EXCLUDED.access_level,
  assigned_at = EXCLUDED.assigned_at;
