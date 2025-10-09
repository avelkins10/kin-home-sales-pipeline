-- Add invited_by column to users table
-- This column stores the user ID of who invited the user

ALTER TABLE users 
ADD COLUMN IF NOT EXISTS invited_by UUID REFERENCES users(id);

-- Add comment for documentation
COMMENT ON COLUMN users.invited_by IS 'User ID of the person who sent the invite';
