#!/usr/bin/env node

const path = require('path');
require('dotenv').config({ path: path.join(process.cwd(), '.env.local') });
const { sql } = require('@vercel/postgres');

(async () => {
  const result = await sql`SELECT email, quickbase_user_id, name FROM users WHERE email = 'addison.r@kinhome.com'`;
  console.log('Addison user record:', result.rows[0]);
  process.exit(0);
})();
