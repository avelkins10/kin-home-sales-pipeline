// lib/db/client.ts

import { sql as vercelSql } from '@vercel/postgres';

// If DATABASE_URL is set but POSTGRES_URL is not, use DATABASE_URL
// This helps with scripts that run outside Next.js context
if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  // @vercel/postgres will use DATABASE_URL automatically if POSTGRES_URL is not set
  // But we need to ensure it's available for the module
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

// Re-export sql for use throughout the application
export const sql = vercelSql;
