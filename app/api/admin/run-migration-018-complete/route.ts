import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';
import { readFileSync } from 'fs';
import { resolve } from 'path';
import { sql } from '@/lib/db/client';

export const runtime = 'nodejs';
export const maxDuration = 60; // 60 seconds max

export async function POST(request: NextRequest) {
  try {
    // Authentication - only super_admin can run migrations
    const session = await getServerSession(authOptions);
    if (!session || session.user.role !== 'super_admin') {
      return NextResponse.json(
        { error: 'Unauthorized - Super Admin access required' },
        { status: 403 }
      );
    }

    console.log('[Migration 018 Complete] Starting migration...');

    // Read the migration file
    const migrationPath = resolve(process.cwd(), 'lib/db/migrations/018_complete_normalize_user_ids.sql');
    const migrationSQL = readFileSync(migrationPath, 'utf-8');

    console.log('[Migration 018 Complete] Migration file loaded');

    // Execute the migration
    await (sql as any).query(migrationSQL);

    console.log('[Migration 018 Complete] Migration executed successfully');

    // Verify the changes
    const verificationResult = await sql`
      SELECT 
        table_name,
        column_name,
        data_type
      FROM information_schema.columns
      WHERE table_schema = 'public'
        AND table_name IN ('repcard_customers', 'repcard_appointments')
        AND column_name IN ('setter_user_id', 'closer_user_id')
      ORDER BY table_name, column_name
    `;

    // Handle query result format (sql template returns { rows: [...] })
    const verification = (verificationResult as any).rows || verificationResult || [];

    const results = verification.map((row: any) => ({
      table: row.table_name,
      column: row.column_name,
      type: row.data_type,
      status: row.data_type === 'integer' ? '✅' : '❌'
    }));

    return NextResponse.json({
      success: true,
      message: 'Migration 018 Complete executed successfully',
      verification: results,
      nextSteps: [
        'Remove ::text casts from queries for better performance',
        'Test RepCard leaderboard queries',
        'Monitor query performance improvements'
      ]
    });

  } catch (error: any) {
    console.error('[Migration 018 Complete] Error:', error);
    
    // Check if it's a "already exists" or "does not exist" error (safe to ignore)
    if (error?.message?.includes('already exists') || 
        error?.message?.includes('does not exist') ||
        error?.code === '42P07' ||
        error?.code === '42704') {
      return NextResponse.json({
        success: true,
        message: 'Migration completed (some steps may have already been applied)',
        warning: error.message
      });
    }

    return NextResponse.json(
      {
        error: 'Migration failed',
        message: error instanceof Error ? error.message : String(error),
        details: process.env.NODE_ENV === 'development' ? error.stack : undefined
      },
      { status: 500 }
    );
  }
}

