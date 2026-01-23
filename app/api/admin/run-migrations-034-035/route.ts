import { NextRequest, NextResponse } from 'next/server';
import { sql } from '@/lib/db/client';
import { readFileSync } from 'fs';
import { resolve } from 'path';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';

/**
 * Run Migrations 034 and 035 API
 * 
 * Safely runs migrations 034 and 035 that add new features.
 * This endpoint should only be accessible to admins.
 */

const NEW_MIGRATIONS = [
  '034_fix_metric_audit_fk_use_repcard_id.sql',
  '035_add_useful_webhook_fields.sql',
];

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

    const results = {
      success: [] as string[],
      skipped: [] as string[],
      failed: [] as Array<{ file: string; error: string }>,
    };

    // Run each migration
    for (const migrationFile of NEW_MIGRATIONS) {
      const migrationPath = resolve(process.cwd(), 'lib/db/migrations', migrationFile);

      try {
        console.log(`[Migrations] Running: ${migrationFile}`);

        const migrationSQL = readFileSync(migrationPath, 'utf-8');
        await (sql as any).query(migrationSQL);

        console.log(`[Migrations] ✅ ${migrationFile} completed`);
        results.success.push(migrationFile);
      } catch (error: any) {
        // Check if error is because object already exists (safe to skip)
        if (
          error?.message?.includes('already exists') ||
          error?.code === '42P07' || // table exists
          error?.code === '42710' || // object exists
          error?.code === '42P16' || // invalid table definition
          error?.code === '42704'    // object does not exist (for DROP IF EXISTS)
        ) {
          console.log(`[Migrations] ⚠️ ${migrationFile} - Already applied (skipping)`);
          results.skipped.push(migrationFile);
        } else {
          console.error(`[Migrations] ❌ ${migrationFile} failed:`, error.message);
          results.failed.push({
            file: migrationFile,
            error: error.message || String(error),
          });
        }
      }
    }

    // Verify final status
    const verification = {
      migration034: false,
      migration035: false,
    };

    try {
      // Check migration 034
      const check034 = await sql`
        SELECT EXISTS (
          SELECT FROM information_schema.columns
          WHERE table_name = 'repcard_metric_audit'
            AND column_name = 'repcard_appointment_id'
        )
      `;
      verification.migration034 = (check034[0] as any)?.exists || false;

      // Check migration 035
      const check035 = await sql`
        SELECT COUNT(*)::int as count
        FROM information_schema.columns
        WHERE table_name = 'repcard_appointments'
          AND column_name IN ('appointment_link', 'remind_at', 'contact_source')
      `;
      verification.migration035 = (check035[0] as any)?.count >= 3;
    } catch (error) {
      console.error('[Migrations] Error verifying status:', error);
    }

    return NextResponse.json({
      success: results.failed.length === 0,
      results,
      verification,
      message: results.failed.length === 0
        ? 'All migrations completed successfully'
        : `${results.failed.length} migration(s) failed`,
    });
  } catch (error: any) {
    console.error('[Migrations] Fatal error:', error);
    return NextResponse.json(
      {
        success: false,
        error: 'Failed to run migrations',
        message: error.message || 'Unknown error',
      },
      { status: 500 }
    );
  }
}

export async function GET(request: NextRequest) {
  try {
    // Check migration status without running
    const status = {
      migration034: false,
      migration035: false,
    };

    try {
      // Check migration 034
      const check034 = await sql`
        SELECT EXISTS (
          SELECT FROM information_schema.columns
          WHERE table_name = 'repcard_metric_audit'
            AND column_name = 'repcard_appointment_id'
        )
      `;
      status.migration034 = (check034[0] as any)?.exists || false;

      // Check migration 035
      const check035 = await sql`
        SELECT COUNT(*)::int as count
        FROM information_schema.columns
        WHERE table_name = 'repcard_appointments'
          AND column_name IN ('appointment_link', 'remind_at', 'contact_source')
      `;
      status.migration035 = (check035[0] as any)?.count >= 3;
    } catch (error) {
      console.error('[Migrations] Error checking status:', error);
    }

    return NextResponse.json({
      status,
      migrations: NEW_MIGRATIONS.map((file) => ({
        file,
        status: file.includes('034') 
          ? (status.migration034 ? 'applied' : 'pending')
          : (status.migration035 ? 'applied' : 'pending'),
      })),
    });
  } catch (error: any) {
    return NextResponse.json(
      {
        error: 'Failed to check migration status',
        message: error.message || 'Unknown error',
      },
      { status: 500 }
    );
  }
}
