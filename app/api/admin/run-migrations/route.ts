import { NextRequest, NextResponse } from 'next/server';
import { sql } from '@/lib/db/client';
import { readFileSync } from 'fs';
import { resolve } from 'path';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';

/**
 * Run Pending Migrations API
 * 
 * Safely runs pending migrations (031, 032, 033) that fix critical issues.
 * This endpoint should only be accessible to admins.
 */

const PENDING_MIGRATIONS = [
  '031_fix_48_hour_speed_calculation.sql',
  '032_repcard_event_driven_metrics.sql',
  '033_fix_repcard_metric_audit_fk.sql',
  '034_fix_metric_audit_fk_use_repcard_id.sql',
  '035_add_useful_webhook_fields.sql',
  '038_change_48h_to_2_calendar_days.sql',
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

    // Check current status first
    const status = {
      hasMetricAudit: false,
      hasDeferrableFK: false,
      hasTrigger: false,
    };

    try {
      const metricAuditCheck = await sql`
        SELECT EXISTS (
          SELECT FROM information_schema.tables
          WHERE table_schema = 'public'
          AND table_name = 'repcard_metric_audit'
        );
      `;
      status.hasMetricAudit = (metricAuditCheck[0] as any)?.exists || false;

      if (status.hasMetricAudit) {
        const fkCheck = await sql`
          SELECT conname, condeferred, condeferrable
          FROM pg_constraint
          WHERE conrelid = 'repcard_metric_audit'::regclass
          AND conname = 'repcard_metric_audit_appointment_id_fkey';
        `;
        const fk = fkCheck[0] as any;
        if (fk) {
          status.hasDeferrableFK = fk.condeferrable === 't' || fk.condeferred === 't';
        }
      }

      const triggerCheck = await sql`
        SELECT tgname
        FROM pg_trigger
        WHERE tgname = 'trigger_update_appointment_metrics';
      `;
      status.hasTrigger = triggerCheck.length > 0;
    } catch (error) {
      console.error('[Migrations] Error checking status:', error);
    }

    // Run each migration
    for (const migrationFile of PENDING_MIGRATIONS) {
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
    const finalStatus = {
      hasMetricAudit: false,
      hasDeferrableFK: false,
      hasTrigger: false,
    };

    try {
      const metricAuditCheck = await sql`
        SELECT EXISTS (
          SELECT FROM information_schema.tables
          WHERE table_schema = 'public'
          AND table_name = 'repcard_metric_audit'
        );
      `;
      finalStatus.hasMetricAudit = (metricAuditCheck[0] as any)?.exists || false;

      if (finalStatus.hasMetricAudit) {
        const fkCheck = await sql`
          SELECT conname, condeferred, condeferrable
          FROM pg_constraint
          WHERE conrelid = 'repcard_metric_audit'::regclass
          AND conname = 'repcard_metric_audit_appointment_id_fkey';
        `;
        const fk = fkCheck[0] as any;
        if (fk) {
          finalStatus.hasDeferrableFK = fk.condeferrable === 't' || fk.condeferred === 't';
        }
      }

      const triggerCheck = await sql`
        SELECT tgname
        FROM pg_trigger
        WHERE tgname = 'trigger_update_appointment_metrics';
      `;
      finalStatus.hasTrigger = triggerCheck.length > 0;
    } catch (error) {
      console.error('[Migrations] Error checking final status:', error);
    }

    return NextResponse.json({
      success: results.failed.length === 0,
      initialStatus: status,
      finalStatus,
      results: {
        success: results.success,
        skipped: results.skipped,
        failed: results.failed,
      },
      message: results.failed.length === 0
        ? 'All migrations completed successfully'
        : `${results.failed.length} migration(s) failed`,
    });
  } catch (error: any) {
    console.error('[Migrations] Fatal error:', error);
    return NextResponse.json(
      {
        success: false,
        error: error.message || String(error),
      },
      { status: 500 }
    );
  }
}

export async function GET(request: NextRequest) {
  try {
    // Check migration status without running
    const status = {
      hasMetricAudit: false,
      hasDeferrableFK: false,
      hasTrigger: false,
      migrations: [] as Array<{ file: string; status: string }>,
    };

    try {
      const metricAuditCheck = await sql`
        SELECT EXISTS (
          SELECT FROM information_schema.tables
          WHERE table_schema = 'public'
          AND table_name = 'repcard_metric_audit'
        );
      `;
      status.hasMetricAudit = (metricAuditCheck[0] as any)?.exists || false;

      if (status.hasMetricAudit) {
        const fkCheck = await sql`
          SELECT conname, condeferred, condeferrable
          FROM pg_constraint
          WHERE conrelid = 'repcard_metric_audit'::regclass
          AND conname = 'repcard_metric_audit_appointment_id_fkey';
        `;
        const fk = fkCheck[0] as any;
        if (fk) {
          status.hasDeferrableFK = fk.condeferrable === 't' || fk.condeferred === 't';
        }
      }

      const triggerCheck = await sql`
        SELECT tgname
        FROM pg_trigger
        WHERE tgname = 'trigger_update_appointment_metrics';
      `;
      status.hasTrigger = triggerCheck.length > 0;

      // Check each migration file
      for (const migrationFile of PENDING_MIGRATIONS) {
        let migrationStatus = 'unknown';
        
        if (migrationFile === '032_repcard_event_driven_metrics.sql') {
          migrationStatus = status.hasMetricAudit && status.hasTrigger ? 'applied' : 'pending';
        } else if (migrationFile === '033_fix_repcard_metric_audit_fk.sql') {
          migrationStatus = status.hasDeferrableFK ? 'applied' : 'pending';
        } else if (migrationFile === '031_fix_48_hour_speed_calculation.sql') {
          migrationStatus = status.hasTrigger ? 'applied' : 'pending';
        } else if (migrationFile === '034_fix_metric_audit_fk_use_repcard_id.sql') {
          // Check if repcard_appointment_id column exists
          try {
            const colCheck = await sql`
              SELECT EXISTS (
                SELECT FROM information_schema.columns
                WHERE table_name = 'repcard_metric_audit'
                  AND column_name = 'repcard_appointment_id'
              )
            `;
            migrationStatus = (colCheck[0] as any)?.exists ? 'applied' : 'pending';
          } catch {
            migrationStatus = 'pending';
          }
        } else if (migrationFile === '035_add_useful_webhook_fields.sql') {
          // Check if new columns exist
          try {
            const colCheck = await sql`
              SELECT COUNT(*)::int as count
              FROM information_schema.columns
              WHERE table_name = 'repcard_appointments'
                AND column_name IN ('appointment_link', 'remind_at', 'contact_source')
            `;
            migrationStatus = (colCheck[0] as any)?.count >= 3 ? 'applied' : 'pending';
          } catch {
            migrationStatus = 'pending';
          }
        } else if (migrationFile === '038_change_48h_to_2_calendar_days.sql') {
          // Check if function uses calendar days (check function definition)
          try {
            const funcCheck = await sql`
              SELECT pg_get_functiondef(oid) as definition
              FROM pg_proc
              WHERE proname = 'calculate_is_within_48_hours'
              LIMIT 1
            `;
            const funcDef = (funcCheck[0] as any)?.definition || '';
            // Migration 038 changes from hours_diff to days_diff
            migrationStatus = funcDef.includes('days_diff') && funcDef.includes('calendar days') ? 'applied' : 'pending';
          } catch {
            migrationStatus = 'pending';
          }
        }

        status.migrations.push({
          file: migrationFile,
          status: migrationStatus,
        });
      }
    } catch (error) {
      console.error('[Migrations] Error checking status:', error);
    }

    return NextResponse.json({
      status,
      pendingMigrations: status.migrations.filter(m => m.status === 'pending').map(m => m.file),
    });
  } catch (error: any) {
    console.error('[Migrations] Error:', error);
    return NextResponse.json(
      {
        error: error.message || String(error),
      },
      { status: 500 }
    );
  }
}
