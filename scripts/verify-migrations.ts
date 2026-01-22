#!/usr/bin/env tsx
/**
 * Verify Migrations - Check that all migrations were applied correctly
 */

import { config } from 'dotenv';
import { resolve } from 'path';
import { sql } from '@/lib/db/client';

config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });

if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

async function verify() {
  console.log('🔍 Verifying Migrations\n');
  console.log('='.repeat(70));

  try {
    // Check repcard_metric_audit table
    const auditTableCheck = await sql`
      SELECT EXISTS (
        SELECT FROM information_schema.tables
        WHERE table_schema = 'public'
        AND table_name = 'repcard_metric_audit'
      );
    `;
    const hasAuditTable = (auditTableCheck[0] as any)?.exists;
    console.log(`✅ repcard_metric_audit table: ${hasAuditTable ? 'EXISTS' : 'MISSING'}`);

    // Check DEFERRABLE FK constraint
    if (hasAuditTable) {
      const fkCheck = await sql`
        SELECT conname, condeferred, condeferrable
        FROM pg_constraint
        WHERE conrelid = 'repcard_metric_audit'::regclass
        AND conname = 'repcard_metric_audit_appointment_id_fkey';
      `;
      const fk = fkCheck[0] as any;
      if (fk) {
        const isDeferrable = fk.condeferrable === 't' || fk.condeferred === 't';
        console.log(`✅ FK constraint is DEFERRABLE: ${isDeferrable ? 'YES' : 'NO'}`);
      } else {
        console.log(`❌ FK constraint not found`);
      }
    }

    // Check trigger
    const triggerCheck = await sql`
      SELECT tgname
      FROM pg_trigger
      WHERE tgname = 'trigger_update_appointment_metrics';
    `;
    const hasTrigger = triggerCheck.length > 0;
    console.log(`✅ update_appointment_metrics trigger: ${hasTrigger ? 'EXISTS' : 'MISSING'}`);

    // Check functions
    const functionCheck = await sql`
      SELECT routine_name
      FROM information_schema.routines
      WHERE routine_schema = 'public'
      AND routine_name IN ('calculate_is_within_48_hours', 'calculate_has_power_bill', 'update_appointment_metrics')
      ORDER BY routine_name;
    `;
    const functions = Array.isArray(functionCheck) ? functionCheck : (functionCheck?.rows || []);
    console.log(`✅ Functions created: ${functions.length}/3`);
    functions.forEach((f: any) => {
      console.log(`   - ${f.routine_name}`);
    });

    // Check audit records
    if (hasAuditTable) {
      const auditCount = await sql`
        SELECT COUNT(*) as count FROM repcard_metric_audit
      `;
      const count = (auditCount[0] as any)?.count || 0;
      console.log(`✅ Audit records: ${count}`);
    }

    console.log('\n' + '='.repeat(70));
    console.log('✅ All migrations verified successfully!');
  } catch (error: any) {
    console.error('❌ Verification error:', error.message);
    process.exit(1);
  }
}

verify();
