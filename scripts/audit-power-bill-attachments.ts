#!/usr/bin/env tsx
import { sql } from '../lib/db/client';

/**
 * Audit script to check power bill attachment detection
 */
async function auditPowerBillAttachments() {
  console.log('🔍 Auditing Power Bill Attachment Detection...\n');

  try {
    // Check what attachment types and file names exist
    const attachmentTypes = await sql`
      SELECT 
        COALESCE(attachment_type, 'NULL') as attachment_type,
        COUNT(*)::int as count
      FROM (
        SELECT attachment_type FROM repcard_customer_attachments
        UNION ALL
        SELECT attachment_type FROM repcard_appointment_attachments
      ) combined
      GROUP BY attachment_type
      ORDER BY count DESC
      LIMIT 20
    `;
    console.log('📎 Attachment Types Found:');
    Array.from(attachmentTypes).forEach((row: any) => {
      console.log(`  ${row.attachment_type}: ${row.count}`);
    });

    // Check file names that might be power bills
    const powerBillFiles = await sql`
      SELECT 
        file_name,
        attachment_type,
        CASE 
          WHEN table_name = 'customer' THEN 'Customer'
          ELSE 'Appointment'
        END as source
      FROM (
        SELECT 
          file_name,
          attachment_type,
          'customer' as table_name
        FROM repcard_customer_attachments
        WHERE file_name IS NOT NULL
        UNION ALL
        SELECT 
          file_name,
          attachment_type,
          'appointment' as table_name
        FROM repcard_appointment_attachments
        WHERE file_name IS NOT NULL
      ) combined
      WHERE (
        file_name ILIKE '%power%' 
        OR file_name ILIKE '%bill%'
        OR file_name ILIKE '%electric%'
        OR file_name ILIKE '%utility%'
        OR attachment_type ILIKE '%power%'
        OR attachment_type ILIKE '%bill%'
      )
      ORDER BY file_name
      LIMIT 50
    `;
    console.log(`\n⚡ Power Bill Related Files (${Array.from(powerBillFiles).length} found):`);
    Array.from(powerBillFiles).forEach((row: any) => {
      console.log(`  [${row.source}] ${row.file_name} (type: ${row.attachment_type || 'NULL'})`);
    });

    // Check appointments that should have power bills
    const appointmentsWithPBs = await sql`
      SELECT 
        a.repcard_appointment_id,
        a.has_power_bill as stored_value,
        COUNT(DISTINCT ca.id)::int as customer_attachments,
        COUNT(DISTINCT aa.id)::int as appointment_attachments,
        CASE 
          WHEN EXISTS (
            SELECT 1 FROM repcard_customer_attachments ca2
            WHERE ca2.repcard_customer_id::text = a.repcard_customer_id::text
              AND (
                (ca2.attachment_type IS NOT NULL AND (
                  ca2.attachment_type ILIKE '%power%' 
                  OR ca2.attachment_type ILIKE '%bill%'
                ))
                OR (ca2.file_name IS NOT NULL AND (
                  ca2.file_name ILIKE '%power%' 
                  OR ca2.file_name ILIKE '%bill%'
                ))
              )
          ) OR EXISTS (
            SELECT 1 FROM repcard_appointment_attachments aa2
            WHERE aa2.repcard_appointment_id::text = a.repcard_appointment_id::text
              AND (
                (aa2.attachment_type IS NOT NULL AND (
                  aa2.attachment_type ILIKE '%power%' 
                  OR aa2.attachment_type ILIKE '%bill%'
                ))
                OR (aa2.file_name IS NOT NULL AND (
                  aa2.file_name ILIKE '%power%' 
                  OR aa2.file_name ILIKE '%bill%'
                ))
              )
          ) THEN TRUE
          ELSE FALSE
        END as should_have_pb
      FROM repcard_appointments a
      LEFT JOIN repcard_customer_attachments ca ON ca.repcard_customer_id::text = a.repcard_customer_id::text
      LEFT JOIN repcard_appointment_attachments aa ON aa.repcard_appointment_id::text = a.repcard_appointment_id::text
      WHERE a.scheduled_at >= NOW() - INTERVAL '7 days'
      GROUP BY a.repcard_appointment_id, a.repcard_customer_id, a.has_power_bill
      HAVING COUNT(DISTINCT ca.id) > 0 OR COUNT(DISTINCT aa.id) > 0
      ORDER BY a.scheduled_at DESC
      LIMIT 20
    `;

    console.log(`\n📋 Recent Appointments with Attachments:`);
    let mismatches = 0;
    Array.from(appointmentsWithPBs).forEach((row: any) => {
      const matches = row.stored_value === row.should_have_pb;
      if (!matches) mismatches++;
      console.log(`  Appointment ${row.repcard_appointment_id}:`);
      console.log(`    Customer attachments: ${row.customer_attachments}`);
      console.log(`    Appointment attachments: ${row.appointment_attachments}`);
      console.log(`    Stored has_power_bill: ${row.stored_value}`);
      console.log(`    Should have PB: ${row.should_have_pb}`);
      console.log(`    ${matches ? '✅' : '❌'} ${matches ? 'Match' : 'MISMATCH'}`);
      console.log('');
    });

    console.log(`\n📊 Summary: ${mismatches} mismatches out of ${Array.from(appointmentsWithPBs).length} appointments with attachments`);

  } catch (error) {
    console.error('❌ Error:', error);
    process.exit(1);
  }
}

auditPowerBillAttachments();
