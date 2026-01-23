/**
 * Backfill door knocks from existing customer data
 * Extracts doorKnocks and verifiedDoorKnocks from customer raw_data
 * and syncs them to repcard_door_knocks table
 */

import { config } from 'dotenv';
import { resolve } from 'path';
import { sql } from '@/lib/db/client';

// Load environment variables
config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });

async function backfillDoorKnocks() {
  console.log('🚀 Starting door knocks backfill from customer data...\n');

  try {
    // Get all customers with door knock data
    const customersResult = await sql`
      SELECT 
        repcard_customer_id,
        id as customer_id,
        setter_user_id,
        created_at,
        raw_data
      FROM repcard_customers
      WHERE (raw_data->'doorKnocks' IS NOT NULL OR raw_data->'verifiedDoorKnocks' IS NOT NULL)
        AND setter_user_id IS NOT NULL
      ORDER BY created_at DESC
    `;

    const customers = Array.isArray(customersResult) ? customersResult : customersResult.rows || [];
    console.log(`📊 Found ${customers.length} customers with door knock data\n`);

    let totalProcessed = 0;
    let totalInserted = 0;
    let totalUpdated = 0;
    let totalErrors = 0;

    for (const customer of customers) {
      try {
        const doorKnocks = (customer.raw_data as any)?.doorKnocks || [];
        const verifiedDoorKnocks = (customer.raw_data as any)?.verifiedDoorKnocks || [];
        
        // Process verified door knocks (preferred) or regular door knocks
        const knocksToProcess = verifiedDoorKnocks.length > 0 ? verifiedDoorKnocks : doorKnocks;
        
        if (knocksToProcess.length === 0) continue;

        for (const doorKnock of knocksToProcess) {
          try {
            // Extract door knock data
            const doorKnockedAt = doorKnock.door_knocked_at || doorKnock.doorKnockedAt || customer.created_at;
            const status = doorKnock.status || null;
            const contactDistance = doorKnock.contact_distance || doorKnock.contactDistance || null;
            const verified = verifiedDoorKnocks.length > 0 && verifiedDoorKnocks.includes(doorKnock);
            
            // Use customer's setter_user_id (door knock might have user name string, not ID)
            const setterUserId = customer.setter_user_id;
            
            // Get office_id from setter
            let officeId: number | null = null;
            if (setterUserId) {
              const setterOfficeResult = await sql`
                SELECT office_id FROM repcard_users 
                WHERE repcard_user_id = ${setterUserId.toString()}::text 
                LIMIT 1
              `;
              const setterOfficeRows = setterOfficeResult.rows || setterOfficeResult;
              if (setterOfficeRows.length > 0) {
                officeId = setterOfficeRows[0].office_id;
              }
            }
            
            // Generate unique door knock ID
            const doorKnockId = `${setterUserId}_${customer.repcard_customer_id}_${new Date(doorKnockedAt).getTime()}`;
            
            // Insert door knock
            const result = await sql`
              INSERT INTO repcard_door_knocks (
                repcard_door_knock_id,
                setter_user_id,
                repcard_customer_id,
                customer_id,
                office_id,
                door_knocked_at,
                status,
                contact_distance,
                latitude,
                longitude,
                verified,
                created_at,
                updated_at,
                raw_data
              )
              VALUES (
                ${doorKnockId},
                ${setterUserId ? setterUserId.toString() : null}::text,
                ${customer.repcard_customer_id.toString()}::text,
                ${customer.customer_id},
                ${officeId},
                ${new Date(doorKnockedAt).toISOString()},
                ${status},
                ${contactDistance ? parseFloat(contactDistance.toString()) : null},
                ${(customer.raw_data as any)?.latitude ? parseFloat((customer.raw_data as any).latitude.toString()) : null},
                ${(customer.raw_data as any)?.longitude ? parseFloat((customer.raw_data as any).longitude.toString()) : null},
                ${verified},
                ${new Date(doorKnockedAt).toISOString()},
                ${new Date().toISOString()},
                ${JSON.stringify(doorKnock)}
              )
              ON CONFLICT (repcard_door_knock_id)
              DO UPDATE SET
                repcard_customer_id = EXCLUDED.repcard_customer_id,
                customer_id = EXCLUDED.customer_id,
                office_id = COALESCE(EXCLUDED.office_id, repcard_door_knocks.office_id),
                door_knocked_at = EXCLUDED.door_knocked_at,
                status = EXCLUDED.status,
                contact_distance = EXCLUDED.contact_distance,
                verified = EXCLUDED.verified,
                updated_at = EXCLUDED.updated_at,
                raw_data = EXCLUDED.raw_data,
                synced_at = NOW()
              RETURNING (xmax = 0) AS inserted
            `;

            const row = result.rows?.[0] || result[0];
            if (row?.inserted) {
              totalInserted++;
            } else {
              totalUpdated++;
            }
            totalProcessed++;
          } catch (doorKnockError) {
            console.warn(`⚠️  Failed to process door knock for customer ${customer.repcard_customer_id}:`, doorKnockError);
            totalErrors++;
          }
        }
      } catch (customerError) {
        console.warn(`⚠️  Failed to process customer ${customer.repcard_customer_id}:`, customerError);
        totalErrors++;
      }

      // Progress update every 100 customers
      if (totalProcessed % 100 === 0) {
        console.log(`📈 Processed ${totalProcessed} door knocks (${totalInserted} inserted, ${totalUpdated} updated, ${totalErrors} errors)...`);
      }
    }

    console.log('\n✅ Door knocks backfill complete!');
    console.log(`📊 Summary:`);
    console.log(`   - Total processed: ${totalProcessed}`);
    console.log(`   - Inserted: ${totalInserted}`);
    console.log(`   - Updated: ${totalUpdated}`);
    console.log(`   - Errors: ${totalErrors}`);

    // Verify results
    const verifyResult = await sql`
      SELECT COUNT(*)::int as total_door_knocks
      FROM repcard_door_knocks
    `;
    const verifyCount = Array.isArray(verifyResult) ? verifyResult[0] : verifyResult.rows?.[0];
    console.log(`\n✅ Total door knocks in database: ${verifyCount?.total_door_knocks || 0}`);

  } catch (error) {
    console.error('❌ Error during door knocks backfill:', error);
    throw error;
  }
}

// Run the backfill
backfillDoorKnocks()
  .then(() => {
    console.log('\n🎉 Done!');
    process.exit(0);
  })
  .catch((error) => {
    console.error('❌ Fatal error:', error);
    process.exit(1);
  });
