#!/usr/bin/env tsx
/**
 * Direct Appointment Sync Script
 * Syncs appointments from RepCard API directly to database
 * 
 * Usage:
 *   npx tsx scripts/sync-appointments-direct.ts
 *   npx tsx scripts/sync-appointments-direct.ts --start-date 2026-01-23 --end-date 2026-01-30
 */

import { config } from 'dotenv';
import { sql } from '@vercel/postgres';

// Load environment variables
config({ path: '.env.local' });

// Set up environment for Vercel Postgres
if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

// Import RepCard client directly (not server-only)
class RepCardClient {
  private baseUrl: string;
  private apiKey: string;

  constructor() {
    this.baseUrl = process.env.REPCARD_API_URL || 'https://api.repcard.com';
    this.apiKey = process.env.REPCARD_API_KEY || '';
    
    if (!this.apiKey) {
      throw new Error('REPCARD_API_KEY environment variable is required');
    }
  }

  private async request<T>(endpoint: string, options: RequestInit = {}): Promise<any> {
    const url = `${this.baseUrl}${endpoint}`;
    const headers = {
      'x-api-key': this.apiKey,
      'Content-Type': 'application/json',
      ...options.headers,
    };

    const response = await fetch(url, { ...options, headers });
    
    if (!response.ok) {
      const errorText = await response.text();
      throw new Error(`RepCard API error: ${response.status} ${response.statusText} - ${errorText}`);
    }

    return response.json();
  }

  async getAppointments(params: { page: number; perPage: number; fromDate?: string; toDate?: string }) {
    const queryParams = new URLSearchParams({
      page: params.page.toString(),
      perPage: params.perPage.toString(),
    });
    if (params.fromDate) queryParams.append('fromDate', params.fromDate);
    if (params.toDate) queryParams.append('toDate', params.toDate);

    return this.request(`/appointments?${queryParams}`);
  }
}

async function syncAppointments(startDate?: string, endDate?: string) {
  console.log('🚀 Starting RepCard Appointment Sync');
  console.log('='.repeat(60));
  
  if (startDate && endDate) {
    console.log(`📅 Date range: ${startDate} to ${endDate}`);
  } else {
    console.log('📅 Date range: All time');
  }
  console.log('='.repeat(60));
  console.log('');

  const client = new RepCardClient();
  let page = 1;
  let hasMore = true;
  let totalFetched = 0;
  let totalInserted = 0;
  let totalUpdated = 0;
  let totalFailed = 0;
  const MAX_PAGES = 20; // Limit to avoid timeout

  while (hasMore && page <= MAX_PAGES) {
    try {
      console.log(`📄 Fetching page ${page}...`);
      const response = await client.getAppointments({
        page,
        perPage: 100,
        fromDate: startDate,
        toDate: endDate
      });

      if (!response || !response.result) {
        console.error(`❌ Invalid response structure on page ${page}`);
        break;
      }

      const appointments = Array.isArray(response.result.data) ? response.result.data : [];
      console.log(`   Found ${appointments.length} appointments`);

      if (appointments.length === 0) {
        hasMore = false;
        break;
      }

      totalFetched += appointments.length;

      // Process each appointment
      for (const appointment of appointments) {
        try {
          // Extract appointment data
          const appointmentId = appointment.id?.toString();
          const customerId = appointment.contact?.id?.toString();
          const setterUserId = appointment.user?.id?.toString() || appointment.userId?.toString();
          const closerUserId = appointment.closer?.id?.toString() || appointment.closerId?.toString();
          const officeId = appointment.office?.id || appointment.officeId;
          const scheduledAt = appointment.startAt || appointment.appt_start_time || appointment.scheduledAt;
          const completedAt = appointment.endAt || appointment.appt_end_time || appointment.completedAt;
          
          // Handle status - it can be an object with title/category, or a string
          const disposition = appointment.status?.title || appointment.status?.category?.title || appointment.disposition || appointment.outcome || null;
          
          // Determine status category from disposition (matching sync-service logic)
          let statusCategory: string | null = null;
          if (disposition) {
            const dispLower = String(disposition).toLowerCase();
            if (dispLower.includes('cancel')) statusCategory = 'cancelled';
            else if (dispLower.includes('reschedule')) statusCategory = 'rescheduled';
            else if (dispLower.includes('no.show') || dispLower.includes('no_show')) statusCategory = 'no_show';
            else if (dispLower.includes('sat.closed') || dispLower.includes('sat_closed') || dispLower.includes('closed')) statusCategory = 'sat_closed';
            else if (dispLower.includes('sat.no.close') || dispLower.includes('sat_no_close')) statusCategory = 'sat_no_close';
            else if (completedAt) statusCategory = 'completed';
            else if (scheduledAt) statusCategory = 'scheduled';
          } else {
            // Fallback if no disposition
            if (completedAt) statusCategory = 'completed';
            else if (scheduledAt) statusCategory = 'scheduled';
            else statusCategory = 'scheduled';
          }
          const notes = appointment.notes || null;
          const duration = appointment.duration || appointment.durationTime || null;
          const isReschedule = appointment.isReschedule || false;
          const rescheduleCount = appointment.rescheduleCount || 0;
          const originalAppointmentId = appointment.originalAppointmentId?.toString() || null;

          if (!appointmentId || !customerId) {
            console.warn(`   ⚠️  Skipping appointment ${appointmentId} - missing ID or customer`);
            totalFailed++;
            continue;
          }

          // Insert or update appointment
          const result = await sql`
            INSERT INTO repcard_appointments (
              repcard_appointment_id,
              customer_id,
              repcard_customer_id,
              setter_user_id,
              closer_user_id,
              office_id,
              disposition,
              status_category,
              scheduled_at,
              completed_at,
              duration,
              notes,
              is_reschedule,
              reschedule_count,
              original_appointment_id,
              created_at,
              updated_at,
              raw_data
            )
            VALUES (
              ${appointmentId}::text,
              NULL,
              ${customerId}::text,
              ${setterUserId || null}::text,
              ${closerUserId || null}::text,
              ${officeId || null},
              ${disposition},
              ${statusCategory},
              ${scheduledAt ? new Date(scheduledAt).toISOString() : null},
              ${completedAt ? new Date(completedAt).toISOString() : null},
              ${duration},
              ${notes},
              ${isReschedule},
              ${rescheduleCount},
              ${originalAppointmentId},
              ${appointment.createdAt ? new Date(appointment.createdAt).toISOString() : new Date().toISOString()},
              ${appointment.updatedAt ? new Date(appointment.updatedAt).toISOString() : new Date().toISOString()},
              ${JSON.stringify(appointment)}
            )
            ON CONFLICT (repcard_appointment_id)
            DO UPDATE SET
              customer_id = EXCLUDED.customer_id,
              setter_user_id = EXCLUDED.setter_user_id,
              closer_user_id = EXCLUDED.closer_user_id,
              office_id = COALESCE(EXCLUDED.office_id, repcard_appointments.office_id),
              disposition = EXCLUDED.disposition,
              status_category = EXCLUDED.status_category,
              scheduled_at = EXCLUDED.scheduled_at,
              completed_at = EXCLUDED.completed_at,
              duration = EXCLUDED.duration,
              notes = EXCLUDED.notes,
              is_reschedule = EXCLUDED.is_reschedule,
              reschedule_count = EXCLUDED.reschedule_count,
              original_appointment_id = EXCLUDED.original_appointment_id,
              updated_at = EXCLUDED.updated_at,
              raw_data = EXCLUDED.raw_data
            RETURNING (xmax = 0) AS inserted
          `;

          const row = Array.from(result)[0];
          if (row?.inserted) {
            totalInserted++;
          } else {
            totalUpdated++;
          }
        } catch (error) {
          console.error(`   ❌ Failed to sync appointment ${appointment.id}:`, error instanceof Error ? error.message : error);
          totalFailed++;
        }
      }

      // Check if there are more pages
      hasMore = response.result.pagination?.hasMore || 
                (response.result.lastPage && page < response.result.lastPage) ||
                (appointments.length === 100); // If we got 100, might be more

      page++;
      
      // Small delay to avoid rate limits
      if (hasMore) {
        await new Promise(resolve => setTimeout(resolve, 500));
      }
    } catch (error) {
      console.error(`❌ Error on page ${page}:`, error instanceof Error ? error.message : error);
      break;
    }
  }

  console.log('');
  console.log('='.repeat(60));
  console.log('✅ Sync Complete!');
  console.log('='.repeat(60));
  console.log(`📊 Total fetched: ${totalFetched}`);
  console.log(`➕ Total inserted: ${totalInserted}`);
  console.log(`🔄 Total updated: ${totalUpdated}`);
  console.log(`❌ Total failed: ${totalFailed}`);
  console.log('');
}

// Parse command line arguments
const args = process.argv.slice(2);
let startDate: string | undefined;
let endDate: string | undefined;

for (let i = 0; i < args.length; i++) {
  if (args[i] === '--start-date' && args[i + 1]) {
    startDate = args[++i];
  } else if (args[i] === '--end-date' && args[i + 1]) {
    endDate = args[++i];
  }
}

// Validate API key
if (!process.env.REPCARD_API_KEY) {
  console.error('❌ REPCARD_API_KEY environment variable is not set!');
  console.error('   Set it with: export REPCARD_API_KEY="your-key-here"');
  process.exit(1);
}

// Run sync
syncAppointments(startDate, endDate)
  .then(() => {
    console.log('✅ Script completed successfully');
    process.exit(0);
  })
  .catch((error) => {
    console.error('❌ Script failed:', error);
    process.exit(1);
  });
