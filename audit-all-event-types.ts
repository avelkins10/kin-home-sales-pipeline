import { config } from 'dotenv';
import { sql } from '@vercel/postgres';

config({ path: '.env.local' });

async function auditEventTypes() {
  try {
    console.log('\n=== All Event Types We\'ve Received ===');
    const eventTypes = await sql`
      SELECT 
        event_type,
        event_sub_type,
        COUNT(*) as count,
        MAX(event_time) as most_recent
      FROM arrivy_events
      GROUP BY event_type, event_sub_type
      ORDER BY count DESC
    `;
    console.table(eventTypes.rows);

    console.log('\n=== Sample Event Data by Type ===');
    const samples = await sql`
      WITH ranked_events AS (
        SELECT 
          event_type,
          event_sub_type,
          title,
          message,
          object_fields::text as object_fields_preview,
          ROW_NUMBER() OVER (PARTITION BY event_type ORDER BY event_time DESC) as rn
        FROM arrivy_events
      )
      SELECT 
        event_type,
        event_sub_type,
        title,
        LEFT(message, 60) as message_preview,
        LEFT(object_fields_preview, 40) as object_fields
      FROM ranked_events
      WHERE rn = 1
      ORDER BY event_type
    `;
    console.table(samples.rows);

  } catch (error) {
    console.error('Error:', error);
  } finally {
    process.exit(0);
  }
}

auditEventTypes();
