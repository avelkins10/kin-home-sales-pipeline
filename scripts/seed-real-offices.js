const { sql } = require('@vercel/postgres');
const path = require('path');

// Load environment variables from .env.local
require('dotenv').config({ path: path.join(__dirname, '../.env.local') });

// 19 real offices from QuickBase with their Record IDs
const REAL_OFFICES = [
  { name: 'Stevens - Iowa 2025', id: 111 },
  { name: 'Molina - KC 2025', id: 118 },
  { name: 'Bontrager - Cincinnati 2025', id: 117 },
  { name: 'Douglass - Winter Haven 2025', id: 122 },
  { name: 'Champagne - Panama City 2025', id: 112 },
  { name: 'Elevate - Orlando East 2025', id: 121 },
  { name: 'Bryant - Columbus 2025', id: 114 },
  { name: 'Allen - Orlando West 2025', id: 124 },
  { name: 'Pensacola', id: 15 },
  { name: 'Kin Home HQ', id: 43 },
  { name: 'Inside Sales', id: 45 },
  { name: 'Richards Mgmt', id: 64 },
  { name: 'Awake Energy Main', id: 85 },
  { name: 'Main Office', id: 95 },
  { name: 'Smooth Solar Main', id: 100 },
  { name: 'Rebellion Solar Main', id: 101 },
  { name: 'Vivint Warm Leads', id: 120 },
  { name: 'Fearless', id: 125 },
  { name: 'Champagne - Crestview 2025', id: 126 }
];

async function seedRealOffices() {
  try {
    console.log('🚀 Starting real office seeding...');
    console.log(`📊 Inserting ${REAL_OFFICES.length} offices with QuickBase IDs\n`);

    // Verify DATABASE_URL is set
    if (!process.env.DATABASE_URL) {
      throw new Error('DATABASE_URL environment variable is required');
    }

    let created = 0;
    let updated = 0;
    let skipped = 0;

    for (const office of REAL_OFFICES) {
      try {
        // Check if office with this ID already exists
        const existingOffice = await sql.query(`
          SELECT id, name FROM offices WHERE quickbase_office_id = $1
        `, [office.id]);

        if (existingOffice.rows.length > 0) {
          // Update existing office name
          await sql.query(`
            UPDATE offices
            SET name = $1, updated_at = NOW()
            WHERE quickbase_office_id = $2
          `, [office.name, office.id]);
          console.log(`🔄 Updated: ${office.name} (ID: ${office.id})`);
          updated++;
        } else {
          // Insert new office with default region (can be updated later)
          await sql.query(`
            INSERT INTO offices (name, quickbase_office_id, is_active, region)
            VALUES ($1, $2, TRUE, 'southeast')
          `, [office.name, office.id]);
          console.log(`✅ Created: ${office.name} (ID: ${office.id})`);
          created++;
        }
      } catch (error) {
        console.error(`❌ Failed to insert ${office.name} (ID: ${office.id}):`, error.message);
        skipped++;
      }
    }

    console.log('\n📊 Summary:');
    console.log(`   ✅ Created: ${created}`);
    console.log(`   🔄 Updated: ${updated}`);
    console.log(`   ⏭️  Skipped: ${skipped}`);
    console.log(`   📈 Total: ${REAL_OFFICES.length}`);

    // Verify all offices were inserted
    const verifyResult = await sql.query(`
      SELECT COUNT(*) as count FROM offices WHERE quickbase_office_id IS NOT NULL
    `);
    console.log(`\n✅ Verification: ${verifyResult.rows[0].count} offices with QuickBase IDs in database`);

    console.log('\n✅ Real office seeding complete!');

  } catch (error) {
    console.error('❌ Seeding failed:', error.message);
    console.error('Full error:', error);
    process.exit(1);
  }
}

seedRealOffices();
