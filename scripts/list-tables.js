const { sql } = require('@vercel/postgres');
const path = require('path');

require('dotenv').config({ path: path.join(__dirname, '../.env.local') });

async function listTables() {
  try {
    const result = await sql.query(`
      SELECT table_name
      FROM information_schema.tables
      WHERE table_schema = 'public'
      ORDER BY table_name
    `);

    console.log('📋 Tables in database:\n');
    result.rows.forEach(row => {
      console.log(`  - ${row.table_name}`);
    });

    console.log(`\nTotal: ${result.rows.length} tables`);

  } catch (error) {
    console.error('❌ Error:', error.message);
  }
}

listTables();
