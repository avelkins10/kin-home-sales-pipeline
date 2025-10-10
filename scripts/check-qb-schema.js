const { sql } = require('@vercel/postgres');
const path = require('path');

require('dotenv').config({ path: path.join(__dirname, '../.env.local') });

async function checkSchema() {
  try {
    // Check if table exists
    const tableCheck = await sql.query(`
      SELECT table_name FROM information_schema.tables
      WHERE table_name = 'quickbase_projects'
    `);

    if (tableCheck.rows.length === 0) {
      console.log('❌ quickbase_projects table does not exist');
      return;
    }

    console.log('✅ quickbase_projects table exists\n');

    // Get all columns
    const columnsResult = await sql.query(`
      SELECT column_name, data_type, is_nullable
      FROM information_schema.columns
      WHERE table_name = 'quickbase_projects'
      ORDER BY ordinal_position
    `);

    console.log('Table columns:');
    columnsResult.rows.forEach(col => {
      console.log(`  - ${col.column_name}: ${col.data_type} (${col.is_nullable === 'YES' ? 'nullable' : 'not null'})`);
    });

    // Sample a few rows to see email fields
    console.log('\n\nSample data (first 3 rows):');
    const sampleResult = await sql.query(`
      SELECT closer_id, closer_name, closer_email, setter_id, setter_name, setter_email
      FROM quickbase_projects
      LIMIT 3
    `);

    sampleResult.rows.forEach((row, idx) => {
      console.log(`\n${idx + 1}.`);
      console.log(`  Closer: ${row.closer_name} (${row.closer_email}) - ID: ${row.closer_id}`);
      console.log(`  Setter: ${row.setter_name} (${row.setter_email}) - ID: ${row.setter_id}`);
    });

  } catch (error) {
    console.error('❌ Error:', error.message);
  }
}

checkSchema();
