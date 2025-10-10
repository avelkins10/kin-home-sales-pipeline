const { sql } = require('@vercel/postgres');
const path = require('path');

require('dotenv').config({ path: path.join(__dirname, '../.env.local') });

async function checkProjectCache() {
  try {
    // Get columns
    const columnsResult = await sql.query(`
      SELECT column_name, data_type
      FROM information_schema.columns
      WHERE table_name = 'project_cache'
      ORDER BY ordinal_position
    `);

    console.log('📋 project_cache columns:\n');
    columnsResult.rows.forEach(col => {
      console.log(`  - ${col.column_name}: ${col.data_type}`);
    });

    // Sample a few rows
    console.log('\n\nSample data (first 2 rows):');
    const sampleResult = await sql.query(`
      SELECT data
      FROM project_cache
      LIMIT 2
    `);

    sampleResult.rows.forEach((row, idx) => {
      console.log(`\n${idx + 1}. Project data:`);
      const data = typeof row.data === 'string' ? JSON.parse(row.data) : row.data;
      console.log('  Fields available:', Object.keys(data).join(', '));

      // Show relevant fields for user matching
      if (data.closer_name) console.log(`  Closer: ${data.closer_name} - ID: ${data.closer_id}`);
      if (data.setter_name) console.log(`  Setter: ${data.setter_name} - ID: ${data.setter_id}`);
    });

  } catch (error) {
    console.error('❌ Error:', error.message);
  }
}

checkProjectCache();
