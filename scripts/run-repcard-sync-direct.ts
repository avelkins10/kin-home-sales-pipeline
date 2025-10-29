/**
 * Direct RepCard Sync Script
 * Runs sync directly using database client (no API needed)
 */

import { runComprehensiveSync } from '../lib/repcard/comprehensive-sync';

async function main() {
  console.log('🚀 Starting RepCard Comprehensive Sync...');
  console.log('='.repeat(60));

  try {
    const results = await runComprehensiveSync({
      skipCustomerAttachments: true,
      skipAppointmentAttachments: true
    });

    console.log('\n✅ Sync Complete!');
    console.log('='.repeat(60));
    console.log('Summary:');
    console.log(`  Users: ${results.users.fetched} fetched, ${results.users.inserted} inserted`);
    console.log(`  Offices: ${results.offices.fetched} fetched, ${results.offices.inserted} inserted`);
    console.log(`  Customers: ${results.customers.fetched} fetched, ${results.customers.inserted} inserted`);
    console.log(`  Appointments: ${results.appointments.fetched} fetched, ${results.appointments.inserted} inserted`);
    console.log(`  Status Logs: ${results.statusLogs.fetched} fetched, ${results.statusLogs.inserted} inserted`);
    console.log(`\nTotal Duration: ${results.completedAt}`);
    
    process.exit(0);
  } catch (error) {
    console.error('❌ Sync failed:', error);
    process.exit(1);
  }
}

main();

