/**
 * Comprehensive RepCard Sync Script
 * 
 * Pulls ALL available data from RepCard:
 * - Users/Reps
 * - Offices
 * - Customers (leads)
 * - Appointments
 * - Status Logs
 * - Customer Attachments
 * - Appointment Attachments
 * 
 * Usage:
 *   npx tsx scripts/run-comprehensive-repcard-sync.ts
 *   npx tsx scripts/run-comprehensive-repcard-sync.ts --incremental
 *   npx tsx scripts/run-comprehensive-repcard-sync.ts --start-date 2025-01-01 --end-date 2025-12-31
 *   npx tsx scripts/run-comprehensive-repcard-sync.ts --skip-attachments
 */

import { runComprehensiveSync } from '../lib/repcard/comprehensive-sync';

// Parse command line arguments
const args = process.argv.slice(2);
const options: {
  startDate?: string;
  endDate?: string;
  incremental?: boolean;
  skipUsers?: boolean;
  skipOffices?: boolean;
  skipCustomers?: boolean;
  skipAppointments?: boolean;
  skipStatusLogs?: boolean;
  skipCustomerAttachments?: boolean;
  skipAppointmentAttachments?: boolean;
} = {};

// Parse arguments
for (let i = 0; i < args.length; i++) {
  const arg = args[i];
  
  if (arg === '--incremental' || arg === '-i') {
    options.incremental = true;
  } else if (arg === '--start-date' && args[i + 1]) {
    options.startDate = args[++i];
  } else if (arg === '--end-date' && args[i + 1]) {
    options.endDate = args[++i];
  } else if (arg === '--skip-users') {
    options.skipUsers = true;
  } else if (arg === '--skip-offices') {
    options.skipOffices = true;
  } else if (arg === '--skip-customers') {
    options.skipCustomers = true;
  } else if (arg === '--skip-appointments') {
    options.skipAppointments = true;
  } else if (arg === '--skip-status-logs') {
    options.skipStatusLogs = true;
  } else if (arg === '--skip-customer-attachments') {
    options.skipCustomerAttachments = true;
  } else if (arg === '--skip-appointment-attachments') {
    options.skipAppointmentAttachments = true;
  } else if (arg === '--skip-attachments') {
    options.skipCustomerAttachments = true;
    options.skipAppointmentAttachments = true;
  } else if (arg === '--help' || arg === '-h') {
    console.log(`
RepCard Comprehensive Sync Script

Pulls ALL available data from RepCard and stores it in the database.

Usage:
  npx tsx scripts/run-comprehensive-repcard-sync.ts [options]

Options:
  --incremental, -i              Run incremental sync (only updates since last sync)
  --start-date YYYY-MM-DD        Start date for date range filtering
  --end-date YYYY-MM-DD          End date for date range filtering
  --skip-users                   Skip syncing users
  --skip-offices                 Skip syncing offices
  --skip-customers               Skip syncing customers
  --skip-appointments            Skip syncing appointments
  --skip-status-logs             Skip syncing status logs
  --skip-customer-attachments    Skip syncing customer attachments
  --skip-appointment-attachments Skip syncing appointment attachments
  --skip-attachments             Skip all attachments (shorthand)
  --help, -h                     Show this help message

Examples:
  # Full sync of everything
  npx tsx scripts/run-comprehensive-repcard-sync.ts

  # Incremental sync (only new/updated records)
  npx tsx scripts/run-comprehensive-repcard-sync.ts --incremental

  # Sync specific date range
  npx tsx scripts/run-comprehensive-repcard-sync.ts --start-date 2025-01-01 --end-date 2025-12-31

  # Sync everything except attachments
  npx tsx scripts/run-comprehensive-repcard-sync.ts --skip-attachments

  # Sync only users and offices
  npx tsx scripts/run-comprehensive-repcard-sync.ts --skip-customers --skip-appointments --skip-status-logs --skip-attachments
`);
    process.exit(0);
  }
}

// Validate date format if provided
if (options.startDate && !/^\d{4}-\d{2}-\d{2}$/.test(options.startDate)) {
  console.error('❌ Invalid start date format. Use YYYY-MM-DD');
  process.exit(1);
}

if (options.endDate && !/^\d{4}-\d{2}-\d{2}$/.test(options.endDate)) {
  console.error('❌ Invalid end date format. Use YYYY-MM-DD');
  process.exit(1);
}

// Check for API key
if (!process.env.REPCARD_API_KEY) {
  console.error('❌ REPCARD_API_KEY environment variable is not set!');
  console.error('   Set it with: export REPCARD_API_KEY="your-key-here"');
  process.exit(1);
}

// Run sync
async function main() {
  console.log('🚀 Starting RepCard Comprehensive Sync');
  console.log('='.repeat(60));
  console.log('Options:', {
    startDate: options.startDate || 'all time',
    endDate: options.endDate || 'today',
    incremental: options.incremental ? 'yes' : 'no',
    syncUsers: !options.skipUsers,
    syncOffices: !options.skipOffices,
    syncCustomers: !options.skipCustomers,
    syncAppointments: !options.skipAppointments,
    syncStatusLogs: !options.skipStatusLogs,
    syncCustomerAttachments: !options.skipCustomerAttachments,
    syncAppointmentAttachments: !options.skipAppointmentAttachments
  });
  console.log('='.repeat(60));
  console.log('');

  try {
    const results = await runComprehensiveSync(options);

    console.log('');
    console.log('='.repeat(60));
    console.log('✅ Comprehensive Sync Complete!');
    console.log('='.repeat(60));
    console.log(`Total Duration: ${(results.totalDuration / 1000).toFixed(2)}s`);
    console.log('');
    console.log('Summary:');
    console.log(`  Users:           ${results.users.recordsFetched} fetched, ${results.users.recordsInserted} inserted, ${results.users.recordsUpdated} updated, ${results.users.recordsFailed} failed`);
    console.log(`  Offices:         ${results.offices.recordsFetched} fetched, ${results.offices.recordsInserted} inserted, ${results.offices.recordsUpdated} updated, ${results.offices.recordsFailed} failed`);
    console.log(`  Customers:       ${results.customers.recordsFetched} fetched, ${results.customers.recordsInserted} inserted, ${results.customers.recordsUpdated} updated, ${results.customers.recordsFailed} failed`);
    console.log(`  Appointments:    ${results.appointments.recordsFetched} fetched, ${results.appointments.recordsInserted} inserted, ${results.appointments.recordsUpdated} updated, ${results.appointments.recordsFailed} failed`);
    console.log(`  Status Logs:     ${results.statusLogs.recordsFetched} fetched, ${results.statusLogs.recordsInserted} inserted, ${results.statusLogs.recordsUpdated} updated, ${results.statusLogs.recordsFailed} failed`);
    console.log(`  Cust Attachments: ${results.customerAttachments.recordsFetched} fetched, ${results.customerAttachments.recordsInserted} inserted, ${results.customerAttachments.recordsUpdated} updated, ${results.customerAttachments.recordsFailed} failed`);
    console.log(`  Appt Attachments: ${results.appointmentAttachments.recordsFetched} fetched, ${results.appointmentAttachments.recordsInserted} inserted, ${results.appointmentAttachments.recordsUpdated} updated, ${results.appointmentAttachments.recordsFailed} failed`);
    console.log('');

    // Check for errors
    const hasErrors = [
      results.users.error,
      results.offices.error,
      results.customers.error,
      results.appointments.error,
      results.statusLogs.error,
      results.customerAttachments.error,
      results.appointmentAttachments.error
    ].some(Boolean);

    if (hasErrors) {
      console.log('⚠️  Some syncs had errors. Check the logs above for details.');
      process.exit(1);
    } else {
      console.log('✅ All syncs completed successfully!');
      process.exit(0);
    }

  } catch (error) {
    console.error('');
    console.error('❌ Comprehensive sync failed:', error);
    console.error('');
    process.exit(1);
  }
}

main();

