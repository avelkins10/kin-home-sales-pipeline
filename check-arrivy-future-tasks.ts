import { config } from 'dotenv';
import { arrivyClient } from './lib/integrations/arrivy/client';

config({ path: '.env.local' });

async function checkArrivyFutureTasks() {
  try {
    if (!arrivyClient) {
      console.error('Arrivy client not configured');
      process.exit(1);
    }

    console.log('\nChecking Arrivy for future tasks...\n');

    // Get tasks scheduled in the future
    const today = new Date();
    const nextMonth = new Date();
    nextMonth.setDate(nextMonth.getDate() + 30);

    const futureStartDate = today.toISOString().split('T')[0];
    const futureEndDate = nextMonth.toISOString().split('T')[0];

    console.log(`Searching for tasks from ${futureStartDate} to ${futureEndDate}`);

    const tasks = await arrivyClient.getTasks({
      start_date: futureStartDate,
      end_date: futureEndDate,
      page_size: 50,
    });

    console.log(`\nFound ${tasks.length} future tasks in Arrivy`);

    if (tasks.length > 0) {
      console.log('\nSample of future tasks:');
      console.table(tasks.slice(0, 10).map(t => ({
        id: t.id,
        customer: t.customer_name,
        scheduled: t.start_datetime ? new Date(t.start_datetime).toISOString().split('T')[0] : 'No date',
        status: t.status,
      })));
    } else {
      console.log('\n⚠️  No future tasks found in Arrivy');
      console.log('This could mean:');
      console.log('1. No tasks are scheduled in Arrivy for the next 30 days');
      console.log('2. Tasks exist but are not within the date range');
    }

  } catch (error) {
    console.error('Error checking Arrivy:', error);
  } finally {
    process.exit(0);
  }
}

checkArrivyFutureTasks();
