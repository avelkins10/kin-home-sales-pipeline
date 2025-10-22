/**
 * Script to audit project statuses and see where all 113 projects are
 * Run with: npx tsx scripts/audit-project-statuses.ts
 */

import { getOfficeMetrics } from '../lib/quickbase/queries';

async function auditStatuses() {
  console.log('Fetching office metrics for Sep 30 - Oct 20, 2025...\n');

  // Use super_admin to see all projects
  // Replace with actual user ID from your session if needed
  const userId = '1'; // Super admin user ID
  const role = 'super_admin';

  try {
    const metrics = await getOfficeMetrics(
      userId,
      role,
      'custom',
      undefined, // all offices
      { startDate: '2025-09-30', endDate: '2025-10-20' },
      'audit-script'
    );

    console.log(`Found ${metrics.length} offices\n`);

    // Find the office with 113 projects or show all
    let totalAcrossAll = {
      total: 0,
      active: 0,
      submitted: 0,
      approved: 0,
      rejected: 0,
      cancelled: 0,
      holds: 0
    };

    metrics.forEach(office => {
      totalAcrossAll.total += office.totalProjects;
      totalAcrossAll.active += office.activeProjects;
      totalAcrossAll.submitted += office.projectsSubmitted;
      totalAcrossAll.approved += office.projectsApproved;
      totalAcrossAll.rejected += office.projectsRejected;
      totalAcrossAll.cancelled += office.cancelledProjects;
      totalAcrossAll.holds += office.onHoldProjects;

      // Show individual office if it has the 113 projects
      if (office.totalProjects === 113 || office.totalProjects > 100) {
        console.log(`━━━ ${office.officeName} ━━━`);
        console.log(`Total Projects: ${office.totalProjects}`);
        console.log(`  Active: ${office.activeProjects}`);
        console.log(`  Submitted: ${office.projectsSubmitted}`);
        console.log(`  Approved: ${office.projectsApproved}`);
        console.log(`  Rejected: ${office.projectsRejected}`);
        console.log(`  Cancelled: ${office.cancelledProjects}`);
        console.log(`  Holds: ${office.onHoldProjects}`);

        const sum = office.activeProjects + office.projectsSubmitted +
                    office.projectsApproved + office.projectsRejected +
                    office.cancelledProjects + office.onHoldProjects;
        console.log(`  ─────────────`);
        console.log(`  Sum: ${sum}`);

        if (sum !== office.totalProjects) {
          console.log(`  ⚠️  MISMATCH! Missing ${office.totalProjects - sum} projects`);
        } else {
          console.log(`  ✓ All projects accounted for`);
        }
        console.log('');
      }
    });

    // Show aggregated totals
    console.log(`━━━ AGGREGATED (All ${metrics.length} Offices) ━━━`);
    console.log(`Total Projects: ${totalAcrossAll.total}`);
    console.log(`  Active: ${totalAcrossAll.active}`);
    console.log(`  Submitted: ${totalAcrossAll.submitted}`);
    console.log(`  Approved: ${totalAcrossAll.approved}`);
    console.log(`  Rejected: ${totalAcrossAll.rejected}`);
    console.log(`  Cancelled: ${totalAcrossAll.cancelled}`);
    console.log(`  Holds: ${totalAcrossAll.holds}`);

    const totalSum = totalAcrossAll.active + totalAcrossAll.submitted +
                     totalAcrossAll.approved + totalAcrossAll.rejected +
                     totalAcrossAll.cancelled + totalAcrossAll.holds;
    console.log(`  ─────────────`);
    console.log(`  Sum: ${totalSum}`);

    if (totalSum !== totalAcrossAll.total) {
      console.log(`  ⚠️  MISMATCH! Missing ${totalAcrossAll.total - totalSum} projects`);
    } else {
      console.log(`  ✓ All projects accounted for`);
    }

  } catch (error) {
    console.error('Error fetching metrics:', error);
    throw error;
  }
}

auditStatuses();
