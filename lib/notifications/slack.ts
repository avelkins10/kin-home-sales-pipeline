/**
 * Slack notification utilities for sending alerts via webhook
 * 
 * Used for sync failure notifications and other operational alerts.
 */

interface SlackNotificationOptions {
  title?: string;
  color?: 'good' | 'warning' | 'danger'; // green, yellow, red
  fields?: Array<{ title: string; value: string; short?: boolean }>;
  footer?: string;
}

interface SyncStats {
  totalUsers: number;
  enriched: number;
  notFoundInContacts: number;
  alreadyUpToDate: number;
  errors: number;
  errorDetails: Array<{email: string, error: string}>;
  notFoundSamples: string[];
}

/**
 * Send a notification to Slack channel via webhook
 */
export async function sendSlackNotification(
  message: string, 
  options: SlackNotificationOptions = {}
): Promise<void> {
  const webhookUrl = process.env.SLACK_WEBHOOK_URL;
  
  if (!webhookUrl) {
    console.warn('⚠️  SLACK_WEBHOOK_URL not configured - skipping notification');
    return;
  }

  // Validate webhook URL format
  if (!webhookUrl.startsWith('https://hooks.slack.com/services/')) {
    console.error('❌ Invalid Slack webhook URL format');
    return;
  }

  try {
    const payload = {
      text: options.title || 'User Sync Notification',
      attachments: [
        {
          color: options.color || 'good',
          text: message,
          fields: options.fields || [],
          footer: options.footer || 'Kin Home Sales Pipeline',
          ts: Math.floor(Date.now() / 1000)
        }
      ]
    };

    const response = await fetch(webhookUrl, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(payload),
      signal: AbortSignal.timeout(5000) // 5 second timeout
    });

    if (!response.ok) {
      throw new Error(`Slack webhook failed: ${response.status} ${response.statusText}`);
    }

    console.log('✅ Slack notification sent successfully');
  } catch (error) {
    console.error('❌ Failed to send Slack notification:', error);
    // Don't throw - notifications are non-critical
  }
}

/**
 * Send formatted notification for sync failures
 */
export async function sendSyncFailureNotification(
  stats: SyncStats, 
  error?: string
): Promise<void> {
  const errorRate = stats.totalUsers > 0 ? Math.round((stats.errors / stats.totalUsers) * 100) : 0;
  const notFoundRate = stats.totalUsers > 0 ? Math.round((stats.notFoundInContacts / stats.totalUsers) * 100) : 0;
  
  // Determine notification type and color
  let title: string;
  let color: 'good' | 'warning' | 'danger';
  let message: string;

  if (error) {
    // Complete failure
    title = '🚨 User Sync Failed';
    color = 'danger';
    message = `Complete sync failure: ${error}`;
  } else if (errorRate > 10) {
    // High error rate
    title = '⚠️ User Sync Issues';
    color = 'warning';
    message = `High error rate detected (${errorRate}%)`;
  } else if (notFoundRate > 20) {
    // High not-found rate
    title = '⚠️ User Sync Issues';
    color = 'warning';
    message = `High not-found rate detected (${notFoundRate}%)`;
  } else {
    // Partial success
    title = '✅ User Sync Completed';
    color = 'good';
    message = `Sync completed with minor issues`;
  }

  const fields = [
    {
      title: 'Status',
      value: error ? 'Failed' : (errorRate > 10 ? 'Partial Failure' : 'Success'),
      short: true
    },
    {
      title: 'Total Users',
      value: stats.totalUsers.toString(),
      short: true
    },
    {
      title: 'Enriched',
      value: stats.enriched.toString(),
      short: true
    },
    {
      title: 'Errors',
      value: `${stats.errors} (${errorRate}%)`,
      short: true
    },
    {
      title: 'Not Found',
      value: `${stats.notFoundInContacts} (${notFoundRate}%)`,
      short: true
    },
    {
      title: 'Already Up-to-Date',
      value: stats.alreadyUpToDate.toString(),
      short: true
    }
  ];

  // Add error details if available
  if (stats.errorDetails.length > 0) {
    const errorSamples = stats.errorDetails.slice(0, 5).map(({ email, error }) => 
      `• ${email}: ${error}`
    ).join('\n');
    
    fields.push({
      title: 'Sample Errors',
      value: errorSamples + (stats.errorDetails.length > 5 ? `\n... and ${stats.errorDetails.length - 5} more` : ''),
      short: false
    });
  }

  // Add not-found samples if available
  if (stats.notFoundSamples.length > 0) {
    const notFoundSamples = stats.notFoundSamples.slice(0, 5).join('\n• ');
    
    fields.push({
      title: 'Sample Not-Found Emails',
      value: `• ${notFoundSamples}` + (stats.notFoundSamples.length > 5 ? `\n... and ${stats.notFoundSamples.length - 5} more` : ''),
      short: false
    });
  }

  await sendSlackNotification(message, {
    title,
    color,
    fields,
    footer: 'View details: /operations/sync-monitoring'
  });
}

/**
 * Send notification for sync timeout
 */
export async function sendSyncTimeoutNotification(
  executionTimeMs: number,
  stats: SyncStats
): Promise<void> {
  const executionMinutes = Math.round(executionTimeMs / 60000);
  
  await sendSlackNotification(
    `Sync execution time exceeded 10 minutes (${executionMinutes}m)`,
    {
      title: '⏰ User Sync Timeout Warning',
      color: 'warning',
      fields: [
        {
          title: 'Execution Time',
          value: `${executionMinutes} minutes`,
          short: true
        },
        {
          title: 'Users Processed',
          value: stats.totalUsers.toString(),
          short: true
        },
        {
          title: 'Status',
          value: 'Still running or timed out',
          short: true
        }
      ],
      footer: 'Consider checking sync performance or increasing timeout'
    }
  );
}
