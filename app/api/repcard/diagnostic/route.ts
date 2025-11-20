import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';
import { sql } from '@/lib/db/client';
import { repcardClient } from '@/lib/repcard/client';

export const runtime = 'nodejs';

export async function GET(request: NextRequest) {
  try {
    // Authentication - allow admins and managers to access diagnostic
    const session = await getServerSession(authOptions);
    const authorizedRoles = ['super_admin', 'regional', 'office_leader'];
    if (!session || !authorizedRoles.includes(session.user.role)) {
      return NextResponse.json(
        { error: 'Unauthorized - Admin access required' },
        { status: 403 }
      );
    }

    const diagnostics: Record<string, any> = {
      timestamp: new Date().toISOString(),
      environment: {
        hasApiKey: !!process.env.REPCARD_API_KEY,
        apiUrl: process.env.REPCARD_API_URL || 'https://api.repcard.com (default)',
        nodeEnv: process.env.NODE_ENV
      },
      apiConnection: null as any,
      database: {
        tables: null as any,
        dataCounts: null as any,
        userLinking: null as any
      }
    };

    // 1. Test API Connection
    try {
      if (process.env.REPCARD_API_KEY) {
        const response = await repcardClient.getUsersMinimal({ page: 1, perPage: 5 });
        
        // Safely access response properties with null checks
        if (response && response.result) {
          const pagination = response.result.pagination || {};
          const data = response.result.data || [];
          
          diagnostics.apiConnection = {
            status: 'success',
            totalUsers: pagination.total || 0,
            sampleUsers: data.slice(0, 3).map((u: any) => ({
              id: u.id,
              name: `${u.firstName || ''} ${u.lastName || ''}`.trim() || 'Unknown',
              email: u.email || 'No email'
            }))
          };
        } else {
          diagnostics.apiConnection = {
            status: 'error',
            error: 'Invalid API response structure',
            rawResponse: JSON.stringify(response).substring(0, 200)
          };
        }
      } else {
        diagnostics.apiConnection = {
          status: 'skipped',
          reason: 'REPCARD_API_KEY not set'
        };
      }
    } catch (error) {
      diagnostics.apiConnection = {
        status: 'error',
        error: error instanceof Error ? error.message : String(error),
        stack: error instanceof Error ? error.stack?.substring(0, 500) : undefined
      };
    }

    // 2. Check Database Tables
    try {
      const tables = await sql`
        SELECT table_name 
        FROM information_schema.tables 
        WHERE table_schema = 'public'
          AND table_name LIKE 'repcard_%'
        ORDER BY table_name;
      `;
      diagnostics.database.tables = {
        count: Array.from(tables).length,
        tables: Array.from(tables).map((t: any) => t.table_name)
      };
    } catch (error) {
      diagnostics.database.tables = {
        error: error instanceof Error ? error.message : String(error)
      };
    }

    // 3. Check Data Counts
    try {
      const counts = await sql`
        SELECT 
          (SELECT COUNT(*) FROM repcard_users) as users,
          (SELECT COUNT(*) FROM repcard_customers) as customers,
          (SELECT COUNT(*) FROM repcard_appointments) as appointments,
          (SELECT COUNT(*) FROM repcard_status_logs) as status_logs,
          (SELECT COUNT(*) FROM repcard_customer_attachments) as customer_attachments,
          (SELECT COUNT(*) FROM repcard_appointment_attachments) as appointment_attachments,
          (SELECT MIN(created_at) FROM repcard_customers) as earliest_customer,
          (SELECT MAX(created_at) FROM repcard_customers) as latest_customer;
      `;
      const c = Array.from(counts)[0];
      diagnostics.database.dataCounts = {
        users: Number(c.users),
        customers: Number(c.customers),
        appointments: Number(c.appointments),
        statusLogs: Number(c.status_logs),
        customerAttachments: Number(c.customer_attachments),
        appointmentAttachments: Number(c.appointment_attachments),
        earliestCustomer: c.earliest_customer,
        latestCustomer: c.latest_customer,
        totalRecords: Number(c.users) + Number(c.customers) + Number(c.appointments) + 
                     Number(c.status_logs) + Number(c.customer_attachments) + Number(c.appointment_attachments)
      };
    } catch (error) {
      diagnostics.database.dataCounts = {
        error: error instanceof Error ? error.message : String(error)
      };
    }

    // 4. Check User Linking
    try {
      const linked = await sql`
        SELECT COUNT(*) as count
        FROM users
        WHERE repcard_user_id IS NOT NULL;
      `;
      const total = await sql`SELECT COUNT(*) as count FROM users;`;
      const linkedCount = Number(Array.from(linked)[0].count);
      const totalCount = Number(Array.from(total)[0].count);
      
      diagnostics.database.userLinking = {
        linked: linkedCount,
        total: totalCount,
        percentage: totalCount > 0 ? Math.round((linkedCount / totalCount) * 100) : 0
      };

      // Sample of linked users
      const sampleUsers = await sql`
        SELECT u.name, u.email, u.role, u.repcard_user_id
        FROM users u
        WHERE u.repcard_user_id IS NOT NULL
        LIMIT 5;
      `;
      diagnostics.database.userLinking.sampleUsers = Array.from(sampleUsers);
    } catch (error) {
      diagnostics.database.userLinking = {
        error: error instanceof Error ? error.message : String(error)
      };
    }

    // 5. Test Leaderboard Query (with current month date range)
    try {
      const now = new Date();
      const startDate = new Date(now.getFullYear(), now.getMonth(), 1).toISOString().split('T')[0];
      const endDate = now.toISOString().split('T')[0];

      // Check if there's data in the date range
      const dateRangeCheck = await sql`
        SELECT 
          COUNT(*) as customers_in_range,
          COUNT(DISTINCT setter_user_id) as unique_setters
        FROM repcard_customers
        WHERE created_at::date >= ${startDate}::date
          AND created_at::date <= ${endDate}::date;
      `;
      const check = Array.from(dateRangeCheck)[0];
      
      diagnostics.leaderboardTest = {
        dateRange: { startDate, endDate },
        customersInRange: Number(check.customers_in_range),
        uniqueSetters: Number(check.unique_setters)
      };
    } catch (error) {
      diagnostics.leaderboardTest = {
        error: error instanceof Error ? error.message : String(error)
      };
    }

    // Determine overall status
    const issues: string[] = [];
    if (!diagnostics.environment.hasApiKey) {
      issues.push('REPCARD_API_KEY not set');
    }
    if (diagnostics.apiConnection?.status === 'error') {
      issues.push(`API connection failed: ${diagnostics.apiConnection.error}`);
    }
    if (diagnostics.database.dataCounts?.totalRecords === 0) {
      issues.push('No data synced from RepCard');
    }
    if (diagnostics.database.userLinking?.linked === 0) {
      issues.push('No users linked to RepCard');
    }
    if (diagnostics.leaderboardTest?.customersInRange === 0 && diagnostics.database.dataCounts?.customers > 0) {
      issues.push('Data exists but outside current date range');
    }

    diagnostics.issues = issues;
    diagnostics.recommendations = [];
    
    // Only mark as healthy if API works AND data exists AND users are linked
    const hasData = diagnostics.database.dataCounts?.totalRecords > 0;
    const hasCustomers = (diagnostics.database.dataCounts?.customers || 0) > 0;
    const hasAppointments = (diagnostics.database.dataCounts?.appointments || 0) > 0;
    const hasLinkedUsers = diagnostics.database.userLinking?.linked > 0;
    const apiHealthy = diagnostics.apiConnection?.status === 'success';
    
    // For analytics to work, we need customers/appointments AND linked users
    const hasAnalyticsData = (hasCustomers || hasAppointments) && hasLinkedUsers;
    
    // If API is healthy but no analytics data, show as needing setup
    if (apiHealthy && !hasAnalyticsData) {
      diagnostics.status = 'needs_setup';
    } else {
      diagnostics.status = issues.length === 0 ? 'healthy' : 'issues_found';
    }

    if (!diagnostics.environment.hasApiKey) {
      diagnostics.recommendations.push({
        priority: 'critical',
        action: 'Add REPCARD_API_KEY to environment variables',
        steps: [
          'Get API key from https://www.repcard.com/settings/api',
          'Add to Vercel: vercel env add REPCARD_API_KEY production',
          'Redeploy after adding'
        ]
      });
    }

    if (diagnostics.database.dataCounts?.totalRecords === 0 && diagnostics.environment.hasApiKey) {
      diagnostics.recommendations.push({
        priority: 'high',
        action: 'Run initial RepCard sync',
        steps: [
          'Navigate to /admin/repcard-sync',
          'Click "Run Full Sync"',
          'Wait 2-5 minutes for sync to complete'
        ]
      });
    }

    if (diagnostics.database.userLinking?.linked === 0) {
      diagnostics.recommendations.push({
        priority: 'high',
        action: 'Link users to RepCard',
        steps: [
          'Run SQL: UPDATE users u SET repcard_user_id = ru.repcard_user_id::text FROM repcard_users ru WHERE LOWER(u.email) = LOWER(ru.email) AND u.repcard_user_id IS NULL;',
          'Or use the admin interface to link users'
        ]
      });
    }

    return NextResponse.json(diagnostics);
  } catch (error) {
    return NextResponse.json(
      {
        error: 'Diagnostic failed',
        message: error instanceof Error ? error.message : String(error)
      },
      { status: 500 }
    );
  }
}

