import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/next-auth.config';
import { qbClient } from '@/lib/quickbase/client';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import { buildProjectAccessClause } from '@/lib/auth/projectAuthorization';
import { getUserEmail } from '@/lib/auth/userIdentity';

const QB_TABLE_PROJECTS = process.env.QUICKBASE_TABLE_PROJECTS || 'br9kwm8na';

export async function GET(request: NextRequest) {
  try {
    const session = await getServerSession(authOptions);
    if (!session?.user?.quickbaseUserId || !session?.user?.role) {
      return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
    }

    const searchParams = request.nextUrl.searchParams;
    const officeId = searchParams.get('officeId') || '109'; // Stevens office
    const timezone = 'America/New_York';

    const userId = session.user.quickbaseUserId;
    const role = session.user.role;
    const userEmail = getUserEmail(session);

    // Simulate "Month to Date" calculation
    const now = new Date();
    const currentYear = now.getFullYear();
    const currentMonth = now.getMonth();

    const monthStart = new Date(currentYear, currentMonth, 1);

    const startDate = new Intl.DateTimeFormat('en-CA', {
      timeZone: timezone,
      year: 'numeric',
      month: '2-digit',
      day: '2-digit',
    }).format(monthStart);

    const endDate = new Intl.DateTimeFormat('en-CA', {
      timeZone: timezone,
      year: 'numeric',
      month: '2-digit',
      day: '2-digit',
    }).format(now);

    // Build WHERE clause exactly as we do in getOfficeMetrics
    const accessClause = buildProjectAccessClause(userEmail, role);
    const timeFilter = `AND {${PROJECT_FIELDS.SALES_DATE}.OAF.'${startDate}'} AND {${PROJECT_FIELDS.SALES_DATE}.OBF.'${endDate}'}`;
    const officeFilter = `AND {${PROJECT_FIELDS.SALES_OFFICE}.EX.${officeId}}`;
    const whereClause = `${accessClause} ${timeFilter} ${officeFilter}`.trim();

    console.log('[DEBUG] Current server time (UTC):', now.toISOString());
    console.log('[DEBUG] Current server time (local):', now.toString());
    console.log('[DEBUG] Start date:', startDate);
    console.log('[DEBUG] End date:', endDate);
    console.log('[DEBUG] WHERE clause:', whereClause);

    // Query QuickBase
    const response = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS,
      where: whereClause,
      select: [
        PROJECT_FIELDS.RECORD_ID,
        PROJECT_FIELDS.PROJECT_ID,
        PROJECT_FIELDS.SALES_DATE,
        PROJECT_FIELDS.SALES_OFFICE,
        PROJECT_FIELDS.PROJECT_STATUS,
        PROJECT_FIELDS.CLOSER_NAME,
      ],
      options: { top: 500 }
    });

    const projects = response.data || [];

    // Analyze the sales dates
    const salesDates = projects.map((p: Record<string, any>) => ({
      projectId: p[PROJECT_FIELDS.PROJECT_ID]?.value,
      salesDate: p[PROJECT_FIELDS.SALES_DATE]?.value,
      office: p[PROJECT_FIELDS.SALES_OFFICE]?.value,
      status: p[PROJECT_FIELDS.PROJECT_STATUS]?.value,
      closer: p[PROJECT_FIELDS.CLOSER_NAME]?.value,
    })).sort((a: any, b: any) => (a.salesDate || '').localeCompare(b.salesDate || ''));

    // Check for dates outside our expected range
    const outsideRange = salesDates.filter((p: any) => {
      const date = p.salesDate;
      if (!date) return false;
      return date < startDate || date > endDate;
    });

    // Group by date to see distribution
    const dateDistribution: Record<string, number> = {};
    salesDates.forEach((p: any) => {
      const date = p.salesDate || 'Unknown';
      dateDistribution[date] = (dateDistribution[date] || 0) + 1;
    });

    return NextResponse.json({
      debug: {
        serverTimeUTC: now.toISOString(),
        serverTimeLocal: now.toString(),
        calculatedStartDate: startDate,
        calculatedEndDate: endDate,
        timezone,
        whereClause,
      },
      results: {
        totalProjects: projects.length,
        officeId,
        dateRange: `${startDate} to ${endDate}`,
      },
      projects: salesDates,
      outsideRange: {
        count: outsideRange.length,
        projects: outsideRange,
      },
      dateDistribution,
    });
  } catch (error) {
    console.error('[DEBUG] Error:', error);
    return NextResponse.json(
      {
        error: 'Failed to debug date filter',
        details: error instanceof Error ? error.message : String(error)
      },
      { status: 500 }
    );
  }
}
