import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';
import { repcardClient } from '@/lib/repcard/client';
import { sql } from '@/lib/db/client';

export const runtime = 'nodejs';

// Cache implementation
const trendsCache = new Map<string, { data: any; timestamp: number }>();
const CACHE_TTL = 900000; // 15 minutes
const MAX_CACHE_ENTRIES = 100;

function cleanCache() {
  const now = Date.now();
  for (const [key, value] of Array.from(trendsCache.entries())) {
    if (now - value.timestamp > CACHE_TTL) {
      trendsCache.delete(key);
    }
  }

  if (trendsCache.size > MAX_CACHE_ENTRIES) {
    const entries = Array.from(trendsCache.entries());
    entries.sort((a, b) => a[1].timestamp - b[1].timestamp);
    const toDelete = entries.slice(0, trendsCache.size - MAX_CACHE_ENTRIES);
    toDelete.forEach(([key]) => trendsCache.delete(key));
  }
}

// Calculate date range from timeRange parameter
function calculateDateRange(timeRange: string, startDate?: string, endDate?: string) {
  const now = new Date();
  let calculatedStartDate: string;
  let calculatedEndDate: string;

  switch (timeRange) {
    case 'week':
      const weekStart = new Date(now);
      weekStart.setDate(now.getDate() - now.getDay());
      calculatedStartDate = weekStart.toISOString().split('T')[0];
      calculatedEndDate = now.toISOString().split('T')[0];
      break;
    case 'month':
      calculatedStartDate = new Date(now.getFullYear(), now.getMonth(), 1).toISOString().split('T')[0];
      calculatedEndDate = now.toISOString().split('T')[0];
      break;
    case 'quarter':
      const quarterStart = new Date(now.getFullYear(), Math.floor(now.getMonth() / 3) * 3, 1);
      calculatedStartDate = quarterStart.toISOString().split('T')[0];
      calculatedEndDate = now.toISOString().split('T')[0];
      break;
    case 'custom':
      if (!startDate || !endDate) {
        throw new Error('startDate and endDate are required for custom time range');
      }
      calculatedStartDate = startDate;
      calculatedEndDate = endDate;
      break;
    case 'last_30':
      const last30 = new Date(now);
      last30.setDate(now.getDate() - 30);
      calculatedStartDate = last30.toISOString().split('T')[0];
      calculatedEndDate = now.toISOString().split('T')[0];
      break;
    case 'last_90':
      const last90 = new Date(now);
      last90.setDate(now.getDate() - 90);
      calculatedStartDate = last90.toISOString().split('T')[0];
      calculatedEndDate = now.toISOString().split('T')[0];
      break;
    case 'last_12_months':
      const last12 = new Date(now);
      last12.setMonth(now.getMonth() - 12);
      calculatedStartDate = last12.toISOString().split('T')[0];
      calculatedEndDate = now.toISOString().split('T')[0];
      break;
    default:
      // Default to last 7 days
      const last7 = new Date(now);
      last7.setDate(now.getDate() - 7);
      calculatedStartDate = last7.toISOString().split('T')[0];
      calculatedEndDate = now.toISOString().split('T')[0];
  }

  return { startDate: calculatedStartDate, endDate: calculatedEndDate };
}

export async function GET(request: NextRequest) {
  const start = Date.now();

  try {
    // Authentication
    const session = await getServerSession(authOptions);
    if (!session) {
      return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
    }

    // Extract parameters
    const { searchParams } = new URL(request.url);
    const timeRange = searchParams.get('timeRange') || 'week';
    const startDate = searchParams.get('startDate') || undefined;
    const endDate = searchParams.get('endDate') || undefined;
    const officeIds = searchParams.get('officeIds')?.split(',').map(id => parseInt(id.trim())).filter(id => !isNaN(id));

    // Calculate date range
    let calculatedStartDate: string;
    let calculatedEndDate: string;
    try {
      const dateRange = calculateDateRange(timeRange, startDate, endDate);
      calculatedStartDate = dateRange.startDate;
      calculatedEndDate = dateRange.endDate;
    } catch (error) {
      return NextResponse.json(
        { error: error instanceof Error ? error.message : 'Invalid date range' },
        { status: 400 }
      );
    }

    // Build cache key
    const cacheKey = `${timeRange}:${calculatedStartDate}:${calculatedEndDate}:${officeIds?.join(',') || 'all'}`;

    // Check cache
    cleanCache();
    const cached = trendsCache.get(cacheKey);
    if (cached && Date.now() - cached.timestamp < CACHE_TTL) {
      return NextResponse.json({
        ...cached.data,
        metadata: { ...cached.data.metadata, cached: true }
      });
    }

    // Fetch users filtered by office if specified
    let users: any[];
    if (officeIds && officeIds.length > 0) {
      const result = await sql.query(
        `SELECT DISTINCT u.id, u.repcard_user_id
         FROM users u
         JOIN offices o ON o.name = ANY(u.sales_office)
         WHERE u.repcard_user_id IS NOT NULL
           AND o.quickbase_office_id = ANY($1::int[])`,
        [officeIds]
      );
      users = result.rows;
    } else {
      users = await sql`
        SELECT id, repcard_user_id
        FROM users
        WHERE repcard_user_id IS NOT NULL
      ` as unknown as any[];
    }

    if (users.length === 0) {
      return NextResponse.json({
        data: [],
        metadata: {
          timeRange,
          startDate: calculatedStartDate,
          endDate: calculatedEndDate,
          officeIds: officeIds?.map(String),
          cached: false,
          calculatedAt: new Date().toISOString()
        }
      });
    }

    // Fetch all customers for the date range
    const allCustomers: any[] = [];
    let page = 1;
    let hasMore = true;

    while (hasMore) {
      try {
        const response = await repcardClient.getCustomers({
          page,
          perPage: 100,
          startDate: calculatedStartDate,
          endDate: calculatedEndDate
        });
        allCustomers.push(...response.result.data);
        hasMore = response.result.currentPage < response.result.lastPage;
        page++;
      } catch (error) {
        console.error(`Failed to fetch customers page ${page}:`, error);
        hasMore = false;
      }
    }

    // Filter customers by users
    const repcardUserIds = new Set(users.map(u => u.repcard_user_id?.toString()));
    const filteredCustomers = allCustomers.filter((c: any) =>
      repcardUserIds.has(c.userId?.toString())
    );

    // Group customers by date
    const trendsMap = new Map<string, number>();

    filteredCustomers.forEach((customer: any) => {
      // Use createdAt or created_at field from customer
      const createdDate = customer.createdAt || customer.created_at;
      if (createdDate) {
        const date = new Date(createdDate).toISOString().split('T')[0];
        trendsMap.set(date, (trendsMap.get(date) || 0) + 1);
      }
    });

    // Convert to array and sort by date
    const trendData = Array.from(trendsMap.entries())
      .map(([date, doors]) => ({
        date: new Date(date).toLocaleDateString('en-US', { month: 'short', day: 'numeric' }),
        doors
      }))
      .sort((a, b) => new Date(a.date).getTime() - new Date(b.date).getTime());

    const response = {
      data: trendData,
      metadata: {
        timeRange,
        startDate: calculatedStartDate,
        endDate: calculatedEndDate,
        officeIds: officeIds?.map(String),
        totalDoors: filteredCustomers.length,
        totalDays: trendData.length,
        cached: false,
        calculatedAt: new Date().toISOString()
      }
    };

    // Cache result
    trendsCache.set(cacheKey, {
      data: response,
      timestamp: Date.now()
    });

    const duration = Date.now() - start;
    console.log(`[Canvassing Trends API] Success in ${duration}ms`);

    return NextResponse.json(response);

  } catch (error) {
    const duration = Date.now() - start;
    console.error(`[Canvassing Trends API] Error after ${duration}ms:`, error);
    return NextResponse.json(
      { error: 'Internal server error', message: error instanceof Error ? error.message : 'Unknown error' },
      { status: 500 }
    );
  }
}
