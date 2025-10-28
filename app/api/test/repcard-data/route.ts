import { NextRequest, NextResponse } from 'next/server';
import { repcardClient } from '@/lib/repcard/client';

export const runtime = 'nodejs';

/**
 * Temporary test endpoint to see all RepCard API responses
 * DELETE THIS FILE after reviewing the data
 *
 * WARNING: No auth - for development only!
 */
export async function GET(request: NextRequest) {
  const { searchParams } = new URL(request.url);
  const startDate = searchParams.get('startDate') || '2025-10-01';
  const endDate = searchParams.get('endDate') || '2025-10-28';

  try {
    const results: any = {
      dateRange: { startDate, endDate },
      samples: {}
    };

    // 1. Get sample customers
    console.log('Fetching customers...');
    const customersResponse = await repcardClient.getCustomers({
      page: 1,
      perPage: 5,
      startDate,
      endDate
    });
    results.samples.customers = {
      totalCount: customersResponse.result.total,
      currentPage: customersResponse.result.currentPage,
      lastPage: customersResponse.result.lastPage,
      sampleData: customersResponse.result.data.slice(0, 3)
    };

    // 2. Get sample appointments
    console.log('Fetching appointments...');
    try {
      const appointmentsResponse = await repcardClient.getAppointments({
        page: 1,
        perPage: 5,
        fromDate: startDate,
        toDate: endDate
      });
      results.samples.appointments = {
        totalCount: appointmentsResponse.result.total || 0,
        currentPage: appointmentsResponse.result.currentPage,
        totalPages: appointmentsResponse.result.totalPages,
        sampleData: appointmentsResponse.result.data.slice(0, 3)
      };
    } catch (error) {
      results.samples.appointments = {
        error: error instanceof Error ? error.message : 'Failed to fetch appointments'
      };
    }

    // 3. Try native leaderboard endpoint
    console.log('Fetching native leaderboard...');
    try {
      const leaderboardResponse = await repcardClient.getLeaderboard({
        startDate,
        endDate
      });
      results.samples.nativeLeaderboard = leaderboardResponse.result;
    } catch (error) {
      results.samples.nativeLeaderboard = {
        error: error instanceof Error ? error.message : 'Failed to fetch leaderboard'
      };
    }

    // 4. Get sample customer status logs
    console.log('Fetching customer status logs...');
    try {
      const statusLogsResponse = await repcardClient.getCustomerStatusLogs({
        page: 1,
        perPage: 5,
        fromDate: startDate,
        toDate: endDate
      });
      results.samples.statusLogs = {
        totalCount: statusLogsResponse.result.total || 0,
        currentPage: statusLogsResponse.result.currentPage,
        totalPages: statusLogsResponse.result.totalPages,
        sampleData: statusLogsResponse.result.data.slice(0, 3)
      };
    } catch (error) {
      results.samples.statusLogs = {
        error: error instanceof Error ? error.message : 'Failed to fetch status logs'
      };
    }

    return NextResponse.json(results, { status: 200 });

  } catch (error) {
    console.error('[Test RepCard Data] Error:', error);
    return NextResponse.json(
      {
        error: 'Failed to fetch RepCard data',
        message: error instanceof Error ? error.message : 'Unknown error'
      },
      { status: 500 }
    );
  }
}
