import { NextRequest, NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { repcardClient } from '@/lib/repcard/client';
import { logApiRequest, logApiResponse } from '@/lib/logging/logger';

export const runtime = 'nodejs';

/**
 * GET /api/repcard/raw-api-data
 * 
 * Fetch raw API responses from RepCard to see actual payload structure
 * Query params:
 * - type: 'users' | 'appointments' | 'customers' | 'teams' | 'all'
 * - limit: number of records to fetch (default: 5)
 */
export async function GET(request: NextRequest) {
  const start = Date.now();
  const path = new URL(request.url).pathname;
  const { searchParams } = new URL(request.url);
  const type = searchParams.get('type') || 'all';
  const limit = parseInt(searchParams.get('limit') || '5', 10);

  try {
    logApiRequest('GET', path, { type, limit });

    // Authentication
    const auth = await requireRole(['closer', 'setter', 'office_leader', 'regional', 'super_admin']);
    if (!auth.authorized) return auth.response;

    const results: any = {
      timestamp: new Date().toISOString(),
      type,
      limit,
      data: {}
    };

    // 1. Get Users
    if (type === 'users' || type === 'all') {
      try {
        console.log('[Raw API Data] Fetching users...');
        const usersResponse = await repcardClient.getUsersMinimal({ page: 1, perPage: limit });
        results.data.users = {
          success: true,
          apiResponse: usersResponse,
          sampleRecords: usersResponse?.result?.data?.slice(0, limit) || [],
          total: usersResponse?.result?.pagination?.total || usersResponse?.result?.total || 0,
          // Extract team-related fields
          teamFields: usersResponse?.result?.data?.[0] ? Object.keys(usersResponse.result.data[0]).filter(key => 
            key.toLowerCase().includes('team') || 
            key.toLowerCase().includes('group') ||
            key.toLowerCase().includes('squad')
          ) : [],
          // Show first record structure
          firstRecordStructure: usersResponse?.result?.data?.[0] || null
        };
      } catch (error) {
        results.data.users = {
          success: false,
          error: error instanceof Error ? error.message : String(error),
          stack: error instanceof Error ? error.stack : undefined
        };
      }
    }

    // 2. Get Appointments
    if (type === 'appointments' || type === 'all') {
      try {
        console.log('[Raw API Data] Fetching appointments...');
        const today = new Date();
        const fromDate = new Date(today);
        fromDate.setDate(fromDate.getDate() - 7); // Last 7 days
        
        const appointmentsResponse = await repcardClient.getAppointments({
          page: 1,
          perPage: limit,
          fromDate: fromDate.toISOString().split('T')[0],
          toDate: today.toISOString().split('T')[0]
        });
        
        results.data.appointments = {
          success: true,
          apiResponse: appointmentsResponse,
          sampleRecords: appointmentsResponse?.result?.data?.slice(0, limit) || [],
          total: appointmentsResponse?.result?.total || appointmentsResponse?.result?.pagination?.total || 0,
          // Extract date/time fields
          dateFields: appointmentsResponse?.result?.data?.[0] ? Object.keys(appointmentsResponse.result.data[0]).filter(key => 
            key.toLowerCase().includes('date') || 
            key.toLowerCase().includes('time') ||
            key.toLowerCase().includes('at') ||
            key.toLowerCase().includes('when')
          ) : [],
          // Show first record structure
          firstRecordStructure: appointmentsResponse?.result?.data?.[0] || null,
          // Show contact/user fields
          contactFields: appointmentsResponse?.result?.data?.[0]?.contact ? Object.keys(appointmentsResponse.result.data[0].contact) : [],
          contactSample: appointmentsResponse?.result?.data?.[0]?.contact || null
        };
      } catch (error) {
        results.data.appointments = {
          success: false,
          error: error instanceof Error ? error.message : String(error),
          stack: error instanceof Error ? error.stack : undefined
        };
      }
    }

    // 3. Get Customers
    if (type === 'customers' || type === 'all') {
      try {
        console.log('[Raw API Data] Fetching customers...');
        const today = new Date();
        const fromDate = new Date(today);
        fromDate.setDate(fromDate.getDate() - 7); // Last 7 days
        
        const customersResponse = await repcardClient.getCustomers({
          page: 1,
          perPage: limit,
          startDate: fromDate.toISOString().split('T')[0],
          endDate: today.toISOString().split('T')[0]
        });
        
        results.data.customers = {
          success: true,
          apiResponse: customersResponse,
          sampleRecords: customersResponse?.result?.data?.slice(0, limit) || [],
          total: customersResponse?.result?.total || customersResponse?.result?.pagination?.total || 0,
          // Extract date fields
          dateFields: customersResponse?.result?.data?.[0] ? Object.keys(customersResponse.result.data[0]).filter(key => 
            key.toLowerCase().includes('date') || 
            key.toLowerCase().includes('time') ||
            key.toLowerCase().includes('at') ||
            key.toLowerCase().includes('created')
          ) : [],
          // Show first record structure
          firstRecordStructure: customersResponse?.result?.data?.[0] || null,
          // Show setter/closer fields
          userFields: customersResponse?.result?.data?.[0] ? Object.keys(customersResponse.result.data[0]).filter(key => 
            key.toLowerCase().includes('setter') || 
            key.toLowerCase().includes('closer') ||
            key.toLowerCase().includes('user') ||
            key.toLowerCase().includes('rep')
          ) : []
        };
      } catch (error) {
        results.data.customers = {
          success: false,
          error: error instanceof Error ? error.message : String(error),
          stack: error instanceof Error ? error.stack : undefined
        };
      }
    }

    // 4. Get Teams (from Offices)
    if (type === 'teams' || type === 'all') {
      try {
        console.log('[Raw API Data] Fetching offices (to extract teams)...');
        const officesResponse = await repcardClient.getOffices();
        
        // Extract teams from offices
        const offices = Array.isArray(officesResponse?.result?.data) ? officesResponse.result.data : 
                       Array.isArray(officesResponse?.result) ? officesResponse.result : [];
        
        const allTeams: any[] = [];
        offices.forEach((office: any) => {
          if (office.teams && Array.isArray(office.teams)) {
            allTeams.push(...office.teams);
          }
        });
        
        results.data.teams = {
          success: true,
          apiResponse: officesResponse,
          sampleTeams: allTeams.slice(0, limit),
          totalTeams: allTeams.length,
          sampleOffices: offices.slice(0, 3),
          // Show team structure
          teamStructure: allTeams[0] || null,
          // Show office structure with teams
          officeWithTeams: offices.find((o: any) => o.teams && o.teams.length > 0) || null
        };
      } catch (error) {
        results.data.teams = {
          success: false,
          error: error instanceof Error ? error.message : String(error),
          stack: error instanceof Error ? error.stack : undefined
        };
      }
    }

    // 5. Get a specific user's details (if we have a user ID from the sample)
    if (type === 'users' || type === 'all') {
      try {
        const firstUserId = results.data.users?.sampleRecords?.[0]?.id;
        if (firstUserId) {
          console.log(`[Raw API Data] Fetching detailed user info for user ${firstUserId}...`);
          const userDetailsResponse = await repcardClient.getUserDetails(firstUserId);
          results.data.userDetails = {
            success: true,
            userId: firstUserId,
            apiResponse: userDetailsResponse,
            fullRecord: userDetailsResponse?.result || null,
            // Extract team-related fields
            teamFields: userDetailsResponse?.result ? Object.keys(userDetailsResponse.result).filter(key => 
              key.toLowerCase().includes('team') || 
              key.toLowerCase().includes('group') ||
              key.toLowerCase().includes('squad')
            ) : []
          };
        }
      } catch (error) {
        results.data.userDetails = {
          success: false,
          error: error instanceof Error ? error.message : String(error),
          note: 'Could not fetch user details (non-critical)'
        };
      }
    }

    const duration = Date.now() - start;
    logApiResponse('GET', path, duration, { status: 200 });

    return NextResponse.json(results, {
      headers: {
        'Content-Type': 'application/json',
      }
    });
  } catch (error) {
    console.error('[RepCard Raw API Data] Error:', error);
    return NextResponse.json(
      {
        success: false,
        error: error instanceof Error ? error.message : 'Unknown error',
        details: error instanceof Error ? error.stack : String(error),
        timestamp: new Date().toISOString()
      },
      { status: 500 }
    );
  }
}
