import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/next-auth.config';
import { getWorkloadDistribution } from '@/lib/db/arrivy';

export const runtime = 'nodejs';
export const dynamic = 'force-dynamic';

export async function GET(request: NextRequest) {
  try {
    // Check authentication
    const session = await getServerSession(authOptions);
    if (!session?.user) {
      return NextResponse.json(
        { error: 'Unauthorized' },
        { status: 401 }
      );
    }

    // Check role
    const operationsRoles = ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'];
    if (!session.user.role || !operationsRoles.includes(session.user.role)) {
      return NextResponse.json(
        { error: 'Forbidden: Operations role required' },
        { status: 403 }
      );
    }

    // Parse query parameters
    const searchParams = request.nextUrl.searchParams;
    const date = searchParams.get('startDate') || searchParams.get('endDate') || new Date().toISOString().split('T')[0];

    // Fetch analytics data
    const analytics = await getWorkloadDistribution({ date, includeScheduled: true });

    return NextResponse.json(analytics, { status: 200 });
  } catch (error) {
    console.error('Error fetching workload distribution:', error);
    return NextResponse.json(
      { error: 'Failed to fetch workload distribution' },
      { status: 500 }
    );
  }
}
