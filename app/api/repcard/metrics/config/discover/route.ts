import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';
import { discoverDispositions } from '@/lib/repcard/metric-config';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

export const runtime = 'nodejs';

/**
 * GET /api/repcard/metrics/config/discover
 * 
 * Discover all unique dispositions and status categories from the database
 * Helps admins see what values exist when configuring metrics
 */
export async function GET(request: NextRequest) {
  const requestId = crypto.randomUUID();
  
  try {
    logApiRequest('repcard-metrics-config-discover', request, { requestId });
    
    // Check authentication
    const session = await getServerSession(authOptions);
    if (!session?.user) {
      return NextResponse.json(
        { error: 'Unauthorized' },
        { status: 401 }
      );
    }
    
    // Only admins can discover dispositions
    const adminRoles = ['super_admin', 'regional', 'office_leader'];
    if (!adminRoles.includes(session.user.role || '')) {
      return NextResponse.json(
        { error: 'Forbidden - Admin access required' },
        { status: 403 }
      );
    }
    
    // Discover dispositions and status categories
    const discovery = await discoverDispositions();
    
    const response = {
      dispositions: discovery.dispositions,
      statusCategories: discovery.statusCategories,
      totalDispositions: discovery.dispositions.length,
      totalStatusCategories: discovery.statusCategories.length
    };
    
    logApiResponse('repcard-metrics-config-discover', request, response, { requestId });
    
    return NextResponse.json(response);
    
  } catch (error) {
    logError('repcard-metrics-config-discover', error as Error, { requestId });
    return NextResponse.json(
      { 
        error: 'Failed to discover dispositions',
        message: error instanceof Error ? error.message : 'Unknown error'
      },
      { status: 500 }
    );
  }
}
