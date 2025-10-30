import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { 
  getPCEscalations,
  createEscalation,
  categorizeEscalationReason,
  getCategoryDisplayName
} from '@/lib/quickbase/queries';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { 
  PCEscalationFilters, 
  PCEscalation, 
  PCEscalationStats, 
  PCCreateEscalationPayload,
  PCEscalationCategory
} from '@/lib/types/operations';
import { cache, invalidateForUser } from './cache';

export const runtime = 'nodejs';
export const dynamic = 'force-dynamic';

const CACHE_TTL = 30 * 1000; // 30 seconds

/**
 * GET /api/operations/escalations
 * Fetch PC escalations with caching
 */
export async function GET(request: NextRequest) {
  const reqId = `req_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
  
  try {
    logApiRequest('GET /api/operations/escalations', {
      reqId,
      url: request.url
    });

    // Require authentication and check role
    const auth = await requireAuth();
    if (!auth.authorized) return auth.response;
    const session = auth.session;

    const allowedRoles = ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'];

    if (!session?.user?.role || !allowedRoles.includes(session.user.role)) {
      return NextResponse.json(
        { error: 'Insufficient permissions' },
        { status: 403 }
      );
    }

    const pcEmail = session.user.email;
    const pcName = session.user.name;
    
    if (!pcEmail || !pcName) {
      return NextResponse.json(
        { error: 'User email and name required' },
        { status: 400 }
      );
    }

    // Parse query parameters
    const { searchParams } = new URL(request.url);
    const category = searchParams.get('category') as PCEscalationCategory | 'all' || 'all';
    const status = searchParams.get('status') || 'all';
    const urgency = searchParams.get('urgency') || 'all';
    const assignedTo = searchParams.get('assignedTo') || 'all';
    const search = searchParams.get('search') || '';

    const filters: PCEscalationFilters = {
      category,
      status,
      urgency: urgency as 'critical' | 'high' | 'normal' | 'all',
      assignedTo,
      search
    };

    // Check cache
    const cacheKey = `pc-escalations:${pcEmail}:${JSON.stringify(filters)}`;
    const cached = cache.get(cacheKey);
    
    if (cached && (Date.now() - cached.timestamp) < CACHE_TTL) {
      logApiResponse('GET /api/operations/escalations', {
        reqId,
        cached: true,
        count: cached.data.length
      });
      
      return NextResponse.json({
        escalations: cached.data,
        stats: cached.stats,
        count: cached.data.length,
        cached: true
      });
    }

    // Fetch from QuickBase
    const role = session.user.role;
    const escalations = await getPCEscalations(pcEmail, pcName, role, filters, reqId);

    // Calculate stats
    const stats: PCEscalationStats = {
      total: escalations.length,
      byCategory: {
        mmu_required: 0,
        rep_promises: 0,
        hoa_issues: 0,
        financing_issues: 0,
        customer_complaints: 0
      },
      byUrgency: {
        critical: 0,
        high: 0,
        normal: 0
      },
      avgResolutionTime: 0
    };

    // Calculate category and urgency counts
    escalations.forEach(escalation => {
      stats.byCategory[escalation.category]++;
      
      // Calculate urgency based on deadline
      const now = new Date();
      const deadline = new Date(escalation.rep72HourDeadline);
      const hoursUntilDeadline = (deadline.getTime() - now.getTime()) / (1000 * 60 * 60);
      
      if (hoursUntilDeadline < 0) {
        stats.byUrgency.critical++;
      } else if (hoursUntilDeadline < 24) {
        stats.byUrgency.critical++;
      } else if (hoursUntilDeadline < 48) {
        stats.byUrgency.high++;
      } else {
        stats.byUrgency.normal++;
      }
    });

    // Calculate average resolution time (placeholder - would need historical data)
    stats.avgResolutionTime = 72; // Default 72 hours

    // Cache the result
    cache.set(cacheKey, {
      data: escalations,
      stats,
      timestamp: Date.now()
    });

    // Cleanup cache if it gets too large
    if (cache.size > 100) {
      const oldestKey = cache.keys().next().value;
      cache.delete(oldestKey);
    }

    logApiResponse('GET /api/operations/escalations', {
      reqId,
      count: escalations.length,
      cached: false
    });

    return NextResponse.json({
      escalations,
      stats,
      count: escalations.length,
      cached: false
    });

  } catch (error) {
    logError('GET /api/operations/escalations', error, reqId);
    return NextResponse.json(
      { error: 'Failed to fetch escalations' },
      { status: 500 }
    );
  }
}

/**
 * POST /api/operations/escalations
 * Create a new escalation
 */
export async function POST(request: NextRequest) {
  const reqId = `req_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
  
  try {
    logApiRequest('POST /api/operations/escalations', {
      reqId
    });

    // Require authentication and check role
    const auth = await requireAuth();
    if (!auth.authorized) return auth.response;
    const session = auth.session;

    const allowedRoles = ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'];

    if (!session?.user?.role || !allowedRoles.includes(session.user.role)) {
      return NextResponse.json(
        { error: 'Insufficient permissions' },
        { status: 403 }
      );
    }

    const pcEmail = session.user.email;
    
    if (!pcEmail) {
      return NextResponse.json(
        { error: 'User email required' },
        { status: 400 }
      );
    }

    const body = await request.json();
    const { 
      projectId,
      recordId,
      reason,
      category,
      description,
      priority
    }: PCCreateEscalationPayload = body;

    // Validate required fields
    if (!projectId || !recordId || !reason || !category || !description || !priority) {
      return NextResponse.json(
        { error: 'Missing required fields: projectId, recordId, reason, category, description, priority' },
        { status: 400 }
      );
    }

    // Validate category
    const validCategories: PCEscalationCategory[] = [
      'mmu_required',
      'rep_promises', 
      'hoa_issues',
      'financing_issues',
      'customer_complaints'
    ];
    
    if (!validCategories.includes(category)) {
      return NextResponse.json(
        { error: 'Invalid category' },
        { status: 400 }
      );
    }

    // Validate priority
    if (!['high', 'normal'].includes(priority)) {
      return NextResponse.json(
        { error: 'Invalid priority' },
        { status: 400 }
      );
    }

    // Create escalation
    const escalationId = await createEscalation(
      {
        projectId,
        recordId,
        reason,
        category,
        description,
        priority
      },
      pcEmail,
      reqId
    );

    // Invalidate cache for this PC
    invalidateForUser(pcEmail);

    logApiResponse('POST /api/operations/escalations', {
      reqId,
      escalationId
    });

    return NextResponse.json({
      success: true,
      escalationId,
      message: 'Escalation created successfully'
    });

  } catch (error) {
    logError('POST /api/operations/escalations', error, reqId);
    return NextResponse.json(
      { error: 'Failed to create escalation' },
      { status: 500 }
    );
  }
}
