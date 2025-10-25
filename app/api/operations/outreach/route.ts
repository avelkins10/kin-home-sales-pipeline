import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { 
  getPCOutreachInitial, 
  getPCOutreachFollowups, 
  getPCOutreachWelcome, 
  getPCOutreachTabData 
} from '@/lib/quickbase/queries';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { PCOutreachFilters, PCOutreachRecord, PCOutreachTabData } from '@/lib/types/operations';
import { OUTREACH_RECORD_FIELDS, QB_TABLE_OUTREACH_RECORDS } from '@/lib/constants/fieldIds';
import { qbClient } from '@/lib/quickbase/client';

export const runtime = 'nodejs';

/**
 * POST /api/operations/outreach
 * Create a new outreach record
 */
export async function POST(request: NextRequest) {
  const reqId = `req_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
  
  try {
    logApiRequest('POST /api/operations/outreach', {
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

    const body = await request.json();
    const { 
      relatedProject, 
      outreachStatus, 
      reportingDueDate, 
      nextOutreachDueDate, 
      note, 
      attemptNote 
    } = body;

    // Validate required fields
    if (!relatedProject) {
      return NextResponse.json(
        { error: 'Related project is required' },
        { status: 400 }
      );
    }

    // Build create data
    const createData = {
      [OUTREACH_RECORD_FIELDS.RELATED_PROJECT]: { value: relatedProject },
      [OUTREACH_RECORD_FIELDS.OUTREACH_STATUS]: { value: outreachStatus || 'No Answer Left Message' },
      [OUTREACH_RECORD_FIELDS.REPORTING_DUE_DATE]: { value: reportingDueDate || new Date().toISOString() },
      [OUTREACH_RECORD_FIELDS.NEXT_OUTREACH_DUE_DATE]: { value: nextOutreachDueDate },
      [OUTREACH_RECORD_FIELDS.NOTE]: { value: note || '' },
      [OUTREACH_RECORD_FIELDS.ATTEMPT_NOTE]: { value: attemptNote || '' }
    };

    // Create the record
    const response = await qbClient.updateRecord({
      to: QB_TABLE_OUTREACH_RECORDS,
      data: [createData]
    });

    const newRecordId = response.metadata?.createdRecordIds?.[0] || response.data?.[0]?.[3]?.value;

    logApiResponse('POST /api/operations/outreach', {
      reqId,
      newRecordId,
      success: true
    });

    return NextResponse.json({
      success: true,
      recordId: newRecordId,
      message: 'Outreach record created successfully'
    });

  } catch (error) {
    logError('POST /api/operations/outreach failed', error, { reqId });
    
    return NextResponse.json(
      { error: 'Failed to create outreach record' },
      { status: 500 }
    );
  }
}

// In-memory cache for outreach data
const outreachCache = new Map<string, { data: any; timestamp: number }>();
const CACHE_TTL = 30 * 1000; // 30 seconds
const MAX_CACHE_SIZE = 100;

interface CacheEntry {
  data: PCOutreachRecord[] | PCOutreachTabData;
  timestamp: number;
}

/**
 * GET /api/operations/outreach
 * Fetch outreach data for PC with optional filtering
 */
export async function GET(request: NextRequest) {
  const reqId = `req_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
  
  try {
    logApiRequest('GET /api/operations/outreach', {
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
    const pcName = session.user.name || pcEmail.split('@')[0];

    // Parse query parameters
    const { searchParams } = new URL(request.url);
    const tab = searchParams.get('tab') as 'initial' | 'followups' | 'welcome' | 'all' || 'initial';
    const status = searchParams.get('status') || 'all';
    const daysOverdue = searchParams.get('daysOverdue') || 'all';
    const lender = searchParams.get('lender') || 'all';
    const salesRep = searchParams.get('salesRep') || 'all';
    const search = searchParams.get('search') || '';

    // Build cache key
    const filters = { tab, status, daysOverdue, lender, salesRep, search };
    const cacheKey = `pc-outreach:${pcEmail}:${tab}:${JSON.stringify(filters)}`;

    // Check cache first
    const cached = outreachCache.get(cacheKey);
    if (cached && Date.now() - cached.timestamp < CACHE_TTL) {
      logApiResponse('GET /api/operations/outreach', {
        reqId,
        cached: true,
        recordCount: Array.isArray(cached.data) ? cached.data.length : 'tabData'
      });

      return NextResponse.json({
        data: cached.data,
        count: Array.isArray(cached.data) ? cached.data.length : 'tabData',
        cached: true
      });
    }

    // Fetch data from QuickBase
    let outreachData: PCOutreachRecord[] | PCOutreachTabData;

    if (tab === 'all') {
      outreachData = await getPCOutreachTabData(pcEmail, pcName, reqId);
    } else {
      switch (tab) {
        case 'initial':
          outreachData = await getPCOutreachInitial(pcEmail, pcName, reqId);
          break;
        case 'followups':
          outreachData = await getPCOutreachFollowups(pcEmail, pcName, reqId);
          break;
        case 'welcome':
          outreachData = await getPCOutreachWelcome(pcEmail, pcName, reqId);
          break;
        default:
          outreachData = await getPCOutreachInitial(pcEmail, pcName, reqId);
      }
    }

    // Apply client-side filters if needed
    let filteredData = outreachData;
    if (Array.isArray(outreachData)) {
      filteredData = applyFilters(outreachData, filters);
    }

    // Cache the result
    outreachCache.set(cacheKey, {
      data: filteredData,
      timestamp: Date.now()
    });

    // Clean up cache if it gets too large
    if (outreachCache.size > MAX_CACHE_SIZE) {
      const now = Date.now();
      for (const [key, entry] of outreachCache.entries()) {
        if (now - entry.timestamp > CACHE_TTL) {
          outreachCache.delete(key);
        }
      }
      
      // If still too large, remove oldest entries
      if (outreachCache.size > MAX_CACHE_SIZE) {
        const entries = Array.from(outreachCache.entries());
        entries.sort((a, b) => a[1].timestamp - b[1].timestamp);
        const toRemove = entries.slice(0, entries.length - MAX_CACHE_SIZE);
        toRemove.forEach(([key]) => outreachCache.delete(key));
      }
    }

    const recordCount = Array.isArray(filteredData) ? filteredData.length : 'tabData';

    logApiResponse('GET /api/operations/outreach', {
      reqId,
      recordCount,
      cached: false
    });

    return NextResponse.json({
      data: filteredData,
      count: recordCount,
      cached: false
    });

  } catch (error) {
    logError('GET /api/operations/outreach failed', error, { reqId });
    
    return NextResponse.json(
      { error: 'Failed to fetch outreach data' },
      { status: 500 }
    );
  }
}

/**
 * PATCH /api/operations/outreach
 * Update one or more outreach records
 */
export async function PATCH(request: NextRequest) {
  const reqId = `req_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
  
  try {
    logApiRequest('PATCH /api/operations/outreach', {
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

    const body = await request.json();
    const { recordIds, updates } = body;

    // Validate required fields
    if (!recordIds || !Array.isArray(recordIds) || recordIds.length === 0) {
      return NextResponse.json(
        { error: 'Record IDs are required' },
        { status: 400 }
      );
    }

    if (!updates || typeof updates !== 'object') {
      return NextResponse.json(
        { error: 'Updates object is required' },
        { status: 400 }
      );
    }

    // Build update data
    const updateData = recordIds.map(recordId => {
      const baseRecord = { [OUTREACH_RECORD_FIELDS.RECORD_ID]: { value: recordId } };
      const recordUpdates: any = { ...baseRecord };

      // Map updates to field IDs
      Object.entries(updates).forEach(([fieldName, value]) => {
        switch (fieldName) {
          case 'outreachStatus':
            recordUpdates[OUTREACH_RECORD_FIELDS.OUTREACH_STATUS] = { value };
            break;
          case 'outreachCompletedDate':
            recordUpdates[OUTREACH_RECORD_FIELDS.OUTREACH_COMPLETED_DATE] = { value };
            break;
          case 'numAttempts':
            recordUpdates[OUTREACH_RECORD_FIELDS.NUM_ATTEMPTS] = { value };
            break;
          case 'reportingDueDate':
            recordUpdates[OUTREACH_RECORD_FIELDS.REPORTING_DUE_DATE] = { value };
            break;
          case 'nextOutreachDueDate':
            recordUpdates[OUTREACH_RECORD_FIELDS.NEXT_OUTREACH_DUE_DATE] = { value };
            break;
          case 'note':
            recordUpdates[OUTREACH_RECORD_FIELDS.NOTE] = { value };
            break;
          case 'attemptNote':
            recordUpdates[OUTREACH_RECORD_FIELDS.ATTEMPT_NOTE] = { value };
            break;
          default:
            // Skip unknown fields
            break;
        }
      });

      return recordUpdates;
    });

    // Update the records
    const response = await qbClient.updateRecord({
      to: QB_TABLE_OUTREACH_RECORDS,
      data: updateData
    });

    logApiResponse('PATCH /api/operations/outreach', {
      reqId,
      updatedCount: recordIds.length,
      success: true
    });

    return NextResponse.json({
      success: true,
      updatedCount: recordIds.length,
      message: `Successfully updated ${recordIds.length} outreach record(s)`
    });

  } catch (error) {
    logError('PATCH /api/operations/outreach failed', error, { reqId });
    
    return NextResponse.json(
      { error: 'Failed to update outreach records' },
      { status: 500 }
    );
  }
}

/**
 * DELETE /api/operations/outreach
 * Delete outreach records by ID(s)
 */
export async function DELETE(request: NextRequest) {
  const reqId = `req_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
  
  try {
    logApiRequest('DELETE /api/operations/outreach', {
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

    // Get record IDs from query string or body
    const { searchParams } = new URL(request.url);
    const idParam = searchParams.get('id');
    let recordIds: number[] = [];

    if (idParam) {
      // Single ID from query string
      recordIds = [parseInt(idParam, 10)];
    } else {
      // Multiple IDs from body
      const body = await request.json();
      recordIds = body.recordIds || [];
    }

    if (!recordIds.length || recordIds.some(id => isNaN(id))) {
      return NextResponse.json(
        { error: 'Valid record IDs are required' },
        { status: 400 }
      );
    }

    // Build where clause for deletion
    const whereClause = `{${OUTREACH_RECORD_FIELDS.RECORD_ID}.XIN.${recordIds.join(',')}}`;

    // Delete the records
    const response = await qbClient.deleteRecord({
      from: QB_TABLE_OUTREACH_RECORDS,
      where: whereClause
    });

    logApiResponse('DELETE /api/operations/outreach', {
      reqId,
      deletedCount: recordIds.length,
      success: true
    });

    return NextResponse.json({
      success: true,
      deletedCount: recordIds.length,
      message: `Successfully deleted ${recordIds.length} outreach record(s)`
    });

  } catch (error) {
    logError('DELETE /api/operations/outreach failed', error, { reqId });
    
    return NextResponse.json(
      { error: 'Failed to delete outreach records' },
      { status: 500 }
    );
  }
}

/**
 * Apply client-side filters to outreach data
 */
function applyFilters(
  data: PCOutreachRecord[], 
  filters: PCOutreachFilters
): PCOutreachRecord[] {
  return data.filter(record => {
    // Status filter
    if (filters.status !== 'all' && record.outreachStatus !== filters.status) {
      return false;
    }

    // Days overdue filter
    if (filters.daysOverdue !== 'all') {
      const days = record.daysOverdue;
      switch (filters.daysOverdue) {
        case '1-3':
          if (days < 1 || days > 3) return false;
          break;
        case '4-7':
          if (days < 4 || days > 7) return false;
          break;
        case '8+':
          if (days < 8) return false;
          break;
      }
    }

    // Lender filter
    if (filters.lender !== 'all' && record.lenderName !== filters.lender) {
      return false;
    }

    // Sales rep filter
    if (filters.salesRep !== 'all' && record.salesRepName !== filters.salesRep) {
      return false;
    }

    // Search filter
    if (filters.search) {
      const searchLower = filters.search.toLowerCase();
      const matchesCustomer = record.customerName.toLowerCase().includes(searchLower);
      const matchesProject = record.projectId.toLowerCase().includes(searchLower);
      if (!matchesCustomer && !matchesProject) {
        return false;
      }
    }

    return true;
  });
}
