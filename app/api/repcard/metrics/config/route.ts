import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';
import {
  getAllMetricConfigs,
  getMetricConfig,
  saveMetricConfig,
  deleteMetricConfig,
  type MetricConfig
} from '@/lib/repcard/metric-config';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

export const runtime = 'nodejs';

/**
 * GET /api/repcard/metrics/config
 * 
 * Get metric configurations
 * Query params:
 * - metric: Optional metric name to filter by
 */
export async function GET(request: NextRequest) {
  const requestId = crypto.randomUUID();
  
  try {
    logApiRequest('repcard-metrics-config-get', request, { requestId });
    
    // Check authentication
    const session = await getServerSession(authOptions);
    if (!session?.user) {
      return NextResponse.json(
        { error: 'Unauthorized' },
        { status: 401 }
      );
    }
    
    // Only admins can view configurations
    const adminRoles = ['super_admin', 'regional', 'office_leader'];
    if (!adminRoles.includes(session.user.role || '')) {
      return NextResponse.json(
        { error: 'Forbidden - Admin access required' },
        { status: 403 }
      );
    }
    
    const { searchParams } = new URL(request.url);
    const metricName = searchParams.get('metric');
    
    let configs: MetricConfig[];
    if (metricName) {
      configs = await getMetricConfig(metricName as any);
    } else {
      configs = await getAllMetricConfigs();
    }
    
    const response = {
      configs,
      count: configs.length
    };
    
    logApiResponse('repcard-metrics-config-get', request, response, { requestId });
    
    return NextResponse.json(response);
    
  } catch (error) {
    logError('repcard-metrics-config-get', error as Error, { requestId });
    return NextResponse.json(
      { 
        error: 'Failed to get metric configurations',
        message: error instanceof Error ? error.message : 'Unknown error'
      },
      { status: 500 }
    );
  }
}

/**
 * POST /api/repcard/metrics/config
 * 
 * Save metric configuration
 * Body: { metric_name, disposition_pattern?, status_category?, is_included, priority }
 */
export async function POST(request: NextRequest) {
  const requestId = crypto.randomUUID();
  
  try {
    logApiRequest('repcard-metrics-config-post', request, { requestId });
    
    // Check authentication
    const session = await getServerSession(authOptions);
    if (!session?.user) {
      return NextResponse.json(
        { error: 'Unauthorized' },
        { status: 401 }
      );
    }
    
    // Only admins can save configurations
    const adminRoles = ['super_admin', 'regional', 'office_leader'];
    if (!adminRoles.includes(session.user.role || '')) {
      return NextResponse.json(
        { error: 'Forbidden - Admin access required' },
        { status: 403 }
      );
    }
    
    const body = await request.json();
    const { metric_name, disposition_pattern, status_category, is_included, priority } = body;
    
    // Validate required fields
    if (!metric_name || typeof is_included !== 'boolean') {
      return NextResponse.json(
        { error: 'Missing required fields: metric_name, is_included' },
        { status: 400 }
      );
    }
    
    // Save configuration
    const config = await saveMetricConfig({
      metric_name,
      disposition_pattern: disposition_pattern || null,
      status_category: status_category || null,
      is_included,
      priority: priority || 0
    });
    
    const response = {
      config,
      message: 'Configuration saved successfully'
    };
    
    logApiResponse('repcard-metrics-config-post', request, response, { requestId });
    
    return NextResponse.json(response);
    
  } catch (error) {
    logError('repcard-metrics-config-post', error as Error, { requestId });
    return NextResponse.json(
      { 
        error: 'Failed to save metric configuration',
        message: error instanceof Error ? error.message : 'Unknown error'
      },
      { status: 500 }
    );
  }
}

/**
 * DELETE /api/repcard/metrics/config
 * 
 * Delete metric configuration
 * Query params:
 * - id: Configuration ID to delete
 */
export async function DELETE(request: NextRequest) {
  const requestId = crypto.randomUUID();
  
  try {
    logApiRequest('repcard-metrics-config-delete', request, { requestId });
    
    // Check authentication
    const session = await getServerSession(authOptions);
    if (!session?.user) {
      return NextResponse.json(
        { error: 'Unauthorized' },
        { status: 401 }
      );
    }
    
    // Only admins can delete configurations
    const adminRoles = ['super_admin', 'regional', 'office_leader'];
    if (!adminRoles.includes(session.user.role || '')) {
      return NextResponse.json(
        { error: 'Forbidden - Admin access required' },
        { status: 403 }
      );
    }
    
    const { searchParams } = new URL(request.url);
    const id = searchParams.get('id');
    
    if (!id) {
      return NextResponse.json(
        { error: 'Missing required parameter: id' },
        { status: 400 }
      );
    }
    
    await deleteMetricConfig(id);
    
    const response = {
      message: 'Configuration deleted successfully'
    };
    
    logApiResponse('repcard-metrics-config-delete', request, response, { requestId });
    
    return NextResponse.json(response);
    
  } catch (error) {
    logError('repcard-metrics-config-delete', error as Error, { requestId });
    return NextResponse.json(
      { 
        error: 'Failed to delete metric configuration',
        message: error instanceof Error ? error.message : 'Unknown error'
      },
      { status: 500 }
    );
  }
}
