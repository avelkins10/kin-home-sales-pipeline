import { NextRequest, NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { getQualityMetricsForUser, formatMetricsForDisplay, calculateCompositeQualityScore } from '@/lib/repcard/qualityMetrics';
import { sql } from '@/lib/db/client';

export const runtime = 'nodejs';

export async function GET(
  request: NextRequest,
  { params }: { params: { userId: string } }
) {
  const start = Date.now();
  const path = new URL(request.url).pathname;
  const requestId = request.headers.get('x-request-id') || `req-${Date.now()}`;
  
  try {
    logApiRequest('GET', path, { endpoint: 'repcard-quality-metrics', requestId });
    
    // Authentication
    const auth = await requireRole(['closer', 'setter', 'office_leader', 'regional', 'super_admin']);
    if (!auth.authorized) return auth.response;
    
    // Authorization check - users can only view their own metrics unless they're managers
    if (['closer', 'setter'].includes(auth.session.user.role) && params.userId !== auth.session.user.id) {
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 403, cached: false, requestId });
      return NextResponse.json(
        { error: 'Unauthorized: You can only view your own quality metrics' },
        { status: 403 }
      );
    }
    
    // Extract and validate parameters
    const { searchParams } = new URL(request.url);
    const startDate = searchParams.get('startDate') || new Date(new Date().getFullYear(), new Date().getMonth(), 1).toISOString().split('T')[0];
    const endDate = searchParams.get('endDate') || new Date().toISOString().split('T')[0];
    const timeRange = searchParams.get('timeRange');
    const useCache = searchParams.get('useCache') !== 'false';
    
    // Validate date format
    const dateRegex = /^\d{4}-\d{2}-\d{2}$/;
    if (!dateRegex.test(startDate) || !dateRegex.test(endDate)) {
      return NextResponse.json(
        { error: 'Invalid date format. Use YYYY-MM-DD' },
        { status: 400 }
      );
    }
    
    // Validate date range
    if (new Date(startDate) > new Date(endDate)) {
      return NextResponse.json(
        { error: 'startDate must be less than or equal to endDate' },
        { status: 400 }
      );
    }
    
    // User lookup
    const userResult = await sql`
      SELECT id, name, email, repcard_user_id, sales_office[1] as office
      FROM users
      WHERE id = ${params.userId}
    `;
    
    if (userResult.length === 0) {
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 404, cached: false, requestId });
      return NextResponse.json(
        { error: 'User not found' },
        { status: 404 }
      );
    }
    
    const user = userResult[0];
    
    // Handle missing RepCard ID
    if (!user.repcard_user_id) {
      const response = {
        hasRepcardData: false,
        message: 'User not linked to RepCard',
        userId: user.id,
        userName: user.name,
        userEmail: user.email,
        office: user.office,
        repcardUserId: null,
        metadata: {
          startDate,
          endDate,
          timeRange,
          cached: false,
          calculatedAt: new Date().toISOString()
        }
      };
      
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 200, cached: false, requestId });
      return NextResponse.json(response);
    }
    
    // Fetch quality metrics
    let qualityMetrics;
    try {
      qualityMetrics = await getQualityMetricsForUser(user.id, startDate, endDate, useCache);
    } catch (error) {
      logError('repcard-quality-metrics', error as Error, { requestId, context: 'quality metrics fetch' });
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 500, cached: false, requestId });
      return NextResponse.json(
        { 
          error: 'Failed to fetch quality metrics',
          message: error instanceof Error ? error.message : 'Unknown error'
        },
        { status: 500 }
      );
    }
    
    // Calculate composite quality score
    const compositeScore = calculateCompositeQualityScore(qualityMetrics);
    
    // Format metrics for display
    const formattedMetrics = formatMetricsForDisplay(qualityMetrics);
    
    // Build response
    const response = {
      userId: user.id,
      userName: user.name,
      userEmail: user.email,
      office: user.office,
      repcardUserId: user.repcard_user_id,
      metrics: {
        appointmentSpeed: {
          totalAppointments: qualityMetrics.appointmentSpeed.totalAppointments,
          appointmentsWithin24Hours: qualityMetrics.appointmentSpeed.appointmentsWithin24Hours,
          percentageWithin24Hours: qualityMetrics.appointmentSpeed.percentageWithin24Hours,
          averageHoursToSchedule: qualityMetrics.appointmentSpeed.averageHoursToSchedule
        },
        attachmentRate: {
          totalCustomers: qualityMetrics.attachmentRate.totalCustomers,
          customersWithAttachments: qualityMetrics.attachmentRate.customersWithAttachments,
          percentageWithAttachments: qualityMetrics.attachmentRate.percentageWithAttachments,
          totalAttachments: qualityMetrics.attachmentRate.totalAttachments
        },
        rescheduleRate: {
          totalCustomers: qualityMetrics.rescheduleRate.totalCustomers,
          totalReschedules: qualityMetrics.rescheduleRate.totalReschedules,
          averageReschedulesPerCustomer: qualityMetrics.rescheduleRate.averageReschedulesPerCustomer,
          customersWithReschedules: qualityMetrics.rescheduleRate.customersWithReschedules
        },
        followUpConsistency: {
          customersRequiringFollowUps: qualityMetrics.followUpConsistency.customersRequiringFollowUps,
          customersWithFollowUps: qualityMetrics.followUpConsistency.customersWithFollowUps,
          percentageWithFollowUps: qualityMetrics.followUpConsistency.percentageWithFollowUps,
          totalFollowUpAppointments: qualityMetrics.followUpConsistency.totalFollowUpAppointments
        }
      },
      compositeScore,
      formattedMetrics: {
        appointmentSpeed: formattedMetrics.appointmentSpeed,
        attachmentRate: formattedMetrics.attachmentRate,
        rescheduleRate: formattedMetrics.rescheduleRate,
        followUpConsistency: formattedMetrics.followUpConsistency,
        compositeScore: `${compositeScore.toFixed(1)}%`
      },
      metadata: {
        startDate,
        endDate,
        timeRange,
        cached: useCache,
        calculatedAt: new Date().toISOString()
      }
    };
    
    const duration = Date.now() - start;
    logApiResponse('GET', path, duration, { status: 200, cached: useCache, requestId });
    return NextResponse.json(response);
    
  } catch (error) {
    const duration = Date.now() - start;
    logError('repcard-quality-metrics', error as Error, { requestId, context: 'repcard-quality-metrics' });
    logApiResponse('GET', path, duration, { status: 500, cached: false, requestId });
    return NextResponse.json(
      { 
        error: 'Internal server error',
        message: error instanceof Error ? error.message : 'Unknown error',
        stack: process.env.NODE_ENV === 'development' ? (error instanceof Error ? error.stack : undefined) : undefined
      },
      { status: 500 }
    );
  }
}
