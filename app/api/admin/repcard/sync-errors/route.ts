import { NextRequest, NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { sql } from '@/lib/db/client';

export const runtime = 'nodejs';

/**
 * GET /api/admin/repcard/sync-errors
 * 
 * Analyzes sync failures and provides insights into what's failing and why
 */
export async function GET(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin']);
    if (!auth.authorized) return auth.response;

    const { searchParams } = new URL(request.url);
    const entityType = searchParams.get('entityType') || undefined;
    const limit = parseInt(searchParams.get('limit') || '20');

    // Get sync history with failure analysis
    let query;
    if (entityType) {
      query = sql`
        SELECT 
          entity_type,
          sync_type,
          status,
          started_at,
          completed_at,
          records_fetched,
          records_inserted,
          records_updated,
          records_failed,
          error_message,
          error_details,
          CASE 
            WHEN records_fetched > 0 THEN 
              ROUND((records_failed::numeric / records_fetched::numeric) * 100, 2)
            ELSE 0
          END as failure_rate_percent
        FROM repcard_sync_log
        WHERE entity_type = ${entityType}
        ORDER BY started_at DESC
        LIMIT ${limit}
      `;
    } else {
      query = sql`
        SELECT 
          entity_type,
          sync_type,
          status,
          started_at,
          completed_at,
          records_fetched,
          records_inserted,
          records_updated,
          records_failed,
          error_message,
          error_details,
          CASE 
            WHEN records_fetched > 0 THEN 
              ROUND((records_failed::numeric / records_fetched::numeric) * 100, 2)
            ELSE 0
          END as failure_rate_percent
        FROM repcard_sync_log
        ORDER BY started_at DESC
        LIMIT ${limit}
      `;
    }

    const syncHistory = Array.from(await query);

    // Get latest sync for each entity type with failure analysis
    const latestSyncsRaw = await sql`
      SELECT DISTINCT ON (entity_type)
        entity_type,
        sync_type,
        status,
        started_at,
        completed_at,
        records_fetched,
        records_inserted,
        records_updated,
        records_failed,
        error_message,
        CASE 
          WHEN records_fetched > 0 THEN 
            ROUND((records_failed::numeric / records_fetched::numeric) * 100, 2)
          ELSE 0
        END as failure_rate_percent,
        CASE 
          WHEN records_failed = 0 THEN 'healthy'
          WHEN records_failed::numeric / NULLIF(records_fetched, 0)::numeric < 0.01 THEN 'low'
          WHEN records_failed::numeric / NULLIF(records_fetched, 0)::numeric < 0.10 THEN 'moderate'
          ELSE 'high'
        END as failure_severity
      FROM repcard_sync_log
      ORDER BY entity_type, started_at DESC
    `;
    const latestSyncs = Array.from(latestSyncsRaw);

    // Calculate summary statistics
    const summary = {
      totalSyncs: syncHistory.length,
      totalFailed: syncHistory.filter((s: any) => s.status === 'failed').length,
      totalCompleted: syncHistory.filter((s: any) => s.status === 'completed').length,
      totalRunning: syncHistory.filter((s: any) => s.status === 'running').length,
      entitiesWithHighFailures: latestSyncs.filter((s: any) => s.failure_severity === 'high').map((s: any) => ({
        entityType: s.entity_type,
        failureRate: s.failure_rate_percent,
        recordsFailed: s.records_failed,
        recordsFetched: s.records_fetched
      })),
      criticalEntitiesStatus: latestSyncs
        .filter((s: any) => ['appointments', 'customers', 'users', 'offices'].includes(s.entity_type))
        .map((s: any) => ({
          entityType: s.entity_type,
          status: s.status,
          failureRate: s.failure_rate_percent,
          recordsFailed: s.records_failed,
          recordsFetched: s.records_fetched,
          severity: s.failure_severity
        }))
    };

    return NextResponse.json({
      success: true,
      summary,
      latestSyncs,
      recentHistory: syncHistory,
      analysis: {
        criticalEntitiesHealthy: summary.criticalEntitiesStatus.every((e: any) => e.severity === 'healthy' || e.severity === 'low'),
        highFailureEntities: summary.entitiesWithHighFailures,
        recommendations: generateRecommendations(summary, latestSyncs)
      }
    });

  } catch (error) {
    console.error('[Sync Errors] Error:', error);
    return NextResponse.json(
      {
        success: false,
        error: 'Failed to analyze sync errors',
        message: error instanceof Error ? error.message : 'Unknown error',
      },
      { status: 500 }
    );
  }
}

function generateRecommendations(summary: any, latestSyncs: any[]): string[] {
  const recommendations: string[] = [];

  // Check critical entities
  const criticalFailures = summary.criticalEntitiesStatus.filter((e: any) => 
    e.severity === 'moderate' || e.severity === 'high'
  );

  if (criticalFailures.length > 0) {
    recommendations.push(
      `⚠️ Critical entities with failures: ${criticalFailures.map((e: any) => e.entityType).join(', ')}. These need immediate attention.`
    );
  } else {
    recommendations.push('✅ Critical entities (appointments, customers, users, offices) are healthy with low failure rates.');
  }

  // Check high failure entities
  if (summary.entitiesWithHighFailures.length > 0) {
    const highFailureList = summary.entitiesWithHighFailures
      .map((e: any) => `${e.entityType} (${e.failureRate}% failure rate)`)
      .join(', ');
    recommendations.push(
      `⚠️ High failure entities: ${highFailureList}. These may be optional or have data format issues.`
    );
  }

  // Check for stuck/running syncs
  const runningSyncs = latestSyncs.filter((s: any) => s.status === 'running');
  const oldRunningSyncs = runningSyncs.filter((s: any) => {
    const startedAt = new Date(s.started_at);
    const hoursAgo = (Date.now() - startedAt.getTime()) / (1000 * 60 * 60);
    return hoursAgo > 1; // Running for more than 1 hour
  });

  if (oldRunningSyncs.length > 0) {
    recommendations.push(
      `⚠️ Stuck syncs detected: ${oldRunningSyncs.map((s: any) => s.entity_type).join(', ')}. These may need manual intervention.`
    );
  }

  // Overall health
  const totalFailures = latestSyncs.reduce((sum: number, s: any) => sum + (s.records_failed || 0), 0);
  const totalFetched = latestSyncs.reduce((sum: number, s: any) => sum + (s.records_fetched || 0), 0);
  const overallFailureRate = totalFetched > 0 ? (totalFailures / totalFetched) * 100 : 0;

  if (overallFailureRate < 1) {
    recommendations.push('✅ Overall sync health is good. Low failure rates across all entities.');
  } else if (overallFailureRate < 5) {
    recommendations.push('⚠️ Overall sync health is moderate. Some entities have elevated failure rates.');
  } else {
    recommendations.push('❌ Overall sync health needs attention. High failure rates detected across multiple entities.');
  }

  return recommendations;
}
