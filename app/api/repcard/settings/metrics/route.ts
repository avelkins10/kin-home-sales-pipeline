import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/next-auth.config';
import { sql } from '@/lib/db/client';

export const runtime = 'nodejs';
export const dynamic = 'force-dynamic';

/**
 * GET /api/repcard/settings/metrics
 * List all available metric definitions
 */
export async function GET(request: NextRequest) {
  try {
    const session = await getServerSession(authOptions);
    
    if (!session?.user?.role || !['super_admin', 'regional', 'office_leader'].includes(session.user.role)) {
      return NextResponse.json(
        { error: 'Forbidden - admin role required' },
        { status: 403 }
      );
    }

    const { searchParams } = new URL(request.url);
    const category = searchParams.get('category');
    const enabled = searchParams.get('enabled');

    let metrics;
    if (category && enabled !== null) {
      metrics = await sql`
        SELECT 
          id,
          metric_key,
          display_name,
          description,
          category,
          data_source,
          unit,
          format,
          aggregation_type,
          enabled,
          leaderboard_supported,
          analytics_supported,
          config
        FROM repcard_metric_definitions
        WHERE category = ${category} AND enabled = ${enabled === 'true'}
        ORDER BY category, display_name
      `;
    } else if (category) {
      metrics = await sql`
        SELECT 
          id,
          metric_key,
          display_name,
          description,
          category,
          data_source,
          unit,
          format,
          aggregation_type,
          enabled,
          leaderboard_supported,
          analytics_supported,
          config
        FROM repcard_metric_definitions
        WHERE category = ${category}
        ORDER BY category, display_name
      `;
    } else if (enabled !== null) {
      metrics = await sql`
        SELECT 
          id,
          metric_key,
          display_name,
          description,
          category,
          data_source,
          unit,
          format,
          aggregation_type,
          enabled,
          leaderboard_supported,
          analytics_supported,
          config
        FROM repcard_metric_definitions
        WHERE enabled = ${enabled === 'true'}
        ORDER BY category, display_name
      `;
    } else {
      metrics = await sql`
        SELECT 
          id,
          metric_key,
          display_name,
          description,
          category,
          data_source,
          unit,
          format,
          aggregation_type,
          enabled,
          leaderboard_supported,
          analytics_supported,
          config
        FROM repcard_metric_definitions
        ORDER BY category, display_name
      `;
    }

    return NextResponse.json({ metrics: metrics.rows || metrics });

  } catch (error) {
    console.error('[RepCard Settings] Error fetching metrics:', error);
    return NextResponse.json(
      { error: 'Failed to fetch metric definitions', details: error instanceof Error ? error.message : String(error) },
      { status: 500 }
    );
  }
}

/**
 * PUT /api/repcard/settings/metrics
 * Update metric definition (enable/disable, update config)
 */
export async function PUT(request: NextRequest) {
  try {
    const session = await getServerSession(authOptions);
    
    if (!session?.user?.role || session.user.role !== 'super_admin') {
      return NextResponse.json(
        { error: 'Forbidden - super_admin role required' },
        { status: 403 }
      );
    }

    const body = await request.json();
    const { metric_key, enabled, config } = body;

    if (!metric_key) {
      return NextResponse.json(
        { error: 'Missing required field: metric_key' },
        { status: 400 }
      );
    }

    if (enabled === undefined && config === undefined) {
      return NextResponse.json(
        { error: 'No fields to update' },
        { status: 400 }
      );
    }

    let result;
    if (enabled !== undefined && config !== undefined) {
      result = await sql`
        UPDATE repcard_metric_definitions
        SET enabled = ${enabled}, config = ${JSON.stringify(config)}, updated_at = NOW()
        WHERE metric_key = ${metric_key}
        RETURNING *
      `;
    } else if (enabled !== undefined) {
      result = await sql`
        UPDATE repcard_metric_definitions
        SET enabled = ${enabled}, updated_at = NOW()
        WHERE metric_key = ${metric_key}
        RETURNING *
      `;
    } else {
      result = await sql`
        UPDATE repcard_metric_definitions
        SET config = ${JSON.stringify(config)}, updated_at = NOW()
        WHERE metric_key = ${metric_key}
        RETURNING *
      `;
    }
    
    const metric = result.rows?.[0];

    if (!metric) {
      return NextResponse.json(
        { error: 'Metric definition not found' },
        { status: 404 }
      );
    }

    return NextResponse.json({ metric });

  } catch (error: any) {
    console.error('[RepCard Settings] Error updating metric:', error);
    return NextResponse.json(
      { error: 'Failed to update metric definition' },
      { status: 500 }
    );
  }
}

