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

    let query = `
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
      WHERE 1=1
    `;
    const params: any[] = [];
    let paramIndex = 1;

    if (category) {
      query += ` AND category = $${paramIndex++}`;
      params.push(category);
    }

    if (enabled !== null) {
      query += ` AND enabled = $${paramIndex++}`;
      params.push(enabled === 'true');
    }

    query += ` ORDER BY category, display_name`;

    const metrics = await sql.query(query, params);

    return NextResponse.json({ metrics: metrics.rows || metrics });

  } catch (error) {
    console.error('[RepCard Settings] Error fetching metrics:', error);
    return NextResponse.json(
      { error: 'Failed to fetch metric definitions' },
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

    const updateFields: string[] = [];
    const updateValues: any[] = [];
    let paramIndex = 1;

    if (enabled !== undefined) {
      updateFields.push(`enabled = $${paramIndex++}`);
      updateValues.push(enabled);
    }

    if (config !== undefined) {
      updateFields.push(`config = $${paramIndex++}`);
      updateValues.push(JSON.stringify(config));
    }

    if (updateFields.length === 0) {
      return NextResponse.json(
        { error: 'No fields to update' },
        { status: 400 }
      );
    }

    updateFields.push(`updated_at = NOW()`);
    updateValues.push(metric_key);

    const query = `
      UPDATE repcard_metric_definitions
      SET ${updateFields.join(', ')}
      WHERE metric_key = $${paramIndex}
      RETURNING *
    `;

    const result = await sql.query(query, updateValues);
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

