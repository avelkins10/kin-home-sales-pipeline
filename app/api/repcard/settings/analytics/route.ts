import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/next-auth.config';
import { sql } from '@/lib/db/client';

export const runtime = 'nodejs';
export const dynamic = 'force-dynamic';

/**
 * GET /api/repcard/settings/analytics
 * List all analytics widget configurations
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

    const configs = await sql`
      SELECT 
        id,
        name,
        description,
        widget_type,
        metric_type,
        date_range_default,
        refresh_interval,
        enabled,
        display_order,
        roles,
        office_ids,
        config,
        created_by,
        created_at,
        updated_at
      FROM repcard_analytics_config
      WHERE enabled = true
      ORDER BY display_order ASC, created_at DESC
    `;

    return NextResponse.json({ configs: configs.rows || configs });

  } catch (error) {
    console.error('[RepCard Settings] Error fetching analytics configs:', error);
    return NextResponse.json(
      { error: 'Failed to fetch analytics configurations' },
      { status: 500 }
    );
  }
}

/**
 * POST /api/repcard/settings/analytics
 * Create new analytics widget configuration
 */
export async function POST(request: NextRequest) {
  try {
    const session = await getServerSession(authOptions);
    
    if (!session?.user?.role || session.user.role !== 'super_admin') {
      return NextResponse.json(
        { error: 'Forbidden - super_admin role required' },
        { status: 403 }
      );
    }

    const body = await request.json();
    const {
      name,
      description,
      widget_type,
      metric_type,
      date_range_default = 'month',
      refresh_interval = 30,
      enabled = true,
      display_order = 0,
      roles = [],
      office_ids = [],
      config = {}
    } = body;

    // Validate required fields
    if (!name || !widget_type || !metric_type) {
      return NextResponse.json(
        { error: 'Missing required fields: name, widget_type, metric_type' },
        { status: 400 }
      );
    }

    const result = await sql`
      INSERT INTO repcard_analytics_config (
        name,
        description,
        widget_type,
        metric_type,
        date_range_default,
        refresh_interval,
        enabled,
        display_order,
        roles,
        office_ids,
        config,
        created_by
      )
      VALUES (
        ${name},
        ${description || null},
        ${widget_type},
        ${metric_type},
        ${date_range_default},
        ${refresh_interval},
        ${enabled},
        ${display_order},
        ${roles.length > 0 ? roles : null},
        ${office_ids.length > 0 ? office_ids : null},
        ${JSON.stringify(config)},
        ${session.user.id}
      )
      RETURNING *
    `;

    const analyticsConfig = result.rows?.[0] || result[0];

    return NextResponse.json({ config: analyticsConfig }, { status: 201 });

  } catch (error: any) {
    console.error('[RepCard Settings] Error creating analytics config:', error);
    
    if (error.code === '23505') { // Unique constraint violation
      return NextResponse.json(
        { error: 'An analytics configuration with this name already exists' },
        { status: 409 }
      );
    }

    return NextResponse.json(
      { error: 'Failed to create analytics configuration' },
      { status: 500 }
    );
  }
}

/**
 * PUT /api/repcard/settings/analytics
 * Update analytics widget configuration
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
    const { id, ...updates } = body;

    if (!id) {
      return NextResponse.json(
        { error: 'Missing required field: id' },
        { status: 400 }
      );
    }

    // Build dynamic update query
    const updateFields: string[] = [];
    const updateValues: any[] = [];
    let paramIndex = 1;

    const allowedFields = [
      'name', 'description', 'widget_type', 'metric_type', 'date_range_default',
      'refresh_interval', 'enabled', 'display_order', 'roles', 'office_ids', 'config'
    ];

    for (const field of allowedFields) {
      if (updates[field] !== undefined) {
        if (field === 'config') {
          updateFields.push(`${field} = $${paramIndex++}`);
          updateValues.push(JSON.stringify(updates[field]));
        } else if (field === 'roles' || field === 'office_ids') {
          updateFields.push(`${field} = $${paramIndex++}`);
          updateValues.push(updates[field].length > 0 ? updates[field] : null);
        } else {
          updateFields.push(`${field} = $${paramIndex++}`);
          updateValues.push(updates[field]);
        }
      }
    }

    if (updateFields.length === 0) {
      return NextResponse.json(
        { error: 'No fields to update' },
        { status: 400 }
      );
    }

    updateFields.push(`updated_at = NOW()`);
    updateValues.push(id);

    const query = `
      UPDATE repcard_analytics_config
      SET ${updateFields.join(', ')}
      WHERE id = $${paramIndex}
      RETURNING *
    `;

    const result = await sql.query(query, updateValues);
    const config = result.rows?.[0] || result[0];

    if (!config) {
      return NextResponse.json(
        { error: 'Analytics configuration not found' },
        { status: 404 }
      );
    }

    return NextResponse.json({ config });

  } catch (error: any) {
    console.error('[RepCard Settings] Error updating analytics config:', error);
    return NextResponse.json(
      { error: 'Failed to update analytics configuration' },
      { status: 500 }
    );
  }
}

/**
 * DELETE /api/repcard/settings/analytics
 * Delete analytics widget configuration
 */
export async function DELETE(request: NextRequest) {
  try {
    const session = await getServerSession(authOptions);
    
    if (!session?.user?.role || session.user.role !== 'super_admin') {
      return NextResponse.json(
        { error: 'Forbidden - super_admin role required' },
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

    await sql`
      DELETE FROM repcard_analytics_config WHERE id = ${id}
    `;

    return NextResponse.json({ success: true });

  } catch (error) {
    console.error('[RepCard Settings] Error deleting analytics config:', error);
    return NextResponse.json(
      { error: 'Failed to delete analytics configuration' },
      { status: 500 }
    );
  }
}

