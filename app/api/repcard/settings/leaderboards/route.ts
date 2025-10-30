import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/next-auth.config';
import { sql } from '@/lib/db/client';

/**
 * GET /api/repcard/settings/leaderboards
 * List all leaderboard configurations
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
        leaderboard_type,
        enabled_metrics,
        rank_by_metric,
        display_order,
        date_range_default,
        roles,
        office_ids,
        is_default,
        enabled,
        created_by,
        created_at,
        updated_at
      FROM repcard_leaderboard_config
      ORDER BY display_order ASC, created_at DESC
    `;

    return NextResponse.json({ configs: configs.rows || configs });

  } catch (error) {
    console.error('[RepCard Settings] Error fetching leaderboard configs:', error);
    return NextResponse.json(
      { error: 'Failed to fetch leaderboard configurations' },
      { status: 500 }
    );
  }
}

/**
 * POST /api/repcard/settings/leaderboards
 * Create new leaderboard configuration
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
      leaderboard_type,
      enabled_metrics,
      rank_by_metric,
      display_order = 0,
      date_range_default = 'month',
      roles = [],
      office_ids = [],
      is_default = false,
      enabled = true
    } = body;

    // Validate required fields
    if (!name || !leaderboard_type || !enabled_metrics || !rank_by_metric) {
      return NextResponse.json(
        { error: 'Missing required fields: name, leaderboard_type, enabled_metrics, rank_by_metric' },
        { status: 400 }
      );
    }

    // Validate enabled_metrics is an array
    if (!Array.isArray(enabled_metrics) || enabled_metrics.length === 0) {
      return NextResponse.json(
        { error: 'enabled_metrics must be a non-empty array' },
        { status: 400 }
      );
    }

    // If setting as default, unset other defaults
    if (is_default) {
      await sql`
        UPDATE repcard_leaderboard_config
        SET is_default = false
        WHERE is_default = true
      `;
    }

    const result = await sql`
      INSERT INTO repcard_leaderboard_config (
        name,
        description,
        leaderboard_type,
        enabled_metrics,
        rank_by_metric,
        display_order,
        date_range_default,
        roles,
        office_ids,
        is_default,
        enabled,
        created_by
      )
      VALUES (
        ${name},
        ${description || null},
        ${leaderboard_type},
        ${enabled_metrics},
        ${rank_by_metric},
        ${display_order},
        ${date_range_default},
        ${roles.length > 0 ? roles : null},
        ${office_ids.length > 0 ? office_ids : null},
        ${is_default},
        ${enabled},
        ${session.user.id}
      )
      RETURNING *
    `;

    const config = result.rows?.[0] || result[0];

    return NextResponse.json({ config }, { status: 201 });

  } catch (error: any) {
    console.error('[RepCard Settings] Error creating leaderboard config:', error);
    
    if (error.code === '23505') { // Unique constraint violation
      return NextResponse.json(
        { error: 'A leaderboard configuration with this name already exists' },
        { status: 409 }
      );
    }

    return NextResponse.json(
      { error: 'Failed to create leaderboard configuration' },
      { status: 500 }
    );
  }
}

/**
 * PUT /api/repcard/settings/leaderboards
 * Update leaderboard configuration
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

    // If setting as default, unset other defaults
    if (updates.is_default === true) {
      await sql`
        UPDATE repcard_leaderboard_config
        SET is_default = false
        WHERE is_default = true AND id != ${id}
      `;
    }

    // Build dynamic update query
    const updateFields: string[] = [];
    const updateValues: any[] = [];
    let paramIndex = 1;

    if (updates.name !== undefined) {
      updateFields.push(`name = $${paramIndex++}`);
      updateValues.push(updates.name);
    }
    if (updates.description !== undefined) {
      updateFields.push(`description = $${paramIndex++}`);
      updateValues.push(updates.description);
    }
    if (updates.leaderboard_type !== undefined) {
      updateFields.push(`leaderboard_type = $${paramIndex++}`);
      updateValues.push(updates.leaderboard_type);
    }
    if (updates.enabled_metrics !== undefined) {
      updateFields.push(`enabled_metrics = $${paramIndex++}`);
      updateValues.push(updates.enabled_metrics);
    }
    if (updates.rank_by_metric !== undefined) {
      updateFields.push(`rank_by_metric = $${paramIndex++}`);
      updateValues.push(updates.rank_by_metric);
    }
    if (updates.display_order !== undefined) {
      updateFields.push(`display_order = $${paramIndex++}`);
      updateValues.push(updates.display_order);
    }
    if (updates.date_range_default !== undefined) {
      updateFields.push(`date_range_default = $${paramIndex++}`);
      updateValues.push(updates.date_range_default);
    }
    if (updates.roles !== undefined) {
      updateFields.push(`roles = $${paramIndex++}`);
      updateValues.push(updates.roles.length > 0 ? updates.roles : null);
    }
    if (updates.office_ids !== undefined) {
      updateFields.push(`office_ids = $${paramIndex++}`);
      updateValues.push(updates.office_ids.length > 0 ? updates.office_ids : null);
    }
    if (updates.is_default !== undefined) {
      updateFields.push(`is_default = $${paramIndex++}`);
      updateValues.push(updates.is_default);
    }
    if (updates.enabled !== undefined) {
      updateFields.push(`enabled = $${paramIndex++}`);
      updateValues.push(updates.enabled);
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
      UPDATE repcard_leaderboard_config
      SET ${updateFields.join(', ')}
      WHERE id = $${paramIndex}
      RETURNING *
    `;

    const result = await sql.query(query, updateValues);
    const config = result.rows?.[0] || result[0];

    if (!config) {
      return NextResponse.json(
        { error: 'Leaderboard configuration not found' },
        { status: 404 }
      );
    }

    return NextResponse.json({ config });

  } catch (error: any) {
    console.error('[RepCard Settings] Error updating leaderboard config:', error);
    return NextResponse.json(
      { error: 'Failed to update leaderboard configuration' },
      { status: 500 }
    );
  }
}

/**
 * DELETE /api/repcard/settings/leaderboards
 * Delete leaderboard configuration
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

    // Prevent deleting default config
    const existing = await sql`
      SELECT is_default FROM repcard_leaderboard_config WHERE id = ${id}
    `;
    const config = existing.rows?.[0] || existing[0];

    if (config?.is_default) {
      return NextResponse.json(
        { error: 'Cannot delete default leaderboard configuration' },
        { status: 400 }
      );
    }

    await sql`
      DELETE FROM repcard_leaderboard_config WHERE id = ${id}
    `;

    return NextResponse.json({ success: true });

  } catch (error) {
    console.error('[RepCard Settings] Error deleting leaderboard config:', error);
    return NextResponse.json(
      { error: 'Failed to delete leaderboard configuration' },
      { status: 500 }
    );
  }
}

