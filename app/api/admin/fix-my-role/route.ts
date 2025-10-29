// app/api/admin/fix-my-role/route.ts
import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/next-auth.config';
import { sql } from '@/lib/db/client';

export const runtime = 'nodejs';

/**
 * POST /api/admin/fix-my-role
 * Fix user role to super_admin (for Austin)
 */
export async function POST(request: NextRequest) {
  try {
    const session = await getServerSession(authOptions);

    if (!session?.user?.email) {
      return NextResponse.json(
        { error: 'Not authenticated' },
        { status: 401 }
      );
    }

    const userEmail = session.user.email;

    // Only allow for Austin's email
    if (!userEmail.toLowerCase().includes('austin')) {
      return NextResponse.json(
        { error: 'This endpoint is only for Austin' },
        { status: 403 }
      );
    }

    // Update role to super_admin and name to Austin Admin
    await sql`
      UPDATE users
      SET role = 'super_admin',
          name = 'Austin Admin'
      WHERE email = ${userEmail}
    `;

    // Get updated user
    const updatedUser = await sql`
      SELECT id, name, email, role
      FROM users
      WHERE email = ${userEmail}
    `;

    return NextResponse.json({
      success: true,
      message: 'Role updated to super_admin',
      user: Array.from(updatedUser)[0]
    });

  } catch (error) {
    console.error('Failed to fix role:', error);
    return NextResponse.json(
      { error: 'Internal server error', details: error instanceof Error ? error.message : 'Unknown error' },
      { status: 500 }
    );
  }
}
