import { NextRequest, NextResponse } from 'next/server';
import { sql } from '@/lib/db/client';

/**
 * API endpoint to diagnose user linking issues
 * GET /api/admin/diagnose-user-linking
 */
export async function GET(request: NextRequest) {
  try {
    // Check repcard_users table
    const repcardUsers = await sql`
      SELECT 
        repcard_user_id,
        email,
        first_name,
        last_name,
        LOWER(email) as email_lower
      FROM repcard_users
      WHERE email IS NOT NULL AND email != ''
      LIMIT 20
    `;

    // Check users table
    const users = await sql`
      SELECT 
        id,
        email,
        name,
        repcard_user_id,
        LOWER(email) as email_lower
      FROM users
      WHERE email IS NOT NULL AND email != ''
      LIMIT 20
    `;

    // Find potential matches
    const potentialMatches = await sql`
      SELECT 
        u.id as user_id,
        u.email as user_email,
        u.name as user_name,
        u.repcard_user_id as current_repcard_id,
        ru.repcard_user_id as repcard_user_id,
        ru.email as repcard_email,
        ru.first_name || ' ' || ru.last_name as repcard_name
      FROM users u
      INNER JOIN repcard_users ru ON LOWER(u.email) = LOWER(ru.email)
      WHERE ru.email IS NOT NULL AND ru.email != ''
      ORDER BY u.email
      LIMIT 20
    `;

    // Count stats
    const stats = await sql`
      SELECT 
        (SELECT COUNT(*) FROM repcard_users WHERE email IS NOT NULL AND email != '') as repcard_users_with_email,
        (SELECT COUNT(*) FROM users WHERE email IS NOT NULL AND email != '') as users_with_email,
        (SELECT COUNT(*) FROM users WHERE repcard_user_id IS NOT NULL) as users_already_linked,
        (SELECT COUNT(*) FROM users u
         INNER JOIN repcard_users ru ON LOWER(u.email) = LOWER(ru.email)
         WHERE u.repcard_user_id IS NULL
           AND ru.email IS NOT NULL AND ru.email != '') as users_ready_to_link
    `;

    return NextResponse.json({
      success: true,
      stats: stats[0],
      repcardUsers: Array.from(repcardUsers),
      users: Array.from(users),
      potentialMatches: Array.from(potentialMatches),
      analysis: {
        repcardUsersWithEmail: stats[0]?.repcard_users_with_email || 0,
        usersWithEmail: stats[0]?.users_with_email || 0,
        usersAlreadyLinked: stats[0]?.users_already_linked || 0,
        usersReadyToLink: stats[0]?.users_ready_to_link || 0
      }
    });
  } catch (error) {
    console.error('[Diagnose API] Error:', error);
    return NextResponse.json(
      {
        success: false,
        error: error instanceof Error ? error.message : 'Unknown error'
      },
      { status: 500 }
    );
  }
}

