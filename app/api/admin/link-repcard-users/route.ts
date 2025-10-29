import { NextRequest, NextResponse } from 'next/server';
import { linkRepCardUsersToUsers } from '@/lib/repcard/comprehensive-sync';

/**
 * API endpoint to manually link RepCard users to users table
 * POST /api/admin/link-repcard-users
 */
export async function POST(request: NextRequest) {
  try {
    console.log('[Link Users API] Starting user linking...');
    
    await linkRepCardUsersToUsers();
    
    return NextResponse.json({
      success: true,
      message: 'Users linked successfully'
    });
  } catch (error) {
    console.error('[Link Users API] Error:', error);
    return NextResponse.json(
      {
        success: false,
        error: error instanceof Error ? error.message : 'Unknown error'
      },
      { status: 500 }
    );
  }
}

