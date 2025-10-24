import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';

export async function GET(request: NextRequest) {
  // TODO: Add requireRole(['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin']) 
  // before implementing operations features
  const auth = await requireAuth();
  if (!auth.authorized) return auth.response;

  return NextResponse.json({ message: 'Operations API endpoint' });
}

export async function POST(request: NextRequest) {
  // TODO: Add requireRole(['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin']) 
  // before implementing operations features
  const auth = await requireAuth();
  if (!auth.authorized) return auth.response;

  return NextResponse.json({ message: 'Operations API endpoint' });
}
