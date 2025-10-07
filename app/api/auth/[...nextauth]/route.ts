// app/api/auth/[...nextauth]/route.ts

import NextAuth from 'next-auth';
import { NextRequest } from 'next/server';
import { authOptions } from '@/lib/auth/next-auth.config';
import { rateLimit } from '@/lib/auth/rateLimit';
import { logWarn } from '@/lib/logging/logger';

const baseHandler = NextAuth(authOptions);

async function rateLimitedAuth(request: NextRequest, ...rest: any[]) {
  if (request.method === 'POST') {
    const ipHeader = request.headers.get('x-forwarded-for') || request.headers.get('x-real-ip') || ''
    const ipAddress = ipHeader.split(',')[0]?.trim() || 'unknown'
    let email: string | undefined
    try {
      const body = await request.clone().json().catch(() => undefined)
      email = typeof body?.email === 'string' ? body.email.toLowerCase() : undefined
    } catch {}
    const allowedIp = rateLimit(['login', ipAddress], 10, 15 * 60_000)
    const allowedEmail = email ? rateLimit(['login', email], 5, 15 * 60_000) : true
    if (!allowedIp || !allowedEmail) {
      logWarn('Rate limit exceeded for NextAuth login', { ipAddress, email: email ? '[redacted]' : undefined })
      return new Response(JSON.stringify({ error: 'Too Many Requests' }), { status: 429 })
    }
  }
  // Delegate to NextAuth
  // @ts-ignore - NextAuth handler signature
  return baseHandler(request, ...rest)
}

export { rateLimitedAuth as GET, rateLimitedAuth as POST };
