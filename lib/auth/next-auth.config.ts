// lib/auth/next-auth.config.ts
import 'server-only'

import { NextAuthOptions } from 'next-auth';
import CredentialsProvider from 'next-auth/providers/credentials';
import { compare } from 'bcryptjs';
import { sql } from '@/lib/db/client';
import { rateLimit } from './rateLimit';
import { logWarn, logInfo } from '@/lib/logging/logger';

export const authOptions: NextAuthOptions = {
  providers: [
    CredentialsProvider({
      credentials: {
        email: { label: "Email", type: "email" },
        password: { label: "Password", type: "password" }
      },
      async authorize(credentials) {
        if (!credentials?.email || !credentials?.password) {
          logWarn('[AUTH] login failed', { email: credentials?.email, important: true });
          return null;
        }
        // Apply in-function rate limiting (defense-in-depth; primary guard is in route handler)
        const emailKey = credentials.email.toLowerCase()
        // Use distinct namespace to avoid double-counting with route-level limiter
        const allowedEmail = rateLimit(['authorize', emailKey], 5, 15 * 60_000)
        if (!allowedEmail) {
          logWarn('Rate limit (email) exceeded inside authorize')
          logWarn('[AUTH] login failed', { email: credentials.email, important: true });
          return null
        }

        const result = await sql`
          SELECT * FROM users WHERE email = ${credentials.email}
        `;

        const user = result.rows[0];
        if (!user) {
          logWarn('[AUTH] login failed', { email: credentials.email, important: true });
          return null;
        }

        const passwordValid = await compare(credentials.password, user.password_hash);
        if (!passwordValid) {
          logWarn('[AUTH] login failed', { email: credentials.email, important: true });
          return null;
        }

        logInfo('[AUTH] login success', { userId: user.id, important: true });
        return {
          id: user.id,
          email: user.email,
          name: user.name,
          role: user.role,
          quickbaseUserId: user.quickbase_user_id,
          salesOffice: user.sales_office,
        };
      }
    })
  ],
  session: {
    strategy: 'jwt',
    maxAge: 30 * 24 * 60 * 60, // 30 days
  },
  cookies: {
    sessionToken: {
      name: process.env.NODE_ENV === 'production' 
        ? '__Secure-next-auth.session-token' 
        : 'next-auth.session-token',
      options: {
        httpOnly: true,
        sameSite: 'lax',
        path: '/',
        secure: process.env.NODE_ENV === 'production', // HTTPS-only in production
      },
    },
    callbackUrl: {
      name: process.env.NODE_ENV === 'production'
        ? '__Secure-next-auth.callback-url'
        : 'next-auth.callback-url',
      options: {
        httpOnly: true,
        sameSite: 'lax',
        path: '/',
        secure: process.env.NODE_ENV === 'production',
      },
    },
    csrfToken: {
      name: process.env.NODE_ENV === 'production'
        ? '__Host-next-auth.csrf-token'
        : 'next-auth.csrf-token',
      options: {
        httpOnly: false,
        sameSite: 'lax',
        path: '/',
        secure: process.env.NODE_ENV === 'production',
      },
    },
  },
  pages: {
    signIn: '/login',
  },
  callbacks: {
    async jwt({ token, user }) {
      if (user) {
        token.id = user.id;
        token.role = user.role;
        token.quickbaseUserId = user.quickbaseUserId;
        token.salesOffice = user.salesOffice;
      }
      return token;
    },
    async session({ session, token }) {
      if (session.user) {
        session.user.id = token.id as string;
        session.user.role = token.role as string;
        session.user.quickbaseUserId = token.quickbaseUserId as string;
        session.user.salesOffice = token.salesOffice as string[];
      }
      return session;
    },
  },
  events: {
    async signIn({ user, account, profile, isNewUser }) {
      logInfo('[AUTH] signIn event', { userId: user.id, isNewUser, important: true });
    },
    async signOut({ session, token }) {
      logInfo('[AUTH] signOut event', { userId: session?.user?.id || token?.id, important: true });
    },
  },
};
