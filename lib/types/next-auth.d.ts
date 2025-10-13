// lib/types/next-auth.d.ts

import NextAuth from 'next-auth';

declare module 'next-auth' {
  interface Session {
    user: {
      id: string;
      email: string;
      name: string;
      role: string;
      quickbaseUserId: string;
      salesOffice?: string[];
      timezone: string;
    };
  }

  interface User {
    id: string;
    email: string;
    name: string;
    role: string;
    quickbaseUserId: string;
    salesOffice?: string[];
    timezone: string;
  }
}

declare module 'next-auth/jwt' {
  interface JWT {
    role: string;
    quickbaseUserId: string;
    salesOffice?: string[];
    timezone: string;
  }
}
