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
    };
  }

  interface User {
    id: string;
    email: string;
    name: string;
    role: string;
    quickbaseUserId: string;
    salesOffice?: string[];
  }
}

declare module 'next-auth/jwt' {
  interface JWT {
    role: string;
    quickbaseUserId: string;
    salesOffice?: string[];
  }
}
