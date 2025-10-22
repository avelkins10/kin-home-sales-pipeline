// lib/auth/config.ts
import { NextAuthOptions } from 'next-auth';
import CredentialsProvider from 'next-auth/providers/credentials';

export const authOptions: NextAuthOptions = {
  providers: [
    CredentialsProvider({
      name: 'credentials',
      credentials: {
        email: { label: 'Email', type: 'email' },
        password: { label: 'Password', type: 'password' }
      },
      async authorize(credentials) {
        if (!credentials?.email || !credentials?.password) {
          return null;
        }

        // In a real implementation, you would validate credentials against your database
        // For now, return a mock user
        return {
          id: '1',
          email: credentials.email,
          name: 'Test User',
          quickbaseUserId: 'test-user-id',
          role: 'closer'
        };
      }
    })
  ],
  session: {
    strategy: 'jwt'
  },
  callbacks: {
    async jwt({ token, user }) {
      if (user) {
        token.quickbaseUserId = user.quickbaseUserId;
        token.role = user.role;
      }
      return token;
    },
    async session({ session, token }) {
      if (token) {
        session.user.quickbaseUserId = token.quickbaseUserId;
        session.user.role = token.role;
      }
      return session;
    }
  }
};
