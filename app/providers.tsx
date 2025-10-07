'use client';

import { QueryClient, QueryClientProvider, useQueryClient } from '@tanstack/react-query';
import { SessionProvider } from 'next-auth/react';
import { Toaster } from 'sonner';
import { useState, useEffect } from 'react';
import { initDB } from '@/lib/offline/storage';
import { syncPendingMutations } from '@/lib/offline/syncQueue';

export function Providers({ children }: { children: React.ReactNode }) {
  const [queryClient] = useState(() => new QueryClient({
    defaultOptions: {
      queries: {
        staleTime: 300000, // 5 minutes - harmonized with page-level options
        refetchOnWindowFocus: false,
        networkMode: 'offlineFirst', // Try cache first when offline
      },
      mutations: {
        retry: 1,
      },
    },
  }));

  // Initialize offline support
  useEffect(() => {
    const initializeOffline = async () => {
      try {
        await initDB();
        await syncPendingMutations();
        console.log('Offline support initialized');
      } catch (error) {
        console.error('Failed to initialize offline support:', error);
      }
    };

    initializeOffline();

    // Add online/offline event listeners
    const handleOnline = () => {
      console.log('Online');
      syncPendingMutations();
      // Invalidate queries to refresh data when back online
      queryClient.invalidateQueries({ queryKey: ['projects'] });
      queryClient.invalidateQueries({ queryKey: ['dashboard-metrics'] });
    };

    const handleOffline = () => {
      console.log('Offline');
    };

    window.addEventListener('online', handleOnline);
    window.addEventListener('offline', handleOffline);

    return () => {
      window.removeEventListener('online', handleOnline);
      window.removeEventListener('offline', handleOffline);
    };
  }, [queryClient]);

  return (
    <QueryClientProvider client={queryClient}>
      <SessionProvider>
        <Toaster position="top-right" />
        {children}
      </SessionProvider>
    </QueryClientProvider>
  );
}
