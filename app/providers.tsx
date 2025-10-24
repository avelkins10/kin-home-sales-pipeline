'use client';

import { QueryClient, QueryClientProvider, useQueryClient } from '@tanstack/react-query';
import { SessionProvider } from 'next-auth/react';
import { Toaster } from 'sonner';
import { useState, useEffect } from 'react';
import { initDB } from '@/lib/offline/storage';
import { syncPendingMutations } from '@/lib/offline/syncQueue';
import { AppContextProvider } from '@/lib/contexts/AppContext';
import { QUERY_KEYS } from '@/lib/constants/apps';

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
      // Use centralized query keys for both apps
      [...QUERY_KEYS.sales, ...QUERY_KEYS.operations].forEach(key => {
        queryClient.invalidateQueries({ queryKey: [key] });
      });
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
        <AppContextProvider>
          <Toaster position="top-right" />
          {children}
        </AppContextProvider>
      </SessionProvider>
    </QueryClientProvider>
  );
}
