'use client'

import { createContext, useContext, useState, useEffect, ReactNode } from 'react';
import { usePathname } from 'next/navigation';

type AppType = 'sales' | 'operations';

interface AppContextType {
  currentApp: AppType;
  setCurrentApp: (app: AppType) => void;
}

const AppContext = createContext<AppContextType | undefined>(undefined);

interface AppContextProviderProps {
  children: ReactNode;
}

export function AppContextProvider({ children }: AppContextProviderProps) {
  // Initialize state with URL-derived value
  const [currentApp, setCurrentApp] = useState<AppType>(() => {
    if (typeof window !== 'undefined') {
      return window.location.pathname.startsWith('/operations') ? 'operations' : 'sales';
    }
    return 'sales';
  });
  const pathname = usePathname();

  // Auto-detect app based on URL path - this is the source of truth
  useEffect(() => {
    if (pathname.startsWith('/operations')) {
      setCurrentApp('operations');
    } else {
      setCurrentApp('sales');
    }
  }, [pathname]);

  // Save to localStorage when currentApp changes (but don't override URL-derived state)
  useEffect(() => {
    localStorage.setItem('kinetic-current-app', currentApp);
  }, [currentApp]);

  // AppContext behavior documentation:
  // - URL pathname is the authoritative source of truth for current app
  // - setCurrentApp is for convenience only and should trigger immediate navigation
  // - Consumers should always navigate after calling setCurrentApp to avoid flicker
  // - The context automatically syncs with URL changes via usePathname
  const value = {
    currentApp,
    setCurrentApp,
  };

  return (
    <AppContext.Provider value={value}>
      {children}
    </AppContext.Provider>
  );
}

export function useAppContext() {
  const context = useContext(AppContext);
  if (context === undefined) {
    throw new Error('useAppContext must be used within an AppContextProvider');
  }
  return context;
}
