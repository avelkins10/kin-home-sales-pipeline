'use client';

import { useState, useEffect } from 'react';
import { WifiOff, Wifi } from 'lucide-react';
import { getPendingHoldUpdates } from '@/lib/offline/offlineMutations';
import { cn } from '@/lib/utils/cn';

export function OfflineIndicator() {
  const [isOnline, setIsOnline] = useState(true);
  const [pendingCount, setPendingCount] = useState(0);
  const [showIndicator, setShowIndicator] = useState(false);
  const [updateAvailable, setUpdateAvailable] = useState<null | { version: number }>(null);

  useEffect(() => {
    // Initialize online status
    setIsOnline(navigator.onLine);

    const handleOnline = () => {
      setIsOnline(true);
      setShowIndicator(false);
    };

    const handleOffline = () => {
      setIsOnline(false);
      setShowIndicator(true);
    };

    // Add event listeners
    window.addEventListener('online', handleOnline);
    window.addEventListener('offline', handleOffline);

    // Update pending count periodically when offline
    const updatePendingCount = async () => {
      const count = await getPendingHoldUpdates();
      setPendingCount(count);
      
      // Show indicator if there are pending updates
      if (count > 0) {
        setShowIndicator(true);
      }
    };

    updatePendingCount();
    const interval = setInterval(updatePendingCount, 5000); // Every 5 seconds

    // Listen for SW update messages
    const onSwMessage = (event: MessageEvent) => {
      if (event.data?.type === 'SW_UPDATED') {
        setUpdateAvailable({ version: event.data.version });
        setShowIndicator(true);
      }
    };
    navigator.serviceWorker?.addEventListener?.('message', onSwMessage as any);

    return () => {
      window.removeEventListener('online', handleOnline);
      window.removeEventListener('offline', handleOffline);
      clearInterval(interval);
      navigator.serviceWorker?.removeEventListener?.('message', onSwMessage as any);
    };
  }, []);

  // Don't render if online and no pending updates
  if (isOnline && pendingCount === 0) {
    return null;
  }

  return (
    <div
      className={cn(
        'fixed top-0 left-0 right-0 z-50 py-2 px-4 transition-all duration-300',
        'flex items-center justify-center gap-2 text-sm font-medium',
        showIndicator ? 'translate-y-0 opacity-100' : '-translate-y-full opacity-0',
        // Offline state
        !isOnline && 'bg-amber-50 border-b border-amber-200 text-amber-800',
        // Syncing state
        isOnline && pendingCount > 0 && 'bg-blue-50 border-b border-blue-200 text-blue-800',
        // Update available
        updateAvailable && 'bg-emerald-50 border-b border-emerald-200 text-emerald-800'
      )}
      role="status"
      aria-live="polite"
    >
      {!isOnline ? (
        <>
          <WifiOff className="h-4 w-4 text-amber-600" data-testid="wifi-off-icon" />
          <span>You are offline</span>
          {pendingCount > 0 && (
            <span className="bg-amber-200 text-amber-800 px-2 py-1 rounded-full text-xs">
              {pendingCount} pending update{pendingCount > 1 ? 's' : ''}
            </span>
          )}
        </>
      ) : updateAvailable ? (
        <>
          <span>New version available</span>
          <button
            className="ml-2 px-2 py-1 text-xs rounded bg-emerald-600 text-white"
            onClick={() => navigator.serviceWorker?.getRegistration()?.then(reg => reg?.waiting?.postMessage?.({ command: 'SKIP_WAITING' }))}
          >
            Update
          </button>
        </>
      ) : (
        <>
          <Wifi className="h-4 w-4 text-blue-600 animate-spin" />
          <span>Syncing{pendingCount > 1 ? ` ${pendingCount} pending` : ''}...</span>
        </>
      )}
    </div>
  );
}
