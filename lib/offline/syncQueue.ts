import { getPendingMutations, clearMutation, updateMutationRetryCount } from './storage';
import { toast } from 'sonner';
import { logSyncEvent, logError, logWarn, logInfo } from '@/lib/logging/logger';

/**
 * Resolve base URL for fetch calls.
 * Precedence (tests/server): TEST_BASE_URL || NEXT_PUBLIC_APP_URL || NEXTAUTH_URL || 'http://localhost:3000'
 * Browser (non-test): allow '' for relative URLs only if window.location.origin matches TEST_BASE_URL when present; otherwise use window.location.origin.
 */
function resolveBaseUrl(): string {
  const inBrowser = typeof window !== 'undefined';
  const inTest = process.env.NODE_ENV === 'test' || (process as any).env.VITEST || (process as any).env.JEST;

  // In tests or on the server, always return an absolute base URL using precedence
  if (!inBrowser || inTest) {
    const base =
      process.env.TEST_BASE_URL ||
      process.env.NEXT_PUBLIC_APP_URL ||
      process.env.NEXTAUTH_URL ||
      'http://localhost:3000';
    return base.replace(/\/$/, '');
  }

  // In the browser (non-test): prefer relative URLs for same-origin calls
  // If TEST_BASE_URL is defined and differs from current origin, use absolute origin to avoid ambiguity
  const origin = window.location?.origin || '';
  const testBase = process.env.TEST_BASE_URL;
  if (testBase && origin && origin !== testBase) {
    return origin.replace(/\/$/, '');
  }
  // Otherwise allow relative URLs
  return '';
}

let isSyncing = false;
const MAX_RETRIES = 3;

export async function syncPendingMutations(): Promise<{ synced: number; failed: number }> {
  // Check if already syncing or offline
  if (isSyncing || !navigator.onLine) {
    return { synced: 0, failed: 0 };
  }

  isSyncing = true;
  let synced = 0;
  let failed = 0;

  try {
    const mutations = await getPendingMutations();
    logSyncEvent('start', { pendingCount: mutations.length });
    const baseUrl = resolveBaseUrl();
    let errorToastShown = false;
    
    for (const mutation of mutations) {
      try {
        // Skip mutations that have exceeded max retries
        if (mutation.retryCount >= MAX_RETRIES) {
          logError('Mutation exceeded max retries', new Error('Max retries exceeded'), { 
            mutationId: mutation.id, 
            projectId: mutation.projectId, 
            retryCount: mutation.retryCount 
          });
          await clearMutation(mutation.id);
          failed++;
          continue;
        }

        // Execute mutation based on type
        if (mutation.type === 'hold-update') {
          const { onHold, holdReason, blockReason } = mutation.data as any;
          const res = await fetch(`${baseUrl}/api/projects/${mutation.projectId}/hold`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ onHold, holdReason, blockReason }),
          });
          if (!res.ok) {
            throw new Error('Failed to sync hold update');
          }
        } else {
          logWarn('Unknown mutation type', { 
            mutationType: mutation.type, 
            mutationId: mutation.id 
          });
          await clearMutation(mutation.id);
          failed++;
          continue;
        }

        // Success - clear mutation from queue
        await clearMutation(mutation.id);
        synced++;
        logInfo('Mutation synced successfully', {
          mutationId: mutation.id,
          projectId: mutation.projectId
        });

        // Show success toast for individual mutations (only if few total)
        if (mutations.length <= 3 && synced <= 3) {
          toast.success('Synced');
        }
        
      } catch (error) {
        logError('Failed to sync mutation', error as Error, { 
          mutationId: mutation.id, 
          projectId: mutation.projectId, 
          retryCount: mutation.retryCount 
        });
        
        // Increment retry count
        const newRetryCount = mutation.retryCount + 1;
        await updateMutationRetryCount(mutation.id, newRetryCount);
        failed++;
        
        // Show error toast at most once per run
        if (!errorToastShown) {
          toast.error('Some offline changes failed');
          errorToastShown = true;
        }
      }
    }

    // Show summary toast only for larger batches (>3)
    if (mutations.length > 3) {
      if (synced > 0) {
        toast.success('Synced');
      }
      if (failed > 0 && !errorToastShown) {
        toast.error('Some offline changes failed');
      }
    }

    logSyncEvent('success', { synced, failed });

  } catch (error) {
    logError('Failed to sync pending mutations', error as Error, {});
    logSyncEvent('failure', { error: error as Error });
    toast.error('Failed to sync offline changes');
  } finally {
    isSyncing = false;
  }

  return { synced, failed };
}

// Auto-sync on reconnect
export const handleOnline = async () => {
  logInfo('Back online - syncing queued mutations');
  toast.info('Syncing');
  await syncPendingMutations();
};

if (typeof window !== 'undefined') {
  window.addEventListener('online', handleOnline);
}

export function triggerSync(): Promise<{ synced: number; failed: number }> {
  if (!navigator.onLine) {
    toast.warning('You are offline. Changes will sync when reconnected.');
    return Promise.resolve({ synced: 0, failed: 0 });
  }
  
  return syncPendingMutations();
}

export function removeSyncListener(): void {
  if (typeof window !== 'undefined') {
    window.removeEventListener('online', handleOnline);
  }
}

// Export for testing
export { isSyncing };
