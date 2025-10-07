import { queueMutation } from './storage';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import { getBaseUrl } from '@/lib/utils/baseUrl';

// Offline detection
export function isOnline(): boolean {
  return navigator.onLine;
}

// Main mutation wrapper for hold updates
export async function updateProjectHoldOffline(
  recordId: number,
  holdData: {
    onHold: boolean;
    holdReason: string;
    blockReason: string;
  }
): Promise<{ success: boolean; queued?: boolean }> {
  try {
    // Build updates object in Quickbase format
    const updates = {
      [PROJECT_FIELDS.ON_HOLD]: { value: holdData.onHold ? 'Yes' : 'No' },
      [PROJECT_FIELDS.HOLD_REASON]: { value: holdData.holdReason },
      [PROJECT_FIELDS.BLOCK_REASON]: { value: holdData.blockReason },
      [PROJECT_FIELDS.DATE_ON_HOLD]: { 
        value: holdData.onHold ? new Date().toISOString() : '' 
      },
    };

    if (isOnline()) {
      // Online: execute mutation via API
      const baseUrl = getBaseUrl();
      const res = await fetch(`${baseUrl}/api/projects/${recordId}/hold`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          onHold: holdData.onHold,
          holdReason: holdData.holdReason,
          blockReason: holdData.blockReason,
        }),
      });
      if (!res.ok) {
        throw new Error('Failed to update project hold');
      }
      return { success: true };
    } else {
      // Offline: queue mutation for later sync (collapse duplicates by projectId)
      await queueMutation('hold-update', recordId, updates, true); // true = collapse duplicates
      return { success: true, queued: true };
    }
  } catch (error) {
    console.error('Failed to update project hold (offline-aware):', error);
    const message = error instanceof Error ? error.message : String(error);
    throw new Error(`Failed to ${isOnline() ? 'update' : 'queue'} hold status: ${message}`);
  }
}

// Optimistic update helper
export function createOptimisticUpdate(
  recordId: number,
  holdData: {
    onHold: boolean;
    holdReason: string;
    blockReason: string;
  }
): any {
  // Return optimistic project update object that matches Quickbase format
  return {
    [3]: { value: recordId }, // recordId
    [PROJECT_FIELDS.ON_HOLD]: { value: holdData.onHold ? 'Yes' : 'No' },
    [PROJECT_FIELDS.HOLD_REASON]: { value: holdData.holdReason },
    [PROJECT_FIELDS.BLOCK_REASON]: { value: holdData.blockReason },
    [PROJECT_FIELDS.DATE_ON_HOLD]: { 
      value: holdData.onHold ? new Date().toISOString() : '' 
    },
  };
}

// Queue status
export async function getPendingHoldUpdates(): Promise<number> {
  try {
    const { getPendingMutations } = await import('./storage');
    const mutations = await getPendingMutations();
    return mutations.filter(m => m.type === 'hold-update').length;
  } catch (error) {
    console.error('Failed to get pending hold updates:', error);
    return 0;
  }
}

// Export main mutation function and helpers
export default {
  updateProjectHold: updateProjectHoldOffline,
  createOptimisticUpdate,
  getPendingHoldUpdates,
  isOnline,
};
