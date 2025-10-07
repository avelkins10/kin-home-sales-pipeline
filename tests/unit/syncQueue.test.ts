import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { syncPendingMutations, triggerSync, handleOnline } from '@/lib/offline/syncQueue';

// Mock dependencies
vi.mock('@/lib/offline/storage', () => ({
  getPendingMutations: vi.fn(),
  clearMutation: vi.fn(),
  updateMutationRetryCount: vi.fn(),
}));

vi.mock('@/lib/quickbase/queries', () => ({
  updateProject: vi.fn(),
}));

vi.mock('sonner', () => ({
  toast: {
    success: vi.fn(),
    error: vi.fn(),
    warning: vi.fn(),
    info: vi.fn(),
  },
}));

import { getPendingMutations, clearMutation, updateMutationRetryCount } from '@/lib/offline/storage';
import { updateProject } from '@/lib/quickbase/queries';
import { toast } from 'sonner';

// Mock navigator.onLine
Object.defineProperty(navigator, 'onLine', {
  writable: true,
  value: true,
});

describe('Sync Queue', () => {
  beforeEach(() => {
    vi.clearAllMocks();
    navigator.onLine = true;
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  describe('syncPendingMutations', () => {
    it('returns early if offline', async () => {
      navigator.onLine = false;

      const result = await syncPendingMutations();

      expect(result).toEqual({ synced: 0, failed: 0 });
      expect(getPendingMutations).not.toHaveBeenCalled();
    });

    it('returns early if already syncing', async () => {
      // Start first sync
      const firstSyncPromise = syncPendingMutations();
      
      // Try second sync while first is running
      const secondResult = await syncPendingMutations();
      
      expect(secondResult).toEqual({ synced: 0, failed: 0 });
      
      // Clean up first sync
      await firstSyncPromise;
    });

    it('syncs single mutation successfully', async () => {
      const mockMutation = {
        id: 'mutation-1',
        type: 'hold-update',
        projectId: '123',
        data: { holdStatus: 'On Hold' },
        retryCount: 0,
        timestamp: Date.now(),
      };

      vi.mocked(getPendingMutations).mockResolvedValue([mockMutation]);
      vi.mocked(updateProject).mockResolvedValue({ success: true });
      vi.mocked(clearMutation).mockResolvedValue();

      const result = await syncPendingMutations();

      expect(updateProject).toHaveBeenCalledWith('123', { holdStatus: 'On Hold' });
      expect(clearMutation).toHaveBeenCalledWith('mutation-1');
      expect(result).toEqual({ synced: 1, failed: 0 });
      expect(toast.success).toHaveBeenCalledWith('Synced');
    });

    it('syncs multiple mutations in order', async () => {
      const mockMutations = [
        {
          id: 'mutation-1',
          type: 'hold-update',
          projectId: '123',
          data: { holdStatus: 'On Hold' },
          retryCount: 0,
          timestamp: Date.now() - 2000,
        },
        {
          id: 'mutation-2',
          type: 'hold-update',
          projectId: '456',
          data: { holdStatus: 'Active' },
          retryCount: 0,
          timestamp: Date.now() - 1000,
        },
        {
          id: 'mutation-3',
          type: 'hold-update',
          projectId: '789',
          data: { holdStatus: 'On Hold' },
          retryCount: 0,
          timestamp: Date.now(),
        },
      ];

      vi.mocked(getPendingMutations).mockResolvedValue(mockMutations);
      vi.mocked(updateProject).mockResolvedValue({ success: true });
      vi.mocked(clearMutation).mockResolvedValue();

      const result = await syncPendingMutations();

      expect(updateProject).toHaveBeenCalledTimes(3);
      expect(updateProject).toHaveBeenNthCalledWith(1, '123', { holdStatus: 'On Hold' });
      expect(updateProject).toHaveBeenNthCalledWith(2, '456', { holdStatus: 'Active' });
      expect(updateProject).toHaveBeenNthCalledWith(3, '789', { holdStatus: 'On Hold' });
      expect(clearMutation).toHaveBeenCalledTimes(3);
      expect(result).toEqual({ synced: 3, failed: 0 });
    });

    it('increments retry count on failure', async () => {
      const mockMutation = {
        id: 'mutation-1',
        type: 'hold-update',
        projectId: '123',
        data: { holdStatus: 'On Hold' },
        retryCount: 0,
        timestamp: Date.now(),
      };

      vi.mocked(getPendingMutations).mockResolvedValue([mockMutation]);
      vi.mocked(updateProject).mockRejectedValue(new Error('API Error'));
      vi.mocked(updateMutationRetryCount).mockResolvedValue();

      const result = await syncPendingMutations();

      expect(updateProject).toHaveBeenCalledWith('123', { holdStatus: 'On Hold' });
      expect(updateMutationRetryCount).toHaveBeenCalledWith('mutation-1', 1);
      expect(clearMutation).not.toHaveBeenCalled();
      expect(result).toEqual({ synced: 0, failed: 1 });
      expect(toast.error).toHaveBeenCalledWith('Some offline changes failed');
    });

    it('clears mutation after max retries exceeded', async () => {
      const mockMutation = {
        id: 'mutation-1',
        type: 'hold-update',
        projectId: '123',
        data: { holdStatus: 'On Hold' },
        retryCount: 3, // MAX_RETRIES
        timestamp: Date.now(),
      };

      vi.mocked(getPendingMutations).mockResolvedValue([mockMutation]);
      vi.mocked(clearMutation).mockResolvedValue();

      const result = await syncPendingMutations();

      expect(updateProject).not.toHaveBeenCalled();
      expect(clearMutation).toHaveBeenCalledWith('mutation-1');
      expect(result).toEqual({ synced: 0, failed: 1 });
    });

    it('handles unknown mutation type', async () => {
      const mockMutation = {
        id: 'mutation-1',
        type: 'unknown-type',
        projectId: '123',
        data: { someField: 'value' },
        retryCount: 0,
        timestamp: Date.now(),
      };

      vi.mocked(getPendingMutations).mockResolvedValue([mockMutation]);
      vi.mocked(clearMutation).mockResolvedValue();

      const result = await syncPendingMutations();

      expect(updateProject).not.toHaveBeenCalled();
      expect(clearMutation).toHaveBeenCalledWith('mutation-1');
      expect(result).toEqual({ synced: 0, failed: 1 });
    });

    it('shows summary toast for multiple mutations', async () => {
      const mockMutations = Array.from({ length: 5 }, (_, i) => ({
        id: `mutation-${i}`,
        type: 'hold-update',
        projectId: `${i}`,
        data: { holdStatus: 'On Hold' },
        retryCount: 0,
        timestamp: Date.now(),
      }));

      vi.mocked(getPendingMutations).mockResolvedValue(mockMutations);
      vi.mocked(updateProject).mockResolvedValue({ success: true });
      vi.mocked(clearMutation).mockResolvedValue();

      const result = await syncPendingMutations();

      expect(result).toEqual({ synced: 5, failed: 0 });
      // Should show summary toast, not individual toasts
      expect(toast.success).toHaveBeenCalledWith('Synced');
      expect(toast.success).toHaveBeenCalledTimes(1);
    });

    it('shows error toast for failed mutations in batch', async () => {
      const mockMutations = [
        {
          id: 'mutation-1',
          type: 'hold-update',
          projectId: '123',
          data: { holdStatus: 'On Hold' },
          retryCount: 0,
          timestamp: Date.now(),
        },
        {
          id: 'mutation-2',
          type: 'hold-update',
          projectId: '456',
          data: { holdStatus: 'Active' },
          retryCount: 0,
          timestamp: Date.now(),
        },
      ];

      vi.mocked(getPendingMutations).mockResolvedValue(mockMutations);
      vi.mocked(updateProject)
        .mockResolvedValueOnce({ success: true })
        .mockRejectedValueOnce(new Error('API Error'));
      vi.mocked(clearMutation).mockResolvedValue();
      vi.mocked(updateMutationRetryCount).mockResolvedValue();

      const result = await syncPendingMutations();

      expect(result).toEqual({ synced: 1, failed: 1 });
      expect(toast.success).toHaveBeenCalledWith('Synced');
      expect(toast.error).toHaveBeenCalledWith('Some offline changes failed');
    });
  });

  describe('triggerSync', () => {
    it('calls syncPendingMutations when online', async () => {
      navigator.onLine = true;
      vi.mocked(getPendingMutations).mockResolvedValue([]);

      const result = await triggerSync();

      expect(result).toEqual({ synced: 0, failed: 0 });
    });

    it('shows warning toast when offline', async () => {
      navigator.onLine = false;

      const result = await triggerSync();

      expect(result).toEqual({ synced: 0, failed: 0 });
      expect(toast.warning).toHaveBeenCalledWith(
        'You are offline. Changes will sync when reconnected.'
      );
    });
  });

  describe('online event listener', () => {
    it('syncs when online event fires', async () => {
      // Mock the sync function
      vi.mocked(getPendingMutations).mockResolvedValue([]);

      // Call the exported handler directly
      await handleOnline();

      expect(toast.info).toHaveBeenCalledWith('Syncing');
      expect(getPendingMutations).toHaveBeenCalled();
    });
  });
});
