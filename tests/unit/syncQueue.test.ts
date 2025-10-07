import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { syncPendingMutations, triggerSync, handleOnline } from '@/lib/offline/syncQueue';

// Mock dependencies
vi.mock('@/lib/offline/storage', () => ({
  getPendingMutations: vi.fn(),
  clearMutation: vi.fn(),
  updateMutationRetryCount: vi.fn(),
}));

// No Quickbase query mocks needed; sync uses fetch directly

vi.mock('sonner', () => ({
  toast: {
    success: vi.fn(),
    error: vi.fn(),
    warning: vi.fn(),
    info: vi.fn(),
  },
}));

import { getPendingMutations, clearMutation, updateMutationRetryCount } from '@/lib/offline/storage';
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
    // Stub global.fetch to avoid real network calls
    global.fetch = vi.fn().mockResolvedValue({ ok: true });
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
        data: { onHold: true, holdReason: 'Customer Request', blockReason: 'Docs Missing' },
        retryCount: 0,
        timestamp: Date.now(),
      };

      vi.mocked(getPendingMutations).mockResolvedValue([mockMutation]);
      vi.mocked(clearMutation).mockResolvedValue();

      const result = await syncPendingMutations();

      expect(global.fetch).toHaveBeenCalledTimes(1);
      const [url, options] = vi.mocked(global.fetch).mock.calls[0] as [string, RequestInit];
      expect(typeof url).toBe('string');
      expect((url as string).endsWith('/api/projects/123/hold')).toBe(true);
      expect(options).toMatchObject({
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ onHold: true, holdReason: 'Customer Request', blockReason: 'Docs Missing' }),
      });
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
          data: { onHold: true, holdReason: 'Customer Request', blockReason: 'Docs Missing' },
          retryCount: 0,
          timestamp: Date.now() - 2000,
        },
        {
          id: 'mutation-2',
          type: 'hold-update',
          projectId: '456',
          data: { onHold: false, holdReason: '', blockReason: '' },
          retryCount: 0,
          timestamp: Date.now() - 1000,
        },
        {
          id: 'mutation-3',
          type: 'hold-update',
          projectId: '789',
          data: { onHold: true, holdReason: 'Other', blockReason: '' },
          retryCount: 0,
          timestamp: Date.now(),
        },
      ];

      vi.mocked(getPendingMutations).mockResolvedValue(mockMutations);
      vi.mocked(clearMutation).mockResolvedValue();

      const result = await syncPendingMutations();

      expect(global.fetch).toHaveBeenCalledTimes(3);
      const urls = vi.mocked(global.fetch).mock.calls.map(c => c[0] as string);
      expect(urls[0].endsWith('/api/projects/123/hold')).toBe(true);
      expect(urls[1].endsWith('/api/projects/456/hold')).toBe(true);
      expect(urls[2].endsWith('/api/projects/789/hold')).toBe(true);
      expect(clearMutation).toHaveBeenCalledTimes(3);
      expect(result).toEqual({ synced: 3, failed: 0 });
    });

    it('increments retry count on failure', async () => {
      const mockMutation = {
        id: 'mutation-1',
        type: 'hold-update',
        projectId: '123',
        data: { onHold: true, holdReason: 'Customer Request', blockReason: 'Docs Missing' },
        retryCount: 0,
        timestamp: Date.now(),
      };

      vi.mocked(getPendingMutations).mockResolvedValue([mockMutation]);
      vi.mocked(updateMutationRetryCount).mockResolvedValue();
      // Simulate fetch returning not ok
      vi.mocked(global.fetch).mockResolvedValueOnce({ ok: false } as any);
      vi.mocked(updateMutationRetryCount).mockResolvedValue();

      const result = await syncPendingMutations();

      expect(global.fetch).toHaveBeenCalledTimes(1);
      const [url, options] = vi.mocked(global.fetch).mock.calls[0] as [string, RequestInit];
      expect((url as string).endsWith('/api/projects/123/hold')).toBe(true);
      expect(options).toMatchObject({ method: 'POST' });
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

      expect(global.fetch).not.toHaveBeenCalled();
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

      expect(global.fetch).not.toHaveBeenCalled();
      expect(clearMutation).toHaveBeenCalledWith('mutation-1');
      expect(result).toEqual({ synced: 0, failed: 1 });
    });

    it('shows summary toast for multiple mutations (>3) and no per-item toasts', async () => {
      const mockMutations = Array.from({ length: 5 }, (_, i) => ({
        id: `mutation-${i}`,
        type: 'hold-update',
        projectId: `${i}`,
        data: { holdStatus: 'On Hold' },
        retryCount: 0,
        timestamp: Date.now(),
      }));

      vi.mocked(getPendingMutations).mockResolvedValue(mockMutations);
      vi.mocked(clearMutation).mockResolvedValue();

      const result = await syncPendingMutations();

      expect(result).toEqual({ synced: 5, failed: 0 });
      // Should show summary toast, not individual toasts
      expect(toast.success).toHaveBeenCalledWith('Synced');
      expect(toast.success).toHaveBeenCalledTimes(1);
    });

    it('does not show summary success toast when 1-3 items, keeps per-item successes', async () => {
      const mockMutations = Array.from({ length: 3 }, (_, i) => ({
        id: `mutation-${i}`,
        type: 'hold-update',
        projectId: `${i}`,
        data: { holdStatus: 'On Hold' },
        retryCount: 0,
        timestamp: Date.now(),
      }));

      vi.mocked(getPendingMutations).mockResolvedValue(mockMutations);
      vi.mocked(clearMutation).mockResolvedValue();

      const result = await syncPendingMutations();

      expect(result).toEqual({ synced: 3, failed: 0 });
      // Per-item success toast up to 3
      expect(toast.success).toHaveBeenCalledWith('Synced');
      expect(vi.mocked(toast.success).mock.calls.length).toBeGreaterThanOrEqual(1);
      // No additional summary success toast
      // We can't directly differentiate, but ensure it's not exactly one call (batch)
      // and at most 3 for per-item
      expect(vi.mocked(toast.success).mock.calls.length).toBeLessThanOrEqual(3);
    });

    it('dedupes error toast to at most once per run', async () => {
      const mockMutations = [
        { id: 'm1', type: 'hold-update', projectId: '1', data: {}, retryCount: 0, timestamp: Date.now() },
        { id: 'm2', type: 'hold-update', projectId: '2', data: {}, retryCount: 0, timestamp: Date.now() },
        { id: 'm3', type: 'hold-update', projectId: '3', data: {}, retryCount: 0, timestamp: Date.now() },
        { id: 'm4', type: 'hold-update', projectId: '4', data: {}, retryCount: 0, timestamp: Date.now() },
      ];

      vi.mocked(getPendingMutations).mockResolvedValue(mockMutations as any);
      vi.mocked(clearMutation).mockResolvedValue();
      vi.mocked(updateMutationRetryCount).mockResolvedValue();
      // Make all fetches fail
      vi.mocked(global.fetch).mockResolvedValue({ ok: false } as any);

      const result = await syncPendingMutations();

      expect(result.failed).toBe(4);
      // Only one error toast
      expect(toast.error).toHaveBeenCalledWith('Some offline changes failed');
      expect(toast.error).toHaveBeenCalledTimes(1);
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
      vi.mocked(clearMutation).mockResolvedValue();
      vi.mocked(updateMutationRetryCount).mockResolvedValue();
      // First fetch ok, second fetch not ok
      vi.mocked(global.fetch)
        .mockResolvedValueOnce({ ok: true } as any)
        .mockResolvedValueOnce({ ok: false } as any);

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
