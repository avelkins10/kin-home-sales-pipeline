import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { queueMutation, getPendingMutations, clearMutation } from '@/lib/offline/storage';

// Mock IndexedDB
const mockDB = {
  add: vi.fn(),
  getAllFromIndex: vi.fn(),
  delete: vi.fn(),
  transaction: vi.fn(),
};

vi.mock('idb', () => ({
  openDB: vi.fn().mockResolvedValue(mockDB),
}));

describe('Sync Queue Deduping', () => {
  beforeEach(() => {
    vi.clearAllMocks();
    // Reset mock implementations
    mockDB.add.mockResolvedValue(1);
    mockDB.getAllFromIndex.mockResolvedValue([]);
    mockDB.delete.mockResolvedValue(undefined);
  });

  afterEach(() => {
    vi.resetAllMocks();
  });

  describe('queueMutation with collapseDuplicates', () => {
    it('should not collapse duplicates when collapseDuplicates is false', async () => {
      mockDB.getAllFromIndex.mockResolvedValue([]);

      await queueMutation('hold-update', 123, { onHold: true }, false);

      expect(mockDB.getAllFromIndex).not.toHaveBeenCalled();
      expect(mockDB.delete).not.toHaveBeenCalled();
      expect(mockDB.add).toHaveBeenCalledTimes(1);
    });

    it('should collapse duplicates when collapseDuplicates is true', async () => {
      const existingMutations = [
        { id: 1, type: 'hold-update', projectId: 123, data: { onHold: false }, timestamp: Date.now() - 1000, retryCount: 0 },
        { id: 2, type: 'hold-update', projectId: 123, data: { onHold: true }, timestamp: Date.now() - 500, retryCount: 0 },
        { id: 3, type: 'hold-update', projectId: 456, data: { onHold: true }, timestamp: Date.now() - 200, retryCount: 0 }, // Different project
      ];

      mockDB.getAllFromIndex.mockResolvedValue(existingMutations);

      await queueMutation('hold-update', 123, { onHold: true }, true);

      // Should delete existing mutations for project 123
      expect(mockDB.delete).toHaveBeenCalledTimes(2);
      expect(mockDB.delete).toHaveBeenCalledWith('pendingMutations', 1);
      expect(mockDB.delete).toHaveBeenCalledWith('pendingMutations', 2);
      expect(mockDB.delete).not.toHaveBeenCalledWith('pendingMutations', 3); // Different project

      // Should add new mutation
      expect(mockDB.add).toHaveBeenCalledTimes(1);
    });

    it('should handle no existing mutations gracefully', async () => {
      mockDB.getAllFromIndex.mockResolvedValue([]);

      await queueMutation('hold-update', 123, { onHold: true }, true);

      expect(mockDB.delete).not.toHaveBeenCalled();
      expect(mockDB.add).toHaveBeenCalledTimes(1);
    });

    it('should only collapse mutations of the same type and project', async () => {
      const existingMutations = [
        { id: 1, type: 'hold-update', projectId: 123, data: { onHold: false }, timestamp: Date.now() - 1000, retryCount: 0 },
        { id: 2, type: 'other-type', projectId: 123, data: { someData: true }, timestamp: Date.now() - 500, retryCount: 0 }, // Different type
        { id: 3, type: 'hold-update', projectId: 456, data: { onHold: true }, timestamp: Date.now() - 200, retryCount: 0 }, // Different project
      ];

      mockDB.getAllFromIndex.mockResolvedValue(existingMutations);

      await queueMutation('hold-update', 123, { onHold: true }, true);

      // Should only delete the matching type and project
      expect(mockDB.delete).toHaveBeenCalledTimes(1);
      expect(mockDB.delete).toHaveBeenCalledWith('pendingMutations', 1);
      expect(mockDB.add).toHaveBeenCalledTimes(1);
    });
  });

  describe('mutation data integrity', () => {
    it('should preserve mutation data when collapsing', async () => {
      const existingMutations = [
        { id: 1, type: 'hold-update', projectId: 123, data: { onHold: false, holdReason: 'Old reason' }, timestamp: Date.now() - 1000, retryCount: 0 },
      ];

      mockDB.getAllFromIndex.mockResolvedValue(existingMutations);

      const newData = { onHold: true, holdReason: 'New reason', blockReason: 'Customer issue' };
      await queueMutation('hold-update', 123, newData, true);

      // Verify the new mutation has the correct data
      expect(mockDB.add).toHaveBeenCalledWith('pendingMutations', expect.objectContaining({
        type: 'hold-update',
        projectId: 123,
        data: newData,
        retryCount: 0,
      }));
    });

    it('should generate unique timestamps for new mutations', async () => {
      const beforeTime = Date.now();
      
      await queueMutation('hold-update', 123, { onHold: true }, false);
      
      const afterTime = Date.now();
      
      expect(mockDB.add).toHaveBeenCalledWith('pendingMutations', expect.objectContaining({
        timestamp: expect.any(Number),
      }));

      const addedMutation = mockDB.add.mock.calls[0][1];
      expect(addedMutation.timestamp).toBeGreaterThanOrEqual(beforeTime);
      expect(addedMutation.timestamp).toBeLessThanOrEqual(afterTime);
    });
  });

  describe('error handling', () => {
    it('should handle database errors gracefully', async () => {
      mockDB.add.mockRejectedValue(new Error('Database error'));

      await expect(queueMutation('hold-update', 123, { onHold: true }, false))
        .rejects.toThrow('Database error');
    });

    it('should handle deletion errors during collapse', async () => {
      const existingMutations = [
        { id: 1, type: 'hold-update', projectId: 123, data: { onHold: false }, timestamp: Date.now() - 1000, retryCount: 0 },
      ];

      mockDB.getAllFromIndex.mockResolvedValue(existingMutations);
      mockDB.delete.mockRejectedValue(new Error('Delete error'));

      await expect(queueMutation('hold-update', 123, { onHold: true }, true))
        .rejects.toThrow('Delete error');
    });
  });

  describe('integration with sync process', () => {
    it('should work with getPendingMutations', async () => {
      // Mock getPendingMutations to return test data
      const mockMutations = [
        { id: 1, type: 'hold-update', projectId: 123, data: { onHold: true }, timestamp: Date.now(), retryCount: 0 },
        { id: 2, type: 'hold-update', projectId: 456, data: { onHold: false }, timestamp: Date.now(), retryCount: 0 },
      ];

      mockDB.getAllFromIndex.mockResolvedValue(mockMutations);

      // This would be called by the sync process
      const pendingMutations = await getPendingMutations();
      
      expect(pendingMutations).toHaveLength(2);
      expect(pendingMutations[0].projectId).toBe(123);
      expect(pendingMutations[1].projectId).toBe(456);
    });

    it('should work with clearMutation', async () => {
      mockDB.delete.mockResolvedValue(undefined);

      await clearMutation(123);

      expect(mockDB.delete).toHaveBeenCalledWith('pendingMutations', 123);
    });
  });
});