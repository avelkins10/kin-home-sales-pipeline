import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';

// Mock the idb library BEFORE importing storage
vi.mock('idb', () => ({
  openDB: vi.fn(),
}));

import {
  initDB,
  resetDB,
  cacheProject,
  getCachedProject,
  cacheProjectList,
  getCachedProjectList,
  queueMutation,
  getPendingMutations,
  clearMutation,
  updateMutationRetryCount,
  clearAllMutations,
} from '@/lib/offline/storage';
import { openDB } from 'idb';

describe('IndexedDB Storage', () => {
  let mockDB: any;
  let mockTransaction: any;
  let mockStore: any;
  let mockIndex: any;

  beforeEach(() => {
    // Reset the DB singleton
    resetDB();

    // Reset mocks
    vi.clearAllMocks();

    // Create mock index
    mockIndex = {
      getAll: vi.fn(),
    };

    // Create mock object store
    mockStore = {
      put: vi.fn(),
      get: vi.fn(),
      add: vi.fn(),
      delete: vi.fn(),
      clear: vi.fn(),
      getAll: vi.fn(),
      keys: vi.fn(),
      index: vi.fn().mockReturnValue(mockIndex),
    };

    // Create mock transaction
    mockTransaction = {
      objectStore: vi.fn().mockReturnValue(mockStore),
      done: Promise.resolve(),
    };

    // Create mock database - idb provides convenience methods on the db object itself
    mockDB = {
      put: vi.fn().mockResolvedValue(undefined),
      get: vi.fn().mockResolvedValue(null),
      add: vi.fn().mockResolvedValue(1),
      delete: vi.fn().mockResolvedValue(undefined),
      clear: vi.fn().mockResolvedValue(undefined),
      transaction: vi.fn().mockReturnValue(mockTransaction),
    };

    // Mock openDB to return our mock database
    vi.mocked(openDB).mockResolvedValue(mockDB);
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  describe('initDB', () => {
    it('creates database with correct schema', async () => {
      await initDB();

      expect(openDB).toHaveBeenCalledWith('kin-solar-db', 1, expect.any(Object));
    });

    it('returns singleton instance', async () => {
      const db1 = await initDB();
      const db2 = await initDB();
      
      expect(db1).toBe(db2);
    });
  });

  describe('cacheProject', () => {
    it('stores project with timestamp', async () => {
      const mockProject = createMockProject(123);
      
      await cacheProject(mockProject);
      
      expect(mockDB.put).toHaveBeenCalledWith('projects', {
        id: 123,
        data: mockProject,
        cachedAt: expect.any(Number),
      });
    });

    it('overwrites existing project', async () => {
      const mockProject1 = createMockProject(123);
      const mockProject2 = createMockProject(123);
      
      await cacheProject(mockProject1);
      await cacheProject(mockProject2);
      
      expect(mockDB.put).toHaveBeenCalledTimes(2);
      expect(mockDB.put).toHaveBeenLastCalledWith('projects', {
        id: 123,
        data: mockProject2,
        cachedAt: expect.any(Number),
      });
    });

    it('handles missing recordId gracefully', async () => {
      const mockProject = { [PROJECT_FIELDS.ON_HOLD]: { value: 'Yes' } }; // No recordId
      
      await cacheProject(mockProject);
      
      expect(mockDB.put).not.toHaveBeenCalled();
    });
  });

  describe('getCachedProject', () => {
    it('returns cached project within TTL', async () => {
      const mockProject = createMockProject(123);
      const cachedData = {
        id: 123,
        data: mockProject,
        cachedAt: Date.now() - 100000, // 100 seconds ago (within 5 minutes)
      };
      
      mockDB.get.mockResolvedValue(cachedData);
      
      const result = await getCachedProject(123);
      
      expect(result).toEqual(mockProject);
      expect(mockDB.get).toHaveBeenCalledWith('projects', 123);
    });

    it('returns null for stale cache', async () => {
      const mockProject = createMockProject(123);
      const cachedData = {
        id: 123,
        data: mockProject,
        cachedAt: Date.now() - 400000, // 400 seconds ago (stale)
      };
      
      mockDB.get.mockResolvedValue(cachedData);
      mockDB.delete.mockResolvedValue(undefined);
      
      const result = await getCachedProject(123);
      
      expect(result).toBeNull();
      expect(mockDB.delete).toHaveBeenCalledWith('projects', 123);
    });

    it('returns null for non-existent project', async () => {
      mockDB.get.mockResolvedValue(undefined);
      
      const result = await getCachedProject(999);
      
      expect(result).toBeNull();
    });
  });

  describe('cacheProjectList', () => {
    it('stores project list with metadata', async () => {
      const mockProjects = [createMockProject(123), createMockProject(456)];
      
      await cacheProjectList(mockProjects, 'user1', 'closer');
      
      // Should cache individual projects
      expect(mockDB.put).toHaveBeenCalledTimes(3); // 2 projects + 1 list metadata
      
      // Should store list metadata
      expect(mockDB.put).toHaveBeenCalledWith('projects', {
        id: -1,
        data: {
          listKey: 'user1-closer',
          projects: [123, 456],
        },
        cachedAt: expect.any(Number),
      });
    });
  });

  describe('getCachedProjectList', () => {
    it('returns cached project list within TTL', async () => {
      const listCache = {
        id: -1,
        data: {
          listKey: 'user1-closer',
          projects: [123, 456],
        },
        cachedAt: Date.now() - 100000, // 100 seconds ago
      };
      
      const mockProject1 = createMockProject(123);
      const mockProject2 = createMockProject(456);
      
      mockDB.get
        .mockResolvedValueOnce(listCache) // List metadata
        .mockResolvedValueOnce({ id: 123, data: mockProject1, cachedAt: Date.now() }) // Project 1
        .mockResolvedValueOnce({ id: 456, data: mockProject2, cachedAt: Date.now() }); // Project 2
      
      const result = await getCachedProjectList('user1', 'closer');
      
      expect(result).toEqual([mockProject1, mockProject2]);
    });

    it('returns empty array for stale cache', async () => {
      const listCache = {
        id: -1,
        data: {
          listKey: 'user1-closer',
          projects: [123],
        },
        cachedAt: Date.now() - 400000, // Stale
      };
      
      mockDB.get.mockResolvedValue(listCache);
      mockDB.delete.mockResolvedValue(undefined);
      
      const result = await getCachedProjectList('user1', 'closer');
      
      expect(result).toEqual([]);
      expect(mockDB.delete).toHaveBeenCalledWith('projects', -1);
    });
  });

  describe('queueMutation', () => {
    it('adds mutation to queue', async () => {
      const mutationData = { onHold: true, holdReason: 'Test' };
      mockDB.add.mockResolvedValue(1);
      
      const id = await queueMutation('hold-update', 123, mutationData);
      
      expect(mockDB.add).toHaveBeenCalledWith('pendingMutations', {
        id: 0,
        type: 'hold-update',
        projectId: 123,
        data: mutationData,
        timestamp: expect.any(Number),
        retryCount: 0,
      });
      expect(id).toBe(1);
    });
  });

  describe('getPendingMutations', () => {
    it('returns mutations ordered by timestamp', async () => {
      const mutations = [
        { id: 1, timestamp: 1000, type: 'hold-update', projectId: 123, data: {}, retryCount: 0 },
        { id: 2, timestamp: 2000, type: 'hold-update', projectId: 456, data: {}, retryCount: 0 },
      ];
      
      mockTransaction.objectStore.mockReturnValue(mockStore);
      mockStore.index.mockReturnValue(mockIndex);
      mockIndex.getAll.mockResolvedValue(mutations);
      
      const result = await getPendingMutations();
      
      expect(result).toEqual(mutations);
      expect(mockStore.index).toHaveBeenCalledWith('by-timestamp');
    });

    it('returns empty array when no mutations', async () => {
      mockTransaction.objectStore.mockReturnValue(mockStore);
      mockStore.index.mockReturnValue(mockIndex);
      mockIndex.getAll.mockResolvedValue([]);
      
      const result = await getPendingMutations();
      
      expect(result).toEqual([]);
    });
  });

  describe('clearMutation', () => {
    it('removes mutation from queue', async () => {
      mockDB.delete.mockResolvedValue(undefined);
      
      await clearMutation(1);
      
      expect(mockDB.delete).toHaveBeenCalledWith('pendingMutations', 1);
    });
  });

  describe('updateMutationRetryCount', () => {
    it('increments retry count', async () => {
      const mutation = {
        id: 1,
        type: 'hold-update',
        projectId: 123,
        data: {},
        timestamp: Date.now(),
        retryCount: 0,
      };
      
      mockDB.get.mockResolvedValue(mutation);
      mockDB.put.mockResolvedValue(undefined);
      
      await updateMutationRetryCount(1, 1);
      
      expect(mockDB.get).toHaveBeenCalledWith('pendingMutations', 1);
      expect(mockDB.put).toHaveBeenCalledWith('pendingMutations', {
        ...mutation,
        retryCount: 1,
      });
    });
  });

  describe('clearAllMutations', () => {
    it('clears all mutations', async () => {
      mockDB.clear.mockResolvedValue(undefined);
      
      await clearAllMutations();
      
      expect(mockDB.clear).toHaveBeenCalledWith('pendingMutations');
    });
  });

  // Helper functions
  function createMockProject(recordId: number) {
    return {
      [3]: { value: recordId },
      [PROJECT_FIELDS.ON_HOLD]: { value: 'No' },
      [PROJECT_FIELDS.HOLD_REASON]: { value: '' },
    };
  }

  function createMockMutation(type: string, projectId: number, data: any) {
    return {
      id: 1,
      type,
      projectId,
      data,
      timestamp: Date.now(),
      retryCount: 0,
    };
  }
});
