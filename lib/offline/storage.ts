import { openDB, DBSchema, IDBPDatabase } from 'idb';
import { QuickbaseProject } from '@/lib/types/project';

interface ProjectDB extends DBSchema {
  projects: {
    key: number;
    value: {
      id: number;
      data: QuickbaseProject;
      cachedAt: number;
    };
  };
  pendingMutations: {
    key: number;
    value: {
      id: number;
      type: 'hold-update';
      projectId: number;
      data: any;
      timestamp: number;
      retryCount: number;
    };
    indexes: {
      'by-timestamp': number;
    };
  };
}

let db: IDBPDatabase<ProjectDB> | null = null;

// For testing only - resets the cached DB instance
export function resetDB(): void {
  db = null;
}

export async function initDB(): Promise<IDBPDatabase<ProjectDB>> {
  if (db) {
    return db;
  }

  try {
    db = await openDB<ProjectDB>('kin-solar-db', 1, {
      upgrade(db) {
        // Create projects object store
        if (!db.objectStoreNames.contains('projects')) {
          const projectStore = db.createObjectStore('projects', { keyPath: 'id' });
        }

        // Create pendingMutations object store
        if (!db.objectStoreNames.contains('pendingMutations')) {
          const mutationStore = db.createObjectStore('pendingMutations', { 
            keyPath: 'id', 
            autoIncrement: true 
          });
          mutationStore.createIndex('by-timestamp', 'timestamp');
        }
      },
    });

    return db;
  } catch (error) {
    console.error('Failed to initialize IndexedDB:', error);
    throw error;
  }
}

export async function cacheProject(project: QuickbaseProject): Promise<void> {
  try {
    const database = await initDB();
    const recordId = project[3]?.value;
    
    if (!recordId) {
      console.warn('Project missing recordId, cannot cache');
      return;
    }

    await database.put('projects', {
      id: recordId,
      data: project,
      cachedAt: Date.now(),
    });
  } catch (error) {
    console.error('Failed to cache project:', error);
  }
}

export async function getCachedProject(id: number): Promise<QuickbaseProject | null> {
  try {
    const database = await initDB();
    const cached = await database.get('projects', id);
    
    if (!cached) {
      return null;
    }

    // Check if cache is within 5 minutes (300000ms)
    const isStale = Date.now() - cached.cachedAt > 300000;
    if (isStale) {
      await database.delete('projects', id);
      return null;
    }

    return cached.data;
  } catch (error) {
    console.error('Failed to get cached project:', error);
    return null;
  }
}

export async function cacheProjectList(
  projects: QuickbaseProject[], 
  userId: string, 
  role: string
): Promise<void> {
  try {
    const database = await initDB();
    const cacheKey = `${userId}-${role}`;
    
    // Store each project individually
    for (const project of projects) {
      await cacheProject(project);
    }

    // Store list metadata
    await database.put('projects', {
      id: -1, // Special ID for list cache
      data: { listKey: cacheKey, projects: projects.map(p => p[3]?.value).filter(Boolean) } as any,
      cachedAt: Date.now(),
    });
  } catch (error) {
    console.error('Failed to cache project list:', error);
  }
}

export async function getCachedProjectList(
  userId: string, 
  role: string
): Promise<QuickbaseProject[]> {
  try {
    const database = await initDB();
    const cacheKey = `${userId}-${role}`;
    
    // Get list metadata
    const listCache = await database.get('projects', -1);
    if (!listCache || !(listCache.data as any).listKey || (listCache.data as any).listKey !== cacheKey) {
      return [];
    }

    // Check if cache is within 5 minutes
    const isStale = Date.now() - listCache.cachedAt > 300000;
    if (isStale) {
      await database.delete('projects', -1);
      return [];
    }

    // Get individual projects
    const projects: QuickbaseProject[] = [];
    for (const projectId of (listCache.data as any).projects) {
      const project = await getCachedProject(projectId);
      if (project) {
        projects.push(project);
      }
    }

    return projects;
  } catch (error) {
    console.error('Failed to get cached project list:', error);
    return [];
  }
}

export async function clearStaleCache(): Promise<void> {
  try {
    const database = await initDB();
    const tx = database.transaction('projects', 'readwrite');
    const store = tx.objectStore('projects');
    const allCached = await store.getAll();
    
    const fiveMinutesAgo = Date.now() - 300000;
    for (const cached of allCached) {
      if (cached.cachedAt < fiveMinutesAgo) {
        await store.delete(cached.id);
      }
    }
    
    await tx.done;
  } catch (error) {
    console.error('Failed to clear stale cache:', error);
  }
}

export async function queueMutation(
  type: 'hold-update',
  projectId: number,
  data: any,
  collapseDuplicates: boolean = false
): Promise<number> {
  try {
    const database = await initDB();
    
    // If collapsing duplicates, remove existing mutations for this project
    if (collapseDuplicates) {
      const existingMutations = await database.getAllFromIndex('pendingMutations', 'by-timestamp');
      const duplicates = existingMutations.filter(m => m.type === type && m.projectId === projectId);
      
      for (const duplicate of duplicates) {
        await database.delete('pendingMutations', duplicate.id);
      }
    }
    
    const mutation = {
      id: 0, // Will be auto-incremented
      type,
      projectId,
      data,
      timestamp: Date.now(),
      retryCount: 0,
    };

    const id = await database.add('pendingMutations', mutation);
    return id;
  } catch (error) {
    console.error('Failed to queue mutation:', error);
    throw error;
  }
}

export async function getPendingMutations(): Promise<Array<{
  id: number;
  type: 'hold-update';
  projectId: number;
  data: any;
  timestamp: number;
  retryCount: number;
}>> {
  try {
    const database = await initDB();
    const tx = database.transaction('pendingMutations', 'readonly');
    const store = tx.objectStore('pendingMutations');
    const index = store.index('by-timestamp');
    
    const mutations = await index.getAll();
    await tx.done;
    
    return mutations;
  } catch (error) {
    console.error('Failed to get pending mutations:', error);
    return [];
  }
}

export async function updateMutationRetryCount(id: number, retryCount: number): Promise<void> {
  try {
    const database = await initDB();
    const mutation = await database.get('pendingMutations', id);
    
    if (mutation) {
      mutation.retryCount = retryCount;
      await database.put('pendingMutations', mutation);
    }
  } catch (error) {
    console.error('Failed to update mutation retry count:', error);
  }
}

export async function clearMutation(id: number): Promise<void> {
  try {
    const database = await initDB();
    await database.delete('pendingMutations', id);
  } catch (error) {
    console.error('Failed to clear mutation:', error);
  }
}

export async function clearAllMutations(): Promise<void> {
  try {
    const database = await initDB();
    await database.clear('pendingMutations');
  } catch (error) {
    console.error('Failed to clear all mutations:', error);
  }
}
