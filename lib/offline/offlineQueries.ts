import { cacheProject, getCachedProject, cacheProjectList, getCachedProjectList } from './storage';
import { QuickbaseProject } from '@/lib/types/project';

// Offline detection helpers
export function isOnline(): boolean {
  return navigator.onLine;
}

export function isOffline(): boolean {
  return !navigator.onLine;
}

// Wrapped query functions
export async function getProjectsForUserOffline(
  userId: string,
  role: string,
  view?: string,
  search?: string,
  sort?: string
): Promise<QuickbaseProject[]> {
  try {
    if (isOnline()) {
      // Online: fetch from API and cache result
      const params = new URLSearchParams();
      if (view) params.set('view', view);
      if (search) params.set('search', search);
      if (sort && sort !== 'default') params.set('sort', sort);

      const url = `/api/projects${params.toString() ? `?${params.toString()}` : ''}`;
      const res = await fetch(url);
      if (!res.ok) throw new Error('Failed to fetch projects');
      const projects = await res.json();
      await cacheProjectList(projects, userId, role);
      return projects;
    } else {
      // Offline: return cached data
      const cachedProjects = await getCachedProjectList(userId, role);
      if (cachedProjects.length === 0) {
        console.warn('No cached projects available offline');
      }
      return cachedProjects;
    }
  } catch (error) {
    console.error('Failed to get projects (offline-aware):', error);

    // Fallback to cache even if online request failed
    if (isOffline()) {
      return await getCachedProjectList(userId, role);
    }

    return [];
  }
}

export async function getProjectByIdOffline(recordId: number): Promise<QuickbaseProject | null> {
  try {
    if (isOnline()) {
      // Online: fetch from API and cache result
      const res = await fetch(`/api/projects/${recordId}`);
      if (!res.ok) throw new Error('Failed to fetch project');
      const project = await res.json();
      if (project) {
        await cacheProject(project);
      }
      return project;
    } else {
      // Offline: return cached data
      const cachedProject = await getCachedProject(recordId);
      if (!cachedProject) {
        console.warn(`No cached project ${recordId} available offline`);
        throw new Error('No cached project');
      }
      return cachedProject;
    }
  } catch (error) {
    console.error('Failed to get project (offline-aware):', error);
    
    // Fallback to cache even if online request failed
    if (isOffline()) {
      return await getCachedProject(recordId);
    }
    
    return null;
  }
}

// Cache warming
export async function warmCache(userId: string, role: string): Promise<void> {
  try {
    if (!isOnline()) {
      console.warn('Cannot warm cache while offline');
      return;
    }

    console.log('Warming cache for user:', userId, 'role:', role);
    const res = await fetch(`/api/projects`);
    if (!res.ok) throw new Error('Failed to warm cache');
    const projects = await res.json();
    await cacheProjectList(projects, userId, role);
    
    // Cache individual projects
    for (const project of projects) {
      await cacheProject(project);
    }
    
    console.log(`Cached ${projects.length} projects`);
  } catch (error) {
    console.error('Failed to warm cache:', error);
  }
}

// Cache status
export async function getCacheStatus(): Promise<{
  isOnline: boolean;
  hasCachedProjects: boolean;
  cacheAge: number;
}> {
  try {
    const cachedProjects = await getCachedProjectList('', '');
    const hasCachedProjects = cachedProjects.length > 0;
    
    let cacheAge = 0;
    if (hasCachedProjects) {
      // Get the oldest cached project age
      const oldestCache = Math.min(
        ...cachedProjects.map(project => {
          const recordId = project[3]?.value;
          if (recordId) {
            // This is a simplified approach - in practice you'd need to track cache timestamps
            return Date.now() - 300000; // Assume 5 minutes old
          }
          return Date.now();
        })
      );
      cacheAge = Date.now() - oldestCache;
    }

    return {
      isOnline: isOnline(),
      hasCachedProjects,
      cacheAge,
    };
  } catch (error) {
    console.error('Failed to get cache status:', error);
    return {
      isOnline: isOnline(),
      hasCachedProjects: false,
      cacheAge: 0,
    };
  }
}

// Export all wrapped functions as default exports for easy drop-in replacement
export default {
  getProjectsForUser: getProjectsForUserOffline,
  getProjectById: getProjectByIdOffline,
  warmCache,
  getCacheStatus,
  isOnline,
  isOffline,
};
