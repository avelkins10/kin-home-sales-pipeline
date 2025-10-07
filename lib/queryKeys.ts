/**
 * Centralized query key helpers for React Query
 * Ensures consistent query key structure across the application
 */

export const projectKey = (id: string | number) => ['project', String(id)] as const;
