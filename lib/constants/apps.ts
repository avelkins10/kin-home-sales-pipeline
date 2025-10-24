/**
 * Centralized app-aware constants for multi-app architecture
 * This file contains route maps, navigation configs, and query keys per app
 */

export type AppType = 'sales' | 'operations';

// App route mappings
export const APP_ROUTES = {
  sales: {
    dashboard: '/',
    projects: '/projects',
    analytics: '/analytics',
    reports: '/reports',
    calendar: '/calendar',
    tasks: '/tasks',
    settings: '/settings',
  },
  operations: {
    dashboard: '/operations',
    workOrders: '/operations/work-orders',
    inventory: '/operations/inventory',
    scheduling: '/operations/scheduling',
    quality: '/operations/quality',
    settings: '/operations/settings',
  },
} as const;

// Navigation configuration per app
export const NAVIGATION_CONFIG = {
  sales: [
    { name: 'Dashboard', href: '/', icon: 'Home' },
    { name: 'Projects', href: '/projects', icon: 'FolderOpen' },
    { name: 'Analytics', href: '/analytics', icon: 'BarChart3' },
    { name: 'Reports', href: '/reports', icon: 'FileText' },
    { name: 'Calendar', href: '/calendar', icon: 'Calendar' },
    { name: 'Tasks', href: '/tasks', icon: 'CheckSquare' },
    { name: 'Settings', href: '/settings', icon: 'Settings' },
  ],
  operations: [
    { name: 'Dashboard', href: '/operations', icon: 'Home' },
    { name: 'Work Orders', href: '/operations/work-orders', icon: 'Wrench' },
    { name: 'Inventory', href: '/operations/inventory', icon: 'Package' },
    { name: 'Scheduling', href: '/operations/scheduling', icon: 'Calendar' },
    { name: 'Quality Control', href: '/operations/quality', icon: 'Shield' },
    { name: 'Settings', href: '/operations/settings', icon: 'Settings' },
  ],
} as const;

// Query keys per app
export const QUERY_KEYS = {
  sales: [
    'projects',
    'dashboard-metrics',
    'urgent-projects',
    'recent-projects',
    'team-activity',
    'analytics-metrics',
    'pipeline-forecast',
    'milestone-timings',
  ],
  operations: [
    'operations-metrics',
    'work-orders',
    'inventory',
    'crew-schedule',
    'quality-control',
    'operations-dashboard',
  ],
} as const;

// Feature flags per app
export const FEATURE_FLAGS = {
  sales: {
    offlineSupport: true,
    realTimeUpdates: true,
    analytics: true,
    reporting: true,
    calendar: true,
    taskManagement: true,
  },
  operations: {
    offlineSupport: true,
    realTimeUpdates: true,
    workOrderManagement: false, // Coming soon
    inventoryManagement: false, // Coming soon
    crewScheduling: false, // Coming soon
    qualityControl: false, // Coming soon
  },
} as const;

// App metadata
export const APP_METADATA = {
  sales: {
    title: 'KINETIC Sales',
    description: 'Solar installation project tracking and sales management',
    color: 'indigo',
    icon: 'TrendingUp',
  },
  operations: {
    title: 'KINETIC Operations',
    description: 'Operations management and workflow optimization',
    color: 'blue',
    icon: 'Settings',
  },
} as const;

// Role-based access control per app
export const APP_ACCESS = {
  sales: {
    roles: [
      'closer', 'setter', 'coordinator', 'team_lead',
      'office_leader', 'area_director', 'divisional',
      'regional', 'super_admin'
    ],
    defaultRoute: '/',
  },
  operations: {
    roles: [
      'operations_coordinator', 'operations_manager',
      'office_leader', 'regional', 'super_admin'
    ],
    defaultRoute: '/operations',
  },
} as const;

// Cross-app access roles
export const CROSS_APP_ROLES = [
  'office_leader', 'regional', 'super_admin', 'operations_manager'
];

// Helper functions
export function getAppRoutes(app: AppType) {
  return APP_ROUTES[app];
}

export function getNavigationConfig(app: AppType) {
  return NAVIGATION_CONFIG[app];
}

export function getQueryKeys(app: AppType) {
  return QUERY_KEYS[app];
}

export function getFeatureFlags(app: AppType) {
  return FEATURE_FLAGS[app];
}

export function getAppMetadata(app: AppType) {
  return APP_METADATA[app];
}

export function getAppAccess(app: AppType) {
  return APP_ACCESS[app];
}

export function canAccessApp(app: AppType, role: string): boolean {
  return APP_ACCESS[app].roles.includes(role);
}

export function getDefaultRouteForRole(role: string): string {
  if (APP_ACCESS.operations.roles.includes(role)) {
    return APP_ACCESS.operations.defaultRoute;
  }
  return APP_ACCESS.sales.defaultRoute;
}
