'use client';

import { useState } from 'react';
import Link from 'next/link';
import { usePathname } from 'next/navigation';
import { useSession, signOut } from 'next-auth/react';
import { useQuery } from '@tanstack/react-query';
import { cn } from '@/lib/utils/cn';
import { NotificationBell } from '@/components/notifications/NotificationBell';
import { NotificationErrorBoundary } from '@/components/errors/NotificationErrorBoundary';
import { NotificationCenter } from '@/components/operations';
import { AppSwitcher } from './AppSwitcher';
import { useAppContext } from '@/lib/contexts/AppContext';
import {
  Home,
  FolderKanban,
  Clock,
  Calendar,
  BarChart3,
  FileText,
  Settings,
  ExternalLink,
  Menu,
  X,
  LogOut,
  User,
  CheckSquare,
  Wrench,
  Package,
  Users,
  Shield,
  MessageSquare,
  Phone,
  AlertTriangle,
  RefreshCw
} from 'lucide-react';
import { Logo } from './Logo';

export function TopNavbar() {
  const { data: session } = useSession();
  const pathname = usePathname();
  const { currentApp } = useAppContext();
  const [mobileMenuOpen, setMobileMenuOpen] = useState(false);
  const [userMenuOpen, setUserMenuOpen] = useState(false);
  const [notificationCenterOpen, setNotificationCenterOpen] = useState(false);

  // Sales navigation items
  const salesNavigationItems = [
    { name: 'Dashboard', href: '/', icon: Home, roles: ['closer', 'setter', 'coordinator', 'team_lead', 'area_director', 'divisional', 'office_leader', 'regional', 'super_admin'] },
    { name: 'Projects', href: '/projects', icon: FolderKanban, roles: ['closer', 'setter', 'coordinator', 'team_lead', 'area_director', 'divisional', 'office_leader', 'regional', 'super_admin'] },
    { name: 'Tasks', href: '/tasks', icon: CheckSquare, roles: ['closer', 'setter', 'coordinator', 'team_lead', 'area_director', 'divisional', 'office_leader', 'regional', 'super_admin'] },
    // { name: 'Holds', href: '/holds', icon: Clock, roles: ['closer', 'setter', 'coordinator', 'team_lead', 'area_director', 'divisional', 'office_leader', 'regional', 'super_admin'] }, // TODO: Implement holds page
    { name: 'Calendar', href: '/calendar', icon: Calendar, roles: ['closer', 'setter', 'coordinator', 'team_lead', 'area_director', 'divisional', 'office_leader', 'regional', 'super_admin'] },
    { name: 'Analytics', href: '/analytics', icon: BarChart3, roles: ['team_lead', 'area_director', 'divisional', 'office_leader', 'regional', 'super_admin'] },
    { name: 'Reports', href: '/reports', icon: FileText, roles: ['team_lead', 'area_director', 'divisional', 'office_leader', 'regional', 'super_admin'] },
    { name: 'Settings', href: '/settings', icon: Settings, roles: ['closer', 'setter', 'coordinator', 'team_lead', 'area_director', 'divisional', 'office_leader', 'regional', 'super_admin'] },
    { name: 'KIN Sales Hub', href: 'https://sites.google.com/kinhome.com/kinhomesalesnetwork/home', icon: ExternalLink, roles: ['closer', 'setter', 'coordinator', 'team_lead', 'area_director', 'divisional', 'office_leader', 'regional', 'super_admin'], external: true },
  ];

  // Operations navigation items
  const operationsNavigationItems = [
    { name: 'Dashboard', href: '/operations', icon: Home, roles: ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'] },
    { name: 'Projects', href: '/operations/projects', icon: FolderKanban, roles: ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'] },
    { name: 'Outreach', href: '/operations/outreach', icon: MessageSquare, roles: ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'] },
    { name: 'Communications', href: '/operations/communications', icon: Phone, roles: ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'] },
    { name: 'Escalations', href: '/operations/escalations', icon: AlertTriangle, roles: ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'] },
    { name: 'Analytics', href: '/operations/analytics', icon: BarChart3, roles: ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'] },
    { name: 'Sync Monitoring', href: '/operations/sync-monitoring', icon: RefreshCw, roles: ['super_admin'] },
    { name: 'Settings', href: '/operations/settings', icon: Settings, roles: ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'] },
  ];

  // Get navigation items based on current app
  const navigationItems = currentApp === 'operations' ? operationsNavigationItems : salesNavigationItems;

  // Filter navigation items based on user role
  const visibleNavItems = navigationItems.filter(item =>
    session?.user?.role && item.roles.includes(session.user.role)
  );

  // Fetch notification counts for operations app
  const { data: unreadCounts } = useQuery({
    queryKey: ['pc-notification-counts'],
    queryFn: async () => {
      const res = await fetch('/api/operations/notifications?limit=1');
      if (!res.ok) throw new Error('Failed to fetch notification counts');
      const data = await res.json();
      return { total: data.unread_count || 0 };
    },
    refetchInterval: 30000, // 30 seconds
    enabled: currentApp === 'operations' && !!session?.user
  });

  // Fetch inbound queue counts for Communications nav item
  const { data: inboundCounts } = useQuery({
    queryKey: ['inbound-queue-counts'],
    queryFn: async () => {
      const res = await fetch('/api/operations/communications/inbound');
      if (!res.ok) throw new Error('Failed to fetch inbound queue counts');
      const data = await res.json();
      return { 
        count: data.count || 0,
        criticalCount: data.criticalCount || 0,
        highCount: data.highCount || 0
      };
    },
    refetchInterval: 30000, // 30 seconds
    enabled: currentApp === 'operations' && !!session?.user
  });

  // Fetch escalation counts for Escalations nav item
  const { data: escalationCounts } = useQuery({
    queryKey: ['escalation-counts'],
    queryFn: async () => {
      const res = await fetch('/api/operations/escalations');
      if (!res.ok) throw new Error('Failed to fetch escalation counts');
      const data = await res.json();
      return { 
        count: data.count || 0,
        criticalCount: data.stats?.byUrgency?.critical || 0,
        highCount: data.stats?.byUrgency?.high || 0
      };
    },
    refetchInterval: 30000, // 30 seconds
    enabled: currentApp === 'operations' && !!session?.user
  });

  // Fetch outreach counts for Outreach nav item
  const { data: outreachCounts } = useQuery({
    queryKey: ['outreach-counts'],
    queryFn: async () => {
      const res = await fetch('/api/operations/outreach/counts');
      if (!res.ok) throw new Error('Failed to fetch outreach counts');
      const data = await res.json();
      return { count: data.pendingCount || 0 };
    },
    refetchInterval: 30000, // 30 seconds
    enabled: currentApp === 'operations' && !!session?.user
  });

  const handleSignOut = () => {
    signOut({ callbackUrl: '/login' });
  };

  return (
    <header className="sticky top-0 z-50 bg-white border-b border-slate-200 shadow-sm">
      <div className="max-w-[1920px] mx-auto px-4 sm:px-6 lg:px-8">
        <div className="flex items-center justify-between h-16">
          {/* Left: App Switcher + Logo + Navigation */}
          <div className="flex items-center gap-8">
            {/* App Switcher */}
            <AppSwitcher />
            
            {/* Logo */}
            <Logo />

            {/* Desktop Navigation */}
            <nav className="hidden md:flex items-center gap-1">
              {visibleNavItems.map((item) => {
                if (item.external) {
                  return (
                    <a
                      key={item.href}
                      href={item.href}
                      target="_blank"
                      rel="noopener noreferrer"
                      className={cn(
                        'flex items-center gap-2 px-3 py-2 rounded-lg text-sm font-medium transition-colors',
                        'text-slate-700 hover:text-indigo-700 hover:bg-indigo-50'
                      )}
                    >
                      <item.icon className="h-4 w-4" />
                      {item.name}
                    </a>
                  );
                }

                // Handle active state detection
                const isActive = pathname === item.href ||
                  (item.href !== '/' && item.href !== '/operations' && pathname.startsWith(item.href)) ||
                  (item.href === '/operations' && pathname === '/operations');

                return (
                  <Link
                    key={item.href}
                    href={item.href}
                    className={cn(
                      'flex items-center gap-2 px-3 py-2 rounded-lg text-sm font-medium transition-colors relative',
                      isActive
                        ? 'bg-indigo-50 text-indigo-700'
                        : 'text-slate-700 hover:text-indigo-700 hover:bg-indigo-50'
                    )}
                  >
                    <item.icon className="h-4 w-4" />
                    {item.name}
                    {/* Show badge for Communications nav item */}
                    {item.href === '/operations/communications' && inboundCounts && inboundCounts.count > 0 && (
                      <span className={cn(
                        'absolute -top-1 -right-1 h-5 w-5 rounded-full text-white text-xs font-bold flex items-center justify-center',
                        inboundCounts.criticalCount > 0 
                          ? 'bg-red-500' 
                          : inboundCounts.highCount > 0 
                            ? 'bg-orange-500' 
                            : 'bg-blue-500'
                      )}>
                        {inboundCounts.count > 9 ? '9+' : inboundCounts.count}
                      </span>
                    )}
                    {/* Show badge for Escalations nav item */}
                    {item.href === '/operations/escalations' && escalationCounts && escalationCounts.count > 0 && (
                      <span className={cn(
                        'absolute -top-1 -right-1 h-5 w-5 rounded-full text-white text-xs font-bold flex items-center justify-center',
                        escalationCounts.criticalCount > 0 
                          ? 'bg-red-500' 
                          : escalationCounts.highCount > 0 
                            ? 'bg-orange-500' 
                            : 'bg-blue-500'
                      )}>
                        {escalationCounts.count > 9 ? '9+' : escalationCounts.count}
                      </span>
                    )}
                    {/* Show badge for Outreach nav item */}
                    {item.href === '/operations/outreach' && outreachCounts && outreachCounts.count > 0 && (
                      <span className="absolute -top-1 -right-1 h-5 w-5 rounded-full bg-orange-500 text-white text-xs font-bold flex items-center justify-center">
                        {outreachCounts.count > 9 ? '9+' : outreachCounts.count}
                      </span>
                    )}
                  </Link>
                );
              })}
            </nav>
          </div>

          {/* Right: Actions */}
          <div className="flex items-center gap-3">
            {/* Notifications */}
            {currentApp === 'operations' ? (
              <button 
                onClick={() => setNotificationCenterOpen(true)} 
                className="relative p-2 rounded-lg hover:bg-slate-100 transition-colors"
              >
                <NotificationBell />
                {unreadCounts?.total > 0 && (
                  <span className="absolute -top-1 -right-1 h-5 w-5 rounded-full bg-red-500 text-white text-xs font-bold flex items-center justify-center">
                    {unreadCounts.total > 9 ? '9+' : unreadCounts.total}
                  </span>
                )}
              </button>
            ) : (
              <NotificationErrorBoundary>
                <NotificationBell />
              </NotificationErrorBoundary>
            )}

            {/* User Menu */}
            <div className="relative">
              <button
                onClick={() => setUserMenuOpen(!userMenuOpen)}
                className="flex items-center gap-2 px-3 py-2 rounded-lg hover:bg-slate-100 transition-colors"
              >
                <div className="h-8 w-8 rounded-full bg-gradient-to-br from-indigo-500 to-indigo-600 flex items-center justify-center">
                  <span className="text-sm font-semibold text-white">
                    {session?.user?.name?.charAt(0).toUpperCase()}
                  </span>
                </div>
                <div className="hidden lg:block text-left">
                  <p className="text-sm font-medium text-slate-900">
                    {session?.user?.name}
                  </p>
                  <p className="text-xs text-slate-500 capitalize">
                    {session?.user?.role?.replace('_', ' ')}
                  </p>
                </div>
              </button>

              {/* User Dropdown */}
              {userMenuOpen && (
                <>
                  <div
                    className="fixed inset-0 z-40"
                    onClick={() => setUserMenuOpen(false)}
                  />
                  <div className="absolute right-0 mt-2 w-56 bg-white rounded-lg shadow-lg border border-slate-200 py-1 z-50">
                    <div className="px-4 py-3 border-b border-slate-200">
                      <p className="text-sm font-medium text-slate-900">
                        {session?.user?.name}
                      </p>
                      <p className="text-xs text-slate-500">
                        {session?.user?.email}
                      </p>
                    </div>
                    <Link
                      href={currentApp === 'operations' ? '/operations/settings' : '/settings'}
                      className="flex items-center gap-2 px-4 py-2 text-sm text-slate-700 hover:bg-slate-50"
                      onClick={() => setUserMenuOpen(false)}
                    >
                      <Settings className="h-4 w-4" />
                      Settings
                    </Link>
                    <button
                      onClick={handleSignOut}
                      className="w-full flex items-center gap-2 px-4 py-2 text-sm text-red-600 hover:bg-red-50"
                    >
                      <LogOut className="h-4 w-4" />
                      Sign out
                    </button>
                  </div>
                </>
              )}
            </div>

            {/* Mobile Menu Button */}
            <button
              onClick={() => setMobileMenuOpen(!mobileMenuOpen)}
              className="md:hidden p-2 rounded-lg hover:bg-slate-100"
            >
              {mobileMenuOpen ? (
                <X className="h-6 w-6 text-slate-700" />
              ) : (
                <Menu className="h-6 w-6 text-slate-700" />
              )}
            </button>
          </div>
        </div>
      </div>

      {/* Mobile Navigation */}
      {mobileMenuOpen && (
        <div className="md:hidden border-t border-slate-200 bg-white">
          <nav className="px-4 py-3 space-y-1">
            {visibleNavItems.map((item) => {
              if (item.external) {
                return (
                  <a
                    key={item.href}
                    href={item.href}
                    target="_blank"
                    rel="noopener noreferrer"
                    className="flex items-center gap-3 px-3 py-2 rounded-lg text-sm font-medium text-slate-700 hover:bg-indigo-50 hover:text-indigo-700"
                    onClick={() => setMobileMenuOpen(false)}
                  >
                    <item.icon className="h-5 w-5" />
                    {item.name}
                  </a>
                );
              }

              const isActive = pathname === item.href || (item.href !== '/' && pathname.startsWith(item.href));

              return (
                <Link
                  key={item.href}
                  href={item.href}
                  className={cn(
                    'flex items-center gap-3 px-3 py-2 rounded-lg text-sm font-medium relative',
                    isActive
                      ? 'bg-indigo-50 text-indigo-700'
                      : 'text-slate-700 hover:bg-indigo-50 hover:text-indigo-700'
                  )}
                  onClick={() => setMobileMenuOpen(false)}
                >
                  <item.icon className="h-5 w-5" />
                  {item.name}
                  {/* Show badge for Communications nav item */}
                  {item.href === '/operations/communications' && inboundCounts && inboundCounts.count > 0 && (
                    <span className={cn(
                      'ml-auto h-5 w-5 rounded-full text-white text-xs font-bold flex items-center justify-center',
                      inboundCounts.criticalCount > 0 
                        ? 'bg-red-500' 
                        : inboundCounts.highCount > 0 
                          ? 'bg-orange-500' 
                          : 'bg-blue-500'
                    )}>
                      {inboundCounts.count > 9 ? '9+' : inboundCounts.count}
                    </span>
                  )}
                  {/* Show badge for Escalations nav item */}
                  {item.href === '/operations/escalations' && escalationCounts && escalationCounts.count > 0 && (
                    <span className={cn(
                      'ml-auto h-5 w-5 rounded-full text-white text-xs font-bold flex items-center justify-center',
                      escalationCounts.criticalCount > 0 
                        ? 'bg-red-500' 
                        : escalationCounts.highCount > 0 
                          ? 'bg-orange-500' 
                          : 'bg-blue-500'
                    )}>
                      {escalationCounts.count > 9 ? '9+' : escalationCounts.count}
                    </span>
                  )}
                  {/* Show badge for Outreach nav item */}
                  {item.href === '/operations/outreach' && outreachCounts && outreachCounts.count > 0 && (
                    <span className="ml-auto h-5 w-5 rounded-full bg-orange-500 text-white text-xs font-bold flex items-center justify-center">
                      {outreachCounts.count > 9 ? '9+' : outreachCounts.count}
                    </span>
                  )}
                </Link>
              );
            })}
          </nav>
        </div>
      )}

      {/* Notification Center for Operations */}
      {currentApp === 'operations' && (
        <NotificationCenter 
          isOpen={notificationCenterOpen} 
          onClose={() => setNotificationCenterOpen(false)} 
        />
      )}
    </header>
  );
}
