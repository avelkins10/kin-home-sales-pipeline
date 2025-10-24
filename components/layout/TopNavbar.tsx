'use client';

import { useState } from 'react';
import Link from 'next/link';
import { usePathname } from 'next/navigation';
import { useSession, signOut } from 'next-auth/react';
import { cn } from '@/lib/utils/cn';
import { NotificationBell } from '@/components/notifications/NotificationBell';
import { NotificationErrorBoundary } from '@/components/errors/NotificationErrorBoundary';
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
  Shield
} from 'lucide-react';
import { Logo } from './Logo';

export function TopNavbar() {
  const { data: session } = useSession();
  const pathname = usePathname();
  const { currentApp } = useAppContext();
  const [mobileMenuOpen, setMobileMenuOpen] = useState(false);
  const [userMenuOpen, setUserMenuOpen] = useState(false);

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
    { name: 'Work Orders', href: '/operations/work-orders', icon: Wrench, roles: ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'] },
    { name: 'Inventory', href: '/operations/inventory', icon: Package, roles: ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'] },
    { name: 'Scheduling', href: '/operations/scheduling', icon: Calendar, roles: ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'] },
    { name: 'Quality Control', href: '/operations/quality', icon: Shield, roles: ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'] },
    { name: 'Settings', href: '/operations/settings', icon: Settings, roles: ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'] },
  ];

  // Get navigation items based on current app
  const navigationItems = currentApp === 'operations' ? operationsNavigationItems : salesNavigationItems;

  // Filter navigation items based on user role
  const visibleNavItems = navigationItems.filter(item =>
    session?.user?.role && item.roles.includes(session.user.role)
  );

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
                      'flex items-center gap-2 px-3 py-2 rounded-lg text-sm font-medium transition-colors',
                      isActive
                        ? 'bg-indigo-50 text-indigo-700'
                        : 'text-slate-700 hover:text-indigo-700 hover:bg-indigo-50'
                    )}
                  >
                    <item.icon className="h-4 w-4" />
                    {item.name}
                  </Link>
                );
              })}
            </nav>
          </div>

          {/* Right: Actions */}
          <div className="flex items-center gap-3">
            {/* Notifications */}
            <NotificationErrorBoundary>
              <NotificationBell />
            </NotificationErrorBoundary>

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
                    'flex items-center gap-3 px-3 py-2 rounded-lg text-sm font-medium',
                    isActive
                      ? 'bg-indigo-50 text-indigo-700'
                      : 'text-slate-700 hover:bg-indigo-50 hover:text-indigo-700'
                  )}
                  onClick={() => setMobileMenuOpen(false)}
                >
                  <item.icon className="h-5 w-5" />
                  {item.name}
                </Link>
              );
            })}
          </nav>
        </div>
      )}
    </header>
  );
}
