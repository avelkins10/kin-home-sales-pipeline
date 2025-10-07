import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/next-auth.config';
import { redirect } from 'next/navigation';
import Link from 'next/link';
import { cn } from '@/lib/utils/cn';
import { OfflineIndicator } from '@/components/ui/OfflineIndicator';
import { WebVitalsCollector } from '@/components/ui/WebVitalsCollector';
import { Home, FolderKanban, Clock, BarChart3, Settings } from 'lucide-react';

export default async function DashboardLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  const session = await getServerSession(authOptions);

  if (!session) {
    redirect('/login');
  }

  // Navigation items with their paths and icons
  const navigationItems = [
    { name: 'Dashboard', href: '/', icon: Home, roles: ['closer', 'setter', 'office_leader', 'regional', 'super_admin'] },
    { name: 'Projects', href: '/projects', icon: FolderKanban, roles: ['closer', 'setter', 'office_leader', 'regional', 'super_admin'] },
    { name: 'Holds', href: '/holds', icon: Clock, roles: ['closer', 'setter', 'office_leader', 'regional', 'super_admin'] },
    { name: 'Analytics', href: '/analytics', icon: BarChart3, roles: ['office_leader', 'regional', 'super_admin'] },
    { name: 'Settings', href: '/settings', icon: Settings, roles: ['closer', 'setter', 'office_leader', 'regional', 'super_admin'] },
  ];

  // Filter navigation items based on user role
  const visibleNavItems = navigationItems.filter(item => 
    item.roles.includes(session.user.role)
  );

  return (
    <div className="min-h-screen bg-slate-50">
      {/* Web Vitals Collection */}
      <WebVitalsCollector />
      
      {/* Offline Indicator */}
      <OfflineIndicator />

      {/* Sidebar Navigation */}
      <div className="fixed inset-y-0 left-0 z-50 w-64 bg-white border-r border-slate-200">
        <div className="flex h-full flex-col">
          {/* Logo */}
          <div className="flex h-16 items-center justify-center border-b border-slate-200">
            <h1 className="text-lg font-bold text-slate-900">
              Kin Home Sales
            </h1>
          </div>

          {/* Navigation */}
          <nav className="flex-1 space-y-1 px-3 py-4">
            {visibleNavItems.map((item) => {
              const Icon = item.icon;
              // This is a server component, so we can't use usePathname
              // We'll use a simple check for now - could be improved with client component wrapper
              const isActive = false; // Simplified for server component

              return (
                <Link
                  key={item.href}
                  href={item.href}
                  className={cn(
                    'group flex items-center gap-3 rounded-lg px-3 py-2.5 text-sm font-medium transition-all duration-200',
                    'hover:bg-indigo-50 hover:text-indigo-700',
                    'focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:ring-offset-2',
                    isActive
                      ? 'bg-indigo-50 text-indigo-700'
                      : 'text-slate-700'
                  )}
                >
                  <Icon className={cn(
                    'h-5 w-5 transition-colors',
                    isActive ? 'text-indigo-600' : 'text-slate-400 group-hover:text-indigo-600'
                  )} />
                  {item.name}
                </Link>
              );
            })}
          </nav>

          {/* User Info */}
          <div className="border-t border-slate-200 p-4">
            <div className="flex items-center mb-3">
              <div className="flex-shrink-0">
                <div className="h-9 w-9 rounded-full bg-gradient-to-br from-indigo-500 to-indigo-600 flex items-center justify-center shadow-sm">
                  <span className="text-sm font-semibold text-white">
                    {session.user.name?.charAt(0).toUpperCase()}
                  </span>
                </div>
              </div>
              <div className="ml-3 flex-1 min-w-0">
                <p className="text-sm font-medium text-slate-900 truncate">
                  {session.user.name}
                </p>
                <p className="text-xs text-slate-500 capitalize truncate">
                  {session.user.role.replace('_', ' ')}
                </p>
              </div>
            </div>
            <form action="/api/auth/signout" method="post">
              <button
                type="submit"
                className="w-full text-sm text-slate-600 hover:text-slate-900 text-left py-1.5 px-2 rounded-md hover:bg-slate-100 transition-colors"
              >
                Sign out
              </button>
            </form>
          </div>
        </div>
      </div>

      {/* Main Content */}
      <div className="pl-64">
        <main className="min-h-screen">
          {children}
        </main>
      </div>
    </div>
  );
}
