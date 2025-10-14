import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/next-auth.config';
import { redirect } from 'next/navigation';
import { OfflineIndicator } from '@/components/ui/OfflineIndicator';
import { WebVitalsCollector } from '@/components/ui/WebVitalsCollector';
import { FrontChatProvider } from '@/components/providers/FrontChatProvider';
import { TopNavbar } from '@/components/layout/TopNavbar';
import 'react-big-calendar/lib/css/react-big-calendar.css';

export default async function DashboardLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  const session = await getServerSession(authOptions);

  if (!session) {
    redirect('/login');
  }

  return (
    <FrontChatProvider>
      <div className="min-h-screen bg-slate-50">
        {/* Web Vitals Collection */}
        <WebVitalsCollector />

        {/* Offline Indicator */}
        <OfflineIndicator />

        {/* Top Navigation */}
        <TopNavbar />

        {/* Main Content - Full Width */}
        <main className="min-h-screen">
          {children}
        </main>
      </div>
    </FrontChatProvider>
  );
}
