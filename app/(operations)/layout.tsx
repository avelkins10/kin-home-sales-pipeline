import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/next-auth.config';
import { redirect } from 'next/navigation';
import { OfflineIndicator } from '@/components/ui/OfflineIndicator';
import { WebVitalsCollector } from '@/components/ui/WebVitalsCollector';
import { FrontChatProvider } from '@/components/providers/FrontChatProvider';
import { TopNavbar } from '@/components/layout/TopNavbar';
import { Metadata } from 'next';
import 'react-big-calendar/lib/css/react-big-calendar.css';

export const metadata: Metadata = {
  title: 'KINETIC Operations - Workflow Management',
  description: 'Manage operations workflows, work orders, inventory, and crew scheduling. Optimize installation processes and quality control.',
  openGraph: {
    title: 'KINETIC Operations - Workflow Management',
    description: 'Manage operations workflows, work orders, inventory, and crew scheduling. Optimize installation processes and quality control.',
    type: 'website',
    siteName: 'KINETIC Platform',
  },
  twitter: {
    card: 'summary_large_image',
    title: 'KINETIC Operations - Workflow Management',
    description: 'Manage operations workflows, work orders, inventory, and crew scheduling.',
  },
};

export default async function OperationsLayout({
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
      <div className="min-h-screen bg-slate-50" data-app-context="operations">
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
