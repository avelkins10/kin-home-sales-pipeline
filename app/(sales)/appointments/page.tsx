import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';
import { redirect } from 'next/navigation';
import { AppointmentSchedule } from '@/components/repcard/AppointmentSchedule';

export const metadata = {
  title: 'Appointment Schedule | RepCard',
  description: 'View and manage appointment schedules'
};

export default async function AppointmentsPage() {
  const session = await getServerSession(authOptions);
  
  if (!session) {
    redirect('/login');
  }

  const userId = (session.user as any).id as string;
  const userRole = session.user.role;

  // Check if user has access
  const allowedRoles = ['closer', 'setter', 'office_leader', 'area_director', 'divisional', 'regional', 'super_admin', 'team_lead'];
  if (!allowedRoles.includes(userRole)) {
    redirect('/');
  }

  return (
    <div className="container mx-auto py-6 space-y-6">
      <div>
        <h1 className="text-3xl font-bold">Appointment Schedule</h1>
        <p className="text-muted-foreground mt-2">
          View and manage your appointment calendar with live RepCard data
        </p>
      </div>
      
      <AppointmentSchedule 
        userId={userId}
        userRole={userRole}
      />
    </div>
  );
}
