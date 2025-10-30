import { Suspense } from 'react'
import { getServerSession } from 'next-auth'
import { redirect } from 'next/navigation'
import { authOptions } from '@/lib/auth/next-auth.config'
import { sql } from '@/lib/db/client'
import { Tabs, TabsList, TabsTrigger, TabsContent } from '@/components/ui/tabs'
import { ProfileTab } from '@/components/settings/ProfileTab'
import { NotificationsTab } from '@/components/settings/NotificationsTab'
import UsersTab from '@/components/settings/UsersTab'
import OfficesTab from '@/components/settings/OfficesTab'
import { Skeleton } from '@/components/ui/skeleton'
import SystemTab from '@/components/settings/SystemTab'
import { AuditLogsTab } from '@/components/settings/AuditLogsTab'
import { MilestoneConfigTab } from '@/components/settings/MilestoneConfigTab'
import { Card, CardContent } from '@/components/ui/card'
import RepCardDebugTab from '@/components/settings/RepCardDebugTab'
import { RepCardConfigTab } from '@/components/settings/RepCardConfigTab'

export default async function SettingsPage() {
  const session = await getServerSession(authOptions)

  if (!session) {
    redirect('/login')
  }

  console.log('[SettingsPage] Loading settings for user:', session.user.id);

  try {
    // Fetch user data
    const userQueryResult = await sql`
      SELECT id, email, name, phone, role, quickbase_user_id, sales_office, is_active, created_at, updated_at
      FROM users
      WHERE id = ${session.user.id}
    `
    const userResult = userQueryResult.rows[0]

    if (!userResult) {
      console.error('[SettingsPage] User not found in database:', session.user.id);
      redirect('/login')
    }

    console.log('[SettingsPage] User found:', userResult.email);

    // Fetch notification settings
    const notificationQueryResult = await sql`
      SELECT * FROM notification_settings
      WHERE user_id = ${session.user.id}
    `
    const notificationSettings = notificationQueryResult.rows[0]
    console.log('[SettingsPage] Notification settings:', notificationSettings ? 'found' : 'not found');

  const user = {
    id: userResult.id,
    email: userResult.email,
    name: userResult.name,
    phone: userResult.phone || '',
    role: userResult.role,
    quickbaseUserId: userResult.quickbase_user_id,
    office: userResult.sales_office?.[0] || '',
    salesOffice: userResult.sales_office || [],
    isActive: userResult.is_active,
    createdAt: userResult.created_at,
    updatedAt: userResult.updated_at
  }

  const defaultNotificationSettings = {
    userId: session.user.id,
    emailEnabled: true,
    urgentAlerts: true,
    dailyDigest: false,
    weeklySummary: false,
    holdThreshold: 7,
    ageWarningThreshold: 90,
    installOverdueThreshold: 14
  }

  const currentNotificationSettings = notificationSettings ? {
    userId: notificationSettings.user_id,
    emailEnabled: notificationSettings.email_enabled,
    urgentAlerts: notificationSettings.urgent_alerts,
    dailyDigest: notificationSettings.daily_digest,
    weeklySummary: notificationSettings.weekly_summary,
    holdThreshold: notificationSettings.hold_threshold,
    ageWarningThreshold: notificationSettings.age_warning_threshold,
    installOverdueThreshold: notificationSettings.install_overdue_threshold
  } : defaultNotificationSettings

    const role = session.user.role
    const hasTeamAccess = ['office_leader', 'regional', 'super_admin'].includes(role)
    const isSuperAdmin = role === 'super_admin'

    return (
    <div className="min-h-screen bg-gray-50 p-6">
      <div className="max-w-5xl mx-auto space-y-6">
        <div>
          <h1 className="text-3xl font-bold">Settings</h1>
          <p className="text-gray-600">Manage your account and preferences</p>
        </div>

        <Tabs defaultValue="profile" className="space-y-6">
          <TabsList>
            <TabsTrigger value="profile">Profile</TabsTrigger>
            <TabsTrigger value="notifications">Notifications</TabsTrigger>
            {isSuperAdmin && (
              <>
                <TabsTrigger value="milestones">Milestones</TabsTrigger>
                <TabsTrigger value="users">Users</TabsTrigger>
                <TabsTrigger value="offices">Offices</TabsTrigger>
                <TabsTrigger value="system">System</TabsTrigger>
                <TabsTrigger value="audit">Audit Logs</TabsTrigger>
                <TabsTrigger value="repcard-debug">RepCard Debug</TabsTrigger>
                <TabsTrigger value="repcard-config">RepCard Config</TabsTrigger>
              </>
            )}
          </TabsList>

          <Suspense fallback={<Skeleton className="h-96 w-full" />}>
            <TabsContent value="profile">
              <ProfileTab user={user} />
            </TabsContent>
          </Suspense>

          <Suspense fallback={<Skeleton className="h-96 w-full" />}>
            <TabsContent value="notifications">
              <NotificationsTab 
                userRole={role} 
                currentSettings={currentNotificationSettings} 
              />
            </TabsContent>
          </Suspense>

          {isSuperAdmin && (
            <>
              <Suspense fallback={<Skeleton className="h-96 w-full" />}>
                <TabsContent value="milestones">
                  <MilestoneConfigTab />
                </TabsContent>
              </Suspense>

              <Suspense fallback={<Skeleton className="h-96 w-full" />}>
                <TabsContent value="users">
                  <UsersTab />
                </TabsContent>
              </Suspense>

              <Suspense fallback={<Skeleton className="h-96 w-full" />}>
                <TabsContent value="offices">
                  <OfficesTab />
                </TabsContent>
              </Suspense>

              <Suspense fallback={<Skeleton className="h-96 w-full" />}>
                <TabsContent value="system">
                  <SystemTab />
                </TabsContent>
              </Suspense>

              <Suspense fallback={<Skeleton className="h-96 w-full" />}>
                <TabsContent value="audit">
                  <AuditLogsTab />
                </TabsContent>
              </Suspense>

              <Suspense fallback={<Skeleton className="h-96 w-full" />}>
                <TabsContent value="repcard-debug">
                  <RepCardDebugTab />
                </TabsContent>
              </Suspense>

              <Suspense fallback={<Skeleton className="h-96 w-full" />}>
                <TabsContent value="repcard-config">
                  <RepCardConfigTab />
                </TabsContent>
              </Suspense>
            </>
          )}
        </Tabs>
      </div>
    </div>
    )
  } catch (error) {
    console.error('[SettingsPage] Error loading settings:', error);
    throw error;
  }
}
