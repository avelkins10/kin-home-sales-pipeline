'use client';

import { useSession } from 'next-auth/react';
import { redirect } from 'next/navigation';
import Link from 'next/link';
import { Card, CardHeader, CardTitle, CardDescription, CardContent } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { FileText, TrendingUp, AlertCircle, BarChart3, ArrowRight } from 'lucide-react';
import { isManagerRole } from '@/lib/utils/role-helpers';

const reports = [
  {
    id: 'intake-weekly',
    title: 'Weekly Intake Quality Report',
    description: 'Track intake quality metrics by closer - first-time pass rates, rejection rates, and common issues',
    icon: TrendingUp,
    href: '/reports/intake-weekly',
    color: 'blue',
    roles: ['office_leader', 'regional', 'super_admin', 'team_lead'],
  },
  {
    id: 'intake-rejections',
    title: 'Office Intake Rejections Analysis',
    description: 'Office-by-office breakdown of intake rejections, common reasons, and closer performance metrics',
    icon: AlertCircle,
    href: '/reports/intake-rejections',
    color: 'red',
    roles: ['office_leader', 'regional', 'super_admin'],
  },
];

export default function ReportsPage() {
  const { data: session, status } = useSession();

  if (status === 'loading') {
    return (
      <div className="space-y-6 p-6">
        <h1 className="text-3xl font-bold">Reports</h1>
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
          {[...Array(2)].map((_, i) => (
            <Card key={i} className="animate-pulse">
              <CardContent className="p-6">
                <div className="h-32 bg-gray-200 rounded" />
              </CardContent>
            </Card>
          ))}
        </div>
      </div>
    );
  }

  if (!session?.user) {
    redirect('/login');
  }

  const userRole = session.user.role || 'closer';

  // Filter reports based on user role
  const availableReports = reports.filter(report =>
    report.roles.includes(userRole)
  );

  return (
    <div className="space-y-6 p-6">
      {/* Header */}
      <div>
        <h1 className="text-3xl font-bold">Reports</h1>
        <p className="text-muted-foreground mt-1">
          Access detailed analytics and performance reports
        </p>
      </div>

      {/* Reports Grid */}
      {availableReports.length === 0 ? (
        <Card>
          <CardContent className="p-12 text-center">
            <FileText className="h-12 w-12 text-muted-foreground mx-auto mb-4" />
            <h3 className="text-lg font-semibold mb-2">No Reports Available</h3>
            <p className="text-muted-foreground">
              You don't have access to any reports. Contact your administrator if you believe this is an error.
            </p>
          </CardContent>
        </Card>
      ) : (
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
          {availableReports.map((report) => {
            const Icon = report.icon;
            const colorClasses = {
              blue: 'bg-blue-500',
              red: 'bg-red-500',
              green: 'bg-green-500',
              purple: 'bg-purple-500',
              orange: 'bg-orange-500',
            }[report.color] || 'bg-gray-500';

            return (
              <Link key={report.id} href={report.href}>
                <Card className="h-full hover:shadow-lg transition-shadow cursor-pointer group">
                  <CardHeader>
                    <div className="flex items-start justify-between mb-2">
                      <div className={`p-3 rounded-lg ${colorClasses} text-white`}>
                        <Icon className="h-6 w-6" />
                      </div>
                      <ArrowRight className="h-5 w-5 text-muted-foreground group-hover:translate-x-1 transition-transform" />
                    </div>
                    <CardTitle className="text-xl">{report.title}</CardTitle>
                    <CardDescription className="mt-2 line-clamp-2">
                      {report.description}
                    </CardDescription>
                  </CardHeader>
                  <CardContent>
                    <Button variant="outline" className="w-full group-hover:bg-primary group-hover:text-primary-foreground transition-colors">
                      View Report
                    </Button>
                  </CardContent>
                </Card>
              </Link>
            );
          })}
        </div>
      )}

      {/* Info Card */}
      <Card className="bg-muted/50">
        <CardContent className="p-6">
          <div className="flex items-start gap-4">
            <BarChart3 className="h-8 w-8 text-primary flex-shrink-0" />
            <div>
              <h3 className="font-semibold mb-1">Need a Custom Report?</h3>
              <p className="text-sm text-muted-foreground">
                Can't find the report you're looking for? Contact your system administrator to request custom analytics and reporting features tailored to your needs.
              </p>
            </div>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}
