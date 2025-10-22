// app/api/analytics/office-metrics/route.ts
import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';
import { logAIRequest, logAIError } from '@/lib/analytics/aiMetrics';

export async function GET(req: NextRequest) {
  try {
    const session = await getServerSession(authOptions);
    if (!session?.user?.quickbaseUserId) {
      return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
    }

    const { searchParams } = new URL(req.url);
    const userId = searchParams.get('userId') || session.user.quickbaseUserId;
    const role = searchParams.get('role') || session.user.role || 'closer';

    logAIRequest('getOfficeMetrics', userId, { role });

    // Mock data - in real implementation, this would query your database
    const officeMetrics = [
      {
        officeId: 'office-1',
        officeName: 'San Francisco',
        totalReps: 12,
        activeReps: 11,
        totalProjects: 45,
        projectsOnHold: 8,
        totalRevenue: 2250000,
        revenueAtRisk: 400000,
        averageHoldDuration: 12.5,
        holdRate: 17.8,
        completionRate: 82.2,
        topPerformers: [
          {
            repId: 'rep-1',
            repName: 'Sarah Johnson',
            projectsCompleted: 8,
            revenueGenerated: 450000,
            holdRate: 12.5
          },
          {
            repId: 'rep-2',
            repName: 'Mike Chen',
            projectsCompleted: 7,
            revenueGenerated: 380000,
            holdRate: 14.3
          }
        ],
        topHoldCategories: [
          {
            category: 'HOA Hold',
            count: 3,
            avgDuration: 18.5,
            totalValue: 150000
          },
          {
            category: 'Finance Hold',
            count: 2,
            avgDuration: 8.2,
            totalValue: 120000
          },
          {
            category: 'Permit Hold',
            count: 2,
            avgDuration: 15.1,
            totalValue: 100000
          }
        ],
        trends: {
          revenue: [
            { date: '2023-10-01', value: 2000000 },
            { date: '2023-10-02', value: 2100000 },
            { date: '2023-10-03', value: 2200000 }
          ],
          holdRate: [
            { date: '2023-10-01', value: 20.5 },
            { date: '2023-10-02', value: 19.2 },
            { date: '2023-10-03', value: 17.8 }
          ],
          completion: [
            { date: '2023-10-01', value: 79.5 },
            { date: '2023-10-02', value: 80.8 },
            { date: '2023-10-03', value: 82.2 }
          ]
        }
      },
      {
        officeId: 'office-2',
        officeName: 'Los Angeles',
        totalReps: 15,
        activeReps: 14,
        totalProjects: 62,
        projectsOnHold: 14,
        totalRevenue: 3100000,
        revenueAtRisk: 700000,
        averageHoldDuration: 15.2,
        holdRate: 22.6,
        completionRate: 77.4,
        topPerformers: [
          {
            repId: 'rep-3',
            repName: 'Emily Rodriguez',
            projectsCompleted: 10,
            revenueGenerated: 520000,
            holdRate: 10.0
          },
          {
            repId: 'rep-4',
            repName: 'David Kim',
            projectsCompleted: 9,
            revenueGenerated: 480000,
            holdRate: 11.1
          }
        ],
        topHoldCategories: [
          {
            category: 'Finance Hold',
            count: 5,
            avgDuration: 12.3,
            totalValue: 250000
          },
          {
            category: 'HOA Hold',
            count: 4,
            avgDuration: 20.1,
            totalValue: 200000
          },
          {
            category: 'Customer Hold',
            count: 3,
            avgDuration: 8.7,
            totalValue: 150000
          }
        ],
        trends: {
          revenue: [
            { date: '2023-10-01', value: 2800000 },
            { date: '2023-10-02', value: 2950000 },
            { date: '2023-10-03', value: 3100000 }
          ],
          holdRate: [
            { date: '2023-10-01', value: 25.8 },
            { date: '2023-10-02', value: 24.1 },
            { date: '2023-10-03', value: 22.6 }
          ],
          completion: [
            { date: '2023-10-01', value: 74.2 },
            { date: '2023-10-02', value: 75.9 },
            { date: '2023-10-03', value: 77.4 }
          ]
        }
      }
    ];

    return NextResponse.json(officeMetrics);
  } catch (error: any) {
    console.error('[API] /api/analytics/office-metrics error:', error);
    logAIError('getOfficeMetrics', 'unknown', error);
    return NextResponse.json({ 
      error: error.message || 'Internal server error' 
    }, { status: 500 });
  }
}