// app/api/analytics/rep-performance/route.ts
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
    const office = searchParams.get('office') || 'all';

    logAIRequest('getRepPerformance', userId, { role, office });

    // Mock data - in real implementation, this would query your database
    const repPerformance = [
      {
        repId: 'rep-1',
        repName: 'Sarah Johnson',
        office: 'San Francisco',
        role: 'closer',
        totalProjects: 12,
        projectsOnHold: 2,
        totalRevenue: 650000,
        revenueAtRisk: 120000,
        averageHoldDuration: 8.5,
        holdRate: 16.7,
        completionRate: 83.3,
        commissionEarned: 19500,
        performanceScore: 92,
        trends: {
          holdRate: 'improving',
          revenue: 'growing',
          completion: 'improving'
        },
        strengths: [
          'Excellent HOA negotiation skills',
          'Strong customer relationship management',
          'Fast permit processing'
        ],
        improvementAreas: [
          'Finance hold resolution time',
          'Follow-up consistency'
        ],
        aiInsights: [
          {
            type: 'success_pattern',
            title: 'HOA Success Pattern',
            description: 'Sarah resolves HOA holds 40% faster than team average using her negotiation approach.',
            impact: 'high',
            recommendation: 'Document and share Sarah\'s HOA negotiation process with the team.'
          },
          {
            type: 'improvement_opportunity',
            title: 'Finance Hold Optimization',
            description: 'Finance holds are taking 12 days on average, 3 days longer than team average.',
            impact: 'medium',
            recommendation: 'Implement proactive finance check process before project start.'
          }
        ]
      },
      {
        repId: 'rep-2',
        repName: 'Mike Chen',
        office: 'San Francisco',
        role: 'closer',
        totalProjects: 10,
        projectsOnHold: 3,
        totalRevenue: 520000,
        revenueAtRisk: 180000,
        averageHoldDuration: 14.2,
        holdRate: 30.0,
        completionRate: 70.0,
        commissionEarned: 15600,
        performanceScore: 78,
        trends: {
          holdRate: 'declining',
          revenue: 'stable',
          completion: 'declining'
        },
        strengths: [
          'Strong technical knowledge',
          'Good customer communication'
        ],
        improvementAreas: [
          'Hold resolution speed',
          'Proactive problem solving',
          'Time management'
        ],
        aiInsights: [
          {
            type: 'performance_alert',
            title: 'Hold Rate Increasing',
            description: 'Mike\'s hold rate has increased from 20% to 30% over the past month.',
            impact: 'high',
            recommendation: 'Schedule coaching session to identify root causes and develop improvement plan.'
          },
          {
            type: 'opportunity',
            title: 'High-Value Project Focus',
            description: 'Mike has 2 high-value projects ($90K each) that could benefit from immediate attention.',
            impact: 'high',
            recommendation: 'Prioritize these projects and provide additional support resources.'
          }
        ]
      },
      {
        repId: 'rep-3',
        repName: 'Emily Rodriguez',
        office: 'Los Angeles',
        role: 'closer',
        totalProjects: 15,
        projectsOnHold: 2,
        totalRevenue: 780000,
        revenueAtRisk: 95000,
        averageHoldDuration: 6.8,
        holdRate: 13.3,
        completionRate: 86.7,
        commissionEarned: 23400,
        performanceScore: 95,
        trends: {
          holdRate: 'improving',
          revenue: 'growing',
          completion: 'improving'
        },
        strengths: [
          'Exceptional hold resolution speed',
          'Strong network of industry contacts',
          'Excellent follow-up process'
        ],
        improvementAreas: [
          'Documentation consistency'
        ],
        aiInsights: [
          {
            type: 'success_pattern',
            title: 'Rapid Resolution Expert',
            description: 'Emily resolves holds 60% faster than team average with 95% success rate.',
            impact: 'high',
            recommendation: 'Create case study of Emily\'s approach for team training.'
          },
          {
            type: 'mentorship_opportunity',
            title: 'Mentorship Opportunity',
            description: 'Emily\'s techniques could help improve team performance significantly.',
            impact: 'high',
            recommendation: 'Pair Emily with struggling reps for mentorship program.'
          }
        ]
      },
      {
        repId: 'rep-4',
        repName: 'David Kim',
        office: 'Los Angeles',
        role: 'setter',
        totalProjects: 8,
        projectsOnHold: 1,
        totalRevenue: 420000,
        revenueAtRisk: 45000,
        averageHoldDuration: 5.2,
        holdRate: 12.5,
        completionRate: 87.5,
        commissionEarned: 12600,
        performanceScore: 88,
        trends: {
          holdRate: 'stable',
          revenue: 'growing',
          completion: 'improving'
        },
        strengths: [
          'Strong lead qualification',
          'Efficient project setup',
          'Good customer onboarding'
        ],
        improvementAreas: [
          'Project volume scaling'
        ],
        aiInsights: [
          {
            type: 'growth_opportunity',
            title: 'Volume Scaling Potential',
            description: 'David has capacity to handle 25% more projects based on current performance.',
            impact: 'medium',
            recommendation: 'Increase lead allocation to David to maximize revenue potential.'
          }
        ]
      }
    ];

    // Filter by office if specified
    const filteredReps = office === 'all' 
      ? repPerformance 
      : repPerformance.filter(rep => rep.office === office);

    return NextResponse.json(filteredReps);
  } catch (error: any) {
    console.error('[API] /api/analytics/rep-performance error:', error);
    logAIError('getRepPerformance', 'unknown', error);
    return NextResponse.json({
      error: error.message || 'Internal server error' 
    }, { status: 500 });
  }
}