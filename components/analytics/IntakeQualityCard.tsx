'use client';

import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { CheckCircle, XCircle, RefreshCw, Clock } from 'lucide-react';
import { formatPercentage } from '@/lib/utils/formatters';

interface IntakeQualityCardProps {
  repName?: string;
  totalProjects: number;
  firstTimePassRate: number;
  rejectionRate: number;
  resubmitSuccessRate: number;
  avgResolutionTime: number | null;
  topRejectionReasons: Array<{ reason: string; count: number }>;
}

export function IntakeQualityCard({
  repName,
  totalProjects,
  firstTimePassRate,
  rejectionRate,
  resubmitSuccessRate,
  avgResolutionTime,
  topRejectionReasons,
}: IntakeQualityCardProps) {
  return (
    <Card>
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <CheckCircle className="h-5 w-5 text-emerald-600" />
          Intake Quality Metrics
          {repName && <span className="text-sm font-normal text-gray-500">• {repName}</span>}
        </CardTitle>
        <p className="text-sm text-gray-600">
          {totalProjects} total project{totalProjects !== 1 ? 's' : ''} analyzed
        </p>
      </CardHeader>
      <CardContent>
        <div className="space-y-4">
          {/* First-Time Pass Rate - PRIMARY METRIC */}
          <div className="flex items-center justify-between p-4 bg-gradient-to-r from-emerald-50 to-green-50 rounded-lg border border-emerald-200">
            <div className="flex items-center gap-3">
              <div className="p-2 bg-white rounded-lg">
                <CheckCircle className="h-6 w-6 text-emerald-600" />
              </div>
              <div>
                <p className="text-sm font-medium text-gray-700">First-Time Pass Rate</p>
                <p className="text-xs text-gray-500">Clean deals approved on first attempt</p>
              </div>
            </div>
            <span className={`text-3xl font-bold ${
              firstTimePassRate >= 90 ? 'text-green-600' :
              firstTimePassRate >= 75 ? 'text-yellow-600' :
              'text-red-600'
            }`}>
              {formatPercentage(firstTimePassRate)}
            </span>
          </div>

          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            {/* Rejection Rate */}
            <div className="flex items-center justify-between p-3 bg-red-50 rounded-lg border border-red-200">
              <div className="flex items-center gap-2">
                <XCircle className="h-5 w-5 text-red-600" />
                <div>
                  <p className="text-sm font-medium text-gray-700">Rejection Rate</p>
                  <p className="text-xs text-gray-500">Rejected on first attempt</p>
                </div>
              </div>
              <span className="text-xl font-semibold text-red-600">
                {formatPercentage(rejectionRate)}
              </span>
            </div>

            {/* Resubmit Success Rate */}
            {rejectionRate > 0 && (
              <div className="flex items-center justify-between p-3 bg-blue-50 rounded-lg border border-blue-200">
                <div className="flex items-center gap-2">
                  <RefreshCw className="h-5 w-5 text-blue-600" />
                  <div>
                    <p className="text-sm font-medium text-gray-700">Resubmit Success</p>
                    <p className="text-xs text-gray-500">Eventually approved</p>
                  </div>
                </div>
                <span className="text-xl font-semibold text-blue-600">
                  {formatPercentage(resubmitSuccessRate)}
                </span>
              </div>
            )}

            {/* Average Resolution Time */}
            {avgResolutionTime !== null && avgResolutionTime > 0 && (
              <div className="flex items-center justify-between p-3 bg-amber-50 rounded-lg border border-amber-200">
                <div className="flex items-center gap-2">
                  <Clock className="h-5 w-5 text-amber-600" />
                  <div>
                    <p className="text-sm font-medium text-gray-700">Avg Resolution Time</p>
                    <p className="text-xs text-gray-500">Days to fix rejections</p>
                  </div>
                </div>
                <span className="text-xl font-semibold text-amber-600">
                  {Math.round(avgResolutionTime)} days
                </span>
              </div>
            )}
          </div>

          {/* Top Rejection Reasons */}
          {topRejectionReasons.length > 0 && (
            <div className="mt-6">
              <h4 className="text-sm font-semibold text-gray-700 mb-3 flex items-center gap-2">
                <XCircle className="h-4 w-4 text-red-600" />
                Top Rejection Reasons
              </h4>
              <div className="space-y-2">
                {topRejectionReasons.map((item, idx) => (
                  <div
                    key={idx}
                    className="flex items-center justify-between p-2 bg-gray-50 rounded-lg hover:bg-gray-100 transition-colors"
                  >
                    <div className="flex items-center gap-2 flex-1">
                      <span className="text-xs font-semibold text-gray-500 w-6">
                        #{idx + 1}
                      </span>
                      <span className="text-sm text-gray-700">{item.reason}</span>
                    </div>
                    <Badge variant="secondary" className="ml-2">
                      {item.count} {item.count === 1 ? 'project' : 'projects'}
                    </Badge>
                  </div>
                ))}
              </div>
            </div>
          )}

          {/* Performance Indicator */}
          <div className="mt-6 p-4 bg-slate-50 rounded-lg border border-slate-200">
            <h4 className="text-sm font-semibold text-gray-700 mb-2">Performance Assessment</h4>
            <div className="space-y-2">
              {firstTimePassRate >= 90 && (
                <p className="text-sm text-green-600 flex items-center gap-2">
                  <CheckCircle className="h-4 w-4" />
                  <span className="font-medium">Excellent!</span> Consistently submitting clean deals
                </p>
              )}
              {firstTimePassRate >= 75 && firstTimePassRate < 90 && (
                <p className="text-sm text-yellow-600 flex items-center gap-2">
                  <CheckCircle className="h-4 w-4" />
                  <span className="font-medium">Good</span> Room for improvement in deal quality
                </p>
              )}
              {firstTimePassRate < 75 && (
                <p className="text-sm text-red-600 flex items-center gap-2">
                  <XCircle className="h-4 w-4" />
                  <span className="font-medium">Needs Attention</span> High rejection rate - coaching recommended
                </p>
              )}
              {topRejectionReasons.length > 0 && (
                <p className="text-xs text-gray-600 mt-2">
                  Focus on: <span className="font-medium">{topRejectionReasons[0].reason}</span>
                </p>
              )}
            </div>
          </div>
        </div>
      </CardContent>
    </Card>
  );
}
