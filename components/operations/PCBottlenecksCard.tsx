'use client';

import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { PCBottleneck } from '@/lib/types/operations';
import { CheckCircle, AlertTriangle, AlertCircle, Clock, Eye } from 'lucide-react';
import { cn } from '@/lib/utils';

interface PCBottlenecksCardProps {
  bottlenecks: PCBottleneck[];
  onViewProjects?: (stageName: string) => void;
}

const getSeverityColor = (severity: string) => {
  switch (severity) {
    case 'critical':
      return 'border-red-500 bg-red-50';
    case 'high':
      return 'border-orange-500 bg-orange-50';
    case 'normal':
      return 'border-yellow-500 bg-yellow-50';
    default:
      return 'border-gray-500 bg-gray-50';
  }
};

const getSeverityBadgeColor = (severity: string) => {
  switch (severity) {
    case 'critical':
      return 'bg-red-100 text-red-800 border-red-200';
    case 'high':
      return 'bg-orange-100 text-orange-800 border-orange-200';
    case 'normal':
      return 'bg-yellow-100 text-yellow-800 border-yellow-200';
    default:
      return 'bg-gray-100 text-gray-800 border-gray-200';
  }
};

const getSeverityIcon = (severity: string) => {
  switch (severity) {
    case 'critical':
      return AlertTriangle;
    case 'high':
      return AlertCircle;
    case 'normal':
      return Clock;
    default:
      return Clock;
  }
};

export function PCBottlenecksCard({ 
  bottlenecks, 
  onViewProjects 
}: PCBottlenecksCardProps) {
  if (!bottlenecks || bottlenecks.length === 0) {
    return (
      <Card className="h-64 mobile:h-72 ipad:h-80">
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <CheckCircle className="h-5 w-5 text-green-600" />
            Project Bottlenecks
          </CardTitle>
        </CardHeader>
        <CardContent className="flex items-center justify-center h-full">
          <div className="text-center text-gray-500">
            <CheckCircle className="h-8 w-8 mx-auto mb-2 text-green-300" />
            <p className="font-medium text-green-600">No bottlenecks detected</p>
            <p className="text-sm">All stages are flowing smoothly!</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  // Sort bottlenecks by severity and avg days
  const sortedBottlenecks = [...bottlenecks].sort((a, b) => {
    const severityOrder = { critical: 3, high: 2, normal: 1 };
    const aSeverity = severityOrder[a.severity as keyof typeof severityOrder] || 0;
    const bSeverity = severityOrder[b.severity as keyof typeof severityOrder] || 0;
    
    if (aSeverity !== bSeverity) {
      return bSeverity - aSeverity;
    }
    
    return b.avgDaysInStage - a.avgDaysInStage;
  });

  return (
    <Card className="shadow-sm hover:shadow-md transition-shadow">
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <AlertTriangle className="h-5 w-5 text-orange-600" />
          Project Bottlenecks
        </CardTitle>
        <p className="text-sm text-gray-600">
          Stages with delays requiring attention
        </p>
      </CardHeader>
      <CardContent className="space-y-3">
        {sortedBottlenecks.map((bottleneck, index) => {
          const SeverityIcon = getSeverityIcon(bottleneck.severity);
          const borderColor = getSeverityColor(bottleneck.severity);
          const badgeColor = getSeverityBadgeColor(bottleneck.severity);
          
          return (
            <div 
              key={index}
              className={cn(
                "border-l-4 p-4 rounded-r-lg hover:bg-gray-50 transition-colors",
                borderColor
              )}
            >
              <div className="flex items-start justify-between">
                <div className="flex-1 space-y-2">
                  <div className="flex items-center gap-2">
                    <SeverityIcon className="h-4 w-4 text-gray-600" />
                    <h4 className="font-semibold text-gray-900">{bottleneck.stageName}</h4>
                    <Badge className={badgeColor}>
                      {bottleneck.severity.toUpperCase()}
                    </Badge>
                  </div>
                  
                  <div className="grid grid-cols-2 gap-2 text-sm text-gray-600">
                    <div>
                      <span className="font-medium">{bottleneck.projectCount}</span> projects stuck
                    </div>
                    <div>
                      Average: <span className="font-medium">{bottleneck.avgDaysInStage.toFixed(1)}</span> days
                    </div>
                  </div>
                  
                  {bottleneck.longestProject && (
                    <div className="text-xs text-gray-500">
                      Longest: {bottleneck.longestProject.customerName} ({bottleneck.longestProject.daysInStage} days)
                    </div>
                  )}
                </div>
                
                {onViewProjects && (
                  <Button
                    variant="outline"
                    size="sm"
                    onClick={() => onViewProjects(bottleneck.stageName)}
                    className="ml-4"
                  >
                    <Eye className="h-4 w-4 mr-1" />
                    View Projects
                  </Button>
                )}
              </div>
            </div>
          );
        })}
      </CardContent>
    </Card>
  );
}
