'use client';

import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Skeleton } from '@/components/ui/skeleton';
import { ChevronRight, Calendar, User } from 'lucide-react';
import type { PCProjectPipelineStage } from '@/lib/types/operations';

interface PCProjectPipelineProps {
  pipeline: PCProjectPipelineStage[];
}

function PCProjectPipelineSkeleton() {
  return (
    <Card>
      <CardHeader>
        <CardTitle>Project Pipeline</CardTitle>
      </CardHeader>
      <CardContent>
        <div className="overflow-x-auto">
          <div className="flex space-x-4 min-w-max">
            {Array.from({ length: 7 }).map((_, i) => (
              <div key={i} className="flex-shrink-0 w-48">
                <div className="border rounded-lg p-4">
                  <div className="flex items-center justify-between mb-3">
                    <Skeleton className="h-5 w-20" />
                    <Skeleton className="h-6 w-8 rounded-full" />
                  </div>
                  <div className="space-y-2">
                    {Array.from({ length: 3 }).map((_, j) => (
                      <div key={j} className="p-2 border rounded">
                        <Skeleton className="h-4 w-24 mb-1" />
                        <Skeleton className="h-3 w-16" />
                      </div>
                    ))}
                  </div>
                </div>
              </div>
            ))}
          </div>
        </div>
      </CardContent>
    </Card>
  );
}

export function PCProjectPipeline({ pipeline }: PCProjectPipelineProps) {
  // Helper to extract value from QuickBase wrapped objects
  const extractValue = (field: any): string => {
    if (typeof field === 'object' && field?.value) {
      return String(field.value);
    }
    return String(field || '');
  };

  const getStageColor = (stageName: string) => {
    switch (stageName) {
      case 'Intake':
        return 'bg-blue-100 text-blue-800 border-blue-200';
      case 'Survey':
      case 'Design':
        return 'bg-purple-100 text-purple-800 border-purple-200';
      case 'Permit':
        return 'bg-yellow-100 text-yellow-800 border-yellow-200';
      case 'NEM':
        return 'bg-orange-100 text-orange-800 border-orange-200';
      case 'Install':
        return 'bg-green-100 text-green-800 border-green-200';
      case 'PTO':
        return 'bg-teal-100 text-teal-800 border-teal-200';
      default:
        return 'bg-gray-100 text-gray-800 border-gray-200';
    }
  };

  const getStageIcon = (stageName: string) => {
    // For now, we'll use a generic icon, but this could be customized per stage
    return <Calendar className="h-4 w-4 text-gray-500" />;
  };

  if (pipeline.length === 0) {
    return (
      <Card>
        <CardHeader>
          <CardTitle>Project Pipeline</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-center py-8 text-gray-500">
            <Calendar className="h-12 w-12 mx-auto mb-4 text-gray-400" />
            <p>No active projects in pipeline</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  return (
    <Card>
      <CardHeader>
        <CardTitle>Project Pipeline</CardTitle>
      </CardHeader>
      <CardContent>
        <div className="overflow-x-auto">
          <div className="flex space-x-4 min-w-max pb-4">
            {pipeline.map((stage, index) => {
              const showConnector = index < pipeline.length - 1;
              
              return (
                <div key={stage.stageName} className="flex-shrink-0 w-48">
                  <div className="border rounded-lg p-4 bg-white">
                    <div className="flex items-center justify-between mb-3">
                      <div className="flex items-center space-x-2">
                        {getStageIcon(stage.stageName)}
                        <h3 className="font-medium text-gray-900">{extractValue(stage.stageName)}</h3>
                      </div>
                      <Badge
                        variant="outline"
                        className={`${getStageColor(extractValue(stage.stageName))} font-semibold`}
                      >
                        {stage.projectCount}
                      </Badge>
                    </div>
                    
                    <div className="space-y-2">
                      {stage.projects.slice(0, 5).map((project, projectIndex) => (
                        <div 
                          key={`${project.projectId}-${projectIndex}`}
                          className="p-2 border rounded bg-gray-50 hover:bg-gray-100 transition-colors cursor-pointer"
                        >
                          <div className="flex items-center space-x-2 mb-1">
                            <User className="h-3 w-3 text-gray-500" />
                            <span className="text-sm font-medium text-gray-900 truncate">
                              {extractValue(project.customerName)}
                            </span>
                          </div>
                          <div className="flex items-center justify-between text-xs text-gray-500">
                            <span>#{project.projectId}</span>
                            <span>{project.daysInStage} days</span>
                          </div>
                        </div>
                      ))}
                      
                      {stage.projects.length > 5 && (
                        <div className="text-center py-2">
                          <span className="text-xs text-gray-500">
                            +{stage.projects.length - 5} more
                          </span>
                        </div>
                      )}
                    </div>
                  </div>
                  
                  {showConnector && (
                    <div className="flex items-center justify-center mt-4">
                      <ChevronRight className="h-5 w-5 text-gray-400" />
                    </div>
                  )}
                </div>
              );
            })}
          </div>
        </div>
      </CardContent>
    </Card>
  );
}

export { PCProjectPipelineSkeleton };
