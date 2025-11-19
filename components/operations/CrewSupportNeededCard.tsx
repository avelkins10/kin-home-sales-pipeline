'use client';

import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Avatar, AvatarFallback } from '@/components/ui/avatar';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { AlertTriangle, TrendingDown, XCircle, HelpCircle, CheckCircle } from 'lucide-react';
import { CrewLeaderboard } from '@/lib/types/operations';
import { cn } from '@/lib/utils';

interface CrewSupportNeededCardProps {
  leaderboard: CrewLeaderboard;
  onCrewClick?: (crewName: string) => void;
}

export function CrewSupportNeededCard({ 
  leaderboard, 
  onCrewClick 
}: CrewSupportNeededCardProps) {
  const { needs_support } = leaderboard;

  const getInitials = (name: string): string => {
    return name
      .split(' ')
      .map(n => n[0])
      .join('')
      .toUpperCase()
      .slice(0, 2);
  };

  const getSeverityIcon = (issue: string) => {
    if (issue.includes('Low on-time')) {
      return <TrendingDown className="h-4 w-4" />;
    } else if (issue.includes('exception')) {
      return <XCircle className="h-4 w-4" />;
    } else if (issue.includes('rating')) {
      return <AlertTriangle className="h-4 w-4" />;
    } else {
      return <HelpCircle className="h-4 w-4" />;
    }
  };

  const getSeverityColor = (issue: string) => {
    if (issue.includes('exception') || issue.includes('No tasks')) {
      return 'text-red-600 bg-red-50';
    } else if (issue.includes('rating')) {
      return 'text-orange-600 bg-orange-50';
    } else {
      return 'text-yellow-600 bg-yellow-50';
    }
  };

  if (!needs_support || needs_support.length === 0) {
    return (
      <Card className="shadow-sm hover:shadow-md transition-shadow">
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <CheckCircle className="h-5 w-5 text-green-600" />
            Needs Support
          </CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-center py-8">
            <CheckCircle className="h-12 w-12 mx-auto mb-3 text-green-600" />
            <p className="text-lg font-semibold text-green-700">All crew members performing well! 🎉</p>
            <p className="text-sm text-gray-600 mt-2">No performance issues detected</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  return (
    <Card className="shadow-sm hover:shadow-md transition-shadow border-orange-200">
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <AlertTriangle className="h-5 w-5 text-orange-600" />
          Needs Support
        </CardTitle>
        <p className="text-sm text-gray-600">
          Crew members requiring attention
        </p>
      </CardHeader>
      <CardContent className="space-y-3">
        {needs_support.map((crew, index) => (
          <Alert
            key={`${crew.crew_name}-${index}`}
            className={cn(
              "transition-all",
              getSeverityColor(crew.issue),
              onCrewClick && "cursor-pointer hover:shadow-md"
            )}
            onClick={() => onCrewClick?.(crew.crew_name)}
          >
            <div className="flex items-start gap-3">
              <div className={cn("flex-shrink-0 mt-0.5", getSeverityColor(crew.issue))}>
                {getSeverityIcon(crew.issue)}
              </div>

              <Avatar className="h-8 w-8 flex-shrink-0">
                <AvatarFallback className="bg-white text-gray-700 text-xs">
                  {getInitials(crew.crew_name)}
                </AvatarFallback>
              </Avatar>

              <div className="flex-1 min-w-0">
                <AlertDescription className="text-sm">
                  <div className="font-semibold text-gray-900 mb-1">
                    {crew.crew_name}
                  </div>
                  <div className="text-gray-700">
                    {crew.issue}
                  </div>
                  {crew.metric_value !== undefined && (
                    <div className="mt-2 inline-flex items-center gap-1">
                      <Badge variant="outline" className={cn("text-xs", getSeverityColor(crew.issue))}>
                        Value: {
                          crew.issue.includes('percentage') 
                            ? `${crew.metric_value.toFixed(1)}%`
                            : crew.issue.includes('rating')
                            ? `${crew.metric_value.toFixed(1)}/5`
                            : crew.metric_value
                        }
                      </Badge>
                    </div>
                  )}
                </AlertDescription>
              </div>
            </div>
          </Alert>
        ))}
      </CardContent>
    </Card>
  );
}







