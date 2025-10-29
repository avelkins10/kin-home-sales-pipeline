'use client';

import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Avatar, AvatarFallback } from '@/components/ui/avatar';
import { Trophy, Star, Award } from 'lucide-react';
import { CrewLeaderboard } from '@/lib/types/operations';
import { cn } from '@/lib/utils';

interface CrewTopPerformersCardProps {
  leaderboard: CrewLeaderboard;
  onCrewClick?: (crewName: string) => void;
}

export function CrewTopPerformersCard({ 
  leaderboard, 
  onCrewClick 
}: CrewTopPerformersCardProps) {
  const { top_performers } = leaderboard;

  const getRankBadge = (index: number) => {
    if (index === 0) {
      return (
        <Badge className="bg-gradient-to-r from-yellow-400 to-yellow-600 text-white border-0">
          <Trophy className="h-3 w-3 mr-1" />
          1st
        </Badge>
      );
    } else if (index === 1) {
      return (
        <Badge className="bg-gradient-to-r from-gray-300 to-gray-400 text-white border-0">
          <Award className="h-3 w-3 mr-1" />
          2nd
        </Badge>
      );
    } else if (index === 2) {
      return (
        <Badge className="bg-gradient-to-r from-orange-400 to-orange-600 text-white border-0">
          <Star className="h-3 w-3 mr-1" />
          3rd
        </Badge>
      );
    } else {
      return (
        <Badge variant="outline" className="bg-gray-50 text-gray-700">
          {index + 1}th
        </Badge>
      );
    }
  };

  const getInitials = (name: string): string => {
    return name
      .split(' ')
      .map(n => n[0])
      .join('')
      .toUpperCase()
      .slice(0, 2);
  };

  if (!top_performers || top_performers.length === 0) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Trophy className="h-5 w-5 text-gray-400" />
            Top Performers
          </CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-center py-8 text-gray-500">
            <Trophy className="h-8 w-8 mx-auto mb-2 text-gray-300" />
            <p>No performance data available</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  return (
    <Card className="shadow-sm hover:shadow-md transition-shadow">
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <Trophy className="h-5 w-5 text-yellow-600" />
          Top Performers
        </CardTitle>
        <p className="text-sm text-gray-600">
          Leading crew members this period
        </p>
      </CardHeader>
      <CardContent className="space-y-4">
        {top_performers.map((performer, index) => {
          const bgClass = index === 0 
            ? 'bg-gradient-to-r from-yellow-50 to-yellow-100' 
            : index === 1 
            ? 'bg-gradient-to-r from-gray-50 to-gray-100'
            : index === 2
            ? 'bg-gradient-to-r from-orange-50 to-orange-100'
            : 'bg-gray-50';

          return (
            <div
              key={performer.crew_name}
              className={cn(
                "flex items-center gap-3 p-4 rounded-lg transition-all",
                bgClass,
                onCrewClick && "cursor-pointer hover:shadow-md"
              )}
              onClick={() => onCrewClick?.(performer.crew_name)}
            >
              <div className="flex-shrink-0">
                {getRankBadge(index)}
              </div>
              
              <Avatar className="h-10 w-10">
                <AvatarFallback className={cn(
                  "font-semibold",
                  index === 0 ? "bg-yellow-200 text-yellow-800" :
                  index === 1 ? "bg-gray-200 text-gray-800" :
                  index === 2 ? "bg-orange-200 text-orange-800" :
                  "bg-blue-200 text-blue-800"
                )}>
                  {getInitials(performer.crew_name)}
                </AvatarFallback>
              </Avatar>

              <div className="flex-1 min-w-0">
                <div className="font-semibold text-gray-900 truncate">
                  {performer.crew_name}
                </div>
                <div className="text-sm text-gray-600">
                  {performer.metric_name}: <span className="font-medium">{performer.metric_value}</span>
                </div>
              </div>

              {index < 3 && (
                <div className="flex-shrink-0">
                  {index === 0 && <Trophy className="h-6 w-6 text-yellow-600" />}
                  {index === 1 && <Award className="h-6 w-6 text-gray-600" />}
                  {index === 2 && <Star className="h-6 w-6 text-orange-600" />}
                </div>
              )}
            </div>
          );
        })}
      </CardContent>
    </Card>
  );
}

