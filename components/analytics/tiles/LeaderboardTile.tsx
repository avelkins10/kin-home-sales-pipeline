'use client';

import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Trophy, DoorOpen, Target, Award, ChevronRight } from 'lucide-react';
import { cn } from '@/lib/utils';

interface RepData {
  userId: number;
  name: string;
  role?: string;
  doorsKnocked?: number;
  appointmentsSet?: number;
  conversionRate?: number;
  appointmentsRun?: number;
  salesClosed?: number;
  closeRate?: number;
}

interface LeaderboardData {
  topDoors: RepData[];
  topConverters: RepData[];
  topClosers: RepData[];
}

interface Props {
  data: LeaderboardData;
  onExpand?: () => void;
  isLoading?: boolean;
}

export function LeaderboardTile({ data, onExpand, isLoading }: Props) {
  const getMedalColor = (rank: number) => {
    if (rank === 1) return 'bg-yellow-500 text-white';
    if (rank === 2) return 'bg-gray-400 text-white';
    if (rank === 3) return 'bg-orange-600 text-white';
    return 'bg-gray-200 text-gray-700';
  };

  if (isLoading) {
    return (
      <Card className="hover:shadow-lg transition-all cursor-pointer border-l-4 border-l-green-500">
        <CardHeader>
          <div className="flex items-center justify-between">
            <CardTitle className="flex items-center gap-2">
              <Trophy className="h-5 w-5 text-green-600" />
              Rep Leaderboards
            </CardTitle>
          </div>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-3 gap-4">
            {[1, 2, 3].map((i) => (
              <div key={i} className="space-y-2 animate-pulse">
                <div className="h-6 bg-gray-200 rounded"></div>
                <div className="h-32 bg-gray-200 rounded"></div>
              </div>
            ))}
          </div>
        </CardContent>
      </Card>
    );
  }

  return (
    <Card
      className="hover:shadow-xl transition-all cursor-pointer border-l-4 border-l-green-500 group"
      onClick={onExpand}
    >
      <CardHeader>
        <div className="flex items-center justify-between">
          <CardTitle className="flex items-center gap-2">
            <Trophy className="h-5 w-5 text-green-600" />
            Rep Leaderboards
          </CardTitle>
          <ChevronRight className="h-5 w-5 text-gray-400 group-hover:text-green-600 transition-colors" />
        </div>
        <p className="text-sm text-muted-foreground">
          Top performers across key metrics
        </p>
      </CardHeader>
      <CardContent>
        <div className="grid grid-cols-3 gap-4">
          {/* Top Doors Knocked */}
          <div className="space-y-3">
            <div className="flex items-center gap-2">
              <DoorOpen className="h-4 w-4 text-blue-600" />
              <h4 className="text-sm font-semibold text-gray-700">Top Doors</h4>
            </div>
            <div className="space-y-2">
              {data.topDoors.slice(0, 3).map((rep, idx) => (
                <div
                  key={rep.userId}
                  className="p-2 rounded-lg bg-gradient-to-br from-blue-50 to-blue-100/50 border border-blue-200"
                >
                  <div className="flex items-center gap-2 mb-1">
                    <div className={cn('w-6 h-6 rounded-full flex items-center justify-center text-xs font-bold', getMedalColor(idx + 1))}>
                      {idx + 1}
                    </div>
                    <span className="text-sm font-medium truncate flex-1">
                      {rep.name}
                    </span>
                  </div>
                  <div className="flex items-center justify-between ml-8">
                    <p className="text-2xl font-bold text-blue-900">
                      {rep.doorsKnocked?.toLocaleString() || 0}
                    </p>
                    <Badge variant="outline" className="text-xs">
                      {rep.conversionRate?.toFixed(1)}%
                    </Badge>
                  </div>
                </div>
              ))}
            </div>
            {data.topDoors.length > 3 && (
              <p className="text-xs text-center text-muted-foreground">
                +{data.topDoors.length - 3} more
              </p>
            )}
          </div>

          {/* Top Converters */}
          <div className="space-y-3">
            <div className="flex items-center gap-2">
              <Target className="h-4 w-4 text-green-600" />
              <h4 className="text-sm font-semibold text-gray-700">Top Converters</h4>
            </div>
            <div className="space-y-2">
              {data.topConverters.slice(0, 3).map((rep, idx) => (
                <div
                  key={rep.userId}
                  className="p-2 rounded-lg bg-gradient-to-br from-green-50 to-green-100/50 border border-green-200"
                >
                  <div className="flex items-center gap-2 mb-1">
                    <div className={cn('w-6 h-6 rounded-full flex items-center justify-center text-xs font-bold', getMedalColor(idx + 1))}>
                      {idx + 1}
                    </div>
                    <span className="text-sm font-medium truncate flex-1">
                      {rep.name}
                    </span>
                  </div>
                  <div className="flex items-center justify-between ml-8">
                    <p className="text-2xl font-bold text-green-900">
                      {rep.conversionRate?.toFixed(1)}%
                    </p>
                    <Badge variant="outline" className="text-xs">
                      {rep.doorsKnocked} doors
                    </Badge>
                  </div>
                </div>
              ))}
            </div>
            {data.topConverters.length > 3 && (
              <p className="text-xs text-center text-muted-foreground">
                +{data.topConverters.length - 3} more
              </p>
            )}
          </div>

          {/* Top Closers */}
          <div className="space-y-3">
            <div className="flex items-center gap-2">
              <Award className="h-4 w-4 text-purple-600" />
              <h4 className="text-sm font-semibold text-gray-700">Top Closers</h4>
            </div>
            <div className="space-y-2">
              {data.topClosers.slice(0, 3).map((rep, idx) => (
                <div
                  key={rep.userId}
                  className="p-2 rounded-lg bg-gradient-to-br from-purple-50 to-purple-100/50 border border-purple-200"
                >
                  <div className="flex items-center gap-2 mb-1">
                    <div className={cn('w-6 h-6 rounded-full flex items-center justify-center text-xs font-bold', getMedalColor(idx + 1))}>
                      {idx + 1}
                    </div>
                    <span className="text-sm font-medium truncate flex-1">
                      {rep.name}
                    </span>
                  </div>
                  <div className="flex items-center justify-between ml-8">
                    <p className="text-2xl font-bold text-purple-900">
                      {rep.salesClosed}
                    </p>
                    <Badge variant="outline" className="text-xs">
                      {rep.closeRate?.toFixed(1)}%
                    </Badge>
                  </div>
                </div>
              ))}
            </div>
            {data.topClosers.length > 3 && (
              <p className="text-xs text-center text-muted-foreground">
                +{data.topClosers.length - 3} more
              </p>
            )}
          </div>
        </div>
      </CardContent>
    </Card>
  );
}
