'use client';

import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from '@/components/ui/table';
import { Badge } from '@/components/ui/badge';
import { Skeleton } from '@/components/ui/skeleton';
import { Trophy } from 'lucide-react';
import { useRouter } from 'next/navigation';

interface CloserData {
  userId: string;
  name: string;
  role: string;
  team: string;
  appointmentsRun: number;
  satClosed: number;
  satNoClose: number;
  noShow: number;
  cancelled: number;
  closeRate: number;
}

interface ClosersLeaderboardProps {
  data: CloserData[];
  isLoading: boolean;
  dateRange: string;
}

export function ClosersLeaderboard({ data, isLoading, dateRange }: ClosersLeaderboardProps) {
  const router = useRouter();

  if (isLoading) {
    return (
      <Card>
        <CardHeader>
          <CardTitle>Closers Leaderboard</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="space-y-2">
            {[...Array(5)].map((_, i) => (
              <Skeleton key={i} className="h-12 w-full" />
            ))}
          </div>
        </CardContent>
      </Card>
    );
  }

  const handleRowClick = (userId: string) => {
    router.push(`/analytics/rep/${userId}`);
  };

  // Sort by satClosed (sales closed) descending
  const sortedData = [...data].sort((a, b) => b.satClosed - a.satClosed);

  return (
    <Card>
      <CardHeader>
        <CardTitle>Closers Leaderboard - {dateRange}</CardTitle>
      </CardHeader>
      <CardContent>
        {sortedData.length === 0 ? (
          <div className="text-center py-8 text-muted-foreground">
            No closers found for this date range
          </div>
        ) : (
          <div className="rounded-md border">
            <Table>
              <TableHeader>
                <TableRow>
                  <TableHead className="w-12">Rank</TableHead>
                  <TableHead>Name</TableHead>
                  <TableHead>Team</TableHead>
                  <TableHead className="text-right">Appointments Run</TableHead>
                  <TableHead className="text-right">Sat Closed</TableHead>
                  <TableHead className="text-right">Sat No Close</TableHead>
                  <TableHead className="text-right">No Show</TableHead>
                  <TableHead className="text-right">Cancelled</TableHead>
                  <TableHead className="text-right">Close Rate</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {sortedData.map((closer, index) => {
                  const rank = index + 1;
                  const medal = rank === 1 ? '🥇' : rank === 2 ? '🥈' : rank === 3 ? '🥉' : null;
                  
                  return (
                    <TableRow
                      key={closer.userId}
                      className="cursor-pointer hover:bg-gray-50"
                      onClick={() => handleRowClick(closer.userId)}
                    >
                      <TableCell>
                        <div className="flex items-center gap-2">
                          {medal && <span className="text-xl">{medal}</span>}
                          <span className="font-semibold">#{rank}</span>
                        </div>
                      </TableCell>
                      <TableCell className="font-medium">{closer.name}</TableCell>
                      <TableCell>
                        <Badge variant="outline">{closer.team}</Badge>
                      </TableCell>
                      <TableCell className="text-right">{closer.appointmentsRun}</TableCell>
                      <TableCell className="text-right font-semibold text-green-600">
                        {closer.satClosed}
                      </TableCell>
                      <TableCell className="text-right">{closer.satNoClose}</TableCell>
                      <TableCell className="text-right text-yellow-600">{closer.noShow}</TableCell>
                      <TableCell className="text-right text-red-600">{closer.cancelled}</TableCell>
                      <TableCell className="text-right">
                        <Badge 
                          variant={closer.closeRate >= 50 ? 'default' : closer.closeRate >= 30 ? 'secondary' : 'destructive'}
                          className={closer.closeRate >= 50 ? 'bg-green-500' : ''}
                        >
                          {closer.closeRate.toFixed(1)}%
                        </Badge>
                      </TableCell>
                    </TableRow>
                  );
                })}
              </TableBody>
            </Table>
          </div>
        )}
      </CardContent>
    </Card>
  );
}
