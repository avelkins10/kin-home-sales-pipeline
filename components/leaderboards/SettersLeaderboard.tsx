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
import { Trophy, CheckCircle2, XCircle } from 'lucide-react';
import { useRouter } from 'next/navigation';

interface SetterData {
  userId: string;
  name: string;
  role: string;
  team: string;
  appointmentsSet: number;
  within48hCount: number;
  withPowerBillCount: number;
  highQualityCount: number;
  lowQualityCount: number;
  doorsKnocked: number;
  estimatedHoursOnDoors: number;
  conversionRate: number;
}

interface SettersLeaderboardProps {
  data: SetterData[];
  isLoading: boolean;
  dateRange: string;
}

export function SettersLeaderboard({ data, isLoading, dateRange }: SettersLeaderboardProps) {
  const router = useRouter();

  if (isLoading) {
    return (
      <Card>
        <CardHeader>
          <CardTitle>Setters Leaderboard</CardTitle>
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

  return (
    <Card>
      <CardHeader>
        <CardTitle>Setters Leaderboard - {dateRange}</CardTitle>
      </CardHeader>
      <CardContent>
        {data.length === 0 ? (
          <div className="text-center py-8 text-muted-foreground">
            No setters found for this date range
          </div>
        ) : (
          <div className="rounded-md border">
            <Table>
              <TableHeader>
                <TableRow>
                  <TableHead className="w-12">Rank</TableHead>
                  <TableHead>Name</TableHead>
                  <TableHead>Team</TableHead>
                  <TableHead className="text-right">Doors</TableHead>
                  <TableHead className="text-right">Hours</TableHead>
                  <TableHead className="text-right">Appointments</TableHead>
                  <TableHead className="text-right">48h Speed</TableHead>
                  <TableHead className="text-right">Power Bill</TableHead>
                  <TableHead className="text-right">High Quality</TableHead>
                  <TableHead className="text-right">Low Quality</TableHead>
                  <TableHead className="text-right">Conversion</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {data.map((setter, index) => {
                  const rank = index + 1;
                  const medal = rank === 1 ? '🥇' : rank === 2 ? '🥈' : rank === 3 ? '🥉' : null;
                  
                  return (
                    <TableRow
                      key={setter.userId}
                      className="cursor-pointer hover:bg-gray-50"
                      onClick={() => handleRowClick(setter.userId)}
                    >
                      <TableCell>
                        <div className="flex items-center gap-2">
                          {medal && <span className="text-xl">{medal}</span>}
                          <span className="font-semibold">#{rank}</span>
                        </div>
                      </TableCell>
                      <TableCell className="font-medium">{setter.name}</TableCell>
                      <TableCell>
                        <Badge variant="outline">{setter.team}</Badge>
                      </TableCell>
                      <TableCell className="text-right">{setter.doorsKnocked}</TableCell>
                      <TableCell className="text-right">{setter.estimatedHoursOnDoors}h</TableCell>
                      <TableCell className="text-right font-semibold">
                        {setter.appointmentsSet}
                      </TableCell>
                      <TableCell className="text-right">
                        <div className="flex items-center justify-end gap-1">
                          {setter.within48hCount > 0 ? (
                            <CheckCircle2 className="w-4 h-4 text-green-600" />
                          ) : (
                            <XCircle className="w-4 h-4 text-gray-400" />
                          )}
                          <span>{setter.within48hCount}</span>
                        </div>
                      </TableCell>
                      <TableCell className="text-right">
                        <div className="flex items-center justify-end gap-1">
                          {setter.withPowerBillCount > 0 ? (
                            <CheckCircle2 className="w-4 h-4 text-green-600" />
                          ) : (
                            <XCircle className="w-4 h-4 text-gray-400" />
                          )}
                          <span>{setter.withPowerBillCount}</span>
                        </div>
                      </TableCell>
                      <TableCell className="text-right">
                        <Badge variant="default" className="bg-green-500">
                          {setter.highQualityCount}
                        </Badge>
                      </TableCell>
                      <TableCell className="text-right">
                        <Badge variant="destructive">
                          {setter.lowQualityCount}
                        </Badge>
                      </TableCell>
                      <TableCell className="text-right">
                        {setter.conversionRate.toFixed(1)}%
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
