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
import { Building2 } from 'lucide-react';

interface OfficeData {
  officeId: number;
  officeName: string;
  doorsKnocked: number;
  appointmentsSet: number;
  salesClosed: number;
  settersCount: number;
  closersCount: number;
  totalReps: number;
  conversionRate: number;
  closeRate: number;
}

interface OfficesLeaderboardProps {
  data: OfficeData[];
  isLoading: boolean;
  dateRange: string;
}

export function OfficesLeaderboard({ data, isLoading, dateRange }: OfficesLeaderboardProps) {
  if (isLoading) {
    return (
      <Card>
        <CardHeader>
          <CardTitle>Offices Leaderboard</CardTitle>
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

  // Sort by appointments set descending
  const sortedData = [...data].sort((a, b) => b.appointmentsSet - a.appointmentsSet);

  return (
    <Card>
      <CardHeader>
        <CardTitle>Offices Leaderboard - {dateRange}</CardTitle>
      </CardHeader>
      <CardContent>
        {sortedData.length === 0 ? (
          <div className="text-center py-8 text-muted-foreground">
            No offices found for this date range
          </div>
        ) : (
          <div className="rounded-md border">
            <Table>
              <TableHeader>
                <TableRow>
                  <TableHead>Office</TableHead>
                  <TableHead className="text-right">Doors</TableHead>
                  <TableHead className="text-right">Appointments</TableHead>
                  <TableHead className="text-right">Sales</TableHead>
                  <TableHead className="text-right">Conversion</TableHead>
                  <TableHead className="text-right">Close Rate</TableHead>
                  <TableHead className="text-right">Setters</TableHead>
                  <TableHead className="text-right">Closers</TableHead>
                  <TableHead className="text-right">Total Reps</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {sortedData.map((office) => (
                  <TableRow key={office.officeId}>
                    <TableCell className="font-medium">{office.officeName}</TableCell>
                    <TableCell className="text-right">{office.doorsKnocked}</TableCell>
                    <TableCell className="text-right font-semibold">
                      {office.appointmentsSet}
                    </TableCell>
                    <TableCell className="text-right font-semibold text-green-600">
                      {office.salesClosed}
                    </TableCell>
                    <TableCell className="text-right">
                      <Badge variant="secondary">
                        {office.conversionRate.toFixed(1)}%
                      </Badge>
                    </TableCell>
                    <TableCell className="text-right">
                      <Badge 
                        variant={office.closeRate >= 50 ? 'default' : office.closeRate >= 30 ? 'secondary' : 'destructive'}
                        className={office.closeRate >= 50 ? 'bg-green-500' : ''}
                      >
                        {office.closeRate.toFixed(1)}%
                      </Badge>
                    </TableCell>
                    <TableCell className="text-right">{office.settersCount}</TableCell>
                    <TableCell className="text-right">{office.closersCount}</TableCell>
                    <TableCell className="text-right">{office.totalReps}</TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          </div>
        )}
      </CardContent>
    </Card>
  );
}
