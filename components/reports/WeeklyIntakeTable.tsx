'use client';

import { useState } from 'react';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Table, TableHeader, TableBody, TableRow, TableHead, TableCell } from '@/components/ui/table';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { FileDown, ChevronDown, ChevronUp } from 'lucide-react';
import { formatPercentage } from '@/lib/utils/formatters';
import { exportAnalyticsToCSV } from '@/lib/utils/csv-export';
import type { WeeklyIntakeCloserReport } from '@/lib/types/reports';
import { toast } from 'sonner';

interface WeeklyIntakeTableProps {
  closers: WeeklyIntakeCloserReport[];
  startDate: string;
  endDate: string;
}

export function WeeklyIntakeTable({ closers, startDate, endDate }: WeeklyIntakeTableProps) {
  const [expandedRows, setExpandedRows] = useState<Set<string>>(new Set());
  const [isExporting, setIsExporting] = useState(false);

  const toggleRow = (closerEmail: string) => {
    const newExpanded = new Set(expandedRows);
    if (newExpanded.has(closerEmail)) {
      newExpanded.delete(closerEmail);
    } else {
      newExpanded.add(closerEmail);
    }
    setExpandedRows(newExpanded);
  };

  const handleExport = () => {
    setIsExporting(true);
    try {
      const exportData = closers.map((closer, index) => ({
        rank: index + 1,
        closerName: closer.closerName,
        office: closer.officeName || 'N/A',
        totalSubmitted: closer.totalSubmitted,
        firstTimeApproved: closer.firstTimeApproved,
        firstTimeRejected: closer.firstTimeRejected,
        pendingReview: closer.pendingReview,
        resubmittedAndApproved: closer.resubmittedAndApproved,
        firstTimePassRate: closer.firstTimePassRate,
        rejectionRate: closer.rejectionRate,
        avgResolutionTime: closer.avgResolutionTime || 'N/A',
        topRejectionReasons: closer.topRejectionReasons.map(r => r.reason).join('; '),
      }));

      const headers = {
        rank: 'Rank',
        closerName: 'Closer',
        office: 'Office',
        totalSubmitted: 'Total Submitted',
        firstTimeApproved: '1st Time Approved',
        firstTimeRejected: '1st Time Rejected',
        pendingReview: 'Pending Review',
        resubmittedAndApproved: 'Resubmitted & Approved',
        firstTimePassRate: 'First-Time Pass Rate (%)',
        rejectionRate: 'Rejection Rate (%)',
        avgResolutionTime: 'Avg Resolution (days)',
        topRejectionReasons: 'Top Rejection Reasons',
      };

      const filename = `weekly-intake-report-${startDate}-to-${endDate}.csv`;
      exportAnalyticsToCSV(exportData, filename, headers);

      toast.success('Report exported successfully');
    } catch (error) {
      toast.error('Failed to export report');
      console.error('Export error:', error);
    } finally {
      setIsExporting(false);
    }
  };

  if (closers.length === 0) {
    return (
      <Card>
        <CardContent className="p-6">
          <div className="bg-slate-50 border border-slate-200 rounded-lg p-4">
            <p className="text-slate-600">No data found for selected date range</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  return (
    <Card>
      <CardHeader>
        <div className="flex items-center justify-between">
          <CardTitle>Closer Breakdown</CardTitle>
          <Button
            variant="outline"
            size="sm"
            onClick={handleExport}
            disabled={isExporting}
          >
            {isExporting ? (
              <>
                <FileDown className="h-4 w-4 mr-2 animate-pulse" />
                Exporting...
              </>
            ) : (
              <>
                <FileDown className="h-4 w-4 mr-2" />
                Export CSV
              </>
            )}
          </Button>
        </div>
        <p className="text-sm text-gray-600">
          {closers.length} closer{closers.length !== 1 ? 's' : ''} • Click row to see rejection reasons
        </p>
      </CardHeader>
      <CardContent>
        <div className="overflow-x-auto">
          <Table>
            <TableHeader>
              <TableRow>
                <TableHead className="w-12">#</TableHead>
                <TableHead className="min-w-40">Closer</TableHead>
                <TableHead className="text-right">Submitted</TableHead>
                <TableHead className="text-right">Approved</TableHead>
                <TableHead className="text-right">Rejected</TableHead>
                <TableHead className="text-right">Pending</TableHead>
                <TableHead className="text-right">Pass %</TableHead>
                <TableHead className="text-right">Avg Fix Time</TableHead>
                <TableHead className="w-12"></TableHead>
              </TableRow>
            </TableHeader>
            <TableBody>
              {closers.map((closer, index) => {
                const isExpanded = expandedRows.has(closer.closerEmail);
                const isTopPerformer = index === 0 && closer.totalSubmitted >= 3;
                const isBottomPerformer = index === closers.length - 1 && closers.length > 1 && closer.totalSubmitted >= 3;

                return (
                  <>
                    <TableRow
                      key={closer.closerEmail}
                      className={`
                        cursor-pointer hover:bg-slate-100 transition-colors
                        ${isTopPerformer ? 'bg-green-50 border-l-4 border-green-500' : ''}
                        ${isBottomPerformer ? 'bg-red-50 border-l-4 border-red-500' : ''}
                      `}
                      onClick={() => toggleRow(closer.closerEmail)}
                    >
                      <TableCell className="font-medium">
                        <span className="inline-flex items-center justify-center w-6 h-6 bg-blue-100 text-blue-700 rounded-full text-xs font-semibold">
                          {index + 1}
                        </span>
                      </TableCell>
                      <TableCell>
                        <div>
                          <div className="font-medium">{closer.closerName}</div>
                          {closer.officeName && (
                            <div className="text-xs text-gray-500">{closer.officeName}</div>
                          )}
                        </div>
                      </TableCell>
                      <TableCell className="text-right font-medium">
                        {closer.totalSubmitted}
                      </TableCell>
                      <TableCell className="text-right text-green-600">
                        {closer.firstTimeApproved}
                      </TableCell>
                      <TableCell className="text-right text-red-600">
                        {closer.firstTimeRejected}
                      </TableCell>
                      <TableCell className="text-right text-gray-500">
                        {closer.pendingReview}
                      </TableCell>
                      <TableCell className="text-right">
                        <span className={`font-semibold ${
                          closer.firstTimePassRate >= 80 ? 'text-green-600' :
                          closer.firstTimePassRate >= 60 ? 'text-yellow-600' :
                          'text-red-600'
                        }`}>
                          {formatPercentage(closer.firstTimePassRate)}
                        </span>
                      </TableCell>
                      <TableCell className="text-right text-sm">
                        {closer.avgResolutionTime ? `${closer.avgResolutionTime}d` : 'N/A'}
                      </TableCell>
                      <TableCell>
                        {closer.topRejectionReasons.length > 0 && (
                          isExpanded ? <ChevronUp className="h-4 w-4" /> : <ChevronDown className="h-4 w-4" />
                        )}
                      </TableCell>
                    </TableRow>

                    {/* Expanded row showing rejection reasons */}
                    {isExpanded && closer.topRejectionReasons.length > 0 && (
                      <TableRow key={`${closer.closerEmail}-details`} className="bg-slate-50">
                        <TableCell colSpan={9} className="py-4">
                          <div className="pl-8">
                            <h4 className="text-sm font-semibold text-gray-700 mb-2">
                              Top Rejection Reasons:
                            </h4>
                            <div className="space-y-1">
                              {closer.topRejectionReasons.map((item, idx) => (
                                <div
                                  key={idx}
                                  className="flex items-center justify-between text-sm"
                                >
                                  <span className="text-gray-600">
                                    {idx + 1}. {item.reason}
                                  </span>
                                  <Badge variant="secondary">
                                    {item.count} {item.count === 1 ? 'project' : 'projects'}
                                  </Badge>
                                </div>
                              ))}
                            </div>
                            {closer.resubmittedAndApproved > 0 && (
                              <p className="text-xs text-gray-500 mt-2">
                                {closer.resubmittedAndApproved} rejected {closer.resubmittedAndApproved === 1 ? 'project was' : 'projects were'} fixed and resubmitted successfully
                              </p>
                            )}
                          </div>
                        </TableCell>
                      </TableRow>
                    )}
                  </>
                );
              })}
            </TableBody>
          </Table>
        </div>
      </CardContent>
    </Card>
  );
}
