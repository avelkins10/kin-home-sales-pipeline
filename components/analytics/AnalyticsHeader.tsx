'use client';

import { Download, Loader2 } from 'lucide-react';
import { Button } from '@/components/ui/button';

interface AnalyticsHeaderProps {
  onExportCSV: () => void;
  isExporting?: boolean;
  title?: string;
  description?: string;
}

export function AnalyticsHeader({
  onExportCSV,
  isExporting = false,
  title = 'Analytics Dashboard',
  description = 'Comprehensive analytics and performance insights for your team'
}: AnalyticsHeaderProps) {
  return (
    <div className="flex flex-col md:flex-row md:items-center md:justify-between gap-4">
      <div className="space-y-1">
        <h1 className="text-3xl font-bold tracking-tight">{title}</h1>
        <p className="text-gray-600">{description}</p>
      </div>
      
      <div className="flex items-center gap-2">
        <Button
          variant="outline"
          onClick={onExportCSV}
          disabled={isExporting}
          className="w-full md:w-auto"
        >
          {isExporting ? (
            <>
              <Loader2 className="mr-2 h-4 w-4 animate-spin" />
              Exporting...
            </>
          ) : (
            <>
              <Download className="mr-2 h-4 w-4" />
              Export CSV
            </>
          )}
        </Button>
      </div>
    </div>
  );
}
