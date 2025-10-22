'use client';

import { ReactNode } from 'react';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { BarChart3, Users, TrendingUp, AlertTriangle } from 'lucide-react';

interface AnalyticsTabsProps {
  overviewContent: ReactNode;
  performanceContent: ReactNode;
  comparisonsContent: ReactNode;
  analysisContent: ReactNode;
  defaultTab?: string;
  onTabChange?: (value: string) => void;
}

export function AnalyticsTabs({
  overviewContent,
  performanceContent,
  comparisonsContent,
  analysisContent,
  defaultTab = 'overview',
  onTabChange
}: AnalyticsTabsProps) {
  return (
    <Tabs
      defaultValue={defaultTab}
      className="w-full"
      onValueChange={onTabChange}
    >
      <TabsList className="grid w-full grid-cols-4 h-auto p-1.5 bg-slate-100 rounded-xl mb-8 shadow-sm">
        <TabsTrigger
          value="overview"
          className="flex items-center justify-center gap-2 py-3 px-4 data-[state=active]:bg-white data-[state=active]:shadow-md rounded-lg transition-all duration-200 font-medium text-sm"
        >
          <BarChart3 className="h-4 w-4" />
          <span className="hidden sm:inline">Overview</span>
        </TabsTrigger>
        <TabsTrigger
          value="performance"
          className="flex items-center justify-center gap-2 py-3 px-4 data-[state=active]:bg-white data-[state=active]:shadow-md rounded-lg transition-all duration-200 font-medium text-sm"
        >
          <Users className="h-4 w-4" />
          <span className="hidden sm:inline">Performance</span>
        </TabsTrigger>
        <TabsTrigger
          value="comparisons"
          className="flex items-center justify-center gap-2 py-3 px-4 data-[state=active]:bg-white data-[state=active]:shadow-md rounded-lg transition-all duration-200 font-medium text-sm"
        >
          <TrendingUp className="h-4 w-4" />
          <span className="hidden sm:inline">Comparisons</span>
        </TabsTrigger>
        <TabsTrigger
          value="analysis"
          className="flex items-center justify-center gap-2 py-3 px-4 data-[state=active]:bg-white data-[state=active]:shadow-md rounded-lg transition-all duration-200 font-medium text-sm"
        >
          <AlertTriangle className="h-4 w-4" />
          <span className="hidden sm:inline">Analysis</span>
        </TabsTrigger>
      </TabsList>

      <TabsContent value="overview" className="mt-0 space-y-6 animate-in fade-in-50 duration-200">
        {overviewContent}
      </TabsContent>

      <TabsContent value="performance" className="mt-0 space-y-6 animate-in fade-in-50 duration-200">
        {performanceContent}
      </TabsContent>

      <TabsContent value="comparisons" className="mt-0 space-y-6 animate-in fade-in-50 duration-200">
        {comparisonsContent}
      </TabsContent>

      <TabsContent value="analysis" className="mt-0 space-y-6 animate-in fade-in-50 duration-200">
        {analysisContent}
      </TabsContent>
    </Tabs>
  );
}
