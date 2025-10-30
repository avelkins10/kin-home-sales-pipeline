'use client';

import { ReactNode } from 'react';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { BarChart3, Users, TrendingUp, AlertTriangle, Trophy, DoorOpen, Zap } from 'lucide-react';

interface AnalyticsTabsProps {
  overviewContent: ReactNode;
  performanceContent: ReactNode;
  comparisonsContent: ReactNode;
  analysisContent: ReactNode;
  leaderboardsContent: ReactNode;
  canvassingContent: ReactNode;
  repcardContent: ReactNode;
  value?: string;
  onTabChange?: (value: string) => void;
}

export function AnalyticsTabs({
  overviewContent,
  performanceContent,
  comparisonsContent,
  analysisContent,
  leaderboardsContent,
  canvassingContent,
  repcardContent,
  value = 'overview',
  onTabChange
}: AnalyticsTabsProps) {
  return (
    <Tabs
      value={value}
      className="w-full"
      onValueChange={onTabChange}
    >
      <TabsList className="grid w-full grid-cols-3 md:grid-cols-7 h-auto p-1.5 bg-slate-100 rounded-xl mb-8 shadow-sm">
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
        <TabsTrigger
          value="leaderboards"
          className="flex items-center justify-center gap-2 py-3 px-4 data-[state=active]:bg-white data-[state=active]:shadow-md rounded-lg transition-all duration-200 font-medium text-sm"
        >
          <Trophy className="h-4 w-4" />
          <span className="hidden sm:inline">Leaderboards</span>
        </TabsTrigger>
        <TabsTrigger
          value="canvassing"
          className="flex items-center justify-center gap-2 py-3 px-4 data-[state=active]:bg-white data-[state=active]:shadow-md rounded-lg transition-all duration-200 font-medium text-sm"
        >
          <DoorOpen className="h-4 w-4" />
          <span className="hidden sm:inline">Canvassing</span>
        </TabsTrigger>
        <TabsTrigger
          value="repcard"
          className="flex items-center justify-center gap-2 py-3 px-4 data-[state=active]:bg-white data-[state=active]:shadow-md rounded-lg transition-all duration-200 font-medium text-sm"
        >
          <Zap className="h-4 w-4" />
          <span className="hidden sm:inline">RepCard</span>
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

      <TabsContent value="leaderboards" className="mt-0 space-y-6 animate-in fade-in-50 duration-200">
        {leaderboardsContent}
      </TabsContent>

      <TabsContent value="canvassing" className="mt-0 space-y-6 animate-in fade-in-50 duration-200">
        {canvassingContent}
      </TabsContent>

      <TabsContent value="repcard" className="mt-0 space-y-6 animate-in fade-in-50 duration-200">
        {repcardContent}
      </TabsContent>
    </Tabs>
  );
}
