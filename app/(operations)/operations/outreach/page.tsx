'use client';

import { useState, useEffect, useMemo } from 'react';
import { useSession } from 'next-auth/react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { Tabs, TabsList, TabsTrigger, TabsContent } from '@/components/ui/tabs';
import { Checkbox } from '@/components/ui/checkbox';
import { Button } from '@/components/ui/button';
import { AlertCircle, Users, Phone, Calendar } from 'lucide-react';
import { toast } from 'sonner';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { 
  OutreachCard, 
  OutreachCardSkeleton, 
  OutreachFilters, 
  BulkActionBar 
} from '@/components/operations';
import { 
  PCOutreachRecord, 
  PCOutreachFilters, 
  PCOutreachTabData, 
  PCOutreachBulkAction 
} from '@/lib/types/operations';

export default function OutreachPage() {
  const { data: session, status } = useSession();
  const queryClient = useQueryClient();

  // State management
  const [activeTab, setActiveTab] = useState<'initial' | 'followups' | 'welcome'>('initial');
  const [filters, setFilters] = useState<PCOutreachFilters>({
    tab: 'initial',
    status: 'all',
    daysOverdue: 'all',
    lender: 'all',
    salesRep: 'all',
    search: ''
  });
  const [selectedRecords, setSelectedRecords] = useState<Set<number>>(new Set());
  const [selectAll, setSelectAll] = useState(false);

  // Update filters when tab changes
  useEffect(() => {
    setFilters(prev => ({ ...prev, tab: activeTab }));
  }, [activeTab]);

  // Fetch all tab counts first
  const { data: tabCounts } = useQuery({
    queryKey: ['pc-outreach-counts', session?.user?.email],
    queryFn: async () => {
      const baseUrl = getBaseUrl();
      const response = await fetch(`${baseUrl}/api/operations/outreach?tab=all`);
      if (!response.ok) {
        throw new Error('Failed to fetch outreach counts');
      }
      const json = await response.json();
      return json.data?.counts || { initial: 0, followups: 0, welcome: 0 };
    },
    enabled: !!session?.user?.email,
    refetchInterval: 30000, // 30 seconds
    staleTime: 15000 // 15 seconds
  });

  // Data fetching for active tab
  const { data: outreachData, isLoading, error, refetch } = useQuery({
    queryKey: ['pc-outreach', session?.user?.email, activeTab, filters],
    queryFn: async () => {
      const baseUrl = getBaseUrl();
      const params = new URLSearchParams({
        tab: activeTab,
        status: filters.status,
        daysOverdue: filters.daysOverdue,
        lender: filters.lender,
        salesRep: filters.salesRep,
        search: filters.search
      });

      const response = await fetch(`${baseUrl}/api/operations/outreach?${params}`);
      if (!response.ok) {
        throw new Error('Failed to fetch outreach data');
      }
      return response.json();
    },
    enabled: !!session?.user?.email,
    refetchInterval: 30000, // 30 seconds
    staleTime: 15000 // 15 seconds
  });

  // Bulk action mutation
  const bulkActionMutation = useMutation({
    mutationFn: async ({ action, recordIds, data }: { 
      action: PCOutreachBulkAction; 
      recordIds: number[]; 
      data?: Record<string, any> 
    }) => {
      const baseUrl = getBaseUrl();
      const response = await fetch(`${baseUrl}/api/operations/outreach/bulk-action`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ action, recordIds, data })
      });

      if (!response.ok) {
        const error = await response.json();
        throw new Error(error.error || 'Bulk action failed');
      }
      return response.json();
    },
    onSuccess: (data) => {
      if (data.success) {
        toast.success(data.result.message);
        setSelectedRecords(new Set());
        setSelectAll(false);
        queryClient.invalidateQueries({ queryKey: ['pc-outreach'] });
      } else {
        toast.error(data.result.message);
      }
    },
    onError: (error) => {
      toast.error(error.message);
    }
  });

  // Extract unique values for filter options
  const availableLenders = useMemo(() => {
    if (!outreachData?.data) return [];
    const lenders = new Set<string>();
    if (Array.isArray(outreachData.data)) {
      outreachData.data.forEach((record: PCOutreachRecord) => {
        if (record.lenderName) lenders.add(record.lenderName);
      });
    }
    return Array.from(lenders).sort();
  }, [outreachData]);

  const availableSalesReps = useMemo(() => {
    if (!outreachData?.data) return [];
    const reps = new Set<string>();
    if (Array.isArray(outreachData.data)) {
      outreachData.data.forEach((record: PCOutreachRecord) => {
        if (record.salesRepName) reps.add(record.salesRepName);
      });
    }
    return Array.from(reps).sort();
  }, [outreachData]);

  // Handle selection
  const handleSelectAll = () => {
    if (!outreachData?.data || !Array.isArray(outreachData.data)) return;
    
    if (selectAll) {
      setSelectedRecords(new Set());
      setSelectAll(false);
    } else {
      const allIds = new Set(outreachData.data.map((record: PCOutreachRecord) => record.recordId));
      setSelectedRecords(allIds);
      setSelectAll(true);
    }
  };

  const handleSelectItem = (recordId: number, selected: boolean) => {
    const newSelected = new Set(selectedRecords);
    if (selected) {
      newSelected.add(recordId);
    } else {
      newSelected.delete(recordId);
    }
    setSelectedRecords(newSelected);
    setSelectAll(newSelected.size === (outreachData?.data?.length || 0));
  };

  const handleBulkAction = (action: PCOutreachBulkAction) => {
    if (selectedRecords.size === 0) return;
    
    bulkActionMutation.mutate({
      action,
      recordIds: Array.from(selectedRecords),
      data: {} // Additional data can be added here
    });
  };

  const handleClearSelection = () => {
    setSelectedRecords(new Set());
    setSelectAll(false);
  };

  // Loading state
  if (status === 'loading' || isLoading) {
    return (
      <div className="space-y-6">
        <div className="flex items-center justify-between">
          <div>
            <h1 className="text-2xl font-bold text-gray-900">Outreach Management</h1>
            <p className="text-gray-600">Manage customer outreach and follow-ups</p>
          </div>
        </div>
        
        <div className="space-y-4">
          {Array.from({ length: 3 }).map((_, i) => (
            <OutreachCardSkeleton key={i} />
          ))}
        </div>
      </div>
    );
  }

  // Error state
  if (error) {
    return (
      <div className="space-y-6">
        <div className="flex items-center justify-between">
          <div>
            <h1 className="text-2xl font-bold text-gray-900">Outreach Management</h1>
            <p className="text-gray-600">Manage customer outreach and follow-ups</p>
          </div>
        </div>
        
        <div className="flex items-center justify-center py-12">
          <div className="text-center">
            <AlertCircle className="h-12 w-12 text-red-500 mx-auto mb-4" />
            <h3 className="text-lg font-medium text-gray-900 mb-2">Failed to load outreach data</h3>
            <p className="text-gray-600 mb-4">There was an error loading the outreach data.</p>
            <Button onClick={() => refetch()}>
              Try Again
            </Button>
          </div>
        </div>
      </div>
    );
  }

  const records = outreachData?.data || [];
  const hasRecords = Array.isArray(records) && records.length > 0;

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-2xl font-bold text-gray-900">Outreach Management</h1>
          <p className="text-gray-600">Manage customer outreach and follow-ups</p>
        </div>
        
        {/* Select All checkbox */}
        {hasRecords && (
          <div className="flex items-center gap-2">
            <Checkbox
              checked={selectAll}
              onCheckedChange={handleSelectAll}
              id="select-all"
            />
            <label htmlFor="select-all" className="text-sm font-medium text-gray-700">
              Select All
            </label>
          </div>
        )}
      </div>

      {/* Filters */}
      <OutreachFilters
        filters={filters}
        onFiltersChange={setFilters}
        availableLenders={availableLenders}
        availableSalesReps={availableSalesReps}
      />

      {/* Tabs */}
      <Tabs value={activeTab} onValueChange={(value) => setActiveTab(value as typeof activeTab)}>
        <TabsList className="grid w-full grid-cols-3">
          <TabsTrigger value="initial" className="flex items-center gap-2">
            <Users className="h-4 w-4" />
            Initial Outreach ({tabCounts?.initial || 0})
          </TabsTrigger>
          <TabsTrigger value="followups" className="flex items-center gap-2">
            <Phone className="h-4 w-4" />
            Follow-ups ({tabCounts?.followups || 0})
          </TabsTrigger>
          <TabsTrigger value="welcome" className="flex items-center gap-2">
            <Calendar className="h-4 w-4" />
            Welcome Calls ({tabCounts?.welcome || 0})
          </TabsTrigger>
        </TabsList>

        {/* Tab Content */}
        <TabsContent value={activeTab} className="mt-6">
          {isLoading ? (
            <div className="space-y-4">
              {Array.from({ length: 3 }).map((_, i) => (
                <OutreachCardSkeleton key={i} />
              ))}
            </div>
          ) : !hasRecords ? (
            <div className="flex items-center justify-center py-12">
              <div className="text-center">
                <Users className="h-12 w-12 text-gray-400 mx-auto mb-4" />
                <h3 className="text-lg font-medium text-gray-900 mb-2">No outreach items found</h3>
                <p className="text-gray-600">
                  {activeTab === 'initial' && 'No initial outreach items due today.'}
                  {activeTab === 'followups' && 'No unresponsive customers found.'}
                  {activeTab === 'welcome' && 'No welcome calls scheduled.'}
                </p>
              </div>
            </div>
          ) : (
            <div className="space-y-4">
              {records.map((record: PCOutreachRecord) => (
                <OutreachCard
                  key={record.recordId}
                  item={record}
                  selected={selectedRecords.has(record.recordId)}
                  onSelect={handleSelectItem}
                  onAction={(recordId, action) => {
                    if (action === 'mark_contacted') {
                      // If no selection, use the clicked record; otherwise use selected records
                      const recordIds = selectedRecords.size === 0 ? [recordId] : Array.from(selectedRecords);
                      bulkActionMutation.mutate({
                        action: 'mark_contacted',
                        recordIds,
                        data: {}
                      });
                    }
                  }}
                />
              ))}
            </div>
          )}
        </TabsContent>
      </Tabs>

      {/* Bulk Action Bar */}
      <BulkActionBar
        selectedCount={selectedRecords.size}
        onAction={handleBulkAction}
        onClearSelection={handleClearSelection}
        isProcessing={bulkActionMutation.isPending}
      />
    </div>
  );
}
