'use client';

import { useState } from 'react';
import { useSession } from 'next-auth/react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { Tabs, TabsList, TabsTrigger, TabsContent } from '@/components/ui/tabs';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { 
  InboundQueueCard, 
  InboundQueueCardSkeleton, 
  CommunicationLog, 
  BulkMessagingPanel, 
  CallQueue 
} from '@/components/operations';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { toast } from 'sonner';
import { 
  PCSalesAidRequest, 
  PCConversationItem, 
  PCBulkMessagingPayload, 
  PCCallQueueItem,
  PCConversationFilters 
} from '@/lib/types/operations';

export default function CommunicationsPage() {
  const { data: session } = useSession();
  const queryClient = useQueryClient();
  
  const [activeTab, setActiveTab] = useState<'inbound' | 'conversations' | 'bulk_messaging' | 'call_queue'>('inbound');
  const [conversationFilters, setConversationFilters] = useState<PCConversationFilters>({
    tab: 'recent',
    projectStage: 'all',
    communicationType: 'all',
    dateRange: '7days',
    search: ''
  });
  const [callQueue, setCallQueue] = useState<PCCallQueueItem[]>([]);

  // Data fetching queries
  const { data: inboundData, isLoading: inboundLoading, error: inboundError } = useQuery({
    queryKey: ['pc-inbound-queue', session?.user?.email],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/operations/communications/inbound`);
      if (!response.ok) throw new Error('Failed to fetch inbound queue');
      return response.json();
    },
    enabled: !!session?.user?.email,
    refetchInterval: 30000, // 30 seconds
  });

  const { data: conversationsData, isLoading: conversationsLoading, error: conversationsError } = useQuery({
    queryKey: ['pc-conversations', session?.user?.email, conversationFilters],
    queryFn: async () => {
      const params = new URLSearchParams();
      Object.entries(conversationFilters).forEach(([key, value]) => {
        if (value && value !== 'all') {
          params.append(key, value);
        }
      });
      
      const response = await fetch(`${getBaseUrl()}/api/operations/communications/conversations?${params}`);
      if (!response.ok) throw new Error('Failed to fetch conversations');
      return response.json();
    },
    enabled: !!session?.user?.email && activeTab === 'conversations',
    refetchInterval: 30000, // 30 seconds
  });

  // Mutations
  const inboundActionMutation = useMutation({
    mutationFn: async ({ recordId, action }: { recordId: number; action: string }) => {
      const response = await fetch(`${getBaseUrl()}/api/operations/communications/inbound/${recordId}`, {
        method: 'PATCH',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ action })
      });
      if (!response.ok) throw new Error('Failed to update sales aid request');
      return response.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['pc-inbound-queue'] });
      toast.success('Sales aid request updated successfully');
    },
    onError: (error) => {
      toast.error('Failed to update sales aid request');
      console.error('Inbound action error:', error);
    }
  });

  const bulkSmsMutation = useMutation({
    mutationFn: async (payload: PCBulkMessagingPayload) => {
      const response = await fetch(`${getBaseUrl()}/api/operations/communications/bulk-sms`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload)
      });
      if (!response.ok) throw new Error('Failed to send bulk SMS');
      return response.json();
    },
    onSuccess: (data) => {
      queryClient.invalidateQueries({ queryKey: ['pc-conversations'] });
      toast.success(`Sent ${data.result.sent} messages successfully${data.result.failed > 0 ? `, ${data.result.failed} failed` : ''}`);
    },
    onError: (error) => {
      toast.error('Failed to send bulk SMS');
      console.error('Bulk SMS error:', error);
    }
  });

  const callMutation = useMutation({
    mutationFn: async ({ projectId, recordId, customerPhone, customerName }: {
      projectId: string;
      recordId: number;
      customerPhone: string;
      customerName: string;
    }) => {
      const response = await fetch(`${getBaseUrl()}/api/operations/communications/call`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ projectId, recordId, customerPhone, customerName })
      });
      if (!response.ok) throw new Error('Failed to initiate call');
      return response.json();
    },
    onSuccess: (data) => {
      queryClient.invalidateQueries({ queryKey: ['pc-conversations'] });
      toast.success('Call initiated successfully');
    },
    onError: (error) => {
      toast.error('Failed to initiate call');
      console.error('Call error:', error);
    }
  });

  // Event handlers
  const handleInboundAction = (recordId: number, action: 'respond' | 'escalate' | 'resolve') => {
    inboundActionMutation.mutate({ recordId, action });
  };

  const handleBulkSms = (payload: PCBulkMessagingPayload) => {
    bulkSmsMutation.mutate(payload);
  };

  const handleCallCustomer = (projectId: string, customerPhone: string, customerName: string) => {
    // Add to call queue or initiate call directly
    const queueItem: PCCallQueueItem = {
      projectId,
      recordId: 0, // Will be filled from project data
      customerName,
      customerPhone,
      projectStage: 'Unknown',
      priority: callQueue.length + 1,
      callStatus: null,
      callSid: null,
      queuedAt: new Date().toISOString()
    };
    
    setCallQueue([...callQueue, queueItem]);
    setActiveTab('call_queue');
  };

  const handleSendSms = (projectId: string, customerPhone: string, customerName: string) => {
    // Switch to bulk messaging tab
    setActiveTab('bulk_messaging');
  };

  const handleCallInitiated = (callSid: string) => {
    toast.success(`Call initiated: ${callSid}`);
  };

  return (
    <div className="container mx-auto px-4 py-6">
      {/* Header */}
      <div className="mb-6">
        <h1 className="text-2xl font-bold text-gray-900">Communication Hub</h1>
        <p className="text-gray-600">Manage customer communications and sales rep inquiries</p>
      </div>

      {/* Badge counts */}
      <div className="flex gap-4 mb-6">
        {inboundData && (
          <>
            <Badge variant="destructive" className="text-sm">
              {inboundData.criticalCount} Critical
            </Badge>
            <Badge variant="secondary" className="text-sm">
              {inboundData.highCount} High Priority
            </Badge>
            <Badge variant="outline" className="text-sm">
              {inboundData.count} Total Inbound
            </Badge>
          </>
        )}
        {conversationsData && (
          <Badge variant="outline" className="text-sm">
            {conversationsData.needsResponseCount} Need Response
          </Badge>
        )}
      </div>

      {/* Tabs */}
      <Tabs value={activeTab} onValueChange={(value) => setActiveTab(value as any)} className="w-full">
        <TabsList className="grid w-full grid-cols-4">
          <TabsTrigger value="inbound">
            Inbound Queue {inboundData && `(${inboundData.count})`}
          </TabsTrigger>
          <TabsTrigger value="conversations">
            Conversations {conversationsData && `(${conversationsData.count})`}
          </TabsTrigger>
          <TabsTrigger value="bulk_messaging">
            Bulk Messaging
          </TabsTrigger>
          <TabsTrigger value="call_queue">
            Call Queue {callQueue.length > 0 && `(${callQueue.length})`}
          </TabsTrigger>
        </TabsList>

        {/* Inbound Queue Tab */}
        <TabsContent value="inbound" className="mt-6">
          <div className="space-y-4">
            {inboundLoading ? (
              <div className="space-y-3">
                {Array.from({ length: 3 }).map((_, i) => (
                  <InboundQueueCardSkeleton key={i} />
                ))}
              </div>
            ) : inboundError ? (
              <div className="text-center py-8 text-red-600">
                <p>Failed to load inbound queue</p>
                <Button 
                  variant="outline" 
                  onClick={() => queryClient.invalidateQueries({ queryKey: ['pc-inbound-queue'] })}
                  className="mt-2"
                >
                  Retry
                </Button>
              </div>
            ) : inboundData?.requests?.length === 0 ? (
              <div className="text-center py-8 text-gray-500">
                <p>No pending inquiries from sales reps</p>
              </div>
            ) : (
              inboundData?.requests?.map((request: PCSalesAidRequest) => (
                <InboundQueueCard
                  key={request.recordId}
                  item={request}
                  onAction={handleInboundAction}
                />
              ))
            )}
          </div>
        </TabsContent>

        {/* Conversations Tab */}
        <TabsContent value="conversations" className="mt-6">
          <div className="space-y-4">
            {/* Filters */}
            <div className="flex gap-4 mb-4">
              <select
                value={conversationFilters.tab}
                onChange={(e) => setConversationFilters({...conversationFilters, tab: e.target.value as any})}
                className="px-3 py-2 border rounded-md"
              >
                <option value="recent">Recent</option>
                <option value="needs_response">Needs Response</option>
                <option value="scheduled">Scheduled</option>
              </select>
              
              <select
                value={conversationFilters.communicationType}
                onChange={(e) => setConversationFilters({...conversationFilters, communicationType: e.target.value as any})}
                className="px-3 py-2 border rounded-md"
              >
                <option value="all">All Types</option>
                <option value="sms">SMS</option>
                <option value="call">Call</option>
                <option value="email">Email</option>
                <option value="note">Note</option>
              </select>
              
              <select
                value={conversationFilters.dateRange}
                onChange={(e) => setConversationFilters({...conversationFilters, dateRange: e.target.value as any})}
                className="px-3 py-2 border rounded-md"
              >
                <option value="7days">Last 7 days</option>
                <option value="30days">Last 30 days</option>
                <option value="90days">Last 90 days</option>
                <option value="all">All time</option>
              </select>
            </div>

            {conversationsLoading ? (
              <div className="text-center py-8 text-gray-500">
                <p>Loading conversations...</p>
              </div>
            ) : conversationsError ? (
              <div className="text-center py-8 text-red-600">
                <p>Failed to load conversations</p>
                <Button 
                  variant="outline" 
                  onClick={() => queryClient.invalidateQueries({ queryKey: ['pc-conversations'] })}
                  className="mt-2"
                >
                  Retry
                </Button>
              </div>
            ) : (
              <CommunicationLog
                conversations={conversationsData?.conversations || []}
                onCallCustomer={handleCallCustomer}
                onSendSms={handleSendSms}
              />
            )}
          </div>
        </TabsContent>

        {/* Bulk Messaging Tab */}
        <TabsContent value="bulk_messaging" className="mt-6">
          <BulkMessagingPanel
            onSend={handleBulkSms}
            isProcessing={bulkSmsMutation.isPending}
          />
        </TabsContent>

        {/* Call Queue Tab */}
        <TabsContent value="call_queue" className="mt-6">
          <CallQueue
            onCallInitiated={handleCallInitiated}
          />
        </TabsContent>
      </Tabs>
    </div>
  );
}
