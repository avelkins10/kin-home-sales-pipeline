'use client';

import { useState, useEffect, useMemo } from 'react';
import { useSession } from 'next-auth/react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { Tabs, TabsList, TabsTrigger, TabsContent } from '@/components/ui/tabs';
import { Button } from '@/components/ui/button';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { AlertDialog, AlertDialogContent, AlertDialogHeader, AlertDialogTitle, AlertDialogTrigger } from '@/components/ui/alert-dialog';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Input } from '@/components/ui/input';
import { Textarea } from '@/components/ui/textarea';
import { RadioGroup, RadioGroupItem } from '@/components/ui/radio-group';
import { Label } from '@/components/ui/label';
import { 
  AlertTriangle, 
  Plus, 
  AlertCircle, 
  Clock, 
  CheckCircle,
  TrendingUp,
  Users,
  Timer
} from 'lucide-react';
import { toast } from 'sonner';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { 
  EscalationCard, 
  EscalationCardSkeleton, 
  EscalationFilters 
} from '@/components/operations';
import { 
  PCEscalation, 
  PCEscalationFilters, 
  PCEscalationAction, 
  PCEscalationStats,
  PCEscalationCategory,
  PCCreateEscalationPayload
} from '@/lib/types/operations';

export default function EscalationsPage() {
  const { data: session } = useSession();
  const queryClient = useQueryClient();
  
  // State management
  const [activeCategory, setActiveCategory] = useState<PCEscalationCategory | 'all'>('all');
  const [filters, setFilters] = useState<PCEscalationFilters>({
    category: 'all',
    status: 'all',
    urgency: 'all',
    assignedTo: 'all',
    search: ''
  });
  const [showCreateDialog, setShowCreateDialog] = useState(false);

  // Create escalation form state
  const [createForm, setCreateForm] = useState({
    projectId: '',
    recordId: '',
    reason: '',
    category: '' as PCEscalationCategory | '',
    description: '',
    priority: 'normal' as 'high' | 'normal'
  });

  // Data fetching
  const { data: escalationsData, isLoading, error } = useQuery({
    queryKey: ['pc-escalations', session?.user?.email, filters],
    queryFn: async () => {
      const params = new URLSearchParams();
      Object.entries(filters).forEach(([key, value]) => {
        if (value && value !== 'all') {
          params.append(key, value);
        }
      });
      
      const response = await fetch(`/api/operations/escalations?${params.toString()}`);
      if (!response.ok) throw new Error('Failed to fetch escalations');
      return response.json();
    },
    enabled: !!session?.user?.email,
    refetchInterval: 30000, // 30 seconds
  });

  const escalations: PCEscalation[] = escalationsData?.escalations || [];
  const stats: PCEscalationStats = escalationsData?.stats || {
    total: 0,
    byCategory: {
      mmu_required: 0,
      rep_promises: 0,
      hoa_issues: 0,
      financing_issues: 0,
      customer_complaints: 0
    },
    byUrgency: {
      critical: 0,
      high: 0,
      normal: 0
    },
    avgResolutionTime: 0
  };

  // Extract unique sales aid reps for filter options
  const availableReps = useMemo(() => {
    const reps = new Set<string>();
    escalations.forEach(escalation => {
      if (escalation.assignedEscalationRep) {
        reps.add(escalation.assignedEscalationRep);
      }
    });
    return Array.from(reps);
  }, [escalations]);

  // Mutations
  const escalationActionMutation = useMutation({
    mutationFn: async ({ escalationId, action, data }: { 
      escalationId: number; 
      action: PCEscalationAction; 
      data?: any 
    }) => {
      const response = await fetch(`/api/operations/escalations/${escalationId}`, {
        method: 'PATCH',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ action, data })
      });
      if (!response.ok) throw new Error('Failed to update escalation');
      return response.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['pc-escalations'] });
      toast.success('Escalation updated successfully');
    },
    onError: (error) => {
      toast.error('Failed to update escalation');
      console.error(error);
    }
  });

  const createEscalationMutation = useMutation({
    mutationFn: async (payload: PCCreateEscalationPayload) => {
      const response = await fetch('/api/operations/escalations', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload)
      });
      if (!response.ok) throw new Error('Failed to create escalation');
      return response.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['pc-escalations'] });
      setShowCreateDialog(false);
      setCreateForm({
        projectId: '',
        recordId: '',
        reason: '',
        category: '',
        description: '',
        priority: 'normal'
      });
      toast.success('Escalation created successfully');
    },
    onError: (error) => {
      toast.error('Failed to create escalation');
      console.error(error);
    }
  });

  // Helper functions
  const handleEscalationAction = (escalationId: number, action: PCEscalationAction, data?: any) => {
    escalationActionMutation.mutate({ escalationId, action, data });
  };

  const handleCreateEscalation = () => {
    if (!createForm.projectId || !createForm.recordId || !createForm.reason || 
        !createForm.category || !createForm.description) {
      toast.error('Please fill in all required fields');
      return;
    }

    // Validate recordId is a valid number
    const recordIdNum = parseInt(createForm.recordId);
    if (isNaN(recordIdNum) || recordIdNum <= 0) {
      toast.error('Record ID must be a valid positive number');
      return;
    }

    // Basic project ID validation (should be alphanumeric)
    if (!/^[A-Za-z0-9-_]+$/.test(createForm.projectId)) {
      toast.error('Project ID must contain only letters, numbers, hyphens, and underscores');
      return;
    }

    createEscalationMutation.mutate({
      projectId: createForm.projectId,
      recordId: recordIdNum,
      reason: createForm.reason,
      category: createForm.category as PCEscalationCategory,
      description: createForm.description,
      priority: createForm.priority
    });
  };

  const filterEscalationsByCategory = (escalations: PCEscalation[], category: PCEscalationCategory | 'all') => {
    if (category === 'all') return escalations;
    return escalations.filter(e => e.category === category);
  };

  const getCategoryDisplayName = (category: PCEscalationCategory): string => {
    const displayNames: Record<PCEscalationCategory, string> = {
      mmu_required: 'MMU Required',
      rep_promises: 'Rep Promises',
      hoa_issues: 'HOA Issues',
      financing_issues: 'Financing Issues',
      customer_complaints: 'Customer Complaints'
    };
    return displayNames[category];
  };

  const getCategoryCount = (category: PCEscalationCategory): number => {
    return stats.byCategory[category] || 0;
  };

  if (error) {
    return (
      <div className="p-6">
        <div className="text-center py-12">
          <AlertCircle className="h-12 w-12 text-red-500 mx-auto mb-4" />
          <h3 className="text-lg font-semibold text-gray-900 mb-2">Failed to load escalations</h3>
          <p className="text-gray-600">Please try refreshing the page.</p>
        </div>
      </div>
    );
  }

  return (
    <div className="p-6 space-y-6">
      {/* Header Section */}
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-2xl font-bold text-gray-900">Escalation Management</h1>
          <p className="text-gray-600">Manage escalated issues and grace periods</p>
        </div>
        <Button onClick={() => setShowCreateDialog(true)} className="flex items-center gap-2">
          <Plus className="h-4 w-4" />
          Create Escalation
        </Button>
      </div>

      {/* Stats Cards */}
      <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Total Escalations</CardTitle>
            <AlertTriangle className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{stats.total}</div>
          </CardContent>
        </Card>
        
        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Critical</CardTitle>
            <AlertCircle className="h-4 w-4 text-red-500" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold text-red-600">{stats.byUrgency.critical}</div>
          </CardContent>
        </Card>
        
        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">High Priority</CardTitle>
            <Clock className="h-4 w-4 text-orange-500" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold text-orange-600">{stats.byUrgency.high}</div>
          </CardContent>
        </Card>
        
        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Avg Resolution</CardTitle>
            <Timer className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{stats.avgResolutionTime}h</div>
          </CardContent>
        </Card>
      </div>

      {/* Filters */}
      <EscalationFilters
        filters={filters}
        onFiltersChange={setFilters}
        availableReps={availableReps}
      />

      {/* Tabs */}
      <Tabs value={activeCategory} onValueChange={(value) => setActiveCategory(value as PCEscalationCategory | 'all')}>
        <TabsList className="grid w-full grid-cols-6">
          <TabsTrigger value="all">All ({stats.total})</TabsTrigger>
          <TabsTrigger value="mmu_required">MMU ({getCategoryCount('mmu_required')})</TabsTrigger>
          <TabsTrigger value="rep_promises">Rep Promises ({getCategoryCount('rep_promises')})</TabsTrigger>
          <TabsTrigger value="hoa_issues">HOA ({getCategoryCount('hoa_issues')})</TabsTrigger>
          <TabsTrigger value="financing_issues">Financing ({getCategoryCount('financing_issues')})</TabsTrigger>
          <TabsTrigger value="customer_complaints">Complaints ({getCategoryCount('customer_complaints')})</TabsTrigger>
        </TabsList>

        {/* Content for each tab */}
        {(['all', 'mmu_required', 'rep_promises', 'hoa_issues', 'financing_issues', 'customer_complaints'] as const).map((category) => (
          <TabsContent key={category} value={category} className="space-y-4">
            {isLoading ? (
              <div className="space-y-4">
                {Array.from({ length: 3 }).map((_, i) => (
                  <EscalationCardSkeleton key={i} />
                ))}
              </div>
            ) : (
              (() => {
                const filteredEscalations = filterEscalationsByCategory(escalations, category);
                return filteredEscalations.length > 0 ? (
                  <div className="space-y-4">
                    {filteredEscalations.map((escalation) => (
                      <EscalationCard
                        key={escalation.recordId}
                        escalation={escalation}
                        onAction={handleEscalationAction}
                      />
                    ))}
                  </div>
                ) : (
                  <div className="text-center py-12">
                    <AlertTriangle className="h-12 w-12 text-gray-400 mx-auto mb-4" />
                    <h3 className="text-lg font-semibold text-gray-900 mb-2">No escalations found</h3>
                    <p className="text-gray-600 mb-4">
                      {category === 'all' 
                        ? 'No escalations match your current filters.'
                        : `No ${getCategoryDisplayName(category as PCEscalationCategory).toLowerCase()} escalations found.`
                      }
                    </p>
                    <Button onClick={() => setShowCreateDialog(true)}>
                      Create Escalation
                    </Button>
                  </div>
                );
              })()
            )}
          </TabsContent>
        ))}
      </Tabs>

      {/* Create Escalation Dialog */}
      <AlertDialog open={showCreateDialog} onOpenChange={setShowCreateDialog}>
        <AlertDialogContent className="max-w-2xl">
          <AlertDialogHeader>
            <AlertDialogTitle>Create New Escalation</AlertDialogTitle>
          </AlertDialogHeader>
          <div className="space-y-4">
            <div className="grid grid-cols-2 gap-4">
              <div>
                <Label htmlFor="projectId">Project ID *</Label>
                <Input
                  id="projectId"
                  value={createForm.projectId}
                  onChange={(e) => setCreateForm({ ...createForm, projectId: e.target.value })}
                  placeholder="Enter project ID (e.g., PROJ-123)"
                  pattern="[A-Za-z0-9-_]+"
                  title="Project ID must contain only letters, numbers, hyphens, and underscores"
                />
                {createForm.projectId && !/^[A-Za-z0-9-_]+$/.test(createForm.projectId) && (
                  <p className="text-sm text-red-500 mt-1">Invalid project ID format</p>
                )}
              </div>
              <div>
                <Label htmlFor="recordId">Record ID *</Label>
                <Input
                  id="recordId"
                  type="number"
                  value={createForm.recordId}
                  onChange={(e) => setCreateForm({ ...createForm, recordId: e.target.value })}
                  placeholder="Enter QuickBase record ID"
                  min="1"
                />
                {createForm.recordId && (isNaN(parseInt(createForm.recordId)) || parseInt(createForm.recordId) <= 0) && (
                  <p className="text-sm text-red-500 mt-1">Record ID must be a positive number</p>
                )}
              </div>
            </div>

            <div>
              <Label htmlFor="category">Category *</Label>
              <Select 
                value={createForm.category} 
                onValueChange={(value) => setCreateForm({ ...createForm, category: value as PCEscalationCategory })}
              >
                <SelectTrigger>
                  <SelectValue placeholder="Select category" />
                </SelectTrigger>
                <SelectContent>
                  <SelectItem value="mmu_required">MMU Required</SelectItem>
                  <SelectItem value="rep_promises">Rep Promises</SelectItem>
                  <SelectItem value="hoa_issues">HOA Issues</SelectItem>
                  <SelectItem value="financing_issues">Financing Issues</SelectItem>
                  <SelectItem value="customer_complaints">Customer Complaints</SelectItem>
                </SelectContent>
              </Select>
            </div>

            <div>
              <Label htmlFor="reason">Reason *</Label>
              <Input
                id="reason"
                value={createForm.reason}
                onChange={(e) => setCreateForm({ ...createForm, reason: e.target.value })}
                placeholder="Enter escalation reason"
                required
              />
            </div>

            <div>
              <Label htmlFor="description">Description *</Label>
              <Textarea
                id="description"
                value={createForm.description}
                onChange={(e) => setCreateForm({ ...createForm, description: e.target.value })}
                placeholder="Describe the escalation in detail..."
                rows={4}
                required
              />
            </div>

            <div>
              <Label>Priority</Label>
              <RadioGroup 
                value={createForm.priority} 
                onValueChange={(value) => setCreateForm({ ...createForm, priority: value as 'high' | 'normal' })}
                className="flex gap-6"
              >
                <div className="flex items-center space-x-2">
                  <RadioGroupItem value="normal" id="normal" />
                  <Label htmlFor="normal">Normal (72 hours)</Label>
                </div>
                <div className="flex items-center space-x-2">
                  <RadioGroupItem value="high" id="high" />
                  <Label htmlFor="high">High (48 hours)</Label>
                </div>
              </RadioGroup>
            </div>

            <div className="flex gap-2 justify-end">
              <Button variant="outline" onClick={() => setShowCreateDialog(false)}>
                Cancel
              </Button>
              <Button 
                onClick={handleCreateEscalation}
                disabled={createEscalationMutation.isPending}
              >
                {createEscalationMutation.isPending ? 'Creating...' : 'Create Escalation'}
              </Button>
            </div>
          </div>
        </AlertDialogContent>
      </AlertDialog>
    </div>
  );
}
