'use client';

import { useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { AlertDialog, AlertDialogContent, AlertDialogHeader, AlertDialogTitle, AlertDialogTrigger } from '@/components/ui/alert-dialog';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Textarea } from '@/components/ui/textarea';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { RadioGroup, RadioGroupItem } from '@/components/ui/radio-group';
import { 
  CheckSquare, 
  Plus, 
  Phone, 
  FileText, 
  DollarSign, 
  MessageSquare, 
  Calendar, 
  Home,
  Clock,
  User,
  AlertCircle
} from 'lucide-react';
import { toast } from 'sonner';
import { PCTask, PCTaskType, PCTaskAssignmentPayload } from '@/lib/types/operations';
import { MessageThread } from './MessageThread';
import { useSession } from 'next-auth/react';

interface TaskAssignmentProps {
  projectId: string;
  recordId: number | null;
  customerName: string;
  salesRepEmail: string;
  salesRepName: string;
  onTaskCreated?: (taskId: number) => void;
}

export function TaskAssignment({
  projectId,
  recordId,
  customerName,
  salesRepEmail,
  salesRepName,
  onTaskCreated
}: TaskAssignmentProps) {
  const [showCreateDialog, setShowCreateDialog] = useState(false);
  const [selectedTaskId, setSelectedTaskId] = useState<number | null>(null);
  const [showTaskMessages, setShowTaskMessages] = useState(false);
  
  // Task creation form state
  const [taskType, setTaskType] = useState<PCTaskType | null>(null);
  const [taskName, setTaskName] = useState('');
  const [taskDescription, setTaskDescription] = useState('');
  const [dueDate, setDueDate] = useState('');
  const [priority, setPriority] = useState<'high' | 'normal' | 'low'>('normal');

  const queryClient = useQueryClient();
  const { data: session } = useSession();

  // Data fetching
  const { data: tasks = [], isLoading } = useQuery({
    queryKey: ['pc-tasks', projectId, recordId],
    queryFn: async () => {
      const params = new URLSearchParams();
      if (recordId) {
        params.append('recordId', recordId.toString());
      } else {
        params.append('projectId', projectId);
      }
      
      const response = await fetch(`/api/operations/tasks?${params}`);
      if (!response.ok) throw new Error('Failed to fetch tasks');
      
      const data = await response.json();
      return data.tasks as PCTask[];
    },
    refetchInterval: 30000, // 30 seconds
    enabled: !!projectId && !!recordId
  });

  // Create task mutation
  const createTaskMutation = useMutation({
    mutationFn: async (payload: PCTaskAssignmentPayload) => {
      const response = await fetch('/api/operations/tasks', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload)
      });

      if (!response.ok) {
        const error = await response.json();
        throw new Error(error.message || 'Failed to create task');
      }

      return response.json();
    },
    onSuccess: (data) => {
      queryClient.invalidateQueries({ queryKey: ['pc-tasks', projectId, recordId] });
      setShowCreateDialog(false);
      resetForm();
      toast.success('Task assigned successfully');
      onTaskCreated?.(data.taskId);
    },
    onError: (error: Error) => {
      toast.error(error.message);
    }
  });

  // Helper functions
  const getTaskTypeIcon = (type: PCTaskType) => {
    switch (type) {
      case 'callback_customer': return <Phone className="h-4 w-4" />;
      case 'collect_documents': return <FileText className="h-4 w-4" />;
      case 'clarify_pricing': return <DollarSign className="h-4 w-4" />;
      case 'handle_objection': return <MessageSquare className="h-4 w-4" />;
      case 'schedule_site_visit': return <Calendar className="h-4 w-4" />;
      case 'resolve_hoa_issue': return <Home className="h-4 w-4" />;
      default: return <CheckSquare className="h-4 w-4" />;
    }
  };

  const getTaskTypeDescription = (type: PCTaskType): string => {
    const descriptions = {
      callback_customer: 'Provide details about what needs to be discussed...',
      collect_documents: 'List the documents needed...',
      clarify_pricing: 'Explain what pricing details need clarification...',
      handle_objection: 'Describe the customer\'s objection...',
      schedule_site_visit: 'Provide preferred dates and times...',
      resolve_hoa_issue: 'Describe the HOA issue...'
    };
    return descriptions[type];
  };

  const formatTaskName = (type: PCTaskType): string => {
    const names = {
      callback_customer: 'Callback Customer',
      collect_documents: 'Collect Documents',
      clarify_pricing: 'Clarify Pricing',
      handle_objection: 'Handle Objection',
      schedule_site_visit: 'Schedule Site Visit',
      resolve_hoa_issue: 'Resolve HOA Issue'
    };
    return names[type];
  };

  const formatDueDate = (date: string): string => {
    const dueDate = new Date(date);
    const now = new Date();
    const diffInDays = Math.ceil((dueDate.getTime() - now.getTime()) / (1000 * 60 * 60 * 24));
    
    if (diffInDays < 0) return `Overdue by ${Math.abs(diffInDays)} days`;
    if (diffInDays === 0) return 'Due today';
    if (diffInDays === 1) return 'Due tomorrow';
    return `Due in ${diffInDays} days`;
  };

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'Completed': return 'bg-green-100 text-green-800 border-green-200';
      case 'In Progress': return 'bg-blue-100 text-blue-800 border-blue-200';
      case 'Blocked': return 'bg-red-100 text-red-800 border-red-200';
      default: return 'bg-gray-100 text-gray-800 border-gray-200';
    }
  };

  const getPriorityColor = (priority: string) => {
    switch (priority) {
      case 'high': return 'bg-red-100 text-red-800 border-red-200';
      case 'normal': return 'bg-yellow-100 text-yellow-800 border-yellow-200';
      case 'low': return 'bg-gray-100 text-gray-800 border-gray-200';
      default: return 'bg-gray-100 text-gray-800 border-gray-200';
    }
  };

  const resetForm = () => {
    setTaskType(null);
    setTaskName('');
    setTaskDescription('');
    setDueDate('');
    setPriority('normal');
  };

  const handleCreateTask = async () => {
    if (!taskType || !taskDescription.trim()) {
      toast.error('Please fill in all required fields');
      return;
    }

    if (createTaskMutation.isPending) {
      return; // Prevent concurrent calls
    }

    const payload: PCTaskAssignmentPayload = {
      projectId,
      recordId: recordId!,
      taskType,
      assignedTo: salesRepEmail,
      name: taskName || formatTaskName(taskType),
      description: taskDescription,
      dueDate: dueDate || null,
      priority
    };

    await createTaskMutation.mutateAsync(payload);
  };

  const handleTaskTypeChange = (type: PCTaskType) => {
    setTaskType(type);
    setTaskName(formatTaskName(type));
    setTaskDescription(getTaskTypeDescription(type));
  };

  // Group tasks by status
  const activeTasks = tasks.filter(task => ['Not Started', 'In Progress'].includes(task.status));
  const completedTasks = tasks.filter(task => task.status === 'Completed');
  const blockedTasks = tasks.filter(task => task.status === 'Blocked');

  if (isLoading) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <CheckSquare className="h-5 w-5" />
            Tasks
          </CardTitle>
        </CardHeader>
        <CardContent>
          <div className="space-y-3">
            {[1, 2, 3].map((i) => (
              <div key={i} className="animate-pulse">
                <div className="h-20 bg-gray-200 rounded-lg"></div>
              </div>
            ))}
          </div>
        </CardContent>
      </Card>
    );
  }

  return (
    <div className="space-y-6">
      {/* Task List */}
      <Card>
        <CardHeader>
          <div className="flex items-center justify-between">
            <CardTitle className="flex items-center gap-2">
              <CheckSquare className="h-5 w-5" />
              Tasks for {customerName}
            </CardTitle>
            <AlertDialog open={showCreateDialog} onOpenChange={setShowCreateDialog}>
              <AlertDialogTrigger asChild>
                <Button className="flex items-center gap-2">
                  <Plus className="h-4 w-4" />
                  Create Task
                </Button>
              </AlertDialogTrigger>
              <AlertDialogContent className="max-w-2xl">
                <AlertDialogHeader>
                  <AlertDialogTitle>Assign Task to {salesRepName}</AlertDialogTitle>
                </AlertDialogHeader>
                
                <div className="space-y-4">
                  {/* Task Type */}
                  <div>
                    <Label htmlFor="taskType">Task Type *</Label>
                    <Select value={taskType || ''} onValueChange={handleTaskTypeChange}>
                      <SelectTrigger>
                        <SelectValue placeholder="Select task type" />
                      </SelectTrigger>
                      <SelectContent>
                        <SelectItem value="callback_customer">
                          <div className="flex items-center gap-2">
                            <Phone className="h-4 w-4" />
                            Request Callback
                          </div>
                        </SelectItem>
                        <SelectItem value="collect_documents">
                          <div className="flex items-center gap-2">
                            <FileText className="h-4 w-4" />
                            Collect Documents
                          </div>
                        </SelectItem>
                        <SelectItem value="clarify_pricing">
                          <div className="flex items-center gap-2">
                            <DollarSign className="h-4 w-4" />
                            Clarify Pricing
                          </div>
                        </SelectItem>
                        <SelectItem value="handle_objection">
                          <div className="flex items-center gap-2">
                            <MessageSquare className="h-4 w-4" />
                            Handle Objection
                          </div>
                        </SelectItem>
                        <SelectItem value="schedule_site_visit">
                          <div className="flex items-center gap-2">
                            <Calendar className="h-4 w-4" />
                            Schedule Site Visit
                          </div>
                        </SelectItem>
                        <SelectItem value="resolve_hoa_issue">
                          <div className="flex items-center gap-2">
                            <Home className="h-4 w-4" />
                            Resolve HOA Issue
                          </div>
                        </SelectItem>
                      </SelectContent>
                    </Select>
                  </div>

                  {/* Task Name */}
                  <div>
                    <Label htmlFor="taskName">Task Name</Label>
                    <Input
                      id="taskName"
                      value={taskName}
                      onChange={(e) => setTaskName(e.target.value)}
                      placeholder="Auto-filled based on task type"
                    />
                  </div>

                  {/* Description */}
                  <div>
                    <Label htmlFor="description">Description *</Label>
                    <Textarea
                      id="description"
                      value={taskDescription}
                      onChange={(e) => setTaskDescription(e.target.value)}
                      placeholder={taskType ? getTaskTypeDescription(taskType) : 'Enter task description...'}
                      rows={4}
                    />
                  </div>

                  {/* Priority */}
                  <div>
                    <Label>Priority</Label>
                    <RadioGroup value={priority} onValueChange={(value) => setPriority(value as 'high' | 'normal' | 'low')}>
                      <div className="flex items-center space-x-2">
                        <RadioGroupItem value="high" id="high" />
                        <Label htmlFor="high" className="text-red-600">High</Label>
                      </div>
                      <div className="flex items-center space-x-2">
                        <RadioGroupItem value="normal" id="normal" />
                        <Label htmlFor="normal" className="text-yellow-600">Normal</Label>
                      </div>
                      <div className="flex items-center space-x-2">
                        <RadioGroupItem value="low" id="low" />
                        <Label htmlFor="low" className="text-gray-600">Low</Label>
                      </div>
                    </RadioGroup>
                  </div>

                  {/* Due Date */}
                  <div>
                    <Label htmlFor="dueDate">Due Date (Optional)</Label>
                    <Input
                      id="dueDate"
                      type="date"
                      value={dueDate}
                      onChange={(e) => setDueDate(e.target.value)}
                      min={new Date().toISOString().split('T')[0]}
                    />
                  </div>
                </div>

                <div className="flex justify-end gap-2 pt-4">
                  <Button variant="outline" onClick={() => setShowCreateDialog(false)}>
                    Cancel
                  </Button>
                  <Button 
                    onClick={handleCreateTask}
                    disabled={createTaskMutation.isPending || !taskType || !taskDescription.trim()}
                  >
                    {createTaskMutation.isPending ? 'Creating...' : 'Create Task'}
                  </Button>
                </div>
              </AlertDialogContent>
            </AlertDialog>
          </div>
        </CardHeader>
        <CardContent>
          {tasks.length === 0 ? (
            <div className="text-center py-8 text-gray-500">
              <CheckSquare className="h-12 w-12 mx-auto mb-4 opacity-50" />
              <p className="text-lg font-medium">No tasks assigned yet</p>
              <p className="text-sm">Create your first task to get started</p>
            </div>
          ) : (
            <div className="space-y-6">
              {/* Active Tasks */}
              {activeTasks.length > 0 && (
                <div>
                  <h3 className="text-sm font-medium text-gray-700 mb-3">Active Tasks</h3>
                  <div className="grid gap-3">
                    {activeTasks.map((task) => (
                      <Card key={task.recordId} className="p-4">
                        <div className="flex items-start justify-between">
                          <div className="flex items-start gap-3">
                            {getTaskTypeIcon(task.taskType)}
                            <div className="flex-1">
                              <div className="flex items-center gap-2 mb-1">
                                <h4 className="font-medium">{task.name}</h4>
                                <Badge className={getStatusColor(task.status)}>
                                  {task.status}
                                </Badge>
                                <Badge className={getPriorityColor(task.priority)}>
                                  {task.priority}
                                </Badge>
                              </div>
                              <p className="text-sm text-gray-600 mb-2">{task.description}</p>
                              <div className="flex items-center gap-4 text-xs text-gray-500">
                                <span className="flex items-center gap-1">
                                  <User className="h-3 w-3" />
                                  {task.assignedTo}
                                </span>
                                {task.dueDate && (
                                  <span className="flex items-center gap-1">
                                    <Clock className="h-3 w-3" />
                                    {formatDueDate(task.dueDate)}
                                  </span>
                                )}
                              </div>
                            </div>
                          </div>
                          <Button
                            variant="outline"
                            size="sm"
                            onClick={() => {
                              setSelectedTaskId(task.recordId);
                              setShowTaskMessages(true);
                            }}
                          >
                            <MessageSquare className="h-4 w-4 mr-1" />
                            Messages
                          </Button>
                        </div>
                      </Card>
                    ))}
                  </div>
                </div>
              )}

              {/* Completed Tasks */}
              {completedTasks.length > 0 && (
                <div>
                  <h3 className="text-sm font-medium text-gray-700 mb-3">Completed Tasks</h3>
                  <div className="grid gap-3">
                    {completedTasks.map((task) => (
                      <Card key={task.recordId} className="p-4 bg-green-50 border-green-200">
                        <div className="flex items-start gap-3">
                          <CheckSquare className="h-4 w-4 text-green-600 mt-0.5" />
                          <div className="flex-1">
                            <div className="flex items-center gap-2 mb-1">
                              <h4 className="font-medium text-green-800">{task.name}</h4>
                              <Badge className="bg-green-100 text-green-800 border-green-200">
                                Completed
                              </Badge>
                            </div>
                            <p className="text-sm text-green-700">{task.description}</p>
                          </div>
                        </div>
                      </Card>
                    ))}
                  </div>
                </div>
              )}

              {/* Blocked Tasks */}
              {blockedTasks.length > 0 && (
                <div>
                  <h3 className="text-sm font-medium text-gray-700 mb-3">Blocked Tasks</h3>
                  <div className="grid gap-3">
                    {blockedTasks.map((task) => (
                      <Card key={task.recordId} className="p-4 bg-red-50 border-red-200">
                        <div className="flex items-start gap-3">
                          <AlertCircle className="h-4 w-4 text-red-600 mt-0.5" />
                          <div className="flex-1">
                            <div className="flex items-center gap-2 mb-1">
                              <h4 className="font-medium text-red-800">{task.name}</h4>
                              <Badge className="bg-red-100 text-red-800 border-red-200">
                                Blocked
                              </Badge>
                            </div>
                            <p className="text-sm text-red-700">{task.description}</p>
                          </div>
                        </div>
                      </Card>
                    ))}
                  </div>
                </div>
              )}
            </div>
          )}
        </CardContent>
      </Card>

      {/* Task Messages */}
      {showTaskMessages && selectedTaskId && (
        <MessageThread
          projectId={projectId}
          recordId={recordId}
          taskId={selectedTaskId}
          currentUserEmail={session?.user?.email || ''}
          currentUserRole={session?.user?.role === 'operations_coordinator' || session?.user?.role === 'operations_manager' ? 'pc' : 'rep'}
        />
      )}
    </div>
  );
}
