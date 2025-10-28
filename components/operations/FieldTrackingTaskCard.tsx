'use client';

import { Card, CardContent } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { Clock, MapPin, User, ExternalLink, Copy } from 'lucide-react';
import type { FieldTrackingTask } from '@/lib/types/operations';
import { format } from 'date-fns';
import { toast } from 'sonner';

interface FieldTrackingTaskCardProps {
  task: FieldTrackingTask;
  onClick: () => void;
}

export function FieldTrackingTaskCard({ task, onClick }: FieldTrackingTaskCardProps) {
  const getStatusColor = (status: string | null) => {
    switch (status) {
      case 'NOT_STARTED':
        return 'bg-gray-100 text-gray-800 border-gray-300';
      case 'ENROUTE':
        return 'bg-blue-100 text-blue-800 border-blue-300';
      case 'STARTED':
        return 'bg-yellow-100 text-yellow-800 border-yellow-300';
      case 'COMPLETE':
        return 'bg-green-100 text-green-800 border-green-300';
      case 'LATE':
      case 'NOSHOW':
        return 'bg-red-100 text-red-800 border-red-300';
      case 'CANCELLED':
        return 'bg-gray-100 text-gray-600 border-gray-300';
      default:
        return 'bg-gray-100 text-gray-600 border-gray-300';
    }
  };

  const getTaskTypeColor = (type: string | null) => {
    switch (type) {
      case 'survey':
        return 'bg-purple-100 text-purple-800';
      case 'install':
        return 'bg-blue-100 text-blue-800';
      case 'inspection':
        return 'bg-green-100 text-green-800';
      case 'service':
        return 'bg-orange-100 text-orange-800';
      default:
        return 'bg-gray-100 text-gray-800';
    }
  };

  const copyTrackerUrl = (e: React.MouseEvent) => {
    e.stopPropagation();
    if (task.tracker_url) {
      navigator.clipboard.writeText(task.tracker_url);
      toast.success('Tracker URL copied to clipboard');
    }
  };

  return (
    <Card 
      className="hover:shadow-md transition-shadow cursor-pointer border-l-4"
      style={{
        borderLeftColor: task.current_status === 'LATE' || task.current_status === 'NOSHOW' ? '#ef4444' :
                         task.current_status === 'STARTED' ? '#eab308' :
                         task.current_status === 'ENROUTE' ? '#3b82f6' :
                         task.current_status === 'COMPLETE' ? '#22c55e' : '#9ca3af'
      }}
      onClick={onClick}
    >
      <CardContent className="p-4">
        <div className="flex items-start justify-between mb-3">
          <div className="flex gap-2">
            <Badge variant="outline" className={getTaskTypeColor(task.task_type)}>
              {task.task_type?.toUpperCase() || 'TASK'}
            </Badge>
            <Badge variant="outline" className={getStatusColor(task.current_status)}>
              {task.current_status || 'PENDING'}
            </Badge>
          </div>
          <div className="flex gap-1">
            {task.tracker_url && (
              <>
                <Button
                  variant="ghost"
                  size="sm"
                  onClick={copyTrackerUrl}
                  className="h-8 w-8 p-0"
                >
                  <Copy className="h-4 w-4" />
                </Button>
                <Button
                  variant="ghost"
                  size="sm"
                  onClick={(e) => {
                    e.stopPropagation();
                    window.open(task.tracker_url!, '_blank');
                  }}
                  className="h-8 w-8 p-0"
                >
                  <ExternalLink className="h-4 w-4" />
                </Button>
              </>
            )}
          </div>
        </div>

        <div className="space-y-2">
          <div>
            <h3 className="font-semibold text-lg">{task.customer_name || 'Unknown Customer'}</h3>
            <p className="text-sm text-gray-600">Project ID: {task.quickbase_project_id}</p>
          </div>

          {task.scheduled_start && (
            <div className="flex items-center text-sm text-gray-600">
              <Clock className="mr-2 h-4 w-4" />
              {format(new Date(task.scheduled_start), 'MMM d, yyyy h:mm a')}
            </div>
          )}

          {task.customer_address && (
            <div className="flex items-center text-sm text-gray-600">
              <MapPin className="mr-2 h-4 w-4" />
              {task.customer_address}
            </div>
          )}

          {task.entity_names && task.entity_names.length > 0 && (
            <div className="flex items-center text-sm text-gray-600">
              <User className="mr-2 h-4 w-4" />
              {task.entity_names.join(', ')}
            </div>
          )}

          {task.latest_status && task.latest_status_time && (
            <div className="text-xs text-gray-500 mt-2 pt-2 border-t">
              Last update: {task.latest_status} at {format(new Date(task.latest_status_time), 'h:mm a')}
            </div>
          )}
        </div>
      </CardContent>
    </Card>
  );
}

