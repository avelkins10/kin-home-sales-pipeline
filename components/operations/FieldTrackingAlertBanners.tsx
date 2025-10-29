'use client';

import { AlertCircle, AlertTriangle, XCircle, Clock, Phone, ChevronRight } from 'lucide-react';
import { Alert, AlertDescription, AlertTitle } from '@/components/ui/alert';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import type { FieldTrackingTask } from '@/lib/types/operations';

interface FieldTrackingAlertBannersProps {
  tasks: FieldTrackingTask[];
  onTaskClick: (taskId: string) => void;
}

export function FieldTrackingAlertBanners({ tasks, onTaskClick }: FieldTrackingAlertBannersProps) {
  // Filter tasks by critical statuses
  const lateTasks = tasks.filter((t) => t.current_status === 'LATE');
  const noshowTasks = tasks.filter((t) => t.current_status === 'NOSHOW');
  const exceptionTasks = tasks.filter((t) => t.current_status === 'EXCEPTION');

  // Don't show anything if no critical tasks
  if (lateTasks.length === 0 && noshowTasks.length === 0 && exceptionTasks.length === 0) {
    return null;
  }

  return (
    <div className="space-y-3">
      {/* LATE Tasks - Red Alert */}
      {lateTasks.length > 0 && (
        <Alert className="border-red-500 bg-red-50 dark:bg-red-950/20">
          <AlertCircle className="h-5 w-5 text-red-600" />
          <div className="flex-1">
            <AlertTitle className="text-red-900 dark:text-red-100 font-semibold">
              {lateTasks.length} {lateTasks.length === 1 ? 'Task' : 'Tasks'} Running Late
            </AlertTitle>
            <AlertDescription className="text-red-800 dark:text-red-200 mt-2">
              <div className="flex flex-col gap-2">
                <p className="text-sm">
                  {lateTasks.length === 1
                    ? 'A task is running behind schedule and needs immediate attention.'
                    : 'Multiple tasks are running behind schedule and need immediate attention.'}
                </p>
                <div className="flex flex-wrap gap-2 mt-1">
                  {lateTasks.slice(0, 3).map((task) => (
                    <button
                      key={task.id}
                      onClick={() => onTaskClick(task.quickbase_project_id || '')}
                      className="flex items-center gap-2 px-3 py-1.5 bg-white dark:bg-gray-800 border border-red-300 rounded-lg hover:bg-red-100 dark:hover:bg-red-900/30 transition-colors text-sm"
                    >
                      <Clock className="h-3 w-3" />
                      <span className="font-medium">{task.customer_name || 'Unknown'}</span>
                      {task.task_type && (
                        <Badge variant="secondary" className="text-xs">
                          {task.task_type}
                        </Badge>
                      )}
                      <ChevronRight className="h-3 w-3 ml-auto" />
                    </button>
                  ))}
                  {lateTasks.length > 3 && (
                    <div className="px-3 py-1.5 text-sm text-red-700 dark:text-red-300 font-medium">
                      +{lateTasks.length - 3} more
                    </div>
                  )}
                </div>
              </div>
            </AlertDescription>
          </div>
        </Alert>
      )}

      {/* NOSHOW Tasks - Orange Alert */}
      {noshowTasks.length > 0 && (
        <Alert className="border-orange-500 bg-orange-50 dark:bg-orange-950/20">
          <XCircle className="h-5 w-5 text-orange-600" />
          <div className="flex-1">
            <AlertTitle className="text-orange-900 dark:text-orange-100 font-semibold">
              {noshowTasks.length} No-Show {noshowTasks.length === 1 ? 'Alert' : 'Alerts'}
            </AlertTitle>
            <AlertDescription className="text-orange-800 dark:text-orange-200 mt-2">
              <div className="flex flex-col gap-2">
                <p className="text-sm">
                  {noshowTasks.length === 1
                    ? 'A crew did not arrive at the scheduled location. Contact the crew immediately.'
                    : 'Multiple crews did not arrive at scheduled locations. Contact crews immediately.'}
                </p>
                <div className="flex flex-wrap gap-2 mt-1">
                  {noshowTasks.slice(0, 3).map((task) => (
                    <button
                      key={task.id}
                      onClick={() => onTaskClick(task.quickbase_project_id || '')}
                      className="flex items-center gap-2 px-3 py-1.5 bg-white dark:bg-gray-800 border border-orange-300 rounded-lg hover:bg-orange-100 dark:hover:bg-orange-900/30 transition-colors text-sm"
                    >
                      <Phone className="h-3 w-3" />
                      <span className="font-medium">{task.customer_name || 'Unknown'}</span>
                      {task.entity_names && task.entity_names.length > 0 && (
                        <Badge variant="secondary" className="text-xs">
                          {task.entity_names[0]}
                        </Badge>
                      )}
                      <ChevronRight className="h-3 w-3 ml-auto" />
                    </button>
                  ))}
                  {noshowTasks.length > 3 && (
                    <div className="px-3 py-1.5 text-sm text-orange-700 dark:text-orange-300 font-medium">
                      +{noshowTasks.length - 3} more
                    </div>
                  )}
                </div>
              </div>
            </AlertDescription>
          </div>
        </Alert>
      )}

      {/* EXCEPTION Tasks - Yellow Alert */}
      {exceptionTasks.length > 0 && (
        <Alert className="border-yellow-500 bg-yellow-50 dark:bg-yellow-950/20">
          <AlertTriangle className="h-5 w-5 text-yellow-600" />
          <div className="flex-1">
            <AlertTitle className="text-yellow-900 dark:text-yellow-100 font-semibold">
              {exceptionTasks.length} {exceptionTasks.length === 1 ? 'Exception' : 'Exceptions'} Reported
            </AlertTitle>
            <AlertDescription className="text-yellow-800 dark:text-yellow-200 mt-2">
              <div className="flex flex-col gap-2">
                <p className="text-sm">
                  {exceptionTasks.length === 1
                    ? 'An issue has been reported that requires attention.'
                    : 'Multiple issues have been reported that require attention.'}
                </p>
                <div className="flex flex-wrap gap-2 mt-1">
                  {exceptionTasks.slice(0, 3).map((task) => (
                    <button
                      key={task.id}
                      onClick={() => onTaskClick(task.quickbase_project_id || '')}
                      className="flex items-center gap-2 px-3 py-1.5 bg-white dark:bg-gray-800 border border-yellow-300 rounded-lg hover:bg-yellow-100 dark:hover:bg-yellow-900/30 transition-colors text-sm"
                    >
                      <AlertTriangle className="h-3 w-3" />
                      <span className="font-medium">{task.customer_name || 'Unknown'}</span>
                      {task.task_type && (
                        <Badge variant="secondary" className="text-xs">
                          {task.task_type}
                        </Badge>
                      )}
                      <ChevronRight className="h-3 w-3 ml-auto" />
                    </button>
                  ))}
                  {exceptionTasks.length > 3 && (
                    <div className="px-3 py-1.5 text-sm text-yellow-700 dark:text-yellow-300 font-medium">
                      +{exceptionTasks.length - 3} more
                    </div>
                  )}
                </div>
              </div>
            </AlertDescription>
          </div>
        </Alert>
      )}
    </div>
  );
}
