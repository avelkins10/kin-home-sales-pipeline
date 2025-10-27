'use client';

import { useState } from 'react';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { Skeleton } from '@/components/ui/skeleton';
import { ProjectDetailModal } from './ProjectDetailModal';
import {
  Calendar,
  Clock,
  AlertTriangle,
  CheckCircle,
  Phone,
  Mail,
  Building2,
  User
} from 'lucide-react';
import type { PCInspectionProject, PCInspectionStatus } from '@/lib/types/operations';

interface InspectionsTableProps {
  projects: PCInspectionProject[];
  status: PCInspectionStatus;
  title: string;
  icon: React.ReactNode;
}

function InspectionTableSkeleton() {
  return (
    <Card>
      <CardHeader>
        <CardTitle>Loading Inspections...</CardTitle>
      </CardHeader>
      <CardContent>
        <div className="space-y-3">
          {Array.from({ length: 3 }).map((_, i) => (
            <div key={i} className="flex items-center justify-between p-3 border rounded-lg">
              <div className="flex items-center space-x-3 flex-1">
                <Skeleton className="h-6 w-24 rounded-full" />
                <div className="space-y-2 flex-1">
                  <Skeleton className="h-4 w-48" />
                  <Skeleton className="h-3 w-32" />
                </div>
              </div>
              <Skeleton className="h-8 w-24" />
            </div>
          ))}
        </div>
      </CardContent>
    </Card>
  );
}

export function InspectionsTable({ projects, status, title, icon }: InspectionsTableProps) {
  const [selectedRecordId, setSelectedRecordId] = useState<number | null>(null);
  const [modalOpen, setModalOpen] = useState(false);

  const getStatusBadge = (inspectionStatus: PCInspectionStatus) => {
    switch (inspectionStatus) {
      case 'waiting_for_inspection':
        return <Badge variant="outline" className="bg-yellow-50 text-yellow-700 border-yellow-200">Waiting</Badge>;
      case 'inspection_scheduled':
        return <Badge variant="outline" className="bg-blue-50 text-blue-700 border-blue-200">Scheduled</Badge>;
      case 'inspection_failed':
        return <Badge variant="outline" className="bg-red-50 text-red-700 border-red-200">Failed</Badge>;
      case 'inspection_passed':
        return <Badge variant="outline" className="bg-green-50 text-green-700 border-green-200">Passed</Badge>;
    }
  };

  const getDaysColor = (days: number, inspectionStatus: PCInspectionStatus) => {
    // Different thresholds for different statuses
    if (inspectionStatus === 'waiting_for_inspection') {
      if (days > 7) return 'text-red-600 font-semibold';
      if (days > 3) return 'text-orange-600';
      return 'text-gray-600';
    }
    if (inspectionStatus === 'inspection_failed') {
      if (days > 3) return 'text-red-600 font-semibold';
      return 'text-orange-600';
    }
    return 'text-gray-600';
  };

  const formatDate = (dateStr: string | null) => {
    if (!dateStr) return 'N/A';
    try {
      const date = new Date(dateStr);
      if (isNaN(date.getTime())) return 'Invalid date';
      return date.toLocaleDateString('en-US', { month: 'short', day: 'numeric', year: 'numeric' });
    } catch {
      return 'Invalid date';
    }
  };

  if (projects.length === 0) {
    return (
      <Card>
        <CardHeader>
          <div className="flex items-center space-x-2">
            {icon}
            <CardTitle>{title}</CardTitle>
            <Badge variant="outline" className="ml-2">0</Badge>
          </div>
        </CardHeader>
        <CardContent>
          <div className="text-center py-8 text-gray-500">
            <CheckCircle className="h-12 w-12 mx-auto mb-4 text-gray-400" />
            <p>No projects in this status</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  return (
    <Card>
      <CardHeader>
        <div className="flex items-center space-x-2">
          {icon}
          <CardTitle>{title}</CardTitle>
          <Badge variant="outline" className="ml-2">{projects.length}</Badge>
        </div>
      </CardHeader>
      <CardContent>
        <div className="overflow-x-auto">
          <table className="w-full">
            <thead>
              <tr className="border-b text-left text-sm text-gray-600">
                <th className="pb-3 pr-4">Project</th>
                <th className="pb-3 pr-4">Customer</th>
                <th className="pb-3 pr-4">Office</th>
                <th className="pb-3 pr-4">Status</th>
                <th className="pb-3 pr-4">Date</th>
                <th className="pb-3 pr-4">Days</th>
                {status === 'inspection_failed' && (
                  <th className="pb-3 pr-4">Failure Reason</th>
                )}
                <th className="pb-3 pr-4">Rep</th>
                <th className="pb-3">Actions</th>
              </tr>
            </thead>
            <tbody>
              {projects.map((project) => (
                <tr
                  key={project.recordId}
                  className="border-b hover:bg-gray-50 transition-colors cursor-pointer"
                  onClick={() => {
                    setSelectedRecordId(project.recordId);
                    setModalOpen(true);
                  }}
                >
                  <td className="py-3 pr-4">
                    <div className="font-medium text-gray-900">#{project.projectId}</div>
                  </td>
                  <td className="py-3 pr-4">
                    <div>
                      <div className="font-medium text-gray-900">{project.customerName}</div>
                      <div className="text-xs text-gray-500 flex items-center mt-1">
                        <Phone className="h-3 w-3 mr-1" />
                        {project.customerPhone || 'N/A'}
                      </div>
                    </div>
                  </td>
                  <td className="py-3 pr-4">
                    <div className="flex items-center text-sm text-gray-600">
                      <Building2 className="h-3 w-3 mr-1" />
                      {project.salesOffice || 'N/A'}
                    </div>
                  </td>
                  <td className="py-3 pr-4">
                    {getStatusBadge(project.inspectionStatus)}
                  </td>
                  <td className="py-3 pr-4">
                    <div className="text-sm">
                      {status === 'waiting_for_inspection' && (
                        <div className="text-gray-600">
                          Installed: {formatDate(project.installCompletedDate)}
                        </div>
                      )}
                      {status === 'inspection_scheduled' && (
                        <div className="text-blue-600 flex items-center">
                          <Calendar className="h-3 w-3 mr-1" />
                          {formatDate(project.inspectionScheduledDate)}
                        </div>
                      )}
                      {status === 'inspection_failed' && (
                        <div className="text-red-600">
                          Failed: {formatDate(project.inspectionFailedDate)}
                        </div>
                      )}
                      {status === 'inspection_passed' && (
                        <div className="text-green-600">
                          Passed: {formatDate(project.inspectionPassedDate)}
                        </div>
                      )}
                    </div>
                  </td>
                  <td className="py-3 pr-4">
                    <div className={`text-sm flex items-center ${getDaysColor(project.daysInStatus, project.inspectionStatus)}`}>
                      <Clock className="h-3 w-3 mr-1" />
                      {project.daysInStatus} {project.daysInStatus === 1 ? 'day' : 'days'}
                    </div>
                  </td>
                  {status === 'inspection_failed' && (
                    <td className="py-3 pr-4">
                      <div className="text-sm text-gray-700 max-w-xs truncate" title={project.failureReason || 'No reason provided'}>
                        {project.failureReason || <span className="text-gray-400 italic">No reason</span>}
                      </div>
                    </td>
                  )}
                  <td className="py-3 pr-4">
                    <div className="text-sm">
                      <div className="font-medium text-gray-900">{project.salesRepName || 'N/A'}</div>
                      {project.lenderName && (
                        <div className="text-xs text-gray-500">{project.lenderName}</div>
                      )}
                    </div>
                  </td>
                  <td className="py-3">
                    <Button
                      size="sm"
                      variant="outline"
                      onClick={(e) => {
                        e.stopPropagation();
                        setSelectedRecordId(project.recordId);
                        setModalOpen(true);
                      }}
                    >
                      View
                    </Button>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </CardContent>

      <ProjectDetailModal
        recordId={selectedRecordId}
        isOpen={modalOpen}
        onClose={() => {
          setModalOpen(false);
          setSelectedRecordId(null);
        }}
      />
    </Card>
  );
}

export { InspectionTableSkeleton };
