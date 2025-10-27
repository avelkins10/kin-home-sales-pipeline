'use client';

import React, { useState } from 'react';
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
  User,
  ChevronDown,
  ChevronRight,
  FileText,
  Lock,
  XCircle,
  Zap,
  Home,
  FileWarning,
  HelpCircle
} from 'lucide-react';
import type {
  PCInspectionProject,
  PCInspectionStatus,
  PCInspectionFailureCategory,
  PCInspectionBlocker
} from '@/lib/types/operations';

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
  const [expandedRows, setExpandedRows] = useState<Set<number>>(new Set());

  const toggleRow = (recordId: number) => {
    setExpandedRows(prev => {
      const next = new Set(prev);
      if (next.has(recordId)) {
        next.delete(recordId);
      } else {
        next.add(recordId);
      }
      return next;
    });
  };

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

  const getFailureCategoryBadge = (category: PCInspectionFailureCategory | null) => {
    if (!category) return null;

    const configs = {
      electrical: { icon: Zap, text: 'Electrical', class: 'bg-yellow-100 text-yellow-800 border-yellow-300' },
      structural: { icon: Home, text: 'Structural', class: 'bg-orange-100 text-orange-800 border-orange-300' },
      code_violation: { icon: FileWarning, text: 'Code Violation', class: 'bg-red-100 text-red-800 border-red-300' },
      documentation: { icon: FileText, text: 'Documentation', class: 'bg-blue-100 text-blue-800 border-blue-300' },
      other: { icon: HelpCircle, text: 'Other', class: 'bg-gray-100 text-gray-800 border-gray-300' }
    };

    const config = configs[category];
    const Icon = config.icon;

    return (
      <Badge variant="outline" className={`${config.class} text-xs`}>
        <Icon className="h-3 w-3 mr-1" />
        {config.text}
      </Badge>
    );
  };

  const getBlockerBadges = (blockers: PCInspectionBlocker[]) => {
    const configs = {
      as_builts_pending: { icon: FileText, text: 'As-Builts Needed', class: 'bg-yellow-100 text-yellow-800 border-yellow-300' },
      permit_pending: { icon: FileWarning, text: 'Permit Pending', class: 'bg-orange-100 text-orange-800 border-orange-300' },
      on_hold: { icon: Lock, text: 'On Hold', class: 'bg-purple-100 text-purple-800 border-purple-300' },
      blocked: { icon: XCircle, text: 'Blocked', class: 'bg-red-100 text-red-800 border-red-300' },
      ready: { icon: CheckCircle, text: 'Ready', class: 'bg-green-100 text-green-800 border-green-300' }
    };

    return blockers.map((blocker, idx) => {
      const config = configs[blocker];
      const Icon = config.icon;

      return (
        <Badge key={idx} variant="outline" className={`${config.class} text-xs`}>
          <Icon className="h-3 w-3 mr-1" />
          {config.text}
        </Badge>
      );
    });
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
                <th className="pb-3 pr-2 w-8"></th>
                <th className="pb-3 pr-4">Project</th>
                <th className="pb-3 pr-4">Customer</th>
                <th className="pb-3 pr-4">Office</th>
                <th className="pb-3 pr-4">Date</th>
                <th className="pb-3 pr-4">Days</th>
                {status === 'waiting_for_inspection' && (
                  <th className="pb-3 pr-4">Blockers</th>
                )}
                {status === 'inspection_failed' && (
                  <th className="pb-3 pr-4">Category</th>
                )}
                <th className="pb-3 pr-4">Rep</th>
                <th className="pb-3">Actions</th>
              </tr>
            </thead>
            <tbody>
              {projects.map((project) => {
                const isExpanded = expandedRows.has(project.recordId);
                return (
                  <React.Fragment key={project.recordId}>
                <tr className="border-b hover:bg-gray-50 transition-colors">
                  <td className="py-3 pr-2">
                    <Button
                      variant="ghost"
                      size="sm"
                      className="h-6 w-6 p-0"
                      onClick={(e) => {
                        e.stopPropagation();
                        toggleRow(project.recordId);
                      }}
                    >
                      {isExpanded ? (
                        <ChevronDown className="h-4 w-4" />
                      ) : (
                        <ChevronRight className="h-4 w-4" />
                      )}
                    </Button>
                  </td>
                  <td className="py-3 pr-4">
                    <div className="font-medium text-gray-900">#{project.projectId}</div>
                    <div className="text-xs text-gray-500">{project.customerName}</div>
                  </td>
                  <td className="py-3 pr-4">
                    <div className="text-xs text-gray-500 flex items-center">
                      <Phone className="h-3 w-3 mr-1" />
                      {project.customerPhone || 'N/A'}
                    </div>
                  </td>
                  <td className="py-3 pr-4">
                    <div className="flex items-center text-sm text-gray-600">
                      <Building2 className="h-3 w-3 mr-1" />
                      {project.salesOffice || 'N/A'}
                    </div>
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
                  {status === 'waiting_for_inspection' && (
                    <td className="py-3 pr-4">
                      <div className="flex flex-wrap gap-1">
                        {getBlockerBadges(project.blockers)}
                      </div>
                    </td>
                  )}
                  {status === 'inspection_failed' && (
                    <td className="py-3 pr-4">
                      {getFailureCategoryBadge(project.failureCategory)}
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

                {/* Expandable Row */}
                {isExpanded && (
                  <tr className="bg-gray-50">
                    <td colSpan={status === 'waiting_for_inspection' || status === 'inspection_failed' ? 9 : 8} className="py-4 px-6">
                      <div className="space-y-3">
                        {/* Failure Details (for failed inspections) */}
                        {status === 'inspection_failed' && project.failureReason && (
                          <div>
                            <div className="text-xs font-semibold text-gray-700 mb-1">Failure Reason:</div>
                            <div className="text-sm text-gray-900 bg-white p-3 rounded border border-red-200">
                              {project.failureReason}
                            </div>
                          </div>
                        )}

                        {/* Blocker Details (for waiting inspections) */}
                        {status === 'waiting_for_inspection' && (
                          <div className="grid grid-cols-2 gap-4">
                            {project.blockReason && (
                              <div>
                                <div className="text-xs font-semibold text-gray-700 mb-1">Block Reason:</div>
                                <div className="text-sm text-gray-900">{project.blockReason}</div>
                              </div>
                            )}
                            {project.holdReason && (
                              <div>
                                <div className="text-xs font-semibold text-gray-700 mb-1">Hold Reason:</div>
                                <div className="text-sm text-gray-900">{project.holdReason}</div>
                              </div>
                            )}
                            {project.permitStatus && (
                              <div>
                                <div className="text-xs font-semibold text-gray-700 mb-1">Permit Status:</div>
                                <div className="text-sm text-gray-900">{project.permitStatus}</div>
                              </div>
                            )}
                            <div>
                              <div className="text-xs font-semibold text-gray-700 mb-1">As-Builts:</div>
                              <div className="text-sm">
                                {project.asBuiltSubmitted ? (
                                  <span className="text-green-600 flex items-center">
                                    <CheckCircle className="h-4 w-4 mr-1" />
                                    Submitted
                                  </span>
                                ) : (
                                  <span className="text-yellow-600 flex items-center">
                                    <AlertTriangle className="h-4 w-4 mr-1" />
                                    Pending
                                  </span>
                                )}
                              </div>
                            </div>
                          </div>
                        )}

                        {/* Common Details */}
                        <div className="grid grid-cols-3 gap-4 pt-2 border-t border-gray-200">
                          <div>
                            <div className="text-xs font-semibold text-gray-700 mb-1">Coordinator:</div>
                            <div className="text-sm text-gray-900">{project.coordinatorEmail}</div>
                          </div>
                          <div>
                            <div className="text-xs font-semibold text-gray-700 mb-1">Sales Rep:</div>
                            <div className="text-sm text-gray-900">{project.salesRepName}</div>
                            <div className="text-xs text-gray-500">{project.salesRepEmail}</div>
                          </div>
                          <div>
                            <div className="text-xs font-semibold text-gray-700 mb-1">Lender:</div>
                            <div className="text-sm text-gray-900">{project.lenderName || 'N/A'}</div>
                          </div>
                        </div>
                      </div>
                    </td>
                  </tr>
                )}
              </React.Fragment>
              );
              })}
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
