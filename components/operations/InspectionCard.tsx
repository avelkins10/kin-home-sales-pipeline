'use client';

import React, { useState } from 'react';
import { Card, CardContent } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import {
  Calendar,
  Clock,
  AlertTriangle,
  CheckCircle,
  Phone,
  Building2,
  User,
  ChevronDown,
  ChevronUp,
  FileText,
  Lock,
  XCircle,
  Zap,
  Home,
  FileWarning,
  HelpCircle,
  ExternalLink
} from 'lucide-react';
import type {
  PCInspectionProject,
  PCInspectionStatus,
  PCInspectionFailureCategory,
  PCInspectionBlocker
} from '@/lib/types/operations';
import { ProjectDetailModal } from './ProjectDetailModal';

interface InspectionCardProps {
  project: PCInspectionProject;
}

export function InspectionCard({ project }: InspectionCardProps) {
  const [isExpanded, setIsExpanded] = useState(false);
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

  const getDateDisplay = () => {
    switch (project.inspectionStatus) {
      case 'waiting_for_inspection':
        return (
          <div className="flex items-center text-sm text-gray-600">
            <Calendar className="h-4 w-4 mr-1" />
            Installed: {formatDate(project.installCompletedDate)}
          </div>
        );
      case 'inspection_scheduled':
        return (
          <div className="flex items-center text-sm text-blue-600">
            <Calendar className="h-4 w-4 mr-1" />
            Scheduled: {formatDate(project.inspectionScheduledDate)}
          </div>
        );
      case 'inspection_failed':
        return (
          <div className="flex items-center text-sm text-red-600">
            <AlertTriangle className="h-4 w-4 mr-1" />
            Failed: {formatDate(project.inspectionFailedDate)}
          </div>
        );
      case 'inspection_passed':
        return (
          <div className="flex items-center text-sm text-green-600">
            <CheckCircle className="h-4 w-4 mr-1" />
            Passed: {formatDate(project.inspectionPassedDate)}
          </div>
        );
    }
  };

  return (
    <>
      <Card className="hover:shadow-md transition-shadow">
        <CardContent className="p-4">
          {/* Header */}
          <div className="flex items-start justify-between mb-3">
            <div className="flex items-center space-x-3">
              <div>
                <div className="font-semibold text-lg text-gray-900">#{project.projectId}</div>
                <div className="text-sm text-gray-600">{project.customerName}</div>
              </div>
            </div>
            <div className="flex items-center space-x-2">
              {getStatusBadge(project.inspectionStatus)}
              <Button
                variant="ghost"
                size="sm"
                className="h-8 w-8 p-0"
                onClick={() => setIsExpanded(!isExpanded)}
              >
                {isExpanded ? (
                  <ChevronUp className="h-4 w-4" />
                ) : (
                  <ChevronDown className="h-4 w-4" />
                )}
              </Button>
            </div>
          </div>

          {/* Main Info Grid */}
          <div className="grid grid-cols-2 gap-3 mb-3">
            <div>
              <div className="text-xs text-gray-500 mb-1">Office</div>
              <div className="flex items-center text-sm text-gray-900">
                <Building2 className="h-3 w-3 mr-1" />
                {project.salesOffice || 'N/A'}
              </div>
            </div>
            <div>
              <div className="text-xs text-gray-500 mb-1">Sales Rep</div>
              <div className="flex items-center text-sm text-gray-900">
                <User className="h-3 w-3 mr-1" />
                {project.salesRepName || 'N/A'}
              </div>
            </div>
            <div>
              <div className="text-xs text-gray-500 mb-1">Date</div>
              {getDateDisplay()}
            </div>
            <div>
              <div className="text-xs text-gray-500 mb-1">Days in Status</div>
              <div className={`text-sm flex items-center ${getDaysColor(project.daysInStatus, project.inspectionStatus)}`}>
                <Clock className="h-3 w-3 mr-1" />
                {project.daysInStatus} {project.daysInStatus === 1 ? 'day' : 'days'}
              </div>
            </div>
          </div>

          {/* Failure Category or Blockers */}
          {project.inspectionStatus === 'inspection_failed' && project.failureCategory && (
            <div className="mb-3">
              <div className="text-xs text-gray-500 mb-1">Failure Category</div>
              <div className="flex flex-wrap gap-1">
                {getFailureCategoryBadge(project.failureCategory)}
              </div>
            </div>
          )}

          {project.inspectionStatus === 'waiting_for_inspection' && project.blockers.length > 0 && (
            <div className="mb-3">
              <div className="text-xs text-gray-500 mb-1">Blockers</div>
              <div className="flex flex-wrap gap-1">
                {getBlockerBadges(project.blockers)}
              </div>
            </div>
          )}

          {/* Actions */}
          <div className="flex items-center space-x-2 pt-3 border-t border-gray-200">
            <Button
              size="sm"
              variant="outline"
              className="flex-1"
              onClick={() => setModalOpen(true)}
            >
              <ExternalLink className="h-3 w-3 mr-1" />
              View Details
            </Button>
            <Button
              size="sm"
              variant="outline"
              onClick={() => window.open(`tel:${project.customerPhone}`, '_self')}
            >
              <Phone className="h-3 w-3" />
            </Button>
          </div>

          {/* Expanded Details */}
          {isExpanded && (
            <div className="mt-4 pt-4 border-t border-gray-200 space-y-3">
              {/* Failure Details (for failed inspections) */}
              {project.inspectionStatus === 'inspection_failed' && project.failureReason && (
                <div>
                  <div className="text-xs font-semibold text-gray-700 mb-1">Failure Reason:</div>
                  <div className="text-sm text-gray-900 bg-red-50 p-3 rounded border border-red-200">
                    {project.failureReason}
                  </div>
                </div>
              )}

              {/* Blocker Details (for waiting inspections) */}
              {project.inspectionStatus === 'waiting_for_inspection' && (
                <div className="grid grid-cols-2 gap-3">
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

              {/* Contact Details */}
              <div className="grid grid-cols-2 gap-3 pt-2 border-t border-gray-100">
                <div>
                  <div className="text-xs font-semibold text-gray-700 mb-1">Customer Phone:</div>
                  <div className="text-sm text-gray-900 flex items-center">
                    <Phone className="h-3 w-3 mr-1" />
                    {project.customerPhone || 'N/A'}
                  </div>
                </div>
                <div>
                  <div className="text-xs font-semibold text-gray-700 mb-1">Coordinator:</div>
                  <div className="text-sm text-gray-900">{project.coordinatorEmail}</div>
                </div>
                {project.lenderName && (
                  <div>
                    <div className="text-xs font-semibold text-gray-700 mb-1">Lender:</div>
                    <div className="text-sm text-gray-900">{project.lenderName}</div>
                  </div>
                )}
              </div>
            </div>
          )}
        </CardContent>
      </Card>

      <ProjectDetailModal
        recordId={project.recordId}
        isOpen={modalOpen}
        onClose={() => setModalOpen(false)}
      />
    </>
  );
}
