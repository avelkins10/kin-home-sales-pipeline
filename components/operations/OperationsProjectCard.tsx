'use client';

import Link from 'next/link';
import { Card } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { MilestoneStatusBadge } from './MilestoneStatusBadge';
import { extractFieldValue } from '@/lib/utils/quickbase-field-helpers';
import { AlertCircle, Clock, MapPin, DollarSign, Zap } from 'lucide-react';
import { formatQuickbaseDate } from '@/lib/utils/date-helpers';

interface OperationsProjectCardProps {
  project: any;
}

export function OperationsProjectCard({ project }: OperationsProjectCardProps) {
  // Format address
  const formatAddress = (address: string) => {
    if (!address) return '';
    const parts = address.split(',');
    return parts[0]?.trim() || address;
  };

  // Format date for display
  const formatDate = (dateField: any) => {
    if (!dateField) return null;
    try {
      const value = typeof dateField === 'object' && dateField.value ? dateField.value : dateField;
      if (!value) return null;
      const formatted = formatQuickbaseDate(value);
      if (!formatted) return null;
      return new Date(formatted).toLocaleDateString('en-US', {
        month: 'short',
        day: 'numeric',
        year: 'numeric'
      });
    } catch {
      return null;
    }
  };

  // Render milestone-specific details
  const renderMilestoneDetails = () => {
    const { currentMilestone } = project;

    switch (currentMilestone) {
      case 'intake':
        return (
          <div className="p-3 bg-orange-50 rounded-lg border border-orange-200">
            <div className="text-sm font-medium text-orange-900 mb-2">Intake Details</div>
            <div className="text-sm text-orange-700">
              {extractFieldValue(project.intakeStatus) || 'Processing intake'}
            </div>
          </div>
        );

      case 'survey':
        const surveyScheduled = formatDate(project.surveyScheduledDate);
        const surveySubmitted = formatDate(project.surveySubmittedDate);
        return (
          <div className="p-3 bg-purple-50 rounded-lg border border-purple-200">
            <div className="text-sm font-medium text-purple-900 mb-2">Survey Details</div>
            <div className="grid grid-cols-2 gap-2 text-sm">
              {surveyScheduled && (
                <div>
                  <div className="text-purple-700 text-xs">Scheduled</div>
                  <div className="font-medium text-purple-900">{surveyScheduled}</div>
                </div>
              )}
              {surveySubmitted && (
                <div>
                  <div className="text-purple-700 text-xs">Submitted</div>
                  <div className="font-medium text-purple-900">{surveySubmitted}</div>
                </div>
              )}
            </div>
          </div>
        );

      case 'design':
        const designStatus = extractFieldValue(project.designStatus);
        const cadApproved = formatDate(project.cadDesignApproved);
        return (
          <div className="p-3 bg-blue-50 rounded-lg border border-blue-200">
            <div className="text-sm font-medium text-blue-900 mb-2">Design Details</div>
            <div className="grid grid-cols-2 gap-2 text-sm">
              {designStatus && (
                <div>
                  <div className="text-blue-700 text-xs">Status</div>
                  <div className="font-medium text-blue-900">{designStatus}</div>
                </div>
              )}
              {cadApproved && (
                <div>
                  <div className="text-blue-700 text-xs">CAD Approved</div>
                  <div className="font-medium text-blue-900">{cadApproved}</div>
                </div>
              )}
            </div>
          </div>
        );

      case 'permitting':
        const permitStatus = extractFieldValue(project.permitStatus);
        const permitSubmitted = formatDate(project.permitSubmitted);
        const nemStatus = extractFieldValue(project.nemStatus);
        return (
          <div className="p-3 bg-teal-50 rounded-lg border border-teal-200">
            <div className="text-sm font-medium text-teal-900 mb-2">Permitting Details</div>
            <div className="grid grid-cols-2 gap-2 text-sm">
              {permitStatus && (
                <div>
                  <div className="text-teal-700 text-xs">AHJ Status</div>
                  <div className="font-medium text-teal-900">{permitStatus}</div>
                </div>
              )}
              {permitSubmitted && (
                <div>
                  <div className="text-teal-700 text-xs">Submitted</div>
                  <div className="font-medium text-teal-900">{permitSubmitted}</div>
                </div>
              )}
              {nemStatus && (
                <div className="col-span-2">
                  <div className="text-teal-700 text-xs">NEM Status</div>
                  <div className="font-medium text-teal-900">{nemStatus}</div>
                </div>
              )}
            </div>
          </div>
        );

      case 'install':
        const installScheduled = formatDate(project.installScheduledDate);
        const installStarted = formatDate(project.installStartedDate);
        return (
          <div className="p-3 bg-indigo-50 rounded-lg border border-indigo-200">
            <div className="text-sm font-medium text-indigo-900 mb-2">Install Details</div>
            <div className="grid grid-cols-2 gap-2 text-sm">
              {installScheduled && (
                <div>
                  <div className="text-indigo-700 text-xs">Scheduled</div>
                  <div className="font-medium text-indigo-900">{installScheduled}</div>
                </div>
              )}
              {installStarted && (
                <div>
                  <div className="text-indigo-700 text-xs">Started</div>
                  <div className="font-medium text-indigo-900">{installStarted}</div>
                </div>
              )}
            </div>
          </div>
        );

      case 'inspections':
        const inspectionScheduled = formatDate(project.inspectionScheduledDate);
        const inspectionFailed = formatDate(project.inspectionFailedDate);
        return (
          <div className="p-3 bg-amber-50 rounded-lg border border-amber-200">
            <div className="text-sm font-medium text-amber-900 mb-2">Inspection Details</div>
            <div className="grid grid-cols-2 gap-2 text-sm">
              {inspectionScheduled && (
                <div>
                  <div className="text-amber-700 text-xs">Scheduled</div>
                  <div className="font-medium text-amber-900">{inspectionScheduled}</div>
                </div>
              )}
              {inspectionFailed && (
                <div>
                  <div className="text-red-700 text-xs">Failed Date</div>
                  <div className="font-medium text-red-900">{inspectionFailed}</div>
                </div>
              )}
            </div>
          </div>
        );

      case 'pto':
        const ptoStatus = extractFieldValue(project.ptoStatus);
        const ptoSubmitted = formatDate(project.ptoSubmitted);
        return (
          <div className="p-3 bg-emerald-50 rounded-lg border border-emerald-200">
            <div className="text-sm font-medium text-emerald-900 mb-2">PTO Details</div>
            <div className="grid grid-cols-2 gap-2 text-sm">
              {ptoStatus && (
                <div>
                  <div className="text-emerald-700 text-xs">Status</div>
                  <div className="font-medium text-emerald-900">{ptoStatus}</div>
                </div>
              )}
              {ptoSubmitted && (
                <div>
                  <div className="text-emerald-700 text-xs">Submitted</div>
                  <div className="font-medium text-emerald-900">{ptoSubmitted}</div>
                </div>
              )}
            </div>
          </div>
        );

      default:
        return null;
    }
  };

  return (
    <Link href={`/operations/projects/${project.projectId}`}>
      <Card className="p-4 hover:shadow-md transition-shadow cursor-pointer">
        {/* Header: Customer Info */}
        <div className="mb-3">
          <div className="flex items-start justify-between mb-1">
            <div>
              <h3 className="font-semibold text-lg text-slate-900">
                {project.customerName || 'Unknown Customer'}
              </h3>
              <div className="flex items-center gap-1 text-sm text-slate-600">
                <MapPin className="w-3.5 h-3.5" />
                <span>{formatAddress(project.customerAddress || '')}</span>
              </div>
            </div>
            <div className="text-right">
              <div className="text-xs text-slate-500">Project ID</div>
              <div className="font-mono text-sm font-medium">{project.projectId}</div>
            </div>
          </div>
        </div>

        {/* Milestone Badge and Days in Stage */}
        <div className="mb-3 flex items-center justify-between">
          <MilestoneStatusBadge
            milestone={project.currentMilestone}
            status={project.milestoneStatus}
            isBlocked={project.isBlocked}
            isOnHold={project.isOnHold}
          />
          <div className="text-right">
            <div className="text-xs text-slate-500">Days in Stage</div>
            <div className={`font-bold text-lg ${
              project.daysInStage > 7 ? 'text-amber-600' : 'text-slate-700'
            }`}>
              {project.daysInStage || 0}
            </div>
          </div>
        </div>

        {/* Block/Hold Status */}
        {(project.isBlocked || project.isOnHold) && (
          <div className="mb-3 space-y-2">
            {project.isBlocked && (
              <div className="flex items-start gap-2 p-2 bg-red-50 rounded border border-red-200">
                <AlertCircle className="w-4 h-4 text-red-600 mt-0.5 flex-shrink-0" />
                <div>
                  <div className="text-sm font-medium text-red-900">Blocked</div>
                  <div className="text-xs text-red-700">{project.blockReason || 'No reason provided'}</div>
                </div>
              </div>
            )}
            {project.isOnHold && (
              <div className="flex items-start gap-2 p-2 bg-yellow-50 rounded border border-yellow-200">
                <Clock className="w-4 h-4 text-yellow-600 mt-0.5 flex-shrink-0" />
                <div>
                  <div className="text-sm font-medium text-yellow-900">On Hold</div>
                  <div className="text-xs text-yellow-700">{project.holdReason || 'No reason provided'}</div>
                </div>
              </div>
            )}
          </div>
        )}

        {/* Milestone-Specific Details */}
        {renderMilestoneDetails()}

        {/* Footer: Project Basics */}
        <div className="mt-3 pt-3 border-t border-slate-200">
          <div className="grid grid-cols-3 gap-4 text-sm">
            <div>
              <div className="text-xs text-slate-500 mb-0.5">System Size</div>
              <div className="flex items-center gap-1 font-medium text-slate-700">
                <Zap className="w-3.5 h-3.5" />
                {project.systemSizeKW ? `${project.systemSizeKW} kW` : 'N/A'}
              </div>
            </div>
            <div>
              <div className="text-xs text-slate-500 mb-0.5">Price</div>
              <div className="flex items-center gap-1 font-medium text-slate-700">
                <DollarSign className="w-3.5 h-3.5" />
                {project.systemPrice ? `$${project.systemPrice.toLocaleString()}` : 'N/A'}
              </div>
            </div>
            <div>
              <div className="text-xs text-slate-500 mb-0.5">Office</div>
              <div className="font-medium text-slate-700 truncate">
                {project.salesOffice || 'N/A'}
              </div>
            </div>
          </div>
          <div className="mt-3 grid grid-cols-2 gap-4 text-xs">
            <div>
              <span className="text-slate-500">Closer:</span>{' '}
              <span className="font-medium text-slate-700">{project.closerName || 'N/A'}</span>
            </div>
            <div>
              <span className="text-slate-500">Setter:</span>{' '}
              <span className="font-medium text-slate-700">{project.setterName || 'N/A'}</span>
            </div>
          </div>
        </div>
      </Card>
    </Link>
  );
}
