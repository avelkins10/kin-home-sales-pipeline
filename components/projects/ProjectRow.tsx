'use client';

import Link from 'next/link';
import { useRouter } from 'next/navigation';
import { MapPin, Calendar, Building2, Eye } from 'lucide-react';
import { useQueryClient } from '@tanstack/react-query';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import { QuickbaseProject } from '@/lib/types/project';
import { TrafficLightPipeline } from './TrafficLightPipeline';
import { Button } from '@/components/ui/button';
import { StatusBadge } from './StatusBadge';
import { RejectionReasonBadge } from './RejectionReasonBadge';
import { UnreadBadge } from '@/components/ui/UnreadBadge';
import { useProjectUnreadCount } from '@/lib/hooks/useNotifications';
import { parseCustomerName, getProjectAge } from '@/lib/utils/project-helpers';
import { detectHoldStatus, extractHoldType } from '@/lib/utils/hold-detection';
import { formatSystemSize, formatCurrency } from '@/lib/utils/formatters';
import { projectKey } from '@/lib/queryKeys';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { getProjectCompletionPercentage } from '@/lib/utils/milestone-engine';
import { useMilestoneConfig } from '@/lib/hooks/useMilestoneConfig';

interface ProjectRowProps {
  project: QuickbaseProject;
}

// Updated 2025-01-07: Current active version

export function ProjectRow({ project }: ProjectRowProps) {
  const queryClient = useQueryClient();
  const router = useRouter();
  const { config } = useMilestoneConfig();

  // Extract data from project
  const recordId = project[PROJECT_FIELDS.RECORD_ID]?.value;
  const unreadCount = useProjectUnreadCount(Number(recordId));
  const customerName = project[PROJECT_FIELDS.CUSTOMER_NAME]?.value || '';
  const customerAddress = project[PROJECT_FIELDS.CUSTOMER_ADDRESS]?.value || '';
  const projectStatus = project[PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
  const systemSizeKw = parseFloat(project[PROJECT_FIELDS.SYSTEM_SIZE_KW]?.value || '0');
  const systemPrice = parseFloat(project[PROJECT_FIELDS.SYSTEM_PRICE]?.value || '0');
  const salesDate = project[PROJECT_FIELDS.SALES_DATE]?.value || '';
  const closerName = project[PROJECT_FIELDS.CLOSER_NAME]?.value || '';
  const setterName = project[PROJECT_FIELDS.SETTER_NAME]?.value || '';

  // Handle SALES_OFFICE - could be string or array
  const officeValue = project[PROJECT_FIELDS.SALES_OFFICE]?.value;
  const office = Array.isArray(officeValue)
    ? officeValue.join(', ')
    : (typeof officeValue === 'string' ? officeValue : '');

  // PPW fields
  const soldPPW = parseFloat(project[2292]?.value || '0') || null;
  const netPPW = parseFloat(project[2480]?.value || '0') || null;

  // Calculate derived values
  const isOnHold = detectHoldStatus(projectStatus);
  const holdType = extractHoldType(projectStatus);
  const parsedName = parseCustomerName(customerName);
  const projectAge = getProjectAge(project);
  const completionPercentage = getProjectCompletionPercentage(project, config);

  // Get rejection reasons for rejected projects
  const isRejected = projectStatus.toLowerCase().includes('reject');
  const rejectionReasons = project[PROJECT_FIELDS.INTAKE_MISSING_ITEMS_COMBINED]?.value ?? null;

  // Format sales date
  const formatSalesDate = (date: string) => {
    if (!date) return '';
    try {
      const d = new Date(date);
      return d.toLocaleDateString('en-US', { month: 'short', day: 'numeric', year: 'numeric' });
    } catch {
      return '';
    }
  };

  // Prefetch project detail on hover
  const handlePrefetch = () => {
    if (!recordId) return;

    queryClient.prefetchQuery({
      queryKey: projectKey(String(recordId)),
      queryFn: async () => {
        const response = await fetch(`${getBaseUrl()}/api/projects/${recordId}`);
        if (!response.ok) throw new Error('Failed to prefetch project');
        return response.json();
      },
      staleTime: 300000,
    });

    router.prefetch(`/projects/${recordId}`);
  };

  return (
    <Link
      href={`/projects/${recordId}`}
      className="block bg-white border border-slate-200 rounded-lg hover:shadow-md transition-shadow duration-200 cursor-pointer"
      onMouseEnter={handlePrefetch}
      data-testid="project-row"
    >
      {/* Header Section */}
      <div className="p-4 border-b border-slate-100">
        <div className="flex items-start justify-between">
          {/* Left: Customer Name + Badge */}
          <div className="flex-1">
            <div className="flex items-center gap-2 mb-1">
              <h3 className="text-lg font-semibold text-slate-900">
                {parsedName.firstName} {parsedName.lastName}
              </h3>
              <StatusBadge status={projectStatus} />
              {unreadCount > 0 && (
                <UnreadBadge count={unreadCount} variant="default" size="small" />
              )}
            </div>
            {isRejected && rejectionReasons && (
              <RejectionReasonBadge reasons={rejectionReasons} />
            )}
            <div className="flex items-center gap-1 text-sm text-slate-600">
              <MapPin className="h-3.5 w-3.5" />
              <span>{customerAddress}</span>
            </div>
          </div>

          {/* Right: System Metrics */}
          <div className="text-right">
            <div className="text-2xl font-bold text-slate-900">{formatSystemSize(systemSizeKw)}</div>
            <div className="text-lg font-semibold text-emerald-600">{formatCurrency(systemPrice)}</div>
            <div className="text-xs text-slate-500">Net PPW: {netPPW ? `$${netPPW.toFixed(2)}/W` : 'N/A'}</div>
          </div>
        </div>

        {/* Metadata Row */}
        <div className="flex items-center gap-4 mt-3 text-xs text-slate-600">
          {setterName && (
            <div className="flex items-center gap-1">
              <span className="font-medium">Setter:</span> {setterName}
            </div>
          )}
          {closerName && (
            <div className="flex items-center gap-1">
              <span className="font-medium">Closer:</span> {closerName}
            </div>
          )}
          {office && (
            <div className="flex items-center gap-1">
              <Building2 className="h-3.5 w-3.5" />
              <span className="font-medium">Office:</span> {office}
            </div>
          )}
          {salesDate && (
            <div className="flex items-center gap-1">
              <Calendar className="h-3.5 w-3.5" />
              <span className="font-medium">Sold:</span> {formatSalesDate(salesDate)}
            </div>
          )}
          {projectAge > 0 && (
            <div className="flex items-center gap-1">
              <Calendar className="h-3.5 w-3.5" />
              <span className="font-medium">Project Age:</span> {projectAge} days
            </div>
          )}
        </div>
      </div>

      {/* Progress Section */}
      <div className="px-4 py-3 bg-slate-50">
        <div className="flex items-center justify-between mb-2">
          <span className="text-xs font-semibold text-slate-700">Project Progress</span>
          <span className="text-xs font-bold text-slate-900">{completionPercentage}% Complete</span>
        </div>
        <div className="w-full h-2 bg-slate-200 rounded-full overflow-hidden">
          <div
            className="h-full bg-gradient-to-r from-emerald-500 to-emerald-600 transition-all duration-300"
            style={{ width: `${completionPercentage}%` }}
          />
        </div>
      </div>

      {/* Traffic Lights Section */}
      <div className="px-4 py-4">
        <TrafficLightPipeline project={project} />
      </div>

      {/* Footer Section */}
      <div className="px-4 py-3 border-t border-slate-100 flex items-center justify-center">
        <div className="flex items-center gap-2 text-sm text-slate-500">
          <Eye className="h-4 w-4" />
          <span>Click to view details</span>
        </div>
      </div>
    </Link>
  );
}
