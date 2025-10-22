'use client';

import Link from 'next/link';
import { useRouter } from 'next/navigation';
import { MapPin, Calendar, Building2, Eye } from 'lucide-react';
import { useQueryClient } from '@tanstack/react-query';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import { QuickbaseProject } from '@/lib/types/project';
import { TrafficLightPipeline } from './TrafficLightPipeline';
import { TrafficLightPipelineMobile } from './TrafficLightPipelineMobile';
import { Button } from '@/components/ui/button';
import { StatusBadge } from './StatusBadge';
import { RejectionReasonBadge } from './RejectionReasonBadge';
import { UnreadBadge } from '@/components/ui/UnreadBadge';
import { TaskBadge } from '@/components/ui/TaskBadge';
import { useProjectUnreadCount } from '@/lib/hooks/useNotifications';
import { parseCustomerName, getProjectAge, determineProjectOwnership } from '@/lib/utils/project-helpers';
import { detectHoldStatus, extractHoldType } from '@/lib/utils/hold-detection';
import { formatSystemSize, formatCurrency } from '@/lib/utils/formatters';
import { formatQuickbaseDate } from '@/lib/utils/date-helpers';
import { projectKey } from '@/lib/queryKeys';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { getProjectCompletionPercentage } from '@/lib/utils/milestone-engine';
import { useMilestoneConfig } from '@/lib/hooks/useMilestoneConfig';
import { OwnershipBadge } from './OwnershipBadge';
import { isManagerRole } from '@/lib/utils/role-helpers';
import { useIsMobile } from '@/lib/hooks/useMediaQuery';

interface ProjectRowProps {
  project: QuickbaseProject;
  userEmail: string; // NEW: Current user's email for ownership determination
  userRole: string;  // NEW: Current user's role for manager-specific logic
}

// Updated 2025-01-07: Current active version

export function ProjectRow({ project, userEmail, userRole }: ProjectRowProps) {
  const queryClient = useQueryClient();
  const router = useRouter();
  const { config } = useMilestoneConfig();
  const isMobile = useIsMobile();

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
  const closerEmail = project[PROJECT_FIELDS.CLOSER_EMAIL]?.value || null;
  const setterEmail = project[PROJECT_FIELDS.SETTER_EMAIL]?.value || null;

  // Handle SALES_OFFICE - could be string or array
  const officeValue = project[PROJECT_FIELDS.SALES_OFFICE]?.value;
  const office = Array.isArray(officeValue)
    ? officeValue.join(', ')
    : (typeof officeValue === 'string' ? officeValue : '');

  // PPW fields
  const soldPPW = parseFloat(project[2292]?.value || '0') || null;
  const netPPW = parseFloat(project[2480]?.value || '0') || null;

  // Task count fields
  const totalTasks = parseInt(project[PROJECT_FIELDS.TOTAL_TASKS]?.value || '0') || 0;
  const unapprovedTasks = parseInt(project[PROJECT_FIELDS.UNAPPROVED_TASKS]?.value || '0') || 0;

  // Calculate derived values
  const isOnHold = detectHoldStatus(projectStatus);
  const holdType = extractHoldType(projectStatus);
  const parsedName = parseCustomerName(customerName);
  const projectAge = getProjectAge(project);
  const completionPercentage = getProjectCompletionPercentage(project, config);

  // Determine project ownership for badge display
  const ownership = determineProjectOwnership(
    userEmail,
    closerEmail,
    setterEmail,
    closerName,
    setterName
  );

  // Suppress ownership badge for reps on their own projects to reduce visual noise
  const showOwnershipBadge = isManagerRole(userRole) || ownership.status !== 'mine';

  // Get rejection reasons for rejected projects
  const isRejected = projectStatus.toLowerCase().includes('reject');
  const rejectionReasons = project[PROJECT_FIELDS.INTAKE_MISSING_ITEMS_COMBINED]?.value ?? null;

  // Format sales date using timezone-aware utility
  const formattedSalesDate = formatQuickbaseDate(salesDate);

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
      <div className={isMobile ? "p-3 border-b border-slate-100" : "p-4 border-b border-slate-100"}>
        <div className={isMobile ? "space-y-2" : "flex items-start justify-between"}>
          {/* Left: Customer Name + Badge */}
          <div className="flex-1">
            <div className="flex items-center gap-2 mb-1 flex-wrap">
              <h3 className={isMobile ? "text-base font-semibold text-slate-900" : "text-lg font-semibold text-slate-900"}>
                {parsedName.firstName} {parsedName.lastName}
              </h3>
              <StatusBadge status={projectStatus} />
              {showOwnershipBadge && (
                <OwnershipBadge
                  status={ownership.status}
                  displayName={ownership.displayName}
                />
              )}
              {unreadCount > 0 && (
                <UnreadBadge count={unreadCount} variant="default" size="small" />
              )}
              {unapprovedTasks > 0 && (
                <TaskBadge count={unapprovedTasks} size="small" />
              )}
            </div>
            {isRejected && rejectionReasons && (
              <RejectionReasonBadge reasons={rejectionReasons} />
            )}
            <div className="flex items-center gap-1 text-sm text-slate-600">
              <MapPin className="h-3.5 w-3.5 flex-shrink-0" />
              <span className={isMobile ? "line-clamp-1" : ""}>{customerAddress}</span>
            </div>
          </div>

          {/* Right: System Metrics - stacks below on mobile */}
          <div className={isMobile ? "flex items-center gap-3 mt-2" : "text-right"}>
            <div className={isMobile ? "text-left" : ""}>
              <div className={isMobile ? "text-xl font-bold text-slate-900" : "text-2xl font-bold text-slate-900"}>{formatSystemSize(systemSizeKw)}</div>
              <div className={isMobile ? "text-base font-semibold text-emerald-600" : "text-lg font-semibold text-emerald-600"}>{formatCurrency(systemPrice)}</div>
            </div>
            {netPPW && (
              <div className={isMobile ? "text-xs text-slate-500" : "text-xs text-slate-500"}>
                Net PPW: ${netPPW.toFixed(2)}/W
              </div>
            )}
          </div>
        </div>

        {/* Metadata Row - compressed on mobile */}
        <div className={isMobile ? "flex items-center gap-3 mt-2 text-xs text-slate-600" : "flex items-center gap-4 mt-3 text-xs text-slate-600"}>
          {setterName && (
            <div className="flex items-center gap-1">
              <span className="font-medium">Setter:</span> <span className={isMobile ? "truncate max-w-[80px]" : ""}>{setterName}</span>
            </div>
          )}
          {closerName && (
            <div className="flex items-center gap-1">
              <span className="font-medium">Closer:</span> <span className={isMobile ? "truncate max-w-[80px]" : ""}>{closerName}</span>
            </div>
          )}
          {/* Hide office and sales date on mobile to save space */}
          {!isMobile && office && (
            <div className="flex items-center gap-1">
              <Building2 className="h-3.5 w-3.5" />
              <span className="font-medium">Office:</span> {office}
            </div>
          )}
          {!isMobile && formattedSalesDate && (
            <div className="flex items-center gap-1">
              <Calendar className="h-3.5 w-3.5" />
              <span className="font-medium">Sold:</span> {formattedSalesDate}
            </div>
          )}
          {projectAge > 0 && (
            <div className="flex items-center gap-1">
              <Calendar className="h-3.5 w-3.5 flex-shrink-0" />
              <span className="font-medium">Age:</span> {projectAge}d
            </div>
          )}
        </div>
      </div>

      {/* Progress Section */}
      <div className={isMobile ? "px-3 py-2 bg-slate-50" : "px-4 py-3 bg-slate-50"}>
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

      {/* Traffic Lights Section - mobile-optimized version on small screens */}
      <div className={isMobile ? "px-3 py-3" : "px-4 py-4"}>
        {isMobile ? (
          <TrafficLightPipelineMobile project={project} />
        ) : (
          <TrafficLightPipeline project={project} />
        )}
      </div>

      {/* Footer Section - hide on mobile to save space */}
      {!isMobile && (
        <div className="px-4 py-3 border-t border-slate-100 flex items-center justify-center">
          <div className="flex items-center gap-2 text-sm text-slate-500">
            <Eye className="h-4 w-4" />
            <span>Click to view details</span>
          </div>
        </div>
      )}
    </Link>
  );
}
