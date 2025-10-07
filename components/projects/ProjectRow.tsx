'use client';

import Link from 'next/link';
import { useRouter } from 'next/navigation';
import { Phone, Loader2 } from 'lucide-react';
import { useQueryClient, useIsFetching } from '@tanstack/react-query';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import { QuickbaseProject } from '@/lib/types/project';
import { TrafficLightPipeline } from './TrafficLightPipeline';
import { PPWDisplay } from './PPWDisplay';
import { ProjectAgeIndicator } from './ProjectAgeIndicator';
import { HoldBanner } from './HoldBanner';
import { StatusBadge } from './StatusBadge';
import { parseCustomerName, formatAddress, getProjectAge } from '@/lib/utils/project-helpers';
import { detectHoldStatus, extractHoldType } from '@/lib/utils/hold-detection';
import { formatSystemSize, formatCurrency } from '@/lib/utils/formatters';
import { projectKey } from '@/lib/queryKeys';

interface ProjectRowProps {
  project: QuickbaseProject;
}

// Updated 2025-01-07: Current active version

export function ProjectRow({ project }: ProjectRowProps) {
  const queryClient = useQueryClient();
  const router = useRouter();

  // Extract data from project
  const recordId = project[PROJECT_FIELDS.RECORD_ID]?.value;
  const customerName = project[PROJECT_FIELDS.CUSTOMER_NAME]?.value || '';
  const customerAddress = project[PROJECT_FIELDS.CUSTOMER_ADDRESS]?.value || '';
  const customerPhone = project[PROJECT_FIELDS.CUSTOMER_PHONE]?.value || '';
  const projectStatus = project[PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
  const systemSizeKw = parseFloat(project[PROJECT_FIELDS.SYSTEM_SIZE_KW]?.value || '0');
  const systemPrice = parseFloat(project[PROJECT_FIELDS.SYSTEM_PRICE]?.value || '0');
  const salesDate = project[PROJECT_FIELDS.SALES_DATE]?.value || '';
  const closerName = project[PROJECT_FIELDS.CLOSER_NAME]?.value || '';
  const setterName = project[PROJECT_FIELDS.SETTER_NAME]?.value || '';

  // PPW fields - using the field IDs from quickbase-config.json
  const soldPPW = parseFloat(project[2292]?.value || '0') || null; // soldGross
  const commissionablePPW = parseFloat(project[2480]?.value || '0') || null; // commissionable

  // Calculate derived values
  const isOnHold = detectHoldStatus(projectStatus);
  const holdType = extractHoldType(projectStatus);
  const projectAge = getProjectAge(project);
  const parsedName = parseCustomerName(customerName);
  const formattedAddress = formatAddress(customerAddress);

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

  // Format phone number for tel: link
  const phoneLink = customerPhone ? `tel:${customerPhone.replace(/\D/g, '')}` : '';

  // Check if this project is currently being fetched
  const isFetching = useIsFetching({ queryKey: ['project', recordId] }) > 0;

  // Prefetch project detail on hover for instant navigation
  const handlePrefetch = () => {
    // Guard against missing recordId to avoid prefetching /api/projects/undefined
    if (!recordId) {
      return;
    }
    
    const idKey = String(recordId);
    
    // Prefetch the query data
    queryClient.prefetchQuery({
      queryKey: projectKey(idKey),
      queryFn: async () => {
        const response = await fetch(`/api/projects/${idKey}`);
        if (!response.ok) throw new Error('Failed to prefetch project');
        return response.json();
      },
      staleTime: 300000, // 5 minutes (matches detail page cache)
    });

    // Prefetch the route for instant navigation
    router.prefetch(`/projects/${recordId}`);
  };

  return (
    <div>
      {/* Hold banner (if on hold) */}
      {isOnHold && (
        <HoldBanner project={project} holdType={holdType} />
      )}

      {/* Main row content - Single Link wrapper */}
      <Link
        href={`/projects/${recordId}`}
        className={`
          block group bg-white border border-slate-200 rounded-lg
          hover:shadow-md hover:border-indigo-200
          transition-all duration-200 ease-in-out
          ${isOnHold ? 'rounded-t-none border-t-0' : ''}
        `}
        data-testid="project-row"
        onMouseEnter={handlePrefetch}
        onClick={() => console.log('[ProjectRow] Navigating to project (prefetched):', recordId)}
      >
        <div className="flex items-center gap-6 p-4">
          {/* Column 1 - Customer Info */}
          <div className="flex-shrink-0 w-56 space-y-1">
            <div className="flex items-center gap-2 flex-wrap">
              <div className="font-semibold text-gray-900 group-hover:text-indigo-600 transition-colors" data-testid="customer-name">
                {parsedName.firstName} {parsedName.lastName}
              </div>
              <StatusBadge status={projectStatus} />
            </div>
            <div className="text-sm text-slate-600">
              {formattedAddress.line1}
            </div>
            {formattedAddress.line2 && (
              <div className="text-sm text-slate-600">
                {formattedAddress.line2}
              </div>
            )}
            {customerPhone && (
              <a
                href={phoneLink}
                onClick={(e) => {
                  e.stopPropagation();
                  e.preventDefault();
                  window.location.href = phoneLink;
                }}
                className="
                  inline-flex items-center gap-1.5 text-sm text-indigo-600 hover:text-indigo-700
                  hover:underline transition-colors
                  focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:ring-offset-2 rounded
                "
                data-testid="customer-phone"
              >
                <Phone className="h-3.5 w-3.5" />
                {customerPhone}
              </a>
            )}

            {/* Sales Date & Team */}
            <div className="pt-2 mt-2 border-t border-slate-200 space-y-0.5">
              {salesDate && (
                <div className="text-xs text-slate-500">
                  <span className="font-medium">Sold:</span> {formatSalesDate(salesDate)}
                </div>
              )}
              {closerName && (
                <div className="text-xs text-slate-500">
                  <span className="font-medium">Closer:</span> {closerName}
                </div>
              )}
              {setterName && (
                <div className="text-xs text-slate-500">
                  <span className="font-medium">Setter:</span> {setterName}
                </div>
              )}
            </div>
          </div>

          {/* Column 2 - Traffic Lights */}
          <div className="flex-1 min-w-0" data-testid="traffic-lights">
            <TrafficLightPipeline project={project} />
          </div>

          {/* Column 3 - Metrics */}
          <div className="flex-shrink-0 w-64 bg-slate-50 rounded-lg p-3 space-y-2">
            {/* System Size */}
            <div className="flex items-baseline justify-between">
              <span className="text-xs text-slate-500 uppercase tracking-wide">Size</span>
              <span className="text-lg font-bold text-slate-900">
                {formatSystemSize(systemSizeKw)}
              </span>
            </div>

            {/* PPW Section */}
            <div className="space-y-1 pt-1 border-t border-slate-200">
              <div className="flex items-baseline justify-between">
                <span className="text-xs text-slate-500">Sold PPW</span>
                <span className="text-sm font-semibold text-slate-900">
                  {soldPPW ? `$${soldPPW.toFixed(2)}/W` : 'N/A'}
                </span>
              </div>
              <div className="flex items-baseline justify-between">
                <span className="text-xs text-slate-500">Comm. PPW</span>
                <PPWDisplay soldPPW={soldPPW} commissionablePPW={commissionablePPW} />
              </div>
            </div>

            {/* Gross Cost */}
            <div className="flex items-baseline justify-between pt-1 border-t border-slate-200">
              <span className="text-xs text-slate-500 uppercase tracking-wide">Gross Cost</span>
              <span className="text-lg font-bold text-emerald-600">
                {formatCurrency(systemPrice)}
              </span>
            </div>
          </div>

          {/* Column 4 - Age & Loading Indicator */}
          <div className="flex-shrink-0 w-20 flex items-center justify-end" data-testid="project-age">
            {isFetching ? (
              <Loader2 className="h-4 w-4 animate-spin text-indigo-600" />
            ) : (
              <ProjectAgeIndicator age={projectAge} />
            )}
          </div>
        </div>
      </Link>
    </div>
  );
}
