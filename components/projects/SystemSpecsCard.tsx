'use client'

import { Zap, DollarSign, Battery, Grid, Building2 } from 'lucide-react'
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'
import { QuickbaseProject } from '@/lib/types/project'
import { formatCurrency, formatSystemSize } from '@/lib/utils/formatters'

interface SystemSpecsCardProps {
  project: QuickbaseProject
}

export function SystemSpecsCard({ project }: SystemSpecsCardProps) {
  // Extract system specification data
  const systemSize = parseFloat(project[PROJECT_FIELDS.SYSTEM_SIZE_KW]?.value || '0')
  const systemPrice = parseFloat(project[PROJECT_FIELDS.SYSTEM_PRICE]?.value || '0')
  const numPanels = parseInt(project[PROJECT_FIELDS.NUMBER_OF_PANELS]?.value || '0')
  const moduleType = project[PROJECT_FIELDS.MODULE]?.value || ''
  const moduleBrand = project[PROJECT_FIELDS.MODULE_BRAND]?.value || ''
  const inverter = project[PROJECT_FIELDS.INVERTER]?.value || ''
  const inverterBrand = project[PROJECT_FIELDS.INVERTER_BRAND]?.value || ''
  const inverterCount = parseInt(project[PROJECT_FIELDS.INVERTER_COUNT]?.value || '0')
  const batteryModel = project[PROJECT_FIELDS.BATTERY_MODEL]?.value || ''
  const batteryQuantity = parseInt(project[PROJECT_FIELDS.BATTERY_QUANTITY]?.value || '0')

  // Extract financing data
  const financing = project[PROJECT_FIELDS.FINANCING]?.value || ''
  const financingPlan = project[PROJECT_FIELDS.FINANCING_PLAN]?.value || ''

  return (
    <Card>
      <CardHeader>
        <CardTitle className="text-xl font-semibold">System Specifications</CardTitle>
      </CardHeader>
      <CardContent className="space-y-6">
        {/* Primary Specifications */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          {/* System Size */}
          <div className="flex items-center gap-3">
            <Zap className="w-5 h-5 text-blue-600" />
            <div>
              <p className="text-sm text-gray-600 font-medium">System Size</p>
              <p className="text-base text-gray-900 font-semibold">
                {formatSystemSize(systemSize)}
              </p>
            </div>
          </div>

          {/* System Price */}
          <div className="flex items-center gap-3">
            <DollarSign className="w-5 h-5 text-blue-600" />
            <div>
              <p className="text-sm text-gray-600 font-medium">System Price</p>
              <p className="text-base text-gray-900 font-semibold">
                {formatCurrency(systemPrice)}
              </p>
            </div>
          </div>

          {/* Number of Panels */}
          <div className="flex items-center gap-3">
            <Grid className="w-5 h-5 text-blue-600" />
            <div>
              <p className="text-sm text-gray-600 font-medium">Panels</p>
              <p className="text-base text-gray-900 font-semibold">
                {numPanels > 0 ? `${numPanels} panels` : 'N/A'}
              </p>
            </div>
          </div>

          {/* Module Brand */}
          <div className="flex items-center gap-3">
            <div className="w-5 h-5 bg-amber-500 rounded flex items-center justify-center text-white text-xs font-bold">
              ☀️
            </div>
            <div>
              <p className="text-sm text-gray-600 font-medium">Panel Brand</p>
              <p className="text-base text-gray-900 font-semibold">
                {moduleBrand || moduleType || 'N/A'}
              </p>
            </div>
          </div>
        </div>

        {/* Equipment Details */}
        <div className="border-t border-gray-200 pt-4">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            {/* Module Model */}
            {moduleType && (
              <div className="flex items-start gap-3">
                <div className="w-5 h-5 bg-gray-400 rounded flex-shrink-0 mt-0.5" />
                <div className="flex-1">
                  <p className="text-sm text-gray-600 font-medium">Panel Model</p>
                  <p className="text-sm text-gray-900 font-mono">
                    {moduleType}
                  </p>
                </div>
              </div>
            )}

            {/* Inverter */}
            <div className="flex items-start gap-3">
              <div className="w-5 h-5 bg-indigo-500 rounded flex-shrink-0 mt-0.5 flex items-center justify-center text-white text-xs font-bold">
                ⚡
              </div>
              <div className="flex-1">
                <p className="text-sm text-gray-600 font-medium">Inverter Brand</p>
                <p className="text-base text-gray-900 font-semibold">
                  {inverterBrand || inverter || 'N/A'}
                  {inverterCount > 0 && (
                    <Badge variant="secondary" className="ml-2">
                      {inverterCount}x
                    </Badge>
                  )}
                </p>
                {inverter && (
                  <p className="text-xs text-gray-600 font-mono mt-1">
                    {inverter}
                  </p>
                )}
              </div>
            </div>
          </div>
        </div>

        {/* Battery Section */}
        {batteryModel && batteryQuantity > 0 && (
          <div className="border-t border-gray-200 pt-4">
            <div className="flex items-center gap-3">
              <Battery className="w-5 h-5 text-green-600" />
              <div className="flex-1">
                <p className="text-sm text-gray-600 font-medium">Battery</p>
                <p className="text-base text-gray-900 font-semibold">{batteryModel}</p>
              </div>
              <Badge variant="secondary" className="bg-green-100 text-green-700">
                {batteryQuantity}x
              </Badge>
            </div>
          </div>
        )}

        {/* Financing Section */}
        {(financing || financingPlan) && (
          <div className="border-t border-gray-200 pt-4">
            <h4 className="text-sm font-medium text-gray-600 mb-3">Financing Details</h4>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              {/* Lender */}
              {financing && (
                <div className="flex items-start gap-3">
                  <Building2 className="w-5 h-5 text-blue-600 flex-shrink-0 mt-0.5" />
                  <div className="flex-1">
                    <p className="text-sm text-gray-600 font-medium">Lender</p>
                    <p className="text-base text-gray-900 font-semibold">
                      {financing}
                    </p>
                  </div>
                </div>
              )}

              {/* Finance Type/Plan */}
              {financingPlan && (
                <div className="flex items-start gap-3">
                  <div className="w-5 h-5 bg-blue-500 rounded flex-shrink-0 mt-0.5 flex items-center justify-center text-white text-xs font-bold">
                    💳
                  </div>
                  <div className="flex-1">
                    <p className="text-sm text-gray-600 font-medium">Finance Type</p>
                    <p className="text-base text-gray-900 font-semibold">
                      {financingPlan}
                    </p>
                  </div>
                </div>
              )}
            </div>
          </div>
        )}
      </CardContent>
    </Card>
  )
}
