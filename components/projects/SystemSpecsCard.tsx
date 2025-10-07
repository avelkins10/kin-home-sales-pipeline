'use client'

import { Zap, DollarSign, Battery, Grid } from 'lucide-react'
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'
import { QuickbaseProject } from '@/lib/types/project'
import { formatCurrency, formatSystemSize, formatPPW } from '@/lib/utils/formatters'

interface SystemSpecsCardProps {
  project: QuickbaseProject
}

export function SystemSpecsCard({ project }: SystemSpecsCardProps) {
  // Extract system specification data
  const systemSize = parseFloat(project[PROJECT_FIELDS.SYSTEM_SIZE_KW]?.value || '0')
  const systemPrice = parseFloat(project[PROJECT_FIELDS.SYSTEM_PRICE]?.value || '0')
  const numPanels = parseInt(project[PROJECT_FIELDS.NUMBER_OF_PANELS]?.value || '0')
  const moduleType = project[PROJECT_FIELDS.MODULE]?.value || ''
  const inverter = project[PROJECT_FIELDS.INVERTER]?.value || ''
  const batteryModel = project[PROJECT_FIELDS.BATTERY_MODEL]?.value || ''
  const batteryQuantity = parseInt(project[PROJECT_FIELDS.BATTERY_QUANTITY]?.value || '0')
  const grossPPW = parseFloat(project[PROJECT_FIELDS.GROSS_PPW]?.value || '0')
  const netPPW = parseFloat(project[PROJECT_FIELDS.NET_PPW]?.value || '0')
  const commissionablePPW = parseFloat(project[PROJECT_FIELDS.COMMISSIONABLE_PPW]?.value || '0')

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

          {/* Module */}
          <div className="flex items-center gap-3">
            <div className="w-5 h-5 bg-gray-400 rounded" />
            <div>
              <p className="text-sm text-gray-600 font-medium">Module</p>
              <p className="text-base text-gray-900 font-semibold">
                {moduleType || 'N/A'}
              </p>
            </div>
          </div>

          {/* Inverter */}
          <div className="flex items-center gap-3 md:col-span-2">
            <div className="w-5 h-5 bg-gray-400 rounded" />
            <div>
              <p className="text-sm text-gray-600 font-medium">Inverter</p>
              <p className="text-base text-gray-900 font-semibold">
                {inverter || 'N/A'}
              </p>
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

        {/* Pricing Breakdown */}
        <div className="border-t border-gray-200 pt-4">
          <h4 className="text-sm font-medium text-gray-600 mb-3">Pricing Breakdown</h4>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            {/* Gross PPW */}
            <div className="text-center">
              <p className="text-xs text-gray-500 font-medium">Gross PPW</p>
              <p className="text-lg font-semibold text-gray-900">
                {formatPPW(grossPPW)}
              </p>
            </div>

            {/* Net PPW */}
            <div className="text-center">
              <p className="text-xs text-gray-500 font-medium">Net PPW</p>
              <p className="text-lg font-semibold text-gray-900">
                {formatPPW(netPPW)}
              </p>
            </div>

            {/* Commissionable PPW */}
            <div className="text-center">
              <p className="text-xs text-gray-500 font-medium">Commissionable PPW</p>
              <p className="text-lg font-semibold text-gray-900">
                {formatPPW(commissionablePPW)}
              </p>
            </div>
          </div>
        </div>
      </CardContent>
    </Card>
  )
}
