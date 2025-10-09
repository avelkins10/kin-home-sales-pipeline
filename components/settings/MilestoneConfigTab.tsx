'use client'

import { useState } from 'react'
import { Card, CardHeader, CardTitle, CardContent, CardDescription } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import { ChevronDown, ChevronUp, Settings, Info } from 'lucide-react'
import milestonesConfig from '@/lib/config/milestones.json'
import { cn } from '@/lib/utils/cn'

interface MilestoneConfigTabProps {
  // Future: Add ability to edit and save configurations
}

export function MilestoneConfigTab({}: MilestoneConfigTabProps) {
  const [expandedMilestones, setExpandedMilestones] = useState<Set<string>>(new Set())

  const toggleMilestone = (milestoneId: string) => {
    const newExpanded = new Set(expandedMilestones)
    if (newExpanded.has(milestoneId)) {
      newExpanded.delete(milestoneId)
    } else {
      newExpanded.add(milestoneId)
    }
    setExpandedMilestones(newExpanded)
  }

  const getUsageColor = (usage: string) => {
    const percent = parseFloat(usage.replace('%', ''))
    if (percent >= 80) return 'text-green-600 bg-green-50'
    if (percent >= 50) return 'text-blue-600 bg-blue-50'
    if (percent >= 20) return 'text-orange-600 bg-orange-50'
    return 'text-red-600 bg-red-50'
  }

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex items-start justify-between">
        <div>
          <h2 className="text-2xl font-bold text-slate-900">Milestone Configuration</h2>
          <p className="mt-2 text-sm text-slate-600">
            View and manage how milestones are calculated across your application.
          </p>
        </div>
        <div className="flex gap-2">
          <Button variant="outline" size="sm" disabled>
            <Settings className="w-4 h-4 mr-2" />
            Edit Config
          </Button>
        </div>
      </div>

      {/* Info Card */}
      <Card className="border-blue-200 bg-blue-50">
        <CardContent className="pt-6">
          <div className="flex gap-3">
            <Info className="w-5 h-5 text-blue-600 flex-shrink-0 mt-0.5" />
            <div className="text-sm text-blue-900">
              <p className="font-medium mb-1">Configuration Priority Order</p>
              <ol className="list-decimal list-inside space-y-1 text-blue-800">
                <li>Status Field (100% usage where available)</li>
                <li>Primary completion/date fields</li>
                <li>Fallback/backup timestamp fields</li>
                <li>Calculated or estimated fields</li>
              </ol>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Milestones */}
      <div className="space-y-4">
        {milestonesConfig.milestones.map((milestone) => {
          const isExpanded = expandedMilestones.has(milestone.id)

          return (
            <Card key={milestone.id} className="overflow-hidden">
              <CardHeader
                className="cursor-pointer hover:bg-slate-50 transition-colors"
                onClick={() => toggleMilestone(milestone.id)}
              >
                <div className="flex items-center justify-between">
                  <div className="flex items-center gap-3">
                    <div className="text-2xl">{milestone.icon}</div>
                    <div>
                      <CardTitle className="text-lg">{milestone.name}</CardTitle>
                      <CardDescription className="text-sm mt-1">
                        {milestone.description}
                      </CardDescription>
                    </div>
                  </div>
                  <div className="flex items-center gap-3">
                    <Badge variant="outline" className={cn('font-mono text-xs', milestone.color)}>
                      {milestone.id}
                    </Badge>
                    {isExpanded ? (
                      <ChevronUp className="w-5 h-5 text-slate-400" />
                    ) : (
                      <ChevronDown className="w-5 h-5 text-slate-400" />
                    )}
                  </div>
                </div>
              </CardHeader>

              {isExpanded && (
                <CardContent className="pt-0 pb-6">
                  <div className="space-y-6">
                    {/* Status Field */}
                    {milestone.statusField && (
                      <div>
                        <h4 className="text-sm font-semibold text-slate-900 mb-3">
                          ⭐ Status Field (Primary)
                        </h4>
                        <div className="bg-green-50 border border-green-200 rounded-lg p-4">
                          <div className="flex items-start justify-between">
                            <div>
                              <p className="font-mono text-sm text-slate-900">
                                {milestone.statusField.fieldName}
                              </p>
                              <p className="text-xs text-slate-600 mt-1">
                                Field ID: {milestone.statusField.fieldId}
                              </p>
                              {milestone.statusField.notes && (
                                <p className="text-xs text-slate-600 mt-2">
                                  {milestone.statusField.notes}
                                </p>
                              )}
                            </div>
                            <Badge className={getUsageColor(milestone.statusField.usage)}>
                              {milestone.statusField.usage} usage
                            </Badge>
                          </div>
                        </div>
                      </div>
                    )}

                    {/* Completion Fields */}
                    {milestone.completionFields && (
                      <div>
                        <h4 className="text-sm font-semibold text-slate-900 mb-3">
                          ✅ Completion Detection
                        </h4>
                        <div className="space-y-3">
                          {/* Primary */}
                          {milestone.completionFields.primary && milestone.completionFields.primary.length > 0 && (
                            <div>
                              <p className="text-xs font-medium text-slate-600 mb-2">Primary Fields:</p>
                              <div className="space-y-2">
                                {milestone.completionFields.primary.map((field: any, idx: number) => (
                                  <div
                                    key={idx}
                                    className="bg-slate-50 border border-slate-200 rounded-lg p-3 text-sm"
                                  >
                                    <div className="flex items-start justify-between">
                                      <div>
                                        <p className="font-mono text-slate-900">{field.fieldName}</p>
                                        <p className="text-xs text-slate-600 mt-1">
                                          ID: {field.fieldId}
                                        </p>
                                        {field.notes && (
                                          <p className="text-xs text-slate-600 mt-2">{field.notes}</p>
                                        )}
                                      </div>
                                      <Badge className={getUsageColor(field.usage)} variant="secondary">
                                        {field.usage}
                                      </Badge>
                                    </div>
                                  </div>
                                ))}
                              </div>
                            </div>
                          )}

                          {/* Dates */}
                          {milestone.completionFields.dates && (
                            <div>
                              <p className="text-xs font-medium text-slate-600 mb-2">Date Fields:</p>
                              <div className="space-y-2">
                                {milestone.completionFields.dates.primary && Array.isArray(milestone.completionFields.dates.primary) && milestone.completionFields.dates.primary.map((field: any, idx: number) => (
                                  <div
                                    key={idx}
                                    className="bg-slate-50 border border-slate-200 rounded-lg p-3 text-sm"
                                  >
                                    <div className="flex items-start justify-between">
                                      <div>
                                        <p className="font-mono text-slate-900">{field.fieldName}</p>
                                        <p className="text-xs text-slate-600 mt-1">
                                          ID: {field.fieldId}
                                        </p>
                                      </div>
                                      <Badge className={getUsageColor(field.usage)} variant="secondary">
                                        {field.usage}
                                      </Badge>
                                    </div>
                                  </div>
                                ))}
                              </div>
                            </div>
                          )}
                        </div>
                      </div>
                    )}

                    {/* In-Progress Fields */}
                    {milestone.inProgressFields && (
                      <div>
                        <h4 className="text-sm font-semibold text-slate-900 mb-3">
                          ⏳ In-Progress Detection
                        </h4>
                        <div className="space-y-3">
                          {/* Substeps */}
                          {milestone.inProgressFields.substeps && milestone.inProgressFields.substeps.length > 0 && (
                            <div>
                              <p className="text-xs font-medium text-slate-600 mb-2">
                                Substeps ({milestone.inProgressFields.substeps.length} total):
                              </p>
                              <div className="space-y-2">
                                {milestone.inProgressFields.substeps.map((substep: any, idx: number) => (
                                  <div
                                    key={idx}
                                    className="bg-amber-50 border border-amber-200 rounded-lg p-3 text-sm"
                                  >
                                    <div className="flex items-start justify-between">
                                      <div>
                                        <p className="font-semibold text-slate-900">
                                          {idx + 1}. {substep.label}
                                        </p>
                                        <p className="text-xs text-slate-600 mt-1">
                                          ID: {substep.fieldId} • {substep.fieldName}
                                        </p>
                                      </div>
                                      <Badge className={getUsageColor(substep.usage)} variant="secondary">
                                        {substep.usage}
                                      </Badge>
                                    </div>
                                  </div>
                                ))}
                              </div>
                            </div>
                          )}
                        </div>
                      </div>
                    )}

                    {/* Helper Fields */}
                    {milestone.helperFields && milestone.helperFields.length > 0 && (
                      <div>
                        <h4 className="text-sm font-semibold text-slate-900 mb-3">
                          🔧 Helper Fields ({milestone.helperFields.length})
                        </h4>
                        <div className="grid grid-cols-2 gap-2">
                          {milestone.helperFields.map((helper: any, idx: number) => (
                            <div
                              key={idx}
                              className="bg-blue-50 border border-blue-200 rounded p-2 text-xs"
                            >
                              <p className="font-mono text-slate-900">{helper.fieldName}</p>
                              <p className="text-slate-600 mt-1">
                                ID: {helper.fieldId} • Type: {helper.type}
                              </p>
                              {helper.usage && (
                                <Badge className={cn('mt-2 text-xs', getUsageColor(helper.usage))} variant="secondary">
                                  {helper.usage}
                                </Badge>
                              )}
                            </div>
                          ))}
                        </div>
                      </div>
                    )}

                    {/* Dependencies */}
                    {milestone.dependencies && milestone.dependencies.length > 0 && (
                      <div>
                        <h4 className="text-sm font-semibold text-slate-900 mb-3">
                          🔗 Dependencies
                        </h4>
                        <div className="flex gap-2 flex-wrap">
                          {milestone.dependencies.map((depId: string) => {
                            const depMilestone = milestonesConfig.milestones.find(m => m.id === depId)
                            return (
                              <Badge key={depId} variant="outline">
                                {depMilestone?.icon} {depMilestone?.name || depId}
                              </Badge>
                            )
                          })}
                        </div>
                      </div>
                    )}
                  </div>
                </CardContent>
              )}
            </Card>
          )
        })}
      </div>

      {/* Funding Milestones */}
      <Card>
        <CardHeader>
          <div className="flex items-center gap-2">
            <div className="text-2xl">💰</div>
            <div>
              <CardTitle>Funding Milestones (M1/M2/M3)</CardTitle>
              <CardDescription>Dashboard status tracking fields</CardDescription>
            </div>
          </div>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-3 gap-4">
            {milestonesConfig.fundingMilestones?.dashboardFields && Object.entries(milestonesConfig.fundingMilestones.dashboardFields).map(([key, field]: [string, any]) => (
              <div key={key} className="bg-green-50 border border-green-200 rounded-lg p-4">
                <p className="font-semibold text-slate-900 mb-2">
                  {key.replace('Status', '').replace(/([A-Z])/g, ' $1').trim()}
                </p>
                <p className="text-xs text-slate-600">
                  Field ID: {field.fieldId}
                </p>
                <Badge className={`${getUsageColor(field.usage)} mt-2`} variant="secondary">
                  {field.usage} usage
                </Badge>
              </div>
            ))}
          </div>
        </CardContent>
      </Card>
    </div>
  )
}
