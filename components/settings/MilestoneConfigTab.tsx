'use client'

import { useState, useEffect } from 'react'
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import { Card, CardHeader, CardTitle, CardContent, CardDescription } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Label } from '@/components/ui/label'
import { Textarea } from '@/components/ui/textarea'
import { ChevronDown, ChevronUp, Settings, Info, Save, X, RotateCcw, Edit2, Check } from 'lucide-react'
import { cn } from '@/lib/utils/cn'
import { toast } from 'sonner'
import { getBaseUrl } from '@/lib/utils/baseUrl'

interface MilestoneConfigTabProps {
  // Future: Add ability to edit and save configurations
}

export function MilestoneConfigTab({}: MilestoneConfigTabProps) {
  const [expandedMilestones, setExpandedMilestones] = useState<Set<string>>(new Set())
  const [editingMilestone, setEditingMilestone] = useState<string | null>(null)
  const [editedConfig, setEditedConfig] = useState<any>(null)

  const queryClient = useQueryClient()

  // Fetch milestone configurations
  const { data: milestonesConfig, isLoading } = useQuery({
    queryKey: ['milestone-config'],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/admin/milestones/config`)
      if (!response.ok) throw new Error('Failed to fetch milestone config')
      return response.json()
    },
    staleTime: 300000, // 5 minutes
  })

  // Save milestone configuration mutation
  const saveMutation = useMutation({
    mutationFn: async ({ milestoneId, configuration, notes }: {
      milestoneId: string
      configuration: any
      notes?: string
    }) => {
      const response = await fetch(`${getBaseUrl()}/api/admin/milestones/config`, {
        method: 'PATCH',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ milestoneId, configuration, notes })
      })
      if (!response.ok) {
        const error = await response.json()
        throw new Error(error.error || 'Failed to save configuration')
      }
      return response.json()
    },
    onSuccess: (data) => {
      queryClient.invalidateQueries({ queryKey: ['milestone-config'] })
      setEditingMilestone(null)
      setEditedConfig(null)
      toast.success(`Milestone configuration saved for ${data.milestoneId}`)
    },
    onError: (error: Error) => {
      toast.error(error.message)
    }
  })

  // Reset milestone configuration mutation
  const resetMutation = useMutation({
    mutationFn: async (milestoneId: string) => {
      const response = await fetch(`${getBaseUrl()}/api/admin/milestones/config/reset`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ milestoneId })
      })
      if (!response.ok) {
        const error = await response.json()
        throw new Error(error.error || 'Failed to reset configuration')
      }
      return response.json()
    },
    onSuccess: (data) => {
      queryClient.invalidateQueries({ queryKey: ['milestone-config'] })
      toast.success(`Reset ${data.milestoneId} to default configuration`)
    },
    onError: (error: Error) => {
      toast.error(error.message)
    }
  })

  const toggleMilestone = (milestoneId: string) => {
    const newExpanded = new Set(expandedMilestones)
    if (newExpanded.has(milestoneId)) {
      newExpanded.delete(milestoneId)
    } else {
      newExpanded.add(milestoneId)
    }
    setExpandedMilestones(newExpanded)
  }

  const startEditing = (milestone: any) => {
    setEditingMilestone(milestone.id)
    setEditedConfig(JSON.parse(JSON.stringify(milestone))) // Deep clone
  }

  const cancelEditing = () => {
    setEditingMilestone(null)
    setEditedConfig(null)
  }

  const saveEditing = () => {
    if (!editedConfig) return

    saveMutation.mutate({
      milestoneId: editedConfig.id,
      configuration: editedConfig,
      notes: `Updated via Settings UI`
    })
  }

  const resetToDefault = (milestoneId: string) => {
    if (confirm(`Are you sure you want to reset "${milestoneId}" to default configuration? This will delete any custom overrides.`)) {
      resetMutation.mutate(milestoneId)
    }
  }

  const updateEditedField = (path: string[], value: any) => {
    if (!editedConfig) return

    const newConfig = JSON.parse(JSON.stringify(editedConfig))
    let current = newConfig

    for (let i = 0; i < path.length - 1; i++) {
      if (!current[path[i]]) {
        current[path[i]] = {}
      }
      current = current[path[i]]
    }

    current[path[path.length - 1]] = value
    setEditedConfig(newConfig)
  }

  const getUsageColor = (usage: string) => {
    const percent = parseFloat(usage.replace('%', ''))
    if (percent >= 80) return 'text-green-600 bg-green-50'
    if (percent >= 50) return 'text-blue-600 bg-blue-50'
    if (percent >= 20) return 'text-orange-600 bg-orange-50'
    return 'text-red-600 bg-red-50'
  }

  if (isLoading) {
    return <div className="text-center py-8">Loading milestone configurations...</div>
  }

  if (!milestonesConfig) {
    return <div className="text-center py-8 text-red-600">Failed to load milestone configurations</div>
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
              <p className="mt-3 text-xs text-blue-700">
                💡 <strong>Tip:</strong> Custom configurations override defaults. Click &quot;Edit&quot; on any milestone to customize field mappings.
              </p>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Milestones */}
      <div className="space-y-4">
        {milestonesConfig.milestones.map((milestone: any) => {
          const isExpanded = expandedMilestones.has(milestone.id)
          const isEditing = editingMilestone === milestone.id
          const config = isEditing ? editedConfig : milestone
          const isCustom = milestone._metadata?.isCustom

          return (
            <Card key={milestone.id} className={cn("overflow-hidden", isCustom && "border-purple-300 bg-purple-50/30")}>
              <CardHeader
                className="cursor-pointer hover:bg-slate-50 transition-colors"
                onClick={() => !isEditing && toggleMilestone(milestone.id)}
              >
                <div className="flex items-center justify-between">
                  <div className="flex items-center gap-3">
                    <div className="text-2xl">{milestone.icon}</div>
                    <div>
                      <div className="flex items-center gap-2">
                        <CardTitle className="text-lg">{milestone.name}</CardTitle>
                        {isCustom && (
                          <Badge variant="secondary" className="bg-purple-100 text-purple-700 text-xs">
                            Custom
                          </Badge>
                        )}
                      </div>
                      <CardDescription className="text-sm mt-1">
                        {milestone.description}
                      </CardDescription>
                    </div>
                  </div>
                  <div className="flex items-center gap-2">
                    {!isEditing && isExpanded && (
                      <>
                        <Button
                          variant="outline"
                          size="sm"
                          onClick={(e) => {
                            e.stopPropagation()
                            startEditing(milestone)
                          }}
                        >
                          <Edit2 className="w-3 h-3 mr-1" />
                          Edit
                        </Button>
                        {isCustom && (
                          <Button
                            variant="outline"
                            size="sm"
                            onClick={(e) => {
                              e.stopPropagation()
                              resetToDefault(milestone.id)
                            }}
                            disabled={resetMutation.isPending}
                          >
                            <RotateCcw className="w-3 h-3 mr-1" />
                            Reset
                          </Button>
                        )}
                      </>
                    )}
                    {isEditing && (
                      <>
                        <Button
                          variant="default"
                          size="sm"
                          onClick={(e) => {
                            e.stopPropagation()
                            saveEditing()
                          }}
                          disabled={saveMutation.isPending}
                        >
                          <Check className="w-3 h-3 mr-1" />
                          Save
                        </Button>
                        <Button
                          variant="outline"
                          size="sm"
                          onClick={(e) => {
                            e.stopPropagation()
                            cancelEditing()
                          }}
                        >
                          <X className="w-3 h-3 mr-1" />
                          Cancel
                        </Button>
                      </>
                    )}
                    <Badge variant="outline" className={cn('font-mono text-xs')}>
                      {milestone.id}
                    </Badge>
                    {!isEditing && (
                      isExpanded ? (
                        <ChevronUp className="w-5 h-5 text-slate-400" />
                      ) : (
                        <ChevronDown className="w-5 h-5 text-slate-400" />
                      )
                    )}
                  </div>
                </div>
              </CardHeader>

              {isExpanded && (
                <CardContent className="pt-0 pb-6">
                  <div className="space-y-6">
                    {/* Status Field */}
                    {config.statusField && (
                      <div>
                        <h4 className="text-sm font-semibold text-slate-900 mb-3">
                          ⭐ Status Field (Primary)
                        </h4>
                        <div className={cn("border rounded-lg p-4", isEditing ? "bg-white" : "bg-green-50 border-green-200")}>
                          <div className="space-y-3">
                            <div className="grid grid-cols-2 gap-3">
                              <div>
                                <Label className="text-xs">Field ID</Label>
                                <Input
                                  type="number"
                                  value={config.statusField.fieldId}
                                  onChange={(e) => updateEditedField(['statusField', 'fieldId'], parseInt(e.target.value))}
                                  disabled={!isEditing}
                                  className="mt-1"
                                />
                              </div>
                              <div>
                                <Label className="text-xs">Usage %</Label>
                                <Input
                                  value={config.statusField.usage}
                                  onChange={(e) => updateEditedField(['statusField', 'usage'], e.target.value)}
                                  disabled={!isEditing}
                                  className="mt-1"
                                />
                              </div>
                            </div>
                            <div>
                              <Label className="text-xs">Field Name</Label>
                              <Input
                                value={config.statusField.fieldName}
                                onChange={(e) => updateEditedField(['statusField', 'fieldName'], e.target.value)}
                                disabled={!isEditing}
                                className="mt-1"
                              />
                            </div>
                            <div>
                              <Label className="text-xs">Notes</Label>
                              <Textarea
                                value={config.statusField.notes || ''}
                                onChange={(e) => updateEditedField(['statusField', 'notes'], e.target.value)}
                                disabled={!isEditing}
                                className="mt-1"
                                rows={2}
                              />
                            </div>
                          </div>
                        </div>
                      </div>
                    )}

                    {/* Dependencies */}
                    {config.dependencies && config.dependencies.length > 0 && (
                      <div>
                        <h4 className="text-sm font-semibold text-slate-900 mb-3">
                          🔗 Dependencies
                        </h4>
                        <div className="flex gap-2 flex-wrap">
                          {config.dependencies.map((depId: string) => {
                            const depMilestone = milestonesConfig.milestones.find((m: any) => m.id === depId)
                            return (
                              <Badge key={depId} variant="outline">
                                {depMilestone?.icon} {depMilestone?.name || depId}
                              </Badge>
                            )
                          })}
                        </div>
                      </div>
                    )}

                    {!isEditing && (
                      <div className="pt-4 border-t border-slate-200">
                        <p className="text-xs text-slate-500">
                          💡 Click &quot;Edit&quot; to customize field IDs, usage percentages, and other configuration details
                        </p>
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
