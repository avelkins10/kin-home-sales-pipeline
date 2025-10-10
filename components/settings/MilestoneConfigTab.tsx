'use client'

import React, { useState, useEffect } from 'react'
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import { Card, CardHeader, CardTitle, CardContent, CardDescription } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Label } from '@/components/ui/label'
import { Textarea } from '@/components/ui/textarea'
import {
  ChevronDown,
  ChevronUp,
  Info,
  Save,
  X,
  RotateCcw,
  Edit2,
  Check,
  Circle,
  CheckCircle,
  Clock,
  AlertCircle,
  Ban,
  Plus,
  Trash2,
  GripVertical
} from 'lucide-react'
import { cn } from '@/lib/utils/cn'
import { toast } from 'sonner'
import { getBaseUrl } from '@/lib/utils/baseUrl'

interface MilestoneConfigTabProps {}

export function MilestoneConfigTab({}: MilestoneConfigTabProps) {
  const [expandedMilestones, setExpandedMilestones] = useState<Set<string>>(new Set())
  const [editingMilestone, setEditingMilestone] = useState<string | null>(null)
  const [editedConfig, setEditedConfig] = useState<any>(null)
  const [hasChanges, setHasChanges] = useState(false)

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
    mutationFn: async ({
      milestoneId,
      configuration,
      notes
    }: {
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
      queryClient.invalidateQueries({ queryKey: ['projects'] })
      setEditingMilestone(null)
      setEditedConfig(null)
      setHasChanges(false)
      toast.success(`Configuration saved! Traffic lights will update on next project load.`)
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
      queryClient.invalidateQueries({ queryKey: ['projects'] })
      toast.success(`Reset to defaults. Traffic lights will update on next project load.`)
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
    setHasChanges(false)
  }

  const cancelEditing = () => {
    if (hasChanges && !confirm('You have unsaved changes. Are you sure you want to cancel?')) {
      return
    }
    setEditingMilestone(null)
    setEditedConfig(null)
    setHasChanges(false)
  }

  const saveEditing = () => {
    if (!editedConfig) return

    saveMutation.mutate({
      milestoneId: editedConfig.id,
      configuration: editedConfig,
      notes: `Updated via Settings UI at ${new Date().toISOString()}`
    })
  }

  const resetToDefault = (milestoneId: string) => {
    if (
      confirm(
        `Are you sure you want to reset "${milestoneId}" to default configuration? This will delete any custom overrides and traffic lights will update immediately.`
      )
    ) {
      resetMutation.mutate(milestoneId)
    }
  }

  // Update field in edited config
  const updateField = (path: (string | number)[], value: any) => {
    if (!editedConfig) return

    const newConfig = JSON.parse(JSON.stringify(editedConfig))
    let current = newConfig

    for (let i = 0; i < path.length - 1; i++) {
      current = current[path[i]]
    }

    current[path[path.length - 1]] = value
    setEditedConfig(newConfig)
    setHasChanges(true)
  }

  // Helper for numeric field updates
  const updateNumericField = (path: (string | number)[]) => (e: React.ChangeEvent<HTMLInputElement>) => {
    updateField(path, e.target.value ? parseInt(e.target.value) : 0)
  }

  // Helper for string field updates
  const updateStringField = (path: (string | number)[]) => (
    e: React.ChangeEvent<HTMLInputElement> | React.ChangeEvent<HTMLTextAreaElement>
  ) => {
    updateField(path, e.target.value)
  }

  const getUsageColor = (usage: string) => {
    if (!usage) return 'text-slate-600 bg-slate-50'
    const percent = parseFloat(usage.replace('%', ''))
    if (percent >= 80) return 'text-green-600 bg-green-50'
    if (percent >= 50) return 'text-blue-600 bg-blue-50'
    if (percent >= 20) return 'text-orange-600 bg-orange-50'
    return 'text-red-600 bg-red-50'
  }

  const getTrafficLightIcon = (color: string) => {
    switch (color) {
      case 'green':
        return <CheckCircle className="w-4 h-4 text-emerald-500" />
      case 'yellow':
        return <Clock className="w-4 h-4 text-amber-500" />
      case 'blue':
        return <Circle className="w-4 h-4 text-blue-500" />
      case 'red':
        return <AlertCircle className="w-4 h-4 text-rose-500" />
      case 'gray':
        return <Ban className="w-4 h-4 text-slate-400" />
      default:
        return <Circle className="w-4 h-4 text-slate-400" />
    }
  }

  if (isLoading) {
    return (
      <div className="text-center py-8">
        <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-indigo-600 mx-auto"></div>
        <p className="mt-4 text-slate-600">Loading milestone configurations...</p>
      </div>
    )
  }

  if (!milestonesConfig) {
    return (
      <div className="text-center py-8 text-red-600">
        Failed to load milestone configurations
      </div>
    )
  }

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex items-start justify-between">
        <div>
          <h2 className="text-2xl font-bold text-slate-900">
            Milestone Configuration
          </h2>
          <p className="mt-2 text-sm text-slate-600">
            Configure your 7-milestone traffic light system. Changes affect all traffic lights and project views.
          </p>
          <div className="mt-2 flex items-center gap-2">
            <Badge variant="outline" className="text-xs">
              Version {milestonesConfig.version}
            </Badge>
            <Badge variant="secondary" className="text-xs">
              Last Updated: {milestonesConfig.lastUpdated}
            </Badge>
          </div>
        </div>
      </div>

      {/* Quick Reference Cards */}
      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
        <Card className="border-blue-200 bg-blue-50">
          <CardContent className="pt-6">
            <div className="flex gap-3">
              <Info className="w-5 h-5 text-blue-600 flex-shrink-0 mt-0.5" />
              <div className="text-sm text-blue-900">
                <p className="font-medium mb-1">How It Works</p>
                <p className="text-xs text-blue-800">
                  Edit milestone names, substeps, field IDs, and logic. Changes save to database and override defaults.
                </p>
              </div>
            </div>
          </CardContent>
        </Card>

        <Card className="border-purple-200 bg-purple-50">
          <CardContent className="pt-6">
            <div className="flex gap-3">
              <div className="flex-shrink-0 mt-0.5">
                <div className="flex gap-1">
                  {getTrafficLightIcon('green')}
                  {getTrafficLightIcon('yellow')}
                  {getTrafficLightIcon('red')}
                </div>
              </div>
              <div className="text-sm text-purple-900">
                <p className="font-medium mb-1">Traffic Light States</p>
                <div className="space-y-0.5 text-purple-800 text-xs">
                  <div className="flex items-center gap-1">
                    {getTrafficLightIcon('green')} <span>Complete</span>
                  </div>
                  <div className="flex items-center gap-1">
                    {getTrafficLightIcon('yellow')} <span>In Progress</span>
                  </div>
                  <div className="flex items-center gap-1">
                    {getTrafficLightIcon('red')} <span>Blocked</span>
                  </div>
                </div>
              </div>
            </div>
          </CardContent>
        </Card>

        <Card className="border-green-200 bg-green-50">
          <CardContent className="pt-6">
            <div className="flex gap-3">
              <CheckCircle className="w-5 h-5 text-green-600 flex-shrink-0 mt-0.5" />
              <div className="text-sm text-green-900">
                <p className="font-medium mb-1">Live Updates</p>
                <p className="text-xs text-green-800">
                  Saved configurations immediately affect traffic lights on next project view load.
                </p>
              </div>
            </div>
          </CardContent>
        </Card>
      </div>

      {/* Milestones */}
      <div className="space-y-4">
        {milestonesConfig.milestones.map((milestone: any) => {
          const isExpanded = expandedMilestones.has(milestone.id)
          const isEditing = editingMilestone === milestone.id
          const config = isEditing ? editedConfig : milestone
          const isCustom = milestone._metadata?.isCustom

          return (
            <Card
              key={milestone.id}
              className={cn(
                'overflow-hidden transition-all',
                isCustom && 'border-purple-300 bg-purple-50/30',
                isEditing && 'ring-2 ring-indigo-400 shadow-lg'
              )}
            >
              <CardHeader
                className={cn(
                  "transition-colors",
                  !isEditing && "cursor-pointer hover:bg-slate-50"
                )}
                onClick={() => !isEditing && toggleMilestone(milestone.id)}
              >
                <div className="flex items-center justify-between">
                  <div className="flex items-center gap-3 flex-1">
                    <div className="text-2xl">{config.icon || milestone.icon}</div>
                    <div className="flex-1">
                      {isEditing ? (
                        <div className="space-y-2">
                          <div className="flex items-center gap-2">
                            <Input
                              value={config.name || ''}
                              onChange={updateStringField(['name'])}
                              className="font-semibold text-lg h-8 max-w-xs"
                              placeholder="Milestone name"
                            />
                            <Badge variant="outline" className="font-mono text-xs bg-slate-100">
                              Order {config.order}
                            </Badge>
                            {isCustom && (
                              <Badge variant="secondary" className="bg-purple-100 text-purple-700 text-xs">
                                Custom
                              </Badge>
                            )}
                          </div>
                          <Input
                            value={config.description || ''}
                            onChange={updateStringField(['description'])}
                            className="text-sm h-8"
                            placeholder="Milestone description"
                          />
                        </div>
                      ) : (
                        <div>
                          <div className="flex items-center gap-2">
                            <CardTitle className="text-lg">{config.name}</CardTitle>
                            <Badge variant="outline" className="font-mono text-xs bg-slate-100">
                              Order {config.order}
                            </Badge>
                            {isCustom && (
                              <Badge variant="secondary" className="bg-purple-100 text-purple-700 text-xs">
                                Custom
                              </Badge>
                            )}
                          </div>
                          <CardDescription className="text-sm mt-1">
                            {config.description}
                          </CardDescription>
                        </div>
                      )}
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
                          className="gap-1"
                        >
                          <Edit2 className="w-3 h-3" />
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
                            className="gap-1"
                          >
                            <RotateCcw className="w-3 h-3" />
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
                          disabled={saveMutation.isPending || !hasChanges}
                          className="gap-1 bg-green-600 hover:bg-green-700"
                        >
                          <Check className="w-3 h-3" />
                          {saveMutation.isPending ? 'Saving...' : 'Save Changes'}
                        </Button>
                        <Button
                          variant="outline"
                          size="sm"
                          onClick={(e) => {
                            e.stopPropagation()
                            cancelEditing()
                          }}
                          className="gap-1"
                        >
                          <X className="w-3 h-3" />
                          Cancel
                        </Button>
                      </>
                    )}
                    <Badge variant="outline" className={cn('font-mono text-xs')}>
                      {milestone.id}
                    </Badge>
                    {!isEditing &&
                      (isExpanded ? (
                        <ChevronUp className="w-5 h-5 text-slate-400" />
                      ) : (
                        <ChevronDown className="w-5 h-5 text-slate-400" />
                      ))}
                  </div>
                </div>
              </CardHeader>

              {isExpanded && (
                <CardContent className="pt-0 pb-6">
                  <div className="space-y-6">
                    {/* Substeps Section - Most Important for Editing */}
                    {config.inProgressFields?.substeps && (
                      <div>
                        <div className="flex items-center justify-between mb-3">
                          <h4 className="text-sm font-semibold text-slate-900 flex items-center gap-2">
                            <span className="text-lg">🔄</span>
                            Substeps (In-Progress Tracking)
                          </h4>
                        </div>
                        <div className="space-y-2">
                          {config.inProgressFields.substeps.map((substep: any, idx: number) => (
                            <div
                              key={idx}
                              className={cn(
                                "rounded-lg p-3 border",
                                isEditing ? "bg-white border-amber-300" : "bg-amber-50 border-amber-200"
                              )}
                            >
                              {isEditing ? (
                                <div className="grid grid-cols-4 gap-3">
                                  <div>
                                    <Label className="text-xs text-slate-600">Order</Label>
                                    <Input
                                      value={substep.order?.toString() || ''}
                                      onChange={updateNumericField(['inProgressFields', 'substeps', idx, 'order'])}
                                      className="h-8 mt-1"
                                    />
                                  </div>
                                  <div>
                                    <Label className="text-xs text-slate-600">Label</Label>
                                    <Input
                                      value={substep.label || ''}
                                      onChange={updateStringField(['inProgressFields', 'substeps', idx, 'label'])}
                                      className="h-8 mt-1"
                                      placeholder="Substep label"
                                    />
                                  </div>
                                  <div>
                                    <Label className="text-xs text-slate-600">Field ID</Label>
                                    <Input
                                      value={substep.fieldId?.toString() || ''}
                                      onChange={updateNumericField(['inProgressFields', 'substeps', idx, 'fieldId'])}
                                      className="h-8 mt-1 font-mono"
                                    />
                                  </div>
                                  <div>
                                    <Label className="text-xs text-slate-600">Usage</Label>
                                    <Input
                                      value={substep.usage || ''}
                                      onChange={updateStringField(['inProgressFields', 'substeps', idx, 'usage'])}
                                      className="h-8 mt-1"
                                      placeholder="e.g., 95%"
                                    />
                                  </div>
                                </div>
                              ) : (
                                <div className="grid grid-cols-4 gap-3">
                                  <div>
                                    <p className="text-xs text-slate-600">Order</p>
                                    <Badge variant="outline" className="mt-1">{substep.order}</Badge>
                                  </div>
                                  <div>
                                    <p className="text-xs text-slate-600">Label</p>
                                    <p className="text-sm font-medium mt-1">{substep.label}</p>
                                  </div>
                                  <div>
                                    <p className="text-xs text-slate-600">Field ID</p>
                                    <p className="font-mono text-sm font-semibold mt-1">{substep.fieldId}</p>
                                  </div>
                                  <div>
                                    <p className="text-xs text-slate-600">Usage</p>
                                    <Badge className={cn(getUsageColor(substep.usage), 'mt-1')} variant="secondary">
                                      {substep.usage}
                                    </Badge>
                                  </div>
                                </div>
                              )}
                              {substep.notes && !isEditing && (
                                <p className="text-xs text-slate-600 mt-2 border-t border-amber-100 pt-2">
                                  {substep.notes}
                                </p>
                              )}
                            </div>
                          ))}
                        </div>

                        {/* HOA Substeps */}
                        {config.inProgressFields.hoa && (
                          <div className="mt-3">
                            <p className="text-xs font-medium text-purple-700 mb-2">
                              HOA Substeps (Conditional for Permitting)
                            </p>
                            <div className="space-y-2">
                              {config.inProgressFields.hoa.map((substep: any, idx: number) => (
                                <div
                                  key={idx}
                                  className={cn(
                                    "rounded-lg p-3 border",
                                    isEditing ? "bg-white border-purple-300" : "bg-purple-50 border-purple-200"
                                  )}
                                >
                                  {isEditing ? (
                                    <div className="grid grid-cols-4 gap-3">
                                      <div>
                                        <Label className="text-xs text-slate-600">Order</Label>
                                        <Input
                                          value={substep.order?.toString() || ''}
                                          onChange={updateNumericField(['inProgressFields', 'hoa', idx, 'order'])}
                                          className="h-8 mt-1"
                                        />
                                      </div>
                                      <div>
                                        <Label className="text-xs text-slate-600">Label</Label>
                                        <Input
                                          value={substep.label || ''}
                                          onChange={updateStringField(['inProgressFields', 'hoa', idx, 'label'])}
                                          className="h-8 mt-1"
                                        />
                                      </div>
                                      <div>
                                        <Label className="text-xs text-slate-600">Field ID</Label>
                                        <Input
                                          value={substep.fieldId?.toString() || ''}
                                          onChange={updateNumericField(['inProgressFields', 'hoa', idx, 'fieldId'])}
                                          className="h-8 mt-1 font-mono"
                                        />
                                      </div>
                                      <div>
                                        <Label className="text-xs text-slate-600">Usage</Label>
                                        <Input
                                          value={substep.usage || ''}
                                          onChange={updateStringField(['inProgressFields', 'hoa', idx, 'usage'])}
                                          className="h-8 mt-1"
                                        />
                                      </div>
                                    </div>
                                  ) : (
                                    <div className="grid grid-cols-4 gap-3">
                                      <div>
                                        <p className="text-xs text-slate-600">Order</p>
                                        <Badge variant="outline" className="mt-1">{substep.order}</Badge>
                                      </div>
                                      <div>
                                        <p className="text-xs text-slate-600">Label</p>
                                        <p className="text-sm font-medium mt-1">{substep.label}</p>
                                      </div>
                                      <div>
                                        <p className="text-xs text-slate-600">Field ID</p>
                                        <p className="font-mono text-sm font-semibold mt-1">{substep.fieldId}</p>
                                      </div>
                                      <div>
                                        <p className="text-xs text-slate-600">Usage</p>
                                        <Badge className={cn(getUsageColor(substep.usage), 'mt-1')} variant="secondary">
                                          {substep.usage}
                                        </Badge>
                                      </div>
                                    </div>
                                  )}
                                </div>
                              ))}
                            </div>
                          </div>
                        )}
                      </div>
                    )}

                    {/* Completion Fields */}
                    {config.completionFields && (
                      <div>
                        <h4 className="text-sm font-semibold text-slate-900 mb-3 flex items-center gap-2">
                          <span className="text-lg">✅</span>
                          Completion Fields
                        </h4>
                        <div className="text-xs text-slate-600 mb-2">
                          These fields determine when the milestone is marked as complete
                        </div>
                        <div className="space-y-3">
                          {config.completionFields.primary && (
                            <div className="space-y-2">
                              <p className="text-xs font-medium text-emerald-700">Primary Fields</p>
                              {config.completionFields.primary.map((field: any, idx: number) => (
                                <div
                                  key={idx}
                                  className={cn(
                                    "rounded-lg p-3 border",
                                    isEditing ? "bg-white border-emerald-300" : "bg-emerald-50 border-emerald-200"
                                  )}
                                >
                                  {isEditing ? (
                                    <div className="grid grid-cols-3 gap-3">
                                      <div>
                                        <Label className="text-xs text-slate-600">Field ID</Label>
                                        <Input
                                          value={field.fieldId?.toString() || ''}
                                          onChange={updateNumericField(['completionFields', 'primary', idx, 'fieldId'])}
                                          className="h-8 mt-1 font-mono"
                                        />
                                      </div>
                                      <div>
                                        <Label className="text-xs text-slate-600">Field Name</Label>
                                        <Input
                                          value={field.fieldName || ''}
                                          onChange={updateStringField(['completionFields', 'primary', idx, 'fieldName'])}
                                          className="h-8 mt-1"
                                        />
                                      </div>
                                      <div>
                                        <Label className="text-xs text-slate-600">Usage</Label>
                                        <Input
                                          value={field.usage || ''}
                                          onChange={updateStringField(['completionFields', 'primary', idx, 'usage'])}
                                          className="h-8 mt-1"
                                        />
                                      </div>
                                    </div>
                                  ) : (
                                    <div className="grid grid-cols-3 gap-3">
                                      <div>
                                        <p className="text-xs text-slate-600">Field ID</p>
                                        <p className="font-mono text-sm font-semibold">{field.fieldId}</p>
                                      </div>
                                      <div>
                                        <p className="text-xs text-slate-600">Field Name</p>
                                        <p className="text-sm">{field.fieldName}</p>
                                      </div>
                                      <div>
                                        <p className="text-xs text-slate-600">Usage</p>
                                        <Badge className={getUsageColor(field.usage)} variant="secondary">
                                          {field.usage}
                                        </Badge>
                                      </div>
                                    </div>
                                  )}
                                </div>
                              ))}
                            </div>
                          )}
                        </div>
                      </div>
                    )}

                    {/* Blocked State */}
                    {config.blockedState && (
                      <div>
                        <h4 className="text-sm font-semibold text-slate-900 mb-3 flex items-center gap-2">
                          <span className="text-lg">🚫</span>
                          Blocked State Logic
                        </h4>
                        <div className={cn(
                          "rounded-lg p-4 border",
                          isEditing ? "bg-white border-rose-300" : "bg-rose-50 border-rose-200"
                        )}>
                          {isEditing ? (
                            <div className="space-y-3">
                              <div>
                                <Label className="text-xs text-slate-600">Condition</Label>
                                <Input
                                  value={config.blockedState.condition || ''}
                                  onChange={updateStringField(['blockedState', 'condition'])}
                                  className="h-8 mt-1"
                                  placeholder="e.g., NEM Signatures sent but not submitted"
                                />
                              </div>
                              <div className="grid grid-cols-2 gap-3">
                                <div>
                                  <Label className="text-xs text-slate-600">Threshold</Label>
                                  <Input
                                    value={config.blockedState.threshold?.toString() || ''}
                                    onChange={updateNumericField(['blockedState', 'threshold'])}
                                    className="h-8 mt-1"
                                  />
                                </div>
                                <div>
                                  <Label className="text-xs text-slate-600">Unit</Label>
                                  <Input
                                    value={config.blockedState.unit || ''}
                                    onChange={updateStringField(['blockedState', 'unit'])}
                                    className="h-8 mt-1"
                                    placeholder="e.g., days"
                                  />
                                </div>
                              </div>
                              <div>
                                <Label className="text-xs text-slate-600">Message</Label>
                                <Textarea
                                  value={config.blockedState.message || ''}
                                  onChange={updateStringField(['blockedState', 'message'])}
                                  className="mt-1"
                                  rows={2}
                                />
                              </div>
                            </div>
                          ) : (
                            <div>
                              <div className="grid grid-cols-2 gap-4 mb-3">
                                <div>
                                  <p className="text-xs font-medium text-slate-600 mb-1">Condition</p>
                                  <p className="text-sm">{config.blockedState.condition}</p>
                                </div>
                                <div>
                                  <p className="text-xs font-medium text-slate-600 mb-1">Threshold</p>
                                  <p className="text-sm">
                                    {config.blockedState.threshold} {config.blockedState.unit}
                                  </p>
                                </div>
                              </div>
                              <div className="pt-3 border-t border-rose-100">
                                <p className="text-xs font-medium text-slate-600 mb-1">Message</p>
                                <p className="text-sm text-rose-700">{config.blockedState.message}</p>
                              </div>
                            </div>
                          )}
                        </div>
                      </div>
                    )}

                    {/* Edit Mode Helper */}
                    {!isEditing && (
                      <div className="pt-4 border-t border-slate-200">
                        <div className="flex items-center gap-2 text-xs text-slate-500">
                          <Info className="w-4 h-4" />
                          <span>
                            Click &quot;Edit&quot; to customize milestone name, substep labels, field IDs, thresholds, and more
                          </span>
                        </div>
                      </div>
                    )}

                    {/* Save Reminder */}
                    {isEditing && hasChanges && (
                      <div className="pt-4 border-t border-orange-200 bg-orange-50 -mx-6 -mb-6 px-6 py-4 rounded-b-lg">
                        <div className="flex items-center gap-2 text-sm text-orange-900">
                          <AlertCircle className="w-4 h-4" />
                          <span className="font-medium">You have unsaved changes</span>
                        </div>
                        <p className="text-xs text-orange-700 mt-1">
                          Click &quot;Save Changes&quot; to apply your edits. Changes will affect all traffic lights and project views.
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

      {/* Help Section */}
      <Card className="border-indigo-200 bg-indigo-50">
        <CardContent className="pt-6">
          <div className="flex gap-3">
            <Info className="w-5 h-5 text-indigo-600 flex-shrink-0 mt-0.5" />
            <div className="text-sm text-indigo-900">
              <p className="font-medium mb-2">Configuration Guide</p>
              <ul className="space-y-1 text-xs text-indigo-800">
                <li><strong>Milestone Names:</strong> Display names shown in traffic lights and project views</li>
                <li><strong>Substep Labels:</strong> Progress indicators shown during in-progress state (e.g., &quot;Submitted&quot;, &quot;Approved&quot;)</li>
                <li><strong>Field IDs:</strong> QuickBase field IDs that map to this milestone&apos;s data</li>
                <li><strong>Usage %:</strong> How often this field is populated in your projects (for reference)</li>
                <li><strong>Blocked Logic:</strong> Conditions that trigger red traffic light (e.g., stuck for X days)</li>
                <li><strong>Custom Configurations:</strong> Purple badges indicate customized milestones that override defaults</li>
                <li><strong>Reset:</strong> Removes custom configuration and reverts to system defaults</li>
              </ul>
            </div>
          </div>
        </CardContent>
      </Card>
    </div>
  )
}
