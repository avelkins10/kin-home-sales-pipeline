'use client'

import { useState } from 'react'
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
  Ban
} from 'lucide-react'
import { cn } from '@/lib/utils/cn'
import { toast } from 'sonner'
import { getBaseUrl } from '@/lib/utils/baseUrl'

interface MilestoneConfigTabProps {}

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
    if (
      confirm(
        `Are you sure you want to reset "${milestoneId}" to default configuration? This will delete any custom overrides.`
      )
    ) {
      resetMutation.mutate(milestoneId)
    }
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
            7-milestone traffic light system based on analysis of 1,529 projects
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

      {/* Info Cards */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
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
                <div className="space-y-1 text-purple-800 text-xs">
                  <div className="flex items-center gap-2">
                    {getTrafficLightIcon('green')} <span>Complete</span>
                  </div>
                  <div className="flex items-center gap-2">
                    {getTrafficLightIcon('yellow')} <span>In Progress</span>
                  </div>
                  <div className="flex items-center gap-2">
                    {getTrafficLightIcon('blue')} <span>Ready to Start</span>
                  </div>
                  <div className="flex items-center gap-2">
                    {getTrafficLightIcon('red')} <span>Blocked</span>
                  </div>
                  <div className="flex items-center gap-2">
                    {getTrafficLightIcon('gray')} <span>Pending</span>
                  </div>
                </div>
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
                'overflow-hidden',
                isCustom && 'border-purple-300 bg-purple-50/30'
              )}
            >
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
                        <Badge
                          variant="outline"
                          className="font-mono text-xs bg-slate-100"
                        >
                          Order {milestone.order}
                        </Badge>
                        {isCustom && (
                          <Badge
                            variant="secondary"
                            className="bg-purple-100 text-purple-700 text-xs"
                          >
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
                    {/* Status Field */}
                    {config.statusField && (
                      <div>
                        <h4 className="text-sm font-semibold text-slate-900 mb-3 flex items-center gap-2">
                          <span className="text-lg">⭐</span>
                          Status Field (Primary Indicator)
                        </h4>
                        <div
                          className={cn(
                            'border rounded-lg p-4',
                            'bg-green-50 border-green-200'
                          )}
                        >
                          <div className="grid grid-cols-3 gap-4">
                            <div>
                              <p className="text-xs font-medium text-slate-600 mb-1">
                                Field ID
                              </p>
                              <p className="font-mono text-sm font-semibold">
                                {config.statusField.fieldId}
                              </p>
                            </div>
                            <div>
                              <p className="text-xs font-medium text-slate-600 mb-1">
                                Field Name
                              </p>
                              <p className="text-sm">{config.statusField.fieldName}</p>
                            </div>
                            <div>
                              <p className="text-xs font-medium text-slate-600 mb-1">
                                Usage
                              </p>
                              <Badge
                                className={getUsageColor(config.statusField.usage)}
                                variant="secondary"
                              >
                                {config.statusField.usage}
                              </Badge>
                            </div>
                          </div>
                          {config.statusField.notes && (
                            <p className="text-xs text-slate-600 mt-3 border-t border-green-100 pt-3">
                              💡 {config.statusField.notes}
                            </p>
                          )}
                        </div>
                      </div>
                    )}

                    {/* Completion Fields */}
                    {config.completionFields && (
                      <div>
                        <h4 className="text-sm font-semibold text-slate-900 mb-3 flex items-center gap-2">
                          <span className="text-lg">✅</span>
                          Completion Fields
                        </h4>
                        <div className="space-y-3">
                          {/* Primary */}
                          {config.completionFields.primary && (
                            <div>
                              <p className="text-xs font-medium text-emerald-700 mb-2">
                                Primary Fields
                              </p>
                              <div className="space-y-2">
                                {config.completionFields.primary.map(
                                  (field: any, idx: number) => (
                                    <div
                                      key={idx}
                                      className="bg-emerald-50 border border-emerald-200 rounded-lg p-3"
                                    >
                                      <div className="grid grid-cols-3 gap-3">
                                        <div>
                                          <p className="text-xs text-slate-600">Field ID</p>
                                          <p className="font-mono text-sm font-semibold">
                                            {field.fieldId}
                                          </p>
                                        </div>
                                        <div>
                                          <p className="text-xs text-slate-600">
                                            Field Name
                                          </p>
                                          <p className="text-sm">{field.fieldName}</p>
                                        </div>
                                        <div>
                                          <p className="text-xs text-slate-600">Usage</p>
                                          <Badge
                                            className={getUsageColor(field.usage)}
                                            variant="secondary"
                                          >
                                            {field.usage}
                                          </Badge>
                                        </div>
                                      </div>
                                      {field.notes && (
                                        <p className="text-xs text-slate-600 mt-2">
                                          {field.notes}
                                        </p>
                                      )}
                                    </div>
                                  )
                                )}
                              </div>
                            </div>
                          )}

                          {/* Backup */}
                          {config.completionFields.backup && (
                            <div>
                              <p className="text-xs font-medium text-blue-700 mb-2">
                                Backup/Fallback Fields
                              </p>
                              <div className="space-y-2">
                                {config.completionFields.backup.map(
                                  (field: any, idx: number) => (
                                    <div
                                      key={idx}
                                      className="bg-blue-50 border border-blue-200 rounded-lg p-3"
                                    >
                                      <div className="grid grid-cols-3 gap-3">
                                        <div>
                                          <p className="text-xs text-slate-600">Field ID</p>
                                          <p className="font-mono text-sm font-semibold">
                                            {field.fieldId}
                                          </p>
                                        </div>
                                        <div>
                                          <p className="text-xs text-slate-600">
                                            Field Name
                                          </p>
                                          <p className="text-sm">{field.fieldName}</p>
                                        </div>
                                        <div>
                                          <p className="text-xs text-slate-600">Usage</p>
                                          <Badge
                                            className={getUsageColor(field.usage)}
                                            variant="secondary"
                                          >
                                            {field.usage}
                                          </Badge>
                                        </div>
                                      </div>
                                    </div>
                                  )
                                )}
                              </div>
                            </div>
                          )}

                          {/* Optional */}
                          {config.completionFields.optional && (
                            <div>
                              <p className="text-xs font-medium text-purple-700 mb-2">
                                Optional Fields (Conditional)
                              </p>
                              <div className="space-y-2">
                                {config.completionFields.optional.map(
                                  (field: any, idx: number) => (
                                    <div
                                      key={idx}
                                      className="bg-purple-50 border border-purple-200 rounded-lg p-3"
                                    >
                                      <div className="grid grid-cols-3 gap-3">
                                        <div>
                                          <p className="text-xs text-slate-600">Field ID</p>
                                          <p className="font-mono text-sm font-semibold">
                                            {field.fieldId}
                                          </p>
                                        </div>
                                        <div>
                                          <p className="text-xs text-slate-600">
                                            Field Name
                                          </p>
                                          <p className="text-sm">{field.fieldName}</p>
                                        </div>
                                        <div>
                                          <p className="text-xs text-slate-600">Usage</p>
                                          <Badge
                                            className={getUsageColor(field.usage)}
                                            variant="secondary"
                                          >
                                            {field.usage}
                                          </Badge>
                                        </div>
                                      </div>
                                      {field.notes && (
                                        <p className="text-xs text-slate-600 mt-2">
                                          {field.notes}
                                        </p>
                                      )}
                                    </div>
                                  )
                                )}
                              </div>
                            </div>
                          )}
                        </div>
                      </div>
                    )}

                    {/* In-Progress Fields (Substeps) */}
                    {config.inProgressFields?.substeps && (
                      <div>
                        <h4 className="text-sm font-semibold text-slate-900 mb-3 flex items-center gap-2">
                          <span className="text-lg">🔄</span>
                          Substeps (In-Progress Tracking)
                        </h4>
                        <div className="space-y-2">
                          {config.inProgressFields.substeps.map(
                            (substep: any, idx: number) => (
                              <div
                                key={idx}
                                className="bg-amber-50 border border-amber-200 rounded-lg p-3"
                              >
                                <div className="flex items-start justify-between">
                                  <div className="flex-1 grid grid-cols-4 gap-3">
                                    <div>
                                      <p className="text-xs text-slate-600">Order</p>
                                      <Badge variant="outline" className="mt-1">
                                        {substep.order}
                                      </Badge>
                                    </div>
                                    <div>
                                      <p className="text-xs text-slate-600">Label</p>
                                      <p className="text-sm font-medium mt-1">
                                        {substep.label}
                                      </p>
                                    </div>
                                    <div>
                                      <p className="text-xs text-slate-600">Field ID</p>
                                      <p className="font-mono text-sm font-semibold mt-1">
                                        {substep.fieldId}
                                      </p>
                                    </div>
                                    <div>
                                      <p className="text-xs text-slate-600">Usage</p>
                                      <Badge
                                        className={cn(
                                          getUsageColor(substep.usage),
                                          'mt-1'
                                        )}
                                        variant="secondary"
                                      >
                                        {substep.usage}
                                      </Badge>
                                    </div>
                                  </div>
                                </div>
                                {substep.notes && (
                                  <p className="text-xs text-slate-600 mt-2 border-t border-amber-100 pt-2">
                                    {substep.notes}
                                  </p>
                                )}
                              </div>
                            )
                          )}
                        </div>

                        {/* HOA Substeps (if applicable - for Permitting) */}
                        {config.inProgressFields.hoa && (
                          <div className="mt-3">
                            <p className="text-xs font-medium text-purple-700 mb-2">
                              HOA Substeps (Conditional)
                            </p>
                            <div className="space-y-2">
                              {config.inProgressFields.hoa.map(
                                (substep: any, idx: number) => (
                                  <div
                                    key={idx}
                                    className="bg-purple-50 border border-purple-200 rounded-lg p-3"
                                  >
                                    <div className="grid grid-cols-4 gap-3">
                                      <div>
                                        <p className="text-xs text-slate-600">Order</p>
                                        <Badge variant="outline" className="mt-1">
                                          {substep.order}
                                        </Badge>
                                      </div>
                                      <div>
                                        <p className="text-xs text-slate-600">Label</p>
                                        <p className="text-sm font-medium mt-1">
                                          {substep.label}
                                        </p>
                                      </div>
                                      <div>
                                        <p className="text-xs text-slate-600">Field ID</p>
                                        <p className="font-mono text-sm font-semibold mt-1">
                                          {substep.fieldId}
                                        </p>
                                      </div>
                                      <div>
                                        <p className="text-xs text-slate-600">Usage</p>
                                        <Badge
                                          className={cn(
                                            getUsageColor(substep.usage),
                                            'mt-1'
                                          )}
                                          variant="secondary"
                                        >
                                          {substep.usage}
                                        </Badge>
                                      </div>
                                    </div>
                                  </div>
                                )
                              )}
                            </div>
                          </div>
                        )}
                      </div>
                    )}

                    {/* Scheduled Fields */}
                    {config.scheduledFields && (
                      <div>
                        <h4 className="text-sm font-semibold text-slate-900 mb-3 flex items-center gap-2">
                          <span className="text-lg">📅</span>
                          Scheduled Date Fields
                        </h4>
                        <div className="space-y-2">
                          {config.scheduledFields.primary && (
                            <div className="bg-blue-50 border border-blue-200 rounded-lg p-3">
                              <p className="text-xs font-medium text-blue-700 mb-2">
                                Primary
                              </p>
                              <div className="grid grid-cols-3 gap-3">
                                <div>
                                  <p className="text-xs text-slate-600">Field ID</p>
                                  <p className="font-mono text-sm font-semibold">
                                    {config.scheduledFields.primary.fieldId}
                                  </p>
                                </div>
                                <div>
                                  <p className="text-xs text-slate-600">Field Name</p>
                                  <p className="text-sm">
                                    {config.scheduledFields.primary.fieldName}
                                  </p>
                                </div>
                                <div>
                                  <p className="text-xs text-slate-600">Usage</p>
                                  <Badge
                                    className={getUsageColor(
                                      config.scheduledFields.primary.usage
                                    )}
                                    variant="secondary"
                                  >
                                    {config.scheduledFields.primary.usage}
                                  </Badge>
                                </div>
                              </div>
                              {config.scheduledFields.primary.notes && (
                                <p className="text-xs text-slate-600 mt-2">
                                  {config.scheduledFields.primary.notes}
                                </p>
                              )}
                            </div>
                          )}

                          {config.scheduledFields.estimated && (
                            <div className="bg-slate-50 border border-slate-200 rounded-lg p-3">
                              <p className="text-xs font-medium text-slate-700 mb-2">
                                Estimated/Fallback
                              </p>
                              <div className="grid grid-cols-3 gap-3">
                                <div>
                                  <p className="text-xs text-slate-600">Field ID</p>
                                  <p className="font-mono text-sm font-semibold">
                                    {config.scheduledFields.estimated.fieldId}
                                  </p>
                                </div>
                                <div>
                                  <p className="text-xs text-slate-600">Field Name</p>
                                  <p className="text-sm">
                                    {config.scheduledFields.estimated.fieldName}
                                  </p>
                                </div>
                                <div>
                                  <p className="text-xs text-slate-600">Usage</p>
                                  <Badge
                                    className={getUsageColor(
                                      config.scheduledFields.estimated.usage
                                    )}
                                    variant="secondary"
                                  >
                                    {config.scheduledFields.estimated.usage}
                                  </Badge>
                                </div>
                              </div>
                              {config.scheduledFields.estimated.notes && (
                                <p className="text-xs text-slate-600 mt-2">
                                  {config.scheduledFields.estimated.notes}
                                </p>
                              )}
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
                        <div className="bg-rose-50 border border-rose-200 rounded-lg p-4">
                          <div className="grid grid-cols-2 gap-4">
                            <div>
                              <p className="text-xs font-medium text-slate-600 mb-1">
                                Condition
                              </p>
                              <p className="text-sm">
                                {config.blockedState.condition}
                              </p>
                            </div>
                            <div>
                              <p className="text-xs font-medium text-slate-600 mb-1">
                                Threshold
                              </p>
                              <p className="text-sm">
                                {config.blockedState.threshold}{' '}
                                {config.blockedState.unit}
                              </p>
                            </div>
                          </div>
                          <div className="mt-3 pt-3 border-t border-rose-100">
                            <p className="text-xs font-medium text-slate-600 mb-1">
                              Message
                            </p>
                            <p className="text-sm text-rose-700">
                              {config.blockedState.message}
                            </p>
                          </div>
                          {config.blockedState.notes && (
                            <p className="text-xs text-slate-600 mt-2">
                              💡 {config.blockedState.notes}
                            </p>
                          )}
                        </div>
                      </div>
                    )}

                    {/* Overdue Settings */}
                    {config.overdue && (
                      <div>
                        <h4 className="text-sm font-semibold text-slate-900 mb-3 flex items-center gap-2">
                          <span className="text-lg">⏰</span>
                          Overdue Thresholds{' '}
                          <Badge variant="secondary" className="text-xs">
                            Currently Disabled
                          </Badge>
                        </h4>
                        <div className="bg-orange-50 border border-orange-200 rounded-lg p-4">
                          {config.overdue.projectAgeThreshold && (
                            <div className="mb-2">
                              <p className="text-xs font-medium text-slate-600">
                                Project Age Threshold
                              </p>
                              <p className="text-sm mt-1">
                                {config.overdue.projectAgeThreshold} days
                              </p>
                            </div>
                          )}
                          {config.overdue.threshold && (
                            <div className="mb-2">
                              <p className="text-xs font-medium text-slate-600">
                                Threshold
                              </p>
                              <p className="text-sm mt-1">
                                {config.overdue.threshold} {config.overdue.unit}
                              </p>
                            </div>
                          )}
                          {config.overdue.notes && (
                            <p className="text-xs text-slate-600 mt-2 pt-2 border-t border-orange-100">
                              {config.overdue.notes}
                            </p>
                          )}
                        </div>
                      </div>
                    )}

                    {/* SLA Tracking */}
                    {config.slaTracking && (
                      <div>
                        <h4 className="text-sm font-semibold text-slate-900 mb-3 flex items-center gap-2">
                          <span className="text-lg">⏱️</span>
                          SLA Tracking
                        </h4>
                        <div className="bg-indigo-50 border border-indigo-200 rounded-lg p-4">
                          {config.slaTracking.deadline && (
                            <div className="grid grid-cols-3 gap-4">
                              <div>
                                <p className="text-xs font-medium text-slate-600 mb-1">
                                  Field ID
                                </p>
                                <p className="font-mono text-sm font-semibold">
                                  {config.slaTracking.deadline.fieldId}
                                </p>
                              </div>
                              <div>
                                <p className="text-xs font-medium text-slate-600 mb-1">
                                  Urgent Threshold
                                </p>
                                <p className="text-sm">
                                  {config.slaTracking.deadline.urgentThreshold}{' '}
                                  {config.slaTracking.deadline.unit}
                                </p>
                              </div>
                              <div>
                                <p className="text-xs font-medium text-slate-600 mb-1">
                                  Usage
                                </p>
                                <Badge
                                  className={getUsageColor(
                                    config.slaTracking.deadline.usage
                                  )}
                                  variant="secondary"
                                >
                                  {config.slaTracking.deadline.usage}
                                </Badge>
                              </div>
                            </div>
                          )}
                        </div>
                      </div>
                    )}

                    {/* Helper Fields */}
                    {config.helperFields && config.helperFields.length > 0 && (
                      <div>
                        <h4 className="text-sm font-semibold text-slate-900 mb-3 flex items-center gap-2">
                          <span className="text-lg">📊</span>
                          Helper Fields (Additional Data)
                        </h4>
                        <div className="space-y-2">
                          {config.helperFields.map((field: any, idx: number) => (
                            <div
                              key={idx}
                              className="bg-slate-50 border border-slate-200 rounded-lg p-3"
                            >
                              <div className="grid grid-cols-4 gap-3">
                                <div>
                                  <p className="text-xs text-slate-600">Field ID</p>
                                  <p className="font-mono text-sm font-semibold">
                                    {field.fieldId}
                                  </p>
                                </div>
                                <div>
                                  <p className="text-xs text-slate-600">Field Name</p>
                                  <p className="text-sm">{field.fieldName}</p>
                                </div>
                                <div>
                                  <p className="text-xs text-slate-600">Type</p>
                                  <Badge variant="outline">{field.type}</Badge>
                                </div>
                                <div>
                                  <p className="text-xs text-slate-600">Usage</p>
                                  <Badge
                                    className={getUsageColor(field.usage)}
                                    variant="secondary"
                                  >
                                    {field.usage}
                                  </Badge>
                                </div>
                              </div>
                            </div>
                          ))}
                        </div>
                      </div>
                    )}

                    {/* Dependencies */}
                    {config.dependencies && config.dependencies.length > 0 && (
                      <div>
                        <h4 className="text-sm font-semibold text-slate-900 mb-3 flex items-center gap-2">
                          <span className="text-lg">🔗</span>
                          Dependencies (Must Complete First)
                        </h4>
                        <div className="flex gap-2 flex-wrap">
                          {config.dependencies.map((depId: string) => {
                            const depMilestone = milestonesConfig.milestones.find(
                              (m: any) => m.id === depId
                            )
                            return (
                              <Badge
                                key={depId}
                                variant="outline"
                                className="text-sm py-1.5 px-3"
                              >
                                {depMilestone?.icon} {depMilestone?.name || depId}
                              </Badge>
                            )
                          })}
                        </div>
                      </div>
                    )}

                    {/* UI Notes */}
                    {config.uiNotes && (
                      <div className="pt-4 border-t border-slate-200">
                        <h4 className="text-sm font-semibold text-slate-900 mb-2 flex items-center gap-2">
                          <span className="text-lg">💡</span>
                          UI Implementation Notes
                        </h4>
                        <p className="text-xs text-slate-600 bg-slate-50 rounded-lg p-3 border border-slate-200">
                          {config.uiNotes}
                        </p>
                      </div>
                    )}

                    {!isEditing && (
                      <div className="pt-4 border-t border-slate-200">
                        <p className="text-xs text-slate-500">
                          💡 Click &quot;Edit&quot; to customize field IDs, usage
                          percentages, and other configuration details
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
      {milestonesConfig.fundingMilestones && (
        <Card>
          <CardHeader>
            <div className="flex items-center gap-2">
              <div className="text-2xl">💰</div>
              <div>
                <CardTitle>Funding Milestones (M1/M2/M3)</CardTitle>
                <CardDescription>
                  {milestonesConfig.fundingMilestones.description}
                </CardDescription>
              </div>
            </div>
          </CardHeader>
          <CardContent>
            <div className="grid grid-cols-3 gap-4">
              {milestonesConfig.fundingMilestones?.dashboardFields &&
                Object.entries(milestonesConfig.fundingMilestones.dashboardFields).map(
                  ([key, field]: [string, any]) => (
                    <div
                      key={key}
                      className="bg-green-50 border border-green-200 rounded-lg p-4"
                    >
                      <p className="font-semibold text-slate-900 mb-2">
                        {field.fieldName}
                      </p>
                      <p className="text-xs text-slate-600 font-mono mb-2">
                        Field ID: {field.fieldId}
                      </p>
                      <Badge
                        className={`${getUsageColor(field.usage)}`}
                        variant="secondary"
                      >
                        {field.usage} usage
                      </Badge>
                    </div>
                  )
                )}
            </div>
          </CardContent>
        </Card>
      )}

      {/* Traffic Light Color Reference */}
      {milestonesConfig.trafficLightColors && (
        <Card>
          <CardHeader>
            <div className="flex items-center gap-2">
              <div className="text-2xl">🚦</div>
              <div>
                <CardTitle>Traffic Light Color System</CardTitle>
                <CardDescription>
                  State colors and their meanings
                </CardDescription>
              </div>
            </div>
          </CardHeader>
          <CardContent>
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
              {Object.entries(milestonesConfig.trafficLightColors).map(
                ([colorKey, colorInfo]: [string, any]) => (
                  <div
                    key={colorKey}
                    className="border rounded-lg p-4 flex items-start gap-3"
                  >
                    {getTrafficLightIcon(colorKey)}
                    <div className="flex-1">
                      <p className="font-semibold text-sm text-slate-900">
                        {colorInfo.name}
                      </p>
                      <p className="text-xs text-slate-600 mt-1">
                        {colorInfo.description}
                      </p>
                      <p className="text-xs text-slate-500 mt-2 bg-slate-50 rounded p-2">
                        {colorInfo.condition}
                      </p>
                    </div>
                  </div>
                )
              )}
            </div>
          </CardContent>
        </Card>
      )}

      {/* Configuration Notes */}
      {milestonesConfig.configurationNotes && (
        <Card className="border-indigo-200 bg-indigo-50">
          <CardHeader>
            <div className="flex items-center gap-2">
              <div className="text-2xl">📝</div>
              <div>
                <CardTitle>Configuration Notes</CardTitle>
                <CardDescription className="text-indigo-700">
                  Key highlights and implementation details
                </CardDescription>
              </div>
            </div>
          </CardHeader>
          <CardContent>
            <ul className="space-y-2">
              {milestonesConfig.configurationNotes.map((note: string, idx: number) => (
                <li key={idx} className="text-sm text-indigo-900 flex gap-2">
                  <span className="flex-shrink-0">•</span>
                  <span>{note}</span>
                </li>
              ))}
            </ul>
          </CardContent>
        </Card>
      )}
    </div>
  )
}
