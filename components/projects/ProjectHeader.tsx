'use client'

import { useRouter } from 'next/navigation'
import { ArrowLeft, AlertTriangle } from 'lucide-react'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'
import { QuickbaseProject } from '@/lib/types/project'
import { getCurrentMilestone, getMilestoneStatus, isProjectOnHold, getHoldReason } from '@/lib/utils/milestone-engine'
import { cn } from '@/lib/utils/cn'
import { SalesSupportButton } from '@/components/support/SalesSupportButton'

interface ProjectHeaderProps {
  project: QuickbaseProject
}

export function ProjectHeader({ project }: ProjectHeaderProps) {
  const router = useRouter()

  // Extract project data
  const customerName = project[PROJECT_FIELDS.CUSTOMER_NAME]?.value || 'Unknown Customer'
  const recordId = project[PROJECT_FIELDS.RECORD_ID]?.value
  const projectId = project[PROJECT_FIELDS.PROJECT_ID]?.value || recordId || 'Unknown'
  const projectStatus = project[PROJECT_FIELDS.PROJECT_STATUS]?.value || 'Active'
  const priority = project[PROJECT_FIELDS.PROJECT_PRIORITY]?.value
  const onHold = isProjectOnHold(project)
  const holdReason = getHoldReason(project)

  // Get milestone and urgency info from engine
  const currentMilestoneId = getCurrentMilestone(project)
  const milestoneStatus = getMilestoneStatus(project, currentMilestoneId)
  const currentMilestone = milestoneStatus.name
  const urgency = milestoneStatus.urgency

  // Get color based on milestone state and urgency
  const getMilestoneColor = () => {
    if (milestoneStatus.state === 'complete') {
      return 'border-green-500 text-green-700 bg-green-50'
    }
    if (milestoneStatus.state === 'overdue') {
      return 'border-rose-600 text-rose-700 bg-rose-50'
    }
    if (milestoneStatus.state === 'blocked') {
      return 'border-red-500 text-red-700 bg-red-50'
    }
    if (urgency === 'critical' || urgency === 'urgent') {
      return 'border-orange-500 text-orange-700 bg-orange-50'
    }
    if (milestoneStatus.state === 'in-progress') {
      return 'border-amber-500 text-amber-700 bg-amber-50'
    }
    return 'border-slate-400 text-slate-700 bg-slate-50'
  }

  const milestoneColor = getMilestoneColor()

  const handleBackClick = () => {
    router.back()
  }

  const scrollToHoldManagement = () => {
    const holdCard = document.getElementById('hold-management')
    if (holdCard) {
      holdCard.scrollIntoView({ behavior: 'smooth' })
    }
  }

  return (
    <div className="space-y-4" data-testid="project-header">
      {/* Back Button and Support Button */}
      <div className="flex items-center justify-between">
        <Button
          variant="ghost"
          onClick={handleBackClick}
          className="flex items-center gap-2 text-gray-600 hover:text-gray-900"
        >
          <ArrowLeft className="w-4 h-4" />
          Back to Projects
        </Button>

        {recordId && (
          <SalesSupportButton
            projectContext={{
              projectId: String(projectId),
              customerName,
              recordId: Number(recordId),
            }}
            variant="outline"
            size="sm"
          />
        )}
      </div>

      {/* Header Row */}
      <div className="flex flex-col sm:flex-row sm:items-center sm:justify-between gap-4">
        {/* Left Side - Customer Info */}
        <div>
          <h1 className="text-3xl font-bold text-gray-900">{customerName}</h1>
          <p className="text-gray-600 text-sm">Project #{projectId}</p>
        </div>

        {/* Right Side - Status Badges */}
        <div className="flex flex-wrap gap-2">
          {/* Project Status */}
          <Badge
            variant={onHold ? 'destructive' : projectStatus === 'Completed' ? 'secondary' : 'default'}
          >
            {onHold ? 'On Hold' : projectStatus}
          </Badge>

          {/* Priority Badge */}
          {priority && (priority === 'Insane' || priority === 'Urgent') && (
            <Badge variant={priority === 'Insane' ? 'destructive' : 'secondary'}>
              {priority}
            </Badge>
          )}

          {/* Current Milestone */}
          <Badge
            variant="outline"
            className={cn(milestoneColor)}
          >
            {currentMilestone}
          </Badge>
        </div>
      </div>

      {/* Hold Alert */}
      {onHold && (
        <div className="bg-red-50 border border-red-200 rounded-lg p-4">
          <div className="flex items-start gap-3">
            <AlertTriangle className="w-5 h-5 text-red-600 mt-0.5 flex-shrink-0" />
            <div className="flex-1">
              <h3 className="text-sm font-medium text-red-800">Project on Hold</h3>
              <p className="text-sm text-red-700 mt-1">
                {holdReason || 'No reason provided'}
              </p>
              <Button
                variant="link"
                size="sm"
                onClick={scrollToHoldManagement}
                className="text-red-600 hover:text-red-800 p-0 h-auto mt-2"
              >
                Manage Hold →
              </Button>
            </div>
          </div>
        </div>
      )}
    </div>
  )
}
