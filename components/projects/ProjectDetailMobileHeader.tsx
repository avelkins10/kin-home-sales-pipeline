'use client'

import { useRouter } from 'next/navigation'
import { ArrowLeft, Phone, MessageSquare, Mail } from 'lucide-react'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'
import { QuickbaseProject } from '@/lib/types/project'
import { getCurrentMilestone, getMilestoneStatus, isProjectOnHold } from '@/lib/utils/milestone-engine'
import { useMilestoneConfig } from '@/lib/hooks/useMilestoneConfig'
import { cn } from '@/lib/utils/cn'
import { useIsMobile } from '@/lib/hooks/useMediaQuery'
import { useState, useEffect } from 'react'

interface ProjectDetailMobileHeaderProps {
  project: QuickbaseProject
}

export function ProjectDetailMobileHeader({ project }: ProjectDetailMobileHeaderProps) {
  const router = useRouter()
  const isMobile = useIsMobile()
  const [isScrolled, setIsScrolled] = useState(false)

  // Fetch dynamic milestone configuration
  const { config } = useMilestoneConfig()

  // Extract project data
  const customerName = project[PROJECT_FIELDS.CUSTOMER_NAME]?.value || 'Unknown Customer'
  const projectStatus = project[PROJECT_FIELDS.PROJECT_STATUS]?.value || 'Active'
  const onHold = isProjectOnHold(project)

  // Get milestone info
  const currentMilestoneId = getCurrentMilestone(project, config)
  const milestoneStatus = getMilestoneStatus(project, currentMilestoneId, config)
  const currentMilestone = milestoneStatus.name
  const urgency = milestoneStatus.urgency

  // Extract customer contact info (same logic as CustomerContactCard)
  const phoneRaw = project[PROJECT_FIELDS.CUSTOMER_PHONE]?.value || ''
  const emailRaw = project[PROJECT_FIELDS.CUSTOMER_CITY]?.value || '' // Field 149 has email

  // Clean phone
  let phone = ''
  if (typeof phoneRaw === 'string') {
    phone = phoneRaw.trim()
  } else if (phoneRaw && typeof phoneRaw === 'object' && 'value' in phoneRaw) {
    phone = String(phoneRaw.value).trim()
  }

  // Clean email
  let email = ''
  if (typeof emailRaw === 'string') {
    const emailMatch = emailRaw.match(/[\w.-]+@[\w.-]+\.\w+/)
    email = emailMatch ? emailMatch[0] : emailRaw.trim()
  } else if (emailRaw && typeof emailRaw === 'object' && 'value' in emailRaw) {
    email = String(emailRaw.value).trim()
  }

  // Get milestone color
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

  // Detect scroll for blur effect
  useEffect(() => {
    const handleScroll = () => {
      setIsScrolled(window.scrollY > 10)
    }

    window.addEventListener('scroll', handleScroll)
    return () => window.removeEventListener('scroll', handleScroll)
  }, [])

  const handleBackClick = () => {
    router.back()
  }

  // Don't render on desktop
  if (!isMobile) {
    return null
  }

  return (
    <div
      className={cn(
        'sticky top-0 z-50 bg-white border-b transition-all duration-200',
        isScrolled ? 'border-gray-200 shadow-md backdrop-blur-lg bg-white/95' : 'border-transparent'
      )}
    >
      {/* Top Row - Back button and contact actions */}
      <div className="flex items-center justify-between px-3 py-2">
        {/* Back Button */}
        <Button
          variant="ghost"
          size="sm"
          onClick={handleBackClick}
          className="h-8 px-2 text-gray-600 hover:text-gray-900"
        >
          <ArrowLeft className="w-4 h-4 mr-1" />
          <span className="text-sm">Back</span>
        </Button>

        {/* Contact Action Buttons */}
        <div className="flex gap-1">
          <Button
            asChild
            variant="ghost"
            size="sm"
            disabled={!phone}
            className="h-8 w-8 p-0"
            title={phone ? 'Call customer' : 'Phone number not available'}
          >
            <a href={phone ? `tel:${phone}` : '#'}>
              <Phone className="w-4 h-4" />
            </a>
          </Button>
          <Button
            asChild
            variant="ghost"
            size="sm"
            disabled={!phone}
            className="h-8 w-8 p-0"
            title={phone ? 'Text customer' : 'Phone number not available'}
          >
            <a href={phone ? `sms:${phone}` : '#'}>
              <MessageSquare className="w-4 h-4" />
            </a>
          </Button>
          <Button
            asChild
            variant="ghost"
            size="sm"
            disabled={!email}
            className="h-8 w-8 p-0"
            title={email ? 'Email customer' : 'Email not available'}
          >
            <a href={email ? `mailto:${email}` : '#'}>
              <Mail className="w-4 h-4" />
            </a>
          </Button>
        </div>
      </div>

      {/* Bottom Row - Customer name and badges */}
      <div className="px-3 pb-2">
        <h1 className="text-lg font-bold text-gray-900 truncate mb-2">{customerName}</h1>
        <div className="flex flex-wrap gap-1.5">
          {/* Status Badge */}
          <Badge
            variant={onHold ? 'destructive' : projectStatus === 'Completed' ? 'secondary' : 'default'}
            className="text-xs"
          >
            {onHold ? 'On Hold' : projectStatus}
          </Badge>

          {/* Current Milestone */}
          <Badge
            variant="outline"
            className={cn(milestoneColor, 'text-xs')}
          >
            {currentMilestone}
          </Badge>
        </div>
      </div>
    </div>
  )
}
