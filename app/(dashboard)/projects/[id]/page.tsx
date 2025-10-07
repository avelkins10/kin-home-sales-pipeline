import { Suspense } from 'react'
import { getServerSession } from 'next-auth'
import { authOptions } from '@/lib/auth/next-auth.config'
import { notFound, redirect } from 'next/navigation'
import { getProjectById } from '@/lib/quickbase/queries'
import { ProjectHeader } from '@/components/projects/ProjectHeader'
import { CustomerContactCard } from '@/components/projects/CustomerContactCard'
import { SystemSpecsCard } from '@/components/projects/SystemSpecsCard'
import { TeamMembersCard } from '@/components/projects/TeamMembersCard'
import { AddersCard } from '@/components/projects/AddersCard'
import { Timeline } from '@/components/milestones/Timeline'
import { HoldManagementCard } from '@/components/projects/HoldManagementCard'

export default async function ProjectDetailPage({
  params,
}: {
  params: { id: string }
}) {
  // Check authentication
  const session = await getServerSession(authOptions)
  if (!session) {
    redirect('/login')
  }

  // Parse and validate project ID
  const projectId = parseInt(params.id)
  if (isNaN(projectId)) {
    notFound()
  }

  // Fetch project data server-side
  const project = await getProjectById(projectId)
  
  if (!project) {
    notFound()
  }

  return (
    <div className="min-h-screen bg-gray-50 p-6">
      <div className="max-w-7xl mx-auto space-y-6">
        {/* Project Header */}
        <ProjectHeader project={project} />

        {/* Main Content Grid */}
        <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
          {/* Left Column - 2/3 width */}
          <div className="lg:col-span-2 space-y-6">
            {/* Customer Contact */}
            <CustomerContactCard project={project} />

            {/* System Specifications */}
            <SystemSpecsCard project={project} />

            {/* Project Timeline */}
            <div className="bg-white rounded-lg shadow-sm border p-6">
              <h2 className="text-xl font-semibold text-gray-900 mb-4">
                Project Timeline
              </h2>
              <Suspense fallback={<div className="h-32 bg-gray-100 rounded animate-pulse" />}>
                <Timeline project={project} />
              </Suspense>
            </div>
          </div>

          {/* Right Column - 1/3 width */}
          <div className="space-y-6">
            {/* Team Members */}
            <TeamMembersCard project={project} />

            {/* Adders */}
            <AddersCard project={project} />

            {/* Hold Management */}
            <HoldManagementCard project={project} />
          </div>
        </div>
      </div>
    </div>
  )
}
