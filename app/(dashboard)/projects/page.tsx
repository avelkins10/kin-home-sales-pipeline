'use client'

import { useSession } from 'next-auth/react'
import { redirect } from 'next/navigation'
import { Suspense } from 'react'
import { ProjectTableView } from '@/components/projects/ProjectTableView'
import { ProjectFilterChips } from '@/components/projects/ProjectFilterChips'
import { SearchBar } from '@/components/projects/SearchBar'

interface ProjectsPageProps {
  searchParams: {
    search?: string
    view?: string
  }
}

export default function ProjectsPage({ searchParams }: ProjectsPageProps) {
  const { data: session, status } = useSession()

  if (status === 'loading') {
    return <div>Loading...</div>
  }

  if (!session) {
    redirect('/login')
  }

  return (
    <div className="min-h-screen bg-slate-50">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Page Header */}
        <div className="mb-8">
          <h1 className="text-2xl font-bold text-slate-900">My Projects</h1>
          <p className="mt-1.5 text-sm text-slate-600">Manage and track all your solar installation projects</p>
        </div>

        {/* Controls Section */}
        <div className="mb-6 space-y-4">
          {/* Search Bar */}
          <SearchBar defaultValue={searchParams.search} />

          {/* Filter Chips */}
          <ProjectFilterChips />
        </div>

        {/* Project Table */}
        <Suspense fallback={<div>Loading projects...</div>}>
          <ProjectTableView
            userId={session.user.quickbaseUserId}
            role={session.user.role}
            view={searchParams.view || 'all'}
            search={searchParams.search || ''}
          />
        </Suspense>
      </div>
    </div>
  )
}
