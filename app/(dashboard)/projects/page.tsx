'use client'

import { useSession } from 'next-auth/react'
import { redirect } from 'next/navigation'
import { Suspense, useState } from 'react'
import { ProjectTableView } from '@/components/projects/ProjectTableView'
import { ProjectFilterChips } from '@/components/projects/ProjectFilterChips'
import { SearchBar } from '@/components/projects/SearchBar'
import { SortDropdown } from '@/components/projects/SortDropdown'
import { OwnershipFilterToggle } from '@/components/projects/OwnershipFilterToggle'
import { OfficeFilterDropdown } from '@/components/projects/OfficeFilterDropdown'
import { isManagerRole } from '@/lib/utils/role-helpers'

interface ProjectsPageProps {
  searchParams: {
    search?: string
    view?: string
    sort?: string
    memberEmail?: string
    ownership?: string // NEW: Ownership filter (all | my-projects | team-projects)
    office?: string // NEW: Office filter
  }
}

export default function ProjectsPage({ searchParams }: ProjectsPageProps) {
  const { data: session, status } = useSession()
  const [isFetching, setIsFetching] = useState(false)
  const [fetchReason, setFetchReason] = useState<'manual' | 'background'>('background')

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
          {/* Search Bar, Office Filter, and Sort */}
          <div className="flex flex-col sm:flex-row gap-3">
            <div className="flex-1">
              <SearchBar defaultValue={searchParams.search} isFetching={isFetching && fetchReason === 'manual'} />
            </div>
            <OfficeFilterDropdown isFetching={isFetching && fetchReason === 'manual'} />
            <SortDropdown isFetching={isFetching && fetchReason === 'manual'} />
          </div>

          {/* Ownership Filter Toggle (Team Projects option only for managers) */}
          <OwnershipFilterToggle
            currentOwnership={searchParams.ownership || 'all'}
            userRole={session.user.role}
          />

          {/* Filter Chips */}
          <ProjectFilterChips isFetching={isFetching && fetchReason === 'manual'} />
        </div>

        {/* Project Table */}
        <Suspense fallback={<div>Loading projects...</div>}>
          <ProjectTableView
            userId={session.user.quickbaseUserId}
            role={session.user.role}
            userEmail={session.user.email} // NEW: Pass user email for ownership indicators
            view={searchParams.view || 'all'}
            search={searchParams.search || ''}
            sort={searchParams.sort || 'default'}
            memberEmail={searchParams.memberEmail}
            ownership={searchParams.ownership || 'all'} // NEW: Pass ownership filter
            office={searchParams.office} // NEW: Pass office filter
            onFetchingChange={(fetching, reason) => {
              setIsFetching(fetching);
              if (reason) setFetchReason(reason);
            }}
          />
        </Suspense>
      </div>
    </div>
  )
}
