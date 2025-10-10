// app/api/admin/users/lookup/route.ts
export const runtime = 'nodejs'
import { NextRequest, NextResponse } from 'next/server'
import { requireRole } from '@/lib/auth/guards'
import { logInfo, logError } from '@/lib/logging/logger'
import { z } from 'zod'
import { quickbaseLookupSchema } from '@/lib/validation/admin'
import { 
  searchQuickbaseUsers, 
  getUserByQuickbaseId, 
  getUserProjectActivity,
  getActiveUsersFromQuickbase 
} from '@/lib/quickbase/userQueries'

/**
 * GET /api/admin/users/lookup?search=xxx&activeOnly=true&monthsBack=6
 * Search QuickBase users (manual user creation with auto-fill)
 */
export async function GET(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin', 'office_leader', 'area_director', 'divisional', 'regional'])
    if (!auth.authorized) {
      return auth.response
    }

    const { searchParams } = new URL(request.url)
    const search = searchParams.get('search')
    const activeOnly = searchParams.get('activeOnly') === 'true'
    const monthsBack = parseInt(searchParams.get('monthsBack') || '6')
    const quickbaseId = searchParams.get('quickbaseId')

    // If quickbaseId is provided, get specific user details
    if (quickbaseId) {
      const userData = await getUserByQuickbaseId(quickbaseId)
      if (!userData) {
        return NextResponse.json(
          { error: 'User not found in QuickBase' },
          { status: 404 }
        )
      }

      // Get activity statistics
      const activity = await getUserProjectActivity(quickbaseId)

      return NextResponse.json({
        user: userData,
        activity: {
          totalProjects: activity.totalProjects,
          activeProjects: activity.activeProjects,
          lastProjectDate: activity.lastProjectDate,
          offices: activity.offices
        }
      })
    }

    // Validate search parameter
    if (!search) {
      return NextResponse.json(
        { error: 'Search term is required' },
        { status: 400 }
      )
    }

    const validatedSearch = quickbaseLookupSchema.parse({
      searchTerm: search,
      searchType: 'name' // Default to name search
    })

    // Search QuickBase users with optional activity filter
    // Pass monthsBack directly to search function for efficient filtering
    const users = await searchQuickbaseUsers(
      validatedSearch.searchTerm,
      activeOnly ? monthsBack : undefined
    )

    logInfo('QuickBase user lookup', { 
      searchTerm: search,
      activeOnly,
      monthsBack,
      resultCount: users.length
    })

    return NextResponse.json({
      users,
      total: users.length,
      activeOnly,
      monthsBack
    })
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        { error: 'Validation failed', message: error.errors[0].message },
        { status: 400 }
      )
    }

    logError('Failed to lookup QuickBase users', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to lookup QuickBase users' },
      { status: 500 }
    )
  }
}

/**
 * GET /api/admin/users/lookup/active?monthsBack=6&role=closer
 * Get active users from QuickBase (for smart sync preview)
 */
export async function POST(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) {
      return auth.response
    }

    const body = await request.json()
    const { monthsBack = 6, role } = body

    if (monthsBack < 1 || monthsBack > 24) {
      return NextResponse.json(
        { error: 'monthsBack must be between 1 and 24' },
        { status: 400 }
      )
    }

    // Get active users from QuickBase
    const activeUsers = await getActiveUsersFromQuickbase(monthsBack)

    // Filter by role if specified
    let filteredUsers = activeUsers
    if (role && role !== 'both') {
      filteredUsers = activeUsers.filter(user => user.role === role)
    }

    // Calculate statistics
    const stats = {
      total: filteredUsers.length,
      closers: filteredUsers.filter(u => u.role === 'closer').length,
      setters: filteredUsers.filter(u => u.role === 'setter').length,
      withEmail: filteredUsers.filter(u => u.email).length,
      withPhone: filteredUsers.filter(u => u.phone).length,
      uniqueOffices: Array.from(new Set(filteredUsers.map(u => u.office).filter(Boolean))).length
    }

    logInfo('Active users lookup', { 
      monthsBack,
      role,
      stats
    })

    return NextResponse.json({
      users: filteredUsers,
      stats,
      monthsBack,
      role
    })
  } catch (error) {
    logError('Failed to get active users', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to get active users' },
      { status: 500 }
    )
  }
}


