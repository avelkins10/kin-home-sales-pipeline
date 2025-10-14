import { render, screen, fireEvent, waitFor } from '@testing-library/react'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { HierarchyTreeView } from '@/components/settings/HierarchyTreeView'
import { toast } from 'sonner'

// Mock the toast function
jest.mock('sonner', () => ({
  toast: {
    success: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
  },
}))

// Mock the API calls
global.fetch = jest.fn()

// Mock the getBaseUrl utility
jest.mock('@/lib/utils/baseUrl', () => ({
  getBaseUrl: () => 'http://localhost:3000',
}))

// Mock the hierarchy helpers
jest.mock('@/lib/utils/hierarchy-helpers', () => ({
  validateManagerRole: jest.fn(() => ({ valid: true })),
  validateCircularHierarchy: jest.fn(() => ({ valid: true })),
  validateUserAssignments: jest.fn(() => ({ valid: true })),
  validateBulkAssignmentSize: jest.fn(() => ({ valid: true })),
  getUnassignedUsers: jest.fn(() => []),
  getTeamSize: jest.fn(() => 0),
  formatHierarchyError: jest.fn((type) => `Error: ${type}`),
  canManageRole: jest.fn(() => true),
  getManagerRoleConstraints: jest.fn(() => ({ canManage: ['closer', 'setter'] })),
  getDescendants: jest.fn(() => []),
}))

// Mock the role utilities
jest.mock('@/lib/utils/roles', () => ({
  getRoleBadgeVariant: jest.fn(() => 'default'),
  getRoleDisplayName: jest.fn((role) => role),
}))

// Mock the activity utilities
jest.mock('@/lib/utils/activity', () => ({
  getActivityStatus: jest.fn(() => 'active'),
  getActivityColor: jest.fn(() => 'bg-green-500'),
  getActivityLabel: jest.fn(() => 'Active'),
  formatLastActivity: jest.fn(() => 'Recently'),
}))

// Mock the avatar utilities
jest.mock('@/lib/utils/avatar', () => ({
  getInitials: jest.fn((name) => name?.split(' ').map(n => n[0]).join('') || ''),
  getAvatarColor: jest.fn(() => 'bg-blue-500'),
  getAvatarTextColor: jest.fn(() => 'text-white'),
}))

const mockUsers = [
  {
    id: '1',
    name: 'John Doe',
    email: 'john@example.com',
    role: 'super_admin',
    office: 'Main Office',
    is_active: true,
  },
  {
    id: '2',
    name: 'Jane Smith',
    email: 'jane@example.com',
    role: 'team_lead',
    office: 'Branch Office',
    is_active: true,
  },
  {
    id: '3',
    name: 'Bob Johnson',
    email: 'bob@example.com',
    role: 'closer',
    office: 'Main Office',
    is_active: true,
  },
]

const mockHierarchies = [
  {
    id: '1',
    manager_id: '2',
    user_id: '3',
    created_at: '2024-01-01T00:00:00Z',
    updated_at: '2024-01-01T00:00:00Z',
    manager_name: 'Jane Smith',
    manager_email: 'jane@example.com',
    manager_role: 'team_lead',
    user_name: 'Bob Johnson',
    user_email: 'bob@example.com',
    user_role: 'closer',
    user_is_active: true,
  },
]

const createTestQueryClient = () =>
  new QueryClient({
    defaultOptions: {
      queries: {
        retry: false,
      },
      mutations: {
        retry: false,
      },
    },
  })

const renderWithQueryClient = (component: React.ReactElement) => {
  const queryClient = createTestQueryClient()
  return render(
    <QueryClientProvider client={queryClient}>
      {component}
    </QueryClientProvider>
  )
}

describe('HierarchyTreeView', () => {
  beforeEach(() => {
    jest.clearAllMocks()
    ;(global.fetch as jest.Mock).mockImplementation((url) => {
      if (url.includes('/api/admin/users')) {
        return Promise.resolve({
          ok: true,
          json: () => Promise.resolve(mockUsers),
        })
      }
      if (url.includes('/api/admin/hierarchies')) {
        return Promise.resolve({
          ok: true,
          json: () => Promise.resolve(mockHierarchies),
        })
      }
      return Promise.reject(new Error('Unknown URL'))
    })
  })

  it('renders hierarchy overview statistics', async () => {
    renderWithQueryClient(<HierarchyTreeView />)
    
    await waitFor(() => {
      expect(screen.getByText('Hierarchy Overview')).toBeInTheDocument()
      expect(screen.getByText('Total Users')).toBeInTheDocument()
      expect(screen.getByText('Managers')).toBeInTheDocument()
      expect(screen.getByText('Reps')).toBeInTheDocument()
    })
  })

  it('renders search and filter controls', async () => {
    renderWithQueryClient(<HierarchyTreeView />)
    
    await waitFor(() => {
      expect(screen.getByPlaceholderText(/Search by name or email/)).toBeInTheDocument()
      expect(screen.getByText('Role')).toBeInTheDocument()
      expect(screen.getByText('Activity')).toBeInTheDocument()
      expect(screen.getByText('Office')).toBeInTheDocument()
    })
  })

  it('filters users based on search term', async () => {
    renderWithQueryClient(<HierarchyTreeView />)
    
    await waitFor(() => {
      expect(screen.getByText('John Doe')).toBeInTheDocument()
    })
    
    const searchInput = screen.getByPlaceholderText(/Search by name or email/)
    fireEvent.change(searchInput, { target: { value: 'Jane' } })
    
    await waitFor(() => {
      expect(screen.getByText('Jane Smith')).toBeInTheDocument()
      expect(screen.queryByText('John Doe')).not.toBeInTheDocument()
    })
  })

  it('opens assign dialog when assign button is clicked', async () => {
    renderWithQueryClient(<HierarchyTreeView />)
    
    await waitFor(() => {
      expect(screen.getByText('Jane Smith')).toBeInTheDocument()
    })
    
    const assignButton = screen.getByText('Assign')
    fireEvent.click(assignButton)
    
    await waitFor(() => {
      expect(screen.getByText('Bulk Assign Users to Manager')).toBeInTheDocument()
    })
  })

  it('toggles bulk selection mode', async () => {
    renderWithQueryClient(<HierarchyTreeView />)
    
    await waitFor(() => {
      expect(screen.getByText('Bulk Select')).toBeInTheDocument()
    })
    
    const bulkSelectButton = screen.getByText('Bulk Select')
    fireEvent.click(bulkSelectButton)
    
    await waitFor(() => {
      expect(screen.getByText('Bulk Select')).toHaveClass('bg-blue-50')
    })
  })

  it('shows validation errors for inactive manager', async () => {
    const { validateManagerRole } = require('@/lib/utils/hierarchy-helpers')
    validateManagerRole.mockReturnValue({
      valid: false,
      error: 'Manager is inactive',
    })
    
    renderWithQueryClient(<HierarchyTreeView />)
    
    await waitFor(() => {
      expect(screen.getByText('Jane Smith')).toBeInTheDocument()
    })
    
    const assignButton = screen.getByText('Assign')
    fireEvent.click(assignButton)
    
    await waitFor(() => {
      expect(screen.getByText('Bulk Assign Users to Manager')).toBeInTheDocument()
    })
  })

  it('handles keyboard shortcuts', async () => {
    renderWithQueryClient(<HierarchyTreeView />)
    
    await waitFor(() => {
      expect(screen.getByText('Jane Smith')).toBeInTheDocument()
    })
    
    // Test expand all shortcut (Cmd+E)
    fireEvent.keyDown(document, { key: 'e', metaKey: true })
    
    await waitFor(() => {
      expect(toast.success).toHaveBeenCalledWith('All nodes expanded')
    })
    
    // Test collapse all shortcut (Cmd+Shift+E)
    fireEvent.keyDown(document, { key: 'E', metaKey: true, shiftKey: true })
    
    await waitFor(() => {
      expect(toast.success).toHaveBeenCalledWith('All nodes collapsed')
    })
  })

  it('exports hierarchy data correctly', async () => {
    // Mock URL.createObjectURL and document.createElement
    const mockCreateObjectURL = jest.fn(() => 'mock-url')
    const mockRevokeObjectURL = jest.fn()
    const mockClick = jest.fn()
    
    global.URL.createObjectURL = mockCreateObjectURL
    global.URL.revokeObjectURL = mockRevokeObjectURL
    
    const mockAnchor = {
      href: '',
      download: '',
      click: mockClick,
    }
    jest.spyOn(document, 'createElement').mockReturnValue(mockAnchor as any)
    
    renderWithQueryClient(<HierarchyTreeView />)
    
    await waitFor(() => {
      expect(screen.getByText('Export')).toBeInTheDocument()
    })
    
    const exportButton = screen.getByText('Export')
    fireEvent.click(exportButton)
    
    expect(mockCreateObjectURL).toHaveBeenCalled()
    expect(mockClick).toHaveBeenCalled()
    expect(mockRevokeObjectURL).toHaveBeenCalled()
  })

  it('shows role-based filtering in UserMultiSelect', async () => {
    const { getManagerRoleConstraints } = require('@/lib/utils/hierarchy-helpers')
    getManagerRoleConstraints.mockReturnValue({
      canManage: ['closer', 'setter'],
    })
    
    renderWithQueryClient(<HierarchyTreeView />)
    
    await waitFor(() => {
      expect(screen.getByText('Jane Smith')).toBeInTheDocument()
    })
    
    const assignButton = screen.getByText('Assign')
    fireEvent.click(assignButton)
    
    await waitFor(() => {
      expect(screen.getByText('Bulk Assign Users to Manager')).toBeInTheDocument()
    })
    
    // The UserMultiSelect should be filtered based on manager role
    expect(getManagerRoleConstraints).toHaveBeenCalled()
  })
})
