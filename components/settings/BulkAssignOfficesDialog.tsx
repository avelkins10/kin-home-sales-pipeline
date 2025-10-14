'use client'

import React, { useState } from 'react'
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query'
import { useRouter } from 'next/navigation'
import { toast } from 'sonner'
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from '@/components/ui/dialog'
import { Button } from '@/components/ui/button'
import { Label } from '@/components/ui/label'
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select'
import { Badge } from '@/components/ui/badge'
import {
  Tooltip,
  TooltipContent,
  TooltipProvider,
  TooltipTrigger,
} from '@/components/ui/tooltip'
import { X, Users, Building2, HelpCircle, AlertCircle, AlertTriangle, CheckCircle } from 'lucide-react'
import { Alert, AlertDescription } from '@/components/ui/alert'
import { Checkbox } from '@/components/ui/checkbox'
import { OfficeMultiSelect } from './OfficeMultiSelect'
import { getBaseUrl } from '@/lib/utils/baseUrl'
import { cn } from '@/lib/utils/cn'

interface User {
  id: string
  name: string
  email: string
  role: string
}

interface Office {
  id: string
  name: string
  region: string
  managerCount: number
}

interface BulkAssignOfficesDialogProps {
  open: boolean
  onOpenChange: (open: boolean) => void
}

export default function BulkAssignOfficesDialog({
  open,
  onOpenChange,
}: BulkAssignOfficesDialogProps) {
  const router = useRouter()
  const queryClient = useQueryClient()
  
  const [selectedManagers, setSelectedManagers] = useState<string[]>([])
  const [selectedOffices, setSelectedOffices] = useState<string[]>([])
  const [accessLevel, setAccessLevel] = useState<'view' | 'manage' | 'admin'>(
    (typeof window !== 'undefined' ? localStorage.getItem('bulkAccessLevel') : null) as 'view' | 'manage' | 'admin' || 'manage'
  )
  const [existingAssignments, setExistingAssignments] = useState<Array<{managerId: string, officeName: string}>>([])
  const [acknowledgeDuplicates, setAcknowledgeDuplicates] = useState(false)
  const [validationErrors, setValidationErrors] = useState<string[]>([])
  const [validationWarnings, setValidationWarnings] = useState<string[]>([])
  const [errorDetails, setErrorDetails] = useState<{title: string, details: string[], suggestions: string[]} | null>(null)
  const [errorDialogOpen, setErrorDialogOpen] = useState(false)

  // Fetch managers with appropriate roles
  const { data: managers = [], isLoading: isLoadingManagers } = useQuery({
    queryKey: ['managers-for-bulk-assign'],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/admin/users?role=office_leader,area_director,divisional,regional,super_admin`)
      if (!response.ok) throw new Error('Failed to fetch managers')
      return response.json() as Promise<User[]>
    },
    enabled: open,
  })

  // Fetch offices
  const { data: offices = [], isLoading: isLoadingOffices } = useQuery({
    queryKey: ['offices'],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/admin/offices`)
      if (!response.ok) throw new Error('Failed to fetch offices')
      return response.json() as Promise<Office[]>
    },
    enabled: open,
  })

  // Fetch existing assignments for selected managers
  const { data: currentAssignments = [] } = useQuery({
    queryKey: ['office-assignments', selectedManagers],
    queryFn: async () => {
      if (selectedManagers.length === 0) return []
      const response = await fetch(`${getBaseUrl()}/api/admin/office-assignments?userIds=${selectedManagers.join(',')}`)
      if (!response.ok) throw new Error('Failed to fetch existing assignments')
      return response.json() as Promise<Array<{userId: string, officeName: string, accessLevel: string}>>
    },
    enabled: open && selectedManagers.length > 0,
  })

  // Bulk assign mutation
  const bulkAssignMutation = useMutation({
    mutationFn: async (data: {
      userIds: string[]
      officeNames: string[]
      accessLevel: 'view' | 'manage' | 'admin'
    }) => {
      const response = await fetch(`${getBaseUrl()}/api/admin/office-assignments/bulk`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(data),
      })

      if (!response.ok) {
        const error = await response.json()
        throw new Error(error.error || 'Failed to assign offices')
      }

      return response.json()
    },
    onSuccess: (data) => {
      toast.success('Office assignments created successfully!', {
        description: `Created ${data.total} assignments for ${selectedManagers.length} managers across ${selectedOffices.length} offices.`,
        action: {
          label: 'View Hierarchy',
          onClick: () => router.push('/settings?tab=hierarchy')
        }
      })
      queryClient.invalidateQueries({ queryKey: ['offices'] })
      queryClient.invalidateQueries({ queryKey: ['users'] })
      handleClose()
    },
    onError: (error: Error) => {
      // Parse API errors and show detailed error dialog
      const errorMessage = error.message.toLowerCase()
      let errorDetails: {title: string, details: string[], suggestions: string[]} = {
        title: 'Bulk Assignment Failed',
        details: [error.message],
        suggestions: ['Please review your selections and try again']
      }

      if (errorMessage.includes('invalid users')) {
        errorDetails = {
          title: 'Invalid Managers Selected',
          details: ['Some selected managers are invalid or no longer exist'],
          suggestions: ['Refresh the page and select valid managers', 'Check that managers have appropriate roles']
        }
      } else if (errorMessage.includes('invalid offices')) {
        errorDetails = {
          title: 'Invalid Offices Selected',
          details: ['Some selected offices do not exist in QuickBase'],
          suggestions: ['Verify office names match exactly with QuickBase', 'Check spelling and case sensitivity']
        }
      } else if (errorMessage.includes('permission')) {
        errorDetails = {
          title: 'Permission Denied',
          details: ['You do not have permission to assign these offices'],
          suggestions: ['Contact your system administrator', 'Check your role permissions']
        }
      }

      setErrorDetails(errorDetails)
      setErrorDialogOpen(true)
    },
  })

  const handleClose = () => {
    setSelectedManagers([])
    setSelectedOffices([])
    setAccessLevel('manage') // Reset to 'manage' as default
    setExistingAssignments([])
    setAcknowledgeDuplicates(false)
    setValidationErrors([])
    setValidationWarnings([])
    setErrorDetails(null)
    setErrorDialogOpen(false)
    onOpenChange(false)
  }

  // Validation logic
  const validateSelections = () => {
    const errors: string[] = []
    const warnings: string[] = []

    // Check for existing assignments
    const duplicates = currentAssignments.filter(assignment => 
      selectedManagers.includes(assignment.userId) && 
      selectedOffices.includes(assignment.officeName)
    )

    if (duplicates.length > 0 && !acknowledgeDuplicates) {
      warnings.push(`Some managers already have some of these offices assigned. This will update their access level to ${accessLevel}.`)
    }

    // Validate office names against known offices
    const validOfficeNames = offices.map(o => o.name)
    const invalidOffices = selectedOffices.filter(office => !validOfficeNames.includes(office))
    
    if (invalidOffices.length > 0) {
      errors.push(`Office names may not exist in QuickBase: ${invalidOffices.join(', ')}. Verify spelling and case.`)
    }

    setValidationErrors(errors)
    setValidationWarnings(warnings)
  }

  // Run validation when selections change
  React.useEffect(() => {
    if (selectedManagers.length > 0 || selectedOffices.length > 0) {
      validateSelections()
    } else {
      setValidationErrors([])
      setValidationWarnings([])
    }
  }, [selectedManagers, selectedOffices, currentAssignments, acknowledgeDuplicates, accessLevel, offices])

  const handleSubmit = () => {
    if (selectedManagers.length === 0 || selectedOffices.length === 0) {
      toast.error('Please select at least one manager and one office')
      return
    }

    if (validationErrors.length > 0) {
      toast.error('Please fix validation errors before submitting')
      return
    }

    const duplicates = currentAssignments.filter(assignment => 
      selectedManagers.includes(assignment.userId) && 
      selectedOffices.includes(assignment.officeName)
    )

    if (duplicates.length > 0 && !acknowledgeDuplicates) {
      toast.error('Please acknowledge duplicate assignments')
      return
    }

    bulkAssignMutation.mutate({
      userIds: selectedManagers,
      officeNames: selectedOffices,
      accessLevel,
    })
  }

  const handleManagerToggle = (userId: string) => {
    setSelectedManagers(prev => 
      prev.includes(userId) 
        ? prev.filter(id => id !== userId)
        : [...prev, userId]
    )
  }

  const handleRemoveManager = (userId: string) => {
    setSelectedManagers(prev => prev.filter(id => id !== userId))
  }

  const handleClearAllManagers = () => {
    setSelectedManagers([])
  }

  const isValid = selectedManagers.length > 0 && selectedOffices.length > 0 && validationErrors.length === 0

  return (
    <Dialog open={open} onOpenChange={bulkAssignMutation.isPending ? undefined : onOpenChange}>
      <DialogContent className="max-w-2xl" aria-busy={bulkAssignMutation.isPending}>
        <DialogHeader>
          <DialogTitle>Bulk Assign Offices</DialogTitle>
          <DialogDescription>
            Assign multiple offices to multiple managers with the same access level.
            <br />
            <span className="text-sm text-gray-600">
              Example: Assign Phoenix, Tucson, and Las Vegas offices to 2 area directors with &apos;Manage&apos; access.
              This will create 6 total assignments (2 managers × 3 offices).
            </span>
          </DialogDescription>
        </DialogHeader>

        <div className="space-y-6">
          {/* Managers Selection */}
          <div className="space-y-3">
            <div className="flex items-center gap-2">
              <Label className="flex items-center gap-2">
                <Users className="h-4 w-4" />
                Select Managers
              </Label>
              <TooltipProvider>
                <Tooltip>
                  <TooltipTrigger asChild>
                    <HelpCircle className="h-4 w-4 text-gray-400 cursor-help" aria-label="Managers field help" />
                  </TooltipTrigger>
                  <TooltipContent>
                    <p>Select one or more managers to assign offices to. Only users with manager roles (office_leader, area_director, divisional, regional, super_admin) are shown. Managers can be assigned to multiple offices.</p>
                  </TooltipContent>
                </Tooltip>
              </TooltipProvider>
            </div>
            <p className="text-sm text-gray-600">
              Tip: Use the search feature (if implemented) to quickly find managers in large organizations.
            </p>
            
            <div className="space-y-2">
              <div className="border rounded-md p-3 max-h-48 overflow-y-auto">
                {isLoadingManagers ? (
                  <div className="text-center text-sm text-gray-500 py-4">
                    Loading managers...
                  </div>
                ) : managers.length === 0 ? (
                  <div className="text-center text-sm text-gray-500 py-4">
                    No managers found
                  </div>
                ) : (
                  <div className="space-y-2">
                    {managers.map((manager) => {
                      const isSelected = selectedManagers.includes(manager.id)
                      return (
                        <button
                          key={manager.id}
                          onClick={() => handleManagerToggle(manager.id)}
                          className={cn(
                            'flex w-full items-center justify-between rounded-sm px-2 py-2 text-sm hover:bg-gray-100',
                            isSelected && 'bg-gray-50'
                          )}
                        >
                          <div className="flex items-center gap-2">
                            <div
                              className={cn(
                                'flex h-4 w-4 items-center justify-center rounded border',
                                isSelected
                                  ? 'border-primary bg-primary text-white'
                                  : 'border-gray-300'
                              )}
                            >
                              {isSelected && <X className="h-3 w-3" />}
                            </div>
                            <div className="text-left">
                              <div className="font-medium">{manager.name}</div>
                              <div className="text-xs text-gray-500">{manager.email}</div>
                            </div>
                          </div>
                          <Badge variant="outline" className="text-xs">
                            {manager.role.replace('_', ' ')}
                          </Badge>
                        </button>
                      )
                    })}
                  </div>
                )}
              </div>

              {selectedManagers.length > 0 && (
                <div className="flex items-center justify-between">
                  <span className="text-sm text-gray-600">
                    {selectedManagers.length} manager{selectedManagers.length !== 1 ? 's' : ''} selected
                  </span>
                  <Button
                    variant="ghost"
                    size="sm"
                    onClick={handleClearAllManagers}
                  >
                    Clear All
                  </Button>
                </div>
              )}

              {/* Selected managers badges */}
              {selectedManagers.length > 0 && (
                <div className="flex flex-wrap gap-1">
                  {selectedManagers.map((userId) => {
                    const manager = managers.find(m => m.id === userId)
                    return (
                      <Badge key={userId} variant="secondary" className="gap-1">
                        {manager?.name}
                        <button
                          onClick={() => handleRemoveManager(userId)}
                          className="ml-1 rounded-full hover:bg-gray-300"
                          aria-label={`Remove ${manager?.name} from selection`}
                        >
                          <X className="h-3 w-3" />
                        </button>
                      </Badge>
                    )
                  })}
                </div>
              )}
            </div>
          </div>

          {/* Offices Selection */}
          <div className="space-y-3">
            <div className="flex items-center gap-2">
              <Label className="flex items-center gap-2">
                <Building2 className="h-4 w-4" />
                Select Offices
              </Label>
              <TooltipProvider>
                <Tooltip>
                  <TooltipTrigger asChild>
                    <HelpCircle className="h-4 w-4 text-gray-400 cursor-help" aria-label="Offices field help" />
                  </TooltipTrigger>
                  <TooltipContent>
                    <p>Select one or more offices to assign. Each selected manager will be assigned to all selected offices. Office names must match exactly with QuickBase SALES_OFFICE field (case-sensitive).</p>
                  </TooltipContent>
                </Tooltip>
              </TooltipProvider>
            </div>
            <p className="text-sm text-gray-600">
              Note: Office assignments determine which projects managers can see. Managers see ALL projects in assigned offices, regardless of who the closer/setter is.
            </p>
            <OfficeMultiSelect
              value={selectedOffices}
              onChange={setSelectedOffices}
              placeholder="Select offices to assign..."
            />
          </div>

          {/* Access Level */}
          <div className="space-y-3">
            <div className="flex items-center gap-2">
              <Label>Access Level</Label>
              <TooltipProvider>
                <Tooltip>
                  <TooltipTrigger asChild>
                    <HelpCircle className="h-4 w-4 text-gray-400 cursor-help" aria-label="Access level help" />
                  </TooltipTrigger>
                  <TooltipContent>
                    <p>Access level determines what managers can do with office data. This does NOT affect project visibility - all managers see all projects in assigned offices regardless of access level.</p>
                  </TooltipContent>
                </Tooltip>
              </TooltipProvider>
            </div>
            <Select value={accessLevel} onValueChange={(value: 'view' | 'manage' | 'admin') => {
              setAccessLevel(value)
              if (typeof window !== 'undefined') {
                localStorage.setItem('bulkAccessLevel', value)
              }
            }}>
              <SelectTrigger>
                <SelectValue />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="view">View - Read-only access (rarely used). Manager can see projects but cannot make changes.</SelectItem>
                <SelectItem value="manage">Manage - Standard access (recommended). Manager can see all projects and manage office operations.</SelectItem>
                <SelectItem value="admin">Admin - Full access (use for area directors and above). Manager can manage users and office settings.</SelectItem>
              </SelectContent>
            </Select>
            <p className="text-sm text-gray-600">
              Recommended: Use &apos;Manage&apos; for office leaders, &apos;Admin&apos; for area directors and above.
            </p>
          </div>

          {/* Preview */}
          {isValid && (
            <div className="bg-gray-50 rounded-md p-4">
              <div className="flex items-center gap-2 mb-2">
                <h4 className="font-medium text-sm">Assignment Preview</h4>
                <TooltipProvider>
                  <Tooltip>
                    <TooltipTrigger asChild>
                      <HelpCircle className="h-3 w-3 text-gray-400 cursor-help" />
                    </TooltipTrigger>
                    <TooltipContent>
                      <p>This preview shows exactly what will be created in the database. Review carefully before submitting.</p>
                    </TooltipContent>
                  </Tooltip>
                </TooltipProvider>
              </div>
              <p className="text-sm text-gray-600">
                You are about to assign <strong>{selectedOffices.length}</strong> office{selectedOffices.length !== 1 ? 's' : ''} to{' '}
                <strong>{selectedManagers.length}</strong> manager{selectedManagers.length !== 1 ? 's' : ''} with{' '}
                <strong>{accessLevel}</strong> access level.
              </p>
              <p className="text-xs text-gray-500 mt-1">
                This will create {selectedManagers.length * selectedOffices.length} total assignments.
                {selectedManagers.length * selectedOffices.length > 20 && (
                  <span className="text-orange-600 font-medium"> This is a large operation and may take a few seconds.</span>
                )}
              </p>
              <p className="text-xs text-gray-500 mt-1">
                Example: {selectedManagers.length} managers × {selectedOffices.length} offices = {selectedManagers.length * selectedOffices.length} assignments
              </p>
            </div>
          )}

          {/* Validation Messages */}
          {validationErrors.length > 0 && (
            <Alert variant="destructive">
              <AlertCircle className="h-4 w-4" />
              <AlertDescription>
                <ul className="list-disc list-inside">
                  {validationErrors.map((error, i) => <li key={i}>{error}</li>)}
                </ul>
              </AlertDescription>
            </Alert>
          )}

          {validationWarnings.length > 0 && (
            <Alert variant="default" className="border-yellow-200 bg-yellow-50">
              <AlertTriangle className="h-4 w-4" />
              <AlertDescription>
                <div className="space-y-2">
                  <ul className="list-disc list-inside">
                    {validationWarnings.map((warning, i) => <li key={i}>{warning}</li>)}
                  </ul>
                  <div className="flex items-center space-x-2">
                    <Checkbox
                      id="acknowledge-duplicates"
                      checked={acknowledgeDuplicates}
                      onCheckedChange={(checked) => setAcknowledgeDuplicates(checked === true)}
                    />
                    <label htmlFor="acknowledge-duplicates" className="text-sm">
                      I acknowledge that this will update existing assignments
                    </label>
                  </div>
                </div>
              </AlertDescription>
            </Alert>
          )}

          {/* Progress indicator during submission */}
          {bulkAssignMutation.isPending && (
            <div className="bg-blue-50 border border-blue-200 rounded-md p-3">
              <div className="flex items-center gap-2">
                <div className="animate-spin rounded-full h-4 w-4 border-b-2 border-blue-600"></div>
                <span className="text-sm text-blue-800">
                  Creating {selectedManagers.length * selectedOffices.length} assignments... This may take a few seconds for large batches.
                </span>
              </div>
            </div>
          )}
        </div>

        <DialogFooter>
          <div className="flex items-center justify-between w-full">
            <a 
              href="https://github.com/your-org/rep-dashboard/blob/main/docs/MANAGER-SETUP-GUIDE.md" 
              target="_blank" 
              rel="noopener noreferrer"
              className="text-sm text-gray-600 hover:text-gray-900 underline"
              aria-label="Open Manager Setup Guide"
            >
              Need help? See the Manager Setup Guide
            </a>
            <div className="flex gap-2">
              <Button 
                variant="outline" 
                onClick={handleClose}
                disabled={bulkAssignMutation.isPending}
              >
                Cancel
              </Button>
              <Button
                onClick={handleSubmit}
                disabled={!isValid || bulkAssignMutation.isPending}
                className="min-w-[120px]"
              >
                {bulkAssignMutation.isPending ? (
                  <>
                    <div className="animate-spin rounded-full h-4 w-4 border-b-2 border-white mr-2"></div>
                    Assigning...
                  </>
                ) : (
                  'Assign Offices'
                )}
              </Button>
            </div>
          </div>
        </DialogFooter>
      </DialogContent>

      {/* Error Details Dialog */}
      <Dialog open={errorDialogOpen} onOpenChange={setErrorDialogOpen}>
        <DialogContent className="max-w-lg">
          <DialogHeader>
            <DialogTitle className="flex items-center gap-2">
              <AlertCircle className="h-5 w-5 text-red-500" />
              {errorDetails?.title}
            </DialogTitle>
            <DialogDescription>
              The bulk assignment operation failed. Please review the details below.
            </DialogDescription>
          </DialogHeader>
          
          {errorDetails && (
            <div className="space-y-4">
              <div>
                <h4 className="font-medium mb-2">Error Details:</h4>
                <ul className="list-disc list-inside text-sm text-gray-600 space-y-1">
                  {errorDetails.details.map((detail, i) => (
                    <li key={i}>{detail}</li>
                  ))}
                </ul>
              </div>
              
              <div>
                <h4 className="font-medium mb-2">Suggested Actions:</h4>
                <ul className="list-disc list-inside text-sm text-gray-600 space-y-1">
                  {errorDetails.suggestions.map((suggestion, i) => (
                    <li key={i}>{suggestion}</li>
                  ))}
                </ul>
              </div>
              
              <div className="bg-blue-50 p-3 rounded-md">
                <p className="text-sm text-blue-800">
                  <strong>Need more help?</strong> Check the{' '}
                  <a 
                    href="https://github.com/your-org/rep-dashboard/blob/main/docs/HIERARCHY-TROUBLESHOOTING.md" 
                    target="_blank" 
                    rel="noopener noreferrer"
                    className="underline hover:text-blue-900"
                  >
                    Hierarchy Troubleshooting Guide
                  </a>
                </p>
              </div>
            </div>
          )}
          
          <DialogFooter>
            <Button onClick={() => setErrorDialogOpen(false)}>
              Close
            </Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>
    </Dialog>
  )
}
