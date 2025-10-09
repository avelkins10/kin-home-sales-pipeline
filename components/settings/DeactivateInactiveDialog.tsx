'use client'

import { useState } from 'react'
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query'
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
import { Switch } from '@/components/ui/switch'
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Progress } from '@/components/ui/progress'
import { toast } from 'sonner'
import { getBaseUrl } from '@/lib/utils/baseUrl'
import { UserMinus, Users, AlertTriangle, Calendar, Mail } from 'lucide-react'

interface DeactivateInactiveDialogProps {
  open: boolean
  onOpenChange: (open: boolean) => void
}

interface InactiveUser {
  id: string
  email: string
  name: string
  role: string
  last_project_date?: string
  created_at: string
  days_since_last_activity: number
}

interface DeactivateResponse {
  success: boolean
  dryRun: boolean
  usersToDeactivate?: InactiveUser[]
  deactivatedUsers?: InactiveUser[]
  deactivatedCount?: number
  count?: number
  thresholdDate: string
  message: string
}

export function DeactivateInactiveDialog({ open, onOpenChange }: DeactivateInactiveDialogProps) {
  const [dryRun, setDryRun] = useState(true)
  const [monthsInactive, setMonthsInactive] = useState(12)
  const [deactivateResults, setDeactivateResults] = useState<DeactivateResponse | null>(null)

  const queryClient = useQueryClient()

  // Preview inactive users
  const { data: previewData, isLoading: isPreviewLoading } = useQuery({
    queryKey: ['inactive-users-preview', monthsInactive],
    queryFn: async () => {
      const response = await fetch(
        `${getBaseUrl()}/api/admin/users/deactivate-inactive?monthsInactive=${monthsInactive}`
      )
      
      if (!response.ok) {
        throw new Error('Failed to preview inactive users')
      }
      
      return response.json()
    },
    enabled: open,
  })

  const deactivateMutation = useMutation({
    mutationFn: async (params: { monthsInactive: number; dryRun: boolean }) => {
      const response = await fetch(`${getBaseUrl()}/api/admin/users/deactivate-inactive`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(params),
      })

      if (!response.ok) {
        const error = await response.json()
        throw new Error(error.error || 'Failed to deactivate users')
      }

      return response.json()
    },
    onSuccess: (data: DeactivateResponse) => {
      setDeactivateResults(data)
      
      if (!data.dryRun) {
        queryClient.invalidateQueries({ queryKey: ['users'] })
        toast.success('Inactive users deactivated successfully!', {
          description: data.message,
        })
      }
    },
    onError: (error: Error) => {
      toast.error('Failed to deactivate users', {
        description: error.message,
      })
    },
  })

  const handleDeactivate = () => {
    deactivateMutation.mutate({ monthsInactive, dryRun })
  }

  const handleConfirmDeactivate = () => {
    if (deactivateResults?.dryRun) {
      setDryRun(false)
      deactivateMutation.mutate({ monthsInactive, dryRun: false })
    }
  }

  const handleClose = () => {
    onOpenChange(false)
    setDeactivateResults(null)
    setDryRun(true)
    setMonthsInactive(12)
  }

  const formatDate = (dateString?: string) => {
    if (!dateString) return 'Never'
    return new Date(dateString).toLocaleDateString()
  }

  const formatDaysAgo = (days: number) => {
    if (days < 30) return `${days} days ago`
    if (days < 365) return `${Math.floor(days / 30)} months ago`
    return `${Math.floor(days / 365)} years ago`
  }

  return (
    <Dialog open={open} onOpenChange={handleClose}>
      <DialogContent className="sm:max-w-4xl max-h-[80vh] overflow-y-auto">
        <DialogHeader>
          <DialogTitle>Deactivate Inactive Users</DialogTitle>
          <DialogDescription>
            Deactivate users who haven&apos;t had project activity in the specified time period.
          </DialogDescription>
        </DialogHeader>
        
        <div className="space-y-6">
          {/* Configuration */}
          {!deactivateResults && (
            <div className="space-y-4">
              <div>
                <Label htmlFor="months-inactive">Inactivity Period</Label>
                <Select value={monthsInactive.toString()} onValueChange={(value) => setMonthsInactive(parseInt(value))}>
                  <SelectTrigger>
                    <SelectValue />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectItem value="6">6 months</SelectItem>
                    <SelectItem value="12">12 months</SelectItem>
                    <SelectItem value="18">18 months</SelectItem>
                    <SelectItem value="24">24 months</SelectItem>
                    <SelectItem value="36">36 months</SelectItem>
                  </SelectContent>
                </Select>
              </div>

              <div className="flex items-center space-x-2">
                <Switch
                  id="dry-run"
                  checked={dryRun}
                  onCheckedChange={setDryRun}
                />
                <Label htmlFor="dry-run">Preview mode (recommended)</Label>
              </div>

              {/* Preview Stats */}
              {previewData && (
                <Card>
                  <CardHeader className="pb-2">
                    <CardTitle className="text-sm flex items-center gap-2">
                      <Users className="h-4 w-4" />
                      Preview Results
                    </CardTitle>
                  </CardHeader>
                  <CardContent>
                    <div className="text-2xl font-bold text-orange-600">
                      {previewData.count} users
                    </div>
                    <p className="text-sm text-gray-600">
                      would be deactivated (no activity in {monthsInactive} months)
                    </p>
                    <p className="text-xs text-gray-500 mt-1">
                      Threshold: {formatDate(previewData.thresholdDate)}
                    </p>
                  </CardContent>
                </Card>
              )}
            </div>
          )}

          {/* Loading State */}
          {deactivateMutation.isPending && (
            <div className="space-y-4">
              <div className="flex items-center gap-2">
                <UserMinus className="h-4 w-4 animate-pulse" />
                <span>Deactivating inactive users...</span>
              </div>
              <Progress value={undefined} className="w-full" />
            </div>
          )}

          {/* Results */}
          {deactivateResults && !deactivateMutation.isPending && (
            <div className="space-y-4">
              <div className="flex items-center gap-2">
                {deactivateResults.dryRun ? (
                  <AlertTriangle className="h-5 w-5 text-yellow-500" />
                ) : (
                  <UserMinus className="h-5 w-5 text-red-500" />
                )}
                <h3 className="text-lg font-semibold">
                  {deactivateResults.dryRun ? 'Preview Results' : 'Deactivation Completed'}
                </h3>
              </div>

              <Card>
                <CardHeader className="pb-2">
                  <CardTitle className="text-sm flex items-center gap-2">
                    <UserMinus className="h-4 w-4 text-red-500" />
                    {deactivateResults.dryRun ? 'Users to Deactivate' : 'Users Deactivated'}
                  </CardTitle>
                </CardHeader>
                <CardContent>
                  <div className="text-2xl font-bold text-red-600">
                    {deactivateResults.count || deactivateResults.deactivatedCount || 0}
                  </div>
                  <p className="text-sm text-gray-600">{deactivateResults.message}</p>
                </CardContent>
              </Card>

              <div className="p-4 bg-gray-50 rounded-lg">
                <p className="text-sm text-gray-700">
                  Threshold: {formatDate(deactivateResults.thresholdDate)}
                </p>
                <p className="text-xs text-gray-500 mt-1">
                  Only closers and setters are affected. Managers and admins are excluded.
                </p>
              </div>
            </div>
          )}

          {/* User List */}
          {(previewData?.inactiveUsers || deactivateResults?.usersToDeactivate || deactivateResults?.deactivatedUsers) && (
            <div>
              <h4 className="text-sm font-medium mb-3">
                {deactivateResults?.dryRun ? 'Users to Deactivate' : 
                 deactivateResults ? 'Deactivated Users' : 'Inactive Users'}
              </h4>
              <div className="space-y-2 max-h-60 overflow-y-auto">
                {(previewData?.inactiveUsers || deactivateResults?.usersToDeactivate || deactivateResults?.deactivatedUsers || []).map((user: InactiveUser) => (
                  <Card key={user.id} className="p-3">
                    <div className="flex items-center justify-between">
                      <div className="flex-1">
                        <div className="flex items-center gap-2 mb-1">
                          <h5 className="font-medium text-sm">{user.name}</h5>
                          <Badge variant="outline" className="text-xs">{user.role}</Badge>
                        </div>
                        <div className="flex items-center gap-4 text-xs text-gray-600">
                          <div className="flex items-center gap-1">
                            <Mail className="h-3 w-3" />
                            {user.email}
                          </div>
                          <div className="flex items-center gap-1">
                            <Calendar className="h-3 w-3" />
                            Last activity: {formatDate(user.last_project_date)}
                          </div>
                          <div className="text-orange-600">
                            {formatDaysAgo(user.days_since_last_activity)}
                          </div>
                        </div>
                      </div>
                    </div>
                  </Card>
                ))}
              </div>
            </div>
          )}
        </div>
        
        <DialogFooter>
          <Button variant="outline" onClick={handleClose}>
            {deactivateResults ? 'Close' : 'Cancel'}
          </Button>
          
          {!deactivateResults && (
            <Button 
              onClick={handleDeactivate}
              disabled={deactivateMutation.isPending || isPreviewLoading}
              variant={dryRun ? "default" : "destructive"}
            >
              {dryRun ? 'Preview Deactivation' : 'Deactivate Users'}
            </Button>
          )}
          
          {deactivateResults?.dryRun && (
            <Button 
              onClick={handleConfirmDeactivate}
              disabled={deactivateMutation.isPending}
              variant="destructive"
            >
              Confirm & Deactivate Users
            </Button>
          )}
        </DialogFooter>
      </DialogContent>
    </Dialog>
  )
}
