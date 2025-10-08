'use client'

import { useState } from 'react'
import { useMutation, useQueryClient } from '@tanstack/react-query'
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
import { RefreshCw, Users, UserPlus, UserCheck, AlertTriangle } from 'lucide-react'

interface SmartSyncDialogProps {
  open: boolean
  onOpenChange: (open: boolean) => void
}

interface SyncResults {
  created: number
  updated: number
  skipped: number
  conflicts: any[]
  inactiveSkipped: number
}

interface SyncResponse {
  success: boolean
  dryRun: boolean
  results: SyncResults
  usersToSync?: any[]
  message: string
}

export function SmartSyncDialog({ open, onOpenChange }: SmartSyncDialogProps) {
  const [dryRun, setDryRun] = useState(true)
  const [monthsBack, setMonthsBack] = useState(6)
  const [role, setRole] = useState('both')
  const [syncResults, setSyncResults] = useState<SyncResponse | null>(null)

  const queryClient = useQueryClient()

  const syncMutation = useMutation({
    mutationFn: async (params: { dryRun: boolean; monthsBack: number; role: string }) => {
      const response = await fetch(`${getBaseUrl()}/api/admin/users/sync`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(params),
      })

      if (!response.ok) {
        const error = await response.json()
        throw new Error(error.error || 'Failed to sync users')
      }

      return response.json()
    },
    onSuccess: (data: SyncResponse) => {
      setSyncResults(data)
      
      if (!data.dryRun) {
        queryClient.invalidateQueries({ queryKey: ['users'] })
        toast.success('Smart sync completed successfully!', {
          description: data.message,
        })
      }
    },
    onError: (error: Error) => {
      toast.error('Smart sync failed', {
        description: error.message,
      })
    },
  })

  const handleSync = () => {
    syncMutation.mutate({ dryRun, monthsBack, role })
  }

  const handleConfirmSync = () => {
    if (syncResults?.dryRun) {
      setDryRun(false)
      syncMutation.mutate({ dryRun: false, monthsBack, role })
    }
  }

  const handleClose = () => {
    onOpenChange(false)
    setSyncResults(null)
    setDryRun(true)
    setMonthsBack(6)
    setRole('both')
  }

  const getRoleLabel = (roleValue: string) => {
    switch (roleValue) {
      case 'closer': return 'Closers'
      case 'setter': return 'Setters'
      case 'both': return 'Both Closers & Setters'
      default: return roleValue
    }
  }

  return (
    <Dialog open={open} onOpenChange={handleClose}>
      <DialogContent className="sm:max-w-2xl">
        <DialogHeader>
          <DialogTitle>Smart Sync</DialogTitle>
          <DialogDescription>
            Synchronize users from QuickBase based on recent activity.
          </DialogDescription>
        </DialogHeader>
        
        <div className="space-y-6">
          {/* Configuration */}
          {!syncResults && (
            <div className="space-y-4">
              <div>
                <Label htmlFor="months-back">Activity Period</Label>
                <Select value={monthsBack.toString()} onValueChange={(value) => setMonthsBack(parseInt(value))}>
                  <SelectTrigger>
                    <SelectValue />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectItem value="3">3 months</SelectItem>
                    <SelectItem value="6">6 months</SelectItem>
                    <SelectItem value="12">12 months</SelectItem>
                    <SelectItem value="24">24 months</SelectItem>
                  </SelectContent>
                </Select>
              </div>

              <div>
                <Label htmlFor="role-filter">Role Filter</Label>
                <Select value={role} onValueChange={setRole}>
                  <SelectTrigger>
                    <SelectValue />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectItem value="both">Both Closers & Setters</SelectItem>
                    <SelectItem value="closer">Closers Only</SelectItem>
                    <SelectItem value="setter">Setters Only</SelectItem>
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
            </div>
          )}

          {/* Loading State */}
          {syncMutation.isPending && (
            <div className="space-y-4">
              <div className="flex items-center gap-2">
                <RefreshCw className="h-4 w-4 animate-spin" />
                <span>Syncing users from QuickBase...</span>
              </div>
              <Progress value={undefined} className="w-full" />
            </div>
          )}

          {/* Results */}
          {syncResults && !syncMutation.isPending && (
            <div className="space-y-4">
              <div className="flex items-center gap-2">
                {syncResults.dryRun ? (
                  <AlertTriangle className="h-5 w-5 text-yellow-500" />
                ) : (
                  <Users className="h-5 w-5 text-green-500" />
                )}
                <h3 className="text-lg font-semibold">
                  {syncResults.dryRun ? 'Preview Results' : 'Sync Completed'}
                </h3>
              </div>

              <div className="grid grid-cols-2 gap-4">
                <Card>
                  <CardHeader className="pb-2">
                    <CardTitle className="text-sm flex items-center gap-2">
                      <UserPlus className="h-4 w-4 text-green-500" />
                      New Users
                    </CardTitle>
                  </CardHeader>
                  <CardContent>
                    <div className="text-2xl font-bold text-green-600">
                      {syncResults.results.created}
                    </div>
                  </CardContent>
                </Card>

                <Card>
                  <CardHeader className="pb-2">
                    <CardTitle className="text-sm flex items-center gap-2">
                      <UserCheck className="h-4 w-4 text-blue-500" />
                      Updated Users
                    </CardTitle>
                  </CardHeader>
                  <CardContent>
                    <div className="text-2xl font-bold text-blue-600">
                      {syncResults.results.updated}
                    </div>
                  </CardContent>
                </Card>

                <Card>
                  <CardHeader className="pb-2">
                    <CardTitle className="text-sm flex items-center gap-2">
                      <AlertTriangle className="h-4 w-4 text-yellow-500" />
                      Skipped
                    </CardTitle>
                  </CardHeader>
                  <CardContent>
                    <div className="text-2xl font-bold text-yellow-600">
                      {syncResults.results.skipped}
                    </div>
                  </CardContent>
                </Card>

                <Card>
                  <CardHeader className="pb-2">
                    <CardTitle className="text-sm flex items-center gap-2">
                      <Users className="h-4 w-4 text-gray-500" />
                      Inactive Skipped
                    </CardTitle>
                  </CardHeader>
                  <CardContent>
                    <div className="text-2xl font-bold text-gray-600">
                      {syncResults.results.inactiveSkipped}
                    </div>
                  </CardContent>
                </Card>
              </div>

              <div className="p-4 bg-gray-50 rounded-lg">
                <p className="text-sm text-gray-700">{syncResults.message}</p>
                <div className="mt-2 text-xs text-gray-500">
                  <p>Activity period: {monthsBack} months</p>
                  <p>Role filter: {getRoleLabel(role)}</p>
                </div>
              </div>

              {/* Conflicts */}
              {syncResults.results.conflicts.length > 0 && (
                <div>
                  <h4 className="text-sm font-medium mb-2 text-red-600">Conflicts</h4>
                  <div className="space-y-2 max-h-32 overflow-y-auto">
                    {syncResults.results.conflicts.map((conflict, index) => (
                      <div key={index} className="p-2 bg-red-50 border border-red-200 rounded text-xs">
                        <div className="font-medium">{conflict.user.name} ({conflict.user.email})</div>
                        <div className="text-red-600">{conflict.error}</div>
                      </div>
                    ))}
                  </div>
                </div>
              )}

              {/* Preview Users */}
              {syncResults.dryRun && syncResults.usersToSync && syncResults.usersToSync.length > 0 && (
                <div>
                  <h4 className="text-sm font-medium mb-2">Preview Users (first 10)</h4>
                  <div className="space-y-2 max-h-40 overflow-y-auto">
                    {syncResults.usersToSync.map((user, index) => (
                      <div key={index} className="flex items-center justify-between p-2 bg-gray-50 rounded text-xs">
                        <div>
                          <span className="font-medium">{user.name}</span>
                          <span className="text-gray-500 ml-2">({user.email})</span>
                        </div>
                        <Badge variant="outline">{user.role}</Badge>
                      </div>
                    ))}
                  </div>
                </div>
              )}
            </div>
          )}
        </div>
        
        <DialogFooter>
          <Button variant="outline" onClick={handleClose}>
            {syncResults ? 'Close' : 'Cancel'}
          </Button>
          
          {!syncResults && (
            <Button 
              onClick={handleSync}
              disabled={syncMutation.isPending}
            >
              {dryRun ? 'Preview Sync' : 'Start Sync'}
            </Button>
          )}
          
          {syncResults?.dryRun && (
            <Button 
              onClick={handleConfirmSync}
              disabled={syncMutation.isPending}
            >
              Confirm & Execute Sync
            </Button>
          )}
        </DialogFooter>
      </DialogContent>
    </Dialog>
  )
}
