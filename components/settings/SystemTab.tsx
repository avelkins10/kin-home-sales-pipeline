'use client'

import { useMemo, useState } from 'react'
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Input } from '@/components/ui/input'
import { Button } from '@/components/ui/button'
import { Label } from '@/components/ui/label'
import { Badge } from '@/components/ui/badge'
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select'
import { AlertCircle, CheckCircle, Clock, Database, Settings, XCircle } from 'lucide-react'
import { toast } from 'sonner'
import type { SystemSettings } from '@/lib/types/settings'

// Extended type for API responses that includes the hasQuickbaseToken flag
type SystemSettingsResponse = SystemSettings & {
  hasQuickbaseToken: boolean
}

type ConnectionStatus = 'idle' | 'testing' | 'success' | 'error'

export default function SystemTab() {
  const queryClient = useQueryClient()
  const [connectionStatus, setConnectionStatus] = useState<ConnectionStatus>('idle')
  const [localSettings, setLocalSettings] = useState<SystemSettings | null>(null)
  const [userTokenInput, setUserTokenInput] = useState<string>('')

  const { data: settings, isLoading } = useQuery<SystemSettingsResponse>({
    queryKey: ['system-settings'],
    queryFn: async () => {
      const res = await fetch('/api/admin/system/settings', { credentials: 'include' })
      if (!res.ok) throw new Error('Failed to load system settings')
      return res.json()
    },
  })

  const currentSettings = useMemo<SystemSettings | null>(() => {
    return localSettings ?? settings ?? null
  }, [localSettings, settings])

  // Get the current token value for display and testing
  const currentToken = useMemo(() => {
    if (userTokenInput) return userTokenInput
    if (settings?.hasQuickbaseToken) return '••••••••••••••••' // Show placeholder when token exists
    return ''
  }, [userTokenInput, settings?.hasQuickbaseToken])

  const updateMutation = useMutation({
    mutationFn: async (body: Partial<SystemSettings>) => {
      const res = await fetch('/api/admin/system/settings', {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(body),
        credentials: 'include',
      })
      if (!res.ok) {
        const err = await res.json().catch(() => ({}))
        throw new Error(err?.message || 'Failed to update system settings')
      }
      return res.json()
    },
    onSuccess: () => {
      toast.success('System settings updated')
      queryClient.invalidateQueries({ queryKey: ['system-settings'] })
      setLocalSettings(null)
      setUserTokenInput('') // Clear user input after successful save
    },
    onError: (error: any) => {
      toast.error(error?.message || 'Failed to update system settings')
    },
  })

  async function testQuickbaseConnection() {
    if (!currentSettings) return
    setConnectionStatus('testing')
    try {
      // Use the user's input token for testing, not the stored/masked value
      const tokenToTest = userTokenInput || currentSettings.quickbaseToken
      if (!tokenToTest) {
        setConnectionStatus('error')
        toast.error('Please enter a token to test')
        return
      }
      
      const res = await fetch('/api/admin/system/test-connection', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        credentials: 'include',
        body: JSON.stringify({
          realm: currentSettings.quickbaseRealm,
          token: tokenToTest,
        }),
      })
      const data = await res.json()
      if (data.success) {
        setConnectionStatus('success')
      } else {
        setConnectionStatus('error')
        toast.error(data.message || 'Quickbase connection failed')
      }
    } catch (e: any) {
      setConnectionStatus('error')
      toast.error('Quickbase connection failed')
    }
  }

  function handleSave() {
    if (!currentSettings) return
    
    // Only include quickbaseToken in the request if the user has entered a new value
    const bodyToSave: Partial<SystemSettings> = { ...currentSettings }
    if (userTokenInput) {
      bodyToSave.quickbaseToken = userTokenInput
    } else {
      // Don't include quickbaseToken in the request if user hasn't changed it
      delete bodyToSave.quickbaseToken
    }
    
    updateMutation.mutate(bodyToSave)
  }

  function setField<K extends keyof SystemSettings>(key: K, value: SystemSettings[K]) {
    setLocalSettings(prev => ({ ...(prev ?? (settings as SystemSettings)), [key]: value }))
  }

  function setSLAField(key: keyof SystemSettings['milestoneSLA'], value: number) {
    setLocalSettings(prev => ({
      ...(prev ?? (settings as SystemSettings)),
      milestoneSLA: {
        ...((prev ?? (settings as SystemSettings)).milestoneSLA),
        [key]: value,
      },
    }))
  }

  function addHoldReason(reason: string) {
    const trimmed = reason.trim()
    if (!trimmed || !currentSettings) return
    if (currentSettings.holdReasons.map(r => r.toLowerCase()).includes(trimmed.toLowerCase())) {
      toast.error('Hold reason already exists')
      return
    }
    setField('holdReasons', [...currentSettings.holdReasons, trimmed])
  }

  function removeHoldReason(reason: string) {
    if (!currentSettings) return
    setField('holdReasons', currentSettings.holdReasons.filter(r => r !== reason))
  }

  const [newReason, setNewReason] = useState('')

  if (isLoading || !currentSettings) {
    return (
      <Card>
        <CardContent className="p-6">
          <p className="text-gray-600">Loading system settings...</p>
        </CardContent>
      </Card>
    )
  }

  const connectionBadge = (
    <div className="flex items-center gap-2">
      {connectionStatus === 'success' && (
        <Badge className="bg-green-600"><CheckCircle className="h-4 w-4 mr-1" />Connected</Badge>
      )}
      {connectionStatus === 'error' && (
        <Badge className="bg-red-600"><XCircle className="h-4 w-4 mr-1" />Failed</Badge>
      )}
      {connectionStatus === 'testing' && (
        <Badge className="bg-amber-500"><Clock className="h-4 w-4 mr-1" />Testing...</Badge>
      )}
    </div>
  )

  return (
    <div className="space-y-6">
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2"><Database className="h-5 w-5" /> Quickbase Connection</CardTitle>
            <CardDescription>Configure Quickbase realm and token. Test without saving.</CardDescription>
          </CardHeader>
          <CardContent className="space-y-4">
            <div className="space-y-2">
              <Label htmlFor="qb-realm">Realm Hostname</Label>
              <Input id="qb-realm" value={currentSettings.quickbaseRealm}
                onChange={(e) => setField('quickbaseRealm', e.target.value)}
                placeholder="kin.quickbase.com" />
            </div>
            <div className="space-y-2">
              <Label htmlFor="qb-token">User Token</Label>
              <Input id="qb-token" type="password" value={currentToken}
                onChange={(e) => setUserTokenInput(e.target.value)}
                placeholder={settings?.hasQuickbaseToken ? "Enter new token to update" : "Enter Quickbase user token"} />
              {settings?.hasQuickbaseToken && !userTokenInput && (
                <p className="text-sm text-gray-600">Token is configured. Enter a new token to update it.</p>
              )}
            </div>
            <div className="flex items-center justify-between">
              <Button type="button" variant="secondary" onClick={testQuickbaseConnection} disabled={connectionStatus === 'testing'}>
                {connectionStatus === 'testing' ? 'Testing...' : 'Test Connection'}
              </Button>
              {connectionBadge}
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2"><Settings className="h-5 w-5" /> Milestone SLA Defaults</CardTitle>
            <CardDescription>Default days per milestone and alert thresholds.</CardDescription>
          </CardHeader>
          <CardContent className="space-y-4">
            <div className="grid grid-cols-1 sm:grid-cols-2 gap-4">
              {(
                [
                  ['survey', 'Survey'],
                  ['design', 'Design'],
                  ['permit', 'Permit'],
                  ['nem', 'NEM'],
                  ['install', 'Install'],
                  ['inspection', 'Inspection'],
                  ['pto', 'PTO'],
                ] as Array<[keyof SystemSettings['milestoneSLA'], string]>
              ).map(([key, label]) => (
                <div key={key} className="space-y-2">
                  <Label htmlFor={`sla-${key}`}>{label} (days)</Label>
                  <Input id={`sla-${key}`} type="number" min={1} max={key === 'permit' ? 90 : key === 'install' || key === 'inspection' ? 30 : 60}
                    value={currentSettings.milestoneSLA[key]}
                    onChange={(e) => setSLAField(key, Number(e.target.value))} />
                </div>
              ))}
            </div>
            <div className="grid grid-cols-1 sm:grid-cols-2 gap-4">
              <div className="space-y-2">
                <Label htmlFor="warning-threshold">Warning Threshold (%)</Label>
                <Input id="warning-threshold" type="number" min={50} max={100}
                  value={currentSettings.warningThreshold}
                  onChange={(e) => setField('warningThreshold', Number(e.target.value))} />
              </div>
              <div className="space-y-2">
                <Label htmlFor="critical-threshold">Critical Threshold (%)</Label>
                <Input id="critical-threshold" type="number" min={50} max={150}
                  value={currentSettings.criticalThreshold}
                  onChange={(e) => setField('criticalThreshold', Number(e.target.value))} />
              </div>
            </div>
          </CardContent>
        </Card>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2"><AlertCircle className="h-5 w-5" /> Hold Reasons</CardTitle>
            <CardDescription>Manage the list of hold reasons available.</CardDescription>
          </CardHeader>
          <CardContent className="space-y-4">
            <div className="flex flex-wrap gap-2">
              {currentSettings.holdReasons.map((reason) => (
                <Badge key={reason} variant="secondary" className="px-2 py-1 cursor-pointer" onClick={() => removeHoldReason(reason)}>
                  <span className="mr-1">{reason}</span>
                  <XCircle className="h-3 w-3" />
                </Badge>
              ))}
            </div>
            <div className="flex gap-2">
              <Input
                placeholder="Add a hold reason"
                value={newReason}
                onChange={(e) => setNewReason(e.target.value)}
                onKeyDown={(e) => {
                  if (e.key === 'Enter') {
                    addHoldReason(newReason)
                    setNewReason('')
                  }
                }}
              />
              <Button type="button" onClick={() => { addHoldReason(newReason); setNewReason('') }}>Add</Button>
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2"><Settings className="h-5 w-5" /> General Settings</CardTitle>
            <CardDescription>Display preferences and session configuration.</CardDescription>
          </CardHeader>
          <CardContent className="space-y-4">
            <div className="grid grid-cols-1 sm:grid-cols-2 gap-4">
              <div className="space-y-2">
                <Label>Date Format</Label>
                <Select value={currentSettings.dateFormat} onValueChange={(v) => setField('dateFormat', v as SystemSettings['dateFormat'])}>
                  <SelectTrigger>
                    <SelectValue placeholder="Select date format" />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectItem value="MM/DD/YYYY">MM/DD/YYYY</SelectItem>
                    <SelectItem value="DD/MM/YYYY">DD/MM/YYYY</SelectItem>
                    <SelectItem value="YYYY-MM-DD">YYYY-MM-DD</SelectItem>
                  </SelectContent>
                </Select>
              </div>
              <div className="space-y-2">
                <Label>Timezone</Label>
                <Select value={currentSettings.timezone} onValueChange={(v) => setField('timezone', v)}>
                  <SelectTrigger>
                    <SelectValue placeholder="Select timezone" />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectItem value="America/New_York">America/New_York</SelectItem>
                    <SelectItem value="America/Chicago">America/Chicago</SelectItem>
                    <SelectItem value="America/Denver">America/Denver</SelectItem>
                    <SelectItem value="America/Los_Angeles">America/Los_Angeles</SelectItem>
                    <SelectItem value="America/Phoenix">America/Phoenix</SelectItem>
                  </SelectContent>
                </Select>
              </div>
            </div>
            <div className="space-y-2">
              <Label htmlFor="session-timeout">Session Timeout (minutes)</Label>
              <Input id="session-timeout" type="number" min={15} max={480}
                value={currentSettings.sessionTimeout}
                onChange={(e) => setField('sessionTimeout', Number(e.target.value))} />
            </div>
          </CardContent>
        </Card>
      </div>

      <div>
        <Button className="w-full" size="lg" disabled={updateMutation.isPending} onClick={handleSave}>
          {updateMutation.isPending ? 'Saving...' : 'Save System Settings'}
        </Button>
      </div>
    </div>
  )
}


