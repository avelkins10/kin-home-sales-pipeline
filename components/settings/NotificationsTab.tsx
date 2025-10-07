'use client'

import { useState } from 'react'
import { useMutation, useQueryClient } from '@tanstack/react-query'
import { Card, CardHeader, CardTitle, CardContent, CardDescription } from '@/components/ui/card'
import { Switch } from '@/components/ui/switch'
import { Input } from '@/components/ui/input'
import { Button } from '@/components/ui/button'
import { Label } from '@/components/ui/label'
import { Bell, Mail, AlertCircle } from 'lucide-react'
import { toast } from 'sonner'
import { NotificationSettings } from '@/lib/types/user'

interface NotificationsTabProps {
  userRole: string
  currentSettings: NotificationSettings
}

export function NotificationsTab({ userRole, currentSettings }: NotificationsTabProps) {
  const queryClient = useQueryClient()
  const [settings, setSettings] = useState<NotificationSettings>(currentSettings)
  
  const hasTeamAccess = ['office_leader', 'regional', 'super_admin'].includes(userRole)

  // Update settings mutation
  const updateMutation = useMutation({
    mutationFn: async (newSettings: NotificationSettings) => {
      const response = await fetch('/api/user/notifications', {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(newSettings),
      })
      
      if (!response.ok) {
        const error = await response.json()
        throw new Error(error.message || 'Failed to update notification settings')
      }
      
      return response.json()
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['user-settings'] })
      toast.success('Notification settings updated')
    },
    onError: (error: Error) => {
      toast.error(error.message)
    },
  })

  // Test notification mutation
  const testNotificationMutation = useMutation({
    mutationFn: async () => {
      const response = await fetch('/api/user/test-notification', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
      })
      
      if (!response.ok) {
        const error = await response.json()
        throw new Error(error.message || 'Failed to send test notification')
      }
      
      return response.json()
    },
    onSuccess: () => {
      toast.success('Test notification sent! Check your email.')
    },
    onError: (error: Error) => {
      toast.error(error.message)
    },
  })

  const handleSave = () => {
    updateMutation.mutate(settings)
  }

  const sendTestNotification = () => {
    testNotificationMutation.mutate()
  }

  const updateSetting = (key: keyof NotificationSettings, value: boolean | number) => {
    setSettings(prev => ({ ...prev, [key]: value }))
  }

  return (
    <div className="space-y-6">
      {/* Email Notifications Card */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Mail className="w-5 h-5" />
            Email Notifications
          </CardTitle>
          <CardDescription>
            Manage how you receive updates and alerts via email
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="flex items-center justify-between">
            <div className="space-y-0.5">
              <Label htmlFor="email-enabled">Enable Email Notifications</Label>
              <p className="text-sm text-gray-500">Master toggle for all email notifications</p>
            </div>
            <Switch
              id="email-enabled"
              checked={settings.emailEnabled}
              onCheckedChange={(checked) => updateSetting('emailEnabled', checked)}
            />
          </div>

          <div className="flex items-center justify-between">
            <div className="space-y-0.5">
              <Label htmlFor="urgent-alerts">Urgent Alerts</Label>
              <p className="text-sm text-gray-500">Receive immediate alerts for critical issues</p>
            </div>
            <Switch
              id="urgent-alerts"
              checked={settings.urgentAlerts}
              onCheckedChange={(checked) => updateSetting('urgentAlerts', checked)}
              disabled={!settings.emailEnabled}
            />
          </div>

          <div className="flex items-center justify-between">
            <div className="space-y-0.5">
              <Label htmlFor="daily-digest">Daily Digest</Label>
              <p className="text-sm text-gray-500">Daily summary email at 8:00 AM</p>
            </div>
            <Switch
              id="daily-digest"
              checked={settings.dailyDigest}
              onCheckedChange={(checked) => updateSetting('dailyDigest', checked)}
              disabled={!settings.emailEnabled}
            />
          </div>

          <div className="flex items-center justify-between">
            <div className="space-y-0.5">
              <Label htmlFor="weekly-summary">Weekly Summary</Label>
              <p className="text-sm text-gray-500">Weekly performance report on Monday at 9:00 AM</p>
            </div>
            <Switch
              id="weekly-summary"
              checked={settings.weeklySummary}
              onCheckedChange={(checked) => updateSetting('weeklySummary', checked)}
              disabled={!settings.emailEnabled}
            />
          </div>
        </CardContent>
      </Card>

      {/* Alert Thresholds Card - Only for office leaders and above */}
      {hasTeamAccess && (
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <AlertCircle className="w-5 h-5" />
              Alert Thresholds
            </CardTitle>
            <CardDescription>
              Configure when you receive urgent alerts based on project status
            </CardDescription>
          </CardHeader>
          <CardContent className="space-y-4">
            <div className="space-y-2">
              <Label htmlFor="hold-threshold">Days on Hold Before Urgent Alert</Label>
              <Input
                id="hold-threshold"
                type="number"
                min="1"
                max="30"
                value={settings.holdThreshold}
                onChange={(e) => updateSetting('holdThreshold', parseInt(e.target.value) || 7)}
              />
              <p className="text-xs text-gray-500">Minimum: 1 day, Maximum: 30 days</p>
            </div>

            <div className="space-y-2">
              <Label htmlFor="age-warning-threshold">Project Age Warning Threshold</Label>
              <Input
                id="age-warning-threshold"
                type="number"
                min="30"
                max="180"
                value={settings.ageWarningThreshold}
                onChange={(e) => updateSetting('ageWarningThreshold', parseInt(e.target.value) || 90)}
              />
              <p className="text-xs text-gray-500">Minimum: 30 days, Maximum: 180 days</p>
            </div>

            <div className="space-y-2">
              <Label htmlFor="install-overdue-threshold">Install Overdue Threshold</Label>
              <Input
                id="install-overdue-threshold"
                type="number"
                min="7"
                max="60"
                value={settings.installOverdueThreshold}
                onChange={(e) => updateSetting('installOverdueThreshold', parseInt(e.target.value) || 14)}
              />
              <p className="text-xs text-gray-500">Minimum: 7 days, Maximum: 60 days</p>
            </div>
          </CardContent>
        </Card>
      )}

      {/* Test Notifications Card */}
      <Card>
        <CardHeader>
          <CardTitle>Test Notifications</CardTitle>
          <CardDescription>
            Send a test email to verify your notification settings
          </CardDescription>
        </CardHeader>
        <CardContent>
          <Button 
            variant="outline" 
            onClick={sendTestNotification}
            disabled={testNotificationMutation.isPending}
            className="w-full"
          >
            <Bell className="w-4 h-4 mr-2" />
            {testNotificationMutation.isPending ? 'Sending...' : 'Send Test Email'}
          </Button>
        </CardContent>
      </Card>

      {/* Save Button */}
      <Button 
        onClick={handleSave}
        disabled={updateMutation.isPending}
        size="lg"
        className="w-full"
      >
        {updateMutation.isPending ? 'Saving...' : 'Save Notification Settings'}
      </Button>
    </div>
  )
}
