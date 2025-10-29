'use client';

import React, { useState, useEffect } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { useSession } from 'next-auth/react';
import { Bell, Mail, Clock, Save, CheckCircle, Loader2 } from 'lucide-react';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Switch } from '@/components/ui/switch';
import { Label } from '@/components/ui/label';

interface PCNotificationPreferences {
  id?: number;
  user_id: string;
  email_enabled: boolean;
  email_frequency: 'immediate' | 'daily' | 'weekly';
  notification_types: string[];
  quiet_hours_start?: string;
  quiet_hours_end?: string;
}

export default function OperationsSettingsPage() {
  const { data: session, status } = useSession();
  const queryClient = useQueryClient();

  // State for preferences
  const [preferences, setPreferences] = useState<PCNotificationPreferences | null>(null);
  const [hasChanges, setHasChanges] = useState(false);
  const [saveSuccess, setSaveSuccess] = useState(false);

  // Check authentication
  if (status === 'loading') {
    return <div className="flex items-center justify-center h-screen">Loading...</div>;
  }

  if (status === 'unauthenticated') {
    return <div className="flex items-center justify-center h-screen">Please log in to view settings.</div>;
  }

  // Fetch preferences
  const { data: preferencesData, isLoading } = useQuery<{ preferences: PCNotificationPreferences }>({
    queryKey: ['notification-preferences', session?.user?.email],
    queryFn: async () => {
      const response = await fetch('/api/operations/settings/notification-preferences');
      if (!response.ok) throw new Error('Failed to fetch preferences');
      return response.json();
    },
    onSuccess: (data) => {
      setPreferences(data.preferences);
    },
  });

  // Save mutation
  const saveMutation = useMutation({
    mutationFn: async (prefs: PCNotificationPreferences) => {
      const response = await fetch('/api/operations/settings/notification-preferences', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(prefs),
      });
      if (!response.ok) throw new Error('Failed to save preferences');
      return response.json();
    },
    onSuccess: (data) => {
      queryClient.invalidateQueries(['notification-preferences']);
      setPreferences(data.preferences);
      setHasChanges(false);
      setSaveSuccess(true);
      setTimeout(() => setSaveSuccess(false), 3000);
    },
  });

  // Handlers
  const handleEmailEnabledChange = (enabled: boolean) => {
    if (!preferences) return;
    setPreferences({ ...preferences, email_enabled: enabled });
    setHasChanges(true);
  };

  const handleFrequencyChange = (frequency: 'immediate' | 'daily' | 'weekly') => {
    if (!preferences) return;
    setPreferences({ ...preferences, email_frequency: frequency });
    setHasChanges(true);
  };

  const handleToggleNotificationType = (type: string) => {
    if (!preferences) return;
    const types = preferences.notification_types || [];
    const newTypes = types.includes(type)
      ? types.filter(t => t !== type)
      : [...types, type];
    setPreferences({ ...preferences, notification_types: newTypes });
    setHasChanges(true);
  };

  const handleSelectAllMilestone = () => {
    if (!preferences) return;
    const milestoneTypes = [
      'milestone_survey_late',
      'milestone_install_late',
      'milestone_nem_overdue',
      'milestone_pto_overdue',
      'milestone_unresponsive_escalation',
    ];
    const types = preferences.notification_types || [];
    const otherTypes = types.filter(t => !t.startsWith('milestone_'));
    setPreferences({ ...preferences, notification_types: [...otherTypes, ...milestoneTypes] });
    setHasChanges(true);
  };

  const handleDeselectAllMilestone = () => {
    if (!preferences) return;
    const types = preferences.notification_types || [];
    const newTypes = types.filter(t => !t.startsWith('milestone_'));
    setPreferences({ ...preferences, notification_types: newTypes });
    setHasChanges(true);
  };

  const handleSelectAllField = () => {
    if (!preferences) return;
    const fieldTypes = [
      'arrivy_task_late',
      'arrivy_task_noshow',
      'arrivy_task_exception',
      'arrivy_task_cancelled',
    ];
    const types = preferences.notification_types || [];
    const otherTypes = types.filter(t => !t.startsWith('arrivy_'));
    setPreferences({ ...preferences, notification_types: [...otherTypes, ...fieldTypes] });
    setHasChanges(true);
  };

  const handleDeselectAllField = () => {
    if (!preferences) return;
    const types = preferences.notification_types || [];
    const newTypes = types.filter(t => !t.startsWith('arrivy_'));
    setPreferences({ ...preferences, notification_types: newTypes });
    setHasChanges(true);
  };

  const handleQuietHoursChange = (field: 'start' | 'end', value: string) => {
    if (!preferences) return;
    const key = field === 'start' ? 'quiet_hours_start' : 'quiet_hours_end';
    // Convert HH:MM to HH:MM:SS
    const timeWithSeconds = value ? `${value}:00` : undefined;
    setPreferences({ ...preferences, [key]: timeWithSeconds });
    setHasChanges(true);
  };

  const handleSave = () => {
    if (!preferences || !hasChanges) return;
    saveMutation.mutate(preferences);
  };

  // Loading state
  if (isLoading || !preferences) {
    return (
      <div className="space-y-4 mobile:space-y-6">
        <div className="flex items-center justify-center h-96">
          <Loader2 className="h-8 w-8 animate-spin text-gray-400" />
        </div>
      </div>
    );
  }

  const isTypeEnabled = (type: string) => preferences.notification_types?.includes(type) || false;

  return (
    <div className="space-y-4 mobile:space-y-6">
      {/* Header */}
      <div className="flex flex-col mobile:flex-row mobile:items-center mobile:justify-between gap-3 mobile:gap-0">
        <div>
          <h1 className="text-xl mobile:text-2xl ipad:text-3xl font-bold text-gray-900">
            Operations Settings
          </h1>
          <p className="text-sm mobile:text-base text-gray-600 mt-0.5">
            Configure operations-specific settings and preferences
          </p>
        </div>
        
        {/* Save Button */}
        <Button
          onClick={handleSave}
          disabled={!hasChanges || saveMutation.isLoading}
          className="flex items-center space-x-2"
        >
          {saveMutation.isLoading ? (
            <Loader2 className="h-4 w-4 animate-spin" />
          ) : saveSuccess ? (
            <CheckCircle className="h-4 w-4" />
          ) : (
            <Save className="h-4 w-4" />
          )}
          <span>{saveMutation.isLoading ? 'Saving...' : saveSuccess ? 'Saved!' : 'Save Changes'}</span>
        </Button>
      </div>

      {/* Success Message */}
      {saveSuccess && (
        <Card className="bg-green-50 border-green-200">
          <CardContent className="p-4">
            <div className="flex items-center space-x-2 text-green-800">
              <CheckCircle className="h-5 w-5" />
              <span className="font-medium">Settings saved successfully!</span>
            </div>
          </CardContent>
        </Card>
      )}

      {/* Error Message */}
      {saveMutation.isError && (
        <Card className="bg-red-50 border-red-200">
          <CardContent className="p-4">
            <div className="text-red-800">
              <span className="font-medium">Failed to save settings. Please try again.</span>
            </div>
          </CardContent>
        </Card>
      )}

      {/* Notification Preferences Card */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center space-x-2">
            <Bell className="h-5 w-5" />
            <span>Notification Preferences</span>
          </CardTitle>
        </CardHeader>
        <CardContent className="space-y-6">
          {/* Email Toggle */}
          <div className="flex items-center justify-between">
            <div className="space-y-0.5">
              <Label className="flex items-center space-x-2">
                <Mail className="h-4 w-4" />
                <span>Email Notifications</span>
              </Label>
              <p className="text-sm text-gray-600">
                Receive email alerts for important events
              </p>
            </div>
            <Switch
              checked={preferences.email_enabled}
              onCheckedChange={handleEmailEnabledChange}
            />
          </div>

          {/* Email Frequency */}
          <div className="space-y-2">
            <Label>Email Frequency</Label>
            <select
              value={preferences.email_frequency}
              onChange={(e) => handleFrequencyChange(e.target.value as any)}
              disabled={!preferences.email_enabled}
              className="w-full px-3 py-2 border rounded-md disabled:opacity-50 disabled:cursor-not-allowed"
            >
              <option value="immediate">Immediate (as they happen)</option>
              <option value="daily">Daily Digest</option>
              <option value="weekly">Weekly Summary</option>
            </select>
          </div>

          {/* Quiet Hours */}
          <div className="space-y-2">
            <Label className="flex items-center space-x-2">
              <Clock className="h-4 w-4" />
              <span>Quiet Hours</span>
            </Label>
            <p className="text-sm text-gray-600 mb-2">
              Don't send emails during these hours
            </p>
            <div className="grid grid-cols-2 gap-4">
              <div>
                <Label className="text-xs text-gray-600">Start Time</Label>
                <input
                  type="time"
                  value={preferences.quiet_hours_start?.substring(0, 5) || ''}
                  onChange={(e) => handleQuietHoursChange('start', e.target.value)}
                  disabled={!preferences.email_enabled}
                  className="w-full px-3 py-2 border rounded-md disabled:opacity-50 disabled:cursor-not-allowed"
                />
              </div>
              <div>
                <Label className="text-xs text-gray-600">End Time</Label>
                <input
                  type="time"
                  value={preferences.quiet_hours_end?.substring(0, 5) || ''}
                  onChange={(e) => handleQuietHoursChange('end', e.target.value)}
                  disabled={!preferences.email_enabled}
                  className="w-full px-3 py-2 border rounded-md disabled:opacity-50 disabled:cursor-not-allowed"
                />
              </div>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Alert Types Card */}
      <Card>
        <CardHeader>
          <CardTitle>Alert Types</CardTitle>
          <p className="text-sm text-gray-600">Choose which alerts you want to receive</p>
        </CardHeader>
        <CardContent className="space-y-6">
          {/* Milestone Alerts */}
          <div>
            <div className="flex items-center justify-between mb-3">
              <h3 className="font-semibold text-gray-900">Milestone Alerts</h3>
              <div className="flex space-x-2">
                <Button
                  onClick={handleSelectAllMilestone}
                  variant="outline"
                  size="sm"
                >
                  Select All
                </Button>
                <Button
                  onClick={handleDeselectAllMilestone}
                  variant="outline"
                  size="sm"
                >
                  Deselect All
                </Button>
              </div>
            </div>
            <div className="space-y-3">
              {[
                { value: 'milestone_survey_late', label: 'Survey Late' },
                { value: 'milestone_install_late', label: 'Install Late' },
                { value: 'milestone_nem_overdue', label: 'NEM Overdue' },
                { value: 'milestone_pto_overdue', label: 'PTO Overdue' },
                { value: 'milestone_unresponsive_escalation', label: 'Unresponsive Escalation' },
              ].map((type) => (
                <div key={type.value} className="flex items-center space-x-2">
                  <input
                    type="checkbox"
                    id={type.value}
                    checked={isTypeEnabled(type.value)}
                    onChange={() => handleToggleNotificationType(type.value)}
                    className="h-4 w-4 rounded border-gray-300"
                  />
                  <Label htmlFor={type.value} className="cursor-pointer">
                    {type.label}
                  </Label>
                </div>
              ))}
            </div>
          </div>

          {/* Field Alerts */}
          <div>
            <div className="flex items-center justify-between mb-3">
              <h3 className="font-semibold text-gray-900">Field Alerts</h3>
              <div className="flex space-x-2">
                <Button
                  onClick={handleSelectAllField}
                  variant="outline"
                  size="sm"
                >
                  Select All
                </Button>
                <Button
                  onClick={handleDeselectAllField}
                  variant="outline"
                  size="sm"
                >
                  Deselect All
                </Button>
              </div>
            </div>
            <div className="space-y-3">
              {[
                { value: 'arrivy_task_late', label: 'Task Running Late' },
                { value: 'arrivy_task_noshow', label: 'Customer No-Show' },
                { value: 'arrivy_task_exception', label: 'Field Exception' },
                { value: 'arrivy_task_cancelled', label: 'Task Cancelled' },
              ].map((type) => (
                <div key={type.value} className="flex items-center space-x-2">
                  <input
                    type="checkbox"
                    id={type.value}
                    checked={isTypeEnabled(type.value)}
                    onChange={() => handleToggleNotificationType(type.value)}
                    className="h-4 w-4 rounded border-gray-300"
                  />
                  <Label htmlFor={type.value} className="cursor-pointer">
                    {type.label}
                  </Label>
                </div>
              ))}
            </div>
          </div>

          {/* Validation Message */}
          {preferences.notification_types.length === 0 && (
            <div className="p-3 bg-yellow-50 border border-yellow-200 rounded-md">
              <p className="text-sm text-yellow-800">
                Please select at least one notification type to receive alerts.
              </p>
            </div>
          )}
        </CardContent>
      </Card>
    </div>
  );
}
