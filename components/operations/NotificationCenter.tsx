'use client';

/**
 * NotificationCenter Component
 * Displays PC milestone notifications in a slide-over panel
 */

import React, { useState, useEffect } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { 
  X, 
  Bell, 
  AlertCircle, 
  Clock, 
  CheckCircle, 
  RefreshCw,
  Filter,
  MoreHorizontal
} from 'lucide-react';
import { Notification } from '@/lib/types/notification';

interface NotificationCenterProps {
  isOpen: boolean;
  onClose: () => void;
}

type FilterType = 'all' | 'unread' | 'critical';
type NotificationType = 'all' 
  | 'milestone_survey_late' 
  | 'milestone_install_late' 
  | 'milestone_nem_overdue' 
  | 'milestone_pto_overdue' 
  | 'milestone_unresponsive_escalation'
  | 'arrivy_task_late'
  | 'arrivy_task_noshow'
  | 'arrivy_task_exception'
  | 'arrivy_task_cancelled';

export function NotificationCenter({ isOpen, onClose }: NotificationCenterProps) {
  const [filter, setFilter] = useState<FilterType>('all');
  const [selectedType, setSelectedType] = useState<NotificationType>('all');
  const [isChecking, setIsChecking] = useState(false);
  
  const queryClient = useQueryClient();

  // Fetch notifications
  const { data: notificationsData, isLoading, error } = useQuery({
    queryKey: ['pc-notifications', filter, selectedType],
    queryFn: async () => {
      const params = new URLSearchParams();
      if (filter === 'unread') params.append('unreadOnly', 'true');
      if (selectedType !== 'all') params.append('type', selectedType);
      
      const response = await fetch(`/api/operations/notifications?${params.toString()}`);
      if (!response.ok) throw new Error('Failed to fetch notifications');
      return response.json();
    },
    refetchInterval: 30000, // 30 seconds
    enabled: isOpen
  });

  // Mark notification as read mutation
  const markAsReadMutation = useMutation({
    mutationFn: async (notificationId: number) => {
      const response = await fetch(`/api/notifications/${notificationId}/read`, {
        method: 'POST'
      });
      if (!response.ok) throw new Error('Failed to mark as read');
      return response.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['pc-notifications'] });
    }
  });

  // Manual check mutation
  const checkMutation = useMutation({
    mutationFn: async () => {
      const response = await fetch('/api/operations/notifications/check', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({})
      });
      if (!response.ok) throw new Error('Failed to run check');
      return response.json();
    },
    onSuccess: (data) => {
      setIsChecking(false);
      queryClient.invalidateQueries({ queryKey: ['pc-notifications'] });
      // Show success message (could be a toast notification)
      console.log('Check completed:', data.summary);
    },
    onError: () => {
      setIsChecking(false);
    }
  });

  // Handle manual check
  const handleCheckNow = async () => {
    setIsChecking(true);
    checkMutation.mutate();
  };

  // Handle notification click
  const handleNotificationClick = (notification: Notification) => {
    if (!notification.is_read) {
      markAsReadMutation.mutate(notification.id);
    }
    // TODO: Navigate to project detail modal
    console.log('Navigate to project:', notification.action_url);
  };

  // Handle mark all as read
  const handleMarkAllRead = async () => {
    const unreadNotifications = notificationsData?.notifications.filter(n => !n.is_read) || [];
    for (const notification of unreadNotifications) {
      markAsReadMutation.mutate(notification.id);
    }
  };

  // Get notification icon and color
  const getNotificationIcon = (notification: Notification) => {
    // Check if it's an Arrivy notification
    if (notification.source === 'arrivy' || notification.type.startsWith('arrivy_')) {
      if (notification.type === 'arrivy_task_cancelled') {
        return <Clock className="h-5 w-5 text-gray-500" />;
      }
      return <AlertCircle className="h-5 w-5 text-red-500" />;
    }
    
    // Default for milestone notifications
    if (notification.priority === 'critical') {
      return <AlertCircle className="h-5 w-5 text-red-500" />;
    }
    return <Clock className="h-5 w-5 text-orange-500" />;
  };

  // Format relative time
  const formatRelativeTime = (date: Date) => {
    const now = new Date();
    const diffMs = now.getTime() - date.getTime();
    const diffHours = Math.floor(diffMs / (1000 * 60 * 60));
    const diffDays = Math.floor(diffHours / 24);

    if (diffDays > 0) return `${diffDays} day${diffDays > 1 ? 's' : ''} ago`;
    if (diffHours > 0) return `${diffHours} hour${diffHours > 1 ? 's' : ''} ago`;
    return 'Just now';
  };

  // Group notifications by date
  const groupNotificationsByDate = (notifications: Notification[]) => {
    const groups: { [key: string]: Notification[] } = {
      'Today': [],
      'Yesterday': [],
      'This Week': [],
      'Older': []
    };

    const now = new Date();
    const today = new Date(now.getFullYear(), now.getMonth(), now.getDate());
    const yesterday = new Date(today.getTime() - 24 * 60 * 60 * 1000);
    const weekAgo = new Date(today.getTime() - 7 * 24 * 60 * 60 * 1000);

    notifications.forEach(notification => {
      const notificationDate = new Date(notification.created_at);
      
      if (notificationDate >= today) {
        groups['Today'].push(notification);
      } else if (notificationDate >= yesterday) {
        groups['Yesterday'].push(notification);
      } else if (notificationDate >= weekAgo) {
        groups['This Week'].push(notification);
      } else {
        groups['Older'].push(notification);
      }
    });

    return Object.entries(groups).filter(([_, notifications]) => notifications.length > 0);
  };

  if (!isOpen) return null;

  const notifications = notificationsData?.notifications || [];
  const groupedNotifications = groupNotificationsByDate(notifications);

  return (
    <>
      {/* Backdrop */}
      <div 
        className="fixed inset-0 bg-black bg-opacity-50 z-40"
        onClick={onClose}
      />
      
      {/* Panel */}
      <div className="fixed right-0 top-0 h-full w-96 bg-white shadow-xl z-50 flex flex-col">
        {/* Header */}
        <div className="flex items-center justify-between p-4 border-b">
          <h2 className="text-lg font-semibold">Notifications</h2>
          <Button variant="ghost" size="sm" onClick={onClose}>
            <X className="h-4 w-4" />
          </Button>
        </div>

        {/* Filters */}
        <div className="p-4 border-b space-y-3">
          {/* Filter tabs */}
          <div className="flex space-x-1">
            {(['all', 'unread', 'critical'] as FilterType[]).map((filterType) => (
              <Button
                key={filterType}
                variant={filter === filterType ? 'default' : 'ghost'}
                size="sm"
                onClick={() => setFilter(filterType)}
                className="capitalize"
              >
                {filterType}
              </Button>
            ))}
          </div>

          {/* Type filter */}
          <div className="flex items-center space-x-2">
            <Filter className="h-4 w-4 text-gray-500" />
            <select
              value={selectedType}
              onChange={(e) => setSelectedType(e.target.value as NotificationType)}
              className="text-sm border rounded px-2 py-1"
            >
              <option value="all">All Types</option>
              <optgroup label="Milestone Alerts">
                <option value="milestone_survey_late">Survey Late</option>
                <option value="milestone_install_late">Install Late</option>
                <option value="milestone_nem_overdue">NEM Overdue</option>
                <option value="milestone_pto_overdue">PTO Overdue</option>
                <option value="milestone_unresponsive_escalation">Unresponsive</option>
              </optgroup>
              <optgroup label="Field Alerts">
                <option value="arrivy_task_late">Field Task Late</option>
                <option value="arrivy_task_noshow">Customer No-Show</option>
                <option value="arrivy_task_exception">Field Exception</option>
                <option value="arrivy_task_cancelled">Task Cancelled</option>
              </optgroup>
            </select>
          </div>

          {/* Actions */}
          <div className="flex space-x-2">
            <Button
              variant="outline"
              size="sm"
              onClick={handleMarkAllRead}
              disabled={!notifications.some(n => !n.is_read)}
            >
              <CheckCircle className="h-4 w-4 mr-1" />
              Mark All Read
            </Button>
            <Button
              variant="outline"
              size="sm"
              onClick={handleCheckNow}
              disabled={isChecking}
            >
              <RefreshCw className={`h-4 w-4 mr-1 ${isChecking ? 'animate-spin' : ''}`} />
              Check Now
            </Button>
          </div>
        </div>

        {/* Content */}
        <div className="flex-1 overflow-y-auto">
          {isLoading ? (
            <div className="p-4 space-y-3">
              {[...Array(3)].map((_, i) => (
                <div key={i} className="animate-pulse">
                  <div className="h-20 bg-gray-200 rounded-lg"></div>
                </div>
              ))}
            </div>
          ) : error ? (
            <div className="p-4 text-center text-red-500">
              Failed to load notifications
            </div>
          ) : notifications.length === 0 ? (
            <div className="p-4 text-center text-gray-500">
              <Bell className="h-12 w-12 mx-auto mb-2 text-gray-300" />
              <p>No notifications</p>
            </div>
          ) : (
            <div className="p-4 space-y-4">
              {groupedNotifications.map(([groupName, groupNotifications]) => (
                <div key={groupName}>
                  <h3 className="text-sm font-medium text-gray-500 mb-2">{groupName}</h3>
                  <div className="space-y-2">
                    {groupNotifications.map((notification) => (
                      <Card
                        key={notification.id}
                        className={`cursor-pointer transition-colors hover:bg-gray-50 ${
                          !notification.is_read ? 'border-l-4 border-l-blue-500' : ''
                        }`}
                        onClick={() => handleNotificationClick(notification)}
                      >
                        <CardContent className="p-3">
                          <div className="flex items-start space-x-3">
                            {getNotificationIcon(notification)}
                            <div className="flex-1 min-w-0">
                              <div className="flex items-center justify-between">
                                <h4 className="text-sm font-medium text-gray-900 truncate">
                                  {notification.title}
                                </h4>
                                {!notification.is_read && (
                                  <div className="w-2 h-2 bg-blue-500 rounded-full flex-shrink-0"></div>
                                )}
                              </div>
                              <p className="text-xs text-gray-600 mt-1 line-clamp-2">
                                {notification.message}
                              </p>
                              <div className="flex items-center justify-between mt-2">
                                <span className="text-xs text-gray-400">
                                  {formatRelativeTime(new Date(notification.created_at))}
                                </span>
                                <Badge
                                  variant={notification.priority === 'critical' ? 'destructive' : 'secondary'}
                                  className="text-xs"
                                >
                                  {notification.priority}
                                </Badge>
                              </div>
                            </div>
                            <Button variant="ghost" size="sm">
                              <MoreHorizontal className="h-4 w-4" />
                            </Button>
                          </div>
                        </CardContent>
                      </Card>
                    ))}
                  </div>
                </div>
              ))}
            </div>
          )}
        </div>

        {/* Footer */}
        <div className="p-4 border-t text-center text-xs text-gray-500">
          Auto-refreshes every 30 seconds
        </div>
      </div>
    </>
  );
}
