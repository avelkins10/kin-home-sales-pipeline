'use client';

import React, { useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { useSession } from 'next-auth/react';
import { Bell, AlertCircle, Clock, CheckCircle, Filter, Search, RefreshCw, ExternalLink } from 'lucide-react';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import type { Notification } from '@/lib/types/notification';
import { formatDistanceToNow } from 'date-fns';

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
type SourceType = 'all' | 'system' | 'arrivy';

interface NotificationsResponse {
  notifications: Notification[];
  unread_count: number;
  has_more: boolean;
}

export default function AlertsPage() {
  const { data: session, status } = useSession();
  const queryClient = useQueryClient();

  // State
  const [filter, setFilter] = useState<FilterType>('all');
  const [selectedType, setSelectedType] = useState<NotificationType>('all');
  const [selectedSource, setSelectedSource] = useState<SourceType>('all');
  const [searchQuery, setSearchQuery] = useState('');
  const [offset, setOffset] = useState(0);
  const limit = 20;

  // Check authentication and role
  if (status === 'loading') {
    return <div className="flex items-center justify-center h-screen">Loading...</div>;
  }

  if (status === 'unauthenticated') {
    return <div className="flex items-center justify-center h-screen">Please log in to view alerts.</div>;
  }

  const operationsRoles = ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'];
  if (!session?.user?.role || !operationsRoles.includes(session.user.role)) {
    return <div className="flex items-center justify-center h-screen">Access denied. Operations role required.</div>;
  }

  // Fetch notifications
  const { data: notificationsData, isLoading, error, refetch } = useQuery<NotificationsResponse>({
    queryKey: ['alerts', filter, selectedType, selectedSource, searchQuery, offset],
    queryFn: async () => {
      const params = new URLSearchParams();
      if (filter === 'unread') params.append('unreadOnly', 'true');
      if (selectedType !== 'all') params.append('type', selectedType);
      if (selectedSource !== 'all') params.append('source', selectedSource);
      if (searchQuery) params.append('search', searchQuery);
      params.append('limit', limit.toString());
      params.append('offset', offset.toString());
      
      const response = await fetch(`/api/operations/notifications?${params.toString()}`);
      if (!response.ok) throw new Error('Failed to fetch alerts');
      return response.json();
    },
    refetchInterval: 30000, // Auto-refresh every 30 seconds
    keepPreviousData: true,
  });

  // Mark as read mutation
  const markAsReadMutation = useMutation({
    mutationFn: async (notificationId: number) => {
      const response = await fetch(`/api/notifications/${notificationId}/read`, {
        method: 'POST',
      });
      if (!response.ok) throw new Error('Failed to mark as read');
      return response.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries(['alerts']);
      queryClient.invalidateQueries(['unread-counts']);
    },
  });

  // Mark all as read mutation
  const markAllReadMutation = useMutation({
    mutationFn: async () => {
      const unreadNotifications = notificationsData?.notifications.filter(n => !n.is_read) || [];
      for (const notification of unreadNotifications) {
        await fetch(`/api/notifications/${notification.id}/read`, { method: 'POST' });
      }
    },
    onSuccess: () => {
      queryClient.invalidateQueries(['alerts']);
      queryClient.invalidateQueries(['unread-counts']);
    },
  });

  // Handlers
  const handleNotificationClick = (notification: Notification) => {
    if (!notification.is_read) {
      markAsReadMutation.mutate(notification.id);
    }
    if (notification.action_url) {
      window.location.href = notification.action_url;
    }
  };

  const handleMarkAllRead = () => {
    markAllReadMutation.mutate();
  };

  const handleResetFilters = () => {
    setFilter('all');
    setSelectedType('all');
    setSelectedSource('all');
    setSearchQuery('');
    setOffset(0);
  };

  const handleLoadMore = () => {
    setOffset(prev => prev + limit);
  };

  // Get notification icon
  const getNotificationIcon = (notification: Notification) => {
    if (notification.source === 'arrivy' || notification.type.startsWith('arrivy_')) {
      if (notification.type === 'arrivy_task_cancelled') {
        return <Clock className="h-6 w-6 text-gray-500" />;
      }
      return <AlertCircle className="h-6 w-6 text-red-500" />;
    }
    
    if (notification.priority === 'critical') {
      return <AlertCircle className="h-6 w-6 text-red-500" />;
    }
    return <Clock className="h-6 w-6 text-orange-500" />;
  };

  // Get notification type label
  const getTypeLabel = (type: string): string => {
    const labels: Record<string, string> = {
      'milestone_survey_late': 'Survey Late',
      'milestone_install_late': 'Install Late',
      'milestone_nem_overdue': 'NEM Overdue',
      'milestone_pto_overdue': 'PTO Overdue',
      'milestone_unresponsive_escalation': 'Unresponsive',
      'arrivy_task_late': 'Task Late',
      'arrivy_task_noshow': 'No-Show',
      'arrivy_task_exception': 'Exception',
      'arrivy_task_cancelled': 'Cancelled',
    };
    return labels[type] || type;
  };

  // Calculate metrics
  const totalAlerts = notificationsData?.notifications.length || 0;
  const unreadCount = notificationsData?.notifications.filter(n => !n.is_read).length || 0;
  const criticalCount = notificationsData?.notifications.filter(n => n.priority === 'critical').length || 0;
  const arrivyCount = notificationsData?.notifications.filter(n => n.source === 'arrivy').length || 0;

  // Group notifications by date
  const groupNotificationsByDate = (notifications: Notification[]) => {
    const now = new Date();
    const today = new Date(now.getFullYear(), now.getMonth(), now.getDate());
    const yesterday = new Date(today);
    yesterday.setDate(yesterday.getDate() - 1);
    const weekAgo = new Date(today);
    weekAgo.setDate(weekAgo.getDate() - 7);

    const groups: Record<string, Notification[]> = {
      'Today': [],
      'Yesterday': [],
      'This Week': [],
      'Older': [],
    };

    notifications.forEach(notification => {
      const notifDate = new Date(notification.created_at);
      if (notifDate >= today) {
        groups['Today'].push(notification);
      } else if (notifDate >= yesterday) {
        groups['Yesterday'].push(notification);
      } else if (notifDate >= weekAgo) {
        groups['This Week'].push(notification);
      } else {
        groups['Older'].push(notification);
      }
    });

    return groups;
  };

  const groupedNotifications = notificationsData ? groupNotificationsByDate(notificationsData.notifications) : {};

  return (
    <div className="container mx-auto px-4 py-8 max-w-7xl">
      {/* Header */}
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-gray-900 mb-2">Alerts & Notifications</h1>
        <p className="text-gray-600">Manage milestone and field operation alerts</p>
      </div>

      {/* Action Buttons */}
      <div className="flex justify-between items-center mb-6">
        <div className="flex space-x-2">
          <Button
            onClick={() => refetch()}
            variant="outline"
            size="sm"
          >
            <RefreshCw className="h-4 w-4 mr-2" />
            Check Now
          </Button>
          <Button
            onClick={handleMarkAllRead}
            variant="outline"
            size="sm"
            disabled={unreadCount === 0 || markAllReadMutation.isLoading}
          >
            <CheckCircle className="h-4 w-4 mr-2" />
            Mark All Read
          </Button>
        </div>
      </div>

      {/* Metrics Cards */}
      <div className="grid grid-cols-1 md:grid-cols-4 gap-4 mb-6">
        <Card>
          <CardContent className="p-6">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-gray-600">Total Alerts</p>
                <p className="text-2xl font-bold">{totalAlerts}</p>
              </div>
              <Bell className="h-8 w-8 text-blue-500" />
            </div>
          </CardContent>
        </Card>
        
        <Card>
          <CardContent className="p-6">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-gray-600">Unread</p>
                <p className="text-2xl font-bold">{unreadCount}</p>
              </div>
              <AlertCircle className="h-8 w-8 text-orange-500" />
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardContent className="p-6">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-gray-600">Critical</p>
                <p className="text-2xl font-bold">{criticalCount}</p>
              </div>
              <AlertCircle className="h-8 w-8 text-red-500" />
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardContent className="p-6">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-gray-600">Field Alerts</p>
                <p className="text-2xl font-bold">{arrivyCount}</p>
              </div>
              <Bell className="h-8 w-8 text-green-500" />
            </div>
          </CardContent>
        </Card>
      </div>

      {/* Filter Panel */}
      <Card className="mb-6">
        <CardContent className="p-4">
          <div className="flex flex-col md:flex-row gap-4">
            {/* Status Filter */}
            <div className="flex space-x-2">
              {(['all', 'unread', 'critical'] as FilterType[]).map((filterType) => (
                <Button
                  key={filterType}
                  variant={filter === filterType ? 'default' : 'outline'}
                  size="sm"
                  onClick={() => setFilter(filterType)}
                  className="capitalize"
                >
                  {filterType}
                </Button>
              ))}
            </div>

            {/* Type Filter */}
            <div className="flex items-center space-x-2">
              <Filter className="h-4 w-4 text-gray-500" />
              <select
                value={selectedType}
                onChange={(e) => setSelectedType(e.target.value as NotificationType)}
                className="text-sm border rounded px-3 py-1.5"
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
                  <option value="arrivy_task_late">Task Late</option>
                  <option value="arrivy_task_noshow">No-Show</option>
                  <option value="arrivy_task_exception">Exception</option>
                  <option value="arrivy_task_cancelled">Cancelled</option>
                </optgroup>
              </select>
            </div>

            {/* Source Filter */}
            <div className="flex items-center space-x-2">
              <select
                value={selectedSource}
                onChange={(e) => setSelectedSource(e.target.value as SourceType)}
                className="text-sm border rounded px-3 py-1.5"
              >
                <option value="all">All Sources</option>
                <option value="system">System</option>
                <option value="arrivy">Arrivy</option>
              </select>
            </div>

            {/* Search */}
            <div className="flex-1 flex items-center space-x-2">
              <Search className="h-4 w-4 text-gray-500" />
              <Input
                placeholder="Search alerts..."
                value={searchQuery}
                onChange={(e) => setSearchQuery(e.target.value)}
                className="flex-1"
              />
            </div>

            {/* Reset */}
            <Button
              onClick={handleResetFilters}
              variant="ghost"
              size="sm"
            >
              Reset
            </Button>
          </div>
        </CardContent>
      </Card>

      {/* Alerts List */}
      {isLoading ? (
        <div className="space-y-4">
          {[...Array(5)].map((_, i) => (
            <Card key={i} className="animate-pulse">
              <CardContent className="p-6">
                <div className="h-20 bg-gray-200 rounded"></div>
              </CardContent>
            </Card>
          ))}
        </div>
      ) : error ? (
        <Card>
          <CardContent className="p-6 text-center">
            <AlertCircle className="h-12 w-12 text-red-500 mx-auto mb-4" />
            <p className="text-red-600">Failed to load alerts. Please try again.</p>
            <Button onClick={() => refetch()} className="mt-4">Retry</Button>
          </CardContent>
        </Card>
      ) : totalAlerts === 0 ? (
        <Card>
          <CardContent className="p-12 text-center">
            <CheckCircle className="h-16 w-16 text-green-500 mx-auto mb-4" />
            <h3 className="text-xl font-semibold mb-2">No alerts to display</h3>
            <p className="text-gray-600">
              {searchQuery || selectedType !== 'all' || selectedSource !== 'all'
                ? "No alerts match your filters. Try adjusting your criteria."
                : "You're all caught up!"}
            </p>
          </CardContent>
        </Card>
      ) : (
        <>
          {/* Grouped Alerts */}
          {Object.entries(groupedNotifications).map(([groupName, notifications]) => {
            if (notifications.length === 0) return null;

            return (
              <div key={groupName} className="mb-6">
                <h2 className="text-lg font-semibold text-gray-700 mb-3">{groupName}</h2>
                <div className="space-y-3">
                  {notifications.map((notification) => (
                    <Card
                      key={notification.id}
                      className={`cursor-pointer hover:shadow-md transition-shadow ${
                        notification.priority === 'critical' ? 'border-l-4 border-red-500' : ''
                      } ${!notification.is_read ? 'bg-blue-50' : ''}`}
                      onClick={() => handleNotificationClick(notification)}
                    >
                      <CardContent className="p-4">
                        <div className="flex items-start space-x-4">
                          {/* Icon */}
                          <div className="flex-shrink-0 mt-1">
                            {getNotificationIcon(notification)}
                          </div>

                          {/* Content */}
                          <div className="flex-1 min-w-0">
                            <div className="flex items-start justify-between mb-2">
                              <h3 className="font-semibold text-gray-900 truncate">
                                {notification.title}
                              </h3>
                              {!notification.is_read && (
                                <div className="ml-2 h-2 w-2 bg-blue-500 rounded-full flex-shrink-0"></div>
                              )}
                            </div>

                            {notification.message && (
                              <p className="text-sm text-gray-600 mb-3 line-clamp-2">
                                {notification.message}
                              </p>
                            )}

                            <div className="flex items-center space-x-2 flex-wrap">
                              <Badge variant={notification.priority === 'critical' ? 'destructive' : 'secondary'}>
                                {notification.priority}
                              </Badge>
                              <Badge variant="outline">
                                {notification.source}
                              </Badge>
                              <Badge variant="outline">
                                {getTypeLabel(notification.type)}
                              </Badge>
                              <span className="text-xs text-gray-500">
                                {formatDistanceToNow(new Date(notification.created_at), { addSuffix: true })}
                              </span>
                            </div>
                          </div>

                          {/* Action Icon */}
                          {notification.action_url && (
                            <ExternalLink className="h-5 w-5 text-gray-400 flex-shrink-0" />
                          )}
                        </div>
                      </CardContent>
                    </Card>
                  ))}
                </div>
              </div>
            );
          })}

          {/* Pagination */}
          {notificationsData?.has_more && (
            <div className="text-center mt-6">
              <Button onClick={handleLoadMore} variant="outline">
                Load More
              </Button>
              <p className="text-sm text-gray-600 mt-2">
                Showing {totalAlerts} alerts
              </p>
            </div>
          )}
        </>
      )}
    </div>
  );
}

