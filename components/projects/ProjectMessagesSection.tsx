'use client';

import { useState, useEffect } from 'react';
import { useSession } from 'next-auth/react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { Card, CardContent } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Loader2, MessagesSquare, Plus, X, Send } from 'lucide-react';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { toast } from 'sonner';
import { MentionTextarea } from '@/components/ui/mention-textarea';
import { MessageText } from '@/components/ui/message-text';
import type { ProjectMessage } from '@/lib/types/message';

interface ProjectMessagesSectionProps {
  projectId: string;
  wrapped?: boolean; // Whether to wrap in Card component (default true)
}

export function ProjectMessagesSection({ projectId, wrapped = true }: ProjectMessagesSectionProps) {
  const [isAddingMessage, setIsAddingMessage] = useState(false);
  const [messageContent, setMessageContent] = useState('');
  const queryClient = useQueryClient();
  const { data: session } = useSession();

  // Fetch messages
  const { data: messagesData, isLoading } = useQuery({
    queryKey: ['project-messages', projectId],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/projects/${projectId}/messages`);
      if (!response.ok) throw new Error('Failed to fetch messages');
      return response.json();
    },
  });

  const messages = messagesData?.messages || [];

  // Mark message notifications as read when section is viewed
  useEffect(() => {
    async function markNotificationsAsRead() {
      try {
        const response = await fetch(
          `${getBaseUrl()}/api/notifications?projectId=${projectId}&unreadOnly=true`
        );

        if (response.ok) {
          const data = await response.json();
          const notifications = data.notifications || [];

          // Filter for internal_message notifications
          const messageNotifications = notifications.filter(
            (n: any) => n.type === 'internal_message'
          );

          // Mark each unread message notification as read
          for (const notification of messageNotifications) {
            await fetch(`${getBaseUrl()}/api/notifications/${notification.id}/read`, {
              method: 'POST',
            });
          }

          // Invalidate notification queries to update counts
          if (messageNotifications.length > 0) {
            queryClient.invalidateQueries({ queryKey: ['notifications'] });
          }
        }
      } catch (error) {
        console.error('Failed to mark message notifications as read:', error);
      }
    }

    markNotificationsAsRead();
  }, [projectId, queryClient]);

  // Create message mutation
  const createMessageMutation = useMutation({
    mutationFn: async (content: string) => {
      const response = await fetch(`${getBaseUrl()}/api/projects/${projectId}/messages`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ message: content }),
      });
      if (!response.ok) throw new Error('Failed to send message');
      return response.json();
    },
    onSuccess: (data) => {
      queryClient.invalidateQueries({ queryKey: ['project-messages', projectId] });
      setMessageContent('');
      setIsAddingMessage(false);

      const notificationCount = data.notifications_sent || 0;
      if (notificationCount > 0) {
        toast.success(`Message sent to ${notificationCount} team ${notificationCount === 1 ? 'member' : 'members'}`);
      } else {
        toast.success('Message sent');
      }
    },
    onError: () => {
      toast.error('Failed to send message');
    },
  });

  const handleSubmitMessage = () => {
    if (!messageContent.trim()) return;
    createMessageMutation.mutate(messageContent.trim());
  };

  const formatDate = (dateValue: any) => {
    if (!dateValue) return 'Unknown date';

    try {
      const date = new Date(dateValue);

      // Check if date is valid
      if (isNaN(date.getTime())) {
        return 'Unknown date';
      }

      const now = new Date();
      const diffMs = now.getTime() - date.getTime();
      const diffMins = Math.floor(diffMs / 60000);
      const diffHours = Math.floor(diffMs / 3600000);
      const diffDays = Math.floor(diffMs / 86400000);

      if (diffMins < 1) return 'Just now';
      if (diffMins < 60) return `${diffMins}m ago`;
      if (diffHours < 24) return `${diffHours}h ago`;
      if (diffDays < 7) return `${diffDays}d ago`;

      // For older dates, show formatted date
      return date.toLocaleDateString('en-US', {
        month: 'short',
        day: 'numeric',
        year: date.getFullYear() !== now.getFullYear() ? 'numeric' : undefined
      });
    } catch (error) {
      console.error('Error formatting date:', dateValue, error);
      return 'Unknown date';
    }
  };

  const getRoleBadgeColor = (role: string) => {
    switch (role) {
      case 'super_admin':
      case 'regional':
        return 'bg-purple-100 text-purple-700';
      case 'office_leader':
        return 'bg-blue-100 text-blue-700';
      case 'coordinator':
        return 'bg-green-100 text-green-700';
      case 'closer':
        return 'bg-indigo-100 text-indigo-700';
      case 'setter':
        return 'bg-orange-100 text-orange-700';
      default:
        return 'bg-slate-100 text-slate-700';
    }
  };

  const getRoleLabel = (role: string) => {
    switch (role) {
      case 'super_admin':
        return 'Admin';
      case 'regional':
        return 'Regional';
      case 'office_leader':
        return 'Office Leader';
      case 'coordinator':
        return 'Coordinator';
      case 'closer':
        return 'Closer';
      case 'setter':
        return 'Setter';
      default:
        return role;
    }
  };

  const isOwnMessage = (message: ProjectMessage) => {
    const userEmail = session?.user?.email;
    return message.sender_id === userEmail;
  };

  const content = (
    <>
      {/* Header */}
      <div className="flex items-center justify-between mb-4">
        <div className="flex items-center gap-2">
          <MessagesSquare className="h-5 w-5 text-blue-600" />
          <h3 className="text-lg font-semibold text-slate-900">Team Messages</h3>
          {messages.length > 0 && (
            <span className="px-2 py-0.5 text-xs bg-blue-100 text-blue-700 rounded-full">
              {messages.length}
            </span>
          )}
        </div>

        {!isAddingMessage && (
          <Button
            onClick={() => setIsAddingMessage(true)}
            size="sm"
            className="bg-blue-600 hover:bg-blue-700 text-white"
          >
            <Plus className="h-4 w-4 mr-1" />
            New Message
          </Button>
        )}
      </div>

        {/* Add Message Form */}
        {isAddingMessage && (
          <div className="mb-4 p-4 bg-blue-50 rounded-lg border border-blue-200">
            <div className="flex items-start justify-between mb-2">
              <label className="text-sm font-medium text-slate-700">New Message</label>
              <button
                onClick={() => {
                  setIsAddingMessage(false);
                  setMessageContent('');
                }}
                className="text-slate-400 hover:text-slate-600"
              >
                <X className="h-4 w-4" />
              </button>
            </div>
            <MentionTextarea
              value={messageContent}
              onChange={setMessageContent}
              placeholder="Message the team about this project... Type @ to mention someone"
              maxLength={10000}
              disabled={createMessageMutation.isPending}
              className="w-full px-3 py-2 border border-slate-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 min-h-[100px] resize-y"
            />
            <div className="flex items-center justify-between mt-2">
              <div className="text-xs text-slate-500">
                Visible to all team members assigned to this project
              </div>
              <Button
                onClick={handleSubmitMessage}
                disabled={!messageContent.trim() || createMessageMutation.isPending}
                size="sm"
                className="bg-blue-600 hover:bg-blue-700 text-white"
              >
                {createMessageMutation.isPending ? (
                  <>
                    <Loader2 className="h-4 w-4 mr-1 animate-spin" />
                    Sending...
                  </>
                ) : (
                  <>
                    <Send className="h-4 w-4 mr-1" />
                    Send Message
                  </>
                )}
              </Button>
            </div>
          </div>
        )}

        {/* Messages List */}
        {isLoading ? (
          <div className="flex items-center justify-center py-8">
            <Loader2 className="h-6 w-6 animate-spin text-blue-600" />
          </div>
        ) : messages.length > 0 ? (
          <div className="space-y-3">
            {messages.map((message: ProjectMessage) => {
              const isOwn = isOwnMessage(message);

              return (
                <div
                  key={message.id}
                  className={`p-4 border rounded-lg transition-colors ${
                    isOwn
                      ? 'bg-blue-50 border-blue-200'
                      : 'bg-white border-slate-200 hover:border-blue-200'
                  }`}
                >
                  {/* Message Header */}
                  <div className="flex items-start justify-between mb-2">
                    <div className="flex items-center gap-2">
                      <span className="text-sm font-medium text-slate-900">
                        {message.sender_name}
                        {isOwn && <span className="text-slate-500 ml-1">(you)</span>}
                      </span>
                      <span className={`px-2 py-0.5 text-xs font-medium rounded ${getRoleBadgeColor(message.sender_role)}`}>
                        {getRoleLabel(message.sender_role)}
                      </span>
                    </div>
                    <span className="text-xs text-slate-500">
                      {formatDate(message.created_at)}
                    </span>
                  </div>

                  {/* Message Content */}
                  <MessageText
                    text={message.message}
                    className="text-sm text-slate-700 whitespace-pre-wrap"
                  />

                  {/* System Message Indicator */}
                  {message.is_system_message && (
                    <div className="mt-2">
                      <span className="inline-flex items-center px-2 py-0.5 text-xs bg-slate-100 text-slate-600 rounded">
                        System Message
                      </span>
                    </div>
                  )}
                </div>
              );
            })}
          </div>
        ) : (
          <div className="text-center py-8">
            <MessagesSquare className="h-12 w-12 text-slate-300 mx-auto mb-3" />
            <p className="text-sm text-slate-600">No messages yet</p>
            <p className="text-xs text-slate-500 mt-1">
              Start a conversation with your team about this project
            </p>
          </div>
        )}
    </>
  );

  if (wrapped) {
    return (
      <Card id="messages">
        <CardContent className="p-6">
          {content}
        </CardContent>
      </Card>
    );
  }

  return content;
}
