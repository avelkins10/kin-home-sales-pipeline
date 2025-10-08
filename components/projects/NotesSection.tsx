'use client';

import { useState, useEffect } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { Card, CardContent } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Loader2, MessageSquare, Plus, X } from 'lucide-react';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { toast } from 'sonner';
import { NOTE_FIELDS } from '@/lib/constants/noteFieldIds';
import { useProjectUnreadCount } from '@/lib/hooks/useNotifications';

interface NotesSectionProps {
  projectId: string;
}

export function NotesSection({ projectId }: NotesSectionProps) {
  const [isAddingNote, setIsAddingNote] = useState(false);
  const [noteContent, setNoteContent] = useState('');
  const queryClient = useQueryClient();

  // Fetch notes
  const { data: notes, isLoading } = useQuery({
    queryKey: ['project-notes', projectId],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/projects/${projectId}/notes`);
      if (!response.ok) throw new Error('Failed to fetch notes');
      return response.json();
    },
  });

  // Mark all notifications for this project as read when section is viewed
  useEffect(() => {
    async function markNotificationsAsRead() {
      try {
        // Get all unread notifications for this project
        const response = await fetch(
          `${getBaseUrl()}/api/notifications?projectId=${projectId}&unreadOnly=true`
        );

        if (response.ok) {
          const data = await response.json();
          const notifications = data.notifications || [];

          // Mark each unread notification as read
          for (const notification of notifications) {
            await fetch(`${getBaseUrl()}/api/notifications/${notification.id}/read`, {
              method: 'POST',
            });
          }

          // Invalidate notification queries to update counts
          if (notifications.length > 0) {
            queryClient.invalidateQueries({ queryKey: ['notifications'] });
          }
        }
      } catch (error) {
        console.error('Failed to mark notifications as read:', error);
      }
    }

    markNotificationsAsRead();
  }, [projectId, queryClient]);

  // Create note mutation
  const createNoteMutation = useMutation({
    mutationFn: async (content: string) => {
      const response = await fetch(`${getBaseUrl()}/api/projects/${projectId}/notes`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ content }),
      });
      if (!response.ok) throw new Error('Failed to create note');
      return response.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['project-notes', projectId] });
      setNoteContent('');
      setIsAddingNote(false);
      toast.success('Note added successfully');
    },
    onError: () => {
      toast.error('Failed to add note');
    },
  });

  const handleSubmitNote = () => {
    if (!noteContent.trim()) return;
    createNoteMutation.mutate(noteContent.trim());
  };

  const formatDate = (dateValue: any) => {
    if (!dateValue) return 'Unknown date';

    try {
      let date: Date;

      // Handle Quickbase timestamp (milliseconds since epoch)
      if (typeof dateValue === 'number') {
        date = new Date(dateValue);
      }
      // Handle ISO string
      else if (typeof dateValue === 'string') {
        date = new Date(dateValue);
      }
      // Handle Date object
      else if (dateValue instanceof Date) {
        date = dateValue;
      }
      else {
        console.log('[NotesSection] Unknown date format:', dateValue);
        return 'Unknown date';
      }

      // Check if date is valid
      if (isNaN(date.getTime())) {
        console.log('[NotesSection] Invalid date:', dateValue);
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
      console.error('[NotesSection] Error formatting date:', dateValue, error);
      return 'Unknown date';
    }
  };

  const getUserName = (userObj: any) => {
    // Handle null/undefined
    if (!userObj) return 'Unknown User';

    // Handle string (direct username)
    if (typeof userObj === 'string') return userObj;

    // Handle Quickbase user object format
    // Quickbase returns: { email: 'user@example.com', name: 'User Name', id: 12345 }
    if (userObj?.name && userObj.name.trim()) {
      return userObj.name;
    }

    if (userObj?.email) {
      // Extract name from email (before @)
      const emailName = userObj.email.split('@')[0];
      // Convert user.name or username to "User Name" or "Username"
      return emailName
        .split('.')
        .map((part: string) => part.charAt(0).toUpperCase() + part.slice(1))
        .join(' ');
    }

    return 'Unknown User';
  };

  return (
    <Card>
      <CardContent className="p-6">
        {/* Header */}
        <div className="flex items-center justify-between mb-4">
          <div className="flex items-center gap-2">
            <MessageSquare className="h-5 w-5 text-indigo-600" />
            <h3 className="text-lg font-semibold text-slate-900">Project Notes</h3>
            {notes && notes.length > 0 && (
              <span className="px-2 py-0.5 text-xs bg-indigo-100 text-indigo-700 rounded-full">
                {notes.length}
              </span>
            )}
          </div>

          {!isAddingNote && (
            <Button
              onClick={() => setIsAddingNote(true)}
              size="sm"
              className="bg-indigo-600 hover:bg-indigo-700 text-white"
            >
              <Plus className="h-4 w-4 mr-1" />
              Add Note
            </Button>
          )}
        </div>

        {/* Add Note Form */}
        {isAddingNote && (
          <div className="mb-4 p-4 bg-slate-50 rounded-lg border border-slate-200">
            <div className="flex items-start justify-between mb-2">
              <label className="text-sm font-medium text-slate-700">New Note</label>
              <button
                onClick={() => {
                  setIsAddingNote(false);
                  setNoteContent('');
                }}
                className="text-slate-400 hover:text-slate-600"
              >
                <X className="h-4 w-4" />
              </button>
            </div>
            <textarea
              value={noteContent}
              onChange={(e) => setNoteContent(e.target.value)}
              placeholder="Add a note for operations..."
              className="w-full px-3 py-2 border border-slate-300 rounded-md focus:outline-none focus:ring-2 focus:ring-indigo-500 min-h-[100px] resize-y"
              maxLength={5000}
            />
            <div className="flex items-center justify-between mt-2">
              <div className="flex items-center gap-2 text-xs text-slate-500">
                <span className="px-2 py-1 bg-indigo-100 text-indigo-700 rounded">
                  Rep Visible
                </span>
                <span>•</span>
                <span>Category: Sales</span>
              </div>
              <Button
                onClick={handleSubmitNote}
                disabled={!noteContent.trim() || createNoteMutation.isPending}
                size="sm"
                className="bg-indigo-600 hover:bg-indigo-700 text-white"
              >
                {createNoteMutation.isPending ? (
                  <>
                    <Loader2 className="h-4 w-4 mr-1 animate-spin" />
                    Saving...
                  </>
                ) : (
                  'Submit Note'
                )}
              </Button>
            </div>
          </div>
        )}

        {/* Notes List */}
        {isLoading ? (
          <div className="flex items-center justify-center py-8">
            <Loader2 className="h-6 w-6 animate-spin text-indigo-600" />
          </div>
        ) : notes && notes.length > 0 ? (
          <div className="space-y-3">
            {notes.map((note: any) => {
              // Quickbase returns data in format: { "3": { "value": ... }, "6": { "value": ... }, ... }
              const recordId = note[NOTE_FIELDS.RECORD_ID]?.value;
              const content = note[NOTE_FIELDS.NOTE_CONTENT]?.value;
              const category = note[NOTE_FIELDS.CATEGORY]?.value;
              const createdBy = note[NOTE_FIELDS.CREATED_BY]?.value;
              const dateCreated = note[NOTE_FIELDS.DATE_CREATED]?.value;

              // Debug logging - console log the full note object to understand structure
              if (recordId === note[NOTE_FIELDS.RECORD_ID]?.value) {
                console.log('[NotesSection] Processing note:', {
                  recordId,
                  content: content?.substring(0, 50),
                  category,
                  createdBy,
                  dateCreated,
                  createdByType: typeof createdBy,
                  dateCreatedType: typeof dateCreated,
                  rawFields: Object.keys(note).join(', ')
                });
              }

              return (
                <div
                  key={recordId}
                  className="p-4 bg-white border border-slate-200 rounded-lg hover:border-indigo-200 transition-colors"
                >
                  <div className="flex items-start justify-between mb-2">
                    <div className="flex items-center gap-2">
                      <span className="px-2 py-1 text-xs font-medium bg-slate-100 text-slate-700 rounded">
                        {category || 'Sales'}
                      </span>
                      <span className="text-xs text-slate-500">
                        {formatDate(dateCreated)}
                      </span>
                    </div>
                    <span className="text-xs text-indigo-600 font-medium">
                      Rep Visible
                    </span>
                  </div>

                  <p className="text-sm text-slate-700 whitespace-pre-wrap">
                    {content}
                  </p>

                  <div className="mt-2 text-xs text-slate-500">
                    By: {getUserName(createdBy)}
                  </div>
                </div>
              );
            })}
          </div>
        ) : (
          <div className="text-center py-8">
            <MessageSquare className="h-12 w-12 text-slate-300 mx-auto mb-3" />
            <p className="text-sm text-slate-600">No notes yet</p>
            <p className="text-xs text-slate-500 mt-1">
              Click &ldquo;Add Note&rdquo; to start a conversation with operations
            </p>
          </div>
        )}
      </CardContent>
    </Card>
  );
}
