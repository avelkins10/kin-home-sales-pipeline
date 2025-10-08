'use client';

import { useState, useEffect } from 'react';
import { Card, CardContent } from '@/components/ui/card';
import { Tabs, TabsList, TabsTrigger, TabsContent } from '@/components/ui/tabs';
import { MessageSquare, MessagesSquare } from 'lucide-react';
import { NotesSection } from './NotesSection';
import { ProjectMessagesSection } from './ProjectMessagesSection';
import { useNotifications } from '@/lib/hooks/useNotifications';

interface ProjectCommunicationTabsProps {
  projectId: string;
}

export function ProjectCommunicationTabs({ projectId }: ProjectCommunicationTabsProps) {
  const STORAGE_KEY = `project-comm-tab-${projectId}`;

  // Fetch unread notifications for this project
  const { data: notificationsData } = useNotifications({
    projectId: parseInt(projectId),
    unreadOnly: true,
    enablePolling: true,
  });

  const notifications = notificationsData?.notifications || [];

  // Count unread by type
  const unreadNotes = notifications.filter(n => n.type === 'quickbase_note').length;
  const unreadMessages = notifications.filter(n => n.type === 'internal_message').length;

  // Determine initial tab - prioritize unread messages, then check localStorage, then default to notes
  const getInitialTab = (): string => {
    // If there are unread messages, show messages first
    if (unreadMessages > 0) {
      return 'messages';
    }

    // Otherwise check localStorage
    if (typeof window !== 'undefined') {
      const saved = localStorage.getItem(STORAGE_KEY);
      if (saved) return saved;
    }

    // Default to notes
    return 'notes';
  };

  const [activeTab, setActiveTab] = useState(getInitialTab());

  // Save tab selection to localStorage
  const handleTabChange = (value: string) => {
    setActiveTab(value);
    if (typeof window !== 'undefined') {
      localStorage.setItem(STORAGE_KEY, value);
    }
  };

  // Update active tab if unread messages come in while user is on notes tab
  useEffect(() => {
    if (activeTab === 'notes' && unreadMessages > 0) {
      // Only auto-switch if there's new unread messages
      const hasNewMessages = unreadMessages > 0;
      if (hasNewMessages) {
        // Optional: you can remove this if you don't want auto-switching
        // setActiveTab('messages');
      }
    }
  }, [unreadMessages, activeTab]);

  return (
    <Card>
      <CardContent className="p-6">
        <Tabs value={activeTab} onValueChange={handleTabChange} className="w-full">
          <TabsList className="grid w-full grid-cols-2 mb-6">
            <TabsTrigger value="notes" className="flex items-center gap-2">
              <MessageSquare className="h-4 w-4" />
              <span>Project Notes</span>
              {unreadNotes > 0 && (
                <span className="ml-1 px-1.5 py-0.5 text-xs bg-indigo-600 text-white rounded-full">
                  {unreadNotes}
                </span>
              )}
            </TabsTrigger>
            <TabsTrigger value="messages" className="flex items-center gap-2">
              <MessagesSquare className="h-4 w-4" />
              <span>Team Messages</span>
              {unreadMessages > 0 && (
                <span className="ml-1 px-1.5 py-0.5 text-xs bg-blue-600 text-white rounded-full">
                  {unreadMessages}
                </span>
              )}
            </TabsTrigger>
          </TabsList>

          <TabsContent value="notes" className="mt-0">
            <NotesSection projectId={projectId} wrapped={false} />
          </TabsContent>

          <TabsContent value="messages" className="mt-0">
            <ProjectMessagesSection projectId={projectId} wrapped={false} />
          </TabsContent>
        </Tabs>
      </CardContent>
    </Card>
  );
}
