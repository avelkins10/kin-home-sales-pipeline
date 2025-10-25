'use client';

import { useState, useEffect, useRef } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { Textarea } from '@/components/ui/textarea';
import { Avatar, AvatarFallback } from '@/components/ui/avatar';
import { Tooltip, TooltipContent, TooltipProvider, TooltipTrigger } from '@/components/ui/tooltip';
import { MessageSquare, Send, X, Paperclip, AtSign } from 'lucide-react';
import { toast } from 'sonner';
import { PCMessage } from '@/lib/types/operations';
import { getInitials } from '@/lib/utils/avatar';

interface MessageThreadProps {
  projectId: string;
  recordId: number | null;
  taskId: number | null;
  currentUserEmail: string;
  currentUserRole: 'pc' | 'rep';
}

export function MessageThread({
  projectId,
  recordId,
  taskId,
  currentUserEmail,
  currentUserRole
}: MessageThreadProps) {
  const [newMessage, setNewMessage] = useState('');
  const [mentions, setMentions] = useState<string[]>([]);
  const [isSending, setIsSending] = useState(false);
  const [showMentionDropdown, setShowMentionDropdown] = useState(false);
  const [mentionQuery, setMentionQuery] = useState('');
  const [cursorPosition, setCursorPosition] = useState(0);
  const messagesEndRef = useRef<HTMLDivElement>(null);
  const textareaRef = useRef<HTMLTextAreaElement>(null);
  const queryClient = useQueryClient();

  // Mock team members for mention autocomplete
  const teamMembers = [
    { name: 'John Smith', email: 'john@example.com', role: 'Sales Rep' },
    { name: 'Jane Doe', email: 'jane@example.com', role: 'PC Coordinator' },
    { name: 'Mike Johnson', email: 'mike@example.com', role: 'Sales Rep' },
    { name: 'Sarah Wilson', email: 'sarah@example.com', role: 'PC Manager' }
  ];

  // Data fetching
  const { data: messages = [], isLoading } = useQuery({
    queryKey: ['pc-messages', projectId, recordId, taskId],
    queryFn: async () => {
      const params = new URLSearchParams();
      if (recordId) {
        params.append('recordId', recordId.toString());
      } else {
        params.append('projectId', projectId);
      }
      if (taskId) params.append('taskId', taskId.toString());
      
      const response = await fetch(`/api/operations/messages?${params}`);
      if (!response.ok) throw new Error('Failed to fetch messages');
      
      const data = await response.json();
      return data.messages as PCMessage[];
    },
    refetchInterval: 10000, // 10 seconds
    enabled: !!projectId && !!recordId
  });

  // Send message mutation
  const sendMessageMutation = useMutation({
    mutationFn: async (content: string) => {
      const response = await fetch('/api/operations/messages', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          projectId,
          recordId,
          content,
          taskId,
          mentions
        })
      });

      if (!response.ok) {
        const error = await response.json();
        throw new Error(error.message || 'Failed to send message');
      }

      return response.json();
    },
    onSuccess: () => {
      setNewMessage('');
      setMentions([]);
      queryClient.invalidateQueries({ queryKey: ['pc-messages', projectId, recordId, taskId] });
      scrollToBottom();
      toast.success('Message sent successfully');
    },
    onError: (error: Error) => {
      toast.error(error.message);
    }
  });

  // Mark messages as read mutation
  const markAsReadMutation = useMutation({
    mutationFn: async (messageIds: number[]) => {
      const response = await fetch('/api/operations/messages', {
        method: 'PATCH',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ messageIds })
      });

      if (!response.ok) {
        const error = await response.json();
        throw new Error(error.message || 'Failed to mark messages as read');
      }

      return response.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['pc-messages', projectId, recordId, taskId] });
    }
  });

  // Helper functions
  const formatRelativeTime = (date: string): string => {
    const now = new Date();
    const messageDate = new Date(date);
    const diffInHours = (now.getTime() - messageDate.getTime()) / (1000 * 60 * 60);
    
    if (diffInHours < 1) return 'Just now';
    if (diffInHours < 24) return `${Math.floor(diffInHours)}h ago`;
    if (diffInHours < 48) return 'Yesterday';
    if (diffInHours < 168) return `${Math.floor(diffInHours / 24)}d ago`;
    
    return messageDate.toLocaleDateString();
  };

  const extractMentions = (content: string): string[] => {
    const mentionRegex = /@(\w+)/g;
    const mentions: string[] = [];
    let match;
    
    while ((match = mentionRegex.exec(content)) !== null) {
      mentions.push(match[1]);
    }
    
    return [...new Set(mentions)]; // Remove duplicates
  };

  const renderMessageContent = (content: string, mentions: string[]) => {
    if (mentions.length === 0) {
      return <span>{content}</span>;
    }
    
    // Split content by mentions and create React elements
    let parts = [content];
    mentions.forEach(mention => {
      const newParts: (string | JSX.Element)[] = [];
      parts.forEach(part => {
        if (typeof part === 'string') {
          const regex = new RegExp(`(@${mention})`, 'g');
          const splitParts = part.split(regex);
          splitParts.forEach((splitPart, index) => {
            if (splitPart === `@${mention}`) {
              newParts.push(
                <span key={`${mention}-${index}`} className="text-blue-600 font-medium">
                  {splitPart}
                </span>
              );
            } else if (splitPart) {
              newParts.push(splitPart);
            }
          });
        } else {
          newParts.push(part);
        }
      });
      parts = newParts;
    });
    
    return <span>{parts}</span>;
  };

  const scrollToBottom = () => {
    messagesEndRef.current?.scrollIntoView({ behavior: 'smooth' });
  };

  const handleSendMessage = async () => {
    if (!newMessage.trim() || isSending) return;

    setIsSending(true);
    const extractedMentions = extractMentions(newMessage);
    setMentions(extractedMentions);
    
    try {
      await sendMessageMutation.mutateAsync(newMessage);
    } finally {
      setIsSending(false);
    }
  };

  const handleKeyPress = (e: React.KeyboardEvent) => {
    if (e.key === 'Enter' && !e.shiftKey) {
      e.preventDefault();
      handleSendMessage();
    }
  };

  const handleTextareaChange = (e: React.ChangeEvent<HTMLTextAreaElement>) => {
    const value = e.target.value;
    const cursorPos = e.target.selectionStart;
    
    setNewMessage(value);
    setCursorPosition(cursorPos);
    
    // Check for @ mention
    const textBeforeCursor = value.substring(0, cursorPos);
    const mentionMatch = textBeforeCursor.match(/@(\w*)$/);
    
    if (mentionMatch) {
      setMentionQuery(mentionMatch[1]);
      setShowMentionDropdown(true);
    } else {
      setShowMentionDropdown(false);
    }
  };

  const handleMentionSelect = (member: { name: string; email: string }) => {
    const textBeforeCursor = newMessage.substring(0, cursorPosition);
    const textAfterCursor = newMessage.substring(cursorPosition);
    const beforeMention = textBeforeCursor.replace(/@\w*$/, '');
    
    const newText = `${beforeMention}@${member.name} ${textAfterCursor}`;
    setNewMessage(newText);
    setShowMentionDropdown(false);
    setMentionQuery('');
    
    // Focus back to textarea
    setTimeout(() => {
      textareaRef.current?.focus();
      const newCursorPos = beforeMention.length + member.name.length + 2; // +2 for @ and space
      textareaRef.current?.setSelectionRange(newCursorPos, newCursorPos);
    }, 0);
  };

  const filteredTeamMembers = teamMembers.filter(member =>
    member.name.toLowerCase().includes(mentionQuery.toLowerCase()) ||
    member.email.toLowerCase().includes(mentionQuery.toLowerCase())
  );

  // Auto-scroll to bottom when new messages arrive
  useEffect(() => {
    scrollToBottom();
  }, [messages]);

  // Mark unread messages as read when component is focused or messages change
  useEffect(() => {
    if (messages.length > 0) {
      const unreadMessages = messages.filter(
        message => !message.isRead && message.sentBy !== currentUserEmail
      );
      
      if (unreadMessages.length > 0) {
        const messageIds = unreadMessages.map(message => message.recordId);
        markAsReadMutation.mutate(messageIds);
      }
    }
  }, [messages, currentUserEmail, markAsReadMutation]);

  // Group messages by date
  const groupedMessages = messages.reduce((groups, message) => {
    const date = new Date(message.date).toDateString();
    if (!groups[date]) groups[date] = [];
    groups[date].push(message);
    return groups;
  }, {} as Record<string, PCMessage[]>);

  if (isLoading) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <MessageSquare className="h-5 w-5" />
            Messages
          </CardTitle>
        </CardHeader>
        <CardContent>
          <div className="flex items-center justify-center py-8">
            <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600"></div>
          </div>
        </CardContent>
      </Card>
    );
  }

  return (
    <Card>
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <MessageSquare className="h-5 w-5" />
          Messages
          {taskId && (
            <Badge variant="secondary" className="ml-2">
              Task #{taskId}
            </Badge>
          )}
        </CardTitle>
      </CardHeader>
      <CardContent className="p-0">
        {/* Messages List */}
        <div className="h-96 overflow-y-auto p-4 space-y-4">
          {Object.keys(groupedMessages).length === 0 ? (
            <div className="flex flex-col items-center justify-center py-8 text-gray-500">
              <MessageSquare className="h-12 w-12 mb-4" />
              <p className="text-lg font-medium">No messages yet</p>
              <p className="text-sm">Start the conversation!</p>
            </div>
          ) : (
            Object.entries(groupedMessages).map(([date, dateMessages]) => (
              <div key={date}>
                <div className="sticky top-0 bg-white py-2 text-center">
                  <Badge variant="outline" className="text-xs">
                    {date === new Date().toDateString() ? 'Today' : 
                     date === new Date(Date.now() - 86400000).toDateString() ? 'Yesterday' : 
                     date}
                  </Badge>
                </div>
                <div className="space-y-3">
                  {dateMessages.map((message) => (
                    <div
                      key={message.recordId}
                      className={`flex gap-3 ${message.sentBy === currentUserEmail ? 'justify-end' : 'justify-start'}`}
                    >
                      {message.sentBy !== currentUserEmail && (
                        <Avatar className="h-8 w-8">
                          <AvatarFallback className="text-xs">
                            {getInitials(message.sentBy)}
                          </AvatarFallback>
                        </Avatar>
                      )}
                      
                      <div className={`max-w-xs lg:max-w-md ${message.sentBy === currentUserEmail ? 'order-first' : ''}`}>
                        <div className="flex items-center gap-2 mb-1">
                          <span className="text-sm font-medium">
                            {message.sentBy === currentUserEmail ? 'You' : message.sentBy}
                          </span>
                          <Badge 
                            variant={message.sentByRole === 'pc' ? 'default' : 'secondary'}
                            className="text-xs"
                          >
                            {message.sentByRole === 'pc' ? 'PC' : 'Rep'}
                          </Badge>
                          <span className="text-xs text-gray-500">
                            {formatRelativeTime(message.date)}
                          </span>
                        </div>
                        
                        <div
                          className={`p-3 rounded-lg ${
                            message.sentBy === currentUserEmail
                              ? 'bg-blue-600 text-white'
                              : 'bg-gray-100 text-gray-900'
                          }`}
                        >
                          <div>
                            {renderMessageContent(message.content, message.mentions)}
                          </div>
                          {!message.isRead && message.sentBy !== currentUserEmail && (
                            <div className="flex items-center gap-1 mt-2">
                              <div className="w-2 h-2 bg-blue-500 rounded-full" />
                              <span className="text-xs text-blue-600 font-medium">Unread</span>
                            </div>
                          )}
                        </div>
                      </div>
                      
                      {message.sentBy === currentUserEmail && (
                        <Avatar className="h-8 w-8">
                          <AvatarFallback className="text-xs">
                            {getInitials(currentUserEmail)}
                          </AvatarFallback>
                        </Avatar>
                      )}
                    </div>
                  ))}
                </div>
              </div>
            ))
          )}
          <div ref={messagesEndRef} />
        </div>

        {/* Message Input */}
        <div className="border-t p-4 relative">
          <div className="flex gap-2">
            <div className="flex-1 relative">
              <Textarea
                ref={textareaRef}
                value={newMessage}
                onChange={handleTextareaChange}
                onKeyPress={handleKeyPress}
                placeholder="Type your message... (use @username to mention someone)"
                className="min-h-[60px] max-h-32 resize-none pr-10"
                maxLength={5000}
              />
              
              {/* Mention Autocomplete Dropdown */}
              {showMentionDropdown && (
                <div className="absolute bottom-full left-0 right-0 mb-2 bg-white border border-gray-200 rounded-lg shadow-lg z-10 max-h-48 overflow-y-auto">
                  {filteredTeamMembers.length > 0 ? (
                    filteredTeamMembers.map((member) => (
                      <button
                        key={member.email}
                        onClick={() => handleMentionSelect(member)}
                        className="w-full px-3 py-2 text-left hover:bg-gray-50 flex items-center gap-2"
                      >
                        <Avatar className="h-6 w-6">
                          <AvatarFallback className="text-xs">
                            {getInitials(member.name)}
                          </AvatarFallback>
                        </Avatar>
                        <div>
                          <div className="font-medium text-sm">{member.name}</div>
                          <div className="text-xs text-gray-500">{member.email}</div>
                        </div>
                      </button>
                    ))
                  ) : (
                    <div className="px-3 py-2 text-sm text-gray-500">No team members found</div>
                  )}
                </div>
              )}
            </div>
            
            <div className="flex flex-col gap-2">
              <TooltipProvider>
                <Tooltip>
                  <TooltipTrigger asChild>
                    <Button
                      variant="outline"
                      size="sm"
                      disabled
                      className="h-8 w-8 p-0"
                    >
                      <Paperclip className="h-4 w-4" />
                    </Button>
                  </TooltipTrigger>
                  <TooltipContent>
                    <p>Attachments coming soon</p>
                  </TooltipContent>
                </Tooltip>
              </TooltipProvider>
              
              <Button
                onClick={handleSendMessage}
                disabled={!newMessage.trim() || isSending}
                size="sm"
                className="h-8 w-8 p-0"
              >
                <Send className="h-4 w-4" />
              </Button>
            </div>
          </div>
          <div className="flex justify-between items-center mt-2 text-xs text-gray-500">
            <span>{newMessage.length}/5000 characters</span>
            <div className="flex gap-2">
              <Button
                variant="ghost"
                size="sm"
                onClick={() => setNewMessage('Will do')}
                className="text-xs h-6"
              >
                Will do
              </Button>
              <Button
                variant="ghost"
                size="sm"
                onClick={() => setNewMessage('Need more info')}
                className="text-xs h-6"
              >
                Need more info
              </Button>
              <Button
                variant="ghost"
                size="sm"
                onClick={() => setNewMessage('Completed')}
                className="text-xs h-6"
              >
                Completed
              </Button>
            </div>
          </div>
        </div>
      </CardContent>
    </Card>
  );
}
