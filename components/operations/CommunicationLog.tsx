'use client';

import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { PCConversationItem } from '@/lib/types/operations';
import { Phone, MessageSquare, Mail, FileText, Clock } from 'lucide-react';

interface CommunicationLogProps {
  conversations: PCConversationItem[];
  onCallCustomer?: (projectId: string, customerPhone: string, customerName: string) => void;
  onSendSms?: (projectId: string, customerPhone: string, customerName: string) => void;
}

export function CommunicationLog({
  conversations,
  onCallCustomer,
  onSendSms
}: CommunicationLogProps) {
  // Helper to extract value from QuickBase wrapped objects
  const extractValue = (field: any): string => {
    if (typeof field === 'object' && field?.value) {
      return String(field.value);
    }
    return String(field || '');
  };

  const formatRelativeTime = (date: string) => {
    const now = new Date();
    const messageDate = new Date(date);
    const diffInHours = (now.getTime() - messageDate.getTime()) / (1000 * 60 * 60);

    if (diffInHours < 1) return 'Just now';
    if (diffInHours < 24) return `${Math.floor(diffInHours)}h ago`;
    if (diffInHours < 48) return 'Yesterday';
    if (diffInHours < 168) return `${Math.floor(diffInHours / 24)}d ago`;

    return messageDate.toLocaleDateString();
  };

  const getCommunicationIcon = (type: string) => {
    switch (type) {
      case 'sms': return <MessageSquare className="w-4 h-4" />;
      case 'call': return <Phone className="w-4 h-4" />;
      case 'email': return <Mail className="w-4 h-4" />;
      default: return <FileText className="w-4 h-4" />;
    }
  };

  const getCommunicationColor = (type: string) => {
    switch (type) {
      case 'sms': return 'text-blue-600';
      case 'call': return 'text-green-600';
      case 'email': return 'text-purple-600';
      default: return 'text-gray-600';
    }
  };

  const truncateContent = (content: any, maxLines: number = 3) => {
    // Extract value from wrapped object if needed
    const contentStr = extractValue(content);
    const lines = contentStr.split('\n');
    if (lines.length <= maxLines) return contentStr;

    return lines.slice(0, maxLines).join('\n') + '...';
  };

  const groupConversationsByDate = (conversations: PCConversationItem[]) => {
    const groups: { [key: string]: PCConversationItem[] } = {};
    
    conversations.forEach(conversation => {
      const date = new Date(conversation.date);
      const today = new Date();
      const yesterday = new Date(today);
      yesterday.setDate(yesterday.getDate() - 1);
      
      let groupKey: string;
      if (date.toDateString() === today.toDateString()) {
        groupKey = 'Today';
      } else if (date.toDateString() === yesterday.toDateString()) {
        groupKey = 'Yesterday';
      } else if (date.getTime() > today.getTime() - 7 * 24 * 60 * 60 * 1000) {
        groupKey = 'This Week';
      } else {
        groupKey = 'Older';
      }
      
      if (!groups[groupKey]) {
        groups[groupKey] = [];
      }
      groups[groupKey].push(conversation);
    });
    
    return groups;
  };

  const groupedConversations = groupConversationsByDate(conversations);

  if (conversations.length === 0) {
    return (
      <Card>
        <CardHeader>
          <CardTitle>Communication History</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-center py-8 text-gray-500">
            <FileText className="w-12 h-12 mx-auto mb-4 text-gray-300" />
            <p>No recent communications</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  return (
    <Card>
      <CardHeader>
        <CardTitle>Communication History</CardTitle>
      </CardHeader>
      <CardContent>
        <div className="max-h-96 overflow-y-auto">
          {Object.entries(groupedConversations).map(([groupName, groupConversations]) => (
            <div key={groupName} className="mb-6">
              <h3 className="text-sm font-semibold text-gray-700 mb-3 sticky top-0 bg-white py-1">
                {groupName}
              </h3>
              
              <div className="space-y-4">
                {groupConversations.map((conversation) => (
                  <div key={conversation.recordId} className="flex gap-3">
                    {/* Timeline icon */}
                    <div className={`flex-shrink-0 w-8 h-8 rounded-full flex items-center justify-center ${getCommunicationColor(conversation.communicationType)} bg-gray-100`}>
                      {getCommunicationIcon(conversation.communicationType)}
                    </div>
                    
                    {/* Content */}
                    <div className="flex-1 min-w-0">
                      <div className="bg-gray-50 rounded-lg p-3">
                        <div className="flex items-center justify-between mb-2">
                          <div className="flex items-center gap-2">
                            <span className="text-sm font-medium text-gray-900">
                              {extractValue(conversation.noteBy)}
                            </span>
                            <Badge
                              variant="outline"
                              className={`text-xs ${getCommunicationColor(conversation.communicationType)}`}
                            >
                              {conversation.communicationType.toUpperCase()}
                            </Badge>
                            {conversation.isNemBlocker && (
                              <Badge variant="destructive" className="text-xs">
                                NEM Blocked
                              </Badge>
                            )}
                          </div>
                          <div className="flex items-center gap-1 text-xs text-gray-500">
                            <Clock className="w-3 h-3" />
                            {formatRelativeTime(conversation.date)}
                          </div>
                        </div>

                        <div className="mb-2">
                          <span className="text-sm font-medium text-gray-700">
                            {extractValue(conversation.projectId)}
                          </span>
                          <span className="text-sm text-gray-600 ml-2">
                            {extractValue(conversation.customerName)}
                          </span>
                        </div>
                        
                        <div className="text-sm text-gray-700 whitespace-pre-wrap">
                          {truncateContent(conversation.content)}
                        </div>
                        
                        {/* Call duration or SMS status */}
                        {conversation.duration && (
                          <div className="text-xs text-gray-500 mt-1">
                            Duration: {Math.floor(conversation.duration / 60)}m {conversation.duration % 60}s
                          </div>
                        )}
                        
                        {conversation.status && (
                          <div className="text-xs text-gray-500 mt-1">
                            Status: {conversation.status}
                          </div>
                        )}
                        
                        {/* Quick actions */}
                        {(onCallCustomer || onSendSms) && conversation.customerPhone && (
                          <div className="flex gap-2 mt-2">
                            {onCallCustomer && (
                              <Button
                                size="sm"
                                variant="outline"
                                onClick={() => onCallCustomer(conversation.projectId, conversation.customerPhone, conversation.customerName)}
                                className="h-6 px-2 text-xs"
                              >
                                <Phone className="w-3 h-3 mr-1" />
                                Call
                              </Button>
                            )}
                            {onSendSms && (
                              <Button
                                size="sm"
                                variant="outline"
                                onClick={() => onSendSms(conversation.projectId, conversation.customerPhone, conversation.customerName)}
                                className="h-6 px-2 text-xs"
                              >
                                <MessageSquare className="w-3 h-3 mr-1" />
                                SMS
                              </Button>
                            )}
                          </div>
                        )}
                      </div>
                    </div>
                  </div>
                ))}
              </div>
            </div>
          ))}
        </div>
      </CardContent>
    </Card>
  );
}
