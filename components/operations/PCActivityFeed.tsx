'use client';

import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Skeleton } from '@/components/ui/skeleton';
import { 
  MessageSquare, 
  AlertCircle, 
  Clock, 
  User,
  Calendar
} from 'lucide-react';
import type { PCActivityFeedItem } from '@/lib/types/operations';

interface PCActivityFeedProps {
  activityFeed: PCActivityFeedItem[];
}

function PCActivityFeedSkeleton() {
  return (
    <Card>
      <CardHeader>
        <CardTitle>Recent Activity</CardTitle>
        <p className="text-sm text-gray-600">Last 7 days</p>
      </CardHeader>
      <CardContent>
        <div className="space-y-4">
          {Array.from({ length: 5 }).map((_, i) => (
            <div key={i} className="flex items-start space-x-3">
              <Skeleton className="h-8 w-8 rounded-full" />
              <div className="flex-1 space-y-2">
                <div className="flex items-center space-x-2">
                  <Skeleton className="h-4 w-24" />
                  <Skeleton className="h-4 w-16" />
                </div>
                <Skeleton className="h-3 w-32" />
                <Skeleton className="h-4 w-full" />
                <Skeleton className="h-4 w-3/4" />
              </div>
            </div>
          ))}
        </div>
      </CardContent>
    </Card>
  );
}

export function PCActivityFeed({ activityFeed }: PCActivityFeedProps) {
  const formatRelativeTime = (dateString: string) => {
    const date = new Date(dateString);
    if (isNaN(date.getTime())) return 'Invalid date';
    
    const now = new Date();
    const diffTime = now.getTime() - date.getTime();
    const diffMinutes = Math.floor(diffTime / (1000 * 60));
    const diffHours = Math.floor(diffTime / (1000 * 60 * 60));
    const diffDays = Math.floor(diffTime / (1000 * 60 * 60 * 24));

    if (diffMinutes < 1) return 'Just now';
    if (diffMinutes < 60) return `${diffMinutes}m ago`;
    if (diffHours < 24) return `${diffHours}h ago`;
    if (diffDays === 1) return 'Yesterday';
    if (diffDays < 7) return `${diffDays}d ago`;
    
    return date.toLocaleDateString();
  };

  const truncateNote = (note: string, maxLength: number = 120) => {
    if (note.length <= maxLength) return note;
    return note.substring(0, maxLength) + '...';
  };

  if (activityFeed.length === 0) {
    return (
      <Card>
        <CardHeader>
          <CardTitle>Recent Activity</CardTitle>
          <p className="text-sm text-gray-600">Last 7 days</p>
        </CardHeader>
        <CardContent>
          <div className="text-center py-8 text-gray-500">
            <MessageSquare className="h-12 w-12 mx-auto mb-4 text-gray-400" />
            <p>No recent activity</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  return (
    <Card>
      <CardHeader>
        <CardTitle>Recent Activity</CardTitle>
        <p className="text-sm text-gray-600">Last 7 days</p>
      </CardHeader>
      <CardContent>
        <div className="max-h-96 overflow-y-auto space-y-4">
          {activityFeed.map((item, index) => (
            <div key={item.recordId} className="flex items-start space-x-3">
              {/* Timeline indicator */}
              <div className="flex flex-col items-center">
                <div className={`p-2 rounded-full ${
                  item.isNemBlocker 
                    ? 'bg-red-100 text-red-600' 
                    : 'bg-blue-100 text-blue-600'
                }`}>
                  {item.isNemBlocker ? (
                    <AlertCircle className="h-4 w-4" />
                  ) : (
                    <MessageSquare className="h-4 w-4" />
                  )}
                </div>
                {index < activityFeed.length - 1 && (
                  <div className="w-0.5 h-8 bg-gray-200 mt-2" />
                )}
              </div>
              
              {/* Content */}
              <div className="flex-1 min-w-0">
                <div className="flex items-center space-x-2 mb-1">
                  <span className="text-sm font-medium text-gray-900">
                    {item.noteBy}
                  </span>
                  <span className="text-xs text-gray-500">•</span>
                  <span className="text-xs text-gray-500">
                    {formatRelativeTime(item.date)}
                  </span>
                  {item.isNemBlocker && (
                    <Badge variant="destructive" className="text-xs">
                      NEM Blocked
                    </Badge>
                  )}
                </div>
                
                <div className="flex items-center space-x-2 mb-2">
                  <User className="h-3 w-3 text-gray-400" />
                  <span className="text-sm font-medium text-gray-700">
                    {item.customerName}
                  </span>
                  <span className="text-xs text-gray-500">
                    #{item.projectId}
                  </span>
                </div>
                
                <div className="text-sm text-gray-700 bg-gray-50 rounded-lg p-3">
                  <p className="whitespace-pre-wrap">
                    {truncateNote(item.note)}
                  </p>
                  {item.note.length > 120 && (
                    <button className="text-blue-600 hover:text-blue-800 text-xs mt-1">
                      Read more
                    </button>
                  )}
                </div>
              </div>
            </div>
          ))}
        </div>
      </CardContent>
    </Card>
  );
}

export { PCActivityFeedSkeleton };
