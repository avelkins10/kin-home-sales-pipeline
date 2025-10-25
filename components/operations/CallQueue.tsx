'use client';

import { useState, useEffect } from 'react';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { PCCallQueueItem } from '@/lib/types/operations';
import { Phone, PhoneOff, ChevronUp, ChevronDown, X, Play, Pause, Clock } from 'lucide-react';

interface CallQueueProps {
  onCallInitiated?: (callSid: string) => void;
}

export function CallQueue({ onCallInitiated }: CallQueueProps) {
  const [queue, setQueue] = useState<PCCallQueueItem[]>([]);
  const [currentCall, setCurrentCall] = useState<PCCallQueueItem | null>(null);
  const [callStatus, setCallStatus] = useState<'idle' | 'calling' | 'completed'>('idle');
  const [callStartTime, setCallStartTime] = useState<Date | null>(null);
  const [callDuration, setCallDuration] = useState(0);

  // Load queue from localStorage on mount
  useEffect(() => {
    const savedQueue = localStorage.getItem('call-queue');
    if (savedQueue) {
      try {
        setQueue(JSON.parse(savedQueue));
      } catch (error) {
        console.error('Failed to load call queue from localStorage:', error);
      }
    }
  }, []);

  // Save queue to localStorage whenever it changes
  useEffect(() => {
    localStorage.setItem('call-queue', JSON.stringify(queue));
  }, [queue]);

  // Update call duration timer
  useEffect(() => {
    let interval: NodeJS.Timeout;
    
    if (callStatus === 'calling' && callStartTime) {
      interval = setInterval(() => {
        setCallDuration(Math.floor((Date.now() - callStartTime.getTime()) / 1000));
      }, 1000);
    }
    
    return () => {
      if (interval) clearInterval(interval);
    };
  }, [callStatus, callStartTime]);

  const addToQueue = (item: PCCallQueueItem) => {
    const newItem = {
      ...item,
      queuedAt: new Date().toISOString(),
      priority: queue.length + 1
    };
    setQueue([...queue, newItem]);
  };

  const removeFromQueue = (projectId: string) => {
    setQueue(queue.filter(item => item.projectId !== projectId));
  };

  const moveUp = (projectId: string) => {
    const index = queue.findIndex(item => item.projectId === projectId);
    if (index > 0) {
      const newQueue = [...queue];
      [newQueue[index], newQueue[index - 1]] = [newQueue[index - 1], newQueue[index]];
      setQueue(newQueue);
    }
  };

  const moveDown = (projectId: string) => {
    const index = queue.findIndex(item => item.projectId === projectId);
    if (index < queue.length - 1) {
      const newQueue = [...queue];
      [newQueue[index], newQueue[index + 1]] = [newQueue[index + 1], newQueue[index]];
      setQueue(newQueue);
    }
  };

  const startCalling = async () => {
    if (queue.length === 0) return;
    
    const firstItem = queue[0];
    setCurrentCall(firstItem);
    setCallStatus('calling');
    setCallStartTime(new Date());
    setCallDuration(0);
    
    // Update queue item status
    setQueue(prev => prev.map((item, index) => 
      index === 0 
        ? { ...item, callStatus: 'calling' as const }
        : item
    ));
    
    // Simulate call initiation (replace with actual API call)
    try {
      const response = await fetch('/api/operations/communications/call', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          projectId: firstItem.projectId,
          recordId: firstItem.recordId,
          customerPhone: firstItem.customerPhone,
          customerName: firstItem.customerName
        })
      });
      
      const result = await response.json();
      
      if (result.success && result.result.callSid) {
        // Update current call with SID
        setCurrentCall(prev => prev ? { ...prev, callSid: result.result.callSid } : null);
        
        // Notify parent component
        if (onCallInitiated) {
          onCallInitiated(result.result.callSid);
        }
      }
    } catch (error) {
      console.error('Failed to initiate call:', error);
      setCallStatus('idle');
      setCurrentCall(null);
    }
  };

  const endCall = () => {
    if (currentCall) {
      // Update current call status
      setCurrentCall(prev => prev ? { ...prev, callStatus: 'completed' as const } : null);
      
      // Remove from queue
      setQueue(prev => prev.filter(item => item.projectId !== currentCall.projectId));
      
      // Reset call state
      setCallStatus('idle');
      setCurrentCall(null);
      setCallStartTime(null);
      setCallDuration(0);
    }
  };

  const nextCall = () => {
    if (queue.length > 0) {
      startCalling();
    } else {
      setCallStatus('idle');
      setCurrentCall(null);
    }
  };

  const formatDuration = (seconds: number) => {
    const mins = Math.floor(seconds / 60);
    const secs = seconds % 60;
    return `${mins}:${secs.toString().padStart(2, '0')}`;
  };

  const getPriorityColor = (priority: number) => {
    if (priority <= 3) return 'bg-red-100 text-red-800';
    if (priority <= 6) return 'bg-orange-100 text-orange-800';
    return 'bg-blue-100 text-blue-800';
  };

  return (
    <Card>
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <Phone className="w-5 h-5" />
          Call Queue
        </CardTitle>
        <p className="text-sm text-gray-600">Manage outbound calls</p>
      </CardHeader>
      <CardContent className="space-y-6">
        {/* Current Call Section */}
        {currentCall && (
          <div className="border rounded-lg p-4 bg-green-50 border-green-200">
            <div className="flex items-center justify-between mb-3">
              <h3 className="font-medium text-green-900">Current Call</h3>
              <Badge variant="secondary" className="bg-green-100 text-green-800">
                {callStatus === 'calling' ? 'In Progress' : 'Completed'}
              </Badge>
            </div>
            
            <div className="space-y-2">
              <div className="flex items-center gap-2">
                <span className="font-medium">{currentCall.customerName}</span>
                <Badge variant="outline">{currentCall.projectId}</Badge>
              </div>
              
              <div className="text-sm text-gray-600">
                {currentCall.customerPhone} • {currentCall.projectStage}
              </div>
              
              {callStatus === 'calling' && (
                <div className="flex items-center gap-2 text-sm text-green-700">
                  <Clock className="w-4 h-4" />
                  <span>Duration: {formatDuration(callDuration)}</span>
                </div>
              )}
            </div>
            
            <div className="flex gap-2 mt-4">
              <Button
                size="sm"
                variant="outline"
                onClick={endCall}
                disabled={callStatus === 'calling'}
                className="flex items-center gap-1"
              >
                <PhoneOff className="w-4 h-4" />
                End Call
              </Button>
              
              <Button
                size="sm"
                variant="default"
                onClick={nextCall}
                className="flex items-center gap-1"
              >
                <Play className="w-4 h-4" />
                Next Call
              </Button>
            </div>
          </div>
        )}

        {/* Queue Section */}
        <div className="space-y-4">
          <div className="flex items-center justify-between">
            <h3 className="font-medium">Call Queue</h3>
            <div className="flex items-center gap-2">
              <Badge variant="secondary">
                {queue.length} in queue
              </Badge>
              {queue.length > 0 && !currentCall && (
                <Button
                  size="sm"
                  onClick={startCalling}
                  className="flex items-center gap-1"
                >
                  <Play className="w-4 h-4" />
                  Start Calling
                </Button>
              )}
            </div>
          </div>
          
          {queue.length === 0 ? (
            <div className="text-center py-8 text-gray-500">
              <Phone className="w-12 h-12 mx-auto mb-4 text-gray-300" />
              <p>No calls in queue</p>
              <p className="text-sm">Add customers from priority queue or conversations</p>
            </div>
          ) : (
            <div className="space-y-2">
              {queue.slice(0, 5).map((item, index) => (
                <div
                  key={item.projectId}
                  className={`flex items-center gap-3 p-3 border rounded-lg ${
                    index === 0 ? 'border-blue-200 bg-blue-50' : 'border-gray-200'
                  }`}
                >
                  <div className="flex items-center gap-2">
                    <Badge 
                      variant="outline" 
                      className={getPriorityColor(item.priority)}
                    >
                      #{item.priority}
                    </Badge>
                    {index === 0 && (
                      <Badge variant="default" className="text-xs">
                        Up Next
                      </Badge>
                    )}
                  </div>
                  
                  <div className="flex-1 min-w-0">
                    <div className="font-medium text-sm truncate">
                      {item.customerName}
                    </div>
                    <div className="text-xs text-gray-600 truncate">
                      {item.projectId} • {item.customerPhone} • {item.projectStage}
                    </div>
                  </div>
                  
                  <div className="flex items-center gap-1">
                    {index > 0 && (
                      <Button
                        size="sm"
                        variant="ghost"
                        onClick={() => moveUp(item.projectId)}
                        className="h-6 w-6 p-0"
                      >
                        <ChevronUp className="w-3 h-3" />
                      </Button>
                    )}
                    
                    {index < queue.length - 1 && (
                      <Button
                        size="sm"
                        variant="ghost"
                        onClick={() => moveDown(item.projectId)}
                        className="h-6 w-6 p-0"
                      >
                        <ChevronDown className="w-3 h-3" />
                      </Button>
                    )}
                    
                    <Button
                      size="sm"
                      variant="ghost"
                      onClick={() => removeFromQueue(item.projectId)}
                      className="h-6 w-6 p-0 text-red-600 hover:text-red-700"
                    >
                      <X className="w-3 h-3" />
                    </Button>
                  </div>
                </div>
              ))}
              
              {queue.length > 5 && (
                <div className="text-center py-2 text-sm text-gray-500">
                  + {queue.length - 5} more calls
                </div>
              )}
            </div>
          )}
        </div>
      </CardContent>
    </Card>
  );
}
