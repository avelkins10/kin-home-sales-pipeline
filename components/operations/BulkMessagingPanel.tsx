'use client';

import { useState, useEffect } from 'react';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Checkbox } from '@/components/ui/checkbox';
import { Badge } from '@/components/ui/badge';
import { PCBulkMessagingPayload, PCBulkMessagingRecipient } from '@/lib/types/operations';
import { getSmsTemplate, listAvailableTemplates, validateSmsLength, SmsTemplateType } from '@/lib/integrations/twilio/templates';
import { Check, X, Users, MessageSquare, Send } from 'lucide-react';

interface BulkMessagingPanelProps {
  onSend: (payload: PCBulkMessagingPayload) => void;
  isProcessing: boolean;
}

export function BulkMessagingPanel({ onSend, isProcessing }: BulkMessagingPanelProps) {
  const [step, setStep] = useState(1);
  const [selectedTemplate, setSelectedTemplate] = useState<SmsTemplateType | null>(null);
  const [recipients, setRecipients] = useState<PCBulkMessagingRecipient[]>([]);
  const [selectedRecipients, setSelectedRecipients] = useState<Set<string>>(new Set());
  const [variables, setVariables] = useState<Record<string, string>>({});
  const [filters, setFilters] = useState({
    projectStage: 'all',
    lender: 'all',
    salesRep: 'all'
  });
  const [previewMessage, setPreviewMessage] = useState('');

  // Fetch recipients when filters change
  useEffect(() => {
    fetchRecipients();
  }, [filters]);

  // Generate preview when template or variables change
  useEffect(() => {
    if (selectedTemplate && Object.keys(variables).length > 0) {
      generatePreview();
    }
  }, [selectedTemplate, variables]);

  const fetchRecipients = async () => {
    try {
      const params = new URLSearchParams();
      if (filters.projectStage !== 'all') params.append('projectStage', filters.projectStage);
      if (filters.lender !== 'all') params.append('lender', filters.lender);
      if (filters.salesRep !== 'all') params.append('salesRep', filters.salesRep);

      const response = await fetch(`/api/operations/communications/recipients?${params}`);
      const data = await response.json();
      
      if (response.ok) {
        setRecipients(data.recipients || []);
      }
    } catch (error) {
      console.error('Failed to fetch recipients:', error);
    }
  };

  const generatePreview = () => {
    if (!selectedTemplate) return;
    
    try {
      const sampleVariables = {
        ...variables,
        customerName: 'John Doe',
        projectId: 'PROJ-12345',
        coordinatorName: 'PC Name',
        coordinatorPhone: '+1234567890'
      };
      
      const message = getSmsTemplate(selectedTemplate, sampleVariables);
      setPreviewMessage(message);
    } catch (error) {
      console.error('Failed to generate preview:', error);
    }
  };

  const handleRecipientToggle = (projectId: string) => {
    const newSelected = new Set(selectedRecipients);
    if (newSelected.has(projectId)) {
      newSelected.delete(projectId);
    } else {
      newSelected.add(projectId);
    }
    setSelectedRecipients(newSelected);
  };

  const handleSelectAll = () => {
    setSelectedRecipients(new Set(recipients.map(r => r.projectId)));
  };

  const handleClearAll = () => {
    setSelectedRecipients(new Set());
  };

  const handleSend = () => {
    if (!selectedTemplate || selectedRecipients.size === 0) return;
    
    const selectedRecipientData = recipients.filter(r => selectedRecipients.has(r.projectId));
    
    const payload: PCBulkMessagingPayload = {
      templateType: selectedTemplate,
      recipients: selectedRecipientData,
      variables,
      scheduledTime: null
    };
    
    onSend(payload);
  };

  const validateForm = () => {
    if (!selectedTemplate) return false;
    if (selectedRecipients.size === 0) return false;
    
    // Check if all required variables are filled
    const template = listAvailableTemplates().find(t => t.type === selectedTemplate);
    if (template?.requiredVariables) {
      for (const variable of template.requiredVariables) {
        if (!variables[variable]) return false;
      }
    }
    
    return true;
  };

  const templates = listAvailableTemplates();
  const selectedTemplateData = templates.find(t => t.type === selectedTemplate);
  const smsValidation = previewMessage ? validateSmsLength(previewMessage) : null;

  return (
    <Card>
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <MessageSquare className="w-5 h-5" />
          Bulk Messaging
        </CardTitle>
      </CardHeader>
      <CardContent className="space-y-6">
        {/* Step 1: Select Recipients */}
        <div className="space-y-4">
          <div className="flex items-center gap-2">
            <div className={`w-6 h-6 rounded-full flex items-center justify-center text-sm font-medium ${
              step >= 1 ? 'bg-blue-600 text-white' : 'bg-gray-200 text-gray-600'
            }`}>
              1
            </div>
            <h3 className="font-medium">Select Recipients</h3>
          </div>
          
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            <Select value={filters.projectStage} onValueChange={(value) => setFilters({...filters, projectStage: value})}>
              <SelectTrigger>
                <SelectValue placeholder="Project Stage" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="all">All Stages</SelectItem>
                <SelectItem value="Survey">Survey</SelectItem>
                <SelectItem value="Design">Design</SelectItem>
                <SelectItem value="Permitting">Permitting</SelectItem>
                <SelectItem value="Installation">Installation</SelectItem>
                <SelectItem value="PTO">PTO</SelectItem>
              </SelectContent>
            </Select>
            
            <Select value={filters.lender} onValueChange={(value) => setFilters({...filters, lender: value})}>
              <SelectTrigger>
                <SelectValue placeholder="Lender" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="all">All Lenders</SelectItem>
                {/* Add more lenders as needed */}
              </SelectContent>
            </Select>
            
            <Select value={filters.salesRep} onValueChange={(value) => setFilters({...filters, salesRep: value})}>
              <SelectTrigger>
                <SelectValue placeholder="Sales Rep" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="all">All Sales Reps</SelectItem>
                {/* Add more sales reps as needed */}
              </SelectContent>
            </Select>
          </div>
          
          <div className="flex items-center justify-between">
            <div className="flex items-center gap-2">
              <Button variant="outline" size="sm" onClick={handleSelectAll}>
                Select All
              </Button>
              <Button variant="outline" size="sm" onClick={handleClearAll}>
                Clear All
              </Button>
            </div>
            <Badge variant="secondary">
              {selectedRecipients.size} selected
            </Badge>
          </div>
          
          <div className="max-h-48 overflow-y-auto border rounded-lg p-3 space-y-2">
            {recipients.map((recipient) => (
              <div key={recipient.projectId} className="flex items-center gap-2">
                <Checkbox
                  checked={selectedRecipients.has(recipient.projectId)}
                  onCheckedChange={() => handleRecipientToggle(recipient.projectId)}
                />
                <div className="flex-1 min-w-0">
                  <div className="text-sm font-medium truncate">{recipient.customerName}</div>
                  <div className="text-xs text-gray-500 truncate">
                    {recipient.projectId} • {recipient.projectStage} • {recipient.salesRepName}
                  </div>
                </div>
              </div>
            ))}
          </div>
        </div>

        {/* Step 2: Choose Template */}
        <div className="space-y-4">
          <div className="flex items-center gap-2">
            <div className={`w-6 h-6 rounded-full flex items-center justify-center text-sm font-medium ${
              step >= 2 ? 'bg-blue-600 text-white' : 'bg-gray-200 text-gray-600'
            }`}>
              2
            </div>
            <h3 className="font-medium">Choose Template</h3>
          </div>
          
          <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
            {templates.map((template) => (
              <div
                key={template.type}
                className={`p-3 border rounded-lg cursor-pointer transition-colors ${
                  selectedTemplate === template.type
                    ? 'border-blue-500 bg-blue-50'
                    : 'border-gray-200 hover:border-gray-300'
                }`}
                onClick={() => setSelectedTemplate(template.type as SmsTemplateType)}
              >
                <div className="flex items-center gap-2 mb-1">
                  <input
                    type="radio"
                    checked={selectedTemplate === template.type}
                    onChange={() => setSelectedTemplate(template.type as SmsTemplateType)}
                    className="w-4 h-4"
                  />
                  <span className="font-medium">{template.name}</span>
                </div>
                <p className="text-sm text-gray-600">{template.description}</p>
              </div>
            ))}
          </div>
        </div>

        {/* Step 3: Fill Variables */}
        {selectedTemplateData && (
          <div className="space-y-4">
            <div className="flex items-center gap-2">
              <div className={`w-6 h-6 rounded-full flex items-center justify-center text-sm font-medium ${
                step >= 3 ? 'bg-blue-600 text-white' : 'bg-gray-200 text-gray-600'
              }`}>
                3
              </div>
              <h3 className="font-medium">Fill Variables</h3>
            </div>
            
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              {selectedTemplateData.requiredVariables?.map((variable) => (
                <div key={variable}>
                  <label className="text-sm font-medium text-gray-700 mb-1 block">
                    {variable}
                  </label>
                  <Input
                    value={variables[variable] || ''}
                    onChange={(e) => setVariables({...variables, [variable]: e.target.value})}
                    placeholder={`Enter ${variable}`}
                  />
                </div>
              ))}
            </div>
          </div>
        )}

        {/* Step 4: Preview & Send */}
        <div className="space-y-4">
          <div className="flex items-center gap-2">
            <div className={`w-6 h-6 rounded-full flex items-center justify-center text-sm font-medium ${
              step >= 4 ? 'bg-blue-600 text-white' : 'bg-gray-200 text-gray-600'
            }`}>
              4
            </div>
            <h3 className="font-medium">Preview & Send</h3>
          </div>
          
          {previewMessage && (
            <div className="space-y-3">
              <div className="p-3 bg-gray-50 rounded-lg">
                <div className="text-sm font-medium text-gray-700 mb-1">Message Preview:</div>
                <div className="text-sm text-gray-900 whitespace-pre-wrap">{previewMessage}</div>
              </div>
              
              {smsValidation && (
                <div className="flex items-center gap-2 text-sm">
                  <span className="text-gray-600">
                    {smsValidation.characterCount} characters, {smsValidation.segmentCount} segment(s)
                  </span>
                  {smsValidation.segmentCount > 1 && (
                    <Badge variant="warning">Multiple segments</Badge>
                  )}
                </div>
              )}
            </div>
          )}
          
          <div className="flex items-center justify-between">
            <div className="flex items-center gap-2">
              <Users className="w-4 h-4 text-gray-500" />
              <span className="text-sm text-gray-600">
                {selectedRecipients.size} recipients selected
              </span>
            </div>
            
            <Button
              onClick={handleSend}
              disabled={!validateForm() || isProcessing}
              className="flex items-center gap-2"
            >
              {isProcessing ? (
                <>
                  <div className="w-4 h-4 border-2 border-white border-t-transparent rounded-full animate-spin" />
                  Sending...
                </>
              ) : (
                <>
                  <Send className="w-4 h-4" />
                  Send Messages
                </>
              )}
            </Button>
          </div>
        </div>
      </CardContent>
    </Card>
  );
}
