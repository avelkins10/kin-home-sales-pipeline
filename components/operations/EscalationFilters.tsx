'use client';

import { useState, useEffect } from 'react';
import { Card } from '@/components/ui/card';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Input } from '@/components/ui/input';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Search, X } from 'lucide-react';
import { PCEscalationFilters, PCEscalationCategory } from '@/lib/types/operations';

interface EscalationFiltersProps {
  filters: PCEscalationFilters;
  onFiltersChange: (filters: PCEscalationFilters) => void;
  availableReps: string[];
}

export function EscalationFilters({ filters, onFiltersChange, availableReps }: EscalationFiltersProps) {
  const [searchValue, setSearchValue] = useState(filters.search);

  // Debounce search input
  useEffect(() => {
    const timer = setTimeout(() => {
      if (searchValue !== filters.search) {
        onFiltersChange({ ...filters, search: searchValue });
      }
    }, 300);

    return () => clearTimeout(timer);
  }, [searchValue, filters, onFiltersChange]);

  const handleFilterChange = (key: keyof PCEscalationFilters, value: string) => {
    onFiltersChange({ ...filters, [key]: value });
  };

  const clearFilters = () => {
    const defaultFilters: PCEscalationFilters = {
      category: 'all',
      status: 'all',
      urgency: 'all',
      assignedTo: 'all',
      search: ''
    };
    setSearchValue('');
    onFiltersChange(defaultFilters);
  };

  const getActiveFilterCount = (): number => {
    let count = 0;
    if (filters.category !== 'all') count++;
    if (filters.status !== 'all') count++;
    if (filters.urgency !== 'all') count++;
    if (filters.assignedTo !== 'all') count++;
    if (filters.search) count++;
    return count;
  };

  const getCategoryDisplayName = (category: PCEscalationCategory): string => {
    const displayNames: Record<PCEscalationCategory, string> = {
      mmu_required: 'MMU Required',
      rep_promises: 'Rep Promises',
      hoa_issues: 'HOA Issues',
      financing_issues: 'Financing Issues',
      customer_complaints: 'Customer Complaints'
    };
    return displayNames[category];
  };

  const activeFilterCount = getActiveFilterCount();

  return (
    <Card className="p-4">
      <div className="grid grid-cols-1 md:grid-cols-3 lg:grid-cols-6 gap-3">
        {/* Category Filter */}
        <div>
          <label className="block text-sm font-medium mb-1">Category</label>
          <Select 
            value={filters.category} 
            onValueChange={(value) => handleFilterChange('category', value)}
          >
            <SelectTrigger className="h-10">
              <SelectValue placeholder="All Categories" />
            </SelectTrigger>
            <SelectContent>
              <SelectItem value="all">All Categories</SelectItem>
              <SelectItem value="mmu_required">MMU Required</SelectItem>
              <SelectItem value="rep_promises">Rep Promises</SelectItem>
              <SelectItem value="hoa_issues">HOA Issues</SelectItem>
              <SelectItem value="financing_issues">Financing Issues</SelectItem>
              <SelectItem value="customer_complaints">Customer Complaints</SelectItem>
            </SelectContent>
          </Select>
        </div>

        {/* Status Filter */}
        <div>
          <label className="block text-sm font-medium mb-1">Status</label>
          <Select 
            value={filters.status} 
            onValueChange={(value) => handleFilterChange('status', value)}
          >
            <SelectTrigger className="h-10">
              <SelectValue placeholder="All Statuses" />
            </SelectTrigger>
            <SelectContent>
              <SelectItem value="all">All Statuses</SelectItem>
              <SelectItem value="Escalated to Sales Aid">Escalated to Sales Aid</SelectItem>
              <SelectItem value="Working With Rep">Working With Rep</SelectItem>
              <SelectItem value="Resolved by Rep">Resolved by Rep</SelectItem>
              <SelectItem value="Task Completed">Task Completed</SelectItem>
            </SelectContent>
          </Select>
        </div>

        {/* Urgency Filter */}
        <div>
          <label className="block text-sm font-medium mb-1">Urgency</label>
          <Select 
            value={filters.urgency} 
            onValueChange={(value) => handleFilterChange('urgency', value)}
          >
            <SelectTrigger className="h-10">
              <SelectValue placeholder="All Urgency" />
            </SelectTrigger>
            <SelectContent>
              <SelectItem value="all">All Urgency</SelectItem>
              <SelectItem value="critical">Critical</SelectItem>
              <SelectItem value="high">High</SelectItem>
              <SelectItem value="normal">Normal</SelectItem>
            </SelectContent>
          </Select>
        </div>

        {/* Assigned To Filter */}
        <div>
          <label className="block text-sm font-medium mb-1">Assigned To</label>
          <Select 
            value={filters.assignedTo} 
            onValueChange={(value) => handleFilterChange('assignedTo', value)}
          >
            <SelectTrigger className="h-10">
              <SelectValue placeholder="All Reps" />
            </SelectTrigger>
            <SelectContent>
              <SelectItem value="all">All Reps</SelectItem>
              <SelectItem value="unassigned">Unassigned</SelectItem>
              {availableReps.map((rep) => (
                <SelectItem key={rep} value={rep}>
                  {rep}
                </SelectItem>
              ))}
            </SelectContent>
          </Select>
        </div>

        {/* Search Input */}
        <div className="lg:col-span-2">
          <label className="block text-sm font-medium mb-1">Search</label>
          <div className="relative">
            <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-gray-400" />
            <Input
              placeholder="Search by customer name or project ID..."
              value={searchValue}
              onChange={(e) => setSearchValue(e.target.value)}
              className="h-10 pl-10"
            />
          </div>
        </div>

        {/* Clear Filters Button */}
        <div className="flex items-end">
          <Button
            variant="outline"
            onClick={clearFilters}
            className="h-10 w-full"
            disabled={activeFilterCount === 0}
          >
            Clear Filters
          </Button>
        </div>
      </div>

      {/* Active Filters Display */}
      {activeFilterCount > 0 && (
        <div className="mt-4 pt-4 border-t">
          <div className="flex items-center gap-2 flex-wrap">
            <span className="text-sm text-gray-600">Active filters:</span>
            <Badge variant="secondary" className="text-xs">
              {activeFilterCount} filter{activeFilterCount !== 1 ? 's' : ''}
            </Badge>
            
            {filters.category !== 'all' && (
              <Badge 
                variant="outline" 
                className="text-xs cursor-pointer hover:bg-gray-100"
                onClick={() => handleFilterChange('category', 'all')}
              >
                Category: {getCategoryDisplayName(filters.category as PCEscalationCategory)}
                <X className="ml-1 h-3 w-3" />
              </Badge>
            )}
            
            {filters.status !== 'all' && (
              <Badge 
                variant="outline" 
                className="text-xs cursor-pointer hover:bg-gray-100"
                onClick={() => handleFilterChange('status', 'all')}
              >
                Status: {filters.status}
                <X className="ml-1 h-3 w-3" />
              </Badge>
            )}
            
            {filters.urgency !== 'all' && (
              <Badge 
                variant="outline" 
                className="text-xs cursor-pointer hover:bg-gray-100"
                onClick={() => handleFilterChange('urgency', 'all')}
              >
                Urgency: {filters.urgency}
                <X className="ml-1 h-3 w-3" />
              </Badge>
            )}
            
            {filters.assignedTo !== 'all' && (
              <Badge 
                variant="outline" 
                className="text-xs cursor-pointer hover:bg-gray-100"
                onClick={() => handleFilterChange('assignedTo', 'all')}
              >
                Assigned: {filters.assignedTo === 'unassigned' ? 'Unassigned' : filters.assignedTo}
                <X className="ml-1 h-3 w-3" />
              </Badge>
            )}
            
            {filters.search && (
              <Badge 
                variant="outline" 
                className="text-xs cursor-pointer hover:bg-gray-100"
                onClick={() => {
                  setSearchValue('');
                  handleFilterChange('search', '');
                }}
              >
                Search: "{filters.search}"
                <X className="ml-1 h-3 w-3" />
              </Badge>
            )}
          </div>
        </div>
      )}
    </Card>
  );
}
