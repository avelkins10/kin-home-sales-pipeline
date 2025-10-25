'use client';

import { Card } from '@/components/ui/card';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Input } from '@/components/ui/input';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Search, X } from 'lucide-react';
import { PCOutreachFilters, PCOutreachStatus } from '@/lib/types/operations';
import { useState, useEffect } from 'react';

interface OutreachFiltersProps {
  filters: PCOutreachFilters;
  onFiltersChange: (filters: PCOutreachFilters) => void;
  availableLenders: string[];
  availableSalesReps: string[];
}

export function OutreachFilters({ 
  filters, 
  onFiltersChange, 
  availableLenders, 
  availableSalesReps 
}: OutreachFiltersProps) {
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

  const handleFilterChange = (key: keyof PCOutreachFilters, value: string) => {
    onFiltersChange({ ...filters, [key]: value });
  };

  const clearFilters = () => {
    const defaultFilters: PCOutreachFilters = {
      tab: filters.tab,
      status: 'all',
      daysOverdue: 'all',
      lender: 'all',
      salesRep: 'all',
      search: ''
    };
    setSearchValue('');
    onFiltersChange(defaultFilters);
  };

  // Count active filters (excluding 'all' values and empty search)
  const activeFiltersCount = [
    filters.status !== 'all',
    filters.daysOverdue !== 'all',
    filters.lender !== 'all',
    filters.salesRep !== 'all',
    filters.search !== ''
  ].filter(Boolean).length;

  const statusOptions: Array<{ value: PCOutreachStatus | 'all'; label: string }> = [
    { value: 'all', label: 'All Status' },
    { value: 'Complete', label: 'Complete' },
    { value: 'No Answer Left Message', label: 'No Answer Left Message' },
    { value: 'Complete - No Answer', label: 'Complete - No Answer' }
  ];

  const daysOverdueOptions = [
    { value: 'all', label: 'All Days' },
    { value: '1-3', label: '1-3 days' },
    { value: '4-7', label: '4-7 days' },
    { value: '8+', label: '8+ days' }
  ];

  const lenderOptions = [
    { value: 'all', label: 'All Lenders' },
    ...availableLenders.map(lender => ({ value: lender, label: lender }))
  ];

  const salesRepOptions = [
    { value: 'all', label: 'All Sales Reps' },
    ...availableSalesReps.map(rep => ({ value: rep, label: rep }))
  ];

  return (
    <Card className="p-4">
      <div className="space-y-4">
        {/* Filter controls */}
        <div className="grid grid-cols-1 md:grid-cols-3 lg:grid-cols-6 gap-3">
          {/* Status filter */}
          <div>
            <label className="text-sm font-medium text-gray-700 mb-1 block">
              Status
            </label>
            <Select
              value={filters.status}
              onValueChange={(value) => handleFilterChange('status', value)}
            >
              <SelectTrigger className="h-10">
                <SelectValue />
              </SelectTrigger>
              <SelectContent>
                {statusOptions.map(option => (
                  <SelectItem key={option.value} value={option.value}>
                    {option.label}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>

          {/* Days overdue filter */}
          <div>
            <label className="text-sm font-medium text-gray-700 mb-1 block">
              Days Overdue
            </label>
            <Select
              value={filters.daysOverdue}
              onValueChange={(value) => handleFilterChange('daysOverdue', value)}
            >
              <SelectTrigger className="h-10">
                <SelectValue />
              </SelectTrigger>
              <SelectContent>
                {daysOverdueOptions.map(option => (
                  <SelectItem key={option.value} value={option.value}>
                    {option.label}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>

          {/* Lender filter */}
          <div>
            <label className="text-sm font-medium text-gray-700 mb-1 block">
              Lender
            </label>
            <Select
              value={filters.lender}
              onValueChange={(value) => handleFilterChange('lender', value)}
            >
              <SelectTrigger className="h-10">
                <SelectValue />
              </SelectTrigger>
              <SelectContent>
                {lenderOptions.map(option => (
                  <SelectItem key={option.value} value={option.value}>
                    {option.label}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>

          {/* Sales rep filter */}
          <div>
            <label className="text-sm font-medium text-gray-700 mb-1 block">
              Sales Rep
            </label>
            <Select
              value={filters.salesRep}
              onValueChange={(value) => handleFilterChange('salesRep', value)}
            >
              <SelectTrigger className="h-10">
                <SelectValue />
              </SelectTrigger>
              <SelectContent>
                {salesRepOptions.map(option => (
                  <SelectItem key={option.value} value={option.value}>
                    {option.label}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>

          {/* Search input */}
          <div className="lg:col-span-2">
            <label className="text-sm font-medium text-gray-700 mb-1 block">
              Search
            </label>
            <div className="relative">
              <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-gray-400" />
              <Input
                placeholder="Search by customer name or project ID"
                value={searchValue}
                onChange={(e) => setSearchValue(e.target.value)}
                className="h-10 pl-10"
              />
            </div>
          </div>
        </div>

        {/* Active filters and clear button */}
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-2">
            {activeFiltersCount > 0 && (
              <Badge variant="secondary" className="text-xs">
                {activeFiltersCount} filter{activeFiltersCount > 1 ? 's' : ''} active
              </Badge>
            )}
          </div>

          {activeFiltersCount > 0 && (
            <Button
              variant="outline"
              size="sm"
              onClick={clearFilters}
              className="h-8"
            >
              <X className="h-4 w-4 mr-1" />
              Clear Filters
            </Button>
          )}
        </div>
      </div>
    </Card>
  );
}
