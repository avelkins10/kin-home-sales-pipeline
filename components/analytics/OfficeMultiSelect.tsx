'use client';

import { useState, useEffect } from 'react';
import { Building2, ChevronDown, Check } from 'lucide-react';
import { Button } from '@/components/ui/button';
import { Checkbox } from '@/components/ui/checkbox';
import { Popover, PopoverContent, PopoverTrigger } from '@/components/ui/popover';
import { Badge } from '@/components/ui/badge';

interface Office {
  id: number;
  name: string;
  projectCount?: number;
}

interface OfficeMultiSelectProps {
  selectedOfficeIds: number[];
  availableOffices: Office[];
  onChange: (officeIds: number[]) => void;
  disabled?: boolean;
  placeholder?: string;
}

export function OfficeMultiSelect({
  selectedOfficeIds,
  availableOffices,
  onChange,
  disabled = false,
  placeholder = 'Select offices'
}: OfficeMultiSelectProps) {
  const [open, setOpen] = useState(false);
  const [tempSelection, setTempSelection] = useState<number[]>(selectedOfficeIds);

  // Update temp selection when selectedOfficeIds changes
  useEffect(() => {
    setTempSelection(selectedOfficeIds);
  }, [selectedOfficeIds]);

  const allSelected = tempSelection.length === availableOffices.length && availableOffices.length > 0;
  const someSelected = tempSelection.length > 0 && tempSelection.length < availableOffices.length;

  const handleSelectAll = () => {
    if (allSelected) {
      setTempSelection([]);
    } else {
      setTempSelection(availableOffices.map(office => office.id));
    }
  };

  const handleOfficeToggle = (officeId: number) => {
    setTempSelection(prev => 
      prev.includes(officeId) 
        ? prev.filter(id => id !== officeId)
        : [...prev, officeId]
    );
  };

  const handleApply = () => {
    onChange(tempSelection);
    setOpen(false);
  };

  const handleClear = () => {
    setTempSelection([]);
  };

  const handleOpenChange = (newOpen: boolean) => {
    if (newOpen) {
      setTempSelection(selectedOfficeIds);
    }
    setOpen(newOpen);
  };

  const getButtonText = () => {
    if (selectedOfficeIds.length === 0) {
      return placeholder;
    }
    
    if (selectedOfficeIds.length === availableOffices.length) {
      return `All Offices (${availableOffices.length})`;
    }
    
    if (selectedOfficeIds.length === 1) {
      const office = availableOffices.find(o => o.id === selectedOfficeIds[0]);
      return office?.name || '1 Office';
    }
    
    if (selectedOfficeIds.length <= 3) {
      const selectedNames = selectedOfficeIds
        .map(id => availableOffices.find(o => o.id === id)?.name)
        .filter(Boolean);
      return selectedNames.join(', ');
    }
    
    return `${selectedOfficeIds.length} Offices Selected`;
  };

  return (
    <Popover open={open} onOpenChange={handleOpenChange}>
      <PopoverTrigger asChild>
        <Button
          variant="outline"
          role="combobox"
          aria-expanded={open}
          className="w-full md:w-auto justify-between min-w-[200px]"
          disabled={disabled}
        >
          <div className="flex items-center gap-2">
            <Building2 className="h-4 w-4" />
            <span className="truncate">{getButtonText()}</span>
          </div>
          <div className="flex items-center gap-2">
            {selectedOfficeIds.length > 0 && (
              <Badge variant="secondary" className="text-xs">
                {selectedOfficeIds.length}
              </Badge>
            )}
            <ChevronDown className="h-4 w-4 shrink-0 opacity-50" />
          </div>
        </Button>
      </PopoverTrigger>
      <PopoverContent className="w-80 p-0" align="start">
        <div className="flex flex-col max-h-96">
          {/* Header */}
          <div className="p-3 border-b">
            <div className="flex items-center justify-between">
              <h4 className="text-sm font-semibold">Select Offices</h4>
              <div className="flex items-center space-x-2">
                <Checkbox
                  id="select-all"
                  checked={allSelected}
                  ref={(el) => {
                    if (el) el.indeterminate = someSelected;
                  }}
                  onCheckedChange={handleSelectAll}
                />
                <label
                  htmlFor="select-all"
                  className="text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70"
                >
                  Select All
                </label>
              </div>
            </div>
          </div>

          {/* Office List */}
          <div className="flex-1 overflow-y-auto">
            {availableOffices.map((office) => (
              <div
                key={office.id}
                className="flex items-center space-x-2 p-2 hover:bg-slate-50"
              >
                <Checkbox
                  id={`office-${office.id}`}
                  checked={tempSelection.includes(office.id)}
                  onCheckedChange={() => handleOfficeToggle(office.id)}
                />
                <label
                  htmlFor={`office-${office.id}`}
                  className="flex-1 text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70 cursor-pointer"
                >
                  <div className="flex items-center justify-between">
                    <span>{office.name}</span>
                    {office.projectCount !== undefined && (
                      <span className="text-xs text-muted-foreground">
                        {office.projectCount} projects
                      </span>
                    )}
                  </div>
                </label>
              </div>
            ))}
          </div>

          {/* Footer */}
          <div className="p-3 border-t">
            <div className="flex gap-2">
              <Button
                variant="ghost"
                size="sm"
                onClick={handleClear}
                className="flex-1"
              >
                Clear
              </Button>
              <Button
                size="sm"
                onClick={handleApply}
                className="flex-1"
              >
                Apply
              </Button>
            </div>
          </div>
        </div>
      </PopoverContent>
    </Popover>
  );
}
