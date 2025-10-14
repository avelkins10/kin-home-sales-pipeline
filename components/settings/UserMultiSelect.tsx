'use client';

import { useState, useMemo } from 'react';
import { Check, ChevronDown, X, Users } from 'lucide-react';
import { User } from '@/lib/types/user';
import { UserRole } from '@/lib/types/project';
import { Button } from '@/components/ui/button';
import { Checkbox } from '@/components/ui/checkbox';
import { Badge } from '@/components/ui/badge';
import { Avatar, AvatarFallback } from '@/components/ui/avatar';
import { Popover, PopoverContent, PopoverTrigger } from '@/components/ui/popover';
import { Input } from '@/components/ui/input';
import { ScrollArea } from '@/components/ui/scroll-area';
import { cn } from '@/lib/utils/cn';
import { getInitials } from '@/lib/utils/avatar';

interface UserMultiSelectProps {
  value: string[];
  onChange: (userIds: string[]) => void;
  users: User[];
  excludeUserIds?: string[];
  roleFilter?: UserRole[];
  maxSelections?: number;
  placeholder?: string;
  disabled?: boolean;
  className?: string;
}

export function UserMultiSelect({
  value,
  onChange,
  users,
  excludeUserIds = [],
  roleFilter,
  maxSelections = 50,
  placeholder = 'Select users...',
  disabled = false,
  className
}: UserMultiSelectProps) {
  const [open, setOpen] = useState(false);
  const [searchTerm, setSearchTerm] = useState('');

  // Filter users based on search, exclusions, and role filter
  const filteredUsers = useMemo(() => {
    let filtered = users.filter(user => 
      !excludeUserIds.includes(user.id) &&
      (roleFilter ? roleFilter.includes(user.role) : true)
    );

    if (searchTerm) {
      const term = searchTerm.toLowerCase();
      filtered = filtered.filter(user =>
        user.name?.toLowerCase().includes(term) ||
        user.email.toLowerCase().includes(term)
      );
    }

    return filtered;
  }, [users, excludeUserIds, roleFilter, searchTerm]);

  const selectedUsers = useMemo(() => {
    return users.filter(user => value.includes(user.id));
  }, [users, value]);

  const handleUserToggle = (userId: string) => {
    if (value.includes(userId)) {
      onChange(value.filter(id => id !== userId));
    } else if (value.length < maxSelections) {
      onChange([...value, userId]);
    }
  };

  const handleClearAll = () => {
    onChange([]);
  };

  const handleRemoveUser = (userId: string) => {
    onChange(value.filter(id => id !== userId));
  };

  const isMaxReached = value.length >= maxSelections;

  return (
    <div className={cn('w-full', className)}>
      <Popover open={open} onOpenChange={setOpen}>
        <PopoverTrigger asChild>
          <Button
            variant="outline"
            role="combobox"
            aria-expanded={open}
            className="w-full justify-between"
            disabled={disabled}
          >
            <div className="flex items-center gap-2">
              <Users className="h-4 w-4" />
              {value.length === 0 ? (
                <span className="text-muted-foreground">{placeholder}</span>
              ) : (
                <span>{value.length} user{value.length !== 1 ? 's' : ''} selected</span>
              )}
            </div>
            <ChevronDown className="h-4 w-4 shrink-0 opacity-50" />
          </Button>
        </PopoverTrigger>
        <PopoverContent className="w-full p-0" align="start">
          <div className="p-3 border-b">
            <Input
              placeholder="Search users..."
              value={searchTerm}
              onChange={(e) => setSearchTerm(e.target.value)}
              className="h-8"
            />
          </div>
          
          <ScrollArea className="max-h-64">
            <div className="p-2">
              {filteredUsers.length === 0 ? (
                <div className="text-center py-4 text-sm text-muted-foreground">
                  {searchTerm ? 'No users found' : 'No users available'}
                </div>
              ) : (
                <div className="space-y-1">
                  {filteredUsers.map((user) => {
                    const isSelected = value.includes(user.id);
                    const isDisabled = !isSelected && isMaxReached;
                    
                    return (
                      <div
                        key={user.id}
                        data-testid="user-item"
                        className={cn(
                          'flex items-center gap-3 p-2 rounded-md cursor-pointer transition-colors',
                          'hover:bg-gray-50',
                          isSelected && 'bg-blue-50',
                          isDisabled && 'opacity-50 cursor-not-allowed'
                        )}
                        onClick={() => !isDisabled && handleUserToggle(user.id)}
                      >
                        <Checkbox
                          checked={isSelected}
                          disabled={isDisabled}
                          value={user.id}
                          className="pointer-events-none"
                        />
                        <Avatar className="h-8 w-8">
                          <AvatarFallback className="text-xs">
                            {getInitials(user.name || user.email)}
                          </AvatarFallback>
                        </Avatar>
                        <div className="flex-1 min-w-0">
                          <div className="text-sm font-medium truncate">
                            {user.name || user.email}
                          </div>
                          {user.name && (
                            <div className="text-xs text-muted-foreground truncate">
                              {user.email}
                            </div>
                          )}
                        </div>
                        <Badge variant="secondary" className="text-xs">
                          {user.role}
                        </Badge>
                        {!user.isActive && (
                          <div className="w-2 h-2 bg-gray-400 rounded-full" title="Inactive" />
                        )}
                      </div>
                    );
                  })}
                </div>
              )}
            </div>
          </ScrollArea>
          
          {value.length > 0 && (
            <div className="p-3 border-t">
              <div className="flex items-center justify-between mb-2">
                <span className="text-sm text-muted-foreground">
                  {value.length} user{value.length !== 1 ? 's' : ''} selected
                </span>
                <Button
                  variant="ghost"
                  size="sm"
                  onClick={handleClearAll}
                  className="h-6 px-2 text-xs"
                >
                  Clear all
                </Button>
              </div>
              
              <div className="flex flex-wrap gap-1">
                {selectedUsers.map((user) => (
                  <Badge
                    key={user.id}
                    variant="secondary"
                    className="text-xs pr-1"
                  >
                    {user.name || user.email}
                    <Button
                      variant="ghost"
                      size="sm"
                      className="h-4 w-4 p-0 ml-1 hover:bg-transparent"
                      onClick={(e) => {
                        e.stopPropagation();
                        handleRemoveUser(user.id);
                      }}
                    >
                      <X className="h-3 w-3" />
                    </Button>
                  </Badge>
                ))}
              </div>
            </div>
          )}
        </PopoverContent>
      </Popover>
    </div>
  );
}
