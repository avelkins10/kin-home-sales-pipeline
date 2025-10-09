'use client';

import { Alert, AlertDescription, AlertTitle } from '@/components/ui/alert';
import { Badge } from '@/components/ui/badge';
import { Info } from 'lucide-react';
import { getRoleDisplayName, getRoleDescription, isOfficeBasedRole } from '@/lib/utils/role-helpers';
import { cn } from '@/lib/utils/cn';

interface RoleExplanationProps {
  role: string;
  offices?: string[];
  className?: string;
}

export function RoleExplanation({ role, offices, className }: RoleExplanationProps) {
  const displayName = getRoleDisplayName(role);
  const description = getRoleDescription(role, offices);
  const isOfficeBased = isOfficeBasedRole(role);

  return (
    <Alert className={cn('', className)}>
      <Info className="h-4 w-4" />
      <AlertTitle>Your Access Level: {displayName}</AlertTitle>
      <AlertDescription className="space-y-3">
        <p>{description}</p>
        
        {/* Office badges for office-based roles */}
        {isOfficeBased && offices && offices.length > 0 && (
          <div className="space-y-2">
            <p className="text-sm font-medium">Assigned Offices:</p>
            <div className="flex flex-wrap gap-2">
              {offices.map((office, index) => (
                <Badge key={index} variant="secondary" className="text-xs">
                  {office}
                </Badge>
              ))}
            </div>
          </div>
        )}
        
        {/* Additional context for office-based roles */}
        {isOfficeBased && (
          <p className="text-sm text-muted-foreground">
            This means you have complete visibility into your territory, regardless of whether 
            individual sales reps have active accounts in the system.
          </p>
        )}
      </AlertDescription>
    </Alert>
  );
}
