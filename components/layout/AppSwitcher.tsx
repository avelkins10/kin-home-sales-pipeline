'use client'

import { useState } from 'react';
import { useRouter } from 'next/navigation';
import { useSession } from 'next-auth/react';
import { useAppContext } from '@/lib/contexts/AppContext';
import { canAccessBothApps } from '@/lib/utils/app-helpers';
import { TrendingUp, Settings, ChevronDown } from 'lucide-react';
import { Button } from '@/components/ui/button';
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuTrigger,
} from '@/components/ui/dropdown-menu';

export function AppSwitcher() {
  const { data: session } = useSession();
  const { currentApp, setCurrentApp } = useAppContext();
  const router = useRouter();
  const [isOpen, setIsOpen] = useState(false);

  // Only show app switcher for users with access to both apps
  if (!session?.user?.role || !canAccessBothApps(session.user.role)) {
    return null;
  }

  const handleAppSwitch = (app: 'sales' | 'operations') => {
    setCurrentApp(app);
    setIsOpen(false);
    
    if (app === 'sales') {
      router.push('/');
    } else {
      router.push('/operations');
    }
  };

  const getAppIcon = (app: 'sales' | 'operations') => {
    return app === 'sales' ? TrendingUp : Settings;
  };

  const getAppName = (app: 'sales' | 'operations') => {
    return app === 'sales' ? 'Sales App' : 'Operations App';
  };

  const CurrentIcon = getAppIcon(currentApp);

  return (
    <DropdownMenu open={isOpen} onOpenChange={setIsOpen}>
      <DropdownMenuTrigger asChild>
        <Button
          variant="ghost"
          className="flex items-center space-x-2 px-3 py-2 text-sm font-medium text-gray-700 hover:text-gray-900 hover:bg-gray-100"
        >
          <CurrentIcon className="h-4 w-4" />
          <span>{getAppName(currentApp)}</span>
          <ChevronDown className="h-4 w-4" />
        </Button>
      </DropdownMenuTrigger>
      <DropdownMenuContent align="start" className="w-48">
        <DropdownMenuItem
          onClick={() => handleAppSwitch('sales')}
          className="flex items-center space-x-2"
        >
          <TrendingUp className="h-4 w-4" />
          <span>Sales App</span>
        </DropdownMenuItem>
        <DropdownMenuItem
          onClick={() => handleAppSwitch('operations')}
          className="flex items-center space-x-2"
        >
          <Settings className="h-4 w-4" />
          <span>Operations App</span>
        </DropdownMenuItem>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}

