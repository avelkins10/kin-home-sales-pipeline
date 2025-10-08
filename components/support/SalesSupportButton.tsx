'use client';

import { MessageSquare } from 'lucide-react';
import { Button } from '@/components/ui/button';
import { toast } from 'sonner';
import { openChatWithProject, showFrontChat, type ProjectContext } from '@/lib/integrations/frontChat';

interface SalesSupportButtonProps {
  projectContext?: ProjectContext;
  variant?: 'default' | 'outline' | 'ghost';
  size?: 'default' | 'sm' | 'lg';
  className?: string;
}

export function SalesSupportButton({
  projectContext,
  variant = 'outline',
  size = 'default',
  className,
}: SalesSupportButtonProps) {
  const handleClick = () => {
    if (projectContext) {
      // Open chat with project context
      openChatWithProject(projectContext);

      // Show confirmation toast
      toast.success('Project details sent to support', {
        description: `Regarding: ${projectContext.customerName}`,
        duration: 2000,
      });
    } else {
      // Open generic chat
      showFrontChat();
    }
  };

  return (
    <Button
      onClick={handleClick}
      variant={variant}
      size={size}
      className={className}
    >
      <MessageSquare className="h-4 w-4 mr-2" />
      Contact Sales Support
    </Button>
  );
}
