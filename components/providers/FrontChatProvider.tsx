'use client';

import { useEffect, useState } from 'react';
import { useSession } from 'next-auth/react';
import { loadFrontChatScript, initializeFrontChat } from '@/lib/integrations/frontChat';
import { getBaseUrl } from '@/lib/utils/baseUrl';

export function FrontChatProvider({ children }: { children: React.ReactNode }) {
  const { data: session, status } = useSession();
  const [isInitialized, setIsInitialized] = useState(false);

  useEffect(() => {
    // Only initialize if user is authenticated and we haven't initialized yet
    if (status !== 'authenticated' || !session?.user || isInitialized) {
      return;
    }

    const chatId = process.env.NEXT_PUBLIC_FRONT_CHAT_ID;
    if (!chatId) {
      console.warn('[FrontChat] Chat ID not configured');
      return;
    }

    async function setupFrontChat() {
      try {
        // Load the Front Chat script
        await loadFrontChatScript(chatId!);
        console.log('[FrontChat] Script loaded successfully');

        // Fetch user hash from API
        const baseUrl = getBaseUrl();
        const response = await fetch(`${baseUrl}/api/chat/user-hash`);

        if (!response.ok) {
          console.error('[FrontChat] Failed to fetch user hash');
          return;
        }

        const { userHash, email } = await response.json();

        // Initialize Front Chat with user identity
        initializeFrontChat(chatId!, {
          email,
          name: session?.user?.name || email,
          userHash,
        });

        setIsInitialized(true);
      } catch (error) {
        console.error('[FrontChat] Setup error:', error);
      }
    }

    setupFrontChat();
  }, [status, session, isInitialized]);

  return <>{children}</>;
}
