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

        // Try to fetch user hash for identity verification
        try {
          const baseUrl = getBaseUrl();
          const response = await fetch(`${baseUrl}/api/chat/user-hash`);

          if (response.ok) {
            const { userHash, email } = await response.json();

            // Initialize Front Chat with verified user identity
            initializeFrontChat(chatId!, {
              email,
              name: session?.user?.name || email,
              userHash,
            });
            console.log('[FrontChat] Initialized with user identity');
          } else {
            console.warn('[FrontChat] User hash fetch failed, using simple mode');
            // Fall back to simple mode without identity
            initializeFrontChat(chatId!);
          }
        } catch (hashError) {
          console.warn('[FrontChat] Identity verification failed, using simple mode:', hashError);
          // Fall back to simple mode without identity
          initializeFrontChat(chatId!);
        }

        setIsInitialized(true);
      } catch (error) {
        console.error('[FrontChat] Setup error:', error);
      }
    }

    setupFrontChat();
  }, [status, session, isInitialized]);

  return <>{children}</>;
}
