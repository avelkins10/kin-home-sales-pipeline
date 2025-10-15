// Front Chat integration utilities

// Extend Window interface to include FrontChat
declare global {
  interface Window {
    FrontChat?: (command: string, ...args: any[]) => void;
  }
}

export interface ProjectContext {
  projectId: string;
  customerName: string;
  recordId: number;
}

export interface FrontChatIdentity {
  email: string;
  name?: string;
  userHash: string;
}

/**
 * Load the Front Chat script
 */
export function loadFrontChatScript(chatId: string): Promise<void> {
  return new Promise((resolve, reject) => {
    // Check if already loaded
    if (window.FrontChat) {
      resolve();
      return;
    }

    // Create script element
    const script = document.createElement('script');
    script.src = 'https://chat-assets.frontapp.com/v1/chat.bundle.js';
    script.async = true;

    script.onload = () => {
      // Wait a bit for FrontChat to be available
      const checkInterval = setInterval(() => {
        if (window.FrontChat) {
          clearInterval(checkInterval);
          resolve();
        }
      }, 100);

      // Timeout after 5 seconds
      setTimeout(() => {
        clearInterval(checkInterval);
        if (!window.FrontChat) {
          reject(new Error('Front Chat failed to load'));
        }
      }, 5000);
    };

    script.onerror = () => {
      reject(new Error('Failed to load Front Chat script'));
    };

    document.head.appendChild(script);
  });
}

/**
 * Initialize Front Chat with user identity
 */
export function initializeFrontChat(
  chatId: string,
  identity?: FrontChatIdentity
): void {
  if (!window.FrontChat) {
    console.error('[FrontChat] SDK not loaded');
    return;
  }

  try {
    // Match widget configuration: useDefaultLauncher: true, optional identity
    const config: any = {
      chatId,
      useDefaultLauncher: true, // Use Front's default launcher button
    };

    // Only add identity if we have a valid userHash for identity verification
    // Without userHash, Front Chat expects no identity info (simple mode)
    if (identity?.userHash) {
      config.email = identity.email;
      config.name = identity.name;
      config.userHash = identity.userHash;
    }

    window.FrontChat('init', config);

    console.log('[FrontChat] Initialized successfully');
  } catch (error) {
    console.error('[FrontChat] Initialization error:', error);
  }
}

/**
 * Open the Front Chat window
 */
export function showFrontChat(): void {
  if (!window.FrontChat) {
    console.error('[FrontChat] SDK not loaded');
    return;
  }

  try {
    window.FrontChat('show');
  } catch (error) {
    console.error('[FrontChat] Error showing chat:', error);
  }
}

/**
 * Close the Front Chat window
 */
export function hideFrontChat(): void {
  if (!window.FrontChat) {
    console.error('[FrontChat] SDK not loaded');
    return;
  }

  try {
    window.FrontChat('hide');
  } catch (error) {
    console.error('[FrontChat] Error hiding chat:', error);
  }
}

/**
 * Build QuickBase project URL
 */
function buildQuickBaseProjectUrl(recordId: number): string {
  const realm = process.env.NEXT_PUBLIC_QUICKBASE_REALM || 'kin.quickbase.com';
  const tableId = process.env.NEXT_PUBLIC_QUICKBASE_TABLE_PROJECTS;

  if (!tableId) {
    console.warn('[FrontChat] QuickBase table ID not configured');
    return '';
  }

  return `https://${realm}/db/${tableId}?a=dr&rid=${recordId}`;
}

/**
 * Set project context as custom fields in Front Chat
 */
export function setProjectContext(context: ProjectContext): void {
  if (!window.FrontChat) {
    console.error('[FrontChat] SDK not loaded');
    return;
  }

  const projectUrl = buildQuickBaseProjectUrl(context.recordId);

  try {
    window.FrontChat('identity', {
      customFields: {
        project_url: projectUrl,
        customer_name: context.customerName,
        project_id: context.projectId,
      },
    });

    console.log('[FrontChat] Project context set:', {
      customer: context.customerName,
      projectId: context.projectId,
      url: projectUrl,
    });
  } catch (error) {
    console.error('[FrontChat] Error setting project context:', error);
  }
}

/**
 * Open chat with project context
 */
export function openChatWithProject(context: ProjectContext): void {
  setProjectContext(context);
  showFrontChat();
}
