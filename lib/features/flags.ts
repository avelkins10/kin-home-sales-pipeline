// lib/features/flags.ts
interface FeatureFlags {
  AI_HOLD_RESOLUTION_ENABLED: boolean;
  AI_HOLD_RESOLUTION_CATEGORIES?: string[];
  AI_HOLD_RESOLUTION_USERS?: string[];
  AI_SMART_NOTIFICATIONS_ENABLED: boolean;
}

const defaultFeatureFlags: FeatureFlags = {
  AI_HOLD_RESOLUTION_ENABLED: process.env.NEXT_PUBLIC_AI_HOLD_RESOLUTION_ENABLED === 'true',
  AI_HOLD_RESOLUTION_CATEGORIES: process.env.NEXT_PUBLIC_AI_HOLD_RESOLUTION_CATEGORIES
    ? process.env.NEXT_PUBLIC_AI_HOLD_RESOLUTION_CATEGORIES.split(',')
    : undefined,
  AI_HOLD_RESOLUTION_USERS: process.env.NEXT_PUBLIC_AI_HOLD_RESOLUTION_USERS
    ? process.env.NEXT_PUBLIC_AI_HOLD_RESOLUTION_USERS.split(',')
    : undefined,
  AI_SMART_NOTIFICATIONS_ENABLED: process.env.NEXT_PUBLIC_AI_SMART_NOTIFICATIONS_ENABLED === 'true',
};

export function getFeatureFlags(userId?: string): FeatureFlags {
  const flags = { ...defaultFeatureFlags };

  if (userId && flags.AI_HOLD_RESOLUTION_USERS) {
    flags.AI_HOLD_RESOLUTION_ENABLED = flags.AI_HOLD_RESOLUTION_ENABLED && flags.AI_HOLD_RESOLUTION_USERS.includes(userId);
  }

  return flags;
}

export function isAIHoldResolutionEnabled(userId?: string): boolean {
  const flags = getFeatureFlags(userId);
  return flags.AI_HOLD_RESOLUTION_ENABLED;
}

export function isAISmartNotificationsEnabled(userId?: string): boolean {
  const flags = getFeatureFlags(userId);
  return flags.AI_SMART_NOTIFICATIONS_ENABLED;
}

export function isFeatureEnabled(feature: string, role?: string): boolean {
  // Generic feature flag checker
  switch (feature) {
    case 'ai_hold_analysis':
      return isAIHoldResolutionEnabled();
    case 'ai_smart_notifications':
      return isAISmartNotificationsEnabled();
    default:
      return true; // Default to enabled for unknown features
  }
}