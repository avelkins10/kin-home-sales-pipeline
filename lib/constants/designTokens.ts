// lib/constants/designTokens.ts
// Design system color tokens for consistent styling across the application

// Event type and status color map - combines both event type (survey/install) and status
export const eventColorMap = {
  survey: {
    scheduled: {
      background: '#dbeafe', // blue-100
      border: '#3b82f6', // blue-500
      text: '#1e40af', // blue-800
    },
    completed: {
      background: '#cffafe', // cyan-100
      border: '#06b6d4', // cyan-500
      text: '#155e75', // cyan-900
    },
    pending: {
      background: '#e0f2fe', // sky-100
      border: '#0ea5e9', // sky-500
      text: '#0c4a6e', // sky-900
    },
  },
  install: {
    scheduled: {
      background: '#f3e8ff', // purple-100
      border: '#a855f7', // purple-500
      text: '#6b21a8', // purple-900
    },
    completed: {
      background: '#dcfce7', // green-100
      border: '#22c55e', // green-500
      text: '#15803d', // green-800
    },
    pending: {
      background: '#fae8ff', // fuchsia-100
      border: '#d946ef', // fuchsia-500
      text: '#701a75', // fuchsia-900
    },
  },
} as const;

// Legacy status-only color map (kept for backwards compatibility)
export const statusColorMap = {
  scheduled: {
    background: '#dbeafe', // blue-100
    border: '#3b82f6', // blue-500
    text: '#1e40af', // blue-800
  },
  completed: {
    background: '#dcfce7', // green-100
    border: '#22c55e', // green-500
    text: '#15803d', // green-800
  },
  pending: {
    background: '#fef3c7', // yellow-100
    border: '#f59e0b', // yellow-500
    text: '#d97706', // yellow-800
  },
  default: {
    background: '#f1f5f9', // slate-100
    border: '#64748b', // slate-500
    text: '#475569', // slate-700
  },
} as const;

// RGB equivalents for computed style comparisons (for tests)
export const eventColorMapRgb = {
  survey: {
    scheduled: {
      background: 'rgb(219, 234, 254)',
      border: 'rgb(59, 130, 246)',
      text: 'rgb(30, 64, 175)',
    },
    completed: {
      background: 'rgb(207, 250, 254)',
      border: 'rgb(6, 182, 212)',
      text: 'rgb(21, 94, 117)',
    },
    pending: {
      background: 'rgb(224, 242, 254)',
      border: 'rgb(14, 165, 233)',
      text: 'rgb(12, 74, 110)',
    },
  },
  install: {
    scheduled: {
      background: 'rgb(243, 232, 255)',
      border: 'rgb(168, 85, 247)',
      text: 'rgb(107, 33, 168)',
    },
    completed: {
      background: 'rgb(220, 252, 231)',
      border: 'rgb(34, 197, 94)',
      text: 'rgb(21, 128, 61)',
    },
    pending: {
      background: 'rgb(250, 232, 255)',
      border: 'rgb(217, 70, 239)',
      text: 'rgb(112, 26, 117)',
    },
  },
} as const;

// RGB equivalents for computed style comparisons
export const statusColorMapRgb = {
  scheduled: {
    background: 'rgb(219, 234, 254)',
    border: 'rgb(59, 130, 246)',
    text: 'rgb(30, 64, 175)',
  },
  completed: {
    background: 'rgb(220, 252, 231)',
    border: 'rgb(34, 197, 94)',
    text: 'rgb(21, 128, 61)',
  },
  pending: {
    background: 'rgb(254, 243, 199)',
    border: 'rgb(245, 158, 11)',
    text: 'rgb(217, 119, 6)',
  },
  default: {
    background: 'rgb(241, 245, 249)',
    border: 'rgb(100, 116, 139)',
    text: 'rgb(71, 85, 105)',
  },
} as const;
