// lib/constants/designTokens.ts
// Design system color tokens for consistent styling across the application

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
