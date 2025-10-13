// lib/utils/formatters.ts

export function formatDate(
  date: string | Date | null | undefined,
  timezone?: string
): string {
  if (!date) return 'N/A';

  try {
    const dateObj = typeof date === 'string' ? new Date(date) : date;
    if (isNaN(dateObj.getTime())) return 'N/A';

    // Use the provided timezone or default to America/New_York
    const effectiveTimezone = timezone || 'America/New_York';

    return new Intl.DateTimeFormat('en-US', {
      year: 'numeric',
      month: 'short',
      day: '2-digit',
      timeZone: effectiveTimezone,
    }).format(dateObj);
  } catch {
    return 'N/A';
  }
}

export function formatCurrency(amount: number | null | undefined): string {
  if (amount === null || amount === undefined) return '$0';

  return new Intl.NumberFormat('en-US', {
    style: 'currency',
    currency: 'USD',
    minimumFractionDigits: 0,
    maximumFractionDigits: 0,
  }).format(amount);
}

export function formatSystemSize(kw: number | null | undefined): string {
  if (kw === null || kw === undefined) return 'N/A';
  
  return `${kw.toFixed(1)} kW`;
}

export function formatPPW(ppw: number | null | undefined): string {
  if (ppw === null || ppw === undefined || ppw === 0) return 'N/A';

  const absValue = Math.abs(ppw);
  const sign = ppw < 0 ? '-' : '';
  return `${sign}$${absValue.toFixed(2)}/W`;
}

export function formatPercentage(value: number | null | undefined, decimals: number = 1): string {
  if (value === null || value === undefined) return '0.0%';
  return `${value.toFixed(decimals)}%`;
}

export function formatLargeNumber(num: number | null | undefined): string {
  if (num === null || num === undefined) return '0';
  if (num >= 1000000) {
    return `${(num / 1000000).toFixed(1)}M`;
  }
  if (num >= 1000) {
    return `${(num / 1000).toFixed(1)}K`;
  }
  return num.toString();
}

export function formatCompactCurrency(amount: number | null | undefined): string {
  if (amount === null || amount === undefined) return '$0';
  if (amount >= 1000000) {
    return `$${(amount / 1000000).toFixed(1)}M`;
  }
  if (amount >= 1000) {
    return `$${(amount / 1000).toFixed(1)}K`;
  }
  return `$${amount.toFixed(0)}`;
}

export function formatDaysAgo(
  date: string | Date | null | undefined,
  timezone?: string
): string {
  if (!date) return 'N/A';

  try {
    const dateObj = typeof date === 'string' ? new Date(date) : date;
    if (isNaN(dateObj.getTime())) return 'N/A';

    // Use the provided timezone or default to America/New_York
    const effectiveTimezone = timezone || 'America/New_York';

    // Get current time in user's timezone
    const now = new Date();

    // Calculate difference in days
    const diffTime = Math.abs(now.getTime() - dateObj.getTime());
    const diffDays = Math.ceil(diffTime / (1000 * 60 * 60 * 24));

    if (diffDays === 0) return 'Today';
    if (diffDays === 1) return 'Yesterday';
    if (diffDays < 7) return `${diffDays} days ago`;
    if (diffDays < 30) return `${Math.floor(diffDays / 7)} weeks ago`;
    if (diffDays < 365) return `${Math.floor(diffDays / 30)} months ago`;
    return `${Math.floor(diffDays / 365)} years ago`;
  } catch {
    return 'N/A';
  }
}

/**
 * Format a date with time in the user's timezone
 */
export function formatDateTime(
  date: string | Date | null | undefined,
  timezone?: string
): string {
  if (!date) return 'N/A';

  try {
    const dateObj = typeof date === 'string' ? new Date(date) : date;
    if (isNaN(dateObj.getTime())) return 'N/A';

    // Use the provided timezone or default to America/New_York
    const effectiveTimezone = timezone || 'America/New_York';

    return new Intl.DateTimeFormat('en-US', {
      year: 'numeric',
      month: 'short',
      day: '2-digit',
      hour: 'numeric',
      minute: '2-digit',
      timeZone: effectiveTimezone,
      timeZoneName: 'short',
    }).format(dateObj);
  } catch {
    return 'N/A';
  }
}
