import { differenceInMonths, formatDistanceToNow } from "date-fns"

export type ActivityStatus = "active" | "inactive" | "dormant" | "unknown"

/**
 * Determines user activity status based on last project date
 */
export function getActivityStatus(lastProjectDate: string | undefined): ActivityStatus {
  if (!lastProjectDate) {
    return "unknown"
  }

  try {
    const lastDate = new Date(lastProjectDate)
    const monthsSince = differenceInMonths(new Date(), lastDate)

    if (monthsSince <= 6) {
      return "active"
    } else if (monthsSince <= 12) {
      return "inactive"
    } else {
      return "dormant"
    }
  } catch (error) {
    return "unknown"
  }
}

/**
 * Returns Tailwind color classes for activity indicator dot
 */
export function getActivityColor(status: ActivityStatus): string {
  switch (status) {
    case "active":
      return "bg-green-500"
    case "inactive":
      return "bg-yellow-500"
    case "dormant":
      return "bg-red-500"
    case "unknown":
    default:
      return "bg-gray-400"
  }
}

/**
 * Returns human-readable label for activity status
 */
export function getActivityLabel(status: ActivityStatus): string {
  switch (status) {
    case "active":
      return "Active (last 6 months)"
    case "inactive":
      return "Inactive (6-12 months)"
    case "dormant":
      return "Dormant (12+ months)"
    case "unknown":
    default:
      return "No activity data"
  }
}

/**
 * Formats last activity date as relative time string
 * Returns null to indicate client-only rendering to prevent hydration mismatch
 */
export function formatLastActivity(lastProjectDate: string | undefined): string | null {
  if (!lastProjectDate) {
    return "No recent activity"
  }

  try {
    const lastDate = new Date(lastProjectDate)
    // Return null to signal that this should be rendered client-side only
    // to prevent hydration mismatch from time-relative strings
    if (typeof window === 'undefined') {
      // Server-side: return a stable format
      return lastDate.toLocaleDateString()
    }
    // Client-side: return relative time
    return formatDistanceToNow(lastDate, { addSuffix: true })
  } catch (error) {
    return "Invalid date"
  }
}
