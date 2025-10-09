import { UserRole } from "@/lib/types/project"

/**
 * Extracts initials from a user's name
 */
export function getInitials(name: string): string {
  if (!name || typeof name !== "string") {
    return "??"
  }

  const trimmed = name.trim()
  if (!trimmed) {
    return "??"
  }

  const words = trimmed.split(/\s+/)
  
  if (words.length === 1) {
    // Single word: take first two letters
    return words[0].substring(0, 2).toUpperCase()
  } else {
    // Multiple words: take first letter of first and last word
    const first = words[0].charAt(0)
    const last = words[words.length - 1].charAt(0)
    return (first + last).toUpperCase()
  }
}

/**
 * Returns Tailwind background color class for avatar based on role
 */
export function getAvatarColor(role: UserRole): string {
  switch (role) {
    case "super_admin":
    case "regional":
      return "bg-purple-500"
    case "divisional":
    case "area_director":
      return "bg-blue-500"
    case "office_leader":
      return "bg-green-500"
    case "team_lead":
      return "bg-cyan-500"
    case "closer":
      return "bg-indigo-500"
    case "setter":
      return "bg-orange-500"
    default:
      return "bg-gray-500"
  }
}

/**
 * Returns Tailwind background color class for avatar based on name hash
 * Ensures consistent colors for the same name
 */
export function getAvatarColorFromName(name: string): string {
  if (!name || typeof name !== "string") {
    return "bg-gray-500"
  }

  // Simple hash function
  let hash = 0
  for (let i = 0; i < name.length; i++) {
    const char = name.charCodeAt(i)
    hash = ((hash << 5) - hash) + char
    hash = hash & hash // Convert to 32-bit integer
  }

  // Map hash to one of 8 predefined colors
  const colors = [
    "bg-red-500",
    "bg-orange-500", 
    "bg-yellow-500",
    "bg-green-500",
    "bg-blue-500",
    "bg-indigo-500",
    "bg-purple-500",
    "bg-pink-500"
  ]

  const index = Math.abs(hash) % colors.length
  return colors[index]
}

/**
 * Returns text color class for avatar (white text on colored backgrounds)
 */
export function getAvatarTextColor(): string {
  return "text-white"
}
