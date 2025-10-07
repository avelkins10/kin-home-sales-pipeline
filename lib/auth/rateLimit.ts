type Key = string

const requests: Map<Key, { count: number; resetAt: number }> = new Map()

export function rateLimit(keyParts: Array<string | undefined | null>, limit = 30, windowMs = 60_000): boolean {
  const key = keyParts.filter(Boolean).join(':')
  const now = Date.now()
  const entry = requests.get(key)
  if (!entry || now > entry.resetAt) {
    requests.set(key, { count: 1, resetAt: now + windowMs })
    return true
  }
  if (entry.count >= limit) return false
  entry.count += 1
  return true
}


