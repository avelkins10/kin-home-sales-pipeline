'use client'

import { useEffect, useState } from 'react'
import { Alert, AlertDescription } from '@/components/ui/alert'
import { Button } from '@/components/ui/button'
import { AlertTriangle, Check, X } from 'lucide-react'
import { getBaseUrl } from '@/lib/utils/baseUrl'

interface EmailConfigStatus {
  valid: boolean
  environment: Record<string, string>
  instructions?: string
}

export function EmailConfigBanner() {
  const [status, setStatus] = useState<EmailConfigStatus | null>(null)
  const [loading, setLoading] = useState(true)
  const [dismissed, setDismissed] = useState(false)

  useEffect(() => {
    checkEmailConfig()
  }, [])

  const checkEmailConfig = async () => {
    try {
      const response = await fetch(`${getBaseUrl()}/api/test-email`)

      // If endpoint doesn't exist yet or returns error, fail silently
      if (!response.ok) {
        setLoading(false)
        return
      }

      const data = await response.json()

      setStatus({
        valid: data.emailConfig?.valid || false,
        environment: data.environment || {},
        instructions: data.instructions
      })
    } catch (error) {
      console.error('Failed to check email config:', error)
      // Fail silently - don't show banner if check fails
    } finally {
      setLoading(false)
    }
  }

  if (loading || dismissed || status?.valid) {
    return null
  }

  return (
    <Alert className="bg-amber-50 border-amber-300 mb-4">
      <div className="flex items-start gap-3">
        <AlertTriangle className="h-5 w-5 text-amber-600 mt-0.5" />
        <div className="flex-1">
          <div className="font-semibold text-amber-900 mb-1">
            Email System Not Configured
          </div>
          <AlertDescription className="text-amber-800 text-sm">
            Email notifications are currently disabled. User invites will be created but emails won't be sent automatically.
            You'll need to manually share invite links with new users.
          </AlertDescription>
          {status && (
            <div className="mt-3 space-y-2">
              <div className="text-xs font-medium text-amber-900">Environment Status:</div>
              <div className="flex flex-wrap gap-2 text-xs">
                {Object.entries(status.environment).map(([key, value]) => (
                  <div
                    key={key}
                    className={`inline-flex items-center gap-1 px-2 py-1 rounded ${
                      value === 'true' || value === 'Set' || value === '✓ Set'
                        ? 'bg-green-100 text-green-800'
                        : 'bg-red-100 text-red-800'
                    }`}
                  >
                    {value === 'true' || value === 'Set' || value === '✓ Set' ? (
                      <Check className="h-3 w-3" />
                    ) : (
                      <X className="h-3 w-3" />
                    )}
                    <span>{key}</span>
                  </div>
                ))}
              </div>
            </div>
          )}
          <div className="mt-3 flex gap-2">
            <Button
              size="sm"
              variant="outline"
              className="h-8 text-xs"
              onClick={() => window.open('https://docs.claude.com/en/docs/claude-code', '_blank')}
            >
              View Setup Guide
            </Button>
            <Button
              size="sm"
              variant="ghost"
              className="h-8 text-xs text-amber-700 hover:text-amber-900"
              onClick={() => setDismissed(true)}
            >
              Dismiss
            </Button>
          </div>
        </div>
      </div>
    </Alert>
  )
}
