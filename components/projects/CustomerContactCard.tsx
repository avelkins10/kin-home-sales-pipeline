'use client'

import { Phone, Mail, MapPin, MessageSquare } from 'lucide-react'
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'
import { QuickbaseProject } from '@/lib/types/project'

interface CustomerContactCardProps {
  project: QuickbaseProject
}

export function CustomerContactCard({ project }: CustomerContactCardProps) {
  // Extract customer contact data - Field IDs from fieldIds.ts
  const customerName = project[PROJECT_FIELDS.CUSTOMER_NAME]?.value || 'N/A'
  const phoneRaw = project[PROJECT_FIELDS.CUSTOMER_PHONE]?.value || '' // Field 148

  // WORKAROUND: QuickBase has the email in the CITY field (149) instead of EMAIL field (147)
  // This appears to be a QuickBase data migration issue
  const emailRaw = project[PROJECT_FIELDS.CUSTOMER_CITY]?.value || '' // Field 149 has email

  // The ADDRESS field (146) contains the full address, which is cleaner than trying to piece together city/state/zip
  const addressRaw = project[PROJECT_FIELDS.CUSTOMER_ADDRESS]?.value || '' // Field 146

  // Clean phone - extract just the number part if it's an object or has extra data
  let phone = ''
  if (typeof phoneRaw === 'string') {
    phone = phoneRaw.trim()
  } else if (phoneRaw && typeof phoneRaw === 'object' && 'value' in phoneRaw) {
    phone = String(phoneRaw.value).trim()
  }

  // Clean email - extract just the email part if it's an object or has extra data
  let email = ''
  if (typeof emailRaw === 'string') {
    // Extract email using regex if it's buried in text
    const emailMatch = emailRaw.match(/[\w.-]+@[\w.-]+\.\w+/)
    email = emailMatch ? emailMatch[0] : emailRaw.trim()
  } else if (emailRaw && typeof emailRaw === 'object' && 'value' in emailRaw) {
    email = String(emailRaw.value).trim()
  }

  // The ADDRESS field (146) contains the full address already formatted correctly
  // Just use it as-is
  const fullAddress = typeof addressRaw === 'string' ? addressRaw.trim() : ''

  // Build Google Maps URL
  const mapsUrl = fullAddress ? `https://www.google.com/maps/search/?api=1&query=${encodeURIComponent(fullAddress)}` : ''

  return (
    <Card data-testid="customer-contact">
      <CardHeader>
        <div className="flex items-center justify-between">
          <CardTitle className="text-xl font-semibold">Customer Contact</CardTitle>
          <span className="text-xs text-gray-400">v2.0</span>
        </div>
      </CardHeader>
      <CardContent className="space-y-4">
        {/* Contact Information */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          {/* Phone */}
          <div className="flex items-center gap-3">
            <Phone className="w-4 h-4 text-gray-400" />
            <div>
              <p className="text-sm text-gray-600 font-medium">Phone</p>
              <p className="text-sm text-gray-900">{phone || 'N/A'}</p>
            </div>
          </div>

          {/* Email */}
          <div className="flex items-center gap-3">
            <Mail className="w-4 h-4 text-gray-400" />
            <div>
              <p className="text-sm text-gray-600 font-medium">Email</p>
              <p className="text-sm text-gray-900">{email || 'N/A'}</p>
            </div>
          </div>

          {/* Address */}
          <div className="flex items-start gap-3 md:col-span-2">
            <MapPin className="w-4 h-4 text-gray-400 mt-0.5" />
            <div className="flex-1">
              <p className="text-sm text-gray-600 font-medium">Address</p>
              {fullAddress ? (
                <a
                  href={mapsUrl}
                  target="_blank"
                  rel="noopener noreferrer"
                  className="text-sm text-blue-600 hover:text-blue-800 hover:underline"
                >
                  {fullAddress}
                </a>
              ) : (
                <p className="text-sm text-gray-900">N/A</p>
              )}
            </div>
          </div>
        </div>

        {/* Action Buttons */}
        <div className="flex flex-col sm:flex-row gap-2 pt-4 border-t border-gray-200">
          <Button
            asChild
            className="flex-1"
            disabled={!phone}
          >
            <a href={phone ? `tel:${phone}` : '#'}>
              <Phone className="w-4 h-4 mr-2" />
              Call
            </a>
          </Button>
          
          <Button
            asChild
            variant="outline"
            className="flex-1"
            disabled={!phone}
          >
            <a href={phone ? `sms:${phone}` : '#'}>
              <MessageSquare className="w-4 h-4 mr-2" />
              Text
            </a>
          </Button>
          
          <Button
            asChild
            variant="outline"
            className="flex-1"
            disabled={!email}
          >
            <a href={email ? `mailto:${email}` : '#'}>
              <Mail className="w-4 h-4 mr-2" />
              Email
            </a>
          </Button>
        </div>
      </CardContent>
    </Card>
  )
}
