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
  // Extract customer contact data
  const customerName = project[PROJECT_FIELDS.CUSTOMER_NAME]?.value || 'N/A'
  const phoneRaw = project[PROJECT_FIELDS.CUSTOMER_PHONE]?.value || ''
  const emailRaw = project[PROJECT_FIELDS.CUSTOMER_EMAIL]?.value || ''
  const addressRaw = project[PROJECT_FIELDS.CUSTOMER_ADDRESS]?.value || ''
  const city = project[PROJECT_FIELDS.CUSTOMER_CITY]?.value || ''
  const state = project[PROJECT_FIELDS.CUSTOMER_STATE]?.value || ''
  const zip = project[PROJECT_FIELDS.CUSTOMER_ZIP]?.value || ''

  // Clean phone - sometimes formatted as "(407) 924-3849" or might have extra data
  const phone = typeof phoneRaw === 'string' ? phoneRaw.trim() : ''

  // Clean email - sometimes might have extra data
  const email = typeof emailRaw === 'string' ? emailRaw.trim() : ''

  // The CUSTOMER_ADDRESS field (146) often contains concatenated data
  // Use dedicated city/state/zip fields instead
  // Only parse the raw address if the dedicated fields are empty
  let address = ''

  if (addressRaw && typeof addressRaw === 'string') {
    // If we have dedicated city/state/zip, just extract street from raw address
    if (city || state || zip) {
      // Remove URLs and emails, then take first part before comma
      let cleaned = addressRaw.replace(/https?:\/\/[^\s,]+/g, '').trim()
      cleaned = cleaned.replace(/[\w.-]+@[\w.-]+\.\w+/g, '').trim()
      cleaned = cleaned.replace(/,+/g, ',').replace(/\s+/g, ' ').trim()
      cleaned = cleaned.replace(/^,|,$/g, '').trim()
      const parts = cleaned.split(',').map(p => p.trim()).filter(Boolean)
      address = parts[0] || ''
    } else {
      // No dedicated fields, try to parse everything from raw address
      address = addressRaw.trim()
    }
  }

  // Build full address for display
  const fullAddress = [address, city, state, zip].filter(Boolean).join(', ')

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
                  <span className="block">{address}</span>
                  <span className="block">{[city, state, zip].filter(Boolean).join(', ')}</span>
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
