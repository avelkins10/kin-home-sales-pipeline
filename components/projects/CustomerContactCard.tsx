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
  const phone = project[PROJECT_FIELDS.CUSTOMER_PHONE]?.value || ''
  const email = project[PROJECT_FIELDS.CUSTOMER_EMAIL]?.value || ''
  const address = project[PROJECT_FIELDS.CUSTOMER_ADDRESS]?.value || ''
  const city = project[PROJECT_FIELDS.CUSTOMER_CITY]?.value || ''
  const state = project[PROJECT_FIELDS.CUSTOMER_STATE]?.value || ''
  const zip = project[PROJECT_FIELDS.CUSTOMER_ZIP]?.value || ''

  // Build full address
  const fullAddress = [address, city, state, zip].filter(Boolean).join(', ')

  return (
    <Card data-testid="customer-contact">
      <CardHeader>
        <CardTitle className="text-xl font-semibold">Customer Contact</CardTitle>
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
            <div>
              <p className="text-sm text-gray-600 font-medium">Address</p>
              <p className="text-sm text-gray-900">{fullAddress || 'N/A'}</p>
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
