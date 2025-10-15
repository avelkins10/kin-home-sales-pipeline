'use client'

import { User, Phone, MessageSquare } from 'lucide-react'
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'
import { QuickbaseProject } from '@/lib/types/project'

interface TeamMembersCardProps {
  project: QuickbaseProject
}

export function TeamMembersCard({ project }: TeamMembersCardProps) {
  // Extract team member data
  const closerName = project[PROJECT_FIELDS.CLOSER_NAME]?.value || ''
  const closerId = project[PROJECT_FIELDS.CLOSER_ID]?.value || ''
  const closerEmail = project[PROJECT_FIELDS.CLOSER_EMAIL]?.value || ''
  const closerPhone = project[PROJECT_FIELDS.CLOSER_PHONE]?.value || ''

  const setterName = project[PROJECT_FIELDS.SETTER_NAME]?.value || ''
  const setterId = project[PROJECT_FIELDS.SETTER_ID]?.value || ''
  const setterEmail = project[PROJECT_FIELDS.SETTER_EMAIL]?.value || ''
  const setterPhone = project[PROJECT_FIELDS.SETTER_PHONE]?.value || ''

  const coordinatorName = project[PROJECT_FIELDS.PROJECT_COORDINATOR]?.value || ''
  const coordinatorId = project[PROJECT_FIELDS.PROJECT_COORDINATOR_ID]?.value || ''
  const coordinatorEmail = project[PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL]?.value || ''
  const coordinatorPhone = project[PROJECT_FIELDS.PROJECT_COORDINATOR_PHONE]?.value || ''

  const salesOffice = project[PROJECT_FIELDS.SALES_OFFICE]?.value || ''

  // Team members array with phone contact information
  const teamMembers = [
    {
      role: 'Closer',
      name: closerName,
      id: closerId,
      phone: closerPhone,
      hasContact: !!closerPhone
    },
    {
      role: 'Setter',
      name: setterName,
      id: setterId,
      phone: setterPhone,
      hasContact: !!setterPhone
    },
    {
      role: 'Coordinator',
      name: coordinatorName,
      id: coordinatorId,
      phone: coordinatorPhone,
      hasContact: !!coordinatorPhone
    }
  ].filter(member => member.name) // Only show members with names

  return (
    <Card>
      <CardHeader>
        <div className="flex items-center justify-between">
          <CardTitle className="text-xl font-semibold">Team Members</CardTitle>
          {salesOffice && (
            <Badge variant="secondary" className="bg-blue-100 text-blue-700">
              {salesOffice}
            </Badge>
          )}
        </div>
      </CardHeader>
      <CardContent>
        {teamMembers.length > 0 ? (
          <div className="space-y-4">
            {teamMembers.map((member, index) => (
              <div key={member.role}>
                <div className="flex items-center gap-3">
                  {/* Avatar */}
                  <div className="w-10 h-10 rounded-full bg-blue-100 flex items-center justify-center">
                    <User className="w-5 h-5 text-blue-600" />
                  </div>

                  {/* Member Info */}
                  <div className="flex-1">
                    <p className="text-xs uppercase text-gray-500 font-medium">
                      {member.role}
                    </p>
                    <p 
                      className="text-sm text-gray-900 font-semibold"
                      data-testid={member.role.toLowerCase() === 'closer' ? 'closer-name' : undefined}
                    >
                      {member.name}
                    </p>
                  </div>

                  {/* Action Buttons */}
                  <div className="flex gap-1">
                    <Button
                      asChild
                      variant="ghost"
                      size="sm"
                      disabled={!member.phone}
                      className="h-8 w-8 p-0"
                      title={member.phone ? `Call ${member.name}` : 'Phone number not available'}
                    >
                      <a href={member.phone ? `tel:${member.phone}` : '#'}>
                        <Phone className="w-4 h-4" />
                      </a>
                    </Button>
                    <Button
                      asChild
                      variant="ghost"
                      size="sm"
                      disabled={!member.phone}
                      className="h-8 w-8 p-0"
                      title={member.phone ? `Text ${member.name}` : 'Phone number not available'}
                    >
                      <a href={member.phone ? `sms:${member.phone}` : '#'}>
                        <MessageSquare className="w-4 h-4" />
                      </a>
                    </Button>
                  </div>
                </div>

                {/* Divider */}
                {index < teamMembers.length - 1 && (
                  <div className="border-b border-gray-200 py-3" />
                )}
              </div>
            ))}
          </div>
        ) : (
          <div className="text-center py-6">
            <User className="w-12 h-12 text-gray-300 mx-auto mb-2" />
            <p className="text-sm text-gray-500">No team members assigned</p>
          </div>
        )}

        {/* Contact Note */}
        {teamMembers.length > 0 && teamMembers.some(m => !m.hasContact) && (
          <div className="mt-4 p-3 bg-gray-50 rounded-lg">
            <p className="text-xs text-gray-600">
              Some contact information may not be available. Check Quickbase for additional details.
            </p>
          </div>
        )}
      </CardContent>
    </Card>
  )
}
