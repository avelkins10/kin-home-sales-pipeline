import { Suspense } from 'react'
import { getServerSession } from 'next-auth'
import { authOptions } from '@/lib/auth/next-auth.config'
import { redirect } from 'next/navigation'
import { getProjectsForUser } from '@/lib/quickbase/queries'
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'
import { calculateMilestoneState } from '@/lib/utils/traffic-lights'
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'

export default async function StatusValidationPage() {
  const session = await getServerSession(authOptions)

  if (!session) {
    redirect('/login')
  }

  // Only allow super_admin
  if (session.user.role !== 'super_admin') {
    return <div className="p-8">Access denied - Super Admin only</div>
  }

  const projects = await getProjectsForUser(session.user.quickbaseUserId, session.user.role)
  const sampleProjects = projects.slice(0, 5) // First 5 projects for debugging

  return (
    <div className="min-h-screen bg-gray-50 p-6">
      <div className="max-w-7xl mx-auto space-y-6">
        <div>
          <h1 className="text-3xl font-bold">Status Validation Debug</h1>
          <p className="text-gray-600">Compare QuickBase field values with calculated milestone states</p>
        </div>

        <div className="space-y-6">
          {sampleProjects.map((project: any) => {
            const recordId = project[PROJECT_FIELDS.RECORD_ID]?.value
            const customerName = project[PROJECT_FIELDS.CUSTOMER_NAME]?.value
            const projectStatus = project[PROJECT_FIELDS.PROJECT_STATUS]?.value

            // Calculate all milestone states
            const intakeState = calculateMilestoneState(project, 'intake')
            const surveyState = calculateMilestoneState(project, 'survey')
            const designState = calculateMilestoneState(project, 'design')
            const nemState = calculateMilestoneState(project, 'nem')
            const permitState = calculateMilestoneState(project, 'permit')
            const installState = calculateMilestoneState(project, 'install')
            const inspectionState = calculateMilestoneState(project, 'inspection')

            // Get raw field values
            // TODO: Add missing field IDs to fieldIds.ts
            // const surveyApproved = project[PROJECT_FIELDS.SURVEY_APPROVED]?.value
            // const surveySubmitted = project[PROJECT_FIELDS.SURVEY_SUBMITTED]?.value
            // const cadDesignApproved = project[PROJECT_FIELDS.CAD_DESIGN_APPROVED]?.value
            // const designCompleted = project[PROJECT_FIELDS.DESIGN_COMPLETED]?.value
            // const nemApproved = project[PROJECT_FIELDS.NEM_APPROVED]?.value
            // const nemSubmitted = project[PROJECT_FIELDS.NEM_SUBMITTED]?.value
            // const permitApproved = project[PROJECT_FIELDS.PERMIT_APPROVED]?.value
            // const permitSubmitted = project[PROJECT_FIELDS.PERMIT_SUBMITTED]?.value
            // const installCompleted = project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value
            // const ptoApproved = project[PROJECT_FIELDS.PTO_APPROVED]?.value

            return (
              <Card key={recordId}>
                <CardHeader>
                  <div className="flex justify-between items-start">
                    <div>
                      <CardTitle>{customerName}</CardTitle>
                      <p className="text-sm text-gray-600">Project #{recordId}</p>
                      <p className="text-sm text-gray-600 mt-1">Status: {projectStatus || 'N/A'}</p>
                    </div>
                  </div>
                </CardHeader>
                <CardContent className="space-y-4">
                  {/* Calculated States */}
                  <div>
                    <h3 className="font-semibold mb-2">Calculated Milestone States</h3>
                    <div className="flex gap-2 flex-wrap">
                      <Badge variant={intakeState === 'complete' ? 'default' : 'secondary'}>
                        Intake: {intakeState}
                      </Badge>
                      <Badge variant={surveyState === 'complete' ? 'default' : 'secondary'}>
                        Survey: {surveyState}
                      </Badge>
                      <Badge variant={designState === 'complete' ? 'default' : 'secondary'}>
                        Design: {designState}
                      </Badge>
                      <Badge variant={nemState === 'complete' ? 'default' : 'secondary'}>
                        NEM: {nemState}
                      </Badge>
                      <Badge variant={permitState === 'complete' ? 'default' : 'secondary'}>
                        Permit: {permitState}
                      </Badge>
                      <Badge variant={installState === 'complete' ? 'default' : 'secondary'}>
                        Install: {installState}
                      </Badge>
                      <Badge variant={inspectionState === 'complete' ? 'default' : 'secondary'}>
                        Inspection: {inspectionState}
                      </Badge>
                    </div>
                  </div>

                  {/* Raw Field Values - Temporarily disabled until field IDs are added */}
                  {/* <div>
                    <h3 className="font-semibold mb-2">Raw QuickBase Field Values</h3>
                    <div className="grid grid-cols-2 gap-2 text-xs font-mono bg-gray-50 p-3 rounded">
                      <div>Survey Submitted: {surveySubmitted || 'null'}</div>
                      <div>Survey Approved: {surveyApproved || 'null'}</div>
                      <div>Design Completed: {designCompleted || 'null'}</div>
                      <div>CAD Design Approved: {cadDesignApproved || 'null'}</div>
                      <div>NEM Submitted: {nemSubmitted || 'null'}</div>
                      <div>NEM Approved: {nemApproved || 'null'}</div>
                      <div>Permit Submitted: {permitSubmitted || 'null'}</div>
                      <div>Permit Approved: {permitApproved || 'null'}</div>
                      <div>Install Completed: {installCompleted || 'null'}</div>
                      <div>PTO Approved: {ptoApproved || 'null'}</div>
                    </div>
                  </div> */}
                </CardContent>
              </Card>
            )
          })}
        </div>
      </div>
    </div>
  )
}
