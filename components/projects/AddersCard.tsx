'use client'

import { Plus, CheckCircle, Clock } from 'lucide-react'
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'
import { QuickbaseProject } from '@/lib/types/project'
import { formatCurrency } from '@/lib/utils/formatters'

interface AddersCardProps {
  project: QuickbaseProject
}

export function AddersCard({ project }: AddersCardProps) {
  // Extract adders data
  const totalAdders = parseInt(project[PROJECT_FIELDS.TOTAL_ADDERS]?.value || '0')
  const totalAdderCost = parseFloat(project[PROJECT_FIELDS.TOTAL_ADDER_COST]?.value || '0')
  const salesFacingAdderList = project[PROJECT_FIELDS.SALES_FACING_ADDER_LIST]?.value || ''
  const approvedAdders = parseInt(project[PROJECT_FIELDS.NUM_APPROVED_ADDERS]?.value || '0')
  const needsReviewAdders = parseInt(project[PROJECT_FIELDS.NUM_NEEDS_REVIEW_ADDERS]?.value || '0')

  // Parse adder list
  const adderList = salesFacingAdderList
    ? salesFacingAdderList.split(',').map((adder: string) => adder.trim()).filter(Boolean)
    : []

  // Check if needs review (urgency indicator)
  const needsReview = needsReviewAdders > 0

  return (
    <Card className={needsReview ? 'border-orange-200' : ''}>
      <CardHeader>
        <div className="flex items-center justify-between">
          <CardTitle className="text-xl font-semibold">Adders</CardTitle>
          {totalAdders > 0 && (
            <Badge variant="secondary">
              {totalAdders} {totalAdders === 1 ? 'Adder' : 'Adders'}
            </Badge>
          )}
        </div>
      </CardHeader>
      <CardContent className="space-y-4">
        {totalAdders > 0 ? (
          <>
            {/* Summary Row */}
            <div className="space-y-3">
              {/* Total Cost */}
              <div className="text-center">
                <p className="text-2xl font-bold text-gray-900">
                  {formatCurrency(totalAdderCost)}
                </p>
                <p className="text-sm text-gray-600">Total Adder Cost</p>
              </div>

              {/* Status Badges */}
              <div className="flex justify-center gap-2">
                {approvedAdders > 0 && (
                  <Badge variant="secondary" className="bg-green-100 text-green-700">
                    <CheckCircle className="w-3 h-3 mr-1" />
                    {approvedAdders} Approved
                  </Badge>
                )}
                {needsReviewAdders > 0 && (
                  <Badge variant="secondary" className="bg-yellow-100 text-yellow-700">
                    <Clock className="w-3 h-3 mr-1" />
                    {needsReviewAdders} Needs Review
                  </Badge>
                )}
              </div>
            </div>

            {/* Adder List */}
            {adderList.length > 0 && (
              <div className="border-t border-gray-200 pt-4">
                <h4 className="text-sm font-medium text-gray-600 mb-3">Adder List</h4>
                <div className="space-y-2 max-h-48 overflow-y-auto">
                  {adderList.slice(0, 10).map((adder: string, index: number) => (
                    <div key={index} className="flex items-center gap-2">
                      <Plus className="w-3 h-3 text-gray-400 flex-shrink-0" />
                      <span className="text-sm text-gray-700">{adder}</span>
                    </div>
                  ))}
                  {adderList.length > 10 && (
                    <div className="flex items-center gap-2 text-sm text-gray-500">
                      <Plus className="w-3 h-3 text-gray-400 flex-shrink-0" />
                      <span>+ {adderList.length - 10} more adders</span>
                    </div>
                  )}
                </div>
              </div>
            )}
          </>
        ) : (
          /* Empty State */
          <div className="text-center py-6">
            <Plus className="w-12 h-12 text-gray-300 mx-auto mb-2" />
            <p className="text-sm text-gray-500">No adders on this project</p>
          </div>
        )}

        {/* Urgency Indicator */}
        {needsReview && (
          <div className="mt-4 p-3 bg-orange-50 border border-orange-200 rounded-lg">
            <p className="text-sm text-orange-700">
              <Clock className="w-4 h-4 inline mr-1" />
              Action needed: {needsReviewAdders} adder{needsReviewAdders > 1 ? 's' : ''} require{needsReviewAdders === 1 ? 's' : ''} review
            </p>
          </div>
        )}
      </CardContent>
    </Card>
  )
}
