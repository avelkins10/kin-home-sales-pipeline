'use client'

import { useQuery } from '@tanstack/react-query'
import { Plus, CheckCircle, Clock, DollarSign } from 'lucide-react'
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'
import { ADDER_FIELDS } from '@/lib/constants/adderFieldIds'
import { QuickbaseProject } from '@/lib/types/project'
import { formatCurrency } from '@/lib/utils/formatters'

interface AddersCardProps {
  project: QuickbaseProject
}

interface Adder {
  [key: number]: { value: string | number }
}

export function AddersCard({ project }: AddersCardProps) {
  const recordId = project[PROJECT_FIELDS.RECORD_ID]?.value

  // Fetch adders from API
  const { data: adders = [], isLoading } = useQuery({
    queryKey: ['adders', recordId],
    queryFn: async () => {
      const response = await fetch(`/api/projects/${recordId}/adders`)
      if (!response.ok) throw new Error('Failed to fetch adders')
      return response.json() as Promise<Adder[]>
    },
    enabled: !!recordId,
    staleTime: 5 * 60 * 1000, // 5 minutes
  })

  // Calculate totals
  const totalAdders = adders.length
  const totalAdderCost = adders.reduce((sum, adder) => {
    const cost = parseFloat(String(adder[ADDER_FIELDS.TOTAL_COST]?.value || '0'))
    return sum + cost
  }, 0)

  // Count statuses
  const approvedAdders = adders.filter(adder => {
    const status = String(adder[ADDER_FIELDS.STATUS]?.value || '').toLowerCase()
    return status.includes('approved')
  }).length

  const needsReviewAdders = adders.filter(adder => {
    const status = String(adder[ADDER_FIELDS.STATUS]?.value || '').toLowerCase()
    return status.includes('pending') || status.includes('review')
  }).length

  const needsReview = needsReviewAdders > 0

  if (isLoading) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="text-xl font-semibold">Adders</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-center py-6">
            <div className="animate-pulse">
              <div className="h-4 bg-gray-200 rounded w-3/4 mx-auto mb-2"></div>
              <div className="h-4 bg-gray-200 rounded w-1/2 mx-auto"></div>
            </div>
          </div>
        </CardContent>
      </Card>
    )
  }

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
            <div className="border-t border-gray-200 pt-4">
              <h4 className="text-sm font-medium text-gray-600 mb-3">
                Included Adders ({totalAdders})
              </h4>
              <div className="space-y-2.5 max-h-64 overflow-y-auto">
                {adders.map((adder, index) => {
                  const productName = String(adder[ADDER_FIELDS.PRODUCT_NAME]?.value || `Adder #${index + 1}`)
                  const cost = parseFloat(String(adder[ADDER_FIELDS.TOTAL_COST]?.value || '0'))
                  const status = String(adder[ADDER_FIELDS.STATUS]?.value || 'Unknown')
                  const whosPaying = String(adder[ADDER_FIELDS.WHOS_PAYING]?.value || '')
                  const qty = parseInt(String(adder[ADDER_FIELDS.QTY]?.value || '1'))

                  return (
                    <div
                      key={adder[ADDER_FIELDS.RECORD_ID]?.value || index}
                      className="flex items-start justify-between gap-3 py-2 px-3 bg-slate-50 rounded-lg hover:bg-slate-100 transition-colors"
                    >
                      <div className="flex-1 min-w-0 space-y-1">
                        <div className="flex items-start gap-2">
                          <Plus className="w-3.5 h-3.5 text-emerald-600 flex-shrink-0 mt-0.5" />
                          <div className="flex-1">
                            <span className="text-sm text-gray-800 font-medium block">
                              {productName}
                            </span>
                            <div className="flex items-center gap-2 mt-1">
                              {qty > 1 && (
                                <span className="text-xs text-gray-500">Qty: {qty}</span>
                              )}
                              {whosPaying && (
                                <span className="text-xs text-gray-500">
                                  {whosPaying.includes('Rep') ? '👤 Rep' : '🏠 Customer'}
                                </span>
                              )}
                              <span className={`text-xs px-1.5 py-0.5 rounded ${
                                status.toLowerCase().includes('approved')
                                  ? 'bg-green-100 text-green-700'
                                  : status.toLowerCase().includes('pending')
                                  ? 'bg-yellow-100 text-yellow-700'
                                  : 'bg-gray-100 text-gray-600'
                              }`}>
                                {status}
                              </span>
                            </div>
                          </div>
                        </div>
                      </div>
                      <div className="text-right flex-shrink-0">
                        <span className="text-sm font-semibold text-gray-900 whitespace-nowrap">
                          {formatCurrency(cost)}
                        </span>
                      </div>
                    </div>
                  )
                })}
              </div>
            </div>
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
