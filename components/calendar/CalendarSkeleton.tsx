import { Card, CardContent, CardHeader } from '@/components/ui/card'
import { cn } from '@/lib/utils/cn'

export const CalendarSkeleton = () => {
  return (
    <Card data-testid="loading-skeleton" className="bg-white rounded-lg border shadow-sm">
      <CardHeader>
        <div className="h-8 w-48 bg-slate-200 rounded animate-pulse" />
      </CardHeader>
      <CardContent>
        {/* Skeleton toolbar */}
        <div className="h-10 w-full bg-slate-200 rounded mb-4 animate-pulse" />
        
        {/* Skeleton calendar grid */}
        <div className="grid grid-cols-7 gap-2">
          {/* Generate 35 skeleton cells (7 columns × 5 rows) */}
          {Array.from({ length: 35 }).map((_, index) => (
            <div
              key={index}
              className="h-20 bg-slate-100 rounded p-2 space-y-1"
            >
              {/* Small skeleton rectangles inside each cell */}
              <div className="h-3 w-16 bg-slate-200 rounded animate-pulse" />
              <div className="h-2 w-12 bg-slate-200 rounded animate-pulse" />
              <div className="h-2 w-14 bg-slate-200 rounded animate-pulse" />
            </div>
          ))}
        </div>
      </CardContent>
    </Card>
  )
}
