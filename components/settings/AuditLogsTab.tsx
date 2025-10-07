'use client'

import { useState, useRef, useEffect } from 'react'
import { useQuery } from '@tanstack/react-query'
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from '@/components/ui/table'
import { Input } from '@/components/ui/input'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select'
import { Calendar } from '@/components/ui/calendar'
import { Popover, PopoverContent, PopoverTrigger } from '@/components/ui/popover'
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from '@/components/ui/dialog'
import { FileDown, Calendar as CalendarIcon, Search, Eye, Loader2 } from 'lucide-react'
import { format } from 'date-fns'
import { toast } from 'sonner'
import type { AuditLog } from '@/lib/types/audit'

export function AuditLogsTab() {
  const [dateRange, setDateRange] = useState<{ from: Date; to: Date }>({
    from: new Date(Date.now() - 30 * 24 * 60 * 60 * 1000), // 30 days ago
    to: new Date(),
  })
  const [actionFilter, setActionFilter] = useState<string>('all')
  const [searchQuery, setSearchQuery] = useState('')
  const [selectedLog, setSelectedLog] = useState<AuditLog | null>(null)
  const [page, setPage] = useState(1)
  const [isExporting, setIsExporting] = useState(false)
  const abortControllerRef = useRef<AbortController | null>(null)

  // Fetch audit logs
  const { data: logsData, isLoading } = useQuery({
    queryKey: ['audit-logs', dateRange, actionFilter, searchQuery, page],
    queryFn: async () => {
      const params = new URLSearchParams({
        from: dateRange.from.toISOString(),
        to: dateRange.to.toISOString(),
        page: page.toString(),
        limit: '50',
      })

      if (actionFilter !== 'all') params.set('action', actionFilter)
      if (searchQuery) params.set('search', searchQuery)

      const response = await fetch(`/api/admin/audit-logs?${params.toString()}`)
      if (!response.ok) throw new Error('Failed to fetch audit logs')
      return response.json() as Promise<{ logs: AuditLog[]; total: number; pages: number }>
    },
  })

  // Cleanup abort controller on unmount
  useEffect(() => {
    return () => {
      if (abortControllerRef.current) {
        abortControllerRef.current.abort()
      }
    }
  }, [])

  const exportToCSV = async () => {
    setIsExporting(true)
    
    // Create new abort controller for this export
    abortControllerRef.current = new AbortController()
    
    try {
      const params = new URLSearchParams({
        from: dateRange.from.toISOString(),
        to: dateRange.to.toISOString(),
        format: 'csv',
      })

      if (actionFilter !== 'all') params.set('action', actionFilter)
      if (searchQuery) params.set('search', searchQuery)

      const response = await fetch(`/api/admin/audit-logs/export?${params.toString()}`, {
        signal: abortControllerRef.current.signal
      })
      if (!response.ok) throw new Error('Export failed')

      const blob = await response.blob()
      const url = window.URL.createObjectURL(blob)
      const a = document.createElement('a')
      a.href = url
      a.download = `audit-logs-${format(new Date(), 'yyyy-MM-dd')}.csv`
      a.click()
      window.URL.revokeObjectURL(url)

      toast.success('Audit logs exported successfully')
    } catch (error) {
      if (error instanceof DOMException && error.name === 'AbortError') {
        // Export was aborted, don't show error toast
        return
      }
      toast.error('Failed to export audit logs')
    } finally {
      setIsExporting(false)
      abortControllerRef.current = null
    }
  }

  const getActionBadgeVariant = (action: string) => {
    switch (action) {
      case 'delete':
        return 'destructive'
      case 'create':
        return 'default'
      case 'update':
        return 'secondary'
      default:
        return 'outline'
    }
  }

  return (
    <div className="space-y-6">
      {/* Header */}
      <div>
        <h2 className="text-2xl font-bold">Audit Logs</h2>
        <p className="text-gray-600">View all system activity and user actions</p>
      </div>

      {/* Filters */}
      <div className="flex flex-wrap items-center gap-4">
        {/* Date Range Picker */}
        <Popover>
          <PopoverTrigger asChild>
            <Button variant="outline" className="w-64">
              <CalendarIcon className="w-4 h-4 mr-2" />
              {format(dateRange.from, 'MMM dd')} - {format(dateRange.to, 'MMM dd, yyyy')}
            </Button>
          </PopoverTrigger>
          <PopoverContent className="w-auto p-0" align="start">
            <Calendar
              mode="range"
              selected={{ from: dateRange.from, to: dateRange.to }}
              onSelect={(range) => {
                if (range?.from && range?.to) {
                  setDateRange({ from: range.from, to: range.to })
                }
              }}
            />
          </PopoverContent>
        </Popover>

        {/* Action Filter */}
        <Select value={actionFilter} onValueChange={setActionFilter}>
          <SelectTrigger className="w-48">
            <SelectValue placeholder="Filter by action" />
          </SelectTrigger>
          <SelectContent>
            <SelectItem value="all">All Actions</SelectItem>
            <SelectItem value="login">Login</SelectItem>
            <SelectItem value="logout">Logout</SelectItem>
            <SelectItem value="create">Create</SelectItem>
            <SelectItem value="update">Update</SelectItem>
            <SelectItem value="delete">Delete</SelectItem>
            <SelectItem value="export">Export</SelectItem>
          </SelectContent>
        </Select>

        {/* Search */}
        <div className="relative flex-1">
          <Search className="absolute left-3 top-1/2 -translate-y-1/2 w-4 h-4 text-gray-400" />
          <Input
            placeholder="Search by user, resource, or IP..."
            value={searchQuery}
            onChange={(e) => setSearchQuery(e.target.value)}
            className="pl-10"
          />
        </div>

        {/* Export Button */}
        <Button 
          variant="outline" 
          onClick={exportToCSV}
          disabled={isExporting}
          className={isExporting ? "opacity-75 cursor-wait" : ""}
        >
          {isExporting ? (
            <>
              <Loader2 className="w-4 h-4 mr-2 animate-spin" />
              Exporting...
            </>
          ) : (
            <>
              <FileDown className="w-4 h-4 mr-2" />
              Export CSV
            </>
          )}
        </Button>
      </div>

      {/* Audit Logs Table */}
      <div className="border rounded-lg">
        <Table>
          <TableHeader>
            <TableRow>
              <TableHead>Timestamp</TableHead>
              <TableHead>User</TableHead>
              <TableHead>Action</TableHead>
              <TableHead>Resource</TableHead>
              <TableHead>IP Address</TableHead>
              <TableHead className="text-right">Details</TableHead>
            </TableRow>
          </TableHeader>
          <TableBody>
            {isLoading ? (
              <TableRow>
                <TableCell colSpan={6} className="text-center py-8">
                  Loading audit logs...
                </TableCell>
              </TableRow>
            ) : logsData && logsData.logs.length > 0 ? (
              logsData.logs.map((log) => (
                <TableRow key={log.id}>
                  <TableCell className="font-mono text-sm">
                    {format(new Date(log.timestamp), 'MMM dd, HH:mm:ss')}
                  </TableCell>
                  <TableCell className="font-medium">{log.userName}</TableCell>
                  <TableCell>
                    <Badge variant={getActionBadgeVariant(log.action)}>
                      {log.action}
                    </Badge>
                  </TableCell>
                  <TableCell>
                    {log.resource}
                    {log.resourceId && (
                      <span className="text-xs text-gray-500 ml-1">#{log.resourceId}</span>
                    )}
                  </TableCell>
                  <TableCell className="font-mono text-xs">{log.ipAddress}</TableCell>
                  <TableCell className="text-right">
                    <Button
                      variant="ghost"
                      size="sm"
                      onClick={() => setSelectedLog(log)}
                    >
                      <Eye className="w-4 h-4" />
                    </Button>
                  </TableCell>
                </TableRow>
              ))
            ) : (
              <TableRow>
                <TableCell colSpan={6} className="text-center py-8">
                  No audit logs found
                </TableCell>
              </TableRow>
            )}
          </TableBody>
        </Table>
      </div>

      {/* Pagination */}
      {logsData && logsData.pages > 1 && (
        <div className="flex items-center justify-between">
          <p className="text-sm text-gray-600">
            Showing page {page} of {logsData.pages} ({logsData.total} total logs)
          </p>
          <div className="flex gap-2">
            <Button
              variant="outline"
              onClick={() => setPage(page - 1)}
              disabled={page === 1}
            >
              Previous
            </Button>
            <Button
              variant="outline"
              onClick={() => setPage(page + 1)}
              disabled={page === logsData.pages}
            >
              Next
            </Button>
          </div>
        </div>
      )}

      {/* Detail Dialog */}
      <Dialog open={!!selectedLog} onOpenChange={() => setSelectedLog(null)}>
        <DialogContent className="max-w-2xl">
          <DialogHeader>
            <DialogTitle>Audit Log Details</DialogTitle>
            <DialogDescription>
              {selectedLog && format(new Date(selectedLog.timestamp), 'PPpp')}
            </DialogDescription>
          </DialogHeader>
          {selectedLog && (
            <div className="space-y-4">
              <div className="grid grid-cols-2 gap-4 text-sm">
                <div>
                  <span className="text-gray-600">User:</span>
                  <p className="font-medium">{selectedLog.userName}</p>
                </div>
                <div>
                  <span className="text-gray-600">Action:</span>
                  <p className="font-medium">{selectedLog.action}</p>
                </div>
                <div>
                  <span className="text-gray-600">Resource:</span>
                  <p className="font-medium">{selectedLog.resource}</p>
                </div>
                <div>
                  <span className="text-gray-600">IP Address:</span>
                  <p className="font-mono">{selectedLog.ipAddress}</p>
                </div>
              </div>

              {Object.keys(selectedLog.changes).length > 0 && (
                <div>
                  <h4 className="font-medium mb-2">Changes:</h4>
                  <div className="bg-gray-50 rounded-lg p-4 space-y-2">
                    {Object.entries(selectedLog.changes).map(([field, change]) => (
                      <div key={field} className="text-sm">
                        <span className="font-medium">{field}:</span>
                        <div className="flex gap-2 mt-1">
                          <div className="flex-1">
                            <span className="text-xs text-gray-500">Old:</span>
                            <p className="text-red-600">{JSON.stringify(change.old)}</p>
                          </div>
                          <div className="flex-1">
                            <span className="text-xs text-gray-500">New:</span>
                            <p className="text-green-600">{JSON.stringify(change.new)}</p>
                          </div>
                        </div>
                      </div>
                    ))}
                  </div>
                </div>
              )}

              <div className="text-xs text-gray-500">
                <span className="font-medium">User Agent:</span>
                <p className="mt-1 font-mono">{selectedLog.userAgent}</p>
              </div>
            </div>
          )}
        </DialogContent>
      </Dialog>
    </div>
  )
}