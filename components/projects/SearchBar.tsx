'use client'

import { useState, useEffect } from 'react'
import { useRouter, useSearchParams } from 'next/navigation'
import { Input } from '@/components/ui/input'
import { Search, Loader2 } from 'lucide-react'
import { useIsMobile } from '@/lib/hooks/useMediaQuery'

interface SearchBarProps {
  defaultValue?: string
  isFetching?: boolean
}

export function SearchBar({ defaultValue, isFetching = false }: SearchBarProps) {
  const [search, setSearch] = useState(defaultValue || '')
  const [isSearching, setIsSearching] = useState(false)
  const router = useRouter()
  const searchParams = useSearchParams()
  const isMobile = useIsMobile()
  
  // Search is limited to 100 characters for performance and QuickBase query limits
  const MAX_SEARCH_LENGTH = 100

  // Sync local state with search params changes
  useEffect(() => {
    const currentSearch = searchParams.get('search') || ''
    setSearch(currentSearch)
  }, [searchParams])

  // Debounced URL update
  useEffect(() => {
    const timer = setTimeout(() => {
      setIsSearching(true)
      const params = new URLSearchParams(searchParams)
      if (search) {
        params.set('search', search)
      } else {
        params.delete('search')
      }
      router.push(`/projects?${params.toString()}`)
    }, 300)

    return () => clearTimeout(timer)
  }, [search, router, searchParams])

  // Clear isSearching when parent passes isFetching=false
  useEffect(() => {
    if (!isFetching) {
      setIsSearching(false)
    }
  }, [isFetching])

  return (
    <div className="relative max-w-md">
      {isSearching || isFetching ? (
        <Loader2 className="absolute left-3.5 top-1/2 transform -translate-y-1/2 h-4 w-4 text-indigo-600 animate-spin" />
      ) : (
        <Search className="absolute left-3.5 top-1/2 transform -translate-y-1/2 h-4 w-4 text-slate-400" />
      )}
      <Input
        type="search"
        placeholder={isMobile ? "Search projects..." : "Search by customer name, project ID, or address..."}
        value={search}
        onChange={(e) => setSearch(e.target.value.slice(0, MAX_SEARCH_LENGTH))}
        maxLength={MAX_SEARCH_LENGTH}
        className={`
          pl-10 pr-4 py-2.5 w-full
          bg-white
          ${isSearching || isFetching ? 'border-indigo-300' : 'border-slate-200'}
          focus:border-indigo-300 focus:ring-2 focus:ring-indigo-100
          placeholder:text-slate-400
          text-slate-900
          transition-all duration-200
        `}
        aria-label="Search projects"
      />
    </div>
  )
}
