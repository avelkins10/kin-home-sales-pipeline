'use client'

import { useState, useEffect } from 'react'
import { useRouter, useSearchParams } from 'next/navigation'
import { Input } from '@/components/ui/input'
import { Search } from 'lucide-react'

interface SearchBarProps {
  defaultValue?: string
}

export function SearchBar({ defaultValue }: SearchBarProps) {
  const [search, setSearch] = useState(defaultValue || '')
  const router = useRouter()
  const searchParams = useSearchParams()

  // Sync local state with search params changes
  useEffect(() => {
    const currentSearch = searchParams.get('search') || ''
    setSearch(currentSearch)
  }, [searchParams])

  // Debounced URL update
  useEffect(() => {
    const timer = setTimeout(() => {
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

  return (
    <div className="relative max-w-md">
      <Search className="absolute left-3.5 top-1/2 transform -translate-y-1/2 h-4 w-4 text-slate-400" />
      <Input
        type="search"
        placeholder="Search by customer name, project ID, or address..."
        value={search}
        onChange={(e) => setSearch(e.target.value)}
        className="
          pl-10 pr-4 py-2.5 w-full
          bg-white border-slate-200
          focus:border-indigo-300 focus:ring-2 focus:ring-indigo-100
          placeholder:text-slate-400
          text-slate-900
          transition-all duration-200
        "
        aria-label="Search projects"
      />
    </div>
  )
}
