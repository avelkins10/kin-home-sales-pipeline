'use client'

import { useState, useRef, useEffect, ReactNode } from 'react'
import { ChevronDown } from 'lucide-react'
import { cn } from '@/lib/utils/cn'

export interface AccordionSection {
  id: string
  title: string
  badge?: ReactNode // Optional badge (e.g., unread count)
  defaultOpen?: boolean
  children: ReactNode
}

interface AccordionMobileProps {
  sections: AccordionSection[]
  className?: string
  /** Save/restore expanded state to localStorage */
  persistKey?: string
}

/**
 * Mobile-optimized accordion component for collapsible sections
 * Features:
 * - Smooth expand/collapse animations
 * - Optional state persistence to localStorage
 * - Badge support for notifications
 * - Touch-friendly design
 * - Accessible keyboard navigation
 */
export function AccordionMobile({ sections, className, persistKey }: AccordionMobileProps) {
  // Initialize expanded state from localStorage or defaults
  const getInitialExpanded = () => {
    if (persistKey && typeof window !== 'undefined') {
      const stored = localStorage.getItem(`accordion-${persistKey}`)
      if (stored) {
        try {
          return JSON.parse(stored) as Record<string, boolean>
        } catch {
          // Ignore parse errors, fall back to defaults
        }
      }
    }

    // Use defaultOpen from sections
    return sections.reduce((acc, section) => {
      acc[section.id] = section.defaultOpen ?? false
      return acc
    }, {} as Record<string, boolean>)
  }

  const [expanded, setExpanded] = useState<Record<string, boolean>>(getInitialExpanded)

  // Save expanded state to localStorage
  useEffect(() => {
    if (persistKey && typeof window !== 'undefined') {
      localStorage.setItem(`accordion-${persistKey}`, JSON.stringify(expanded))
    }
  }, [expanded, persistKey])

  const toggleSection = (id: string) => {
    setExpanded(prev => ({
      ...prev,
      [id]: !prev[id]
    }))
  }

  return (
    <div className={cn('space-y-3', className)}>
      {sections.map((section) => (
        <AccordionSection
          key={section.id}
          section={section}
          isExpanded={expanded[section.id] ?? false}
          onToggle={() => toggleSection(section.id)}
        />
      ))}
    </div>
  )
}

interface AccordionSectionProps {
  section: AccordionSection
  isExpanded: boolean
  onToggle: () => void
}

function AccordionSection({ section, isExpanded, onToggle }: AccordionSectionProps) {
  const contentRef = useRef<HTMLDivElement>(null)
  const [height, setHeight] = useState<number | undefined>(undefined)

  // Calculate content height when expanded
  useEffect(() => {
    if (isExpanded && contentRef.current) {
      setHeight(contentRef.current.scrollHeight)
    } else {
      setHeight(0)
    }
  }, [isExpanded])

  // Recalculate height when content changes (e.g., async data loaded)
  useEffect(() => {
    if (isExpanded && contentRef.current) {
      const observer = new ResizeObserver(() => {
        if (contentRef.current) {
          setHeight(contentRef.current.scrollHeight)
        }
      })

      observer.observe(contentRef.current)
      return () => observer.disconnect()
    }
  }, [isExpanded])

  return (
    <div className="bg-white rounded-lg shadow-sm border border-gray-200 overflow-hidden">
      {/* Header - Clickable to expand/collapse */}
      <button
        onClick={onToggle}
        className={cn(
          'w-full flex items-center justify-between p-4',
          'text-left font-semibold text-gray-900',
          'hover:bg-gray-50 active:bg-gray-100',
          'transition-colors duration-150',
          'focus:outline-none focus:ring-2 focus:ring-inset focus:ring-blue-500'
        )}
        aria-expanded={isExpanded}
        aria-controls={`accordion-content-${section.id}`}
      >
        <div className="flex items-center gap-2 flex-1">
          <span className="text-base">{section.title}</span>
          {section.badge && <span className="ml-2">{section.badge}</span>}
        </div>
        <ChevronDown
          className={cn(
            'w-5 h-5 text-gray-400 transition-transform duration-200 flex-shrink-0',
            isExpanded && 'transform rotate-180'
          )}
        />
      </button>

      {/* Content - Animates height */}
      <div
        id={`accordion-content-${section.id}`}
        ref={contentRef}
        className="overflow-hidden transition-all duration-200 ease-in-out"
        style={{ height: height !== undefined ? `${height}px` : 'auto' }}
      >
        <div className="p-4 pt-0 border-t border-gray-100">
          {section.children}
        </div>
      </div>
    </div>
  )
}
