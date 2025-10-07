import { describe, it, expect } from 'vitest'
import {
  formatDiffValue,
  getDiffColor,
  calculateChangeSummary,
  hasSignificantChange,
} from '@/lib/utils/jsonDiff'

describe('JSON Diff Utilities', () => {
  describe('formatDiffValue', () => {
    it('formats null value', () => {
      expect(formatDiffValue(null)).toBe('null')
    })

    it('formats undefined value', () => {
      expect(formatDiffValue(undefined)).toBe('null')
    })

    it('formats boolean true', () => {
      expect(formatDiffValue(true)).toBe('true')
    })

    it('formats boolean false', () => {
      expect(formatDiffValue(false)).toBe('false')
    })

    it('formats string value', () => {
      expect(formatDiffValue('test string')).toBe('test string')
    })

    it('truncates long strings', () => {
      const longString = 'a'.repeat(150)
      const result = formatDiffValue(longString)
      expect(result).toContain('...')
      expect(result.length).toBeLessThanOrEqual(103) // 100 + '...'
    })

    it('formats number value', () => {
      expect(formatDiffValue(42)).toBe('42')
    })

    it('formats object value', () => {
      const obj = { name: 'John', age: 30 }
      const result = formatDiffValue(obj)
      expect(result).toContain('"name": "John"')
      expect(result).toContain('"age": 30')
    })

    it('formats array value', () => {
      const arr = ['a', 'b', 'c']
      const result = formatDiffValue(arr)
      expect(result).toContain('"a"')
      expect(result).toContain('"b"')
      expect(result).toContain('"c"')
    })
  })

  describe('getDiffColor', () => {
    it('returns red for deleted value', () => {
      expect(getDiffColor(true, false)).toBe('text-red-600')
    })

    it('returns green for added value', () => {
      expect(getDiffColor(false, true)).toBe('text-green-600')
    })

    it('returns blue for changed value', () => {
      expect(getDiffColor(true, true)).toBe('text-blue-600')
    })

    it('returns gray for no change', () => {
      expect(getDiffColor(false, false)).toBe('text-gray-600')
    })
  })

  describe('calculateChangeSummary', () => {
    it('returns summary for single change', () => {
      const changes = { name: { old: 'John', new: 'Jane' } }
      expect(calculateChangeSummary(changes)).toBe('1 field changed')
    })

    it('returns summary for multiple changes', () => {
      const changes = {
        name: { old: 'John', new: 'Jane' },
        email: { old: 'john@example.com', new: 'jane@example.com' },
        phone: { old: '111-111-1111', new: '222-222-2222' },
      }
      expect(calculateChangeSummary(changes)).toBe('3 fields changed')
    })

    it('returns summary for no changes', () => {
      const changes = {}
      expect(calculateChangeSummary(changes)).toBe('0 fields changed')
    })
  })

  describe('hasSignificantChange', () => {
    it('returns true for different strings', () => {
      expect(hasSignificantChange('test', 'changed')).toBe(true)
    })

    it('returns false for same strings', () => {
      expect(hasSignificantChange('test', 'test')).toBe(false)
    })

    it('returns false for whitespace-only difference', () => {
      expect(hasSignificantChange('test', ' test ')).toBe(false)
    })

    it('returns true for different numbers', () => {
      expect(hasSignificantChange(10, 20)).toBe(true)
    })

    it('returns false for same numbers', () => {
      expect(hasSignificantChange(10, 10)).toBe(false)
    })

    it('returns true for different objects', () => {
      expect(hasSignificantChange({ a: 1 }, { a: 2 })).toBe(true)
    })

    it('returns false for same objects (deep equality)', () => {
      expect(hasSignificantChange({ a: 1, b: 2 }, { a: 1, b: 2 })).toBe(false)
    })

    it('handles null values correctly', () => {
      expect(hasSignificantChange(null, 'value')).toBe(true)
      expect(hasSignificantChange('value', null)).toBe(true)
      expect(hasSignificantChange(null, null)).toBe(false)
    })

    it('handles undefined values correctly', () => {
      expect(hasSignificantChange(undefined, 'value')).toBe(true)
      expect(hasSignificantChange('value', undefined)).toBe(true)
      expect(hasSignificantChange(undefined, undefined)).toBe(false)
    })

    it('handles boolean values correctly', () => {
      expect(hasSignificantChange(true, false)).toBe(true)
      expect(hasSignificantChange(false, true)).toBe(true)
      expect(hasSignificantChange(true, true)).toBe(false)
      expect(hasSignificantChange(false, false)).toBe(false)
    })

    it('handles arrays correctly', () => {
      expect(hasSignificantChange([1, 2, 3], [1, 2, 4])).toBe(true)
      expect(hasSignificantChange([1, 2, 3], [1, 2, 3])).toBe(false)
    })

    it('handles nested objects correctly', () => {
      const obj1 = { user: { name: 'John', age: 30 } }
      const obj2 = { user: { name: 'John', age: 31 } }
      const obj3 = { user: { name: 'John', age: 30 } }
      
      expect(hasSignificantChange(obj1, obj2)).toBe(true)
      expect(hasSignificantChange(obj1, obj3)).toBe(false)
    })
  })
})