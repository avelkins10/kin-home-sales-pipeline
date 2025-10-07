import { describe, it, expect } from 'vitest'
import { z } from 'zod'
import { systemSettingsSchema } from '@/lib/validation/systemSettings'

function validate(input: unknown) {
  return () => systemSettingsSchema.parse(input)
}

describe('System Settings Validation', () => {
  describe('systemSettingsSchema', () => {
    it('accepts valid complete settings', () => {
      const valid = {
        quickbaseRealm: 'kin.quickbase.com',
        quickbaseToken: 'token123',
        milestoneSLA: { survey: 7, design: 10, permit: 21, nem: 14, install: 7, inspection: 5, pto: 10 },
        warningThreshold: 75,
        criticalThreshold: 100,
        holdReasons: ['Finance', 'Roof'],
        dateFormat: 'MM/DD/YYYY',
        timezone: 'America/Phoenix',
        sessionTimeout: 60,
      }
      expect(validate(valid)).not.toThrow()
    })

    it('accepts partial updates', () => {
      expect(validate({ quickbaseRealm: 'new.quickbase.com' })).not.toThrow()
    })

    it('accepts empty object', () => {
      expect(validate({})).not.toThrow()
    })

    it('validates milestone SLA ranges', () => {
      const base = { survey: 1, design: 1, permit: 1, nem: 1, install: 1, inspection: 1, pto: 1 }
      expect(validate({ milestoneSLA: { ...base, survey: 60 } })).not.toThrow()
      expect(validate({ milestoneSLA: { ...base, design: 60 } })).not.toThrow()
      expect(validate({ milestoneSLA: { ...base, permit: 90 } })).not.toThrow()
      expect(validate({ milestoneSLA: { ...base, nem: 60 } })).not.toThrow()
      expect(validate({ milestoneSLA: { ...base, install: 30 } })).not.toThrow()
      expect(validate({ milestoneSLA: { ...base, inspection: 30 } })).not.toThrow()
      expect(validate({ milestoneSLA: { ...base, pto: 60 } })).not.toThrow()

      expect(validate({ milestoneSLA: { ...base, survey: 0 } })).toThrow()
      expect(validate({ milestoneSLA: { ...base, permit: 91 } })).toThrow()
    })

    it('rejects negative SLA values', () => {
      expect(validate({ milestoneSLA: { survey: -1, design: 10, permit: 21, nem: 14, install: 7, inspection: 5, pto: 10 } })).toThrow()
    })

    it('rejects non-integer SLA values', () => {
      expect(validate({ milestoneSLA: { survey: 7.5, design: 10, permit: 21, nem: 14, install: 7, inspection: 5, pto: 10 } })).toThrow()
    })

    it('validates warning threshold range (50-100)', () => {
      expect(validate({ warningThreshold: 75 })).not.toThrow()
      expect(validate({ warningThreshold: 49 })).toThrow()
      expect(validate({ warningThreshold: 101 })).toThrow()
    })

    it('validates critical threshold range (50-150)', () => {
      expect(validate({ criticalThreshold: 100 })).not.toThrow()
      expect(validate({ criticalThreshold: 49 })).toThrow()
      expect(validate({ criticalThreshold: 151 })).toThrow()
    })

    it('validates date format enum', () => {
      expect(validate({ dateFormat: 'MM/DD/YYYY' })).not.toThrow()
      expect(validate({ dateFormat: 'DD/MM/YYYY' })).not.toThrow()
      expect(validate({ dateFormat: 'YYYY-MM-DD' })).not.toThrow()
      // @ts-expect-error
      expect(validate({ dateFormat: 'invalid' })).toThrow()
    })

    it('validates session timeout range (15-480)', () => {
      expect(validate({ sessionTimeout: 60 })).not.toThrow()
      expect(validate({ sessionTimeout: 14 })).toThrow()
      expect(validate({ sessionTimeout: 481 })).toThrow()
    })

    it('validates holdReasons is array of strings', () => {
      expect(validate({ holdReasons: ['Finance', 'Roof'] })).not.toThrow()
      // @ts-expect-error
      expect(validate({ holdReasons: ['Finance', 123] })).toThrow()
      // @ts-expect-error
      expect(validate({ holdReasons: 'Finance' })).toThrow()
    })
  })
})

describe('SLA Calculations', () => {
  const warnAt = (slaDays: number, warningPct: number) => (slaDays * warningPct) / 100
  const criticalAt = (slaDays: number, criticalPct: number) => (slaDays * criticalPct) / 100

  it('calculates warning threshold correctly', () => {
    expect(warnAt(10, 75)).toBe(7.5)
    expect(warnAt(20, 80)).toBe(16)
  })

  it('calculates critical threshold correctly', () => {
    expect(criticalAt(10, 100)).toBe(10)
    expect(criticalAt(14, 110)).toBe(15.4)
  })

  it('handles threshold edge cases', () => {
    expect(warnAt(7, 75)).toBe(5.25)
  })
})

describe('Hold Reasons Management', () => {
  it('adds new hold reason to array', () => {
    const arr = ['Finance', 'Roof']
    const next = [...arr, 'Customer']
    expect(next).toEqual(['Finance', 'Roof', 'Customer'])
  })

  it('prevents duplicate hold reasons', () => {
    const arr = ['Finance', 'Roof']
    const toAdd = 'Finance'
    const next = arr.includes(toAdd) ? arr : [...arr, toAdd]
    expect(next).toEqual(['Finance', 'Roof'])
  })

  it('removes hold reason from array', () => {
    const arr = ['Finance', 'Roof', 'Customer']
    const next = arr.filter(r => r !== 'Roof')
    expect(next).toEqual(['Finance', 'Customer'])
  })

  it('handles removing non-existent reason', () => {
    const arr = ['Finance', 'Roof']
    const next = arr.filter(r => r !== 'Customer')
    expect(next).toEqual(['Finance', 'Roof'])
  })
})


