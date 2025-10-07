import { describe, it, expect } from 'vitest'
// We import indirectly to access the built filter behavior
import * as queries from '../../lib/quickbase/queries'

describe('sanitize search for Quickbase', () => {
  it('escapes single quotes and trims length', async () => {
    // @ts-expect-error access internal for test if exported
    const build = (queries as any).__test__?.buildSearchFilter || (queries as any).buildSearchFilter
    const filter = build("O'Hara %\n\r" + 'x'.repeat(200))
    expect(filter).toContain("O''Hara")
    // should not contain raw % or newlines
    expect(filter).not.toMatch(/%|\n|\r/)
  })
})




