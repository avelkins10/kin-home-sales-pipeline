import { describe, it, expect } from 'vitest'
import { buildRoleClause } from '../../lib/quickbase/queries'

describe('buildRoleClause', () => {
  describe('super_admin role', () => {
    it('should return all-projects clause', () => {
      const result = buildRoleClause('user123', 'super_admin')
      expect(result).toBe('{3.GT.0}')
    })

    it('should return all-projects clause regardless of salesOffice', () => {
      const result = buildRoleClause('user123', 'super_admin', ['Phoenix', 'Tucson'])
      expect(result).toBe('{3.GT.0}')
    })
  })

  describe('regional role', () => {
    it('should return all-projects clause', () => {
      const result = buildRoleClause('user123', 'regional')
      expect(result).toBe('{3.GT.0}')
    })

    it('should return all-projects clause regardless of salesOffice', () => {
      const result = buildRoleClause('user123', 'regional', ['Phoenix'])
      expect(result).toBe('{3.GT.0}')
    })
  })

  describe('office_leader role', () => {
    it('should return office-based clause for single office', () => {
      const result = buildRoleClause('user123', 'office_leader', ['Phoenix'])
      expect(result).toBe("{2087.EX.'Phoenix'}")
    })

    it('should return office-based clause for multiple offices', () => {
      const result = buildRoleClause('user123', 'office_leader', ['Phoenix', 'Tucson'])
      expect(result).toBe("{2087.EX.'Phoenix'} OR {2087.EX.'Tucson'}")
    })

    it('should return all-projects clause when no office assigned', () => {
      const result = buildRoleClause('user123', 'office_leader')
      expect(result).toBe('{3.GT.0}')
    })

    it('should return all-projects clause when empty office array', () => {
      const result = buildRoleClause('user123', 'office_leader', [])
      expect(result).toBe('{3.GT.0}')
    })
  })

  describe('closer role', () => {
    it('should return closer-based clause for single user', () => {
      const result = buildRoleClause('user123', 'closer')
      expect(result).toBe("{516.EX.'user123'}")
    })

    it('should return closer-based clause for multiple users', () => {
      const result = buildRoleClause('user123,user456', 'closer')
      expect(result).toBe("{516.EX.'user123'} OR {516.EX.'user456'}")
    })

    it('should handle whitespace in user IDs', () => {
      const result = buildRoleClause(' user123 , user456 ', 'closer')
      expect(result).toBe("{516.EX.'user123'} OR {516.EX.'user456'}")
    })

    it('should ignore salesOffice parameter', () => {
      const result = buildRoleClause('user123', 'closer', ['Phoenix'])
      expect(result).toBe("{516.EX.'user123'}")
    })
  })

  describe('setter role', () => {
    it('should return setter-based clause for single user', () => {
      const result = buildRoleClause('user123', 'setter')
      expect(result).toBe("{329.EX.'user123'}")
    })

    it('should return setter-based clause for multiple users', () => {
      const result = buildRoleClause('user123,user456', 'setter')
      expect(result).toBe("{329.EX.'user123'} OR {329.EX.'user456'}")
    })

    it('should handle whitespace in user IDs', () => {
      const result = buildRoleClause(' user123 , user456 ', 'setter')
      expect(result).toBe("{329.EX.'user123'} OR {329.EX.'user456'}")
    })

    it('should ignore salesOffice parameter', () => {
      const result = buildRoleClause('user123', 'setter', ['Phoenix'])
      expect(result).toBe("{329.EX.'user123'}")
    })
  })

  describe('coordinator role', () => {
    it('should return coordinator-based clause for single user', () => {
      const result = buildRoleClause('user123', 'coordinator')
      expect(result).toBe("{819.EX.'user123'}")
    })

    it('should return coordinator-based clause for multiple users', () => {
      const result = buildRoleClause('user123,user456', 'coordinator')
      expect(result).toBe("{819.EX.'user123'} OR {819.EX.'user456'}")
    })

    it('should handle whitespace in user IDs', () => {
      const result = buildRoleClause(' user123 , user456 ', 'coordinator')
      expect(result).toBe("{819.EX.'user123'} OR {819.EX.'user456'}")
    })

    it('should ignore salesOffice parameter', () => {
      const result = buildRoleClause('user123', 'coordinator', ['Phoenix'])
      expect(result).toBe("{819.EX.'user123'}")
    })
  })

  describe('unknown role', () => {
    it('should default to closer-based clause', () => {
      const result = buildRoleClause('user123', 'unknown_role')
      expect(result).toBe("{516.EX.'user123'}")
    })

    it('should default to closer-based clause for multiple users', () => {
      const result = buildRoleClause('user123,user456', 'unknown_role')
      expect(result).toBe("{516.EX.'user123'} OR {516.EX.'user456'}")
    })
  })

  describe('edge cases', () => {
    it('should handle empty user ID', () => {
      const result = buildRoleClause('', 'closer')
      expect(result).toBe("{516.EX.''}")
    })

    it('should handle single space user ID', () => {
      const result = buildRoleClause(' ', 'closer')
      expect(result).toBe("{516.EX.''}")
    })

    it('should handle multiple spaces in user ID', () => {
      const result = buildRoleClause('   ', 'closer')
      expect(result).toBe("{516.EX.''}")
    })

    it('should handle office names with special characters', () => {
      const result = buildRoleClause('user123', 'office_leader', ['Phoenix-AZ', 'Las Vegas'])
      expect(result).toBe("{2087.EX.'Phoenix-AZ'} OR {2087.EX.'Las Vegas'}")
    })

    it('should handle office names with spaces', () => {
      const result = buildRoleClause('user123', 'office_leader', ['Las Vegas', 'San Francisco'])
      expect(result).toBe("{2087.EX.'Las Vegas'} OR {2087.EX.'San Francisco'}")
    })
  })
})
