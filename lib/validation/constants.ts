import { z } from 'zod'

export const roleEnum = z.enum(['closer', 'setter', 'office_leader', 'regional', 'super_admin'])

export const regionEnum = z.enum(['southwest', 'southeast', 'midwest', 'northeast', 'west'])


