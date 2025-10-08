import { z } from 'zod'

export const roleEnum = z.enum([
  'closer', 
  'setter', 
  'team_lead', 
  'office_leader', 
  'area_director', 
  'divisional', 
  'regional', 
  'super_admin'
])

export const regionEnum = z.enum(['southwest', 'southeast', 'midwest', 'northeast', 'west'])


