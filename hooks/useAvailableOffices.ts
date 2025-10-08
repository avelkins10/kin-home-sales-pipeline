import { useQuery } from '@tanstack/react-query'
import { getBaseUrl } from '@/lib/utils/baseUrl'

export function useAvailableOffices() {
  return useQuery({
    queryKey: ['available-offices'],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/admin/offices`)
      if (!response.ok) {
        throw new Error('Failed to fetch offices')
      }
      const offices = await response.json()
      return offices.map((office: any) => office.name).sort()
    },
    staleTime: 5 * 60 * 1000, // 5 minutes
  })
}
