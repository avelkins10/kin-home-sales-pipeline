// lib/db/offices.ts
import { sql } from '@/lib/db/client'
import { CANONICAL_OFFICES, normalizeOfficeName, isValidOffice } from '@/lib/constants/offices'
import { logError } from '@/lib/logging/logger'
import { qbClient } from '@/lib/quickbase/client'
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'

/**
 * Validate office names against canonical offices list
 * @param names Array of office names to validate
 * @returns Array of validated/normalized office names
 * @throws Error with invalid office names if validation fails
 */
export async function validateOffices(names: string[]): Promise<string[]> {
  if (!names || names.length === 0) {
    return []
  }

  // Get canonical offices from database
  const result = await sql.query('SELECT name FROM offices ORDER BY name')
  const dbOffices = result.rows.map(row => row.name)
  
  // Combine with canonical offices from constants
  const allCanonicalOffices = [...new Set([...CANONICAL_OFFICES, ...dbOffices])]
  
  const validatedOffices: string[] = []
  const invalidOffices: string[] = []
  
  for (const name of names) {
    const normalized = normalizeOfficeName(name.trim())
    
    if (isValidOffice(normalized) || allCanonicalOffices.includes(normalized)) {
      validatedOffices.push(normalized)
    } else {
      invalidOffices.push(name)
    }
  }
  
  if (invalidOffices.length > 0) {
    const error = new Error(`Invalid office names: ${invalidOffices.join(', ')}`)
    error.name = 'ValidationError'
    throw error
  }
  
  return validatedOffices
}

/**
 * Get all available offices for dropdowns
 * @returns Array of office names
 */
export async function getAvailableOffices(): Promise<string[]> {
  try {
    const result = await sql.query('SELECT name FROM offices ORDER BY name')
    const dbOffices = result.rows.map(row => row.name)
    
    // Combine with canonical offices from constants
    const allOffices = [...new Set([...CANONICAL_OFFICES, ...dbOffices])]
    
    return allOffices.sort()
  } catch (error) {
    logError('Failed to get available offices', error as Error)
    // Fallback to canonical offices only
    return [...CANONICAL_OFFICES].sort()
  }
}

/**
 * Check if an office exists in the database
 * @param officeName Office name to check
 * @returns True if office exists
 */
export async function officeExists(officeName: string): Promise<boolean> {
  try {
    const normalized = normalizeOfficeName(officeName.trim())
    const result = await sql.query('SELECT id FROM offices WHERE name = $1', [normalized])
    return result.rows.length > 0
  } catch (error) {
    logError('Failed to check office existence', error as Error)
    return false
  }
}

/**
 * Sync offices from QuickBase projects table
 * Queries for unique SALES_OFFICE values and creates office records
 * @returns Sync results with created/skipped counts
 */
export async function syncOfficesFromQuickBase(): Promise<{
  created: number
  skipped: number
  errors: string[]
}> {
  const results = {
    created: 0,
    skipped: 0,
    errors: [] as string[]
  }

  try {
    // Query QuickBase for all unique SALES_OFFICE values
    console.log('[syncOfficesFromQuickBase] Querying QuickBase for unique offices...')
    const response = await qbClient.queryRecords({
      from: process.env.QUICKBASE_TABLE_PROJECTS || 'br9kwm8na',
      select: [PROJECT_FIELDS.SALES_OFFICE],
      where: `{${PROJECT_FIELDS.SALES_OFFICE}.XEX.''}`, // Not empty
    })

    // Extract unique office names
    const officeNames = new Set<string>()
    response.data?.forEach((record: any) => {
      const officeName = record[PROJECT_FIELDS.SALES_OFFICE]?.value
      if (officeName && typeof officeName === 'string') {
        const normalized = normalizeOfficeName(officeName.trim())
        if (normalized) {
          officeNames.add(normalized)
        }
      }
    })

    console.log(`[syncOfficesFromQuickBase] Found ${officeNames.size} unique offices in QuickBase`)

    // Insert offices into database
    for (const officeName of officeNames) {
      try {
        // Check if office already exists
        const exists = await officeExists(officeName)
        if (exists) {
          console.log(`[syncOfficesFromQuickBase] Office already exists: ${officeName}`)
          results.skipped++
          continue
        }

        // Determine region based on office name (simple mapping)
        const region = determineRegion(officeName)

        // Insert office
        await sql.query(
          'INSERT INTO offices (name, region) VALUES ($1, $2)',
          [officeName, region]
        )
        console.log(`[syncOfficesFromQuickBase] Created office: ${officeName} (${region})`)
        results.created++
      } catch (error) {
        const errorMsg = `Failed to create office ${officeName}: ${error instanceof Error ? error.message : String(error)}`
        console.error(`[syncOfficesFromQuickBase] ${errorMsg}`)
        results.errors.push(errorMsg)
      }
    }

    console.log('[syncOfficesFromQuickBase] Sync complete:', results)
    return results
  } catch (error) {
    logError('Failed to sync offices from QuickBase', error as Error)
    throw error
  }
}

/**
 * Determine region for an office based on its name
 * Simple mapping based on common geographic knowledge
 */
function determineRegion(officeName: string): string {
  const southwest = ['Phoenix', 'Tucson', 'Las Vegas', 'Albuquerque', 'El Paso']
  const southeast = ['Atlanta', 'Jacksonville', 'Miami', 'Orlando', 'Tampa', 'Charlotte', 'Raleigh', 'Richmond', 'Norfolk', 'Nashville', 'Memphis', 'Birmingham', 'Mobile', 'Montgomery', 'Tallahassee', 'Gainesville', 'Fort Lauderdale', 'West Palm Beach', 'Fort Myers', 'Sarasota', 'Clearwater', 'St. Petersburg', 'Lakeland', 'Daytona Beach', 'Melbourne', 'Vero Beach', 'Pensacola', 'Panama City', 'Gulfport', 'Biloxi', 'Hattiesburg', 'Meridian', 'Jackson', 'New Orleans', 'Baton Rouge', 'Little Rock']
  const midwest = ['Chicago', 'Detroit', 'Cleveland', 'Cincinnati', 'Columbus', 'Indianapolis', 'Milwaukee', 'Minneapolis', 'Kansas City', 'St. Louis', 'Omaha', 'Des Moines', 'Wichita', 'Grand Rapids']
  const northeast = ['New York', 'Boston', 'Philadelphia', 'Baltimore', 'Washington', 'Pittsburgh', 'Buffalo', 'Rochester', 'Syracuse', 'Albany', 'Hartford', 'Providence']
  const west = ['Denver', 'Salt Lake City', 'Austin', 'Dallas', 'Houston', 'San Antonio', 'Oklahoma City']

  if (southwest.includes(officeName)) return 'southwest'
  if (southeast.includes(officeName)) return 'southeast'
  if (midwest.includes(officeName)) return 'midwest'
  if (northeast.includes(officeName)) return 'northeast'
  if (west.includes(officeName)) return 'west'

  // Default to west for unknown offices
  return 'west'
}
