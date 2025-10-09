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
  const allCanonicalOffices = Array.from(new Set([...CANONICAL_OFFICES, ...dbOffices]))
  
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
    const allOffices = Array.from(new Set([...CANONICAL_OFFICES, ...dbOffices]))
    
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
    // Query QuickBase for all unique SALES_OFFICE values with pagination
    console.log('[syncOfficesFromQuickBase] Querying QuickBase for unique offices...')
    
    const top = 1000
    let skip = 0
    const officeNames = new Set<string>()
    
    while (true) {
      const response = await qbClient.queryRecords({
        from: process.env.QUICKBASE_TABLE_PROJECTS || 'br9kwm8na',
        select: [PROJECT_FIELDS.SALES_OFFICE],
        where: `{${PROJECT_FIELDS.SALES_OFFICE}.XEX.''}`, // Not empty
        options: { top, skip }
      })

      // Extract unique office names from this batch
      response.data?.forEach((record: any) => {
        const officeName = record[PROJECT_FIELDS.SALES_OFFICE]?.value
        if (officeName && typeof officeName === 'string') {
          const normalized = normalizeOfficeName(officeName.trim())
          if (normalized) {
            officeNames.add(normalized)
          }
        }
      })

      // If we got fewer records than requested, we've reached the end
      if (!response.data || response.data.length < top) {
        break
      }
      
      skip += top
    }

    console.log(`[syncOfficesFromQuickBase] Found ${officeNames.size} unique offices in QuickBase`)

    // Insert offices into database using UPSERT
    for (const officeName of Array.from(officeNames)) {
      try {
        // Use UPSERT to prevent race conditions
        const result = await sql.query(
          'INSERT INTO offices (name, is_active) VALUES ($1, TRUE) ON CONFLICT (name) DO NOTHING',
          [officeName]
        )
        
        if (result.rowCount && result.rowCount > 0) {
          console.log(`[syncOfficesFromQuickBase] Created office: ${officeName}`)
          results.created++
        } else {
          console.log(`[syncOfficesFromQuickBase] Office already exists: ${officeName}`)
          results.skipped++
        }
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

