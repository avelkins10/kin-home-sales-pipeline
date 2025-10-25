/**
 * Sync run logger for tracking user sync operations in the database
 * 
 * Provides functions to create, update, and query sync run records
 * for monitoring and analytics.
 */

import { sql } from '@vercel/postgres';

interface SyncResult {
  success: boolean;
  stats: {
    totalUsers: number;
    enriched: number;
    notFoundInContacts: number;
    alreadyUpToDate: number;
    errors: number;
    errorDetails: Array<{email: string, error: string}>;
    notFoundSamples: string[];
  };
  executionTimeMs: number;
  timestamp: string;
}

interface SyncRunRecord {
  id: number;
  started_at: string;
  completed_at: string | null;
  status: 'running' | 'success' | 'partial' | 'failed';
  total_users: number;
  enriched: number;
  not_found: number;
  already_up_to_date: number;
  errors: number;
  error_details: any;
  not_found_samples: any;
  triggered_by: 'cron' | 'manual';
  triggered_by_user_id: string | null;
  execution_time_ms: number | null;
}

interface SyncRunStats {
  total_runs: number;
  success_rate: number;
  avg_execution_time_ms: number;
  last_success_at: string | null;
  consecutive_failures: number;
}

/**
 * Create a new sync run record with status 'running'
 * Uses advisory lock to prevent race conditions
 */
export async function createSyncRun(
  triggeredBy: 'cron' | 'manual', 
  triggeredByUserId?: string
): Promise<number> {
  try {
    // Use advisory lock to prevent concurrent sync starts
    const lockId = 12345; // Fixed lock ID for sync operations
    
    const { rows } = await sql`
      SELECT pg_try_advisory_lock(${lockId}) as acquired
    `;
    
    if (!rows[0]?.acquired) {
      throw new Error('Another sync operation is already in progress');
    }
    
    try {
      // Check if sync is already running (double-check)
      const { rows: runningRows } = await sql`
        SELECT COUNT(*) as running_count
        FROM user_sync_runs
        WHERE status = 'running'
      `;
      
      if (parseInt(runningRows[0]?.running_count) > 0) {
        throw new Error('Sync is already running');
      }
      
      // Create new sync run record
      const { rows: insertRows } = await sql`
        INSERT INTO user_sync_runs (triggered_by, triggered_by_user_id, status)
        VALUES (${triggeredBy}, ${triggeredByUserId || null}, 'running')
        RETURNING id
      `;
      
      return insertRows[0].id;
      
    } finally {
      // Release the advisory lock
      await sql`SELECT pg_advisory_unlock(${lockId})`;
    }
    
  } catch (error) {
    console.error('❌ Failed to create sync run record:', error);
    throw error;
  }
}

/**
 * Update sync run with final statistics
 */
export async function updateSyncRun(
  runId: number, 
  result: SyncResult
): Promise<void> {
  try {
    // Determine status based on result - align with success flag logic
    let status: 'success' | 'partial' | 'failed';
    if (!result.success) {
      status = 'failed';
    } else if (result.stats.errors > 0) {
      status = 'partial';
    } else {
      status = 'success';
    }

    await sql`
      UPDATE user_sync_runs 
      SET 
        completed_at = NOW(),
        status = ${status},
        total_users = ${result.stats.totalUsers},
        enriched = ${result.stats.enriched},
        not_found = ${result.stats.notFoundInContacts},
        already_up_to_date = ${result.stats.alreadyUpToDate},
        errors = ${result.stats.errors},
        error_details = ${sql.json(result.stats.errorDetails)},
        not_found_samples = ${sql.json(result.stats.notFoundSamples)},
        execution_time_ms = ${result.executionTimeMs},
        updated_at = NOW()
      WHERE id = ${runId}
    `;
  } catch (error) {
    console.error('❌ Failed to update sync run record:', error);
    throw error;
  }
}

/**
 * Fetch recent sync run history for monitoring dashboard
 */
export async function getSyncRunHistory(limit: number = 50): Promise<SyncRunRecord[]> {
  try {
    const { rows } = await sql`
      SELECT 
        id,
        started_at,
        completed_at,
        status,
        total_users,
        enriched,
        not_found,
        already_up_to_date,
        errors,
        error_details,
        not_found_samples,
        triggered_by,
        triggered_by_user_id,
        execution_time_ms
      FROM user_sync_runs
      ORDER BY started_at DESC
      LIMIT ${limit}
    `;
    
    return rows as SyncRunRecord[];
  } catch (error) {
    console.error('❌ Failed to fetch sync run history:', error);
    return [];
  }
}

/**
 * Calculate aggregate statistics for monitoring
 */
export async function getSyncRunStats(): Promise<SyncRunStats> {
  try {
    // Get last 30 days of sync runs
    const { rows } = await sql`
      SELECT 
        COUNT(*) as total_runs,
        AVG(CASE WHEN status = 'success' THEN 1 ELSE 0 END) as success_rate,
        AVG(execution_time_ms) as avg_execution_time_ms,
        MAX(CASE WHEN status = 'success' THEN completed_at END) as last_success_at
      FROM user_sync_runs
      WHERE started_at >= NOW() - INTERVAL '30 days'
    `;

    const stats = rows[0];
    
    // Calculate consecutive failures from the latest run backwards
    const { rows: failureRows } = await sql`
      WITH recent_runs AS (
        SELECT status, started_at
        FROM user_sync_runs
        ORDER BY started_at DESC
        LIMIT 20
      ),
      consecutive_failures AS (
        SELECT 
          status,
          ROW_NUMBER() OVER (ORDER BY started_at DESC) as rn,
          SUM(CASE WHEN status = 'success' THEN 1 ELSE 0 END) 
            OVER (ORDER BY started_at DESC ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) as success_count
        FROM recent_runs
      )
      SELECT COUNT(*) as consecutive_failures
      FROM consecutive_failures
      WHERE success_count = 0 AND status != 'success'
    `;

    return {
      total_runs: parseInt(stats.total_runs) || 0,
      success_rate: Math.round((parseFloat(stats.success_rate) || 0) * 100),
      avg_execution_time_ms: Math.round(parseFloat(stats.avg_execution_time_ms) || 0),
      last_success_at: stats.last_success_at,
      consecutive_failures: parseInt(failureRows[0]?.consecutive_failures) || 0
    };
  } catch (error) {
    console.error('❌ Failed to calculate sync run stats:', error);
    return {
      total_runs: 0,
      success_rate: 0,
      avg_execution_time_ms: 0,
      last_success_at: null,
      consecutive_failures: 0
    };
  }
}

/**
 * Check if a sync is currently running
 */
export async function isSyncRunning(): Promise<boolean> {
  try {
    const { rows } = await sql`
      SELECT COUNT(*) as running_count
      FROM user_sync_runs
      WHERE status = 'running'
    `;
    
    return parseInt(rows[0]?.running_count) > 0;
  } catch (error) {
    console.error('❌ Failed to check sync running status:', error);
    return false;
  }
}

/**
 * Get the most recent sync run
 */
export async function getLatestSyncRun(): Promise<SyncRunRecord | null> {
  try {
    const { rows } = await sql`
      SELECT 
        id,
        started_at,
        completed_at,
        status,
        total_users,
        enriched,
        not_found,
        already_up_to_date,
        errors,
        error_details,
        not_found_samples,
        triggered_by,
        triggered_by_user_id,
        execution_time_ms
      FROM user_sync_runs
      ORDER BY started_at DESC
      LIMIT 1
    `;
    
    return rows[0] as SyncRunRecord || null;
  } catch (error) {
    console.error('❌ Failed to get latest sync run:', error);
    return null;
  }
}
