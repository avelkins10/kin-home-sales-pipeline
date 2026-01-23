/**
 * RepCard Metric Configuration Helper
 * 
 * Provides functions to check if dispositions/statuses should count toward metrics
 * based on admin-configured rules stored in repcard_metric_config table
 */

import { sql } from '@/lib/db/client';

export type MetricName = 
  | 'door_knocked'
  | 'appointment_set'
  | 'sit'
  | 'sat_closed'
  | 'sat_no_close'
  | 'quality_metric'
  | 'appointments_run';

export interface MetricConfig {
  id: string;
  metric_name: string;
  disposition_pattern: string | null;
  status_category: string | null;
  is_included: boolean;
  priority: number;
  created_at: string;
  updated_at: string;
}

/**
 * Check if a disposition/status should count for a metric
 * Uses the database function for consistency
 */
export async function shouldCountForMetric(
  metricName: MetricName,
  disposition: string | null,
  statusCategory: string | null
): Promise<boolean> {
  try {
    const result = await sql`
      SELECT should_count_for_metric(
        ${metricName}::text,
        ${disposition}::text,
        ${statusCategory}::text
      ) as should_count
    `;
    
    const row = Array.isArray(result) ? result[0] : result.rows?.[0];
    return row?.should_count === true;
  } catch (error) {
    console.error('[Metric Config] Error checking metric:', error);
    // Fallback to false on error (conservative)
    return false;
  }
}

/**
 * Get all configurations for a specific metric
 */
export async function getMetricConfig(metricName: MetricName): Promise<MetricConfig[]> {
  try {
    const result = await sql`
      SELECT * FROM repcard_metric_config
      WHERE metric_name = ${metricName}
      ORDER BY priority DESC, created_at ASC
    `;
    
    return Array.isArray(result) ? result : result.rows || [];
  } catch (error) {
    console.error('[Metric Config] Error getting config:', error);
    return [];
  }
}

/**
 * Get all metric configurations
 */
export async function getAllMetricConfigs(): Promise<MetricConfig[]> {
  try {
    const result = await sql`
      SELECT * FROM repcard_metric_config
      ORDER BY metric_name, priority DESC, created_at ASC
    `;
    
    return Array.isArray(result) ? result : result.rows || [];
  } catch (error) {
    console.error('[Metric Config] Error getting all configs:', error);
    return [];
  }
}

/**
 * Save metric configuration
 */
export async function saveMetricConfig(config: Omit<MetricConfig, 'id' | 'created_at' | 'updated_at'>): Promise<MetricConfig> {
  try {
    const result = await sql`
      INSERT INTO repcard_metric_config (
        metric_name,
        disposition_pattern,
        status_category,
        is_included,
        priority
      ) VALUES (
        ${config.metric_name}::text,
        ${config.disposition_pattern}::text,
        ${config.status_category}::text,
        ${config.is_included}::boolean,
        ${config.priority}::integer
      )
      ON CONFLICT (metric_name, disposition_pattern, status_category)
      DO UPDATE SET
        is_included = EXCLUDED.is_included,
        priority = EXCLUDED.priority,
        updated_at = NOW()
      RETURNING *
    `;
    
    const row = Array.isArray(result) ? result[0] : result.rows?.[0];
    return row as MetricConfig;
  } catch (error) {
    console.error('[Metric Config] Error saving config:', error);
    throw error;
  }
}

/**
 * Delete metric configuration
 */
export async function deleteMetricConfig(id: string): Promise<void> {
  try {
    await sql`
      DELETE FROM repcard_metric_config
      WHERE id = ${id}::text
    `;
  } catch (error) {
    console.error('[Metric Config] Error deleting config:', error);
    throw error;
  }
}

/**
 * Discover all unique dispositions and status categories from database
 */
export async function discoverDispositions(): Promise<{
  dispositions: Array<{ value: string; count: number }>;
  statusCategories: Array<{ value: string; count: number }>;
}> {
  try {
    // Get unique dispositions with counts
    const dispositionResult = await sql`
      SELECT 
        disposition as value,
        COUNT(*)::int as count
      FROM repcard_appointments
      WHERE disposition IS NOT NULL
      GROUP BY disposition
      ORDER BY count DESC, disposition ASC
    `;
    
    // Get unique status categories with counts
    const statusResult = await sql`
      SELECT 
        status_category as value,
        COUNT(*)::int as count
      FROM repcard_appointments
      WHERE status_category IS NOT NULL
      GROUP BY status_category
      ORDER BY count DESC, status_category ASC
    `;
    
    const dispositions = Array.isArray(dispositionResult) 
      ? dispositionResult 
      : dispositionResult.rows || [];
    
    const statusCategories = Array.isArray(statusResult)
      ? statusResult
      : statusResult.rows || [];
    
    return {
      dispositions: dispositions.map((r: any) => ({
        value: r.value || '',
        count: r.count || 0
      })),
      statusCategories: statusCategories.map((r: any) => ({
        value: r.value || '',
        count: r.count || 0
      }))
    };
  } catch (error) {
    console.error('[Metric Config] Error discovering dispositions:', error);
    return { dispositions: [], statusCategories: [] };
  }
}
