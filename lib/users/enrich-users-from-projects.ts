/**
 * Background User Enrichment from Project Data
 *
 * Automatically creates/enriches users when projects are fetched.
 * Runs asynchronously and non-blocking to avoid impacting API response times.
 *
 * Strategy:
 * 1. Extract unique closer/setter emails from project data
 * 2. Call ensureUserExists() for each to create/enrich users
 * 3. Handle errors gracefully (don't block project fetching)
 * 4. Log enrichment activity for monitoring
 */

import 'server-only';
import { ensureUserExists } from './enrich-user';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import type { QuickbaseProject } from '@/lib/types/project';

/**
 * Extract unique users (closers + setters) from project data
 * Returns Map of email -> name for deduplication
 */
function extractUsersFromProjects(projects: QuickbaseProject[]): Map<string, string> {
  const userMap = new Map<string, string>();

  projects.forEach((project: QuickbaseProject) => {
    // Extract closer
    const closerEmail = project[PROJECT_FIELDS.CLOSER_EMAIL]?.value;
    const closerName = project[PROJECT_FIELDS.CLOSER_NAME]?.value;
    if (closerEmail && typeof closerEmail === 'string') {
      const email = closerEmail.toLowerCase().trim();
      if (!userMap.has(email)) {
        userMap.set(email, closerName || email);
      }
    }

    // Extract setter
    const setterEmail = project[PROJECT_FIELDS.SETTER_EMAIL]?.value;
    const setterName = project[PROJECT_FIELDS.SETTER_NAME]?.value;
    if (setterEmail && typeof setterEmail === 'string') {
      const email = setterEmail.toLowerCase().trim();
      if (!userMap.has(email)) {
        userMap.set(email, setterName || email);
      }
    }
  });

  return userMap;
}

/**
 * Enrich users from project data (async, non-blocking)
 *
 * This function is fire-and-forget - it runs in the background
 * and doesn't block the API response. Errors are logged but don't
 * propagate to the caller.
 *
 * @param projects - Array of QuickBase project records
 * @param context - Context string for logging (e.g., "getProjectsForUserList", "fetchProjects")
 */
export function enrichUsersFromProjects(
  projects: QuickbaseProject[],
  context: string
): void {
  // Don't await - run in background
  (async () => {
    try {
      if (!projects || projects.length === 0) {
        return;
      }

      const userMap = extractUsersFromProjects(projects);

      if (userMap.size === 0) {
        return;
      }

      console.log(`[enrichUsersFromProjects:${context}] Enriching ${userMap.size} unique users from ${projects.length} projects`);

      let enriched = 0;
      let errors = 0;

      // Process users sequentially to avoid overwhelming the database
      const userEntries = Array.from(userMap.entries());
      for (const [email, name] of userEntries) {
        try {
          await ensureUserExists(email, name);
          enriched++;
        } catch (error) {
          errors++;
          console.error(`[enrichUsersFromProjects:${context}] Failed to enrich user ${email}:`, error);
          // Continue processing other users despite errors
        }
      }

      if (enriched > 0 || errors > 0) {
        console.log(`[enrichUsersFromProjects:${context}] Complete - enriched: ${enriched}, errors: ${errors}`);
      }
    } catch (error) {
      console.error(`[enrichUsersFromProjects:${context}] Unexpected error:`, error);
      // Swallow error - don't let enrichment failures impact the main flow
    }
  })();
}
