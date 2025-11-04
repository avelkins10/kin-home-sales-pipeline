// app/api/admin/diagnose-arrivy-data/route.ts
import { NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';
import { arrivyClient } from '@/lib/integrations/arrivy/client';
import { sql } from '@vercel/postgres';

export const runtime = 'nodejs';
export const maxDuration = 300;

/**
 * Diagnostic endpoint to fetch and display real Arrivy API data
 * Shows actual structure of tasks, events, statuses, templates, etc.
 * 
 * GET /api/admin/diagnose-arrivy-data?taskId=123&limit=5
 */
export async function GET(request: Request) {
  try {
    // Check authentication
    const session = await getServerSession(authOptions);
    if (!session) {
      return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
    }

    if (!arrivyClient) {
      return NextResponse.json({ error: 'Arrivy client not configured' }, { status: 500 });
    }

    const { searchParams } = new URL(request.url);
    const taskId = searchParams.get('taskId');
    const limit = parseInt(searchParams.get('limit') || '5', 10);

    const results: any = {
      timestamp: new Date().toISOString(),
      summary: {},
      samples: {},
    };

    // 1. Fetch recent tasks from Arrivy API
    try {
      const endDate = new Date();
      const startDate = new Date();
      startDate.setDate(startDate.getDate() - 7); // Last 7 days

      const tasks = await arrivyClient.listTasks({
        start_date: startDate,
        end_date: endDate,
      });

      results.summary.tasks_found = tasks.length;
      results.samples.tasks = tasks.slice(0, limit).map(task => ({
        id: task.id,
        url_safe_id: task.url_safe_id,
        external_id: task.external_id,
        customer_name: task.customer_name,
        title: task.title,
        details: task.details,
        template: task.template,
        template_id: task.template_id,
        template_type: typeof task.template,
        status: task.status,
        group: task.group,
        extra_fields: task.extra_fields,
        extra_fields_keys: task.extra_fields ? Object.keys(task.extra_fields) : [],
        extra_fields_sample: task.extra_fields ? Object.entries(task.extra_fields).slice(0, 10).map(([k, v]) => ({
          key: k,
          value_type: typeof v,
          value_preview: typeof v === 'string' ? v.substring(0, 100) : typeof v === 'object' ? JSON.stringify(v).substring(0, 100) : String(v)
        })) : [],
        start_datetime: task.start_datetime,
        end_datetime: task.end_datetime,
        entity_ids: task.entity_ids,
        // Full raw task for reference
        raw_task: task,
      }));
    } catch (error) {
      results.samples.tasks_error = error instanceof Error ? error.message : String(error);
    }

    // 2. If specific task ID provided, get detailed data
    if (taskId) {
      try {
        const taskIdNum = parseInt(taskId, 10);
        const task = await arrivyClient.getTask(taskIdNum);
        
        results.samples.specific_task = {
          id: task.id,
          url_safe_id: task.url_safe_id,
          external_id: task.external_id,
          customer_name: task.customer_name,
          title: task.title,
          details: task.details,
          template: task.template,
          template_id: task.template_id,
          template_type: typeof task.template,
          status: task.status,
          group: task.group,
          extra_fields: task.extra_fields,
          extra_fields_keys: task.extra_fields ? Object.keys(task.extra_fields) : [],
          extra_fields_full: task.extra_fields, // Full extra_fields for inspection
          start_datetime: task.start_datetime,
          end_datetime: task.end_datetime,
          entity_ids: task.entity_ids,
          // Full raw task
          raw_task: task,
        };

        // Get status history for this task
        try {
          const statusHistory = await arrivyClient.getTaskStatuses(taskIdNum);
          results.samples.specific_task_status_history = statusHistory.map(status => ({
            id: status.id,
            type: status.type,
            type_title: status.type_title,
            time: status.time,
            reporter_name: status.reporter_name,
            reporter_id: status.reporter_id,
            notes: status.notes,
            extra_fields: status.extra_fields,
            files: status.files,
            raw_status: status,
          }));
        } catch (error) {
          results.samples.specific_task_status_error = error instanceof Error ? error.message : String(error);
        }
      } catch (error) {
        results.samples.specific_task_error = error instanceof Error ? error.message : String(error);
      }
    }

    // 3. Get sample tasks from database for comparison
    try {
      const dbTasks = await sql`
        SELECT 
          arrivy_task_id,
          customer_name,
          task_type,
          current_status,
          template_id,
          extra_fields,
          scheduled_start,
          synced_at
        FROM arrivy_tasks
        ORDER BY synced_at DESC
        LIMIT ${limit}
      `;

      results.samples.database_tasks = dbTasks.rows.map(t => ({
        arrivy_task_id: t.arrivy_task_id,
        customer_name: t.customer_name,
        task_type: t.task_type,
        current_status: t.current_status,
        template_id: t.template_id,
        extra_fields_keys: t.extra_fields ? Object.keys(t.extra_fields) : [],
        extra_fields_sample: t.extra_fields ? Object.entries(t.extra_fields).slice(0, 10).map(([k, v]) => ({
          key: k,
          value_preview: typeof v === 'string' ? v.substring(0, 50) : String(v).substring(0, 50)
        })) : [],
        scheduled_start: t.scheduled_start,
        synced_at: t.synced_at,
      }));
    } catch (error) {
      results.samples.database_tasks_error = error instanceof Error ? error.message : String(error);
    }

    // 4. Get sample events from database (includes webhook payloads)
    try {
      const dbEvents = await sql`
        SELECT 
          event_id,
          event_type,
          event_sub_type,
          arrivy_task_id,
          event_time,
          reporter_name,
          title,
          message,
          object_fields,
          extra_fields
        FROM arrivy_events
        ORDER BY event_time DESC
        LIMIT ${limit}
      `;

      results.samples.database_events = dbEvents.rows.map(e => ({
        event_id: e.event_id,
        event_type: e.event_type,
        event_sub_type: e.event_sub_type,
        arrivy_task_id: e.arrivy_task_id,
        event_time: e.event_time,
        reporter_name: e.reporter_name,
        title: e.title,
        message: e.message,
        object_fields: e.object_fields, // Full object_fields (webhook payload structure)
        extra_fields: e.extra_fields, // Full extra_fields
        object_fields_keys: e.object_fields ? Object.keys(e.object_fields) : [],
        extra_fields_keys: e.extra_fields ? Object.keys(e.extra_fields) : [],
        // Show first few entries as preview
        object_fields_preview: e.object_fields ? Object.entries(e.object_fields).slice(0, 10).map(([k, v]) => ({
          key: k,
          value_type: typeof v,
          value: v
        })) : [],
        extra_fields_preview: e.extra_fields ? Object.entries(e.extra_fields).slice(0, 10).map(([k, v]) => ({
          key: k,
          value_type: typeof v,
          value: v
        })) : [],
      }));
    } catch (error) {
      results.samples.database_events_error = error instanceof Error ? error.message : String(error);
    }

    // 5. Get sample status history from database
    try {
      const dbStatuses = await sql`
        SELECT 
          arrivy_task_id,
          status_type,
          reported_at,
          reporter_name,
          notes,
          source
        FROM arrivy_task_status
        ORDER BY reported_at DESC
        LIMIT ${limit * 2}
      `;

      results.samples.database_statuses = dbStatuses.rows.map(s => ({
        arrivy_task_id: s.arrivy_task_id,
        status_type: s.status_type,
        reported_at: s.reported_at,
        reporter_name: s.reporter_name,
        notes: s.notes,
        source: s.source,
      }));
    } catch (error) {
      results.samples.database_statuses_error = error instanceof Error ? error.message : String(error);
    }

    // 6. Try to get entities/crew members
    try {
      const entities = await arrivyClient.listEntities();
      results.summary.entities_found = entities.length;
      results.samples.entities = entities.slice(0, limit).map(e => ({
        id: e.id,
        name: e.name,
        email: e.email,
        phone: e.phone,
        entity_type: e.entity_type,
        extra_fields: e.extra_fields,
        raw_entity: e,
      }));
    } catch (error) {
      results.samples.entities_error = error instanceof Error ? error.message : String(error);
    }

    // 7. Analysis: Compare API vs Database and provide mapping recommendations
    results.analysis = {
      task_type_detection_issues: [],
      template_analysis: {
        tasks_with_template_name: 0,
        tasks_with_template_id_only: 0,
        tasks_with_no_template: 0,
        template_names_found: [] as string[],
        template_ids_found: [] as number[],
      },
      extra_fields_analysis: {
        common_keys: {} as Record<string, number>,
        sample_values: {} as Record<string, any>,
        task_type_signals: [] as string[],
      },
      mapping_recommendations: [] as string[],
    };

    if (results.samples.tasks && Array.isArray(results.samples.tasks)) {
      const tasks = results.samples.tasks;
      
      for (const task of tasks) {
        // Template analysis
        if (task.template && typeof task.template === 'string') {
          results.analysis.template_analysis.tasks_with_template_name++;
          if (!results.analysis.template_analysis.template_names_found.includes(task.template)) {
            results.analysis.template_analysis.template_names_found.push(task.template);
          }
        } else if (task.template_id) {
          results.analysis.template_analysis.tasks_with_template_id_only++;
          if (!results.analysis.template_analysis.template_ids_found.includes(task.template_id)) {
            results.analysis.template_analysis.template_ids_found.push(task.template_id);
          }
        } else {
          results.analysis.template_analysis.tasks_with_no_template++;
        }

        // Extra fields analysis
        if (task.extra_fields && typeof task.extra_fields === 'object') {
          for (const [key, value] of Object.entries(task.extra_fields)) {
            results.analysis.extra_fields_analysis.common_keys[key] = 
              (results.analysis.extra_fields_analysis.common_keys[key] || 0) + 1;
            
            // Store sample value for first occurrence
            if (!results.analysis.extra_fields_analysis.sample_values[key]) {
              results.analysis.extra_fields_analysis.sample_values[key] = 
                typeof value === 'string' ? value.substring(0, 100) : value;
            }

            // Check for task type signals
            const keyLower = key.toLowerCase();
            const valueStr = typeof value === 'string' ? value.toLowerCase() : String(value).toLowerCase();
            
            if (keyLower.includes('survey') || valueStr.includes('survey')) {
              if (!results.analysis.extra_fields_analysis.task_type_signals.includes('survey')) {
                results.analysis.extra_fields_analysis.task_type_signals.push('survey');
              }
            }
            if (keyLower.includes('install') || valueStr.includes('install')) {
              if (!results.analysis.extra_fields_analysis.task_type_signals.includes('install')) {
                results.analysis.extra_fields_analysis.task_type_signals.push('install');
              }
            }
            if (keyLower.includes('inspection') || valueStr.includes('inspection')) {
              if (!results.analysis.extra_fields_analysis.task_type_signals.includes('inspection')) {
                results.analysis.extra_fields_analysis.task_type_signals.push('inspection');
              }
            }
          }
        }

        // Check for detection issues
        if (task.raw_task) {
          const detectionIssues = [];
          if (!task.template && !task.template_id) {
            detectionIssues.push('No template data');
          }
          if (!task.extra_fields || Object.keys(task.extra_fields).length === 0) {
            detectionIssues.push('No extra_fields');
          }
          if (!task.details) {
            detectionIssues.push('No details field');
          }
          if (!task.group?.name) {
            detectionIssues.push('No group name');
          }
          
          if (detectionIssues.length > 0) {
            results.analysis.task_type_detection_issues.push({
              task_id: task.id,
              customer_name: task.customer_name,
              issues: detectionIssues,
            });
          }
        }
      }

      // Generate mapping recommendations
      if (results.analysis.template_analysis.tasks_with_template_id_only > 0) {
        results.analysis.mapping_recommendations.push(
          `⚠️ ${results.analysis.template_analysis.tasks_with_template_id_only} tasks have template_id but no template name. Consider fetching template names from Arrivy API or using template_id lookup.`
        );
      }

      if (results.analysis.template_analysis.tasks_with_no_template > 0) {
        results.analysis.mapping_recommendations.push(
          `⚠️ ${results.analysis.template_analysis.tasks_with_no_template} tasks have no template data. Rely on extra_fields or details field for type detection.`
        );
      }

      if (results.analysis.extra_fields_analysis.task_type_signals.length > 0) {
        results.analysis.mapping_recommendations.push(
          `✅ Found task type signals in extra_fields: ${results.analysis.extra_fields_analysis.task_type_signals.join(', ')}. Update detection logic to check these fields.`
        );
      }

      // Sort common keys by frequency
      const sortedKeys = Object.entries(results.analysis.extra_fields_analysis.common_keys)
        .sort((a, b) => b[1] - a[1])
        .slice(0, 20);
      
      results.analysis.extra_fields_analysis.top_keys = sortedKeys.map(([key, count]) => ({
        key,
        count,
        sample_value: results.analysis.extra_fields_analysis.sample_values[key]
      }));
    }

    return NextResponse.json(results, { 
      status: 200,
      headers: {
        'Content-Type': 'application/json',
      }
    });

  } catch (error) {
    return NextResponse.json(
      { 
        error: 'Failed to diagnose Arrivy data',
        message: error instanceof Error ? error.message : String(error),
        stack: process.env.NODE_ENV === 'development' ? (error instanceof Error ? error.stack : undefined) : undefined
      },
      { status: 500 }
    );
  }
}

