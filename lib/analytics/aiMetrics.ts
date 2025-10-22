// Stub for AI metrics logging - temporarily disabled
export function logAIRequest(feature: string, projectId: string, details: any) {
  // Stub implementation
}

export function logAIResponse(feature: string, projectId: string, durationMs: number, response: any) {
  // Stub implementation
}

export function logAIError(feature: string, projectId: string, error: any, details?: any) {
  // Stub implementation
}

export function logAIFeedback(projectId: string, insightType: string, feedback: 'helpful' | 'not_helpful', suggestionId: string | undefined, userId: string) {
  // Stub implementation
}

export function logAIAction(projectId: string, insightType: string, action: string, suggestionId: string | undefined, userId: string, details?: any) {
  // Stub implementation
}

export function logAPIError(route: string, error: any, context?: any) {
  console.error(`[API Error] ${route}:`, error, context);
}
