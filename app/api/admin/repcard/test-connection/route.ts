import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';
import { repcardClient } from '@/lib/repcard/client';

export const runtime = 'nodejs';

/**
 * Test RepCard API connection
 * Checks if API key is configured and if we can connect to RepCard API
 */
export async function GET(request: NextRequest) {
  try {
    const session = await getServerSession(authOptions);
    if (!session || session.user.role !== 'super_admin') {
      return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
    }

    const apiKey = process.env.REPCARD_API_KEY;
    const apiUrl = process.env.REPCARD_API_URL || 'https://api.repcard.com';

    // Check if API key is set
    if (!apiKey) {
      return NextResponse.json({
        connected: false,
        error: 'REPCARD_API_KEY not configured',
        message: 'Set REPCARD_API_KEY environment variable in Vercel settings',
        apiUrl,
        hasApiKey: false
      }, { status: 200 });
    }

    // Test API connection by fetching users (lightweight request)
    try {
      const response = await repcardClient.getUsersMinimal({ page: 1, perPage: 1 });
      
      return NextResponse.json({
        connected: true,
        message: 'Successfully connected to RepCard API',
        apiUrl,
        hasApiKey: true,
        apiKeyLength: apiKey.length,
        testResponse: {
          status: response.status,
          statusCode: response.statusCode,
          message: response.message,
          hasData: !!response.result?.data,
          dataCount: response.result?.data?.length || 0
        }
      });
    } catch (error) {
      return NextResponse.json({
        connected: false,
        error: 'Failed to connect to RepCard API',
        message: error instanceof Error ? error.message : 'Unknown error',
        apiUrl,
        hasApiKey: true,
        apiKeyLength: apiKey.length,
        details: error instanceof Error ? {
          name: error.name,
          stack: error.stack
        } : undefined
      }, { status: 200 });
    }
  } catch (error) {
    return NextResponse.json(
      { 
        connected: false,
        error: 'Test failed', 
        message: error instanceof Error ? error.message : 'Unknown error' 
      },
      { status: 500 }
    );
  }
}
