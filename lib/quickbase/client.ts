// lib/quickbase/client.ts
export class QuickbaseClient {
  private realm: string;
  private token: string;
  private appId: string;

  constructor(realm: string, token: string, appId: string) {
    this.realm = realm;
    this.token = token;
    this.appId = appId;
  }

  async queryRecords(payload: { from: string; select: number[]; where?: string }): Promise<any> {
    // Mock implementation - in real app, this would make Quickbase REST API call
    return { data: [] };
  }

  async updateRecord(payload: { to: string; data: any[] }): Promise<any> {
    // Mock implementation - in real app, this would make Quickbase REST API call
    // Return proper response shape with metadata and data
    return { 
      metadata: { 
        createdRecordIds: [Math.floor(Math.random() * 1000000)],
        totalNumberOfRecordsProcessed: 1
      },
      data: [
        {
          3: { value: Math.floor(Math.random() * 1000000) } // Field 3 contains record ID
        }
      ]
    };
  }

  async uploadFileToRecord(payload: {
    tableId: string;
    recordId: number;
    fieldId: number;
    fileName: string;
    fileData: Buffer
  }): Promise<any> {
    const startTime = Date.now();
    logQuickbaseRequest('POST', `/v1/files`, {
      tableId: payload.tableId,
      recordId: payload.recordId,
      fieldId: payload.fieldId,
      fileName: payload.fileName,
      fileSize: payload.fileData.length
    });

    return this.queueRequest(async () => {
      // Convert buffer to Base64 for QuickBase API
      const base64Data = payload.fileData.toString('base64');

      const response = await this.fetchWithRetry(
        `https://api.quickbase.com/v1/files`,
        {
          method: 'POST',
          headers: {
            'QB-Realm-Hostname': this.realm,
            'Authorization': `QB-USER-TOKEN ${this.token}`,
            'Content-Type': 'application/json',
          },
          body: JSON.stringify({
            to: payload.tableId,
            data: [{
              [3]: { value: payload.recordId }, // Record ID field
              [payload.fieldId]: {
                value: {
                  fileName: payload.fileName,
                  data: base64Data
                }
              }
            }]
          }),
        }
      );

      if (!response.ok) {
        const error = await response.json();
        const errorMessage = error.message || response.statusText;
        logQuickbaseError('POST', '/v1/files', new Error(errorMessage));
        throw new Error(`Quickbase file upload error: ${errorMessage}`);
      }

      const json = await response.json();
      const duration = Date.now() - startTime;
      logQuickbaseResponse('POST', '/v1/files', duration, payload.fileData.length);

      console.log(`[QuickbaseClient] Uploaded file ${payload.fileName} (${payload.fileData.length} bytes) to record ${payload.recordId}`);
      return json;
    });
  }
}

// Create and export a configured client instance
const realm = process.env.QUICKBASE_REALM || '';
const token = process.env.QUICKBASE_TOKEN || '';
const appId = process.env.QUICKBASE_APP_ID || '';

export const qbClient = new QuickbaseClient(realm, token, appId);