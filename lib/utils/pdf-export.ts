import html2canvas from 'html2canvas';
import jsPDF from 'jspdf';

export interface PDFExportOptions {
  scale?: number;
  orientation?: 'portrait' | 'landscape';
  format?: 'a4' | 'letter';
  margin?: number;
}

/**
 * Calculate optimal dimensions for fitting canvas image on PDF page
 */
export function calculatePDFDimensions(
  canvasWidth: number,
  canvasHeight: number,
  format: string,
  margin: number
): { width: number; height: number; x: number; y: number } {
  // PDF page dimensions in mm
  const pageDimensions = {
    a4: { width: 210, height: 297 },
    letter: { width: 216, height: 279 }
  };

  const page = pageDimensions[format as keyof typeof pageDimensions] || pageDimensions.letter;
  const availableWidth = page.width - (margin * 2);
  const availableHeight = page.height - (margin * 2);

  // Calculate scale to fit within page bounds
  const scaleX = availableWidth / canvasWidth;
  const scaleY = availableHeight / canvasHeight;
  const scale = Math.min(scaleX, scaleY, 1); // Don't scale up

  const width = canvasWidth * scale;
  const height = canvasHeight * scale;
  const x = (page.width - width) / 2;
  const y = (page.height - height) / 2;

  return { width, height, x, y };
}

/**
 * Export an HTML element to PDF
 * @param elementId - DOM element ID to capture
 * @param filename - Output PDF filename
 * @param options - Optional configuration
 */
export async function exportElementToPDF(
  elementId: string,
  filename: string,
  options: PDFExportOptions = {}
): Promise<void> {
  try {
    const {
      scale = 2,
      orientation = 'portrait',
      format = 'letter',
      margin = 10
    } = options;

    // Get the element to capture
    const element = document.getElementById(elementId);
    if (!element) {
      throw new Error(`Element with ID "${elementId}" not found`);
    }

    // Check if element is visible
    const rect = element.getBoundingClientRect();
    if (rect.width === 0 || rect.height === 0) {
      throw new Error('Element is not visible or has zero dimensions');
    }

    // Capture element as canvas
    const canvas = await html2canvas(element, {
      scale,
      useCORS: true,
      logging: false,
      backgroundColor: '#ffffff',
      allowTaint: true,
      foreignObjectRendering: true
    });

    // Calculate PDF dimensions
    const { width, height, x, y } = calculatePDFDimensions(
      canvas.width,
      canvas.height,
      format,
      margin
    );

    // Create PDF with resolved orientation
    const resolvedOrientation = options.orientation ?? (width > height ? 'landscape' : 'portrait');
    const pdf = new jsPDF({
      orientation: resolvedOrientation,
      unit: 'mm',
      format
    });

    // Add image to PDF
    const imgData = canvas.toDataURL('image/png', 1.0);
    pdf.addImage(imgData, 'PNG', x, y, width, height);

    // Save PDF
    pdf.save(filename);

  } catch (error) {
    console.error('PDF export failed:', error);
    throw new Error(
      error instanceof Error 
        ? error.message 
        : 'Failed to export PDF. Please try again.'
    );
  }
}

/**
 * Export multiple elements to a single PDF
 * @param elementIds - Array of DOM element IDs to capture
 * @param filename - Output PDF filename
 * @param options - Optional configuration
 */
export async function exportElementsToPDF(
  elementIds: string[],
  filename: string,
  options: PDFExportOptions = {}
): Promise<void> {
  try {
    const {
      scale = 2,
      format = 'letter',
      margin = 10
    } = options;

    const pdf = new jsPDF({
      orientation: 'portrait',
      unit: 'mm',
      format
    });

    let isFirstPage = true;

    for (const elementId of elementIds) {
      const element = document.getElementById(elementId);
      if (!element) {
        console.warn(`Element with ID "${elementId}" not found, skipping`);
        continue;
      }

      // Check if element is visible
      const rect = element.getBoundingClientRect();
      if (rect.width === 0 || rect.height === 0) {
        console.warn(`Element "${elementId}" is not visible, skipping`);
        continue;
      }

      // Capture element as canvas
      const canvas = await html2canvas(element, {
        scale,
        useCORS: true,
        logging: false,
        backgroundColor: '#ffffff',
        allowTaint: true,
        foreignObjectRendering: true
      });

      // Calculate PDF dimensions
      const { width, height, x, y } = calculatePDFDimensions(
        canvas.width,
        canvas.height,
        format,
        margin
      );

      // Add new page if not the first element
      if (!isFirstPage) {
        pdf.addPage();
      }

      // Add image to PDF
      const imgData = canvas.toDataURL('image/png', 1.0);
      pdf.addImage(imgData, 'PNG', x, y, width, height);

      isFirstPage = false;
    }

    // Save PDF
    pdf.save(filename);

  } catch (error) {
    console.error('Multi-element PDF export failed:', error);
    throw new Error(
      error instanceof Error 
        ? error.message 
        : 'Failed to export PDF. Please try again.'
    );
  }
}
