'use client'

import React, { useState, useRef, useCallback } from 'react'
import { Upload, X, File, FileText, Image, AlertCircle, CheckCircle } from 'lucide-react'
import { Button } from '@/components/ui/button'
import { Progress } from '@/components/ui/progress'
import { Alert, AlertDescription } from '@/components/ui/alert'
import { cn } from '@/lib/utils/cn'

interface FileUploadProps {
  onFileSelect: (file: File | null) => void
  accept?: string
  maxSize?: number
  uploadProgress?: number
  disabled?: boolean
  error?: string
  className?: string
  id?: string
  // Controlled API
  file?: File | null
  defaultFile?: File | null
}

// Validation constants
const ALLOWED_MIME_TYPES = [
  'application/pdf',
  'image/jpeg',
  'image/png',
  'application/msword',
  'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
]

const ALLOWED_EXTENSIONS = ['.pdf', '.jpg', '.jpeg', '.png', '.doc', '.docx']

const DEFAULT_MAX_SIZE = 10 * 1024 * 1024 // 10 MB

const FILE_TYPE_LABELS = {
  '.pdf': 'PDF',
  '.jpg': 'JPG',
  '.jpeg': 'JPEG',
  '.png': 'PNG',
  '.doc': 'DOC',
  '.docx': 'DOCX'
}

export const FileUpload = React.forwardRef<HTMLDivElement, FileUploadProps>(
  (
    {
      onFileSelect,
      accept = '.pdf,.jpg,.jpeg,.png,.doc,.docx',
      maxSize = DEFAULT_MAX_SIZE,
      uploadProgress,
      disabled = false,
      error,
      className,
      id,
      file: controlledFile,
      defaultFile = null
    },
    ref
  ) => {
    const [isDragging, setIsDragging] = useState(false)
    const [selectedFile, setSelectedFile] = useState<File | null>(defaultFile)
    const [validationError, setValidationError] = useState<string | null>(null)
    const fileInputRef = useRef<HTMLInputElement>(null)

    // Derive active file from controlled prop or internal state
    const activeFile = controlledFile !== undefined ? controlledFile : selectedFile
    const isControlled = controlledFile !== undefined

    const isUploading = uploadProgress !== undefined && uploadProgress > 0 && uploadProgress < 100

    // Helper function to format file size
    const formatFileSize = useCallback((bytes: number): string => {
      if (bytes < 1024) {
        return `${bytes} bytes`
      }
      if (bytes < 1024 * 1024) {
        return `${(bytes / 1024).toFixed(1)} KB`
      }
      return `${(bytes / (1024 * 1024)).toFixed(1)} MB`
    }, [])

    // Helper function to parse accept prop into allowed types
    const parseAcceptTypes = useCallback((acceptString: string) => {
      const types = acceptString.split(',').map(type => type.trim())
      const extensions: string[] = []
      const mimeTypes: string[] = []
      
      types.forEach(type => {
        if (type.startsWith('.')) {
          extensions.push(type.toLowerCase())
        } else if (type.includes('/')) {
          mimeTypes.push(type.toLowerCase())
        }
      })
      
      return { extensions, mimeTypes }
    }, [])

    // Helper function to validate file
    const validateFile = useCallback((file: File): string | null => {
      // Check file size
      if (file.size > maxSize) {
        return `File size exceeds ${formatFileSize(maxSize)} limit`
      }

      // Derive allowed types
      let { extensions: allowedExtensions, mimeTypes: allowedMimeTypes } = accept 
        ? parseAcceptTypes(accept)
        : { extensions: ALLOWED_EXTENSIONS, mimeTypes: ALLOWED_MIME_TYPES }

      // Fallback to defaults if accept is provided but parses to no types
      if (accept && allowedExtensions.length === 0 && allowedMimeTypes.length === 0) {
        allowedExtensions = ALLOWED_EXTENSIONS
        allowedMimeTypes = ALLOWED_MIME_TYPES
      }

      // Compute extension from file name
      const extension = '.' + file.name.split('.').pop()?.toLowerCase()

      // Implement wildcard-aware MIME matching
      let matchesMime = false
      for (const allowedMime of allowedMimeTypes) {
        if (allowedMime === file.type) {
          // Exact match
          matchesMime = true
          break
        } else if (allowedMime.endsWith('/*')) {
          // Wildcard match
          const mimeFamily = allowedMime.split('/*')[0] + '/'
          if (file.type.startsWith(mimeFamily)) {
            matchesMime = true
            break
          }
        }
      }

      // Implement extension matching
      const matchesExt = allowedExtensions.includes(extension)

      // Determine pass/fail based on whether accept was provided
      const hasAcceptRestriction = accept !== undefined
      const isValid = matchesMime || matchesExt

      if (hasAcceptRestriction && !isValid) {
        // Build allowed list for error message
        const allowedTypes: string[] = []
        
        // Add MIME types (with family labels for wildcards)
        allowedMimeTypes.forEach(mime => {
          if (mime.endsWith('/*')) {
            const family = mime.split('/*')[0]
            allowedTypes.push(`${family}/*`)
          } else {
            allowedTypes.push(mime)
          }
        })
        
        // Add extensions (uppercase without dot)
        allowedExtensions.forEach(ext => {
          allowedTypes.push(ext.slice(1).toUpperCase())
        })
        
        return `Invalid file type. Allowed: ${allowedTypes.join(', ')}`
      }

      // For fallback behavior when no accept prop, use same validation but with default sets
      if (!hasAcceptRestriction && !isValid) {
        const allowedTypes = [...allowedMimeTypes, ...allowedExtensions]
          .map(type => type.startsWith('.') ? type.slice(1).toUpperCase() : type)
          .join(', ')
        return `Invalid file type. Allowed: ${allowedTypes}`
      }

      return null
    }, [maxSize, formatFileSize, accept, parseAcceptTypes])

    // Helper function to get file icon
    const getFileIcon = useCallback((file: File): React.ReactNode => {
      const extension = '.' + file.name.split('.').pop()?.toLowerCase()
      
      if (extension === '.pdf') {
        return <FileText className="h-8 w-8 text-red-500" />
      }
      if (['.jpg', '.jpeg', '.png'].includes(extension)) {
        return <Image className="h-8 w-8 text-blue-500" />
      }
      if (['.doc', '.docx'].includes(extension)) {
        return <File className="h-8 w-8 text-blue-600" />
      }
      return <File className="h-8 w-8 text-gray-500" />
    }, [])

    // Event handlers
    const handleDragEnter = useCallback((e: React.DragEvent) => {
      e.preventDefault()
      e.stopPropagation()
      if (!disabled) {
        setIsDragging(true)
      }
    }, [disabled])

    const handleDragOver = useCallback((e: React.DragEvent) => {
      e.preventDefault()
      e.stopPropagation()
      if (!disabled) {
        e.dataTransfer.dropEffect = 'copy'
      }
    }, [disabled])

    const handleDragLeave = useCallback((e: React.DragEvent) => {
      e.preventDefault()
      e.stopPropagation()
      setIsDragging(false)
    }, [])

    const handleDrop = useCallback((e: React.DragEvent) => {
      e.preventDefault()
      e.stopPropagation()
      setIsDragging(false)

      if (disabled) return

      const files = e.dataTransfer.files
      if (files.length === 0) return

      const newFile = files[0]
      const error = validateFile(newFile)
      
      if (error) {
        setValidationError(error)
        return
      }

      setValidationError(null)
      // Only update internal state if uncontrolled
      if (!isControlled) {
        setSelectedFile(newFile)
      }
      onFileSelect(newFile)
    }, [disabled, validateFile, onFileSelect, isControlled])

    const handleFileInputChange = useCallback((e: React.ChangeEvent<HTMLInputElement>) => {
      const files = e.target.files
      if (!files || files.length === 0) return

      const newFile = files[0]
      const error = validateFile(newFile)
      
      if (error) {
        setValidationError(error)
        return
      }

      setValidationError(null)
      // Only update internal state if uncontrolled
      if (!isControlled) {
        setSelectedFile(newFile)
      }
      onFileSelect(newFile)
    }, [validateFile, onFileSelect, isControlled])

    const handleBrowseClick = useCallback(() => {
      if (!disabled && fileInputRef.current) {
        fileInputRef.current.click()
      }
    }, [disabled])

    const handleRemoveFile = useCallback(() => {
      // Only update internal state if uncontrolled
      if (!isControlled) {
        setSelectedFile(null)
      }
      setValidationError(null)
      if (fileInputRef.current) {
        fileInputRef.current.value = ''
      }
      onFileSelect(null)
    }, [onFileSelect, isControlled])

    const handleKeyDown = useCallback((e: React.KeyboardEvent) => {
      if (e.key === 'Enter' || e.key === ' ') {
        e.preventDefault()
        handleBrowseClick()
      }
    }, [handleBrowseClick])

    return (
      <div ref={ref} className={cn('w-full', className)}>
        {/* Hidden file input */}
        <input
          ref={fileInputRef}
          type="file"
          accept={accept}
          onChange={handleFileInputChange}
          disabled={disabled}
          className="hidden"
          id={id}
        />

        {/* Upload UI */}
        {!activeFile ? (
          // Drag and drop zone
          <div
            className={cn(
              'border-2 border-dashed rounded-lg p-6 transition-colors duration-200',
              'flex flex-col items-center justify-center min-h-[200px]',
              'bg-gray-50 hover:bg-gray-100',
              isDragging && 'border-primary bg-primary/5',
              !isDragging && 'border-gray-300',
              disabled && 'opacity-50 cursor-not-allowed',
              !disabled && 'cursor-pointer'
            )}
            onDragEnter={handleDragEnter}
            onDragOver={handleDragOver}
            onDragLeave={handleDragLeave}
            onDrop={handleDrop}
            onClick={handleBrowseClick}
            onKeyDown={handleKeyDown}
            role="button"
            tabIndex={disabled ? -1 : 0}
            aria-label="File upload area"
            aria-describedby="upload-hint"
          >
            <Upload className="h-12 w-12 text-gray-400 mb-4" />
            <p className="text-base text-gray-600 mb-2">
              Drag and drop your file here
            </p>
            <p className="text-sm text-gray-500 mb-4">or</p>
            <Button variant="outline" disabled={disabled}>
              Browse Files
            </Button>
            <p id="upload-hint" className="text-xs text-gray-500 mt-2 text-center">
              Accepted: PDF, JPG, PNG, DOC, DOCX (max {formatFileSize(maxSize)})
            </p>
          </div>
        ) : (
          // File preview
          <div className="border border-gray-200 rounded-lg p-4 bg-white">
            <div className="flex items-center justify-between">
              <div className="flex items-center space-x-3">
                {getFileIcon(activeFile)}
                <div>
                  <p className="text-sm font-medium text-gray-900">
                    {activeFile.name}
                  </p>
                  <p className="text-xs text-gray-500">
                    {formatFileSize(activeFile.size)}
                  </p>
                </div>
              </div>
              {!isUploading && !disabled && (
                <Button
                  variant="ghost"
                  size="icon"
                  onClick={handleRemoveFile}
                  aria-label="Remove file"
                >
                  <X className="h-4 w-4" />
                </Button>
              )}
            </div>

            {/* Upload progress */}
            {uploadProgress !== undefined && uploadProgress > 0 && uploadProgress < 100 && (
              <div className="mt-3">
                <Progress value={uploadProgress} className="h-2" />
                <p className="text-xs text-gray-600 mt-1">
                  Uploading... {Math.round(uploadProgress)}%
                </p>
              </div>
            )}

            {/* Upload complete */}
            {uploadProgress === 100 && (
              <Alert className="mt-3">
                <CheckCircle className="h-4 w-4" />
                <AlertDescription>
                  File uploaded successfully
                </AlertDescription>
              </Alert>
            )}
          </div>
        )}

        {/* Error states */}
        {(validationError || error) && (
          <Alert variant="destructive" className="mt-3">
            <AlertCircle className="h-4 w-4" />
            <AlertDescription>
              {validationError || error}
            </AlertDescription>
          </Alert>
        )}
      </div>
    )
  }
)

FileUpload.displayName = 'FileUpload'
