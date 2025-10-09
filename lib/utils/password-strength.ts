/**
 * Password strength calculation utilities
 */

export interface PasswordStrength {
  score: number;
  label: 'Weak' | 'Fair' | 'Good' | 'Strong';
  color: 'red' | 'orange' | 'yellow' | 'green';
}

export interface PasswordRequirement {
  met: boolean;
  text: string;
}

/**
 * Calculate password strength score and label
 */
export function calculatePasswordStrength(password: string): PasswordStrength {
  let score = 0;
  
  // Length scoring
  if (password.length >= 8) score += 20;
  if (password.length >= 12) score += 10;
  if (password.length >= 16) score += 10;
  
  // Character type scoring
  if (/[A-Z]/.test(password)) score += 15; // Uppercase
  if (/[a-z]/.test(password)) score += 15; // Lowercase
  if (/[0-9]/.test(password)) score += 15; // Numbers
  if (/[!@#$%^&*()_+\-=\[\]{};':"\\|,.<>\/?]/.test(password)) score += 15; // Special chars
  
  // Variety bonus
  const charTypes = [
    /[A-Z]/.test(password),
    /[a-z]/.test(password),
    /[0-9]/.test(password),
    /[!@#$%^&*()_+\-=\[\]{};':"\\|,.<>\/?]/.test(password)
  ].filter(Boolean).length;
  
  if (charTypes >= 3) score += 10;
  
  // Cap at 100
  score = Math.min(score, 100);
  
  // Determine label and color
  let label: PasswordStrength['label'];
  let color: PasswordStrength['color'];
  
  if (score <= 25) {
    label = 'Weak';
    color = 'red';
  } else if (score <= 50) {
    label = 'Fair';
    color = 'orange';
  } else if (score <= 75) {
    label = 'Good';
    color = 'yellow';
  } else {
    label = 'Strong';
    color = 'green';
  }
  
  return { score, label, color };
}

/**
 * Get password requirements checklist
 */
export function getPasswordRequirements(password: string): PasswordRequirement[] {
  return [
    {
      met: password.length >= 8,
      text: 'At least 8 characters'
    },
    {
      met: /[A-Z]/.test(password),
      text: 'Contains uppercase letter'
    },
    {
      met: /[a-z]/.test(password),
      text: 'Contains lowercase letter'
    },
    {
      met: /[0-9]/.test(password),
      text: 'Contains number'
    },
    {
      met: /[!@#$%^&*()_+\-=\[\]{};':"\\|,.<>\/?]/.test(password),
      text: 'Contains special character'
    }
  ];
}

/**
 * Check if password meets minimum requirements
 */
export function isPasswordValid(password: string): boolean {
  return password.length >= 8;
}
