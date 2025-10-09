'use client';

import { signIn } from 'next-auth/react';
import { useRouter, useSearchParams } from 'next/navigation';
import { useState, useEffect } from 'react';
import Image from 'next/image';
import Link from 'next/link';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { Badge } from '@/components/ui/badge';
import { AlertCircle, CheckCircle, Info } from 'lucide-react';
import { toast } from 'sonner';
import { PasswordStrengthIndicator } from '@/components/auth/PasswordStrengthIndicator';
import { RoleExplanation } from '@/components/auth/RoleExplanation';
import { getRoleDisplayName, getRoleBadgeColor } from '@/lib/utils/role-helpers';
import { isPasswordValid } from '@/lib/utils/password-strength';

interface InviteData {
  email: string;
  name: string;
  role: string;
  office?: string;
  offices?: string[];
  invitedAt: string;
  invitedBy: string;
  invitedByName?: string;
  accepted: boolean;
}

export default function AcceptInvitePage() {
  const [inviteData, setInviteData] = useState<InviteData | null>(null);
  const [password, setPassword] = useState('');
  const [confirmPassword, setConfirmPassword] = useState('');
  const [isLoading, setIsLoading] = useState(false);
  const [isValidating, setIsValidating] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [passwordError, setPasswordError] = useState<string | null>(null);
  
  const router = useRouter();
  const searchParams = useSearchParams();
  const token = searchParams.get('token');

  // Validate token on mount
  useEffect(() => {
    if (!token) {
      setError('Invalid invite link. No token provided.');
      setIsValidating(false);
      return;
    }

    const validateToken = async () => {
      try {
        const response = await fetch(`/api/admin/users/invite?token=${token}`);
        const data = await response.json();

        if (!response.ok) {
          if (response.status === 404) {
            setError('This invite link is invalid or has expired. Please contact your administrator.');
          } else if (response.status === 410) {
            if (data.error === 'Invite has expired') {
              setError('This invite has expired (7 days). Please request a new invite.');
            } else if (data.error === 'Invite has already been used') {
              setError('This invite has already been used. Please sign in instead.');
            } else {
              setError('This invite is no longer valid. Please contact your administrator.');
            }
          } else {
            setError('Failed to validate invite. Please try again.');
          }
        } else {
          setInviteData(data);
        }
      } catch (error) {
        setError('Failed to validate invite. Please check your connection and try again.');
      } finally {
        setIsValidating(false);
      }
    };

    validateToken();
  }, [token]);

  // Validate password match
  useEffect(() => {
    if (confirmPassword && password !== confirmPassword) {
      setPasswordError('Passwords do not match');
    } else if (confirmPassword && password === confirmPassword) {
      setPasswordError(null);
    }
  }, [password, confirmPassword]);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!inviteData) return;

    // Validate password
    if (!isPasswordValid(password)) {
      setPasswordError('Password must be at least 8 characters');
      return;
    }

    if (password !== confirmPassword) {
      setPasswordError('Passwords do not match');
      return;
    }

    setIsLoading(true);
    setPasswordError(null);

    try {
      // Accept invite
      const response = await fetch('/api/admin/users/invite/accept', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          token,
          password,
        }),
      });

      const data = await response.json();

      if (!response.ok) {
        if (response.status === 404) {
          setError('This invite link is invalid or has expired. Please contact your administrator.');
        } else if (response.status === 410) {
          if (data.error === 'Invite has expired') {
            setError('This invite has expired (7 days). Please request a new invite.');
          } else if (data.error === 'Invite has already been used') {
            setError('This invite has already been used. Please sign in instead.');
          } else {
            setError('This invite is no longer valid. Please contact your administrator.');
          }
        } else {
          // For other errors (>=500), keep the form and show toast error
          toast.error(data.error || 'Failed to create account. Please try again.');
        }
        return;
      }

      // Auto-login after successful account creation
      const loginResult = await signIn('credentials', {
        email: inviteData.email,
        password,
        redirect: false,
      });

      if (loginResult?.error) {
        toast.error('Account created but login failed. Please sign in manually.');
        router.push('/login');
      } else {
        toast.success('Welcome to Kin Home Sales Dashboard!');
        router.push('/');
      }
    } catch (error) {
      toast.error('An error occurred. Please try again.');
    } finally {
      setIsLoading(false);
    }
  };

  // Loading state while validating token
  if (isValidating) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-gray-50 py-12 px-4 sm:px-6 lg:px-8">
        <div className="max-w-md w-full space-y-8">
          <div className="text-center">
            <div className="flex justify-center mb-6">
              <Image
                src="/logo.png"
                alt="KINETIC"
                width={64}
                height={64}
                className="h-16 w-16"
              />
            </div>
            <h2 className="text-4xl font-extrabold text-gray-900 tracking-tight">
              KINETIC
            </h2>
            <p className="mt-2 text-sm text-gray-600">
              Validating invite...
            </p>
          </div>
        </div>
      </div>
    );
  }

  // Error state
  if (error) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-gray-50 py-12 px-4 sm:px-6 lg:px-8">
        <div className="max-w-md w-full space-y-8">
          <div className="text-center">
            <div className="flex justify-center mb-6">
              <Image
                src="/logo.png"
                alt="KINETIC"
                width={64}
                height={64}
                className="h-16 w-16"
              />
            </div>
            <h2 className="text-4xl font-extrabold text-gray-900 tracking-tight">
              KINETIC
            </h2>
          </div>
          
          <Card>
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                <AlertCircle className="h-5 w-5 text-red-500" />
                Invalid Invite
              </CardTitle>
            </CardHeader>
            <CardContent className="space-y-4">
              <Alert variant="destructive">
                <AlertCircle className="h-4 w-4" />
                <AlertDescription>{error}</AlertDescription>
              </Alert>
              
              <div className="text-center">
                <Link href="/login">
                  <Button variant="outline" className="w-full">
                    Go to Sign In
                  </Button>
                </Link>
              </div>
            </CardContent>
          </Card>
        </div>
      </div>
    );
  }

  // Main form
  return (
    <div className="min-h-screen flex items-center justify-center bg-gray-50 py-12 px-4 sm:px-6 lg:px-8">
      <div className="max-w-md w-full space-y-8">
        <div className="text-center">
          <div className="flex justify-center mb-6">
            <Image
              src="/logo.png"
              alt="KINETIC"
              width={64}
              height={64}
              className="h-16 w-16"
            />
          </div>
          <h2 className="text-4xl font-extrabold text-gray-900 tracking-tight">
            KINETIC
          </h2>
          <p className="mt-2 text-sm text-gray-600">
            Complete your account setup
          </p>
        </div>
        
        <Card>
          <CardHeader>
            <CardTitle>Accept Invitation</CardTitle>
            <CardDescription>
              You&apos;ve been invited by {inviteData?.invitedByName || inviteData?.invitedBy} to join Kin Home Sales Dashboard
            </CardDescription>
          </CardHeader>
          <CardContent className="space-y-6">
            {/* Invite Details */}
            <div className="space-y-3">
              <div className="flex items-center justify-between">
                <span className="text-sm font-medium text-gray-700">Email:</span>
                <span className="text-sm text-gray-900">{inviteData?.email}</span>
              </div>
              
              <div className="flex items-center justify-between">
                <span className="text-sm font-medium text-gray-700">Name:</span>
                <span className="text-sm text-gray-900">{inviteData?.name}</span>
              </div>
              
              <div className="flex items-center justify-between">
                <span className="text-sm font-medium text-gray-700">Role:</span>
                <Badge className={getRoleBadgeColor(inviteData?.role || '')}>
                  {getRoleDisplayName(inviteData?.role || '')}
                </Badge>
              </div>
              
              {(inviteData?.office || (inviteData?.offices && inviteData.offices.length > 0)) && (
                <div className="flex items-center justify-between">
                  <span className="text-sm font-medium text-gray-700">Office(s):</span>
                  <div className="flex flex-wrap gap-1">
                    {inviteData.office && (
                      <Badge variant="secondary" className="text-xs">
                        {inviteData.office}
                      </Badge>
                    )}
                    {inviteData.offices?.map((office, index) => (
                      <Badge key={index} variant="secondary" className="text-xs">
                        {office}
                      </Badge>
                    ))}
                  </div>
                </div>
              )}
            </div>

            {/* Role Explanation */}
            <RoleExplanation 
              role={inviteData?.role || ''} 
              offices={inviteData?.offices}
            />

            {/* Password Form */}
            <form onSubmit={handleSubmit} className="space-y-4">
              <div className="space-y-2">
                <Label htmlFor="password">Password</Label>
                <Input
                  id="password"
                  name="password"
                  type="password"
                  value={password}
                  onChange={(e) => setPassword(e.target.value)}
                  required
                  disabled={isLoading}
                  placeholder="Enter your password"
                />
                <PasswordStrengthIndicator 
                  password={password} 
                  showRequirements={true}
                />
              </div>
              
              <div className="space-y-2">
                <Label htmlFor="confirmPassword">Confirm Password</Label>
                <Input
                  id="confirmPassword"
                  name="confirmPassword"
                  type="password"
                  value={confirmPassword}
                  onChange={(e) => setConfirmPassword(e.target.value)}
                  required
                  disabled={isLoading}
                  placeholder="Confirm your password"
                />
                {passwordError && (
                  <p className="text-sm text-red-600">{passwordError}</p>
                )}
              </div>
              
              <Button
                type="submit"
                className="w-full"
                disabled={isLoading || !password || !confirmPassword || !!passwordError || !isPasswordValid(password) || password !== confirmPassword}
              >
                {isLoading ? 'Creating Account...' : 'Create Account'}
              </Button>
            </form>
          </CardContent>
        </Card>
      </div>
    </div>
  );
}
