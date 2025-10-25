'use client';

import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Progress } from '@/components/ui/progress';
import { CheckCircle, Clock } from 'lucide-react';
import { cn } from '@/lib/utils';

interface PCSLAComplianceGaugeProps {
  compliance: number;
  title?: string;
  subtitle?: string;
}

const getComplianceColor = (compliance: number) => {
  if (compliance >= 90) return 'text-green-600';
  if (compliance >= 75) return 'text-yellow-600';
  return 'text-red-600';
};

const getComplianceLabel = (compliance: number) => {
  if (compliance >= 90) return 'Excellent';
  if (compliance >= 75) return 'Good';
  return 'Needs Improvement';
};

const getComplianceBgColor = (compliance: number) => {
  if (compliance >= 90) return 'bg-green-50 border-green-200';
  if (compliance >= 75) return 'bg-yellow-50 border-yellow-200';
  return 'bg-red-50 border-red-200';
};

export function PCSLAComplianceGauge({ 
  compliance, 
  title = 'SLA Compliance',
  subtitle 
}: PCSLAComplianceGaugeProps) {
  if (compliance === null || compliance === undefined) {
    return (
      <Card className="h-64 mobile:h-72 ipad:h-80">
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Clock className="h-5 w-5 text-gray-400" />
            {title}
          </CardTitle>
        </CardHeader>
        <CardContent className="flex items-center justify-center h-full">
          <div className="text-center text-gray-500">
            <Clock className="h-8 w-8 mx-auto mb-2 text-gray-300" />
            <p>SLA data not available</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  const complianceColor = getComplianceColor(compliance);
  const complianceLabel = getComplianceLabel(compliance);
  const bgColor = getComplianceBgColor(compliance);

  return (
    <Card className={cn("shadow-sm hover:shadow-md transition-shadow", bgColor)}>
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <CheckCircle className="h-5 w-5 text-blue-600" />
          {title}
        </CardTitle>
        {subtitle && (
          <p className="text-sm text-gray-600">{subtitle}</p>
        )}
      </CardHeader>
      <CardContent className="text-center space-y-4">
        {/* Large percentage value */}
        <div className="space-y-2">
          <div className={cn("text-4xl font-bold", complianceColor)}>
            {compliance.toFixed(1)}%
          </div>
          <div className={cn("text-lg font-medium", complianceColor)}>
            {complianceLabel}
          </div>
        </div>
        
        {/* Progress bar */}
        <div className="space-y-2">
          <Progress 
            value={compliance} 
            className="h-3"
            style={{
              backgroundColor: compliance >= 90 ? '#dcfce7' : 
                             compliance >= 75 ? '#fef3c7' : '#fee2e2'
            }}
          />
          <p className="text-sm text-gray-600">
            Projects meeting SLA requirements
          </p>
        </div>
        
        {/* Additional info */}
        <div className="pt-2 border-t border-gray-200">
          <p className="text-xs text-gray-500">
            Target: 90% compliance
          </p>
        </div>
      </CardContent>
    </Card>
  );
}
