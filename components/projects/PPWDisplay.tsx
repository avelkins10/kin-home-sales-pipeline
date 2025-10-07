'use client';

import { ArrowDown, ArrowUp, Minus } from 'lucide-react';

interface PPWDisplayProps {
  soldPPW: number | null;
  commissionablePPW: number | null;
  expanded?: boolean;
}

export function PPWDisplay({ soldPPW, commissionablePPW, expanded = false }: PPWDisplayProps) {
  // Early return if both values are null
  if (soldPPW === null && commissionablePPW === null) {
    return <span className="text-sm text-gray-400">N/A</span>;
  }

  // Calculate delta
  const delta = (soldPPW && commissionablePPW) ? commissionablePPW - soldPPW : 0;
  const deltaColor = delta > 0 ? 'text-green-500' : delta < 0 ? 'text-red-500' : 'text-gray-500';
  const DeltaIcon = delta > 0 ? ArrowUp : delta < 0 ? ArrowDown : Minus;

  if (expanded) {
    // Expanded vertical layout
    return (
      <div className="space-y-2">
        {/* Sold PPW row */}
        <div className="flex items-center justify-between">
          <span className="text-sm text-gray-500">Sold PPW:</span>
          <span className="text-sm font-bold text-gray-900">
            ${soldPPW?.toFixed(2) || 'N/A'}
          </span>
        </div>
        
        {/* Commissionable PPW row */}
        <div className="flex items-center justify-between">
          <span className="text-sm text-gray-500">Your PPW:</span>
          <span className="text-sm font-bold text-gray-900">
            ${commissionablePPW?.toFixed(2) || 'N/A'}
          </span>
        </div>
        
        {/* Delta row (if not zero) */}
        {delta !== 0 && (
          <div className="flex items-center justify-between">
            <span className="text-sm text-gray-500">Delta:</span>
            <div className={`flex items-center gap-1 text-sm font-bold ${deltaColor}`}>
              <DeltaIcon className="h-3 w-3" />
              ${Math.abs(delta).toFixed(2)}
            </div>
          </div>
        )}
      </div>
    );
  }

  // Compact inline layout (default)
  return (
    <div className="flex flex-col items-end gap-0.5">
      {/* Sold PPW line */}
      <div className="text-sm font-bold text-gray-900">
        Sold: ${soldPPW?.toFixed(2) || 'N/A'}
      </div>
      
      {/* Commissionable PPW line with delta */}
      <div className="text-sm font-bold text-gray-900">
        Yours: ${commissionablePPW?.toFixed(2) || 'N/A'}
        {delta !== 0 && (
          <span className={`ml-1 ${deltaColor}`}>
            <DeltaIcon className="inline h-3 w-3" />
            {delta < 0 ? '-' : '+'}${Math.abs(delta).toFixed(2)}
          </span>
        )}
      </div>
    </div>
  );
}
