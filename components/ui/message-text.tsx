'use client';

import { useSession } from 'next-auth/react';

interface MessageTextProps {
  text: string;
  className?: string;
}

/**
 * Component to render message text with highlighted @mentions
 * Converts @[Name](user-id) to styled mentions
 */
export function MessageText({ text, className = '' }: MessageTextProps) {
  const { data: session } = useSession();
  // Use email as identifier since that's what we store in messages
  const currentUserEmail = session?.user?.email;

  // Parse text and highlight mentions
  const renderText = () => {
    const parts = [];
    let lastIndex = 0;
    const mentionRegex = /@\[([^\]]+)\]\(([^)]+)\)/g;
    let match;

    while ((match = mentionRegex.exec(text)) !== null) {
      const [fullMatch, name, userId] = match;
      const startIndex = match.index;

      // Add text before mention
      if (startIndex > lastIndex) {
        parts.push(
          <span key={`text-${lastIndex}`}>
            {text.slice(lastIndex, startIndex)}
          </span>
        );
      }

      // Add highlighted mention
      // TODO: Check if this is the current user for special highlighting
      parts.push(
        <span
          key={`mention-${startIndex}`}
          className="inline-flex items-center px-1.5 py-0.5 rounded text-sm font-medium bg-blue-100 text-blue-800 hover:bg-blue-200 transition-colors"
          title={name}
        >
          @{name}
        </span>
      );

      lastIndex = startIndex + fullMatch.length;
    }

    // Add remaining text
    if (lastIndex < text.length) {
      parts.push(
        <span key={`text-${lastIndex}`}>
          {text.slice(lastIndex)}
        </span>
      );
    }

    return parts.length > 0 ? parts : text;
  };

  return (
    <div className={className}>
      {renderText()}
    </div>
  );
}
