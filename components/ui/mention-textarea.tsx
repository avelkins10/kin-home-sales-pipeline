'use client';

import { useState, useRef, useEffect, KeyboardEvent } from 'react';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import type { MentionedUser } from '@/lib/types/message';
import { getRoleColorClasses, getRoleDisplayName } from '@/lib/utils/roles';

interface MentionTextareaProps {
  value: string;
  onChange: (value: string) => void;
  placeholder?: string;
  maxLength?: number;
  disabled?: boolean;
  className?: string;
}

export function MentionTextarea({
  value,
  onChange,
  placeholder,
  maxLength = 10000,
  disabled = false,
  className = '',
}: MentionTextareaProps) {
  const [showMentions, setShowMentions] = useState(false);
  const [mentionSearch, setMentionSearch] = useState('');
  const [mentionUsers, setMentionUsers] = useState<MentionedUser[]>([]);
  const [selectedIndex, setSelectedIndex] = useState(0);
  const [mentionStartPos, setMentionStartPos] = useState(0);
  const [isSearching, setIsSearching] = useState(false);
  const textareaRef = useRef<HTMLTextAreaElement>(null);
  const dropdownRef = useRef<HTMLDivElement>(null);

  // Search for users when @ is typed
  useEffect(() => {
    if (mentionSearch.length >= 2) {
      setIsSearching(true);
      const searchUsers = async () => {
        try {
          const response = await fetch(
            `${getBaseUrl()}/api/users/search?q=${encodeURIComponent(mentionSearch)}`
          );
          if (response.ok) {
            const data = await response.json();
            setMentionUsers(data.users || []);
          }
        } catch (error) {
          console.error('Failed to search users:', error);
        } finally {
          setIsSearching(false);
        }
      };
      searchUsers();
    } else {
      setMentionUsers([]);
    }
  }, [mentionSearch]);

  const handleTextChange = (e: React.ChangeEvent<HTMLTextAreaElement>) => {
    const newValue = e.target.value;
    onChange(newValue);

    // Check if user typed @ and get the search term
    const cursorPos = e.target.selectionStart;
    const textBeforeCursor = newValue.slice(0, cursorPos);
    const lastAtIndex = textBeforeCursor.lastIndexOf('@');

    if (lastAtIndex !== -1) {
      const textAfterAt = textBeforeCursor.slice(lastAtIndex + 1);
      const hasSpace = textAfterAt.includes(' ') || textAfterAt.includes('\n');

      if (!hasSpace && textAfterAt.length >= 0) {
        setMentionSearch(textAfterAt);
        setMentionStartPos(lastAtIndex);
        setShowMentions(true);
        setSelectedIndex(0);
      } else {
        setShowMentions(false);
      }
    } else {
      setShowMentions(false);
    }
  };

  const insertMention = (user: MentionedUser) => {
    if (!textareaRef.current) return;

    const beforeMention = value.slice(0, mentionStartPos);
    const afterMention = value.slice(textareaRef.current.selectionStart);

    // Insert mention in format: @[Name](user-id)
    const mentionText = `@[${user.name}](${user.id})`;
    const newValue = beforeMention + mentionText + ' ' + afterMention;

    onChange(newValue);
    setShowMentions(false);
    setMentionSearch('');

    // Set cursor position after mention
    const newCursorPos = beforeMention.length + mentionText.length + 1;
    setTimeout(() => {
      if (textareaRef.current) {
        textareaRef.current.focus();
        textareaRef.current.setSelectionRange(newCursorPos, newCursorPos);
      }
    }, 0);
  };

  const handleKeyDown = (e: KeyboardEvent<HTMLTextAreaElement>) => {
    if (!showMentions || mentionUsers.length === 0) return;

    if (e.key === 'ArrowDown') {
      e.preventDefault();
      setSelectedIndex(prev => (prev + 1) % mentionUsers.length);
    } else if (e.key === 'ArrowUp') {
      e.preventDefault();
      setSelectedIndex(prev => (prev - 1 + mentionUsers.length) % mentionUsers.length);
    } else if (e.key === 'Enter' || e.key === 'Tab') {
      if (mentionUsers[selectedIndex]) {
        e.preventDefault();
        insertMention(mentionUsers[selectedIndex]);
      }
    } else if (e.key === 'Escape') {
      setShowMentions(false);
    }
  };

  // Scroll selected item into view
  useEffect(() => {
    if (dropdownRef.current && showMentions) {
      const selectedElement = dropdownRef.current.children[selectedIndex] as HTMLElement;
      if (selectedElement) {
        selectedElement.scrollIntoView({ block: 'nearest' });
      }
    }
  }, [selectedIndex, showMentions]);


  return (
    <div className="relative">
      <textarea
        ref={textareaRef}
        value={value}
        onChange={handleTextChange}
        onKeyDown={handleKeyDown}
        placeholder={placeholder}
        maxLength={maxLength}
        disabled={disabled}
        className={className}
        autoFocus
      />

      {/* Mention dropdown */}
      {showMentions && (
        <div
          ref={dropdownRef}
          className="absolute bottom-full left-0 mb-2 w-full max-w-md bg-white rounded-lg shadow-lg border border-slate-200 max-h-60 overflow-y-auto z-50"
        >
          {isSearching ? (
            <div className="p-3 text-sm text-slate-500 text-center">
              Searching users...
            </div>
          ) : mentionUsers.length > 0 ? (
            mentionUsers.map((user, index) => (
              <button
                key={user.id}
                onClick={() => insertMention(user)}
                className={`w-full px-3 py-2 text-left hover:bg-slate-50 transition-colors flex items-center justify-between ${
                  index === selectedIndex ? 'bg-indigo-50' : ''
                }`}
              >
                <div className="flex-1 min-w-0">
                  <div className="text-sm font-medium text-slate-900 truncate">
                    {user.name}
                  </div>
                  <div className="text-xs text-slate-500 truncate">
                    {user.email}
                    {user.office && ` • ${user.office}`}
                  </div>
                </div>
                <span className={`ml-2 px-2 py-0.5 text-xs font-medium rounded ${getRoleColorClasses(user.role)}`}>
                  {getRoleDisplayName(user.role)}
                </span>
              </button>
            ))
          ) : mentionSearch.length >= 2 ? (
            <div className="p-3 text-sm text-slate-500 text-center">
              No users found
            </div>
          ) : (
            <div className="p-3 text-sm text-slate-500 text-center">
              Type at least 2 characters to search
            </div>
          )}
        </div>
      )}
    </div>
  );
}
