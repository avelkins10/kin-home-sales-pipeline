import { useState, useEffect } from 'react';

/**
 * Hook to detect media query matches (e.g., screen size)
 *
 * @param query - CSS media query string (e.g., '(max-width: 640px)')
 * @returns boolean indicating if the media query matches
 *
 * @example
 * const isMobile = useMediaQuery('(max-width: 640px)');
 * const isTablet = useMediaQuery('(min-width: 641px) and (max-width: 1024px)');
 */
export function useMediaQuery(query: string): boolean {
  const [matches, setMatches] = useState(false);
  const [mounted, setMounted] = useState(false);

  useEffect(() => {
    setMounted(true);

    // Only run in browser environment
    if (typeof window === 'undefined') {
      return;
    }

    const mediaQuery = window.matchMedia(query);

    // Set initial value
    setMatches(mediaQuery.matches);

    // Define listener
    const handleChange = (event: MediaQueryListEvent) => {
      setMatches(event.matches);
    };

    // Add listener (modern API)
    if (mediaQuery.addEventListener) {
      mediaQuery.addEventListener('change', handleChange);
      return () => mediaQuery.removeEventListener('change', handleChange);
    } else {
      // Fallback for older browsers
      mediaQuery.addListener(handleChange);
      return () => mediaQuery.removeListener(handleChange);
    }
  }, [query]);

  // Return false on server-side render to prevent hydration mismatch
  return mounted ? matches : false;
}

/**
 * Predefined breakpoint hooks matching Tailwind config
 */
export function useIsMobile() {
  return useMediaQuery('(max-width: 639px)'); // Below 'mobile' breakpoint (640px)
}

export function useIsTablet() {
  return useMediaQuery('(min-width: 640px) and (max-width: 1023px)'); // mobile to ipad-lg
}

export function useIsDesktop() {
  return useMediaQuery('(min-width: 1024px)'); // ipad-lg and above
}

export function useIsPhone() {
  return useMediaQuery('(max-width: 474px)'); // Below 'xs' breakpoint (475px)
}
