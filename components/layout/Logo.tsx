import Image from 'next/image';
import Link from 'next/link';

export function Logo() {
  // Check if custom logo exists, otherwise use placeholder
  const hasCustomLogo = false; // Will be true once you upload logo.svg or logo.png

  if (hasCustomLogo) {
    return (
      <Link href="/" className="flex items-center">
        <Image
          src="/logo.svg" // or /logo.png
          alt="KIN Sales"
          width={140}
          height={32}
          className="h-8 w-auto"
          priority
        />
      </Link>
    );
  }

  // Placeholder logo (current)
  return (
    <Link href="/" className="flex items-center gap-2">
      <div className="h-8 w-8 rounded-lg bg-gradient-to-br from-indigo-500 to-indigo-600 flex items-center justify-center shadow-sm">
        <span className="text-white font-bold text-lg">K</span>
      </div>
      <span className="hidden sm:block text-lg font-bold text-slate-900">
        KIN Sales
      </span>
    </Link>
  );
}
