import Image from 'next/image';
import Link from 'next/link';

export function Logo() {
  return (
    <Link href="/" className="flex items-center gap-3">
      <Image
        src="/logo.png"
        alt="KINETIC"
        width={32}
        height={32}
        className="h-8 w-8"
        priority
      />
      <span className="hidden sm:block text-xl font-bold text-slate-900 tracking-tight">
        KINETIC
      </span>
    </Link>
  );
}
