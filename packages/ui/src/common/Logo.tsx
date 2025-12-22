"use client";

import { Avatar, type AvatarProps } from "@mantine/core";

export interface LogoProps extends AvatarProps {
  /** Path to the logo image. Defaults to "/logo.png" */
  src?: string;
  /** Alt text for the logo. Defaults to "Logo" */
  alt?: string;
}

const AVATAR_SIZES: Record<string, number> = {
  xs: 16,
  sm: 26,
  md: 38,
  lg: 56,
  xl: 84,
};

export function Logo({
  src = "/logo.png",
  alt = "Logo",
  size = "md",
  ...props
}: LogoProps) {
  let sizeValue = 38; // default 'md' size

  if (typeof size === "number") {
    sizeValue = size;
  } else if (typeof size === "string" && size in AVATAR_SIZES) {
    sizeValue = AVATAR_SIZES[size]!;
  }

  return (
    <Avatar {...props} size={size}>
      <img
        src={src}
        alt={alt}
        width={sizeValue}
        height={sizeValue}
        style={{ objectFit: "contain" }}
      />
    </Avatar>
  );
}
