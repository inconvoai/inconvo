"use client";

import {
  MantineProvider,
  type MantineProviderProps,
} from "@mantine/core";

export function RepoUiMantineProvider(props: MantineProviderProps) {
  return <MantineProvider {...props} />;
}
