"use client";

import { AppShell, Burger, Group, Text } from "@mantine/core";
import { useDisclosure } from "@mantine/hooks";
import Link from "next/link";
import { Logo } from "@repo/ui";
import { ShellSideNav } from "./ShellSideNav";
import classes from "./Shell.module.css";

interface ShellProps {
  children: React.ReactNode;
}

export function Shell({ children }: ShellProps) {
  const [opened, { toggle, close }] = useDisclosure();

  return (
    <AppShell
      header={{ height: 60 }}
      navbar={{
        width: 250,
        breakpoint: "sm",
        collapsed: { mobile: !opened },
      }}
      padding="md"
    >
      <AppShell.Header>
        <Group h="100%" px="md" justify="space-between">
          <Burger opened={opened} onClick={toggle} hiddenFrom="sm" size="sm" />
          <Group gap="xs">
            <Logo size={32} />
            <Text
              size="xl"
              fw={900}
              component={Link}
              href="/"
              style={{ textDecoration: "none", color: "inherit" }}
            >
              Inconvo
            </Text>
          </Group>
          <Text size="sm" c="dimmed">
            Dev Server
          </Text>
        </Group>
      </AppShell.Header>
      <ShellSideNav onNavClick={close} />
      <AppShell.Main>
        <div className={classes.main}>{children}</div>
      </AppShell.Main>
    </AppShell>
  );
}
