"use client";

import {
  AppShell,
  Group,
  ScrollArea,
  Space,
  Stack,
  Text,
  UnstyledButton,
} from "@mantine/core";
import classes from "./Shell.module.css";
import Link from "next/link";
import {
  IconBraces,
  IconDatabaseEdit,
  IconMessages,
  IconSettings,
} from "@tabler/icons-react";
import { usePathname } from "next/navigation";

interface ShellSideNavProps {
  onNavClick?: () => void;
}

export function ShellSideNav({ onNavClick }: ShellSideNavProps) {
  const pathname = usePathname();

  return (
    <AppShell.Navbar p="md">
      <ScrollArea className={classes.section}>
        <Stack gap="0">
          <Text className={classes.collectionText}>Configure</Text>
          <UnstyledButton
            component={Link}
            href="/context"
            className={classes.link}
            bg={pathname.startsWith("/context") ? "gray.1" : "transparent"}
            prefetch
            onClick={onNavClick}
          >
            <Group>
              <IconBraces size={20} />
              <Text className={classes.linkText}>User Context</Text>
            </Group>
          </UnstyledButton>
          <UnstyledButton
            component={Link}
            href="/schema"
            className={classes.link}
            bg={pathname.startsWith("/schema") ? "gray.1" : "transparent"}
            prefetch
            onClick={onNavClick}
          >
            <Group>
              <IconDatabaseEdit size={20} />
              <Text className={classes.linkText}>Semantic Model</Text>
            </Group>
          </UnstyledButton>
        </Stack>

        <Space h="md" />

        <Stack gap="0">
          <Text className={classes.collectionText}>Explore</Text>
          <UnstyledButton
            component={Link}
            href="/"
            className={classes.link}
            bg={pathname === "/" ? "gray.1" : "transparent"}
            prefetch
            onClick={onNavClick}
          >
            <Group>
              <IconMessages size={20} />
              <Text className={classes.linkText}>Chat</Text>
            </Group>
          </UnstyledButton>
        </Stack>

        <Space h="md" />

        <Stack gap="0">
          <Text className={classes.collectionText}>Settings</Text>
          <UnstyledButton
            component={Link}
            href="/settings"
            className={classes.link}
            bg={pathname.startsWith("/settings") ? "gray.1" : "transparent"}
            prefetch
            onClick={onNavClick}
          >
            <Group>
              <IconSettings size={20} />
              <Text className={classes.linkText}>Configuration</Text>
            </Group>
          </UnstyledButton>
        </Stack>
      </ScrollArea>
    </AppShell.Navbar>
  );
}
