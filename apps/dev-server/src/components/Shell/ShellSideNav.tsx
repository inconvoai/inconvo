"use client";

import {
  AppShell,
  Group,
  ScrollArea,
  Space,
  Stack,
  Text,
  TextInput,
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
import Copy from "~/components/Copy";
import { DEV_AGENT_ID } from "~/lib/constants";

interface ShellSideNavProps {
  onNavClick?: () => void;
}

export function ShellSideNav({ onNavClick }: ShellSideNavProps) {
  const pathname = usePathname();

  return (
    <AppShell.Navbar p="md">
      <div className={classes.section}>
        <Stack gap="xs" px="sm" mb="sm">
          <Text size="xs" c="dimmed" fw={500}>
            Agent ID
          </Text>
          <TextInput
            value={DEV_AGENT_ID}
            readOnly
            size="xs"
            styles={{
              input: {
                fontFamily: "monospace",
                fontSize: "11px",
                cursor: "pointer",
              },
            }}
            rightSection={<Copy value={DEV_AGENT_ID} />}
            onClick={() => navigator.clipboard.writeText(DEV_AGENT_ID)}
          />
        </Stack>
      </div>

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
