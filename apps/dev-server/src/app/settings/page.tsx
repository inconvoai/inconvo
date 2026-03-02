"use client";

import {
  Container,
  Title,
  Text,
  Stack,
  Paper,
  Group,
  Code,
  Alert,
} from "@mantine/core";
import { IconSettings, IconInfoCircle } from "@tabler/icons-react";

export default function SettingsPage() {
  return (
    <Container size="lg" py="md">
      <Stack gap="lg">
        <Group gap="sm">
          <IconSettings size={28} />
          <Title order={2}>Configuration</Title>
        </Group>

        <Text c="dimmed">Manage your dev server configuration.</Text>

        <Alert icon={<IconInfoCircle size={20} />} color="blue" variant="light">
          Your configuration is stored in the <Code>.inconvo.env</Code> file in
          the project root.
        </Alert>

        <Paper withBorder p="lg" radius="md">
          <Stack gap="md">
            <Title order={4}>Reconfigure</Title>
            <Text size="sm" c="dimmed">
              To update your database connection, API keys, or other settings,
              run the setup wizard again from the command line:
            </Text>
            <Code block>pnpm db:init</Code>
          </Stack>
        </Paper>

        <Paper withBorder p="lg" radius="md">
          <Stack gap="md">
            <Title order={4}>Environment Variables</Title>
            <Text size="sm" c="dimmed">
              The following environment variables are used by the dev server:
            </Text>
            <Stack gap="xs">
              <Code>DATABASE_DIALECT</Code>
              <Code>INCONVO_DATABASE_URL</Code>
              <Code>INCONVO_DATABASE_SCHEMA</Code>
              <Code>OPENAI_API_KEY</Code>
            </Stack>
          </Stack>
        </Paper>
      </Stack>
    </Container>
  );
}
