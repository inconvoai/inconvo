import "~/app/layout.css";
import { MantineProvider } from "@mantine/core";
import { RepoUiMantineProvider } from "@repo/ui";
import { type Metadata } from "next";
import { theme } from "~/theme";
import { Shell } from "~/components";

export const metadata: Metadata = {
  title: "Inconvo Dev Server",
  description: "Chat with your data locally",
};

export default function RootLayout({
  children,
}: Readonly<{ children: React.ReactNode }>) {
  return (
    <html lang="en" suppressHydrationWarning>
      <body>
        <MantineProvider theme={theme}>
          <RepoUiMantineProvider theme={theme}>
            <Shell>{children}</Shell>
          </RepoUiMantineProvider>
        </MantineProvider>
      </body>
    </html>
  );
}
