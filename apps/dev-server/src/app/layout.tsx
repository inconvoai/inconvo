import "~/app/layout.css";
import { ColorSchemeScript, MantineProvider } from "@mantine/core";
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
      <head>
        <ColorSchemeScript />
      </head>
      <body>
        <MantineProvider theme={theme}>
          <Shell>{children}</Shell>
        </MantineProvider>
      </body>
    </html>
  );
}
