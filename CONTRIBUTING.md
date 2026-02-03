# Contributing to Inconvo

Thanks for taking the time to contribute to Inconvo — we really appreciate it, and we actively encourage it.

Please take a moment to read this guide so you’re familiar with Inconvo’s contribution best practices.

Thanks for helping make Inconvo even better!

## Developing

The development branch is `main`. This is the branch that all pull
requests should be made against. The changes on the `main`
branch are tagged into a release periodically.

### Prerequisites

- [Node.js](https://nodejs.org/en) version 22.17.0 or higher
- [pnpm package manager](https://pnpm.io/installation) version 10.26.0

### Setup

1. Clone the repo into a public GitHub repository or [fork the repo](https://github.com/inconvoai/inconvo/fork).

   ```
   git clone https://github.com/<github_username>/inconvo.git
   ```

2. Navigate to the project folder

   ```
   cd inconvo
   ```

3. Ensure you are on the correct version of Node.js (22.17.0). If you are using nvm, there is an .nvmrc file that will automatically select the correct version of Node.js when you navigate to the repository.

4. Run `corepack enable` to use the correct version of pnpm (`10.26.0`) as specified in the root `package.json` file.

5. Install the required packages using pnpm.

   ```
   pnpm i
   ```

6. Create your `.inconvo.env` file for the dev-server

   ```
   pnpm --dir apps/dev-server setup
   ```

7. Initialize and push the database schema

   ```
   pnpm --dir apps/dev-server run db:push
   ```

8. Build the packages
   
   ```
   pnpm run build
   ```

## Running

You can run the dev server with:

```
pnpm run dev
```

The dev server should run on port `26686`: [http://localhost:26686](http://localhost:26686/)

## Making a pull request

- Be sure to [check the "Allow edits from maintainers" option](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/working-with-forks/allowing-changes-to-a-pull-request-branch-created-from-a-fork) while creating your PR.
- If your PR refers to or fixes an issue, be sure to add `refs #XXX` or `fixes #XXX` to the PR description. Replacing `XXX` with the respective issue number. See more about [Linking a pull request to an issue](https://docs.github.com/en/issues/tracking-your-work-with-issues/linking-a-pull-request-to-an-issue).
- Ensure all tests pass and code is properly formatted before submitting.

## Troubleshooting

### Port already in use

When receiving the following error message:

```
Error: listen EADDRINUSE: address already in use :::26686
```

The process running on port `26686` should be stopped.

1. Get the `PID` of the process running on PORT `26686`
   ```
   lsof -i :26686
   ```
2. Kill the process
   ```
   sudo kill -9 <PID>
   ```

### Database connection issues

If you're having trouble connecting to the database:

1. Verify your `.inconvo.env` file in `apps/dev-server` has the correct database configuration
2. Ensure the database is running and accessible
3. Check that `INCONVO_DATABASE_URL` is properly formatted for your database dialect

## Getting help

If you run into issues or have questions:

- Check existing [GitHub Issues](https://github.com/inconvoai/inconvo/issues)
- Open a new issue with a detailed description of your problem
- Join our community discussions

We appreciate your contributions to making Inconvo better!
