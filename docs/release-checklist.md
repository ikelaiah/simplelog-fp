# Release Checklist

Use this checklist when preparing a SimpleLog-FP release. Replace `1.0.0` and
`v1.0.0` with the target version for later releases.

## Before Release

- Start from the intended release branch and review the worktree:
  ```bash
  git status --short --branch
  ```
- Confirm all version surfaces match the target release:
  ```bash
  rg "1\\.0\\.0|v1\\.0\\.0|Version Major" README.md CHANGELOG.md packages/lazarus/simplelog_fp.lpk
  ```
- Build the Lazarus test runner:
  ```bash
  lazbuild --build-mode=Debug tests/TestRunner.lpi
  ```
- Run the full local test suite:
  ```bash
  tests/TestRunner.exe --all --format=plainnotiming
  ```
- Build the Lazarus package and verify a consumer against its compiled output:
  ```bash
  lazbuild --build-all packages/lazarus/simplelog_fp.lpk
  fpc -Fupackages/lazarus/lib/x86_64-win64 tests/PackageConsumer.pas
  ```
- Build both examples:
  ```bash
  lazbuild --build-mode=Release examples/SimpleLogExample/SimpleLogExample.lpi
  lazbuild --build-mode=Release examples/ThreadSafeExample/ThreadSafeExample.lpi
  ```
- Check for whitespace problems:
  ```bash
  git diff --check
  ```
- Push the release branch and wait for GitHub Actions CI to pass on Windows and Ubuntu.

## Release

- Ensure `CHANGELOG.md` has a dated entry for the release.
- Commit the release changes.
- Create the signed release tag:
  ```bash
  git tag -s v1.0.0 -m "Release v1.0.0"
  ```
- Push the commit and tag:
  ```bash
  git push origin main
  git push origin v1.0.0
  ```
- Create the GitHub release from the `CHANGELOG.md` entry.

## After Release

- Confirm the GitHub release points to the expected tag.
- Confirm the README version badge and Lazarus package metadata match the released version.
- Confirm no tracked release artifacts were generated locally:
  ```bash
  git status --short --branch
  ```
