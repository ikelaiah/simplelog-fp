# Release Checklist

Use this checklist when preparing a SimpleLog-FP release. Replace `0.9.0` and `v0.9.0` with the target version for later releases.

## Before Release

- Start from `main` with a clean worktree:
  ```bash
  git status --short --branch
  ```
- Confirm all version surfaces match the target release:
  ```bash
  rg "0\\.9\\.0|v0\\.9\\.0|Version Minor" README.md CHANGELOG.md packages/lazarus/simplelog.lpk
  ```
- Build the Lazarus test runner:
  ```bash
  lazbuild tests/TestRunner.lpi
  ```
- Run the full local test suite:
  ```bash
  tests/TestRunner.exe --all --format=plainnotiming
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
  git tag -s v0.9.0 -m "Release v0.9.0"
  ```
- Push the commit and tag:
  ```bash
  git push origin main
  git push origin v0.9.0
  ```
- Create the GitHub release from the `CHANGELOG.md` entry.

## After Release

- Confirm the GitHub release points to the expected tag.
- Confirm the README version badge and Lazarus package metadata match the released version.
- Confirm no tracked release artifacts were generated locally:
  ```bash
  git status --short --branch
  ```
