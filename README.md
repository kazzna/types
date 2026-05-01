# types

## Publish credentials setup

This repository publishes Maven packages to GitHub Packages.

Create a credentials file at `$HOME/.sbt/1.0/ghpackages.credentials` before running `sbt publish`.

Example content:

```ini
realm=GitHub Package Registry
host=maven.pkg.github.com
user=<GitHub user name>
password=<GitHub token with packages:write>
```

Notes:

- Keep this file out of version control.
- Publish is intended for GitHub Actions only.
- In CI, generate the same file before publish.
- The release-tag workflow publishes the package to GitHub Packages and also creates a GitHub Release.
- Set `GITHUB_PACKAGES_MAVEN_URL` in CI, for example:
  - `GITHUB_PACKAGES_MAVEN_URL=https://maven.pkg.github.com/<owner>/<repo>`
