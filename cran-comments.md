## Test environments
* local Ubuntu 24.04 install, R 4.6.1: 0 errors | 0 warnings | 0 notes
* GitHub Actions CI (.github/workflows/R-CMD-check.yaml), all passing:
  - ubuntu-latest, R 4.7.0 (devel)
  - ubuntu-latest, R 4.6.1 (release)
  - ubuntu-latest, R 4.5.3 (oldrel-1)
  - windows-latest, R 4.6.1 (release)
  - macos-latest, R 4.6.1 (release)

## revdepcheck results

We checked 1 reverse dependency (0 from CRAN, 1 from Bioconductor:
uncoverappLib). It was installed and checked directly (with its own
Bioconductor dependency tree) against this release.

 * We saw 0 new problems
 * We failed to check 0 packages
