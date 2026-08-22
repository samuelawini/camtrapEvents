# Contributing to camtrapEvents

Bug reports, documentation corrections and focused code contributions are
welcome. Please open an issue before making a substantial change so that the
scope and intended behaviour can be agreed first.

Create a branch from `main` and keep each pull request focused on one change.
Add or update tests whenever behaviour changes. User-facing changes must also
update the relevant help page, vignette or README section.

Before opening a pull request, run:

```r
roxygen2::roxygenise()
testthat::test_local()
rcmdcheck::rcmdcheck(args = "--no-manual")
```

Pull requests must pass the automated checks on supported R versions and
operating systems. By participating, you agree to follow the repository's
[Code of Conduct](CODE_OF_CONDUCT.md).
