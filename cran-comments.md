## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

[UPDATE BEFORE SUBMISSION: confirm the counts above from the final GitHub
Actions and win-builder runs. If CRAN's spell check flags words such as
"Krishnamoorthy" or "MCMC" in the DESCRIPTION, list them here as correct.]

## Test environments

* local: Windows 11, R 4.2.3 (0 errors | 0 warnings | 0 notes besides "new submission" and local-environment notes)
* GitHub Actions (R-CMD-check, r-lib/actions): ubuntu-latest (R devel, release, oldrel-1),
  macos-latest (R 4.6.1), windows-latest (release)
* win-builder: devel and release [UPDATE BEFORE SUBMISSION]

## Notes for the reviewers

* Sanofi is listed as copyright holder and funder (`cph`, `fnd`) because the
  code was developed by Sanofi employees. The maintainer address is the
  maintainer's personal mailbox and is monitored.
* The package is licensed under Apache-2.0; the NOTICE file is shipped in
  `inst/NOTICE` as required by section 4(d) of the license.
* `start_app_view()` launches an interactive Shiny application. Its example
  is wrapped in `if (interactive())`; all other examples run in well under
  one second on small synthetic data.
* The application code only reads files that the user uploads through the
  interface (up to 30 MB). It writes no files and makes no network requests
  itself (the Bootswatch theme supplied by 'bslib' may load a web font in
  the user's browser). The `shiny.maxRequestSize` option is set while the
  application runs and restored afterwards.
* The DESCRIPTION cites Krishnamoorthy and Mathew (2009), <doi:10.1002/9780470473900>. The DOI resolves (doi.org and Crossref), but the publisher's site (Wiley) answers automated requests with HTTP 403, so the URL check reports "Forbidden".
* There are no reverse dependencies (new package).
