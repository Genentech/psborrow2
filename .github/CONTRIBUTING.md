# Contributing to `psborrow2`

Contributions are welcome, and they are greatly appreciated! Every
little bit helps, and credit will always be given.

## Submit an Issue
The best way to get started is to file an issue on GitHub to
discuss what you would like to work on (bug, feature request, documentation). This 
way you do not spend time working on something that will not be prioritized.

## Fork the Repository and Create a Branch
When you have an issue you are prepared to tackle, fork the repository
and create a branch to work on the issue.

Before you get started making changes to address your issue, we recommend 
successfully running `R CMD CHECK` locally to make sure you 
can catch any problems you address that may cause the package to fail
checks.

## Make Changes Locally
Make your changes locally and commit them. Make sure to write good commit
messages, as these will be used to generate the release notes for the
package.

Make sure to:
- Add tests for any new features or bug fixes
- Update the documentation to reflect any changes (see [`DOCUMENTATION.md`](DOCUMENTATION.md))
- Run `R CMD CHECK` or `devtools::check()` locally to make sure you have not introduced any new problems
- Check spelling with `devtools::spell_check()`

Note that the package style and linting are addressed on the entire package before each release.

## Push to GitHub and Open a Pull Request
When you are finished making changes, push your branch to GitHub and
open a pull request. Make sure to include a description of your changes
and reference the issue you are addressing.

## Code of Conduct
Please note that the `psborrow2` project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.