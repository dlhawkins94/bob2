# Bob: SlackBuild manager & installer

## Prerequisites

You need `development/chicken` to build and run bob2. There's a make target to
install all the necessary eggs.

## Installation

~~~~
$ mkdir -p bin/ obj/
$ sudo make install-deps # install all the eggs, if this is the first build
$ make
$ sudo make install
~~~~

`bob` will be in /usr/local/sbin.

If you upgrade Chicken you may need to rebuild everything (and reinstall all
the eggs).

## Usage

Bob downloads SlackBuild scripts from the main repository along with their
sources, runs those scripts, and installs the results. Command line usage is
fairly similar to `slackpkg`:

~~~~
# bob update
# bob upgrade-all # or,
# bob upgrade pkg-foo pkg-bar etc.
# bob install new-pkg new-pkg2
~~~~

Bob keeps the changelog and repo copy in `/var/lib/bob`. Updates only happen
when the changelog differs, or `slackbuilds.json` is missing.

The build scripts are kept in a partial copy of the repo in `/var/cache/bob`.
When bob builds a package, it first downloads the script tarball and unpacks it
in the appropriate repo dir. It then attempts to download the source & md5
checks it. It then runs the build script. If the script succeeds, it parses the
last few lines of the output to get the package path and installs it (or
upgrades with it). If the script fails with an error, bob does too.

When new packages are installed, bob looks for any dependencies that aren't
installed, and builds and installs those as well. This works recursively so the
entire dependency tree of packages gets installed. Currently, upgrades don't
check for new dependencies. ~~This will probably cause severe headaches down the
line.~~

Package removal is not handled by bob, as that's unnecessary (`removepkg` will
suffice).

Packages may be blacklisted: add a line to `/etc/bob/blacklist.txt`. A number of
packages may be broken on `current`, need manually set build options, or have
`*.info` parameters that prevent automatic installation.

## Other commands

~~~~
# bob unneeded-libraries
~~~~

Prints a list of pkgs in `libraries/` that are not depended on by another pkg.
Bob does not track which packages are installed directly and which are 
installed as dependencies. The assumption is that most packages in `libraries`
are installed as deps. There is not an automatic remove command for this list
for safety reasons; the list just helps you identify packages you may want
to remove.

~~~~
# bob purge-sources
~~~~

Removes all source archives from `/var/cache/bob` to free up space.

~~~~
$ bob search pkgname
~~~~

Searches the repo for the given package name. Prints the name and short
description. If the package is installed, `***` appears before the name.

~~~~
$ bob info pkgname
~~~~

Prints package slackbuild information (`pkg.info`, `slack-desc`).

~~~~
$ bob readme pkgname
~~~~

Prints package readme.




