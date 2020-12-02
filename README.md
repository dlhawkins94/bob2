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
entire dependency tree of packages gets installed. This works for upgrading as
well; upgrading a package will make sure its deps are upgraded, and that any
new deps are installed.

Package removal is not handled by bob, as that's unnecessary (`removepkg` will
suffice).

## Configuration

### /etc/bob/blacklist.txt

Packages listed here (one per line) are ignored by install/upgrade commands. If
bob tries to install a package which depends on a blacklisted package, that
install command will fail.

### /etc/bob/forced_libs.txt

Packages listed here are members of `libraries/` which are not considered
dependencies. This is only relevant to the `unneeded-libraries` command, as they
won't be considered 'removable' by this command.

### /etc/bob/sbopts.json

This file is used to apply tweaks to the package creation process. It's a nested
JSON object in the form `{"pkgname": {"option": ...}}`. There should always be a
`"DEFAULT"` entry which includes all the possible options. This entry will be
updated with package-specific options to produce the final option set.

Each option is described below:

#### "buildenv"

This is used to construct an environment variable list which precedes the call
to the SlackBuild script. The list is specified as a JSON object, i.e.:

~~~~
"buildenv": {
    "MAKEFLAGS": "-j 2",
    "EXT_SRC_DIR": "/some/where"
} => MAKEFLAGS="-j 2" EXT_SRC_DIR="/some/where"
~~~~

#### "manual-sources"

Used to specify a list of sources which must be downloaded and placed in the
sb folder before building. This is necessary when the automatic source
fetching doesn't work due to sign in requirements or weird browser agent stuff.

#### "ignore-deps"

Used to specify a list of dependencies which are specified in the slackbuilds
repo but which should be ignored. Many slackbuilds have an added requirement
which doesn't actually exist as a package, usually called something like
`"%README%"` or `"ATTENTION"`. I guess they're used to prevent auto-installing
because some special steps are needed to prepare the package. If you can
identify what those steps are and get bob to install the package, you can add
those false requirements to this list to prevent bob from blocking on them in
the future.

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




