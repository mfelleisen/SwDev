cs4500-f18-fest
===

Package for running a TEST FEST on student submissions.

Abilities:
- `git checkout` students last commit before a deadline
- run student executables (if present) against staff tests
- use staff executable to validate student tests (if any)
- run a TESTFEST of all student executables on all other student tests


Install
---

In this directory:

```
  $ raco pkg install
```


Documentation
---

After installing, run:

```
  $ raco docs cs4500-f18-fest
```

For the source, check the `scribblings` entry in `./info.rkt`


Usage
---

Basically,

1. Make a `#lang cs4500-f18-fest/manifest` file with paths to executables
2. Run `./xfest <FILE.cs4500>` on your manifest
3. Follow the interactive prompts
4. Results get saved to a directory

See the documentation for details.
