
:tanat

"29.1"

#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("straight" ("2023-09-04 09:22:44" ("emacs") (:type git :host github :repo "radian-software/straight.el" :files ("straight*.el") :branch "master" :package "straight" :local-repo "straight.el")) "org-elpa" ("2023-09-04 09:22:44" nil (:local-repo nil :package "org-elpa" :type git)) "melpa" ("2023-09-04 09:22:44" nil (:type git :host github :repo "melpa/melpa" :build nil :package "melpa" :local-repo "melpa")) "gnu-elpa-mirror" ("2023-09-04 09:22:44" nil (:type git :host github :repo "emacs-straight/gnu-elpa-mirror" :build nil :package "gnu-elpa-mirror" :local-repo "gnu-elpa-mirror")) "nongnu-elpa" ("2023-09-04 09:22:44" nil (:type git :repo "https://git.savannah.gnu.org/git/emacs/nongnu.git" :depth (full single-branch) :local-repo "nongnu-elpa" :build nil :package "nongnu-elpa")) "el-get" ("2023-09-04 09:22:44" nil (:type git :host github :repo "dimitri/el-get" :build nil :files ("*.el" ("recipes" "recipes/el-get.rcp") "methods" "el-get-pkg.el") :flavor melpa :package "el-get" :local-repo "el-get")) "emacsmirror-mirror" ("2023-09-04 09:22:44" nil (:type git :host github :repo "emacs-straight/emacsmirror-mirror" :build nil :package "emacsmirror-mirror" :local-repo "emacsmirror-mirror")) "use-package" ("2023-09-04 09:22:45" ("emacs" "bind-key") (:type git :flavor melpa :files (:defaults (:exclude "bind-key.el" "bind-chord.el" "use-package-chords.el" "use-package-ensure-system-package.el") "use-package-pkg.el") :host github :repo "jwiegley/use-package" :package "use-package" :local-repo "use-package")) "bind-key" ("2023-09-04 09:22:45" ("emacs") (:flavor melpa :files ("bind-key.el" "bind-key-pkg.el") :package "bind-key" :local-repo "use-package" :type git :repo "jwiegley/use-package" :host github)) "org" ("2023-09-04 09:22:45" ("emacs") (:type git :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git" :local-repo "org" :depth full :pre-build (straight-recipes-org-elpa--build) :build (:not autoloads) :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*")) :package "org")) "org-roam" ("2023-09-04 09:22:45" ("emacs" "dash" "org" "emacsql" "magit-section") (:type git :flavor melpa :files (:defaults "extensions/*" "org-roam-pkg.el") :host github :repo "org-roam/org-roam" :package "org-roam" :local-repo "org-roam")) "dash" ("2023-09-04 09:22:45" ("emacs") (:type git :flavor melpa :files ("dash.el" "dash.texi" "dash-pkg.el") :host github :repo "magnars/dash.el" :package "dash" :local-repo "dash.el")) "emacsql" ("2023-09-04 09:22:45" ("emacs") (:type git :flavor melpa :files (:defaults "sqlite" "emacsql-pkg.el") :host github :repo "magit/emacsql" :package "emacsql" :local-repo "emacsql")) "magit-section" ("2023-09-04 09:22:45" ("emacs" "compat" "dash") (:type git :flavor melpa :files ("lisp/magit-section.el" "lisp/magit-section-pkg.el" "docs/magit-section.texi" "Documentation/magit-section.texi" "magit-section-pkg.el") :host github :repo "magit/magit" :package "magit-section" :local-repo "magit")) "compat" ("2023-09-04 09:22:45" ("emacs" "seq") (:type git :host github :repo "emacs-straight/compat" :files ("*" (:exclude ".git")) :package "compat" :local-repo "compat")) "deft" ("2023-09-04 09:22:45" nil (:type git :flavor melpa :host github :repo "jrblevin/deft" :package "deft" :local-repo "deft")) "vertico" ("2023-09-04 09:22:46" ("emacs" "compat") (:files (:defaults "extensions/*") :flavor melpa :repo "minad/vertico" :host github :package "vertico" :type git :local-repo "vertico")) "orderless" ("2023-09-04 09:22:46" ("emacs") (:type git :flavor melpa :host github :repo "oantolin/orderless" :package "orderless" :local-repo "orderless")) "embark" ("2023-09-04 09:22:46" ("emacs" "compat") (:type git :flavor melpa :files ("embark.el" "embark-org.el" "embark.texi" "embark-pkg.el") :host github :repo "oantolin/embark" :package "embark" :local-repo "embark")) "embark-consult" ("2023-09-04 09:22:46" ("emacs" "embark" "consult") (:flavor melpa :files ("embark-consult.el" "embark-consult-pkg.el") :package "embark-consult" :local-repo "embark" :type git :repo "oantolin/embark" :host github)) "consult" ("2023-09-04 09:22:46" ("emacs" "compat") (:type git :flavor melpa :host github :repo "minad/consult" :package "consult" :local-repo "consult")) "marginalia" ("2023-09-04 09:22:46" ("emacs" "compat") (:type git :flavor melpa :host github :repo "minad/marginalia" :package "marginalia" :local-repo "marginalia")) "corfu" ("2023-09-04 09:22:46" ("emacs" "compat") (:type git :flavor melpa :files (:defaults "extensions/corfu-*.el" "corfu-pkg.el") :host github :repo "minad/corfu" :package "corfu" :local-repo "corfu")) "magit" ("2023-09-04 09:22:46" ("emacs" "compat" "dash" "git-commit" "magit-section" "seq" "transient" "with-editor") (:flavor melpa :files ("lisp/magit*.el" "lisp/git-rebase.el" "docs/magit.texi" "docs/AUTHORS.md" "LICENSE" "Documentation/magit.texi" "Documentation/AUTHORS.md" (:exclude "lisp/magit-libgit.el" "lisp/magit-libgit-pkg.el" "lisp/magit-section.el" "lisp/magit-section-pkg.el") "magit-pkg.el") :package "magit" :local-repo "magit" :type git :repo "magit/magit" :host github)) "git-commit" ("2023-09-04 09:22:46" ("emacs" "compat" "transient" "with-editor") (:flavor melpa :files ("lisp/git-commit.el" "lisp/git-commit-pkg.el" "git-commit-pkg.el") :package "git-commit" :local-repo "magit" :type git :repo "magit/magit" :host github)) "transient" ("2023-09-04 09:22:46" ("emacs" "compat") (:type git :flavor melpa :host github :repo "magit/transient" :package "transient" :local-repo "transient")) "with-editor" ("2023-09-04 09:22:46" ("emacs" "compat") (:type git :flavor melpa :host github :repo "magit/with-editor" :package "with-editor" :local-repo "with-editor")) "vterm" ("2023-09-04 09:22:46" ("emacs") (:type git :flavor melpa :files ("CMakeLists.txt" "elisp.c" "elisp.h" "emacs-module.h" "etc" "utf8.c" "utf8.h" "vterm.el" "vterm-module.c" "vterm-module.h" "vterm-pkg.el") :host github :repo "akermu/emacs-libvterm" :package "vterm" :local-repo "emacs-libvterm")) "exec-path-from-shell" ("2023-09-04 09:22:46" ("emacs" "cl-lib") (:type git :flavor melpa :host github :repo "purcell/exec-path-from-shell" :package "exec-path-from-shell" :local-repo "exec-path-from-shell")) "osx-lib" ("2023-09-04 09:22:46" ("emacs") (:type git :flavor melpa :host github :repo "raghavgautam/osx-lib" :package "osx-lib" :local-repo "osx-lib")) "projectile" ("2023-09-04 09:22:46" ("emacs") (:type git :flavor melpa :host github :repo "bbatsov/projectile" :package "projectile" :local-repo "projectile")) "racket-mode" ("2023-09-04 09:22:46" ("emacs") (:type git :flavor melpa :files (:defaults "*.rkt" ("racket" "racket/*") (:exclude "racket/example/*" "racket/test/*") "racket-mode-pkg.el") :host github :repo "greghendershott/racket-mode" :package "racket-mode" :local-repo "racket-mode")) "sly" ("2023-09-04 09:22:46" ("emacs") (:type git :flavor melpa :files (:defaults "lib" "slynk" "contrib" "doc/images" (:exclude "sly-autoloads.el") "sly-pkg.el") :host github :repo "joaotavora/sly" :package "sly" :local-repo "sly")) "auto-complete" ("2023-09-04 09:22:46" ("popup" "cl-lib") (:type git :flavor melpa :files ("*.el" "dict" "auto-complete-pkg.el") :host github :repo "auto-complete/auto-complete" :package "auto-complete" :local-repo "auto-complete")) "popup" ("2023-09-04 09:22:46" ("emacs") (:type git :flavor melpa :host github :repo "auto-complete/popup-el" :package "popup" :local-repo "popup-el")) "ac-sly" ("2023-09-04 09:22:46" ("sly" "auto-complete" "cl-lib") (:type git :flavor melpa :host github :repo "qoocku/ac-sly" :package "ac-sly" :local-repo "ac-sly")) "smartparens" ("2023-09-04 09:22:46" ("dash" "cl-lib") (:type git :flavor melpa :host github :repo "Fuco1/smartparens" :package "smartparens" :local-repo "smartparens")) "rainbow-delimiters" ("2023-09-04 09:22:46" nil (:type git :flavor melpa :host github :repo "Fanael/rainbow-delimiters" :package "rainbow-delimiters" :local-repo "rainbow-delimiters")) "w3m" ("2023-09-04 09:22:46" nil (:type git :flavor melpa :files (:defaults "icons" (:exclude "octet.el" "mew-w3m.el" "w3m-xmas.el" "doc/*.texi") "w3m-pkg.el") :host github :repo "emacs-w3m/emacs-w3m" :package "w3m" :local-repo "emacs-w3m")) "markdown-mode" ("2023-09-04 09:22:47" ("emacs") (:type git :flavor melpa :host github :repo "jrblevin/markdown-mode" :package "markdown-mode" :local-repo "markdown-mode")) "tree-sitter" ("2023-09-04 09:22:47" ("emacs" "tsc") (:type git :flavor melpa :files (:defaults (:exclude "lisp/tree-sitter-tests.el") "tree-sitter-pkg.el") :branch "release" :host github :repo "emacs-tree-sitter/elisp-tree-sitter" :package "tree-sitter" :local-repo "elisp-tree-sitter")) "tsc" ("2023-09-04 09:22:47" ("emacs") (:flavor melpa :files ("core/*.el" "core/Cargo.toml" "core/Cargo.lock" "core/src" "tsc-pkg.el") :package "tsc" :local-repo "elisp-tree-sitter" :type git :repo "emacs-tree-sitter/elisp-tree-sitter" :host github :branch "release")) "tree-sitter-langs" ("2023-09-04 09:22:47" ("emacs" "tree-sitter") (:type git :flavor melpa :files (:defaults "queries" "tree-sitter-langs-pkg.el") :branch "release" :host github :repo "emacs-tree-sitter/tree-sitter-langs" :package "tree-sitter-langs" :local-repo "tree-sitter-langs")) "aggressive-indent" ("2023-09-04 09:22:47" ("emacs") (:type git :flavor melpa :host github :repo "Malabarba/aggressive-indent-mode" :package "aggressive-indent" :local-repo "aggressive-indent-mode")) "undo-tree" ("2023-09-04 09:22:47" ("queue" "emacs") (:type git :host github :repo "emacs-straight/undo-tree" :files ("*" (:exclude ".git")) :package "undo-tree" :local-repo "undo-tree")) "queue" ("2023-09-04 09:22:47" ("cl-lib") (:type git :host github :repo "emacs-straight/queue" :files ("*" (:exclude ".git")) :package "queue" :local-repo "queue")) "which-key" ("2023-09-04 09:22:47" ("emacs") (:type git :flavor melpa :host github :repo "justbur/emacs-which-key" :package "which-key" :local-repo "emacs-which-key")) "avy" ("2023-09-04 09:22:47" ("emacs" "cl-lib") (:type git :flavor melpa :host github :repo "abo-abo/avy" :package "avy" :local-repo "avy")) "crux" ("2023-09-04 09:22:47" ("seq") (:type git :flavor melpa :host github :repo "bbatsov/crux" :package "crux" :local-repo "crux")) "expand-region" ("2023-09-04 09:22:47" nil (:type git :flavor melpa :host github :repo "magnars/expand-region.el" :package "expand-region" :local-repo "expand-region.el"))))

#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("straight" ((straight-x straight-autoloads straight straight-ert-print-hack) (autoload 'straight-remove-unused-repos "straight" "Remove unused repositories from the repos and build directories.
A repo is considered \"unused\" if it was not explicitly requested via
`straight-use-package' during the current Emacs session.
If FORCE is non-nil do not prompt before deleting repos.

(fn &optional FORCE)" t) (autoload 'straight-get-recipe "straight" "Interactively select a recipe from one of the recipe repositories.
All recipe repositories in `straight-recipe-repositories' will
first be cloned. After the recipe is selected, it will be copied
to the kill ring. With a prefix argument, first prompt for a
recipe repository to search. Only that repository will be
cloned.

From Lisp code, SOURCES should be a subset of the symbols in
`straight-recipe-repositories'. Only those recipe repositories
are cloned and searched. If it is nil or omitted, then the value
of `straight-recipe-repositories' is used. If SOURCES is the
symbol `interactive', then the user is prompted to select a
recipe repository, and a list containing that recipe repository
is used for the value of SOURCES. ACTION may be `copy' (copy
recipe to the kill ring), `insert' (insert at point), or nil (no
action, just return it).

Optional arg FILTER must be a unary function.
It takes a package name as its sole argument.
If it returns nil the candidate is excluded.

(fn &optional SOURCES ACTION FILTER)" t) (autoload 'straight-visit-package-website "straight" "Visit the package RECIPE's website.

(fn RECIPE)" t) (autoload 'straight-visit-package "straight" "Open PACKAGE's local repository directory.
When BUILD is non-nil visit PACKAGE's build directory.

(fn PACKAGE &optional BUILD)" t) (autoload 'straight-use-package "straight" "Register, clone, build, and activate a package and its dependencies.
This is the main entry point to the functionality of straight.el.

MELPA-STYLE-RECIPE is either a symbol naming a package, or a list
whose car is a symbol naming a package and whose cdr is a
property list containing e.g. `:type', `:local-repo', `:files',
and VC backend specific keywords.

First, the package recipe is registered with straight.el. If
NO-CLONE is a function, then it is called with two arguments: the
package name as a string, and a boolean value indicating whether
the local repository for the package is available. In that case,
the return value of the function is used as the value of NO-CLONE
instead. In any case, if NO-CLONE is non-nil, then processing
stops here.

Otherwise, the repository is cloned, if it is missing. If
NO-BUILD is a function, then it is called with one argument: the
package name as a string. In that case, the return value of the
function is used as the value of NO-BUILD instead. In any case,
if NO-BUILD is non-nil, then processing halts here. Otherwise,
the package is built and activated. Note that if the package
recipe has a nil `:build' entry, then NO-BUILD is ignored
and processing always stops before building and activation
occurs.

CAUSE is a string explaining the reason why
`straight-use-package' has been called. It is for internal use
only, and is used to construct progress messages. INTERACTIVE is
non-nil if the function has been called interactively. It is for
internal use only, and is used to determine whether to show a
hint about how to install the package permanently.

Return non-nil when package is initially installed, nil otherwise.

(fn MELPA-STYLE-RECIPE &optional NO-CLONE NO-BUILD CAUSE INTERACTIVE)" t) (autoload 'straight-register-package "straight" "Register a package without cloning, building, or activating it.
This function is equivalent to calling `straight-use-package'
with a non-nil argument for NO-CLONE. It is provided for
convenience. MELPA-STYLE-RECIPE is as for
`straight-use-package'.

(fn MELPA-STYLE-RECIPE)") (autoload 'straight-use-package-no-build "straight" "Register and clone a package without building it.
This function is equivalent to calling `straight-use-package'
with nil for NO-CLONE but a non-nil argument for NO-BUILD. It is
provided for convenience. MELPA-STYLE-RECIPE is as for
`straight-use-package'.

(fn MELPA-STYLE-RECIPE)") (autoload 'straight-use-package-lazy "straight" "Register, build, and activate a package if it is already cloned.
This function is equivalent to calling `straight-use-package'
with symbol `lazy' for NO-CLONE. It is provided for convenience.
MELPA-STYLE-RECIPE is as for `straight-use-package'.

(fn MELPA-STYLE-RECIPE)") (autoload 'straight-use-recipes "straight" "Register a recipe repository using MELPA-STYLE-RECIPE.
This registers the recipe and builds it if it is already cloned.
Note that you probably want the recipe for a recipe repository to
include a nil `:build' property, to unconditionally
inhibit the build phase.

This function also adds the recipe repository to
`straight-recipe-repositories', at the end of the list.

(fn MELPA-STYLE-RECIPE)") (autoload 'straight-override-recipe "straight" "Register MELPA-STYLE-RECIPE as a recipe override.
This puts it in `straight-recipe-overrides', depending on the
value of `straight-current-profile'.

(fn MELPA-STYLE-RECIPE)") (autoload 'straight-check-package "straight" "Rebuild a PACKAGE if it has been modified.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. See also `straight-rebuild-package' and
`straight-check-all'.

(fn PACKAGE)" t) (autoload 'straight-check-all "straight" "Rebuild any packages that have been modified.
See also `straight-rebuild-all' and `straight-check-package'.
This function should not be called during init." t) (autoload 'straight-rebuild-package "straight" "Rebuild a PACKAGE.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument RECURSIVE, rebuild
all dependencies as well. See also `straight-check-package' and
`straight-rebuild-all'.

(fn PACKAGE &optional RECURSIVE)" t) (autoload 'straight-rebuild-all "straight" "Rebuild all packages.
See also `straight-check-all' and `straight-rebuild-package'." t) (autoload 'straight-prune-build-cache "straight" "Prune the build cache.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build mtime information and any cached
autoloads discarded.") (autoload 'straight-prune-build-directory "straight" "Prune the build directory.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build directories deleted.") (autoload 'straight-prune-build "straight" "Prune the build cache and build directory.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build mtime information discarded and
their build directories deleted." t) (autoload 'straight-normalize-package "straight" "Normalize a PACKAGE's local repository to its recipe's configuration.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'.

(fn PACKAGE)" t) (autoload 'straight-normalize-all "straight" "Normalize all packages. See `straight-normalize-package'.
Return a list of recipes for packages that were not successfully
normalized. If multiple packages come from the same local
repository, only one is normalized.

PREDICATE, if provided, filters the packages that are normalized.
It is called with the package name as a string, and should return
non-nil if the package should actually be normalized.

(fn &optional PREDICATE)" t) (autoload 'straight-fetch-package "straight" "Try to fetch a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
fetch not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t) (autoload 'straight-fetch-package-and-deps "straight" "Try to fetch a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are fetched
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
fetch not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t) (autoload 'straight-fetch-all "straight" "Try to fetch all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, fetch not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
fetched. If multiple packages come from the same local
repository, only one is fetched.

PREDICATE, if provided, filters the packages that are fetched. It
is called with the package name as a string, and should return
non-nil if the package should actually be fetched.

(fn &optional FROM-UPSTREAM PREDICATE)" t) (autoload 'straight-merge-package "straight" "Try to merge a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
merge not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t) (autoload 'straight-merge-package-and-deps "straight" "Try to merge a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are merged
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
merge not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t) (autoload 'straight-merge-all "straight" "Try to merge all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, merge not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
merged. If multiple packages come from the same local
repository, only one is merged.

PREDICATE, if provided, filters the packages that are merged. It
is called with the package name as a string, and should return
non-nil if the package should actually be merged.

(fn &optional FROM-UPSTREAM PREDICATE)" t) (autoload 'straight-pull-package "straight" "Try to pull a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM, pull
not just from primary remote but also from upstream (for forked
packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t) (autoload 'straight-pull-package-and-deps "straight" "Try to pull a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are pulled
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
pull not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t) (autoload 'straight-pull-all "straight" "Try to pull all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, pull not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
pulled. If multiple packages come from the same local repository,
only one is pulled.

PREDICATE, if provided, filters the packages that are pulled. It
is called with the package name as a string, and should return
non-nil if the package should actually be pulled.

(fn &optional FROM-UPSTREAM PREDICATE)" t) (autoload 'straight-push-package "straight" "Push a PACKAGE to its primary remote, if necessary.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'.

(fn PACKAGE)" t) (autoload 'straight-push-all "straight" "Try to push all packages to their primary remotes.

Return a list of recipes for packages that were not successfully
pushed. If multiple packages come from the same local repository,
only one is pushed.

PREDICATE, if provided, filters the packages that are normalized.
It is called with the package name as a string, and should return
non-nil if the package should actually be normalized.

(fn &optional PREDICATE)" t) (autoload 'straight-freeze-versions "straight" "Write version lockfiles for currently activated packages.
This implies first pushing all packages that have unpushed local
changes. If the package management system has been used since the
last time the init-file was reloaded, offer to fix the situation
by reloading the init-file again. If FORCE is
non-nil (interactively, if a prefix argument is provided), skip
all checks and write the lockfile anyway.

Currently, writing version lockfiles requires cloning all lazily
installed packages. Hopefully, this inconvenient requirement will
be removed in the future.

Multiple lockfiles may be written (one for each profile),
according to the value of `straight-profiles'.

(fn &optional FORCE)" t) (autoload 'straight-thaw-versions "straight" "Read version lockfiles and restore package versions to those listed." t) (autoload 'straight-bug-report "straight" "Test straight.el in a clean environment.
ARGS may be any of the following keywords and their respective values:
  - :pre-bootstrap (Form)...
      Forms evaluated before bootstrapping straight.el
      e.g. (setq straight-repository-branch \"develop\")
      Note this example is already in the default bootstrapping code.

  - :post-bootstrap (Form)...
      Forms evaluated in the testing environment after boostrapping.
      e.g. (straight-use-package \\='(example :type git :host github))

  - :interactive Boolean
      If nil, the subprocess will immediately exit after the test.
      Output will be printed to `straight-bug-report--process-buffer'
      Otherwise, the subprocess will be interactive.

  - :preserve Boolean
      If non-nil, the test directory is left in the directory stored in the
      variable `temporary-file-directory'. Otherwise, it is
      immediately removed after the test is run.

  - :executable String
      Indicate the Emacs executable to launch.
      Defaults to the path of the current Emacs executable.

  - :raw Boolean
      If non-nil, the raw process output is sent to
      `straight-bug-report--process-buffer'. Otherwise, it is
      formatted as markdown for submitting as an issue.

  - :user-dir String
      If non-nil, the test is run with `user-emacs-directory' set to STRING.
      Otherwise, a temporary directory is created and used.
      Unless absolute, paths are expanded relative to the variable
      `temporary-file-directory'.

ARGS are accessible within the :pre/:post-bootsrap phases via the
locally bound plist, straight-bug-report-args.

(fn &rest ARGS)" nil t) (function-put 'straight-bug-report 'lisp-indent-function 0) (autoload 'straight-dependencies "straight" "Return a list of PACKAGE's dependencies.

(fn &optional PACKAGE)" t) (autoload 'straight-dependents "straight" "Return a list of PACKAGE's dependents.

(fn &optional PACKAGE)" t) (register-definition-prefixes "straight" '("straight-")) (register-definition-prefixes "straight-ert-print-hack" '("+without-print-limits")) (defvar straight-x-pinned-packages nil "List of pinned packages.") (register-definition-prefixes "straight-x" '("straight-x-")) (provide 'straight-autoloads)) "bind-key" ((bind-key-autoloads bind-key) (autoload 'bind-key "bind-key" "Bind KEY-NAME to COMMAND in KEYMAP (`global-map' if not passed).

KEY-NAME may be a vector, in which case it is passed straight to
`define-key'.  Or it may be a string to be interpreted as
spelled-out keystrokes, e.g., \"C-c C-z\".  See the documentation
of `edmacro-mode' for details.

COMMAND must be an interactive function, lambda form, or a cons
`(STRING . DEFN)'.

KEYMAP, if present, should be a keymap variable or symbol.
For example:

  (bind-key \"M-h\" #\\='some-interactive-function my-mode-map)

  (bind-key \"M-h\" #\\='some-interactive-function \\='my-mode-map)

If PREDICATE is non-nil, it is a form evaluated to determine when
a key should be bound. It must return non-nil in such cases.
Emacs can evaluate this form at any time that it does redisplay
or operates on menu data structures, so you should write it so it
can safely be called at any time.

(fn KEY-NAME COMMAND &optional KEYMAP PREDICATE)" nil t) (autoload 'unbind-key "bind-key" "Unbind the given KEY-NAME, within the KEYMAP (if specified).
See `bind-key' for more details.

(fn KEY-NAME &optional KEYMAP)" nil t) (autoload 'bind-key* "bind-key" "Similar to `bind-key', but overrides any mode-specific bindings.

(fn KEY-NAME COMMAND &optional PREDICATE)" nil t) (autoload 'bind-keys "bind-key" "Bind multiple keys at once.

Accepts keyword arguments:
:map MAP               - a keymap into which the keybindings should be
                         added
:prefix KEY            - prefix key for these bindings
:prefix-map MAP        - name of the prefix map that should be created
                         for these bindings
:prefix-docstring STR  - docstring for the prefix-map variable
:menu-name NAME        - optional menu string for prefix map
:repeat-docstring STR  - docstring for the repeat-map variable
:repeat-map MAP        - name of the repeat map that should be created
                         for these bindings. If specified, the
                         `repeat-map' property of each command bound
                         (within the scope of the `:repeat-map' keyword)
                         is set to this map.
:exit BINDINGS         - Within the scope of `:repeat-map' will bind the
                         key in the repeat map, but will not set the
                         `repeat-map' property of the bound command.
:continue BINDINGS     - Within the scope of `:repeat-map' forces the
                         same behaviour as if no special keyword had
                         been used (that is, the command is bound, and
                         it's `repeat-map' property set)
:filter FORM           - optional form to determine when bindings apply

The rest of the arguments are conses of keybinding string and a
function symbol (unquoted).

(fn &rest ARGS)" nil t) (autoload 'bind-keys* "bind-key" "Bind multiple keys at once, in `override-global-map'.
Accepts the same keyword arguments as `bind-keys' (which see).

This binds keys in such a way that bindings are not overridden by
other modes.  See `override-global-mode'.

(fn &rest ARGS)" nil t) (autoload 'describe-personal-keybindings "bind-key" "Display all the personal keybindings defined by `bind-key'." t) (register-definition-prefixes "bind-key" '("bind-key" "compare-keybindings" "get-binding-description" "override-global-m" "personal-keybindings")) (provide 'bind-key-autoloads)) "use-package" ((use-package-diminish use-package use-package-lint use-package-delight use-package-jump use-package-autoloads use-package-core use-package-ensure use-package-bind-key) (autoload 'use-package-autoload-keymap "use-package-bind-key" "Load PACKAGE and bind key sequence invoking this function to KEYMAP-SYMBOL.
Then simulate pressing the same key sequence a again, so that the
next key pressed is routed to the newly loaded keymap.

This function supports use-package's :bind-keymap keyword.  It
works by binding the given key sequence to an invocation of this
function for a particular keymap.  The keymap is expected to be
defined by the package.  In this way, loading the package is
deferred until the prefix key sequence is pressed.

(fn KEYMAP-SYMBOL PACKAGE OVERRIDE)") (autoload 'use-package-normalize-binder "use-package-bind-key" "

(fn NAME KEYWORD ARGS)") (defalias 'use-package-normalize/:bind 'use-package-normalize-binder) (defalias 'use-package-normalize/:bind* 'use-package-normalize-binder) (defalias 'use-package-autoloads/:bind 'use-package-autoloads-mode) (defalias 'use-package-autoloads/:bind* 'use-package-autoloads-mode) (autoload 'use-package-handler/:bind "use-package-bind-key" "

(fn NAME KEYWORD ARGS REST STATE &optional BIND-MACRO)") (defalias 'use-package-normalize/:bind-keymap 'use-package-normalize-binder) (defalias 'use-package-normalize/:bind-keymap* 'use-package-normalize-binder) (autoload 'use-package-handler/:bind-keymap "use-package-bind-key" "

(fn NAME KEYWORD ARGS REST STATE &optional OVERRIDE)") (autoload 'use-package-handler/:bind-keymap* "use-package-bind-key" "

(fn NAME KEYWORD ARG REST STATE)") (register-definition-prefixes "use-package-bind-key" '("use-package-handler/:bind*")) (autoload 'use-package "use-package-core" "Declare an Emacs package by specifying a group of configuration options.

For the full documentation, see Info node `(use-package) top'.
Usage:

  (use-package package-name
     [:keyword [option]]...)

:init            Code to run before PACKAGE-NAME has been loaded.
:config          Code to run after PACKAGE-NAME has been loaded.  Note that
                 if loading is deferred for any reason, this code does not
                 execute until the lazy load has occurred.
:preface         Code to be run before everything except `:disabled'; this
                 can be used to define functions for use in `:if', or that
                 should be seen by the byte-compiler.

:mode            Form to be added to `auto-mode-alist'.
:magic           Form to be added to `magic-mode-alist'.
:magic-fallback  Form to be added to `magic-fallback-mode-alist'.
:interpreter     Form to be added to `interpreter-mode-alist'.

:commands        Define autoloads for commands that will be defined by the
                 package.  This is useful if the package is being lazily
                 loaded, and you wish to conditionally call functions in your
                 `:init' block that are defined in the package.
:autoload        Similar to :commands, but it for no-interactive one.
:hook            Specify hook(s) to attach this package to.

:bind            Bind keys, and define autoloads for the bound commands.
:bind*           Bind keys, and define autoloads for the bound commands,
                 *overriding all minor mode bindings*.
:bind-keymap     Bind a key prefix to an auto-loaded keymap defined in the
                 package.  This is like `:bind', but for keymaps.
:bind-keymap*    Like `:bind-keymap', but overrides all minor mode bindings

:defer           Defer loading of a package -- this is implied when using
                 `:commands', `:bind', `:bind*', `:mode', `:magic', `:hook',
                 `:magic-fallback', or `:interpreter'.  This can be an integer,
                 to force loading after N seconds of idle time, if the package
                 has not already been loaded.
:demand          Prevent the automatic deferred loading introduced by constructs
                 such as `:bind' (see `:defer' for the complete list).

:after           Delay the effect of the use-package declaration
                 until after the named libraries have loaded.
                 Before they have been loaded, no other keyword
                 has any effect at all, and once they have been
                 loaded it is as if `:after' was not specified.

:if EXPR         Initialize and load only if EXPR evaluates to a non-nil value.
:disabled        The package is ignored completely if this keyword is present.
:defines         Declare certain variables to silence the byte-compiler.
:functions       Declare certain functions to silence the byte-compiler.
:load-path       Add to the `load-path' before attempting to load the package.
:diminish        Support for diminish.el (if installed).
:delight         Support for delight.el (if installed).
:custom          Call `Custom-set' or `set-default' with each variable
                 definition without modifying the Emacs `custom-file'.
                 (compare with `custom-set-variables').
:custom-face     Call `custom-set-faces' with each face definition.
:ensure          Loads the package using package.el if necessary.
:pin             Pin the package to an archive.

(fn NAME &rest ARGS)" nil t) (function-put 'use-package 'lisp-indent-function 'defun) (register-definition-prefixes "use-package-core" '("use-package-")) (autoload 'use-package-normalize/:delight "use-package-delight" "Normalize arguments to delight.

(fn NAME KEYWORD ARGS)") (autoload 'use-package-handler/:delight "use-package-delight" "

(fn NAME KEYWORD ARGS REST STATE)") (register-definition-prefixes "use-package-delight" '("use-package-normalize-delight")) (autoload 'use-package-normalize/:diminish "use-package-diminish" "

(fn NAME KEYWORD ARGS)") (autoload 'use-package-handler/:diminish "use-package-diminish" "

(fn NAME KEYWORD ARG REST STATE)") (register-definition-prefixes "use-package-diminish" '("use-package-normalize-diminish")) (autoload 'use-package-normalize/:ensure "use-package-ensure" "

(fn NAME KEYWORD ARGS)") (autoload 'use-package-handler/:ensure "use-package-ensure" "

(fn NAME KEYWORD ENSURE REST STATE)") (register-definition-prefixes "use-package-ensure" '("use-package-")) (autoload 'use-package-jump-to-package-form "use-package-jump" "Attempt to find and jump to the `use-package' form that loaded PACKAGE.
This will only find the form if that form actually required
PACKAGE.  If PACKAGE was previously required then this function
will jump to the file that originally required PACKAGE instead.

(fn PACKAGE)" t) (register-definition-prefixes "use-package-jump" '("use-package-find-require")) (autoload 'use-package-lint "use-package-lint" "Check for errors in `use-package' declarations.
For example, if the module's `:if' condition is met, but even
with the specified `:load-path' the module cannot be found." t) (register-definition-prefixes "use-package-lint" '("use-package-lint-declaration")) (provide 'use-package-autoloads)) "org" ((ob-comint org-ctags ox-texinfo ol-doi org-refile org-version org-num ol-mhe ob-shell org-attach ob-C org-entities ob-dot ob-sql ol-eww org-datetree org-macro ob-eval ob-groovy ox-icalendar org-mobile ob-processing oc-csl ob-octave org-table ox-html ol ob-plantuml ol-docview org-fold-core ob-exp ob-ditaa ob-scheme oc-bibtex ol-gnus org-mouse org-inlinetask ob-fortran ob-haskell org-id oc ob-css org-compat org-element ob ob-calc ob-python org-agenda org-persist ob-ocaml org-tempo ob-ref ob-tangle org-fold ob-lilypond ob-clojure org-lint ol-eshell ox-latex org-pcomplete org-duration ox-beamer ol-info ob-table ol-rmail ox-odt org-protocol ob-latex ob-julia org-archive ol-w3m ob-sed ob-perl ob-eshell ol-man org-plot org-clock ob-awk org-list ob-sqlite org-macs ox-man ol-bbdb org-loaddefs ol-irc org org-capture org-colview org-goto ob-R ob-ruby ob-sass ob-matlab ox oc-natbib ob-core ob-makefile org-timer ob-org ob-forth oc-biblatex ob-lob ob-gnuplot org-faces org-habit ox-koma-letter ob-js ob-screen org-element-ast org-footnote oc-basic ol-bibtex ob-java ob-lua ox-md org-cycle ox-ascii org-keys ob-lisp org-attach-git ob-emacs-lisp ob-maxima org-crypt org-feed org-src ox-org org-indent ox-publish)) "dash" ((dash dash-autoloads) (autoload 'dash-fontify-mode "dash" "Toggle fontification of Dash special variables.

Dash-Fontify mode is a buffer-local minor mode intended for Emacs
Lisp buffers.  Enabling it causes the special variables bound in
anaphoric Dash macros to be fontified.  These anaphoras include
`it', `it-index', `acc', and `other'.  In older Emacs versions
which do not dynamically detect macros, Dash-Fontify mode
additionally fontifies Dash macro calls.

See also `dash-fontify-mode-lighter' and
`global-dash-fontify-mode'.

This is a minor mode.  If called interactively, toggle the
`Dash-Fontify mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `dash-fontify-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'global-dash-fontify-mode 'globalized-minor-mode t) (defvar global-dash-fontify-mode nil "Non-nil if Global Dash-Fontify mode is enabled.
See the `global-dash-fontify-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-dash-fontify-mode'.") (custom-autoload 'global-dash-fontify-mode "dash" nil) (autoload 'global-dash-fontify-mode "dash" "Toggle Dash-Fontify mode in all buffers.
With prefix ARG, enable Global Dash-Fontify mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Dash-Fontify mode is enabled in all buffers where
`dash--turn-on-fontify-mode' would do it.

See `dash-fontify-mode' for more information on Dash-Fontify mode.

(fn &optional ARG)" t) (autoload 'dash-register-info-lookup "dash" "Register the Dash Info manual with `info-lookup-symbol'.
This allows Dash symbols to be looked up with \\[info-lookup-symbol]." t) (register-definition-prefixes "dash" '("!cdr" "!cons" "--" "->" "-a" "-butlast" "-c" "-d" "-e" "-f" "-gr" "-i" "-juxt" "-keep" "-l" "-m" "-no" "-o" "-p" "-r" "-s" "-t" "-u" "-value-to-list" "-when-let" "-zip" "dash-")) (provide 'dash-autoloads)) "emacsql" ((emacsql-mysql emacsql-compiler emacsql-sqlite-common emacsql emacsql-sqlite-module emacsql-autoloads emacsql-sqlite emacsql-sqlite-builtin emacsql-pg emacsql-psql) (autoload 'emacsql-show-last-sql "emacsql" "Display the compiled SQL of the s-expression SQL expression before point.
A prefix argument causes the SQL to be printed into the current buffer.

(fn &optional PREFIX)" t) (register-definition-prefixes "emacsql" '("emacsql-")) (register-definition-prefixes "emacsql-compiler" '("emacsql-")) (register-definition-prefixes "emacsql-mysql" '("emacsql-mysql-")) (register-definition-prefixes "emacsql-pg" '("emacsql-pg-connection")) (register-definition-prefixes "emacsql-psql" '("emacsql-psql-")) (register-definition-prefixes "emacsql-sqlite" '("emacsql-sqlite-")) (register-definition-prefixes "emacsql-sqlite-builtin" '("emacsql-sqlite-builtin-connection")) (register-definition-prefixes "emacsql-sqlite-common" '("emacsql-")) (register-definition-prefixes "emacsql-sqlite-module" '("emacsql-sqlite-module-connection")) (provide 'emacsql-autoloads)) "compat" ((compat-27 compat-26 compat-29 compat-macs compat-tests compat-28 compat compat-autoloads compat-25) (register-definition-prefixes "compat" '("compat-")) (register-definition-prefixes "compat-macs" '("compat-")) (register-definition-prefixes "compat-tests" '("compat-tests--" "should-equal")) (provide 'compat-autoloads)) "magit-section" ((magit-section-autoloads magit-section magit-section-pkg) (register-definition-prefixes "magit-section" '("isearch-clean-overlays@magit-mode" "magit-")) (provide 'magit-section-autoloads)) "org-roam" ((org-roam-id org-roam-log org-roam-utils org-roam-db org-roam-capture org-roam-migrate org-roam-dailies org-roam-export org-roam-node org-roam org-roam-overlay org-roam-autoloads org-roam-mode org-roam-protocol org-roam-compat org-roam-graph) (autoload 'org-roam-list-files "org-roam" "Return a list of all Org-roam files under `org-roam-directory'.
See `org-roam-file-p' for how each file is determined to be as
part of Org-Roam.") (register-definition-prefixes "org-roam" '("org-roam-")) (autoload 'org-roam-capture- "org-roam-capture" "Main entry point of `org-roam-capture' module.
GOTO and KEYS correspond to `org-capture' arguments.
INFO is a plist for filling up Org-roam's capture templates.
NODE is an `org-roam-node' construct containing information about the node.
PROPS is a plist containing additional Org-roam properties for each template.
TEMPLATES is a list of org-roam templates.

(fn &key GOTO KEYS NODE INFO PROPS TEMPLATES)") (autoload 'org-roam-capture "org-roam-capture" "Launches an `org-capture' process for a new or existing node.
This uses the templates defined at `org-roam-capture-templates'.
Arguments GOTO and KEYS see `org-capture'.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
The TEMPLATES, if provided, override the list of capture templates (see
`org-roam-capture-'.)
The INFO, if provided, is passed along to the underlying `org-roam-capture-'.

(fn &optional GOTO KEYS &key FILTER-FN TEMPLATES INFO)" t) (register-definition-prefixes "org-roam-capture" '("org-roam-capture-")) (register-definition-prefixes "org-roam-compat" '("org-roam--")) (autoload 'org-roam-dailies-capture-today "org-roam-dailies" "Create an entry in the daily-note for today.
When GOTO is non-nil, go the note without creating an entry.

ELisp programs can set KEYS to a string associated with a template.
In this case, interactive selection will be bypassed.

(fn &optional GOTO KEYS)" t) (autoload 'org-roam-dailies-goto-today "org-roam-dailies" "Find the daily-note for today, creating it if necessary.

ELisp programs can set KEYS to a string associated with a template.
In this case, interactive selection will be bypassed.

(fn &optional KEYS)" t) (autoload 'org-roam-dailies-capture-tomorrow "org-roam-dailies" "Create an entry in the daily-note for tomorrow.

With numeric argument N, use the daily-note N days in the future.

With a `C-u' prefix or when GOTO is non-nil, go the note without
creating an entry.

ELisp programs can set KEYS to a string associated with a template.
In this case, interactive selection will be bypassed.

(fn N &optional GOTO KEYS)" t) (autoload 'org-roam-dailies-goto-tomorrow "org-roam-dailies" "Find the daily-note for tomorrow, creating it if necessary.

With numeric argument N, use the daily-note N days in the
future.

ELisp programs can set KEYS to a string associated with a template.
In this case, interactive selection will be bypassed.

(fn N &optional KEYS)" t) (autoload 'org-roam-dailies-capture-yesterday "org-roam-dailies" "Create an entry in the daily-note for yesteday.

With numeric argument N, use the daily-note N days in the past.

When GOTO is non-nil, go the note without creating an entry.

ELisp programs can set KEYS to a string associated with a template.
In this case, interactive selection will be bypassed.

(fn N &optional GOTO KEYS)" t) (autoload 'org-roam-dailies-goto-yesterday "org-roam-dailies" "Find the daily-note for yesterday, creating it if necessary.

With numeric argument N, use the daily-note N days in the
future.

ELisp programs can set KEYS to a string associated with a template.
In this case, interactive selection will be bypassed.

(fn N &optional KEYS)" t) (autoload 'org-roam-dailies-capture-date "org-roam-dailies" "Create an entry in the daily-note for a date using the calendar.
Prefer past dates, unless PREFER-FUTURE is non-nil.
With a `C-u' prefix or when GOTO is non-nil, go the note without
creating an entry.

ELisp programs can set KEYS to a string associated with a template.
In this case, interactive selection will be bypassed.

(fn &optional GOTO PREFER-FUTURE KEYS)" t) (autoload 'org-roam-dailies-goto-date "org-roam-dailies" "Find the daily-note for a date using the calendar, creating it if necessary.
Prefer past dates, unless PREFER-FUTURE is non-nil.

ELisp programs can set KEYS to a string associated with a template.
In this case, interactive selection will be bypassed.

(fn &optional PREFER-FUTURE KEYS)" t) (autoload 'org-roam-dailies-find-directory "org-roam-dailies" "Find and open `org-roam-dailies-directory'." t) (register-definition-prefixes "org-roam-dailies" '("org-roam-dailies-")) (autoload 'org-roam-db-sync "org-roam-db" "Synchronize the cache state with the current Org files on-disk.
If FORCE, force a rebuild of the cache from scratch.

(fn &optional FORCE)" t) (defvar org-roam-db-autosync-mode nil "Non-nil if Org-Roam-Db-Autosync mode is enabled.
See the `org-roam-db-autosync-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-roam-db-autosync-mode'.") (custom-autoload 'org-roam-db-autosync-mode "org-roam-db" nil) (autoload 'org-roam-db-autosync-mode "org-roam-db" "Global minor mode to keep your Org-roam session automatically synchronized.

Through the session this will continue to setup your
buffers (that are Org-roam file visiting), keep track of the
related changes, maintain cache consistency and incrementally
update the currently active database.

If you need to manually trigger resync of the currently active
database, see `org-roam-db-sync' command.

This is a global minor mode.  If called interactively, toggle the
`Org-Roam-Db-Autosync mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='org-roam-db-autosync-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'org-roam-db-autosync-enable "org-roam-db" "Activate `org-roam-db-autosync-mode'.") (register-definition-prefixes "org-roam-db" '("emacsql-constraint" "org-roam-d")) (register-definition-prefixes "org-roam-export" '("org-roam-export--org-html--reference")) (autoload 'org-roam-graph "org-roam-graph" "Build and possibly display a graph for NODE.
ARG may be any of the following values:
  - nil       show the graph.
  - `\\[universal-argument]'     show the graph for NODE.
  - `\\[universal-argument]' N   show the graph for NODE limiting nodes to N steps.

(fn &optional ARG NODE)" t) (register-definition-prefixes "org-roam-graph" '("org-roam-")) (autoload 'org-roam-update-org-id-locations "org-roam-id" "Scan Org-roam files to update `org-id' related state.
This is like `org-id-update-id-locations', but will automatically
use the currently bound `org-directory' and `org-roam-directory'
along with DIRECTORIES (if any), where the lookup for files in
these directories will be always recursive.

Note: Org-roam doesn't have hard dependency on
`org-id-locations-file' to lookup IDs for nodes that are stored
in the database, but it still tries to properly integrates with
`org-id'. This allows the user to cross-reference IDs outside of
the current `org-roam-directory', and also link with \"id:\"
links to headings/files within the current `org-roam-directory'
that are excluded from identification in Org-roam as
`org-roam-node's, e.g. with \"ROAM_EXCLUDE\" property.

(fn &rest DIRECTORIES)" t) (register-definition-prefixes "org-roam-id" '("org-roam-id-")) (register-definition-prefixes "org-roam-log" '("org-roam-log-")) (autoload 'org-roam-migrate-wizard "org-roam-migrate" "Migrate all notes from to be compatible with Org-roam v2.
1. Convert all notes from v1 format to v2.
2. Rebuild the cache.
3. Replace all file links with ID links." t) (register-definition-prefixes "org-roam-migrate" '("org-roam-migrate-")) (autoload 'org-roam-buffer-display-dedicated "org-roam-mode" "Launch NODE dedicated Org-roam buffer.
Unlike the persistent `org-roam-buffer', the contents of this
buffer won't be automatically changed and will be held in place.

In interactive calls prompt to select NODE, unless called with
`universal-argument', in which case NODE will be set to
`org-roam-node-at-point'.

(fn NODE)" t) (register-definition-prefixes "org-roam-mode" '("org-roam-")) (autoload 'org-roam-node-find "org-roam-node" "Find and open an Org-roam node by its title or alias.
INITIAL-INPUT is the initial input for the prompt.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
If OTHER-WINDOW, visit the NODE in another window.
The TEMPLATES, if provided, override the list of capture templates (see
`org-roam-capture-'.)

(fn &optional OTHER-WINDOW INITIAL-INPUT FILTER-FN PRED &key TEMPLATES)" t) (autoload 'org-roam-node-random "org-roam-node" "Find and open a random Org-roam node.
With prefix argument OTHER-WINDOW, visit the node in another
window instead.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.

(fn &optional OTHER-WINDOW FILTER-FN)" t) (autoload 'org-roam-node-insert "org-roam-node" "Find an Org-roam node and insert (where the point is) an \"id:\" link to it.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
The TEMPLATES, if provided, override the list of capture templates (see
`org-roam-capture-'.)
The INFO, if provided, is passed to the underlying `org-roam-capture-'.

(fn &optional FILTER-FN &key TEMPLATES INFO)" t) (autoload 'org-roam-refile "org-roam-node" "Refile node at point to an Org-roam node.
If region is active, then use it instead of the node at point." t) (autoload 'org-roam-extract-subtree "org-roam-node" "Convert current subtree at point to a node, and extract it into a new file." t) (autoload 'org-roam-ref-find "org-roam-node" "Find and open an Org-roam node that's dedicated to a specific ref.
INITIAL-INPUT is the initial input to the prompt.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.

(fn &optional INITIAL-INPUT FILTER-FN)" t) (register-definition-prefixes "org-roam-node" '("org-roam-")) (register-definition-prefixes "org-roam-overlay" '("org-roam-overlay-")) (register-definition-prefixes "org-roam-protocol" '("org-roam-")) (autoload 'org-roam-version "org-roam-utils" "Return `org-roam' version.
Interactively, or when MESSAGE is non-nil, show in the echo area.

(fn &optional MESSAGE)" t) (autoload 'org-roam-diagnostics "org-roam-utils" "Collect and print info for `org-roam' issues." t) (register-definition-prefixes "org-roam-utils" '("org-roam-")) (provide 'org-roam-autoloads)) "deft" ((deft deft-autoloads) (autoload 'deft-find-file "deft" "Find FILE interactively using the minibuffer.
FILE must exist and be a relative or absolute path, with extension.
If FILE is not inside `deft-directory', fall back to using `find-file'.

(fn FILE)" t) (autoload 'deft-new-file "deft" "Create a new file quickly.
Use either an automatically generated filename or the filter string if non-nil
and `deft-use-filter-string-for-filename' is set.  If the filter string is
non-nil and title is not from filename, use it as the title." t) (autoload 'deft "deft" "Switch to *Deft* buffer and load files." t) (register-definition-prefixes "deft" '("deft-" "org-deft-store-link")) (provide 'deft-autoloads)) "vertico" ((vertico-autoloads vertico-multiform vertico-indexed vertico-grid vertico vertico-quick vertico-flat vertico-directory vertico-reverse vertico-mouse vertico-unobtrusive vertico-repeat vertico-buffer) (defvar vertico-mode nil "Non-nil if Vertico mode is enabled.
See the `vertico-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-mode'.") (custom-autoload 'vertico-mode "vertico" nil) (autoload 'vertico-mode "vertico" "VERTical Interactive COmpletion.

This is a global minor mode.  If called interactively, toggle the
`Vertico mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico" '("vertico-")) (defvar vertico-buffer-mode nil "Non-nil if Vertico-Buffer mode is enabled.
See the `vertico-buffer-mode' command
for a description of this minor mode.") (custom-autoload 'vertico-buffer-mode "vertico-buffer" nil) (autoload 'vertico-buffer-mode "vertico-buffer" "Display Vertico in a buffer instead of the minibuffer.

This is a global minor mode.  If called interactively, toggle the
`Vertico-Buffer mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-buffer-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-buffer" '("vertico-buffer-")) (autoload 'vertico-directory-enter "vertico-directory" "Enter directory or exit completion with current candidate.
Exit with current input if prefix ARG is given.

(fn &optional ARG)" t) (autoload 'vertico-directory-up "vertico-directory" "Delete N names before point.

(fn &optional N)" t) (autoload 'vertico-directory-delete-char "vertico-directory" "Delete N directories or chars before point.

(fn &optional N)" t) (autoload 'vertico-directory-delete-word "vertico-directory" "Delete N directories or words before point.

(fn &optional N)" t) (autoload 'vertico-directory-tidy "vertico-directory" "Tidy shadowed file name, see `rfn-eshadow-overlay'.") (defvar vertico-flat-mode nil "Non-nil if Vertico-Flat mode is enabled.
See the `vertico-flat-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-flat-mode'.") (custom-autoload 'vertico-flat-mode "vertico-flat" nil) (autoload 'vertico-flat-mode "vertico-flat" "Flat, horizontal display for Vertico.

This is a global minor mode.  If called interactively, toggle the
`Vertico-Flat mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-flat-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-flat" '("vertico-flat-")) (defvar vertico-grid-mode nil "Non-nil if Vertico-Grid mode is enabled.
See the `vertico-grid-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-grid-mode'.") (custom-autoload 'vertico-grid-mode "vertico-grid" nil) (autoload 'vertico-grid-mode "vertico-grid" "Grid display for Vertico.

This is a global minor mode.  If called interactively, toggle the
`Vertico-Grid mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-grid-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-grid" '("vertico-grid-")) (defvar vertico-indexed-mode nil "Non-nil if Vertico-Indexed mode is enabled.
See the `vertico-indexed-mode' command
for a description of this minor mode.") (custom-autoload 'vertico-indexed-mode "vertico-indexed" nil) (autoload 'vertico-indexed-mode "vertico-indexed" "Prefix candidates with indices.

This is a global minor mode.  If called interactively, toggle the
`Vertico-Indexed mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-indexed-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-indexed" '("vertico-indexed-")) (defvar vertico-mouse-mode nil "Non-nil if Vertico-Mouse mode is enabled.
See the `vertico-mouse-mode' command
for a description of this minor mode.") (custom-autoload 'vertico-mouse-mode "vertico-mouse" nil) (autoload 'vertico-mouse-mode "vertico-mouse" "Mouse support for Vertico.

This is a global minor mode.  If called interactively, toggle the
`Vertico-Mouse mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-mouse-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-mouse" '("vertico-mouse-")) (defvar vertico-multiform-mode nil "Non-nil if Vertico-Multiform mode is enabled.
See the `vertico-multiform-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-multiform-mode'.") (custom-autoload 'vertico-multiform-mode "vertico-multiform" nil) (autoload 'vertico-multiform-mode "vertico-multiform" "Configure Vertico in various forms per command.

This is a global minor mode.  If called interactively, toggle the
`Vertico-Multiform mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-multiform-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-multiform" '("vertico-multiform-")) (autoload 'vertico-quick-jump "vertico-quick" "Jump to candidate using quick keys." t) (autoload 'vertico-quick-exit "vertico-quick" "Exit with candidate using quick keys." t) (autoload 'vertico-quick-insert "vertico-quick" "Insert candidate using quick keys." t) (register-definition-prefixes "vertico-quick" '("vertico-quick")) (autoload 'vertico-repeat-save "vertico-repeat" "Save Vertico session for `vertico-repeat'.
This function must be registered as `minibuffer-setup-hook'.") (autoload 'vertico-repeat-last "vertico-repeat" "Repeat last Vertico completion SESSION.
If called interactively from an existing Vertico session,
`vertico-repeat-last' will restore the last input and
last selected candidate for the current command.

(fn &optional SESSION)" t) (autoload 'vertico-repeat-select "vertico-repeat" "Select a Vertico session from the session history and repeat it.
If called from an existing Vertico session, you can select among
previous sessions for the current command." t) (autoload 'vertico-repeat "vertico-repeat" "Repeat last Vertico session.
If prefix ARG is non-nil, offer completion menu to select from session history.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-repeat" '("vertico-repeat-")) (defvar vertico-reverse-mode nil "Non-nil if Vertico-Reverse mode is enabled.
See the `vertico-reverse-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-reverse-mode'.") (custom-autoload 'vertico-reverse-mode "vertico-reverse" nil) (autoload 'vertico-reverse-mode "vertico-reverse" "Reverse the Vertico display.

This is a global minor mode.  If called interactively, toggle the
`Vertico-Reverse mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-reverse-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-reverse" '("vertico-reverse-map")) (defvar vertico-unobtrusive-mode nil "Non-nil if Vertico-Unobtrusive mode is enabled.
See the `vertico-unobtrusive-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-unobtrusive-mode'.") (custom-autoload 'vertico-unobtrusive-mode "vertico-unobtrusive" nil) (autoload 'vertico-unobtrusive-mode "vertico-unobtrusive" "Unobtrusive display for Vertico.

This is a global minor mode.  If called interactively, toggle the
`Vertico-Unobtrusive mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-unobtrusive-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-unobtrusive" '("vertico-unobtrusive--orig-count")) (provide 'vertico-autoloads)) "orderless" ((orderless orderless-autoloads) (autoload 'orderless-filter "orderless" "Split STRING into components and find entries TABLE matching all.
The predicate PRED is used to constrain the entries in TABLE.

(fn STRING TABLE &optional PRED)") (autoload 'orderless-all-completions "orderless" "Split STRING into components and find entries TABLE matching all.
The predicate PRED is used to constrain the entries in TABLE.  The
matching portions of each candidate are highlighted.
This function is part of the `orderless' completion style.

(fn STRING TABLE PRED POINT)") (autoload 'orderless-try-completion "orderless" "Complete STRING to unique matching entry in TABLE.
This uses `orderless-all-completions' to find matches for STRING
in TABLE among entries satisfying PRED.  If there is only one
match, it completes to that match.  If there are no matches, it
returns nil.  In any other case it \"completes\" STRING to
itself, without moving POINT.
This function is part of the `orderless' completion style.

(fn STRING TABLE PRED POINT)") (add-to-list 'completion-styles-alist '(orderless orderless-try-completion orderless-all-completions "Completion of multiple components, in any order.")) (autoload 'orderless-ivy-re-builder "orderless" "Convert STR into regexps for use with ivy.
This function is for integration of orderless with ivy, use it as
a value in `ivy-re-builders-alist'.

(fn STR)") (register-definition-prefixes "orderless" '("orderless-")) (provide 'orderless-autoloads)) "embark" ((embark-autoloads embark-org embark) (defun embark--record-this-command nil "Record command which opened the minibuffer.
We record this because it will be the default action.
This function is meant to be added to `minibuffer-setup-hook'." (setq-local embark--command this-command)) (add-hook 'minibuffer-setup-hook #'embark--record-this-command) (autoload 'embark-eldoc-first-target "embark" "Eldoc function reporting the first Embark target at point.
This function uses the eldoc REPORT callback and is meant to be
added to `eldoc-documentation-functions'.

(fn REPORT &rest _)") (autoload 'embark-eldoc-target-types "embark" "Eldoc function reporting the types of all Embark targets at point.
This function uses the eldoc REPORT callback and is meant to be
added to `eldoc-documentation-functions'.

(fn REPORT &rest _)") (autoload 'embark-bindings-in-keymap "embark" "Explore command key bindings in KEYMAP with `completing-read'.
The selected command will be executed.  Interactively, prompt the
user for a KEYMAP variable.

(fn KEYMAP)" t) (autoload 'embark-bindings "embark" "Explore current command key bindings with `completing-read'.
The selected command will be executed.

This shows key bindings from minor mode maps and the local
map (usually set by the major mode), but also less common keymaps
such as those from a text property or overlay, or the overriding
maps: `overriding-terminal-local-map' and `overriding-local-map'.

Additionally, if GLOBAL is non-nil (interactively, if called with
a prefix argument), this command includes global key bindings.

(fn GLOBAL)" t) (autoload 'embark-bindings-at-point "embark" "Explore all key bindings at point with `completing-read'.
The selected command will be executed.

This command lists key bindings found in keymaps specified by the
text properties `keymap' or `local-map', from either buffer text
or an overlay.  These are not widely used in Emacs, and when they
are used can be somewhat hard to discover.  Examples of locations
that have such a keymap are links and images in `eww' buffers,
attachment links in `gnus' article buffers, and the stash line
in a `vc-dir' buffer." t) (autoload 'embark-prefix-help-command "embark" "Prompt for and run a command bound in the prefix used for this command.
The prefix described consists of all but the last event of the
key sequence that ran this command.  This function is intended to
be used as a value for `prefix-help-command'.

In addition to using completion to select a command, you can also
type @ and the key binding (without the prefix)." t) (autoload 'embark-act "embark" "Prompt the user for an action and perform it.
The targets of the action are chosen by `embark-target-finders'.
By default, if called from a minibuffer the target is the top
completion candidate.  When called from a non-minibuffer buffer
there can multiple targets and you can cycle among them by using
`embark-cycle' (which is bound by default to the same key
binding `embark-act' is, but see `embark-cycle-key').

This command uses `embark-prompter' to ask the user to specify an
action, and calls it injecting the target at the first minibuffer
prompt.

If you call this from the minibuffer, it can optionally quit the
minibuffer.  The variable `embark-quit-after-action' controls
whether calling `embark-act' with nil ARG quits the minibuffer,
and if ARG is non-nil it will do the opposite.  Interactively,
ARG is the prefix argument.

If instead you call this from outside the minibuffer, the first
ARG targets are skipped over (if ARG is negative the skipping is
done by cycling backwards) and cycling starts from the following
target.

(fn &optional ARG)" t) (autoload 'embark-act-all "embark" "Prompt the user for an action and perform it on each candidate.
The candidates are chosen by `embark-candidate-collectors'.  By
default, if `embark-select' has been used to select some
candidates, then `embark-act-all' will act on those candidates;
otherwise, if the selection is empty and `embark-act-all' is
called from a minibuffer, then the candidates are the completion
candidates.

This command uses `embark-prompter' to ask the user to specify an
action, and calls it injecting the target at the first minibuffer
prompt.

If you call this from the minibuffer, it can optionally quit the
minibuffer.  The variable `embark-quit-after-action' controls
whether calling `embark-act' with nil ARG quits the minibuffer,
and if ARG is non-nil it will do the opposite.  Interactively,
ARG is the prefix argument.

(fn &optional ARG)" t) (autoload 'embark-dwim "embark" "Run the default action on the current target.
The target of the action is chosen by `embark-target-finders'.

If the target comes from minibuffer completion, then the default
action is the command that opened the minibuffer in the first
place, unless overridden by `embark-default-action-overrides'.

For targets that do not come from minibuffer completion
(typically some thing at point in a regular buffer) and whose
type is not listed in `embark-default-action-overrides', the
default action is given by whatever binding RET has in the action
keymap for the target's type.

See `embark-act' for the meaning of the prefix ARG.

(fn &optional ARG)" t) (autoload 'embark-become "embark" "Make current command become a different command.
Take the current minibuffer input as initial input for new
command.  The new command can be run normally using key bindings or
\\[execute-extended-command], but if the current command is found in a keymap in
`embark-become-keymaps', that keymap is activated to provide
convenient access to the other commands in it.

If FULL is non-nil (interactively, if called with a prefix
argument), the entire minibuffer contents are used as the initial
input of the new command.  By default only the part of the
minibuffer contents between the current completion boundaries is
taken.  What this means is fairly technical, but (1) usually
there is no difference: the completion boundaries include the
entire minibuffer contents, and (2) the most common case where
these notions differ is file completion, in which case the
completion boundaries single out the path component containing
point.

(fn &optional FULL)" t) (autoload 'embark-collect "embark" "Create an Embark Collect buffer.

To control the display, add an entry to `display-buffer-alist'
with key \"Embark Collect\".

In Embark Collect buffers `revert-buffer' is remapped to
`embark-rerun-collect-or-export', which has slightly unusual
behavior if the buffer was obtained by running `embark-collect'
from within a minibuffer completion session.  In that case
rerunning just restarts the completion session, that is, the
command that opened the minibuffer is run again and the
minibuffer contents restored.  You can then interact normally with
the command, perhaps editing the minibuffer contents, and, if you
wish, you can rerun `embark-collect' to get an updated buffer." t) (autoload 'embark-live "embark" "Create a live-updating Embark Collect buffer.

To control the display, add an entry to `display-buffer-alist'
with key \"Embark Live\"." t) (autoload 'embark-export "embark" "Create a type-specific buffer to manage current candidates.
The variable `embark-exporters-alist' controls how to make the
buffer for each type of completion.

In Embark Export buffers `revert-buffer' is remapped to
`embark-rerun-collect-or-export', which has slightly unusual
behavior if the buffer was obtained by running `embark-export'
from within a minibuffer completion session.  In that case
reverting just restarts the completion session, that is, the
command that opened the minibuffer is run again and the
minibuffer contents restored.  You can then interact normally
with the command, perhaps editing the minibuffer contents, and,
if you wish, you can rerun `embark-export' to get an updated
buffer." t) (autoload 'embark-select "embark" "Add or remove the target from the current buffer's selection.
You can act on all selected targets at once with `embark-act-all'.
When called from outside `embark-act' this command will select
the first target at point." t) (register-definition-prefixes "embark" '("embark-")) (register-definition-prefixes "embark-org" '("embark-org-")) (provide 'embark-autoloads)) "consult" ((consult-autoloads consult-org consult-info consult-imenu consult-compile consult-flymake consult-xref consult-register consult-kmacro consult) (autoload 'consult-completion-in-region "consult" "Use minibuffer completion as the UI for `completion-at-point'.

The function is called with 4 arguments: START END COLLECTION PREDICATE.
The arguments and expected return value are as specified for
`completion-in-region'.  Use as a value for `completion-in-region-function'.

The function can be configured via `consult-customize'.

    (consult-customize consult-completion-in-region
                       :completion-styles (basic)
                       :cycle-threshold 3)

These configuration options are supported:

    * :cycle-threshold - Cycling threshold (def: `completion-cycle-threshold')
    * :completion-styles - Use completion styles (def: `completion-styles')
    * :require-match - Require matches when completing (def: nil)
    * :prompt - The prompt string shown in the minibuffer

(fn START END COLLECTION &optional PREDICATE)") (autoload 'consult-outline "consult" "Jump to an outline heading, obtained by matching against `outline-regexp'.

This command supports narrowing to a heading level and candidate preview.
The symbol at point is added to the future history." t) (autoload 'consult-mark "consult" "Jump to a marker in MARKERS list (defaults to buffer-local `mark-ring').

The command supports preview of the currently selected marker position.
The symbol at point is added to the future history.

(fn &optional MARKERS)" t) (autoload 'consult-global-mark "consult" "Jump to a marker in MARKERS list (defaults to `global-mark-ring').

The command supports preview of the currently selected marker position.
The symbol at point is added to the future history.

(fn &optional MARKERS)" t) (autoload 'consult-line "consult" "Search for a matching line.

Depending on the setting `consult-point-placement' the command
jumps to the beginning or the end of the first match on the line
or the line beginning.  The default candidate is the non-empty
line next to point.  This command obeys narrowing.  Optional
INITIAL input can be provided.  The search starting point is
changed if the START prefix argument is set.  The symbol at point
and the last `isearch-string' is added to the future history.

(fn &optional INITIAL START)" t) (autoload 'consult-line-multi "consult" "Search for a matching line in multiple buffers.

By default search across all project buffers.  If the prefix
argument QUERY is non-nil, all buffers are searched.  Optional
INITIAL input can be provided.  The symbol at point and the last
`isearch-string' is added to the future history.  In order to
search a subset of buffers, QUERY can be set to a plist according
to `consult--buffer-query'.

(fn QUERY &optional INITIAL)" t) (autoload 'consult-keep-lines "consult" "Select a subset of the lines in the current buffer with live preview.

The selected lines are kept and the other lines are deleted.  When called
interactively, the lines selected are those that match the minibuffer input.  In
order to match the inverse of the input, prefix the input with `! '.  When
called from Elisp, the filtering is performed by a FILTER function.  This
command obeys narrowing.

FILTER is the filter function.
INITIAL is the initial input.

(fn FILTER &optional INITIAL)" t) (autoload 'consult-focus-lines "consult" "Hide or show lines using overlays.

The selected lines are shown and the other lines hidden.  When called
interactively, the lines selected are those that match the minibuffer input.  In
order to match the inverse of the input, prefix the input with `! '.  With
optional prefix argument SHOW reveal the hidden lines.  Alternatively the
command can be restarted to reveal the lines.  When called from Elisp, the
filtering is performed by a FILTER function.  This command obeys narrowing.

FILTER is the filter function.
INITIAL is the initial input.

(fn FILTER &optional SHOW INITIAL)" t) (autoload 'consult-goto-line "consult" "Read line number and jump to the line with preview.

Enter either a line number to jump to the first column of the
given line or line:column in order to jump to a specific column.
Jump directly if a line number is given as prefix ARG.  The
command respects narrowing and the settings
`consult-goto-line-numbers' and `consult-line-numbers-widen'.

(fn &optional ARG)" t) (autoload 'consult-recent-file "consult" "Find recent file using `completing-read'." t) (autoload 'consult-mode-command "consult" "Run a command from any of the given MODES.

If no MODES are specified, use currently active major and minor modes.

(fn &rest MODES)" t) (autoload 'consult-yank-from-kill-ring "consult" "Select STRING from the kill ring and insert it.
With prefix ARG, put point at beginning, and mark at end, like `yank' does.

This command behaves like `yank-from-kill-ring' in Emacs 28, which also offers
a `completing-read' interface to the `kill-ring'.  Additionally the Consult
version supports preview of the selected string.

(fn STRING &optional ARG)" t) (autoload 'consult-yank-pop "consult" "If there is a recent yank act like `yank-pop'.

Otherwise select string from the kill ring and insert it.
See `yank-pop' for the meaning of ARG.

This command behaves like `yank-pop' in Emacs 28, which also offers a
`completing-read' interface to the `kill-ring'.  Additionally the Consult
version supports preview of the selected string.

(fn &optional ARG)" t) (autoload 'consult-yank-replace "consult" "Select STRING from the kill ring.

If there was no recent yank, insert the string.
Otherwise replace the just-yanked string with the selected string.

There exists no equivalent of this command in Emacs 28.

(fn STRING)" t) (autoload 'consult-bookmark "consult" "If bookmark NAME exists, open it, otherwise create a new bookmark with NAME.

The command supports preview of file bookmarks and narrowing.  See the
variable `consult-bookmark-narrow' for the narrowing configuration.

(fn NAME)" t) (autoload 'consult-complex-command "consult" "Select and evaluate command from the command history.

This command can act as a drop-in replacement for `repeat-complex-command'." t) (autoload 'consult-history "consult" "Insert string from HISTORY of current buffer.
In order to select from a specific HISTORY, pass the history
variable as argument.  INDEX is the name of the index variable to
update, if any.  BOL is the function which jumps to the beginning
of the prompt.  See also `cape-history' from the Cape package.

(fn &optional HISTORY INDEX BOL)" t) (autoload 'consult-isearch-history "consult" "Read a search string with completion from the Isearch history.

This replaces the current search string if Isearch is active, and
starts a new Isearch session otherwise." t) (autoload 'consult-minor-mode-menu "consult" "Enable or disable minor mode.

This is an alternative to `minor-mode-menu-from-indicator'." t) (autoload 'consult-theme "consult" "Disable current themes and enable THEME from `consult-themes'.

The command supports previewing the currently selected theme.

(fn THEME)" t) (autoload 'consult-buffer "consult" "Enhanced `switch-to-buffer' command with support for virtual buffers.

The command supports recent files, bookmarks, views and project files as
virtual buffers.  Buffers are previewed.  Narrowing to buffers (b), files (f),
bookmarks (m) and project files (p) is supported via the corresponding
keys.  In order to determine the project-specific files and buffers, the
`consult-project-function' is used.  The virtual buffer SOURCES
default to `consult-buffer-sources'.  See `consult--multi' for the
configuration of the virtual buffer sources.

(fn &optional SOURCES)" t) (autoload 'consult-project-buffer "consult" "Enhanced `project-switch-to-buffer' command with support for virtual buffers.
The command may prompt you for a project directory if it is invoked from
outside a project.  See `consult-buffer' for more details." t) (autoload 'consult-buffer-other-window "consult" "Variant of `consult-buffer' which opens in other window." t) (autoload 'consult-buffer-other-frame "consult" "Variant of `consult-buffer' which opens in other frame." t) (autoload 'consult-grep "consult" "Search with `grep' for files in DIR where the content matches a regexp.

The initial input is given by the INITIAL argument.  DIR can be
nil, a directory string or a list of file/directory paths.  If
`consult-grep' is called interactively with a prefix argument,
the user can specify the directories or files to search in.
Multiple directories must be separated by comma in the
minibuffer, since they are read via `completing-read-multiple'.
By default the project directory is used if
`consult-project-function' is defined and returns non-nil.
Otherwise the `default-directory' is searched.

The input string is split, the first part of the string (grep
input) is passed to the asynchronous grep process and the second
part of the string is passed to the completion-style filtering.

The input string is split at a punctuation character, which is
given as the first character of the input string.  The format is
similar to Perl-style regular expressions, e.g., /regexp/.
Furthermore command line options can be passed to grep, specified
behind --.  The overall prompt input has the form
`#async-input -- grep-opts#filter-string'.

Note that the grep input string is transformed from Emacs regular
expressions to Posix regular expressions.  Always enter Emacs
regular expressions at the prompt.  `consult-grep' behaves like
builtin Emacs search commands, e.g., Isearch, which take Emacs
regular expressions.  Furthermore the asynchronous input split
into words, each word must match separately and in any order.
See `consult--regexp-compiler' for the inner workings.  In order
to disable transformations of the grep input, adjust
`consult--regexp-compiler' accordingly.

Here we give a few example inputs:

#alpha beta         : Search for alpha and beta in any order.
#alpha.*beta        : Search for alpha before beta.
#\\(alpha\\|beta\\) : Search for alpha or beta (Note Emacs syntax!)
#word -- -C3        : Search for word, include 3 lines as context
#first#second       : Search for first, quick filter for second.

The symbol at point is added to the future history.

(fn &optional DIR INITIAL)" t) (autoload 'consult-git-grep "consult" "Search with `git grep' for files in DIR with INITIAL input.
See `consult-grep' for details.

(fn &optional DIR INITIAL)" t) (autoload 'consult-ripgrep "consult" "Search with `rg' for files in DIR with INITIAL input.
See `consult-grep' for details.

(fn &optional DIR INITIAL)" t) (autoload 'consult-find "consult" "Search for files with `find' in DIR.
The file names must match the input regexp.  INITIAL is the
initial minibuffer input.  See `consult-grep' for details
regarding the asynchronous search and the arguments.

(fn &optional DIR INITIAL)" t) (autoload 'consult-fd "consult" "Search for files with `fd' in DIR.
The file names must match the input regexp.  INITIAL is the
initial minibuffer input.  See `consult-grep' for details
regarding the asynchronous search and the arguments.

(fn &optional DIR INITIAL)" t) (autoload 'consult-locate "consult" "Search with `locate' for files which match input given INITIAL input.

The input is treated literally such that locate can take advantage of
the locate database index.  Regular expressions would often force a slow
linear search through the entire database.  The locate process is started
asynchronously, similar to `consult-grep'.  See `consult-grep' for more
details regarding the asynchronous search.

(fn &optional INITIAL)" t) (autoload 'consult-man "consult" "Search for man page given INITIAL input.

The input string is not preprocessed and passed literally to the
underlying man commands.  The man process is started asynchronously,
similar to `consult-grep'.  See `consult-grep' for more details regarding
the asynchronous search.

(fn &optional INITIAL)" t) (register-definition-prefixes "consult" '("consult-")) (autoload 'consult-compile-error "consult-compile" "Jump to a compilation error in the current buffer.

This command collects entries from compilation buffers and grep
buffers related to the current buffer.  The command supports
preview of the currently selected error." t) (register-definition-prefixes "consult-compile" '("consult-compile--")) (autoload 'consult-flymake "consult-flymake" "Jump to Flymake diagnostic.
When PROJECT is non-nil then prompt with diagnostics from all
buffers in the current project instead of just the current buffer.

(fn &optional PROJECT)" t) (register-definition-prefixes "consult-flymake" '("consult-flymake--")) (autoload 'consult-imenu "consult-imenu" "Select item from flattened `imenu' using `completing-read' with preview.

The command supports preview and narrowing.  See the variable
`consult-imenu-config', which configures the narrowing.
The symbol at point is added to the future history.

See also `consult-imenu-multi'." t) (autoload 'consult-imenu-multi "consult-imenu" "Select item from the imenus of all buffers from the same project.

In order to determine the buffers belonging to the same project, the
`consult-project-function' is used.  Only the buffers with the
same major mode as the current buffer are used.  See also
`consult-imenu' for more details.  In order to search a subset of buffers,
QUERY can be set to a plist according to `consult--buffer-query'.

(fn &optional QUERY)" t) (register-definition-prefixes "consult-imenu" '("consult-imenu-")) (autoload 'consult-info "consult-info" "Full text search through info MANUALS.

(fn &rest MANUALS)" t) (register-definition-prefixes "consult-info" '("consult-info--")) (autoload 'consult-kmacro "consult-kmacro" "Run a chosen keyboard macro.

With prefix ARG, run the macro that many times.
Macros containing mouse clicks are omitted.

(fn ARG)" t) (register-definition-prefixes "consult-kmacro" '("consult-kmacro--")) (autoload 'consult-org-heading "consult-org" "Jump to an Org heading.

MATCH and SCOPE are as in `org-map-entries' and determine which
entries are offered.  By default, all entries of the current
buffer are offered.

(fn &optional MATCH SCOPE)" t) (autoload 'consult-org-agenda "consult-org" "Jump to an Org agenda heading.

By default, all agenda entries are offered.  MATCH is as in
`org-map-entries' and can used to refine this.

(fn &optional MATCH)" t) (register-definition-prefixes "consult-org" '("consult-org--")) (autoload 'consult-register-window "consult-register" "Enhanced drop-in replacement for `register-preview'.

BUFFER is the window buffer.
SHOW-EMPTY must be t if the window should be shown for an empty register list.

(fn BUFFER &optional SHOW-EMPTY)") (autoload 'consult-register-format "consult-register" "Enhanced preview of register REG.
This function can be used as `register-preview-function'.
If COMPLETION is non-nil format the register for completion.

(fn REG &optional COMPLETION)") (autoload 'consult-register "consult-register" "Load register and either jump to location or insert the stored text.

This command is useful to search the register contents.  For quick access
to registers it is still recommended to use the register functions
`consult-register-load' and `consult-register-store' or the built-in
built-in register access functions.  The command supports narrowing, see
`consult-register--narrow'.  Marker positions are previewed.  See
`jump-to-register' and `insert-register' for the meaning of prefix ARG.

(fn &optional ARG)" t) (autoload 'consult-register-load "consult-register" "Do what I mean with a REG.

For a window configuration, restore it.  For a number or text, insert it.
For a location, jump to it.  See `jump-to-register' and `insert-register'
for the meaning of prefix ARG.

(fn REG &optional ARG)" t) (autoload 'consult-register-store "consult-register" "Store register dependent on current context, showing an action menu.

With an active region, store/append/prepend the contents, optionally
deleting the region when a prefix ARG is given.  With a numeric prefix
ARG, store or add the number.  Otherwise store point, frameset, window or
kmacro.

(fn ARG)" t) (register-definition-prefixes "consult-register" '("consult-register-")) (autoload 'consult-xref "consult-xref" "Show xrefs with preview in the minibuffer.

This function can be used for `xref-show-xrefs-function'.
See `xref-show-xrefs-function' for the description of the
FETCHER and ALIST arguments.

(fn FETCHER &optional ALIST)") (register-definition-prefixes "consult-xref" '("consult-xref--")) (provide 'consult-autoloads)) "embark-consult" ((embark-consult-autoloads embark-consult) (register-definition-prefixes "embark-consult" '("embark-consult-")) (provide 'embark-consult-autoloads)) "marginalia" ((marginalia marginalia-autoloads) (defvar marginalia-mode nil "Non-nil if Marginalia mode is enabled.
See the `marginalia-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `marginalia-mode'.") (custom-autoload 'marginalia-mode "marginalia" nil) (autoload 'marginalia-mode "marginalia" "Annotate completion candidates with richer information.

This is a global minor mode.  If called interactively, toggle the
`Marginalia mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='marginalia-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'marginalia-cycle "marginalia" "Cycle between annotators in `marginalia-annotator-registry'." t) (register-definition-prefixes "marginalia" '("marginalia-")) (provide 'marginalia-autoloads)) "corfu" ((corfu-info corfu-echo corfu-autoloads corfu-indexed corfu-popupinfo corfu-history corfu corfu-quick) (autoload 'corfu-mode "corfu" "COmpletion in Region FUnction.

This is a minor mode.  If called interactively, toggle the `Corfu
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `corfu-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'global-corfu-mode 'globalized-minor-mode t) (defvar global-corfu-mode nil "Non-nil if Global Corfu mode is enabled.
See the `global-corfu-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-corfu-mode'.") (custom-autoload 'global-corfu-mode "corfu" nil) (autoload 'global-corfu-mode "corfu" "Toggle Corfu mode in all buffers.
With prefix ARG, enable Global Corfu mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Corfu mode is enabled in all buffers where `corfu--on' would do it.

See `corfu-mode' for more information on Corfu mode.

(fn &optional ARG)" t) (register-definition-prefixes "corfu" '("corfu-" "global-corfu-modes")) (defvar corfu-echo-mode nil "Non-nil if Corfu-Echo mode is enabled.
See the `corfu-echo-mode' command
for a description of this minor mode.") (custom-autoload 'corfu-echo-mode "corfu-echo" nil) (autoload 'corfu-echo-mode "corfu-echo" "Show candidate documentation in echo area.

This is a global minor mode.  If called interactively, toggle the
`Corfu-Echo mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='corfu-echo-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "corfu-echo" '("corfu-echo-")) (defvar corfu-history-mode nil "Non-nil if Corfu-History mode is enabled.
See the `corfu-history-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `corfu-history-mode'.") (custom-autoload 'corfu-history-mode "corfu-history" nil) (autoload 'corfu-history-mode "corfu-history" "Update Corfu history and sort completions by history.

This is a global minor mode.  If called interactively, toggle the
`Corfu-History mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='corfu-history-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "corfu-history" '("corfu-history")) (defvar corfu-indexed-mode nil "Non-nil if Corfu-Indexed mode is enabled.
See the `corfu-indexed-mode' command
for a description of this minor mode.") (custom-autoload 'corfu-indexed-mode "corfu-indexed" nil) (autoload 'corfu-indexed-mode "corfu-indexed" "Prefix candidates with indices.

This is a global minor mode.  If called interactively, toggle the
`Corfu-Indexed mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='corfu-indexed-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "corfu-indexed" '("corfu-indexed-")) (autoload 'corfu-info-documentation "corfu-info" "Show documentation of current candidate.
If called with a prefix ARG, the buffer is persistent.

(fn &optional ARG)" t) (autoload 'corfu-info-location "corfu-info" "Show location of current candidate.
If called with a prefix ARG, the buffer is persistent.

(fn &optional ARG)" t) (register-definition-prefixes "corfu-info" '("corfu-info--")) (defvar corfu-popupinfo-mode nil "Non-nil if Corfu-Popupinfo mode is enabled.
See the `corfu-popupinfo-mode' command
for a description of this minor mode.") (custom-autoload 'corfu-popupinfo-mode "corfu-popupinfo" nil) (autoload 'corfu-popupinfo-mode "corfu-popupinfo" "Corfu info popup minor mode.

This is a global minor mode.  If called interactively, toggle the
`Corfu-Popupinfo mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='corfu-popupinfo-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "corfu-popupinfo" '("corfu-popupinfo-")) (autoload 'corfu-quick-jump "corfu-quick" "Jump to candidate using quick keys." t) (autoload 'corfu-quick-insert "corfu-quick" "Insert candidate using quick keys." t) (autoload 'corfu-quick-complete "corfu-quick" "Complete candidate using quick keys." t) (register-definition-prefixes "corfu-quick" '("corfu-quick")) (provide 'corfu-autoloads)) "transient" ((transient transient-autoloads) (autoload 'transient-insert-suffix "transient" "Insert a SUFFIX into PREFIX before LOC.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
Remove a conflicting binding unless optional KEEP-OTHER is
  non-nil.
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC SUFFIX &optional KEEP-OTHER)") (function-put 'transient-insert-suffix 'lisp-indent-function 'defun) (autoload 'transient-append-suffix "transient" "Insert a SUFFIX into PREFIX after LOC.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
Remove a conflicting binding unless optional KEEP-OTHER is
  non-nil.
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC SUFFIX &optional KEEP-OTHER)") (function-put 'transient-append-suffix 'lisp-indent-function 'defun) (autoload 'transient-replace-suffix "transient" "Replace the suffix at LOC in PREFIX with SUFFIX.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC SUFFIX)") (function-put 'transient-replace-suffix 'lisp-indent-function 'defun) (autoload 'transient-remove-suffix "transient" "Remove the suffix or group at LOC in PREFIX.
PREFIX is a prefix command, a symbol.
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC)") (function-put 'transient-remove-suffix 'lisp-indent-function 'defun) (register-definition-prefixes "transient" '("transient")) (provide 'transient-autoloads)) "with-editor" ((with-editor-autoloads with-editor) (autoload 'with-editor-export-editor "with-editor" "Teach subsequent commands to use current Emacs instance as editor.

Set and export the environment variable ENVVAR, by default
\"EDITOR\".  The value is automatically generated to teach
commands to use the current Emacs instance as \"the editor\".

This works in `shell-mode', `term-mode', `eshell-mode' and
`vterm'.

(fn &optional (ENVVAR \"EDITOR\"))" t) (autoload 'with-editor-export-git-editor "with-editor" "Like `with-editor-export-editor' but always set `$GIT_EDITOR'." t) (autoload 'with-editor-export-hg-editor "with-editor" "Like `with-editor-export-editor' but always set `$HG_EDITOR'." t) (defvar shell-command-with-editor-mode nil "Non-nil if Shell-Command-With-Editor mode is enabled.
See the `shell-command-with-editor-mode' command
for a description of this minor mode.") (custom-autoload 'shell-command-with-editor-mode "with-editor" nil) (autoload 'shell-command-with-editor-mode "with-editor" "Teach `shell-command' to use current Emacs instance as editor.

Teach `shell-command', and all commands that ultimately call that
command, to use the current Emacs instance as editor by executing
\"EDITOR=CLIENT COMMAND&\" instead of just \"COMMAND&\".

CLIENT is automatically generated; EDITOR=CLIENT instructs
COMMAND to use to the current Emacs instance as \"the editor\",
assuming no other variable overrides the effect of \"$EDITOR\".
CLIENT may be the path to an appropriate emacsclient executable
with arguments, or a script which also works over Tramp.

Alternatively you can use the `with-editor-async-shell-command',
which also allows the use of another variable instead of
\"EDITOR\".

This is a global minor mode.  If called interactively, toggle the
`Shell-Command-With-Editor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='shell-command-with-editor-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'with-editor-async-shell-command "with-editor" "Like `async-shell-command' but with `$EDITOR' set.

Execute string \"ENVVAR=CLIENT COMMAND\" in an inferior shell;
display output, if any.  With a prefix argument prompt for an
environment variable, otherwise the default \"EDITOR\" variable
is used.  With a negative prefix argument additionally insert
the COMMAND's output at point.

CLIENT is automatically generated; ENVVAR=CLIENT instructs
COMMAND to use to the current Emacs instance as \"the editor\",
assuming it respects ENVVAR as an \"EDITOR\"-like variable.
CLIENT may be the path to an appropriate emacsclient executable
with arguments, or a script which also works over Tramp.

Also see `async-shell-command' and `shell-command'.

(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER ENVVAR)" t) (autoload 'with-editor-shell-command "with-editor" "Like `shell-command' or `with-editor-async-shell-command'.
If COMMAND ends with \"&\" behave like the latter,
else like the former.

(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER ENVVAR)" t) (register-definition-prefixes "with-editor" '("server-" "shell-command--shell-command-with-editor-mode" "start-file-process--with-editor-process-filter" "with-editor")) (provide 'with-editor-autoloads)) "git-commit" ((git-commit git-commit-pkg git-commit-autoloads) (put 'git-commit-major-mode 'safe-local-variable (lambda (val) (memq val '(text-mode markdown-mode org-mode fundamental-mode git-commit-elisp-text-mode)))) (register-definition-prefixes "git-commit" '("git-commit-" "global-git-commit-mode")) (provide 'git-commit-autoloads)) "magit" ((magit-status magit-blame magit-sequence magit-subtree magit-pull magit-autoloads magit-wip magit-repos magit-sparse-checkout magit-pkg magit-apply magit-core magit-clone magit-autorevert magit-tag magit-refs magit-diff magit-margin magit-commit magit-git magit-branch magit-patch magit-fetch magit-remote magit-files magit-mode magit-merge magit-ediff magit-log magit git-rebase magit-push magit-transient magit-bisect magit-process magit-stash magit-base magit-notes magit-bookmark magit-submodule magit-worktree magit-reflog magit-gitignore magit-extras magit-bundle magit-reset) (autoload 'git-rebase-current-line "git-rebase" "Parse current line into a `git-rebase-action' instance.
If the current line isn't recognized as a rebase line, an
instance with all nil values is returned.") (autoload 'git-rebase-mode "git-rebase" "Major mode for editing of a Git rebase file.

Rebase files are generated when you run \"git rebase -i\" or run
`magit-interactive-rebase'.  They describe how Git should perform
the rebase.  See the documentation for git-rebase (e.g., by
running \"man git-rebase\" at the command line) for details.

(fn)" t) (defconst git-rebase-filename-regexp "/git-rebase-todo\\'") (add-to-list 'auto-mode-alist (cons git-rebase-filename-regexp #'git-rebase-mode)) (register-definition-prefixes "git-rebase" '("git-rebase-" "magit-imenu--rebase-")) (defvar magit-define-global-key-bindings 'default "Which set of key bindings to add to the global keymap, if any.

This option controls which set of Magit key bindings, if any, may
be added to the global keymap, even before Magit is first used in
the current Emacs session.

If the value is nil, no bindings are added.

If `default', maybe add:

    C-x g     `magit-status'
    C-x M-g   `magit-dispatch'
    C-c M-g   `magit-file-dispatch'

If `recommended', maybe add:

    C-x g     `magit-status'
    C-c g     `magit-dispatch'
    C-c f     `magit-file-dispatch'

    These bindings are strongly recommended, but we cannot use
    them by default, because the \"C-c <LETTER>\" namespace is
    strictly reserved for bindings added by the user.

The bindings in the chosen set may be added when
`after-init-hook' is run.  Each binding is added if, and only
if, at that time no other key is bound to the same command,
and no other command is bound to the same key.  In other words
we try to avoid adding bindings that are unnecessary, as well
as bindings that conflict with other bindings.

Adding these bindings is delayed until `after-init-hook' is
run to allow users to set the variable anywhere in their init
file (without having to make sure to do so before `magit' is
loaded or autoloaded) and to increase the likelihood that all
the potentially conflicting user bindings have already been
added.

To set this variable use either `setq' or the Custom interface.
Do not use the function `customize-set-variable' because doing
that would cause Magit to be loaded immediately, when that form
is evaluated (this differs from `custom-set-variables', which
doesn't load the libraries that define the customized variables).

Setting this variable has no effect if `after-init-hook' has
already been run.") (custom-autoload 'magit-define-global-key-bindings "magit" t) (defun magit-maybe-define-global-key-bindings (&optional force) "See variable `magit-define-global-key-bindings'." (when magit-define-global-key-bindings (let ((map (current-global-map))) (pcase-dolist (`(,key \, def) (cond ((eq magit-define-global-key-bindings 'recommended) '(("C-x g" . magit-status) ("C-c g" . magit-dispatch) ("C-c f" . magit-file-dispatch))) ('(("C-x g" . magit-status) ("C-x M-g" . magit-dispatch) ("C-c M-g" . magit-file-dispatch))))) (when (or force (not (or (lookup-key map (kbd key)) (where-is-internal def (make-sparse-keymap) t)))) (define-key map (kbd key) def)))))) (if after-init-time (magit-maybe-define-global-key-bindings) (add-hook 'after-init-hook #'magit-maybe-define-global-key-bindings t)) (autoload 'magit-dispatch "magit" nil t) (autoload 'magit-run "magit" nil t) (autoload 'magit-git-command "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer. \"git \" is
used as initial input, but can be deleted to run another command.

With a prefix argument COMMAND is run in the top-level directory
of the current working tree, otherwise in `default-directory'.

(fn COMMAND)" t) (autoload 'magit-git-command-topdir "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer. \"git \" is
used as initial input, but can be deleted to run another command.

COMMAND is run in the top-level directory of the current
working tree.

(fn COMMAND)" t) (autoload 'magit-shell-command "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer.  With a
prefix argument COMMAND is run in the top-level directory of
the current working tree, otherwise in `default-directory'.

(fn COMMAND)" t) (autoload 'magit-shell-command-topdir "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer.  COMMAND
is run in the top-level directory of the current working tree.

(fn COMMAND)" t) (autoload 'magit-version "magit" "Return the version of Magit currently in use.

If optional argument PRINT-DEST is non-nil, also print the used
versions of Magit, Transient, Git and Emacs to the output stream
selected by that argument.  Interactively use the echo area, or
with a prefix argument use the current buffer.  Additionally put
the output in the kill ring.

(fn &optional PRINT-DEST)" t) (register-definition-prefixes "magit" '("magit-")) (autoload 'magit-stage-buffer-file "magit-apply" "Stage all changes to the file being visited in the current buffer." t) (autoload 'magit-stage-file "magit-apply" "Read one or more files and stage all changes in those files.
With prefix argument FORCE, offer ignored files for completion.

(fn FILES &optional FORCE)" t) (autoload 'magit-stage-modified "magit-apply" "Stage all changes to files modified in the worktree.
Stage all new content of tracked files and remove tracked files
that no longer exist in the working tree from the index also.
With a prefix argument also stage previously untracked (but not
ignored) files.

(fn &optional ALL)" t) (autoload 'magit-unstage-buffer-file "magit-apply" "Unstage all changes to the file being visited in the current buffer." t) (autoload 'magit-unstage-file "magit-apply" "Read one or more files and unstage all changes to those files.

(fn FILES)" t) (autoload 'magit-unstage-all "magit-apply" "Remove all changes from the staging area." t) (register-definition-prefixes "magit-apply" '("magit-")) (put 'magit-auto-revert-mode 'globalized-minor-mode t) (defvar magit-auto-revert-mode (not (or global-auto-revert-mode noninteractive)) "Non-nil if Magit-Auto-Revert mode is enabled.
See the `magit-auto-revert-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-auto-revert-mode'.") (custom-autoload 'magit-auto-revert-mode "magit-autorevert" nil) (autoload 'magit-auto-revert-mode "magit-autorevert" "Toggle Auto-Revert mode in all buffers.
With prefix ARG, enable Magit-Auto-Revert mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Auto-Revert mode is enabled in all buffers where
`magit-turn-on-auto-revert-mode-if-desired' would do it.

See `auto-revert-mode' for more information on Auto-Revert mode.

(fn &optional ARG)" t) (register-definition-prefixes "magit-autorevert" '("auto-revert-buffer" "magit-")) (autoload 'magit-emacs-Q-command "magit-base" "Show a shell command that runs an uncustomized Emacs with only Magit loaded.
See info node `(magit)Debugging Tools' for more information." t) (autoload 'Info-follow-nearest-node--magit-gitman "magit-base" "

(fn FN &optional FORK)") (advice-add 'Info-follow-nearest-node :around #'Info-follow-nearest-node--magit-gitman) (advice-add 'org-man-export :around #'org-man-export--magit-gitman) (autoload 'org-man-export--magit-gitman "magit-base" "

(fn FN LINK DESCRIPTION FORMAT)") (register-definition-prefixes "magit-base" '("magit-")) (autoload 'magit-bisect "magit-bisect" nil t) (autoload 'magit-bisect-start "magit-bisect" "Start a bisect session.

Bisecting a bug means to find the commit that introduced it.
This command starts such a bisect session by asking for a known
good and a known bad commit.  To move the session forward use the
other actions from the bisect transient command (\\<magit-status-mode-map>\\[magit-bisect]).

(fn BAD GOOD ARGS)" t) (autoload 'magit-bisect-reset "magit-bisect" "After bisecting, cleanup bisection state and return to original `HEAD'." t) (autoload 'magit-bisect-good "magit-bisect" "While bisecting, mark the current commit as good.
Use this after you have asserted that the commit does not contain
the bug in question." t) (autoload 'magit-bisect-bad "magit-bisect" "While bisecting, mark the current commit as bad.
Use this after you have asserted that the commit does contain the
bug in question." t) (autoload 'magit-bisect-mark "magit-bisect" "While bisecting, mark the current commit with a bisect term.
During a bisect using alternate terms, commits can still be
marked with `magit-bisect-good' and `magit-bisect-bad', as those
commands map to the correct term (\"good\" to --term-old's value
and \"bad\" to --term-new's).  However, in some cases, it can be
difficult to keep that mapping straight in your head; this
command provides an interface that exposes the underlying terms." t) (autoload 'magit-bisect-skip "magit-bisect" "While bisecting, skip the current commit.
Use this if for some reason the current commit is not a good one
to test.  This command lets Git choose a different one." t) (autoload 'magit-bisect-run "magit-bisect" "Bisect automatically by running commands after each step.

Unlike `git bisect run' this can be used before bisecting has
begun.  In that case it behaves like `git bisect start; git
bisect run'.

(fn CMDLINE &optional BAD GOOD ARGS)" t) (register-definition-prefixes "magit-bisect" '("magit-")) (autoload 'magit-blame-echo "magit-blame" nil t) (autoload 'magit-blame-addition "magit-blame" nil t) (autoload 'magit-blame-removal "magit-blame" nil t) (autoload 'magit-blame-reverse "magit-blame" nil t) (autoload 'magit-blame "magit-blame" nil t) (register-definition-prefixes "magit-blame" '("magit-")) (autoload 'magit-branch "magit" nil t) (autoload 'magit-checkout "magit-branch" "Checkout REVISION, updating the index and the working tree.
If REVISION is a local branch, then that becomes the current
branch.  If it is something else, then `HEAD' becomes detached.
Checkout fails if the working tree or the staging area contain
changes.

(git checkout REVISION).

(fn REVISION &optional ARGS)" t) (function-put 'magit-checkout 'interactive-only 'magit--checkout) (autoload 'magit-branch-create "magit-branch" "Create BRANCH at branch or revision START-POINT.

(fn BRANCH START-POINT)" t) (function-put 'magit-branch-create 'interactive-only 'magit-call-git) (autoload 'magit-branch-and-checkout "magit-branch" "Create and checkout BRANCH at branch or revision START-POINT.

(fn BRANCH START-POINT &optional ARGS)" t) (function-put 'magit-branch-and-checkout 'interactive-only 'magit-call-git) (autoload 'magit-branch-or-checkout "magit-branch" "Hybrid between `magit-checkout' and `magit-branch-and-checkout'.

Ask the user for an existing branch or revision.  If the user
input actually can be resolved as a branch or revision, then
check that out, just like `magit-checkout' would.

Otherwise create and checkout a new branch using the input as
its name.  Before doing so read the starting-point for the new
branch.  This is similar to what `magit-branch-and-checkout'
does.

(fn ARG &optional START-POINT)" t) (function-put 'magit-branch-or-checkout 'interactive-only 'magit-call-git) (autoload 'magit-branch-checkout "magit-branch" "Checkout an existing or new local branch.

Read a branch name from the user offering all local branches and
a subset of remote branches as candidates.  Omit remote branches
for which a local branch by the same name exists from the list
of candidates.  The user can also enter a completely new branch
name.

- If the user selects an existing local branch, then check that
  out.

- If the user selects a remote branch, then create and checkout
  a new local branch with the same name.  Configure the selected
  remote branch as push target.

- If the user enters a new branch name, then create and check
  that out, after also reading the starting-point from the user.

In the latter two cases the upstream is also set.  Whether it is
set to the chosen START-POINT or something else depends on the
value of `magit-branch-adjust-remote-upstream-alist', just like
when using `magit-branch-and-checkout'.

(fn BRANCH &optional START-POINT)" t) (function-put 'magit-branch-checkout 'interactive-only 'magit-call-git) (autoload 'magit-branch-orphan "magit-branch" "Create and checkout an orphan BRANCH with contents from revision START-POINT.

(fn BRANCH START-POINT)" t) (autoload 'magit-branch-spinout "magit-branch" "Create new branch from the unpushed commits.
Like `magit-branch-spinoff' but remain on the current branch.
If there are any uncommitted changes, then behave exactly like
`magit-branch-spinoff'.

(fn BRANCH &optional FROM)" t) (autoload 'magit-branch-spinoff "magit-branch" "Create new branch from the unpushed commits.

Create and checkout a new branch starting at and tracking the
current branch.  That branch in turn is reset to the last commit
it shares with its upstream.  If the current branch has no
upstream or no unpushed commits, then the new branch is created
anyway and the previously current branch is not touched.

This is useful to create a feature branch after work has already
began on the old branch (likely but not necessarily \"master\").

If the current branch is a member of the value of option
`magit-branch-prefer-remote-upstream' (which see), then the
current branch will be used as the starting point as usual, but
the upstream of the starting-point may be used as the upstream
of the new branch, instead of the starting-point itself.

If optional FROM is non-nil, then the source branch is reset
to `FROM~', instead of to the last commit it shares with its
upstream.  Interactively, FROM is only ever non-nil, if the
region selects some commits, and among those commits, FROM is
the commit that is the fewest commits ahead of the source
branch.

The commit at the other end of the selection actually does not
matter, all commits between FROM and `HEAD' are moved to the new
branch.  If FROM is not reachable from `HEAD' or is reachable
from the source branch's upstream, then an error is raised.

(fn BRANCH &optional FROM)" t) (autoload 'magit-branch-reset "magit-branch" "Reset a branch to the tip of another branch or any other commit.

When the branch being reset is the current branch, then do a
hard reset.  If there are any uncommitted changes, then the user
has to confirm the reset because those changes would be lost.

This is useful when you have started work on a feature branch but
realize it's all crap and want to start over.

When resetting to another branch and a prefix argument is used,
then also set the target branch as the upstream of the branch
that is being reset.

(fn BRANCH TO &optional SET-UPSTREAM)" t) (autoload 'magit-branch-delete "magit-branch" "Delete one or multiple branches.

If the region marks multiple branches, then offer to delete
those, otherwise prompt for a single branch to be deleted,
defaulting to the branch at point.

Require confirmation when deleting branches is dangerous in some
way.  Option `magit-no-confirm' can be customized to not require
confirmation in certain cases.  See its docstring to learn why
confirmation is required by default in certain cases or if a
prompt is confusing.

(fn BRANCHES &optional FORCE)" t) (autoload 'magit-branch-rename "magit-branch" "Rename the branch named OLD to NEW.

With a prefix argument FORCE, rename even if a branch named NEW
already exists.

If `branch.OLD.pushRemote' is set, then unset it.  Depending on
the value of `magit-branch-rename-push-target' (which see) maybe
set `branch.NEW.pushRemote' and maybe rename the push-target on
the remote.

(fn OLD NEW &optional FORCE)" t) (autoload 'magit-branch-shelve "magit-branch" "Shelve a BRANCH.
Rename \"refs/heads/BRANCH\" to \"refs/shelved/BRANCH\",
and also rename the respective reflog file.

(fn BRANCH)" t) (autoload 'magit-branch-unshelve "magit-branch" "Unshelve a BRANCH
Rename \"refs/shelved/BRANCH\" to \"refs/heads/BRANCH\",
and also rename the respective reflog file.

(fn BRANCH)" t) (autoload 'magit-branch-configure "magit-branch" nil t) (register-definition-prefixes "magit-branch" '("magit-")) (autoload 'magit-bundle "magit-bundle" nil t) (autoload 'magit-bundle-import "magit-bundle" nil t) (autoload 'magit-bundle-create-tracked "magit-bundle" "Create and track a new bundle.

(fn FILE TAG BRANCH REFS ARGS)" t) (autoload 'magit-bundle-update-tracked "magit-bundle" "Update a bundle that is being tracked using TAG.

(fn TAG)" t) (autoload 'magit-bundle-verify "magit-bundle" "Check whether FILE is valid and applies to the current repository.

(fn FILE)" t) (autoload 'magit-bundle-list-heads "magit-bundle" "List the refs in FILE.

(fn FILE)" t) (register-definition-prefixes "magit-bundle" '("magit-")) (autoload 'magit-clone "magit-clone" nil t) (autoload 'magit-clone-regular "magit-clone" "Create a clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.

(fn REPOSITORY DIRECTORY ARGS)" t) (autoload 'magit-clone-shallow "magit-clone" "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
With a prefix argument read the DEPTH of the clone;
otherwise use 1.

(fn REPOSITORY DIRECTORY ARGS DEPTH)" t) (autoload 'magit-clone-shallow-since "magit-clone" "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
Exclude commits before DATE, which is read from the
user.

(fn REPOSITORY DIRECTORY ARGS DATE)" t) (autoload 'magit-clone-shallow-exclude "magit-clone" "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
Exclude commits reachable from EXCLUDE, which is a
branch or tag read from the user.

(fn REPOSITORY DIRECTORY ARGS EXCLUDE)" t) (autoload 'magit-clone-bare "magit-clone" "Create a bare clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.

(fn REPOSITORY DIRECTORY ARGS)" t) (autoload 'magit-clone-mirror "magit-clone" "Create a mirror of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.

(fn REPOSITORY DIRECTORY ARGS)" t) (autoload 'magit-clone-sparse "magit-clone" "Clone REPOSITORY into DIRECTORY and create a sparse checkout.

(fn REPOSITORY DIRECTORY ARGS)" t) (register-definition-prefixes "magit-clone" '("magit-")) (autoload 'magit-commit "magit-commit" nil t) (autoload 'magit-commit-create "magit-commit" "Create a new commit on `HEAD'.
With a prefix argument, amend to the commit at `HEAD' instead.

(git commit [--amend] ARGS)

(fn &optional ARGS)" t) (autoload 'magit-commit-amend "magit-commit" "Amend the last commit.

(git commit --amend ARGS)

(fn &optional ARGS)" t) (autoload 'magit-commit-extend "magit-commit" "Amend the last commit, without editing the message.

With a prefix argument keep the committer date, otherwise change
it.  The option `magit-commit-extend-override-date' can be used
to inverse the meaning of the prefix argument.  
(git commit
--amend --no-edit)

(fn &optional ARGS OVERRIDE-DATE)" t) (autoload 'magit-commit-reword "magit-commit" "Reword the last commit, ignoring staged changes.

With a prefix argument keep the committer date, otherwise change
it.  The option `magit-commit-reword-override-date' can be used
to inverse the meaning of the prefix argument.

Non-interactively respect the optional OVERRIDE-DATE argument
and ignore the option.

(git commit --amend --only)

(fn &optional ARGS OVERRIDE-DATE)" t) (autoload 'magit-commit-fixup "magit-commit" "Create a fixup commit.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

(fn &optional COMMIT ARGS)" t) (autoload 'magit-commit-squash "magit-commit" "Create a squash commit, without editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

If you want to immediately add a message to the squash commit,
then use `magit-commit-augment' instead of this command.

(fn &optional COMMIT ARGS)" t) (autoload 'magit-commit-augment "magit-commit" "Create a squash commit, editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

(fn &optional COMMIT ARGS)" t) (autoload 'magit-commit-instant-fixup "magit-commit" "Create a fixup commit targeting COMMIT and instantly rebase.

(fn &optional COMMIT ARGS)" t) (autoload 'magit-commit-instant-squash "magit-commit" "Create a squash commit targeting COMMIT and instantly rebase.

(fn &optional COMMIT ARGS)" t) (autoload 'magit-commit-reshelve "magit-commit" "Change the committer date and possibly the author date of `HEAD'.

The current time is used as the initial minibuffer input and the
original author or committer date is available as the previous
history element.

Both the author and the committer dates are changed, unless one
of the following is true, in which case only the committer date
is updated:
- You are not the author of the commit that is being reshelved.
- The command was invoked with a prefix argument.
- Non-interactively if UPDATE-AUTHOR is nil.

(fn DATE UPDATE-AUTHOR &optional ARGS)" t) (autoload 'magit-commit-absorb-modules "magit-commit" "Spread modified modules across recent commits.

(fn PHASE COMMIT)" t) (autoload 'magit-commit-absorb "magit-commit" nil t) (autoload 'magit-commit-autofixup "magit-commit" nil t) (register-definition-prefixes "magit-commit" '("magit-")) (autoload 'magit-diff "magit-diff" nil t) (autoload 'magit-diff-refresh "magit-diff" nil t) (autoload 'magit-diff-dwim "magit-diff" "Show changes for the thing at point.

(fn &optional ARGS FILES)" t) (autoload 'magit-diff-range "magit-diff" "Show differences between two commits.

REV-OR-RANGE should be a range or a single revision.  If it is a
revision, then show changes in the working tree relative to that
revision.  If it is a range, but one side is omitted, then show
changes relative to `HEAD'.

If the region is active, use the revisions on the first and last
line of the region as the two sides of the range.  With a prefix
argument, instead of diffing the revisions, choose a revision to
view changes along, starting at the common ancestor of both
revisions (i.e., use a \"...\" range).

(fn REV-OR-RANGE &optional ARGS FILES)" t) (autoload 'magit-diff-working-tree "magit-diff" "Show changes between the current working tree and the `HEAD' commit.
With a prefix argument show changes between the working tree and
a commit read from the minibuffer.

(fn &optional REV ARGS FILES)" t) (autoload 'magit-diff-staged "magit-diff" "Show changes between the index and the `HEAD' commit.
With a prefix argument show changes between the index and
a commit read from the minibuffer.

(fn &optional REV ARGS FILES)" t) (autoload 'magit-diff-unstaged "magit-diff" "Show changes between the working tree and the index.

(fn &optional ARGS FILES)" t) (autoload 'magit-diff-unmerged "magit-diff" "Show changes that are being merged.

(fn &optional ARGS FILES)" t) (autoload 'magit-diff-while-committing "magit-diff" "While committing, show the changes that are about to be committed.
While amending, invoking the command again toggles between
showing just the new changes or all the changes that will
be committed." t) (autoload 'magit-diff-buffer-file "magit-diff" "Show diff for the blob or file visited in the current buffer.

When the buffer visits a blob, then show the respective commit.
When the buffer visits a file, then show the differences between
`HEAD' and the working tree.  In both cases limit the diff to
the file or blob." t) (autoload 'magit-diff-paths "magit-diff" "Show changes between any two files on disk.

(fn A B)" t) (autoload 'magit-show-commit "magit-diff" "Visit the revision at point in another buffer.
If there is no revision at point or with a prefix argument prompt
for a revision.

(fn REV &optional ARGS FILES MODULE)" t) (register-definition-prefixes "magit-diff" '("magit-")) (autoload 'magit-ediff "magit-ediff" nil) (autoload 'magit-ediff-resolve-all "magit-ediff" "Resolve all conflicts in the FILE at point using Ediff.

If there is no file at point or if it doesn't have any unmerged
changes, then prompt for a file.

See info node `(magit) Ediffing' for more information about this
and alternative commands.

(fn FILE)" t) (autoload 'magit-ediff-resolve-rest "magit-ediff" "Resolve outstanding conflicts in the FILE at point using Ediff.

If there is no file at point or if it doesn't have any unmerged
changes, then prompt for a file.

See info node `(magit) Ediffing' for more information about this
and alternative commands.

(fn FILE)" t) (autoload 'magit-ediff-stage "magit-ediff" "Stage and unstage changes to FILE using Ediff.
FILE has to be relative to the top directory of the repository.

(fn FILE)" t) (autoload 'magit-ediff-compare "magit-ediff" "Compare REVA:FILEA with REVB:FILEB using Ediff.

FILEA and FILEB have to be relative to the top directory of the
repository.  If REVA or REVB is nil, then this stands for the
working tree state.

If the region is active, use the revisions on the first and last
line of the region.  With a prefix argument, instead of diffing
the revisions, choose a revision to view changes along, starting
at the common ancestor of both revisions (i.e., use a \"...\"
range).

(fn REVA REVB FILEA FILEB)" t) (autoload 'magit-ediff-dwim "magit-ediff" "Compare, stage, or resolve using Ediff.
This command tries to guess what file, and what commit or range
the user wants to compare, stage, or resolve using Ediff.  It
might only be able to guess either the file, or range or commit,
in which case the user is asked about the other.  It might not
always guess right, in which case the appropriate `magit-ediff-*'
command has to be used explicitly.  If it cannot read the user's
mind at all, then it asks the user for a command to run." t) (autoload 'magit-ediff-show-staged "magit-ediff" "Show staged changes using Ediff.

This only allows looking at the changes; to stage, unstage,
and discard changes using Ediff, use `magit-ediff-stage'.

FILE must be relative to the top directory of the repository.

(fn FILE)" t) (autoload 'magit-ediff-show-unstaged "magit-ediff" "Show unstaged changes using Ediff.

This only allows looking at the changes; to stage, unstage,
and discard changes using Ediff, use `magit-ediff-stage'.

FILE must be relative to the top directory of the repository.

(fn FILE)" t) (autoload 'magit-ediff-show-working-tree "magit-ediff" "Show changes between `HEAD' and working tree using Ediff.
FILE must be relative to the top directory of the repository.

(fn FILE)" t) (autoload 'magit-ediff-show-commit "magit-ediff" "Show changes introduced by COMMIT using Ediff.

(fn COMMIT)" t) (autoload 'magit-ediff-show-stash "magit-ediff" "Show changes introduced by STASH using Ediff.
`magit-ediff-show-stash-with-index' controls whether a
three-buffer Ediff is used in order to distinguish changes in the
stash that were staged.

(fn STASH)" t) (register-definition-prefixes "magit-ediff" '("magit-ediff-")) (autoload 'magit-git-mergetool "magit-extras" nil t) (autoload 'magit-run-git-gui-blame "magit-extras" "Run `git gui blame' on the given FILENAME and COMMIT.
Interactively run it for the current file and the `HEAD', with a
prefix or when the current file cannot be determined let the user
choose.  When the current buffer is visiting FILENAME instruct
blame to center around the line point is on.

(fn COMMIT FILENAME &optional LINENUM)" t) (autoload 'magit-run-git-gui "magit-extras" "Run `git gui' for the current git repository." t) (autoload 'magit-run-gitk "magit-extras" "Run `gitk' in the current repository." t) (autoload 'magit-run-gitk-branches "magit-extras" "Run `gitk --branches' in the current repository." t) (autoload 'magit-run-gitk-all "magit-extras" "Run `gitk --all' in the current repository." t) (autoload 'ido-enter-magit-status "magit-extras" "Drop into `magit-status' from file switching.

This command does not work in Emacs 26.1.
See https://github.com/magit/magit/issues/3634
and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31707.

To make this command available use something like:

  (add-hook \\='ido-setup-hook
            (lambda ()
              (keymap-set ido-completion-map
                          \"C-x g\" \\='ido-enter-magit-status)))

Starting with Emacs 25.1 the Ido keymaps are defined just once
instead of every time Ido is invoked, so now you can modify it
like pretty much every other keymap:

  (keymap-set ido-common-completion-map
              \"C-x g\" \\='ido-enter-magit-status)" t) (autoload 'magit-project-status "magit-extras" "Run `magit-status' in the current project's root." t) (autoload 'magit-dired-jump "magit-extras" "Visit file at point using Dired.
With a prefix argument, visit in another window.  If there
is no file at point, then instead visit `default-directory'.

(fn &optional OTHER-WINDOW)" t) (autoload 'magit-dired-log "magit-extras" "Show log for all marked files, or the current file.

(fn &optional FOLLOW)" t) (autoload 'magit-dired-am-apply-patches "magit-extras" "In Dired, apply the marked (or next ARG) files as patches.
If inside a repository, then apply in that.  Otherwise prompt
for a repository.

(fn REPO &optional ARG)" t) (autoload 'magit-do-async-shell-command "magit-extras" "Open FILE with `dired-do-async-shell-command'.
Interactively, open the file at point.

(fn FILE)" t) (autoload 'magit-previous-line "magit-extras" "Like `previous-line' but with Magit-specific shift-selection.

Magit's selection mechanism is based on the region but selects an
area that is larger than the region.  This causes `previous-line'
when invoked while holding the shift key to move up one line and
thereby select two lines.  When invoked inside a hunk body this
command does not move point on the first invocation and thereby
it only selects a single line.  Which inconsistency you prefer
is a matter of preference.

(fn &optional ARG TRY-VSCROLL)" t) (function-put 'magit-previous-line 'interactive-only '"use `forward-line' with negative argument instead.") (autoload 'magit-next-line "magit-extras" "Like `next-line' but with Magit-specific shift-selection.

Magit's selection mechanism is based on the region but selects
an area that is larger than the region.  This causes `next-line'
when invoked while holding the shift key to move down one line
and thereby select two lines.  When invoked inside a hunk body
this command does not move point on the first invocation and
thereby it only selects a single line.  Which inconsistency you
prefer is a matter of preference.

(fn &optional ARG TRY-VSCROLL)" t) (function-put 'magit-next-line 'interactive-only 'forward-line) (autoload 'magit-clean "magit-extras" "Remove untracked files from the working tree.
With a prefix argument also remove ignored files,
with two prefix arguments remove ignored files only.

(git clean -f -d [-x|-X])

(fn &optional ARG)" t) (autoload 'magit-generate-changelog "magit-extras" "Insert ChangeLog entries into the current buffer.

The entries are generated from the diff being committed.
If prefix argument, AMENDING, is non-nil, include changes
in HEAD as well as staged changes in the diff to check.

(fn &optional AMENDING)" t) (autoload 'magit-add-change-log-entry "magit-extras" "Find change log file and add date entry and item for current change.
This differs from `add-change-log-entry' (which see) in that
it acts on the current hunk in a Magit buffer instead of on
a position in a file-visiting buffer.

(fn &optional WHOAMI FILE-NAME OTHER-WINDOW)" t) (autoload 'magit-add-change-log-entry-other-window "magit-extras" "Find change log file in other window and add entry and item.
This differs from `add-change-log-entry-other-window' (which see)
in that it acts on the current hunk in a Magit buffer instead of
on a position in a file-visiting buffer.

(fn &optional WHOAMI FILE-NAME)" t) (autoload 'magit-edit-line-commit "magit-extras" "Edit the commit that added the current line.

With a prefix argument edit the commit that removes the line,
if any.  The commit is determined using `git blame' and made
editable using `git rebase --interactive' if it is reachable
from `HEAD', or by checking out the commit (or a branch that
points at it) otherwise.

(fn &optional TYPE)" t) (autoload 'magit-diff-edit-hunk-commit "magit-extras" "From a hunk, edit the respective commit and visit the file.

First visit the file being modified by the hunk at the correct
location using `magit-diff-visit-file'.  This actually visits a
blob.  When point is on a diff header, not within an individual
hunk, then this visits the blob the first hunk is about.

Then invoke `magit-edit-line-commit', which uses an interactive
rebase to make the commit editable, or if that is not possible
because the commit is not reachable from `HEAD' by checking out
that commit directly.  This also causes the actual worktree file
to be visited.

Neither the blob nor the file buffer are killed when finishing
the rebase.  If that is undesirable, then it might be better to
use `magit-rebase-edit-commit' instead of this command.

(fn FILE)" t) (autoload 'magit-reshelve-since "magit-extras" "Change the author and committer dates of the commits since REV.

Ask the user for the first reachable commit whose dates should
be changed.  Then read the new date for that commit.  The initial
minibuffer input and the previous history element offer good
values.  The next commit will be created one minute later and so
on.

This command is only intended for interactive use and should only
be used on highly rearranged and unpublished history.

If KEYID is non-nil, then use that to sign all reshelved commits.
Interactively use the value of the \"--gpg-sign\" option in the
list returned by `magit-rebase-arguments'.

(fn REV KEYID)" t) (autoload 'magit-pop-revision-stack "magit-extras" "Insert a representation of a revision into the current buffer.

Pop a revision from the `magit-revision-stack' and insert it into
the current buffer according to `magit-pop-revision-stack-format'.
Revisions can be put on the stack using `magit-copy-section-value'
and `magit-copy-buffer-revision'.

If the stack is empty or with a prefix argument, instead read a
revision in the minibuffer.  By using the minibuffer history this
allows selecting an item which was popped earlier or to insert an
arbitrary reference or revision without first pushing it onto the
stack.

When reading the revision from the minibuffer, then it might not
be possible to guess the correct repository.  When this command
is called inside a repository (e.g., while composing a commit
message), then that repository is used.  Otherwise (e.g., while
composing an email) then the repository recorded for the top
element of the stack is used (even though we insert another
revision).  If not called inside a repository and with an empty
stack, or with two prefix arguments, then read the repository in
the minibuffer too.

(fn REV TOPLEVEL)" t) (autoload 'magit-copy-section-value "magit-extras" "Save the value of the current section for later use.

Save the section value to the `kill-ring', and, provided that
the current section is a commit, branch, or tag section, push
the (referenced) revision to the `magit-revision-stack' for use
with `magit-pop-revision-stack'.

When `magit-copy-revision-abbreviated' is non-nil, save the
abbreviated revision to the `kill-ring' and the
`magit-revision-stack'.

When the current section is a branch or a tag, and a prefix
argument is used, then save the revision at its tip to the
`kill-ring' instead of the reference name.

When the region is active, then save that to the `kill-ring',
like `kill-ring-save' would, instead of behaving as described
above.  If a prefix argument is used and the region is within
a hunk, then strip the diff marker column and keep only either
the added or removed lines, depending on the sign of the prefix
argument.

(fn ARG)" t) (autoload 'magit-copy-buffer-revision "magit-extras" "Save the revision of the current buffer for later use.

Save the revision shown in the current buffer to the `kill-ring'
and push it to the `magit-revision-stack'.

This command is mainly intended for use in `magit-revision-mode'
buffers, the only buffers where it is always unambiguous exactly
which revision should be saved.

Most other Magit buffers usually show more than one revision, in
some way or another, so this command has to select one of them,
and that choice might not always be the one you think would have
been the best pick.

In such buffers it is often more useful to save the value of
the current section instead, using `magit-copy-section-value'.

When the region is active, then save that to the `kill-ring',
like `kill-ring-save' would, instead of behaving as described
above.

When `magit-copy-revision-abbreviated' is non-nil, save the
abbreviated revision to the `kill-ring' and the
`magit-revision-stack'." t) (autoload 'magit-display-repository-buffer "magit-extras" "Display a Magit buffer belonging to the current Git repository.
The buffer is displayed using `magit-display-buffer', which see.

(fn BUFFER)" t) (autoload 'magit-switch-to-repository-buffer "magit-extras" "Switch to a Magit buffer belonging to the current Git repository.

(fn BUFFER)" t) (autoload 'magit-switch-to-repository-buffer-other-window "magit-extras" "Switch to a Magit buffer belonging to the current Git repository.

(fn BUFFER)" t) (autoload 'magit-switch-to-repository-buffer-other-frame "magit-extras" "Switch to a Magit buffer belonging to the current Git repository.

(fn BUFFER)" t) (autoload 'magit-abort-dwim "magit-extras" "Abort current operation.
Depending on the context, this will abort a merge, a rebase, a
patch application, a cherry-pick, a revert, or a bisect." t) (register-definition-prefixes "magit-extras" '("magit-")) (autoload 'magit-fetch "magit-fetch" nil t) (autoload 'magit-fetch-from-pushremote "magit-fetch" nil t) (autoload 'magit-fetch-from-upstream "magit-fetch" nil t) (autoload 'magit-fetch-other "magit-fetch" "Fetch from another repository.

(fn REMOTE ARGS)" t) (autoload 'magit-fetch-branch "magit-fetch" "Fetch a BRANCH from a REMOTE.

(fn REMOTE BRANCH ARGS)" t) (autoload 'magit-fetch-refspec "magit-fetch" "Fetch a REFSPEC from a REMOTE.

(fn REMOTE REFSPEC ARGS)" t) (autoload 'magit-fetch-all "magit-fetch" "Fetch from all remotes.

(fn ARGS)" t) (autoload 'magit-fetch-all-prune "magit-fetch" "Fetch from all remotes, and prune.
Prune remote tracking branches for branches that have been
removed on the respective remote." t) (autoload 'magit-fetch-all-no-prune "magit-fetch" "Fetch from all remotes." t) (autoload 'magit-fetch-modules "magit-fetch" nil t) (register-definition-prefixes "magit-fetch" '("magit-")) (autoload 'magit-find-file "magit-files" "View FILE from REV.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go
to the line and column corresponding to that location.

(fn REV FILE)" t) (autoload 'magit-find-file-other-window "magit-files" "View FILE from REV, in another window.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go to
the line and column corresponding to that location.

(fn REV FILE)" t) (autoload 'magit-find-file-other-frame "magit-files" "View FILE from REV, in another frame.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go to
the line and column corresponding to that location.

(fn REV FILE)" t) (autoload 'magit-file-dispatch "magit" nil t) (autoload 'magit-blob-visit-file "magit-files" "View the file from the worktree corresponding to the current blob.
When visiting a blob or the version from the index, then go to
the same location in the respective file in the working tree." t) (autoload 'magit-file-checkout "magit-files" "Checkout FILE from REV.

(fn REV FILE)" t) (register-definition-prefixes "magit-files" '("magit-")) (register-definition-prefixes "magit-git" '("magit-")) (autoload 'magit-gitignore "magit-gitignore" nil t) (autoload 'magit-gitignore-in-topdir "magit-gitignore" "Add the Git ignore RULE to the top-level \".gitignore\" file.
Since this file is tracked, it is shared with other clones of the
repository.  Also stage the file.

(fn RULE)" t) (autoload 'magit-gitignore-in-subdir "magit-gitignore" "Add the Git ignore RULE to a \".gitignore\" file in DIRECTORY.
Prompt the user for a directory and add the rule to the
\".gitignore\" file in that directory.  Since such files are
tracked, they are shared with other clones of the repository.
Also stage the file.

(fn RULE DIRECTORY)" t) (autoload 'magit-gitignore-in-gitdir "magit-gitignore" "Add the Git ignore RULE to \"$GIT_DIR/info/exclude\".
Rules in that file only affects this clone of the repository.

(fn RULE)" t) (autoload 'magit-gitignore-on-system "magit-gitignore" "Add the Git ignore RULE to the file specified by `core.excludesFile'.
Rules that are defined in that file affect all local repositories.

(fn RULE)" t) (autoload 'magit-skip-worktree "magit-gitignore" "Call \"git update-index --skip-worktree -- FILE\".

(fn FILE)" t) (autoload 'magit-no-skip-worktree "magit-gitignore" "Call \"git update-index --no-skip-worktree -- FILE\".

(fn FILE)" t) (autoload 'magit-assume-unchanged "magit-gitignore" "Call \"git update-index --assume-unchanged -- FILE\".

(fn FILE)" t) (autoload 'magit-no-assume-unchanged "magit-gitignore" "Call \"git update-index --no-assume-unchanged -- FILE\".

(fn FILE)" t) (register-definition-prefixes "magit-gitignore" '("magit-")) (autoload 'magit-log "magit-log" nil t) (autoload 'magit-log-refresh "magit-log" nil t) (autoload 'magit-log-current "magit-log" "Show log for the current branch.
When `HEAD' is detached or with a prefix argument show log for
one or more revs read from the minibuffer.

(fn REVS &optional ARGS FILES)" t) (autoload 'magit-log-head "magit-log" "Show log for `HEAD'.

(fn &optional ARGS FILES)" t) (autoload 'magit-log-related "magit-log" "Show log for the current branch, its upstream and its push target.
When the upstream is a local branch, then also show its own
upstream.  When `HEAD' is detached, then show log for that, the
previously checked out branch and its upstream and push-target.

(fn REVS &optional ARGS FILES)" t) (autoload 'magit-log-other "magit-log" "Show log for one or more revs read from the minibuffer.
The user can input any revision or revisions separated by a
space, or even ranges, but only branches and tags, and a
representation of the commit at point, are available as
completion candidates.

(fn REVS &optional ARGS FILES)" t) (autoload 'magit-log-branches "magit-log" "Show log for all local branches and `HEAD'.

(fn &optional ARGS FILES)" t) (autoload 'magit-log-matching-branches "magit-log" "Show log for all branches matching PATTERN and `HEAD'.

(fn PATTERN &optional ARGS FILES)" t) (autoload 'magit-log-matching-tags "magit-log" "Show log for all tags matching PATTERN and `HEAD'.

(fn PATTERN &optional ARGS FILES)" t) (autoload 'magit-log-all-branches "magit-log" "Show log for all local and remote branches and `HEAD'.

(fn &optional ARGS FILES)" t) (autoload 'magit-log-all "magit-log" "Show log for all references and `HEAD'.

(fn &optional ARGS FILES)" t) (autoload 'magit-log-buffer-file "magit-log" "Show log for the blob or file visited in the current buffer.
With a prefix argument or when `--follow' is an active log
argument, then follow renames.  When the region is active,
restrict the log to the lines that the region touches.

(fn &optional FOLLOW BEG END)" t) (autoload 'magit-log-trace-definition "magit-log" "Show log for the definition at point.

(fn FILE FN REV)" t) (autoload 'magit-log-merged "magit-log" "Show log for the merge of COMMIT into BRANCH.

More precisely, find merge commit M that brought COMMIT into
BRANCH, and show the log of the range \"M^1..M\". If COMMIT is
directly on BRANCH, then show approximately
`magit-log-merged-commit-count' surrounding commits instead.

This command requires git-when-merged, which is available from
https://github.com/mhagger/git-when-merged.

(fn COMMIT BRANCH &optional ARGS FILES)" t) (autoload 'magit-log-move-to-parent "magit-log" "Move to the Nth parent of the current commit.

(fn &optional N)" t) (autoload 'magit-shortlog "magit-log" nil t) (autoload 'magit-shortlog-since "magit-log" "Show a history summary for commits since REV.

(fn REV ARGS)" t) (autoload 'magit-shortlog-range "magit-log" "Show a history summary for commit or range REV-OR-RANGE.

(fn REV-OR-RANGE ARGS)" t) (autoload 'magit-cherry "magit-log" "Show commits in a branch that are not merged in the upstream branch.

(fn HEAD UPSTREAM)" t) (register-definition-prefixes "magit-log" '("magit-")) (register-definition-prefixes "magit-margin" '("magit-")) (autoload 'magit-merge "magit" nil t) (autoload 'magit-merge-plain "magit-merge" "Merge commit REV into the current branch; using default message.

Unless there are conflicts or a prefix argument is used create a
merge commit using a generic commit message and without letting
the user inspect the result.  With a prefix argument pretend the
merge failed to give the user the opportunity to inspect the
merge.

(git merge --no-edit|--no-commit [ARGS] REV)

(fn REV &optional ARGS NOCOMMIT)" t) (autoload 'magit-merge-editmsg "magit-merge" "Merge commit REV into the current branch; and edit message.
Perform the merge and prepare a commit message but let the user
edit it.

(git merge --edit --no-ff [ARGS] REV)

(fn REV &optional ARGS)" t) (autoload 'magit-merge-nocommit "magit-merge" "Merge commit REV into the current branch; pretending it failed.
Pretend the merge failed to give the user the opportunity to
inspect the merge and change the commit message.

(git merge --no-commit --no-ff [ARGS] REV)

(fn REV &optional ARGS)" t) (autoload 'magit-merge-into "magit-merge" "Merge the current branch into BRANCH and remove the former.

Before merging, force push the source branch to its push-remote,
provided the respective remote branch already exists, ensuring
that the respective pull-request (if any) won't get stuck on some
obsolete version of the commits that are being merged.  Finally
if `forge-branch-pullreq' was used to create the merged branch,
then also remove the respective remote branch.

(fn BRANCH &optional ARGS)" t) (autoload 'magit-merge-absorb "magit-merge" "Merge BRANCH into the current branch and remove the former.

Before merging, force push the source branch to its push-remote,
provided the respective remote branch already exists, ensuring
that the respective pull-request (if any) won't get stuck on some
obsolete version of the commits that are being merged.  Finally
if `forge-branch-pullreq' was used to create the merged branch,
then also remove the respective remote branch.

(fn BRANCH &optional ARGS)" t) (autoload 'magit-merge-squash "magit-merge" "Squash commit REV into the current branch; don't create a commit.

(git merge --squash REV)

(fn REV)" t) (autoload 'magit-merge-preview "magit-merge" "Preview result of merging REV into the current branch.

(fn REV)" t) (autoload 'magit-merge-abort "magit-merge" "Abort the current merge operation.

(git merge --abort)" t) (register-definition-prefixes "magit-merge" '("magit-")) (autoload 'magit-info "magit-mode" "Visit the Magit manual." t) (register-definition-prefixes "magit-mode" '("magit-")) (autoload 'magit-notes "magit" nil t) (register-definition-prefixes "magit-notes" '("magit-notes-")) (autoload 'magit-patch "magit-patch" nil t) (autoload 'magit-patch-create "magit-patch" nil t) (autoload 'magit-patch-apply "magit-patch" nil t) (autoload 'magit-patch-save "magit-patch" "Write current diff into patch FILE.

What arguments are used to create the patch depends on the value
of `magit-patch-save-arguments' and whether a prefix argument is
used.

If the value is the symbol `buffer', then use the same arguments
as the buffer.  With a prefix argument use no arguments.

If the value is a list beginning with the symbol `exclude', then
use the same arguments as the buffer except for those matched by
entries in the cdr of the list.  The comparison is done using
`string-prefix-p'.  With a prefix argument use the same arguments
as the buffer.

If the value is a list of strings (including the empty list),
then use those arguments.  With a prefix argument use the same
arguments as the buffer.

Of course the arguments that are required to actually show the
same differences as those shown in the buffer are always used.

(fn FILE &optional ARG)" t) (autoload 'magit-request-pull "magit-patch" "Request upstream to pull from your public repository.

URL is the url of your publicly accessible repository.
START is a commit that already is in the upstream repository.
END is the last commit, usually a branch name, which upstream
is asked to pull.  START has to be reachable from that commit.

(fn URL START END)" t) (register-definition-prefixes "magit-patch" '("magit-")) (register-definition-prefixes "magit-process" '("magit-" "tramp-sh-handle-")) (autoload 'magit-pull "magit-pull" nil t) (autoload 'magit-pull-from-pushremote "magit-pull" nil t) (autoload 'magit-pull-from-upstream "magit-pull" nil t) (autoload 'magit-pull-branch "magit-pull" "Pull from a branch read in the minibuffer.

(fn SOURCE ARGS)" t) (register-definition-prefixes "magit-pull" '("magit-pull-")) (autoload 'magit-push "magit-push" nil t) (autoload 'magit-push-current-to-pushremote "magit-push" nil t) (autoload 'magit-push-current-to-upstream "magit-push" nil t) (autoload 'magit-push-current "magit-push" "Push the current branch to a branch read in the minibuffer.

(fn TARGET ARGS)" t) (autoload 'magit-push-other "magit-push" "Push an arbitrary branch or commit somewhere.
Both the source and the target are read in the minibuffer.

(fn SOURCE TARGET ARGS)" t) (autoload 'magit-push-refspecs "magit-push" "Push one or multiple REFSPECS to a REMOTE.
Both the REMOTE and the REFSPECS are read in the minibuffer.  To
use multiple REFSPECS, separate them with commas.  Completion is
only available for the part before the colon, or when no colon
is used.

(fn REMOTE REFSPECS ARGS)" t) (autoload 'magit-push-matching "magit-push" "Push all matching branches to another repository.
If multiple remotes exist, then read one from the user.
If just one exists, use that without requiring confirmation.

(fn REMOTE &optional ARGS)" t) (autoload 'magit-push-tags "magit-push" "Push all tags to another repository.
If only one remote exists, then push to that.  Otherwise prompt
for a remote, offering the remote configured for the current
branch as default.

(fn REMOTE &optional ARGS)" t) (autoload 'magit-push-tag "magit-push" "Push a tag to another repository.

(fn TAG REMOTE &optional ARGS)" t) (autoload 'magit-push-notes-ref "magit-push" "Push a notes ref to another repository.

(fn REF REMOTE &optional ARGS)" t) (autoload 'magit-push-implicitly "magit-push" nil t) (autoload 'magit-push-to-remote "magit-push" nil t) (register-definition-prefixes "magit-push" '("magit-")) (autoload 'magit-reflog-current "magit-reflog" "Display the reflog of the current branch.
If `HEAD' is detached, then show the reflog for that instead." t) (autoload 'magit-reflog-other "magit-reflog" "Display the reflog of a branch or another ref.

(fn REF)" t) (autoload 'magit-reflog-head "magit-reflog" "Display the `HEAD' reflog." t) (register-definition-prefixes "magit-reflog" '("magit-reflog-")) (autoload 'magit-show-refs "magit-refs" nil t) (autoload 'magit-show-refs-head "magit-refs" "List and compare references in a dedicated buffer.
Compared with `HEAD'.

(fn &optional ARGS)" t) (autoload 'magit-show-refs-current "magit-refs" "List and compare references in a dedicated buffer.
Compare with the current branch or `HEAD' if it is detached.

(fn &optional ARGS)" t) (autoload 'magit-show-refs-other "magit-refs" "List and compare references in a dedicated buffer.
Compared with a branch read from the user.

(fn &optional REF ARGS)" t) (register-definition-prefixes "magit-refs" '("magit-")) (autoload 'magit-remote "magit-remote" nil t) (autoload 'magit-remote-add "magit-remote" "Add a remote named REMOTE and fetch it.

(fn REMOTE URL &optional ARGS)" t) (autoload 'magit-remote-rename "magit-remote" "Rename the remote named OLD to NEW.

(fn OLD NEW)" t) (autoload 'magit-remote-remove "magit-remote" "Delete the remote named REMOTE.

(fn REMOTE)" t) (autoload 'magit-remote-prune "magit-remote" "Remove stale remote-tracking branches for REMOTE.

(fn REMOTE)" t) (autoload 'magit-remote-prune-refspecs "magit-remote" "Remove stale refspecs for REMOTE.

A refspec is stale if there no longer exists at least one branch
on the remote that would be fetched due to that refspec.  A stale
refspec is problematic because its existence causes Git to refuse
to fetch according to the remaining non-stale refspecs.

If only stale refspecs remain, then offer to either delete the
remote or to replace the stale refspecs with the default refspec.

Also remove the remote-tracking branches that were created due to
the now stale refspecs.  Other stale branches are not removed.

(fn REMOTE)" t) (autoload 'magit-remote-set-head "magit-remote" "Set the local representation of REMOTE's default branch.
Query REMOTE and set the symbolic-ref refs/remotes/<remote>/HEAD
accordingly.  With a prefix argument query for the branch to be
used, which allows you to select an incorrect value if you fancy
doing that.

(fn REMOTE &optional BRANCH)" t) (autoload 'magit-remote-unset-head "magit-remote" "Unset the local representation of REMOTE's default branch.
Delete the symbolic-ref \"refs/remotes/<remote>/HEAD\".

(fn REMOTE)" t) (autoload 'magit-update-default-branch "magit-remote" nil t) (autoload 'magit-remote-unshallow "magit-remote" "Convert a shallow remote into a full one.
If only a single refspec is set and it does not contain a
wildcard, then also offer to replace it with the standard
refspec.

(fn REMOTE)" t) (autoload 'magit-remote-configure "magit-remote" nil t) (register-definition-prefixes "magit-remote" '("magit-")) (autoload 'magit-list-repositories "magit-repos" "Display a list of repositories.

Use the option `magit-repository-directories' to control which
repositories are displayed." t) (register-definition-prefixes "magit-repos" '("magit-")) (autoload 'magit-reset "magit" nil t) (autoload 'magit-reset-mixed "magit-reset" "Reset the `HEAD' and index to COMMIT, but not the working tree.

(git reset --mixed COMMIT)

(fn COMMIT)" t) (autoload 'magit-reset-soft "magit-reset" "Reset the `HEAD' to COMMIT, but not the index and working tree.

(git reset --soft REVISION)

(fn COMMIT)" t) (autoload 'magit-reset-hard "magit-reset" "Reset the `HEAD', index, and working tree to COMMIT.

(git reset --hard REVISION)

(fn COMMIT)" t) (autoload 'magit-reset-keep "magit-reset" "Reset the `HEAD' and index to COMMIT, while keeping uncommitted changes.

(git reset --keep REVISION)

(fn COMMIT)" t) (autoload 'magit-reset-index "magit-reset" "Reset the index to COMMIT.
Keep the `HEAD' and working tree as-is, so if COMMIT refers to the
head this effectively unstages all changes.

(git reset COMMIT .)

(fn COMMIT)" t) (autoload 'magit-reset-worktree "magit-reset" "Reset the worktree to COMMIT.
Keep the `HEAD' and index as-is.

(fn COMMIT)" t) (autoload 'magit-reset-quickly "magit-reset" "Reset the `HEAD' and index to COMMIT, and possibly the working tree.
With a prefix argument reset the working tree otherwise don't.

(git reset --mixed|--hard COMMIT)

(fn COMMIT &optional HARD)" t) (register-definition-prefixes "magit-reset" '("magit-reset-")) (autoload 'magit-sequencer-continue "magit-sequence" "Resume the current cherry-pick or revert sequence." t) (autoload 'magit-sequencer-skip "magit-sequence" "Skip the stopped at commit during a cherry-pick or revert sequence." t) (autoload 'magit-sequencer-abort "magit-sequence" "Abort the current cherry-pick or revert sequence.
This discards all changes made since the sequence started." t) (autoload 'magit-cherry-pick "magit-sequence" nil t) (autoload 'magit-cherry-copy "magit-sequence" "Copy COMMITS from another branch onto the current branch.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then pick all of them,
without prompting.

(fn COMMITS &optional ARGS)" t) (autoload 'magit-cherry-apply "magit-sequence" "Apply the changes in COMMITS but do not commit them.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then apply all of them,
without prompting.

(fn COMMITS &optional ARGS)" t) (autoload 'magit-cherry-harvest "magit-sequence" "Move COMMITS from another BRANCH onto the current branch.
Remove the COMMITS from BRANCH and stay on the current branch.
If a conflict occurs, then you have to fix that and finish the
process manually.

(fn COMMITS BRANCH &optional ARGS)" t) (autoload 'magit-cherry-donate "magit-sequence" "Move COMMITS from the current branch onto another existing BRANCH.
Remove COMMITS from the current branch and stay on that branch.
If a conflict occurs, then you have to fix that and finish the
process manually.  `HEAD' is allowed to be detached initially.

(fn COMMITS BRANCH &optional ARGS)" t) (autoload 'magit-cherry-spinout "magit-sequence" "Move COMMITS from the current branch onto a new BRANCH.
Remove COMMITS from the current branch and stay on that branch.
If a conflict occurs, then you have to fix that and finish the
process manually.

(fn COMMITS BRANCH START-POINT &optional ARGS)" t) (autoload 'magit-cherry-spinoff "magit-sequence" "Move COMMITS from the current branch onto a new BRANCH.
Remove COMMITS from the current branch and checkout BRANCH.
If a conflict occurs, then you have to fix that and finish
the process manually.

(fn COMMITS BRANCH START-POINT &optional ARGS)" t) (autoload 'magit-revert "magit-sequence" nil t) (autoload 'magit-revert-and-commit "magit-sequence" "Revert COMMIT by creating a new commit.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting.

(fn COMMIT &optional ARGS)" t) (autoload 'magit-revert-no-commit "magit-sequence" "Revert COMMIT by applying it in reverse to the worktree.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting.

(fn COMMIT &optional ARGS)" t) (autoload 'magit-am "magit-sequence" nil t) (autoload 'magit-am-apply-patches "magit-sequence" "Apply the patches FILES.

(fn &optional FILES ARGS)" t) (autoload 'magit-am-apply-maildir "magit-sequence" "Apply the patches from MAILDIR.

(fn &optional MAILDIR ARGS)" t) (autoload 'magit-am-continue "magit-sequence" "Resume the current patch applying sequence." t) (autoload 'magit-am-skip "magit-sequence" "Skip the stopped at patch during a patch applying sequence." t) (autoload 'magit-am-abort "magit-sequence" "Abort the current patch applying sequence.
This discards all changes made since the sequence started." t) (autoload 'magit-rebase "magit-sequence" nil t) (autoload 'magit-rebase-onto-pushremote "magit-sequence" nil t) (autoload 'magit-rebase-onto-upstream "magit-sequence" nil t) (autoload 'magit-rebase-branch "magit-sequence" "Rebase the current branch onto a branch read in the minibuffer.
All commits that are reachable from `HEAD' but not from the
selected branch TARGET are being rebased.

(fn TARGET ARGS)" t) (autoload 'magit-rebase-subset "magit-sequence" "Rebase a subset of the current branch's history onto a new base.
Rebase commits from START to `HEAD' onto NEWBASE.
START has to be selected from a list of recent commits.

(fn NEWBASE START ARGS)" t) (autoload 'magit-rebase-interactive "magit-sequence" "Start an interactive rebase sequence.

(fn COMMIT ARGS)" t) (autoload 'magit-rebase-autosquash "magit-sequence" "Combine squash and fixup commits with their intended targets.

(fn ARGS)" t) (autoload 'magit-rebase-edit-commit "magit-sequence" "Edit a single older commit using rebase.

(fn COMMIT ARGS)" t) (autoload 'magit-rebase-reword-commit "magit-sequence" "Reword a single older commit using rebase.

(fn COMMIT ARGS)" t) (autoload 'magit-rebase-remove-commit "magit-sequence" "Remove a single older commit using rebase.

(fn COMMIT ARGS)" t) (autoload 'magit-rebase-continue "magit-sequence" "Restart the current rebasing operation.
In some cases this pops up a commit message buffer for you do
edit.  With a prefix argument the old message is reused as-is.

(fn &optional NOEDIT)" t) (autoload 'magit-rebase-skip "magit-sequence" "Skip the current commit and restart the current rebase operation." t) (autoload 'magit-rebase-edit "magit-sequence" "Edit the todo list of the current rebase operation." t) (autoload 'magit-rebase-abort "magit-sequence" "Abort the current rebase operation, restoring the original branch." t) (register-definition-prefixes "magit-sequence" '("magit-")) (autoload 'magit-sparse-checkout "magit-sparse-checkout" nil t) (autoload 'magit-sparse-checkout-enable "magit-sparse-checkout" "Convert the working tree to a sparse checkout.

(fn &optional ARGS)" t) (autoload 'magit-sparse-checkout-set "magit-sparse-checkout" "Restrict working tree to DIRECTORIES.
To extend rather than override the currently configured
directories, call `magit-sparse-checkout-add' instead.

(fn DIRECTORIES)" t) (autoload 'magit-sparse-checkout-add "magit-sparse-checkout" "Add DIRECTORIES to the working tree.
To override rather than extend the currently configured
directories, call `magit-sparse-checkout-set' instead.

(fn DIRECTORIES)" t) (autoload 'magit-sparse-checkout-reapply "magit-sparse-checkout" "Reapply the sparse checkout rules to the working tree.
Some operations such as merging or rebasing may need to check out
files that aren't included in the sparse checkout.  Call this
command to reset to the sparse checkout state." t) (autoload 'magit-sparse-checkout-disable "magit-sparse-checkout" "Convert sparse checkout to full checkout.
Note that disabling the sparse checkout does not clear the
configured directories.  Call `magit-sparse-checkout-enable' to
restore the previous sparse checkout." t) (register-definition-prefixes "magit-sparse-checkout" '("magit-sparse-checkout-")) (autoload 'magit-stash "magit-stash" nil t) (autoload 'magit-stash-both "magit-stash" "Create a stash of the index and working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn MESSAGE &optional INCLUDE-UNTRACKED)" t) (autoload 'magit-stash-index "magit-stash" "Create a stash of the index only.
Unstaged and untracked changes are not stashed.  The stashed
changes are applied in reverse to both the index and the
worktree.  This command can fail when the worktree is not clean.
Applying the resulting stash has the inverse effect.

(fn MESSAGE)" t) (autoload 'magit-stash-worktree "magit-stash" "Create a stash of unstaged changes in the working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn MESSAGE &optional INCLUDE-UNTRACKED)" t) (autoload 'magit-stash-keep-index "magit-stash" "Create a stash of the index and working tree, keeping index intact.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn MESSAGE &optional INCLUDE-UNTRACKED)" t) (autoload 'magit-snapshot-both "magit-stash" "Create a snapshot of the index and working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn &optional INCLUDE-UNTRACKED)" t) (autoload 'magit-snapshot-index "magit-stash" "Create a snapshot of the index only.
Unstaged and untracked changes are not stashed." t) (autoload 'magit-snapshot-worktree "magit-stash" "Create a snapshot of unstaged changes in the working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn &optional INCLUDE-UNTRACKED)" t) (autoload 'magit-stash-push "magit-stash" nil t) (autoload 'magit-stash-apply "magit-stash" "Apply a stash to the working tree.

First try \"git stash apply --index\", which tries to preserve
the index stored in the stash, if any.  This may fail because
applying the stash could result in conflicts and those have to
be stored in the index, making it impossible to also store the
stash's index there as well.

If the above failed, then try \"git stash apply\".  This fails
(with or without \"--index\") if there are any uncommitted
changes to files that are also modified in the stash.

If both of the above failed, then apply using \"git apply\".
If there are no conflicting files, use \"--3way\".  If there are
conflicting files, then using \"--3way\" requires that those
files are staged first, which may be undesirable, so prompt
the user whether to use \"--3way\" or \"--reject\".

(fn STASH)" t) (autoload 'magit-stash-pop "magit-stash" "Apply a stash to the working tree, on success remove it from stash list.

First try \"git stash pop --index\", which tries to preserve
the index stored in the stash, if any.  This may fail because
applying the stash could result in conflicts and those have to
be stored in the index, making it impossible to also store the
stash's index there as well.

If the above failed, then try \"git stash apply\".  This fails
(with or without \"--index\") if there are any uncommitted
changes to files that are also modified in the stash.

If both of the above failed, then apply using \"git apply\".
If there are no conflicting files, use \"--3way\".  If there are
conflicting files, then using \"--3way\" requires that those
files are staged first, which may be undesirable, so prompt
the user whether to use \"--3way\" or \"--reject\".

(fn STASH)" t) (autoload 'magit-stash-drop "magit-stash" "Remove a stash from the stash list.
When the region is active offer to drop all contained stashes.

(fn STASH)" t) (autoload 'magit-stash-clear "magit-stash" "Remove all stashes saved in REF's reflog by deleting REF.

(fn REF)" t) (autoload 'magit-stash-branch "magit-stash" "Create and checkout a new BRANCH from an existing STASH.
The new branch starts at the commit that was current when the
stash was created.  If the stash applies cleanly, then drop it.

(fn STASH BRANCH)" t) (autoload 'magit-stash-branch-here "magit-stash" "Create and checkout a new BRANCH from an existing STASH.
Use the current branch or `HEAD' as the starting-point of BRANCH.
Then apply STASH, dropping it if it applies cleanly.

(fn STASH BRANCH)" t) (autoload 'magit-stash-format-patch "magit-stash" "Create a patch from STASH

(fn STASH)" t) (autoload 'magit-stash-list "magit-stash" "List all stashes in a buffer." t) (autoload 'magit-stash-show "magit-stash" "Show all diffs of a stash in a buffer.

(fn STASH &optional ARGS FILES)" t) (register-definition-prefixes "magit-stash" '("magit-")) (autoload 'magit-init "magit-status" "Initialize a Git repository, then show its status.

If the directory is below an existing repository, then the user
has to confirm that a new one should be created inside.  If the
directory is the root of the existing repository, then the user
has to confirm that it should be reinitialized.

Non-interactively DIRECTORY is (re-)initialized unconditionally.

(fn DIRECTORY)" t) (autoload 'magit-status "magit-status" "Show the status of the current Git repository in a buffer.

If the current directory isn't located within a Git repository,
then prompt for an existing repository or an arbitrary directory,
depending on option `magit-repository-directories', and show the
status of the selected repository instead.

* If that option specifies any existing repositories, then offer
  those for completion and show the status buffer for the
  selected one.

* Otherwise read an arbitrary directory using regular file-name
  completion.  If the selected directory is the top-level of an
  existing working tree, then show the status buffer for that.

* Otherwise offer to initialize the selected directory as a new
  repository.  After creating the repository show its status
  buffer.

These fallback behaviors can also be forced using one or more
prefix arguments:

* With two prefix arguments (or more precisely a numeric prefix
  value of 16 or greater) read an arbitrary directory and act on
  it as described above.  The same could be accomplished using
  the command `magit-init'.

* With a single prefix argument read an existing repository, or
  if none can be found based on `magit-repository-directories',
  then fall back to the same behavior as with two prefix
  arguments.

(fn &optional DIRECTORY CACHE)" t) (defalias 'magit #'magit-status "Begin using Magit.

This alias for `magit-status' exists for better discoverability.

Instead of invoking this alias for `magit-status' using
\"M-x magit RET\", you should bind a key to `magit-status'
and read the info node `(magit)Getting Started', which
also contains other useful hints.") (autoload 'magit-status-here "magit-status" "Like `magit-status' but with non-nil `magit-status-goto-file-position'." t) (autoload 'magit-status-quick "magit-status" "Show the status of the current Git repository, maybe without refreshing.

If the status buffer of the current Git repository exists but
isn't being displayed in the selected frame, then display it
without refreshing it.

If the status buffer is being displayed in the selected frame,
then also refresh it.

Prefix arguments have the same meaning as for `magit-status',
and additionally cause the buffer to be refresh.

To use this function instead of `magit-status', add this to your
init file: (global-set-key (kbd \"C-x g\") \\='magit-status-quick)." t) (autoload 'magit-status-setup-buffer "magit-status" "

(fn &optional DIRECTORY)") (register-definition-prefixes "magit-status" '("magit-")) (autoload 'magit-submodule "magit-submodule" nil t) (autoload 'magit-submodule-add "magit-submodule" nil t) (autoload 'magit-submodule-read-name-for-path "magit-submodule" "

(fn PATH &optional PREFER-SHORT)") (autoload 'magit-submodule-register "magit-submodule" nil t) (autoload 'magit-submodule-populate "magit-submodule" nil t) (autoload 'magit-submodule-update "magit-submodule" nil t) (autoload 'magit-submodule-synchronize "magit-submodule" nil t) (autoload 'magit-submodule-unpopulate "magit-submodule" nil t) (autoload 'magit-submodule-remove "magit-submodule" "Unregister MODULES and remove their working directories.

For safety reasons, do not remove the gitdirs and if a module has
uncommitted changes, then do not remove it at all.  If a module's
gitdir is located inside the working directory, then move it into
the gitdir of the superproject first.

With the \"--force\" argument offer to remove dirty working
directories and with a prefix argument offer to delete gitdirs.
Both actions are very dangerous and have to be confirmed.  There
are additional safety precautions in place, so you might be able
to recover from making a mistake here, but don't count on it.

(fn MODULES ARGS TRASH-GITDIRS)" t) (autoload 'magit-insert-modules "magit-submodule" "Insert submodule sections.
Hook `magit-module-sections-hook' controls which module sections
are inserted, and option `magit-module-sections-nested' controls
whether they are wrapped in an additional section.") (autoload 'magit-insert-modules-overview "magit-submodule" "Insert sections for all modules.
For each section insert the path and the output of `git describe --tags',
or, failing that, the abbreviated HEAD commit hash.") (autoload 'magit-insert-modules-unpulled-from-upstream "magit-submodule" "Insert sections for modules that haven't been pulled from the upstream.
These sections can be expanded to show the respective commits.") (autoload 'magit-insert-modules-unpulled-from-pushremote "magit-submodule" "Insert sections for modules that haven't been pulled from the push-remote.
These sections can be expanded to show the respective commits.") (autoload 'magit-insert-modules-unpushed-to-upstream "magit-submodule" "Insert sections for modules that haven't been pushed to the upstream.
These sections can be expanded to show the respective commits.") (autoload 'magit-insert-modules-unpushed-to-pushremote "magit-submodule" "Insert sections for modules that haven't been pushed to the push-remote.
These sections can be expanded to show the respective commits.") (autoload 'magit-list-submodules "magit-submodule" "Display a list of the current repository's populated submodules." t) (register-definition-prefixes "magit-submodule" '("magit-")) (autoload 'magit-subtree "magit-subtree" nil t) (autoload 'magit-subtree-import "magit-subtree" nil t) (autoload 'magit-subtree-export "magit-subtree" nil t) (autoload 'magit-subtree-add "magit-subtree" "Add REF from REPOSITORY as a new subtree at PREFIX.

(fn PREFIX REPOSITORY REF ARGS)" t) (autoload 'magit-subtree-add-commit "magit-subtree" "Add COMMIT as a new subtree at PREFIX.

(fn PREFIX COMMIT ARGS)" t) (autoload 'magit-subtree-merge "magit-subtree" "Merge COMMIT into the PREFIX subtree.

(fn PREFIX COMMIT ARGS)" t) (autoload 'magit-subtree-pull "magit-subtree" "Pull REF from REPOSITORY into the PREFIX subtree.

(fn PREFIX REPOSITORY REF ARGS)" t) (autoload 'magit-subtree-push "magit-subtree" "Extract the history of the subtree PREFIX and push it to REF on REPOSITORY.

(fn PREFIX REPOSITORY REF ARGS)" t) (autoload 'magit-subtree-split "magit-subtree" "Extract the history of the subtree PREFIX.

(fn PREFIX COMMIT ARGS)" t) (register-definition-prefixes "magit-subtree" '("magit-")) (autoload 'magit-tag "magit" nil t) (autoload 'magit-tag-create "magit-tag" "Create a new tag with the given NAME at REV.
With a prefix argument annotate the tag.

(git tag [--annotate] NAME REV)

(fn NAME REV &optional ARGS)" t) (autoload 'magit-tag-delete "magit-tag" "Delete one or more tags.
If the region marks multiple tags (and nothing else), then offer
to delete those, otherwise prompt for a single tag to be deleted,
defaulting to the tag at point.

(git tag -d TAGS)

(fn TAGS)" t) (autoload 'magit-tag-prune "magit-tag" "Offer to delete tags missing locally from REMOTE, and vice versa.

(fn TAGS REMOTE-TAGS REMOTE)" t) (autoload 'magit-tag-release "magit-tag" "Create a release tag for `HEAD'.

Assume that release tags match `magit-release-tag-regexp'.

If `HEAD's message matches `magit-release-commit-regexp', then
base the tag on the version string specified by that.  Otherwise
prompt for the name of the new tag using the highest existing
tag as initial input and leaving it to the user to increment the
desired part of the version string.

If `--annotate' is enabled, then prompt for the message of the
new tag.  Base the proposed tag message on the message of the
highest tag, provided that that contains the corresponding
version string and substituting the new version string for that.
Otherwise propose something like \"Foo-Bar 1.2.3\", given, for
example, a TAG \"v1.2.3\" and a repository located at something
like \"/path/to/foo-bar\".

(fn TAG MSG &optional ARGS)" t) (register-definition-prefixes "magit-tag" '("magit-")) (register-definition-prefixes "magit-transient" '("magit-")) (defvar magit-wip-mode nil "Non-nil if Magit-Wip mode is enabled.
See the `magit-wip-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-wip-mode'.") (custom-autoload 'magit-wip-mode "magit-wip" nil) (autoload 'magit-wip-mode "magit-wip" "Save uncommitted changes to work-in-progress refs.

Whenever appropriate (i.e., when dataloss would be a possibility
otherwise) this mode causes uncommitted changes to be committed
to dedicated work-in-progress refs.

For historic reasons this mode is implemented on top of four
other `magit-wip-*' modes, which can also be used individually,
if you want finer control over when the wip refs are updated;
but that is discouraged.

This is a global minor mode.  If called interactively, toggle the
`Magit-Wip mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='magit-wip-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'magit-wip-after-save-mode 'globalized-minor-mode t) (defvar magit-wip-after-save-mode nil "Non-nil if Magit-Wip-After-Save mode is enabled.
See the `magit-wip-after-save-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-wip-after-save-mode'.") (custom-autoload 'magit-wip-after-save-mode "magit-wip" nil) (autoload 'magit-wip-after-save-mode "magit-wip" "Toggle Magit-Wip-After-Save-Local mode in all buffers.
With prefix ARG, enable Magit-Wip-After-Save mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Magit-Wip-After-Save-Local mode is enabled in all buffers where
`magit-wip-after-save-local-mode-turn-on' would do it.

See `magit-wip-after-save-local-mode' for more information on
Magit-Wip-After-Save-Local mode.

(fn &optional ARG)" t) (defvar magit-wip-after-apply-mode nil "Non-nil if Magit-Wip-After-Apply mode is enabled.
See the `magit-wip-after-apply-mode' command
for a description of this minor mode.") (custom-autoload 'magit-wip-after-apply-mode "magit-wip" nil) (autoload 'magit-wip-after-apply-mode "magit-wip" "Commit to work-in-progress refs.

After applying a change using any \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected files to the current wip refs.  For each branch there
may be two wip refs; one contains snapshots of the files as found
in the worktree and the other contains snapshots of the entries
in the index.

This is a global minor mode.  If called interactively, toggle the
`Magit-Wip-After-Apply mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='magit-wip-after-apply-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (defvar magit-wip-before-change-mode nil "Non-nil if Magit-Wip-Before-Change mode is enabled.
See the `magit-wip-before-change-mode' command
for a description of this minor mode.") (custom-autoload 'magit-wip-before-change-mode "magit-wip" nil) (autoload 'magit-wip-before-change-mode "magit-wip" "Commit to work-in-progress refs before certain destructive changes.

Before invoking a revert command or an \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected tracked files to the current wip refs.  For each branch
there may be two wip refs; one contains snapshots of the files
as found in the worktree and the other contains snapshots of the
entries in the index.

Only changes to files which could potentially be affected by the
command which is about to be called are committed.

This is a global minor mode.  If called interactively, toggle the
`Magit-Wip-Before-Change mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='magit-wip-before-change-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'magit-wip-commit-initial-backup "magit-wip" "Before saving, commit current file to a worktree wip ref.

The user has to add this function to `before-save-hook'.

Commit the current state of the visited file before saving the
current buffer to that file.  This backs up the same version of
the file as `backup-buffer' would, but stores the backup in the
worktree wip ref, which is also used by the various Magit Wip
modes, instead of in a backup file as `backup-buffer' would.

This function ignores the variables that affect `backup-buffer'
and can be used along-side that function, which is recommended
because this function only backs up files that are tracked in
a Git repository.") (register-definition-prefixes "magit-wip" '("magit-")) (autoload 'magit-worktree "magit-worktree" nil t) (autoload 'magit-worktree-checkout "magit-worktree" "Checkout BRANCH in a new worktree at PATH.

(fn PATH BRANCH)" t) (autoload 'magit-worktree-branch "magit-worktree" "Create a new BRANCH and check it out in a new worktree at PATH.

(fn PATH BRANCH START-POINT &optional FORCE)" t) (autoload 'magit-worktree-move "magit-worktree" "Move WORKTREE to PATH.

(fn WORKTREE PATH)" t) (register-definition-prefixes "magit-worktree" '("magit-")) (provide 'magit-autoloads)) "vterm" ((vterm-autoloads vterm) (autoload 'vterm-module-compile "vterm" "Compile vterm-module." t) (autoload 'vterm--bookmark-handler "vterm" "Handler to restore a vterm bookmark BMK.

If a vterm buffer of the same name does not exist, the function will create a
new vterm buffer of the name. It also checks the current directory and sets
it to the bookmarked directory if needed.

(fn BMK)") (autoload 'vterm-next-error-function "vterm" "Advance to the next error message and visit the file where the error was.
This is the value of `next-error-function' in Compilation
buffers.  Prefix arg N says how many error messages to move
forwards (or backwards, if negative).

Optional argument RESET clears all the errors.

(fn N &optional RESET)" t) (autoload 'vterm "vterm" "Create an interactive Vterm buffer.
Start a new Vterm session, or switch to an already active
session.  Return the buffer selected (or created).

With a nonnumeric prefix arg, create a new session.

With a string prefix arg, create a new session with arg as buffer name.

With a numeric prefix arg (as in `C-u 42 M-x vterm RET'), switch
to the session with that number, or create it if it doesn't
already exist.

The buffer name used for Vterm sessions is determined by the
value of `vterm-buffer-name'.

(fn &optional ARG)" t) (autoload 'vterm-other-window "vterm" "Create an interactive Vterm buffer in another window.
Start a new Vterm session, or switch to an already active
session.  Return the buffer selected (or created).

With a nonnumeric prefix arg, create a new session.

With a string prefix arg, create a new session with arg as buffer name.

With a numeric prefix arg (as in `C-u 42 M-x vterm RET'), switch
to the session with that number, or create it if it doesn't
already exist.

The buffer name used for Vterm sessions is determined by the
value of `vterm-buffer-name'.

(fn &optional ARG)" t) (register-definition-prefixes "vterm" '("vterm-")) (provide 'vterm-autoloads)) "exec-path-from-shell" ((exec-path-from-shell exec-path-from-shell-autoloads) (autoload 'exec-path-from-shell-copy-envs "exec-path-from-shell" "Set the environment variables with NAMES from the user's shell.

As a special case, if the variable is $PATH, then the variables
`exec-path' and `eshell-path-env' are also set appropriately.
The result is an alist, as described by
`exec-path-from-shell-getenvs'.

(fn NAMES)") (autoload 'exec-path-from-shell-copy-env "exec-path-from-shell" "Set the environment variable $NAME from the user's shell.

As a special case, if the variable is $PATH, then the variables
`exec-path' and `eshell-path-env' are also set appropriately.
Return the value of the environment variable.

(fn NAME)" t) (autoload 'exec-path-from-shell-initialize "exec-path-from-shell" "Initialize environment from the user's shell.

The values of all the environment variables named in
`exec-path-from-shell-variables' are set from the corresponding
values used in the user's shell." t) (register-definition-prefixes "exec-path-from-shell" '("exec-path-from-shell-")) (provide 'exec-path-from-shell-autoloads)) "osx-lib" ((osx-lib-autoloads osx-lib) (autoload 'osx-lib-run-osascript "osx-lib" "Run an SCRIPT-CONTENT as AppleScript/osascipt.

(fn SCRIPT-CONTENT)" t) (autoload 'osx-lib-osx-version "osx-lib" "Get OS version." t) (autoload 'osx-lib-run-js "osx-lib" "Run an SCRIPT-CONTENT as JavaScript.

(fn SCRIPT-CONTENT)" t) (autoload 'osx-lib-do-beep "osx-lib" "Play beep sound.") (autoload 'osx-lib-notify2 "osx-lib" "Create a notification with title as TITLE and message as MESSAGE.

(fn TITLE MESSAGE)") (autoload 'osx-lib-notify3 "osx-lib" "Create a notification with title as TITLE, subtitle as SUBTITLE and message as MESSAGE.

(fn TITLE SUBTITLE MESSAGE)") (autoload 'osx-lib-copy-to-clipboard "osx-lib" "Copy the given TEXT to clipboard.

(fn &optional TEXT)" t) (autoload 'osx-lib-reveal-in-finder-as "osx-lib" "Reveal the supplied file FILE in Finder.
This function runs the actual AppleScript.

(fn FILE)") (autoload 'osx-lib-reveal-in-finder "osx-lib" "Reveal the file associated with the current buffer in the OS X Finder.
In a dired buffer, it will open the current file." t) (autoload 'osx-lib-vpn-connect "osx-lib" "Connect to vpn using given VPN-NAME and PASSWORD.

(fn VPN-NAME PASSWORD)" t) (autoload 'osx-lib-vpn-disconnect "osx-lib" "Disconnect from VPN-NAME vpn.

(fn VPN-NAME)" t) (autoload 'osx-lib-say "osx-lib" "Speak the MESSAGE.

(fn MESSAGE)" t) (autoload 'osx-lib-open-url-at-point "osx-lib" "Open URL at point using default browser.

(fn URL)" t) (autoload 'osx-lib-set-volume "osx-lib" "Set sound output volume to VOL(0-100).

(fn VOL)" t) (autoload 'osx-lib-get-volume "osx-lib" "Get sound output volume (0-100).") (autoload 'osx-lib-start-terminal "osx-lib" "Start terminal in DIR.

(fn &optional (DIR default-directory) CMD-WITH-QUOTED-ARGS)" t) (autoload 'osx-lib-network-quality "osx-lib" "Test the network's quality using the built-in NQ tool." t) (register-definition-prefixes "osx-lib" '("osx-lib-" "with-min-osx-ver")) (provide 'osx-lib-autoloads)) "projectile" ((projectile-autoloads projectile) (autoload 'projectile-version "projectile" "Get the Projectile version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil.

(fn &optional SHOW-VERSION)" t) (autoload 'projectile-invalidate-cache "projectile" "Remove the current project's files from `projectile-projects-cache'.

With a prefix argument PROMPT prompts for the name of the project whose cache
to invalidate.

(fn PROMPT)" t) (autoload 'projectile-purge-file-from-cache "projectile" "Purge FILE from the cache of the current project.

(fn FILE)" t) (autoload 'projectile-purge-dir-from-cache "projectile" "Purge DIR from the cache of the current project.

(fn DIR)" t) (autoload 'projectile-cache-current-file "projectile" "Add the currently visited file to the cache." t) (autoload 'projectile-discover-projects-in-directory "projectile" "Discover any projects in DIRECTORY and add them to the projectile cache.

If DEPTH is non-nil recursively descend exactly DEPTH levels below DIRECTORY and
discover projects there.

(fn DIRECTORY &optional DEPTH)" t) (autoload 'projectile-discover-projects-in-search-path "projectile" "Discover projects in `projectile-project-search-path'.
Invoked automatically when `projectile-mode' is enabled." t) (autoload 'projectile-switch-to-buffer "projectile" "Switch to a project buffer." t) (autoload 'projectile-switch-to-buffer-other-window "projectile" "Switch to a project buffer and show it in another window." t) (autoload 'projectile-switch-to-buffer-other-frame "projectile" "Switch to a project buffer and show it in another frame." t) (autoload 'projectile-display-buffer "projectile" "Display a project buffer in another window without selecting it." t) (autoload 'projectile-project-buffers-other-buffer "projectile" "Switch to the most recently selected buffer project buffer.
Only buffers not visible in windows are returned." t) (autoload 'projectile-multi-occur "projectile" "Do a `multi-occur' in the project's buffers.
With a prefix argument, show NLINES of context.

(fn &optional NLINES)" t) (autoload 'projectile-find-other-file "projectile" "Switch between files with the same name but different extensions.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable
`projectile-other-file-alist'.

(fn &optional FLEX-MATCHING)" t) (autoload 'projectile-find-other-file-other-window "projectile" "Switch between files with different extensions in other window.
Switch between files with the same name but different extensions in other
window.  With FLEX-MATCHING, match any file that contains the base name of
current file.  Other file extensions can be customized with the variable
`projectile-other-file-alist'.

(fn &optional FLEX-MATCHING)" t) (autoload 'projectile-find-other-file-other-frame "projectile" "Switch between files with different extensions in other frame.
Switch between files with the same name but different extensions in other frame.
With FLEX-MATCHING, match any file that contains the base name of current
file.  Other file extensions can be customized with the variable
`projectile-other-file-alist'.

(fn &optional FLEX-MATCHING)" t) (autoload 'projectile-find-file-dwim "projectile" "Jump to a project's files using completion based on context.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works
even if the filename is incomplete, but there's only a single file in the
current project that matches the filename at point.  For example, if
there's only a single file named \"projectile/projectile.el\" but the
current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim' still switches to \"projectile/projectile.el\"
immediately because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list
of files is displayed when a filename appears more than one in the project
or the filename at point is a prefix of more than two files in a project.
For example, if `projectile-find-file-dwim' is executed on a filepath like
\"projectile/\", it lists the content of that directory.  If it is executed
on a partial filename like \"projectile/a\", a list of files with character
\"a\" in that directory is presented.

- If it finds nothing, display a list of all files in project for selecting.

(fn &optional INVALIDATE-CACHE)" t) (autoload 'projectile-find-file-dwim-other-window "projectile" "Jump to a project's files using completion based on context in other window.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works
even if the filename is incomplete, but there's only a single file in the
current project that matches the filename at point.  For example, if
there's only a single file named \"projectile/projectile.el\" but the
current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim-other-window' still switches to
\"projectile/projectile.el\" immediately because this is the only filename
that matches.

- If it finds a list of files, the list is displayed for selecting.  A list
of files is displayed when a filename appears more than one in the project
or the filename at point is a prefix of more than two files in a project.
For example, if `projectile-find-file-dwim-other-window' is executed on a
filepath like \"projectile/\", it lists the content of that directory.  If
it is executed on a partial filename like \"projectile/a\", a list of files
with character \"a\" in that directory is presented.

- If it finds nothing, display a list of all files in project for selecting.

(fn &optional INVALIDATE-CACHE)" t) (autoload 'projectile-find-file-dwim-other-frame "projectile" "Jump to a project's files using completion based on context in other frame.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works
even if the filename is incomplete, but there's only a single file in the
current project that matches the filename at point.  For example, if
there's only a single file named \"projectile/projectile.el\" but the
current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim-other-frame' still switches to
\"projectile/projectile.el\" immediately because this is the only filename
that matches.

- If it finds a list of files, the list is displayed for selecting.  A list
of files is displayed when a filename appears more than one in the project
or the filename at point is a prefix of more than two files in a project.
For example, if `projectile-find-file-dwim-other-frame' is executed on a
filepath like \"projectile/\", it lists the content of that directory.  If
it is executed on a partial filename like \"projectile/a\", a list of files
with character \"a\" in that directory is presented.

- If it finds nothing, display a list of all files in project for selecting.

(fn &optional INVALIDATE-CACHE)" t) (autoload 'projectile-find-file "projectile" "Jump to a project's file using completion.
With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t) (autoload 'projectile-find-file-other-window "projectile" "Jump to a project's file using completion and show it in another window.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t) (autoload 'projectile-find-file-other-frame "projectile" "Jump to a project's file using completion and show it in another frame.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t) (autoload 'projectile-toggle-project-read-only "projectile" "Toggle project read only." t) (autoload 'projectile-add-dir-local-variable "projectile" "Run `add-dir-local-variable' with .dir-locals.el in root of project.

Parameters MODE VARIABLE VALUE are passed directly to `add-dir-local-variable'.

(fn MODE VARIABLE VALUE)") (autoload 'projectile-delete-dir-local-variable "projectile" "Run `delete-dir-local-variable' with .dir-locals.el in root of project.

Parameters MODE VARIABLE VALUE are passed directly to
`delete-dir-local-variable'.

(fn MODE VARIABLE)") (autoload 'projectile-find-dir "projectile" "Jump to a project's directory using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t) (autoload 'projectile-find-dir-other-window "projectile" "Jump to a project's directory in other window using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t) (autoload 'projectile-find-dir-other-frame "projectile" "Jump to a project's directory in other frame using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t) (autoload 'projectile-find-test-file "projectile" "Jump to a project's test file using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t) (autoload 'projectile-find-related-file-other-window "projectile" "Open related file in other window." t) (autoload 'projectile-find-related-file-other-frame "projectile" "Open related file in other frame." t) (autoload 'projectile-find-related-file "projectile" "Open related file." t) (autoload 'projectile-related-files-fn-groups "projectile" "Generate a related-files-fn which relates as KIND for files in each of GROUPS.

(fn KIND GROUPS)") (autoload 'projectile-related-files-fn-extensions "projectile" "Generate a related-files-fn which relates as KIND for files having EXTENSIONS.

(fn KIND EXTENSIONS)") (autoload 'projectile-related-files-fn-test-with-prefix "projectile" "Generate a related-files-fn which relates tests and impl.
Use files with EXTENSION based on TEST-PREFIX.

(fn EXTENSION TEST-PREFIX)") (autoload 'projectile-related-files-fn-test-with-suffix "projectile" "Generate a related-files-fn which relates tests and impl.
Use files with EXTENSION based on TEST-SUFFIX.

(fn EXTENSION TEST-SUFFIX)") (autoload 'projectile-project-info "projectile" "Display info for current project." t) (autoload 'projectile-find-implementation-or-test-other-window "projectile" "Open matching implementation or test file in other window.

See the documentation of `projectile--find-matching-file' and
`projectile--find-matching-test' for how implementation and test files
are determined." t) (autoload 'projectile-find-implementation-or-test-other-frame "projectile" "Open matching implementation or test file in other frame.

See the documentation of `projectile--find-matching-file' and
`projectile--find-matching-test' for how implementation and test files
are determined." t) (autoload 'projectile-toggle-between-implementation-and-test "projectile" "Toggle between an implementation file and its test file.


See the documentation of `projectile--find-matching-file' and
`projectile--find-matching-test' for how implementation and test files
are determined." t) (autoload 'projectile-grep "projectile" "Perform rgrep in the project.

With a prefix ARG asks for files (globbing-aware) which to grep in.
With prefix ARG of `-' (such as `M--'), default the files (without prompt),
to `projectile-grep-default-files'.

With REGEXP given, don't query the user for a regexp.

(fn &optional REGEXP ARG)" t) (autoload 'projectile-ag "projectile" "Run an ag search with SEARCH-TERM in the project.

With an optional prefix argument ARG SEARCH-TERM is interpreted as a
regular expression.

(fn SEARCH-TERM &optional ARG)" t) (autoload 'projectile-ripgrep "projectile" "Run a ripgrep (rg) search with `SEARCH-TERM' at current project root.

With an optional prefix argument ARG SEARCH-TERM is interpreted as a
regular expression.

This command depends on of the Emacs packages ripgrep or rg being
installed to work.

(fn SEARCH-TERM &optional ARG)" t) (autoload 'projectile-regenerate-tags "projectile" "Regenerate the project's [e|g]tags." t) (autoload 'projectile-find-tag "projectile" "Find tag in project." t) (autoload 'projectile-run-command-in-root "projectile" "Invoke `execute-extended-command' in the project's root." t) (autoload 'projectile-run-shell-command-in-root "projectile" "Invoke `shell-command' in the project's root.

(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER)" t) (autoload 'projectile-run-async-shell-command-in-root "projectile" "Invoke `async-shell-command' in the project's root.

(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER)" t) (autoload 'projectile-run-gdb "projectile" "Invoke `gdb' in the project's root." t) (autoload 'projectile-run-shell "projectile" "Invoke `shell' in the project's root.

Switch to the project specific shell buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t) (autoload 'projectile-run-eshell "projectile" "Invoke `eshell' in the project's root.

Switch to the project specific eshell buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t) (autoload 'projectile-run-ielm "projectile" "Invoke `ielm' in the project's root.

Switch to the project specific ielm buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t) (autoload 'projectile-run-term "projectile" "Invoke `term' in the project's root.

Switch to the project specific term buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t) (autoload 'projectile-run-vterm "projectile" "Invoke `vterm' in the project's root.

Switch to the project specific term buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t) (autoload 'projectile-run-vterm-other-window "projectile" "Invoke `vterm' in the project's root.

Switch to the project specific term buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t) (autoload 'projectile-replace "projectile" "Replace literal string in project using non-regexp `tags-query-replace'.

With a prefix argument ARG prompts you for a directory and file name patterns
on which to run the replacement.

(fn &optional ARG)" t) (autoload 'projectile-replace-regexp "projectile" "Replace a regexp in the project using `tags-query-replace'.

With a prefix argument ARG prompts you for a directory on which
to run the replacement.

(fn &optional ARG)" t) (autoload 'projectile-kill-buffers "projectile" "Kill project buffers.

The buffer are killed according to the value of
`projectile-kill-buffers-filter'." t) (autoload 'projectile-save-project-buffers "projectile" "Save all project buffers." t) (autoload 'projectile-dired "projectile" "Open `dired' at the root of the project." t) (autoload 'projectile-dired-other-window "projectile" "Open `dired'  at the root of the project in another window." t) (autoload 'projectile-dired-other-frame "projectile" "Open `dired' at the root of the project in another frame." t) (autoload 'projectile-vc "projectile" "Open `vc-dir' at the root of the project.

For git projects `magit-status-internal' is used if available.
For hg projects `monky-status' is used if available.

If PROJECT-ROOT is given, it is opened instead of the project
root directory of the current buffer file.  If interactively
called with a prefix argument, the user is prompted for a project
directory to open.

(fn &optional PROJECT-ROOT)" t) (autoload 'projectile-recentf "projectile" "Show a list of recently visited files in a project." t) (autoload 'projectile-configure-project "projectile" "Run project configure command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t) (autoload 'projectile-compile-project "projectile" "Run project compilation command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.  Per project default command can be set through
`projectile-project-compilation-cmd'.

(fn ARG)" t) (autoload 'projectile-test-project "projectile" "Run project test command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t) (autoload 'projectile-install-project "projectile" "Run project install command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t) (autoload 'projectile-package-project "projectile" "Run project package command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t) (autoload 'projectile-run-project "projectile" "Run project run command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t) (autoload 'projectile-repeat-last-command "projectile" "Run last projectile external command.

External commands are: `projectile-configure-project',
`projectile-compile-project', `projectile-test-project',
`projectile-install-project', `projectile-package-project',
and `projectile-run-project'.

If the prefix argument SHOW_PROMPT is non nil, the command can be edited.

(fn SHOW-PROMPT)" t) (autoload 'projectile-switch-project "projectile" "Switch to a project we have visited before.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'

(fn &optional ARG)" t) (autoload 'projectile-switch-open-project "projectile" "Switch to a project we have currently opened.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'

(fn &optional ARG)" t) (autoload 'projectile-find-file-in-directory "projectile" "Jump to a file in a (maybe regular) DIRECTORY.

This command will first prompt for the directory the file is in.

(fn &optional DIRECTORY)" t) (autoload 'projectile-find-file-in-known-projects "projectile" "Jump to a file in any of the known projects." t) (autoload 'projectile-cleanup-known-projects "projectile" "Remove known projects that don't exist anymore." t) (autoload 'projectile-clear-known-projects "projectile" "Clear both `projectile-known-projects' and `projectile-known-projects-file'." t) (autoload 'projectile-reset-known-projects "projectile" "Clear known projects and rediscover." t) (autoload 'projectile-remove-known-project "projectile" "Remove PROJECT from the list of known projects.

(fn &optional PROJECT)" t) (autoload 'projectile-remove-current-project-from-known-projects "projectile" "Remove the current project from the list of known projects." t) (autoload 'projectile-add-known-project "projectile" "Add PROJECT-ROOT to the list of known projects.

(fn PROJECT-ROOT)" t) (autoload 'projectile-ibuffer "projectile" "Open an IBuffer window showing all buffers in the current project.

Let user choose another project when PROMPT-FOR-PROJECT is supplied.

(fn PROMPT-FOR-PROJECT)" t) (autoload 'projectile-commander "projectile" "Execute a Projectile command with a single letter.
The user is prompted for a single character indicating the action to invoke.
The `?' character describes then
available actions.

See `def-projectile-commander-method' for defining new methods." t) (autoload 'projectile-browse-dirty-projects "projectile" "Browse dirty version controlled projects.

With a prefix argument, or if CACHED is non-nil, try to use the cached
dirty project list.

(fn &optional CACHED)" t) (autoload 'projectile-edit-dir-locals "projectile" "Edit or create a .dir-locals.el file of the project." t) (defvar projectile-mode nil "Non-nil if Projectile mode is enabled.
See the `projectile-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `projectile-mode'.") (custom-autoload 'projectile-mode "projectile" nil) (autoload 'projectile-mode "projectile" "Minor mode to assist project management and navigation.

When called interactively, toggle `projectile-mode'.  With prefix
ARG, enable `projectile-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `projectile-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `projectile-mode'.
Otherwise behave as if called interactively.

\\{projectile-mode-map}

(fn &optional ARG)" t) (define-obsolete-function-alias 'projectile-global-mode 'projectile-mode "1.0") (register-definition-prefixes "projectile" '("??" "compilation-find-file-projectile-find-compilation-buffer" "def-projectile-commander-method" "delete-file-projectile-remove-from-cache" "project")) (provide 'projectile-autoloads)) "racket-mode" ((racket-wsl racket-browse-url racket-indent racket-font-lock racket-shell racket-describe racket-company-doc racket-common racket-parens racket-visit racket-mode racket-imenu racket-repl-buffer-name racket-eldoc racket-cmd racket-ppss racket-repl racket-keywords-and-builtins racket-profile racket-util racket-complete racket-bug-report racket-custom racket-back-end racket-edit racket-scribble racket-unicode-input-method racket-smart-open racket-xp-complete racket-stepper racket-mode-autoloads racket-show racket-logger racket-debug racket-doc racket-collection racket-xp) (register-definition-prefixes "racket-back-end" '("racket-")) (register-definition-prefixes "racket-browse-url" '("racket-browse-")) (autoload 'racket-bug-report "racket-bug-report" "Fill a buffer with data to make a Racket Mode bug report." t) (defvar racket-start-back-end-hook nil "Hook run after `racket-start-back-end' finishes successfully.") (defvar racket-stop-back-end-hook nil "Hook run before `racket-stop-back-end'.") (autoload 'racket-start-back-end "racket-cmd" "Start a back end process used by Racket Mode.

If a process is already started, this command will stop and restart it.

When successful runs the hook `racket-start-back-end-hook'." t) (autoload 'racket-stop-back-end "racket-cmd" "Stop a back end process used by Racket Mode.

Before doing anything runs the hook `racket-stop-back-end-hook'." t) (register-definition-prefixes "racket-cmd" '("racket--")) (register-definition-prefixes "racket-collection" '("racket-")) (register-definition-prefixes "racket-common" '("racket-")) (register-definition-prefixes "racket-company-doc" '("racket--")) (register-definition-prefixes "racket-complete" '("racket--")) (register-definition-prefixes "racket-custom" '("defface-racket" "racket-")) (autoload 'racket--debug-on-break "racket-debug" "

(fn RESPONSE)") (register-definition-prefixes "racket-debug" '("racket-")) (register-definition-prefixes "racket-describe" '("racket-")) (register-definition-prefixes "racket-doc" '("racket--")) (add-to-list 'hs-special-modes-alist '(racket-mode "(" ")" ";" nil nil)) (register-definition-prefixes "racket-edit" '("racket-")) (register-definition-prefixes "racket-eldoc" '("racket--do-eldoc")) (register-definition-prefixes "racket-font-lock" '("load-theme" "racket-")) (register-definition-prefixes "racket-imenu" '("racket-")) (register-definition-prefixes "racket-indent" '("racket-")) (register-definition-prefixes "racket-keywords-and-builtins" '("racket-")) (register-definition-prefixes "racket-logger" '("racket-")) (autoload 'racket-mode "racket-mode" "Major mode for editing Racket source files.

\\{racket-mode-map}

(fn)" t) (add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode)) (add-to-list 'auto-mode-alist '("\\.rktd\\'" . racket-mode)) (add-to-list 'auto-mode-alist '("\\.rktl\\'" . racket-mode)) (modify-coding-system-alist 'file "\\.rkt[dl]?\\'" 'utf-8) (add-to-list 'interpreter-mode-alist '("racket" . racket-mode)) (autoload 'racket-mode-start-faster "racket-mode" "Compile Racket Mode's .rkt files for faster startup.

Racket Mode is implemented as an Emacs Lisp \"front end\" that
talks to a Racket process \"back end\". Because Racket Mode is
delivered as an Emacs package instead of a Racket package,
installing it does not do the `raco setup` that is normally done
for Racket packages.

This command will do a `raco make` of Racket Mode's .rkt files,
creating bytecode files in `compiled/` subdirectories. As a
result, when a command must start the Racket process, it will
start somewhat faster.

On many computers, the resulting speed up is negligible, and
might not be worth the complication.

If you run this command, ever, you will need to run it again
after:

- Installing an updated version of Racket Mode. Otherwise, you
  might lose some of the speed-up.

- Installing a new version of Racket and/or changing the value of
  the variable `racket-program'. Otherwise, you might get an
  error message due to the bytecode being different versions.

To revert to compiling on startup, use
`racket-mode-start-slower'. " t) (register-definition-prefixes "racket-mode" '("racket-")) (register-definition-prefixes "racket-parens" '("racket-")) (register-definition-prefixes "racket-ppss" '("racket--ppss-")) (register-definition-prefixes "racket-profile" '("racket-")) (autoload 'racket-repl "racket-repl" "Show a Racket REPL buffer in some window.

*IMPORTANT*

The main, intended use of Racket Mode's REPL is that you
`find-file' some specific .rkt file, then run it using
`racket-run' or `racket-run-module-at-point'. The resulting REPL
will correspond to those definitions and match your expectations.

If you really want to start a REPL for no file in particular,
then you could use this `racket-repl' command. But the resulting
REPL will have a minimal \"#lang racket/base\" namespace. You
could enter \"(require racket)\" if you want the equivalent of
\"#lang racket\". You could also \"(require racket/enter)\" if
you want things like \"enter!\". But in some sense you'd be
\"using it wrong\". If you actually don't want to use Racket
Mode's REPL as intended, then consider using a plain Emacs
`shell' buffer to run command-line Racket.

(fn &optional NOSELECT)" t) (autoload 'racket-run "racket-repl" "Save the buffer in REPL and run your program.

As well as evaluating the outermost, file module, automatically
runs the submodules specified by the customization variable
`racket-submodules-to-run'.

See also `racket-run-module-at-point', which runs just the
specific module at point.

The command varies based on how many \\[universal-argument]
prefix arguments you supply.
\\<racket-mode-map>

- \\[racket-run-and-switch-to-repl]

  Follows the `racket-error-context' setting.

- \\[universal-argument] \\[racket-run-and-switch-to-repl]

  Uses errortrace for improved stack traces, as if
  `racket-error-context' were set to \"high\".

  This lets you keep `racket-error-context' set to a faster
  value like \"low\" or \"medium\", then conveniently re-run
  when you need a better strack trace.

- \\[universal-argument] \\[universal-argument] \\[racket-run-and-switch-to-repl]

  Instruments code for step debugging. See `racket-debug-mode'
  and the variable `racket-debuggable-files'.

Each run occurs within a Racket custodian. Any prior run's
custodian is shut down, releasing resources like threads and
ports. Each run's evaluation environment is reset to the contents
of the source file. In other words, like Dr Racket, this provides
the benefit that your source file is the \"single source of
truth\". At the same time, the run gives you a REPL inside the
namespace of the module, giving you the ability to explore it
interactively. Any explorations are temporary, unless you also
make them to your source file, they will be lost on the next run.

See also `racket-run-and-switch-to-repl', which is even more like
Dr Racket's Run command because it selects the REPL window after
running.

In the `racket-repl-mode' buffer, output that describes a file
and position is automatically \"linkified\". Examples of such
text include:

- Racket error messages.
- rackunit test failure location messages.
- print representation of path objects.

To visit these locations, move point there and press RET or mouse
click. Or, use the standard `next-error' and `previous-error'
commands.

(fn &optional PREFIX)" t) (autoload 'racket-run-module-at-point "racket-repl" "Save the buffer and run the module at point.

Like `racket-run' but runs the innermost module around point,
which is determined textually by looking for \"module\",
\"module*\", or \"module+\" forms nested to any depth, else
simply the outermost, file module.

(fn &optional PREFIX)" t) (register-definition-prefixes "racket-repl" '("racket-" "with-racket-repl-buffer")) (autoload 'racket-call-racket-repl-buffer-name-function "racket-repl-buffer-name") (autoload 'racket-repl-buffer-name-shared "racket-repl-buffer-name" "All `racket-mode' edit buffers share one `racket-repl-mode' buffer per back end.

A value for the variable `racket-repl-buffer-name-function'." t) (autoload 'racket-repl-buffer-name-unique "racket-repl-buffer-name" "Each `racket-mode' edit buffer gets its own `racket-repl-mode' buffer.

A value for the variable `racket-repl-buffer-name-function'." t) (autoload 'racket-repl-buffer-name-project "racket-repl-buffer-name" "All `racket-mode' buffers in a project share a `racket-repl-mode' buffer.

A value for the variable `racket-repl-buffer-name-function'.

The \"project\" is determined by `racket-project-root'." t) (register-definition-prefixes "racket-repl-buffer-name" '("racket-")) (register-definition-prefixes "racket-scribble" '("racket--")) (register-definition-prefixes "racket-shell" '("racket-")) (register-definition-prefixes "racket-show" '("racket-")) (autoload 'racket-smart-open-bracket-mode "racket-smart-open" "Minor mode to let you always type `[`' to insert `(` or `[` automatically.

Behaves like the \"Automatically adjust opening square brackets\"
feature in Dr. Racket.

By default, inserts a `(`. Inserts a `[` in the following cases:

  - `let`-like bindings -- forms with `let` in the name as well
    as things like `parameterize`, `with-handlers`, and
    `with-syntax`.

  - `case`, `cond`, `match`, `syntax-case`, `syntax-parse`, and
    `syntax-rules` clauses.

  - `for`-like bindings and `for/fold` accumulators.

  - `class` declaration syntax, such as `init` and `inherit`.

When the previous s-expression in a sequence is a compound
expression, uses the same kind of delimiter.

To force insert `[`, use `quoted-insert'.

Combined with `racket-insert-closing' this means that you can
press the unshifted `[` and `]` keys to get whatever delimiters
follow the Racket conventions for these forms. When something
like `electric-pair-mode' or `paredit-mode' is active, you need
not even press `]`.

Tip: When also using `paredit-mode', enable that first so that
the binding for the `[`' key in the map for
`racket-smart-open-bracket-mode' has higher priority. See also
the variable `minor-mode-map-alist'.

This is a minor mode.  If called interactively, toggle the
`Racket-Smart-Open-Bracket mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `racket-smart-open-bracket-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "racket-smart-open" '("racket-")) (register-definition-prefixes "racket-stepper" '("racket-")) (autoload 'racket-unicode-input-method-enable "racket-unicode-input-method" "Set input method to racket-unicode.

The racket-unicode input method lets you easily type various
Unicode symbols that might be useful when writing Racket code.

To automatically enable the racket-unicode input method in
racket-mode and racket-repl-mode buffers, put the following code
in your Emacs init file:

#+BEGIN_SRC elisp
    (add-hook \\='racket-mode-hook #\\='racket-unicode-input-method-enable)
    (add-hook \\='racket-repl-mode-hook #\\='racket-unicode-input-method-enable)
#+END_SRC

To temporarily enable this input method for a single buffer you
can use \"M-x racket-unicode-input-method-enable\".

Use the standard Emacs key C-\\ to toggle the input method.

When the racket-unicode input method is active, you can for
example type \"All\" and it is immediately replaced with \"∀\". A
few other examples:

| omega     | ω                        |
| x_1       | x₁                       |
| x^1       | x¹                       |
| A         | 𝔸                        |
| test-->>E | test-->>∃ (racket/redex) |
| vdash     | ⊢                        |

To see a table of all key sequences use \"M-x
describe-input-method <RET> racket-unicode\".

If you want to add your own mappings to the \"racket-unicode\"
input method, you may add code like the following example in your
Emacs init file:

#+BEGIN_SRC elisp
    ;; Either (require \\='racket-mode) here, or, if you use
    ;; use-package, put the code below in the :config section.
    (with-temp-buffer
      (racket-unicode-input-method-enable)
      (set-input-method \"racket-unicode\")
      (let ((quail-current-package (assoc \"racket-unicode\"
                                          quail-package-alist)))
        (quail-define-rules ((append . t))
                            (\"^o\" [\"ᵒ\"]))))
#+END_SRC

If you don’t like the highlighting of partially matching tokens you
can turn it off by setting `input-method-highlight-flag' to nil." t) (register-definition-prefixes "racket-util" '("racket-")) (register-definition-prefixes "racket-visit" '("racket--")) (register-definition-prefixes "racket-wsl" '("racket-")) (autoload 'racket-xp-mode "racket-xp" "A minor mode that analyzes expanded code to explain and explore.

This minor mode is an optional enhancement to `racket-mode' edit
buffers. Like any minor mode, you can turn it on or off for a
specific buffer. If you always want to use it, put the following
code in your Emacs init file:

#+BEGIN_SRC elisp
    (require \\='racket-xp)
    (add-hook \\='racket-mode-hook #\\='racket-xp-mode)
#+END_SRC

Note: This mode won't do anything unless/until the Racket Mode
back end is running. It will try to start the back end
automatically. You do /not/ need to `racket-run' the buffer you
are editing.

This mode uses the drracket/check-syntax package to analyze
fully-expanded programs, without needing to evaluate a.k.a.
\"run\" them. The resulting analysis provides information for:

- Visually annotating bindings -- local or imported definitions
  and references to them.

- Visually annotating expressions in a tail position, as well as
  the enclosing expression with respect to which they are in a
  tail position.

- Completion candidates.

- Defintions' source and documentation.

When point is on a definition or use, related items are
highlighted using `racket-xp-def-face' and `racket-xp-use-face'
-- instead of drawing arrows as in Dr Racket. Information is
displayed using the function(s) in the hook variable
`racket-show-functions'; it is also available when hovering the
mouse cursor.

Note: If you find these point-motion features too distracting
and/or slow, in your `racket-xp-mode-hook' you may disable them:

#+BEGIN_SRC elisp
  (require \\='racket-xp)
  (add-hook \\='racket-xp-mode-hook
            (lambda ()
              (remove-hook \\='pre-redisplay-functions
                           #\\='racket-xp-pre-redisplay
                           t)))
#+END_SRC

The remaining features discussed below will still work.

You may also use commands to navigate among a definition and its
uses, or to rename a local definitions and all its uses:

  - `racket-xp-next-definition'
  - `racket-xp-previous-definition'
  - `racket-xp-next-use'
  - `racket-xp-previous-use'

In the following little example, not only does
drracket/check-syntax distinguish the various \"x\" bindings, it
understands the two different imports of \"define\":

#+BEGIN_SRC racket
  #lang racket/base
  (define x 1)
  x
  (let ([x x])
    (+ x 1))
  (module m typed/racket/base
    (define x 2)
    x)
#+END_SRC

When point is on the opening parenthesis of an expression in tail
position, it is highlighted using the face
`racket-xp-tail-position-face'.

When point is on the opening parenthesis of an enclosing
expression with respect to which one or more expressions are in
tail position, it is highlighted using the face
`racket-xp-tail-target-face'.

Furthermore, when point is on the opening parenthesis of either
kind of expression, all of the immediately related expressions
are also highlighted. Various commands move among them:

  - `racket-xp-tail-up'
  - `racket-xp-tail-down'
  - `racket-xp-tail-next-sibling'
  - `racket-xp-tail-previous-sibling'

The function `racket-xp-complete-at-point' is added to the
variable `completion-at-point-functions'. Note that in this case,
it is not smart about submodules; identifiers are assumed to be
definitions from the file's module or its imports. In addition to
supplying completion candidates, it supports the
\":company-location\" property to inspect the definition of a
candidate and the \":company-doc-buffer\" property to view its
documentation.

When you edit the buffer, existing annotations are retained;
their positions are updated to reflect the edit. Annotations for
new or deleted text are not requested until after
`racket-xp-after-change-refresh-delay' seconds. The request is
made asynchronously so that Emacs will not block -- for
moderately complex source files, it can take some seconds simply
to fully expand them, as well as a little more time for the
drracket/check-syntax analysis. When the results are ready, all
annotations for the buffer are completely refreshed.

You may also set `racket-xp-after-change-refresh-delay' to nil
and use the `racket-xp-annotate' command manually.

The mode line changes to reflect the current status of
annotations, and whether or not you had a syntax error.

If you have one or more syntax errors, `racket-xp-next-error' and
`racket-xp-previous-error' navigate among them. Although most
languages will stop after the first syntax error, some like Typed
Racket will try to collect and report multiple errors.

You may use `xref-find-definitions' \\[xref-find-definitions],
`xref-pop-marker-stack' \\[xref-pop-marker-stack], and
`xref-find-references': `racket-xp-mode' adds a backend to the
variable `xref-backend-functions'. This backend uses information
from the drracket/check-syntax static analysis. Its ability to
find references is limited to the current file; when it finds
none it will try the default xref backend implementation which is
grep-based.

Tip: This mode follows the convention that a minor mode may only
use a prefix key consisting of \"C-c\" followed by a punctuation
key. As a result, `racket-xp-control-c-hash-keymap' is bound to
\"C-c #\" by default. Although you might find this awkward to
type, remember that as an Emacs user, you are free to bind this
map to a more convenient prefix, and/or bind any individual
commands directly to whatever keys you prefer.

\\{racket-xp-mode-map}

This is a minor mode.  If called interactively, toggle the
`Racket-Xp mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `racket-xp-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "racket-xp" '("racket-")) (register-definition-prefixes "racket-xp-complete" '("racket-")) (provide 'racket-mode-autoloads)) "sly" ((sly sly-autoloads) (define-obsolete-variable-alias 'sly-setup-contribs 'sly-contribs "2.3.2") (defvar sly-contribs '(sly-fancy) "A list of contrib packages to load with SLY.") (autoload 'sly-setup "sly" "Have SLY load and use extension modules CONTRIBS.
CONTRIBS defaults to `sly-contribs' and is a list (LIB1 LIB2...)
symbols of `provide'd and `require'd Elisp libraries.

If CONTRIBS is nil, `sly-contribs' is *not* affected, otherwise
it is set to CONTRIBS.

However, after `require'ing LIB1, LIB2 ..., this command invokes
additional initialization steps associated with each element
LIB1, LIB2, which can theoretically be reverted by
`sly-disable-contrib.'

Notably, one of the extra initialization steps is affecting the
value of `sly-required-modules' (which see) thus affecting the
libraries loaded in the Slynk servers.

If SLY is currently connected to a Slynk and a contrib in
CONTRIBS has never been loaded, that Slynk is told to load the
associated Slynk extension module.

To ensure that a particular contrib is loaded, use
`sly-enable-contrib' instead.

(fn &optional CONTRIBS)" t) (autoload 'sly-mode "sly" "Minor mode for horizontal SLY functionality.

This is a minor mode.  If called interactively, toggle the `Sly
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `sly-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'sly-editing-mode "sly" "Minor mode for editing `lisp-mode' buffers.

This is a minor mode.  If called interactively, toggle the
`Sly-Editing mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `sly-editing-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'sly "sly" "Start a Lisp implementation and connect to it.

  COMMAND designates a the Lisp implementation to start as an
\"inferior\" process to the Emacs process. It is either a
pathname string pathname to a lisp executable, a list (EXECUTABLE
ARGS...), or a symbol indexing
`sly-lisp-implementations'. CODING-SYSTEM is a symbol overriding
`sly-net-coding-system'.

Interactively, both COMMAND and CODING-SYSTEM are nil and the
prefix argument controls the precise behaviour:

- With no prefix arg, try to automatically find a Lisp.  First
  consult `sly-command-switch-to-existing-lisp' and analyse open
  connections to maybe switch to one of those.  If a new lisp is
  to be created, first lookup `sly-lisp-implementations', using
  `sly-default-lisp' as a default strategy.  Then try
  `inferior-lisp-program' if it looks like it points to a valid
  lisp.  Failing that, guess the location of a lisp
  implementation.

- With a positive prefix arg (one C-u), prompt for a command
  string that starts a Lisp implementation.

- With a negative prefix arg (M-- M-x sly, for example) prompt
  for a symbol indexing one of the entries in
  `sly-lisp-implementations'

(fn &optional COMMAND CODING-SYSTEM INTERACTIVE)" t) (autoload 'sly-connect "sly" "Connect to a running Slynk server. Return the connection.
With prefix arg, asks if all connections should be closed
before.

(fn HOST PORT &optional CODING-SYSTEM INTERACTIVE-P)" t) (autoload 'sly-hyperspec-lookup "sly" "A wrapper for `hyperspec-lookup'

(fn SYMBOL-NAME)" t) (autoload 'sly-info "sly" "Read SLY manual

(fn FILE &optional NODE)" t) (add-hook 'lisp-mode-hook 'sly-editing-mode) (register-definition-prefixes "sly" '("define-sly-" "inferior-lisp-program" "make-sly-" "sly-" "topline")) (provide 'sly-autoloads)) "popup" ((popup popup-autoloads) (register-definition-prefixes "popup" '("popup-")) (provide 'popup-autoloads)) "auto-complete" ((auto-complete auto-complete-pkg auto-complete-config auto-complete-autoloads) (autoload 'auto-complete "auto-complete" "Start auto-completion at current point.

(fn &optional SOURCES)" t) (autoload 'auto-complete-mode "auto-complete" "AutoComplete mode

This is a minor mode.  If called interactively, toggle the
`Auto-Complete mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `auto-complete-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'global-auto-complete-mode 'globalized-minor-mode t) (defvar global-auto-complete-mode nil "Non-nil if Global Auto-Complete mode is enabled.
See the `global-auto-complete-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-auto-complete-mode'.") (custom-autoload 'global-auto-complete-mode "auto-complete" nil) (autoload 'global-auto-complete-mode "auto-complete" "Toggle Auto-Complete mode in all buffers.
With prefix ARG, enable Global Auto-Complete mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Auto-Complete mode is enabled in all buffers where
`auto-complete-mode-maybe' would do it.

See `auto-complete-mode' for more information on Auto-Complete mode.

(fn &optional ARG)" t) (register-definition-prefixes "auto-complete" '("ac-" "auto-complete-mode")) (autoload 'ac-config-default "auto-complete-config" "No documentation.") (register-definition-prefixes "auto-complete-config" '("ac-")) (provide 'auto-complete-autoloads)) "ac-sly" ((ac-sly-autoloads ac-sly) (defface ac-sly-menu-face '((t (:inherit ac-candidate-face))) "Face for slime candidate menu." :group 'auto-complete) (defface ac-sly-selection-face '((t (:inherit ac-selection-face))) "Face for the slime selected candidate." :group 'auto-complete) (defvar ac-source-sly-fuzzy '((init . ac-sly-init) (candidates . ac-source-sly-fuzzy-candidates) (candidate-face . ac-sly-menu-face) (selection-face . ac-sly-selection-face) (prefix . sly-symbol-start-pos) (symbol . "l") (match lambda (prefix candidates) candidates) (document . ac-sly-documentation)) "Source for fuzzy slime completion.") (defvar ac-source-sly-simple '((init . ac-sly-init) (candidates . ac-source-sly-simple-candidates) (candidate-face . ac-sly-menu-face) (selection-face . ac-sly-selection-face) (prefix . sly-symbol-start-pos) (symbol . "l") (document . ac-sly-documentation) (match . ac-source-sly-case-correcting-completions)) "Source for slime completion.") (autoload 'set-up-sly-ac "ac-sly" "Add an optionally-fuzzy slime completion source to `ac-sources'.

(fn &optional FUZZY)" t) (register-definition-prefixes "ac-sly" '("ac-s")) (provide 'ac-sly-autoloads)) "smartparens" ((smartparens-pkg smartparens-haskell smartparens-javascript smartparens-text smartparens-racket smartparens-swift smartparens-clojure smartparens-python smartparens-latex smartparens-rst smartparens-scala smartparens-elixir smartparens-ess smartparens-rust smartparens-autoloads smartparens-ml smartparens-markdown smartparens-org smartparens-c sp-sublimetext-like smartparens-go smartparens-lua smartparens-crystal smartparens-html smartparens-erlang smartparens smartparens-config smartparens-ruby smartparens-unison) (autoload 'sp-cheat-sheet "smartparens" "Generate a cheat sheet of all the smartparens interactive functions.

Without a prefix argument, print only the short documentation and examples.

With non-nil prefix argument ARG, show the full documentation for each function.

You can follow the links to the function or variable help page.
To get back to the full list, use \\[help-go-back].

You can use `beginning-of-defun' and `end-of-defun' to jump to
the previous/next entry.

Examples are fontified using the `font-lock-string-face' for
better orientation.

(fn &optional ARG)" t) (defvar smartparens-mode-map (make-sparse-keymap) "Keymap used for `smartparens-mode'.") (autoload 'sp-use-paredit-bindings "smartparens" "Initiate `smartparens-mode-map' with `sp-paredit-bindings'." t) (autoload 'sp-use-smartparens-bindings "smartparens" "Initiate `smartparens-mode-map' with `sp-smartparens-bindings'." t) (autoload 'smartparens-mode "smartparens" "Toggle smartparens mode.

You can enable pre-set bindings by customizing
`sp-base-key-bindings' variable.  The current content of
`smartparens-mode-map' is:

 \\{smartparens-mode-map}

This is a minor mode.  If called interactively, toggle the
`Smartparens mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `smartparens-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'smartparens-strict-mode "smartparens" "Toggle the strict smartparens mode.

When strict mode is active, `delete-char', `kill-word' and their
backward variants will skip over the pair delimiters in order to
keep the structure always valid (the same way as `paredit-mode'
does).  This is accomplished by remapping them to
`sp-delete-char' and `sp-kill-word'.  There is also function
`sp-kill-symbol' that deletes symbols instead of words, otherwise
working exactly the same (it is not bound to any key by default).

When strict mode is active, this is indicated with \"/s\"
after the smartparens indicator in the mode list.

This is a minor mode.  If called interactively, toggle the
`Smartparens-Strict mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `smartparens-strict-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'smartparens-global-strict-mode 'globalized-minor-mode t) (defvar smartparens-global-strict-mode nil "Non-nil if Smartparens-Global-Strict mode is enabled.
See the `smartparens-global-strict-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `smartparens-global-strict-mode'.") (custom-autoload 'smartparens-global-strict-mode "smartparens" nil) (autoload 'smartparens-global-strict-mode "smartparens" "Toggle Smartparens-Strict mode in all buffers.
With prefix ARG, enable Smartparens-Global-Strict mode if ARG is
positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Smartparens-Strict mode is enabled in all buffers where
`turn-on-smartparens-strict-mode' would do it.

See `smartparens-strict-mode' for more information on
Smartparens-Strict mode.

(fn &optional ARG)" t) (autoload 'turn-on-smartparens-strict-mode "smartparens" "Turn on `smartparens-strict-mode'." t) (autoload 'turn-off-smartparens-strict-mode "smartparens" "Turn off `smartparens-strict-mode'." t) (put 'smartparens-global-mode 'globalized-minor-mode t) (defvar smartparens-global-mode nil "Non-nil if Smartparens-Global mode is enabled.
See the `smartparens-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `smartparens-global-mode'.") (custom-autoload 'smartparens-global-mode "smartparens" nil) (autoload 'smartparens-global-mode "smartparens" "Toggle Smartparens mode in all buffers.
With prefix ARG, enable Smartparens-Global mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Smartparens mode is enabled in all buffers where
`turn-on-smartparens-mode' would do it.

See `smartparens-mode' for more information on Smartparens mode.

(fn &optional ARG)" t) (autoload 'turn-on-smartparens-mode "smartparens" "Turn on `smartparens-mode'.

This function is used to turn on `smartparens-global-mode'.

By default `smartparens-global-mode' ignores buffers with
`mode-class' set to special, but only if they are also not comint
buffers.

Additionally, buffers on `sp-ignore-modes-list' are ignored.

You can still turn on smartparens in these mode manually (or
in mode's startup-hook etc.) by calling `smartparens-mode'." t) (autoload 'turn-off-smartparens-mode "smartparens" "Turn off `smartparens-mode'." t) (autoload 'show-smartparens-mode "smartparens" "Toggle visualization of matching pairs.  When enabled, any

matching pair is highlighted after `sp-show-pair-delay' seconds
of Emacs idle time if the point is immediately in front or after
a pair.  This mode works similarly to `show-paren-mode', but
support custom pairs.

This is a minor mode.  If called interactively, toggle the
`Show-Smartparens mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `show-smartparens-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'show-smartparens-global-mode 'globalized-minor-mode t) (defvar show-smartparens-global-mode nil "Non-nil if Show-Smartparens-Global mode is enabled.
See the `show-smartparens-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `show-smartparens-global-mode'.") (custom-autoload 'show-smartparens-global-mode "smartparens" nil) (autoload 'show-smartparens-global-mode "smartparens" "Toggle Show-Smartparens mode in all buffers.
With prefix ARG, enable Show-Smartparens-Global mode if ARG is
positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Show-Smartparens mode is enabled in all buffers where
`turn-on-show-smartparens-mode' would do it.

See `show-smartparens-mode' for more information on Show-Smartparens
mode.

(fn &optional ARG)" t) (autoload 'turn-on-show-smartparens-mode "smartparens" "Turn on `show-smartparens-mode'." t) (autoload 'turn-off-show-smartparens-mode "smartparens" "Turn off `show-smartparens-mode'." t) (register-definition-prefixes "smartparens" '("smartparens-" "sp-")) (register-definition-prefixes "smartparens-clojure" '("sp-clojure-prefix")) (register-definition-prefixes "smartparens-config" '("sp-lisp-invalid-hyperlink-p")) (register-definition-prefixes "smartparens-crystal" '("sp-crystal-")) (register-definition-prefixes "smartparens-elixir" '("sp-elixir-")) (register-definition-prefixes "smartparens-ess" '("sp-ess-")) (register-definition-prefixes "smartparens-haskell" '("sp-")) (register-definition-prefixes "smartparens-html" '("sp-html-")) (register-definition-prefixes "smartparens-latex" '("sp-latex-")) (register-definition-prefixes "smartparens-lua" '("sp-lua-post-keyword-insert")) (register-definition-prefixes "smartparens-markdown" '("sp-")) (register-definition-prefixes "smartparens-org" '("sp-")) (register-definition-prefixes "smartparens-python" '("sp-python-")) (register-definition-prefixes "smartparens-rst" '("sp-rst-point-after-backtick")) (register-definition-prefixes "smartparens-ruby" '("sp-")) (register-definition-prefixes "smartparens-rust" '("sp-")) (register-definition-prefixes "smartparens-scala" '("sp-scala-wrap-with-indented-newlines")) (register-definition-prefixes "smartparens-swift" '("sp-swift-")) (register-definition-prefixes "smartparens-text" '("sp-text-mode-")) (register-definition-prefixes "sp-sublimetext-like" '("sp-point-not-before-word")) (provide 'smartparens-autoloads)) "rainbow-delimiters" ((rainbow-delimiters rainbow-delimiters-autoloads) (autoload 'rainbow-delimiters-mode "rainbow-delimiters" "Highlight nested parentheses, brackets, and braces according to their depth.

This is a minor mode.  If called interactively, toggle the
`Rainbow-Delimiters mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `rainbow-delimiters-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'rainbow-delimiters-mode-enable "rainbow-delimiters" "Enable `rainbow-delimiters-mode'.") (autoload 'rainbow-delimiters-mode-disable "rainbow-delimiters" "Disable `rainbow-delimiters-mode'.") (register-definition-prefixes "rainbow-delimiters" '("rainbow-delimiters-")) (provide 'rainbow-delimiters-autoloads)) "w3m" ((w3m-antenna w3m-image w3m-autoloads w3m-symbol w3m-form w3m-session w3m-weather w3m-favicon mime-w3m w3m-ems w3m-rss w3m-bug w3m-lnum w3m-bookmark w3mhack w3m-util w3m-tabmenu w3m-fb w3m-proc w3m-namazu w3m w3m-cookie w3m-search w3m-filter bookmark-w3m w3m-dtree w3m-hist w3m-save w3m-perldoc w3m-mail) (autoload 'bookmark-w3m-bookmark-jump "bookmark-w3m" "Default bookmark handler for w3m buffers.

(fn BOOKMARK)") (register-definition-prefixes "bookmark-w3m" '("bookmark-w3m-")) (autoload 'mime-w3m-preview-text/html "mime-w3m" "

(fn ENTITY SITUATION)") (register-definition-prefixes "mime-w3m" '("mime-w3m-")) (autoload 'w3m-retrieve "w3m" "Retrieve web contents pointed to by URL.
It will put the retrieved contents into the current buffer.

If HANDLER is nil, this function will retrieve web contents, return
the content type of the retrieved data, and then come to an end.  This
behavior is what is called a synchronous operation.  You have to
specify HANDLER in order to make this function show its real ability,
which is called an asynchronous operation.

If HANDLER is a function, this function will come to an end in no time.
In this case, contents will be retrieved by the asynchronous process
after a while.  And after finishing retrieving contents successfully,
HANDLER will be called on the buffer where this function starts.  The
content type of the retrieved data will be passed to HANDLER as a
string argument.

NO-UNCOMPRESS specifies whether this function should not uncompress contents.
NO-CACHE specifies whether this function should not use cached contents.
POST-DATA and REFERER will be sent to the web server with a request.

(fn URL &optional NO-UNCOMPRESS NO-CACHE POST-DATA REFERER HANDLER)") (autoload 'w3m-download "w3m" "Download contents of URL to a file named FILENAME.
NO-CACHE is ignored (always download).

(fn &optional URL FILENAME NO-CACHE HANDLER POST-DATA)" t) (autoload 'w3m-goto-url "w3m" "Visit World Wide Web pages in the current buffer.

This is the primitive function of `w3m'.

If the second argument RELOAD is non-nil, reload a content of URL.
Except that if it is \\='redisplay, re-display the page without reloading.
The third argument CHARSET specifies a charset to be used for decoding
a content.
The fourth argument POST-DATA should be a string or a cons cell.
If it is a string, it makes this function request a body as if
the content-type is \"x-www-form-urlencoded\".  If it is a cons cell,
the car of a cell is used as the content-type and the cdr of a cell is
used as the body.
If the fifth argument REFERER is specified, it is used for a Referer:
field for this request.
The remaining HANDLER, ELEMENT[1], BACKGROUND, and SAVE-POS[2] are for
the internal operations of emacs-w3m.
You can also use \"quicksearch\" url schemes such as \"gg:emacs\" which
would search for the term \"emacs\" with the Google search engine.
See the `w3m-search' function and the variable `w3m-uri-replace-alist'.

Notes for the developers:
[1] ELEMENT is a history element which has already been registered in
the `w3m-history-flat' variable.  It is corresponding to URL to be
retrieved at this time, not for the url of the current page.

[2] SAVE-POS leads this function to save the current emacs-w3m window
configuration; i.e. to run `w3m-history-store-position'.
`w3m-history-store-position' should be called in a w3m-mode buffer, so
this will be convenient if a command that calls this function may be
invoked in other than a w3m-mode buffer.

(fn URL &optional RELOAD CHARSET POST-DATA REFERER HANDLER ELEMENT BACKGROUND SAVE-POS)" t) (autoload 'w3m-goto-url-new-session "w3m" "Visit World Wide Web pages in a new buffer.
Open a new tab if you use tabs, i.e., `w3m-display-mode' is set to
`tabbed' or `w3m-use-tab' is set to a non-nil value.

The buffer will get visible if BACKGROUND is nil or there is no other
emacs-w3m buffer regardless of BACKGROUND, otherwise (BACKGROUND is
non-nil) the buffer will be created but not appear to be visible.
BACKGROUND defaults to the value of `w3m-new-session-in-background',
but it could be inverted if called interactively with the prefix arg.

(fn URL &optional RELOAD CHARSET POST-DATA REFERER BACKGROUND)" t) (autoload 'w3m-gohome "w3m" "Go to the Home page." t) (autoload 'w3m-create-empty-session "w3m" "Create an empty page as a new session and visit it." t) (autoload 'w3m "w3m" "Visit World Wide Web pages using the external w3m command.

If no emacs-w3m session already exists: If POINT is at a url
string, visit that. Otherwise, if `w3m-home-page' is defined,
visit that. Otherwise, present a blank page. This behavior can be
over-ridden by setting variable `w3m-quick-start' to nil, in
which case you will always be prompted for a URL.

If an emacs-w3m session already exists: Pop to one of its windows
or frames. You can over-ride this behavior by setting
`w3m-quick-start' to nil, in order to always be prompted for a
URL.

In you have set `w3m-quick-start' to nil, but wish to over-ride
default behavior from the command line, either run this command
with a prefix argument or enter the empty string for the prompt.
In such cases, this command will visit a url at the point or,
lacking that, the URL set in variable `w3m-home-page' or, lacking
that, the \"about:\" page.

Any of five display styles are possible. See `w3m-display-mode'
for a description of those options.

You can also run this command in the batch mode as follows:

  emacs -f w3m http://emacs-w3m.namazu.org/ &

In that case, or if this command is called non-interactively, the
variables `w3m-pop-up-windows' and `w3m-pop-up-frames' will be ignored
(treated as nil) and it will run emacs-w3m at the current (or the
initial) window.

If the optional NEW-SESSION is non-nil, this function creates a new
emacs-w3m buffer.  Besides that, it also makes a new emacs-w3m buffer
if `w3m-make-new-session' is non-nil and a user specifies a url string.

The optional INTERACTIVE-P is for the internal use; it is mainly used
to check whether Emacs calls this function as an interactive command
in the batch mode.

(fn &optional URL NEW-SESSION INTERACTIVE-P)" t) (autoload 'w3m-browse-url "w3m" "Ask emacs-w3m to browse URL.
When called interactively, URL defaults to the string existing around
the cursor position and looking like a url.  If the prefix argument is
given[1] or NEW-SESSION is non-nil, create a new emacs-w3m session.
If REFRESH-IF-EXISTS is non-nil, refresh the page if it already exists
but is older than the site.

[1] More precisely the prefix argument inverts the boolean logic of
`browse-url-new-window-flag' that defaults to nil.

(fn URL &optional NEW-SESSION REFRESH-IF-EXISTS)" t) (autoload 'w3m-find-file "w3m" "Function used to open FILE whose name is expressed in ordinary format.
The file name will be converted into the file: scheme.

(fn FILE)" t) (autoload 'w3m-region "w3m" "Render the region of the current buffer between START and END.
URL specifies the address where the contents come from.  It can be
omitted or nil when the address is not identified.  CHARSET is used
for decoding the contents.  If it is nil, this function attempts to
parse the meta tag to extract the charset.

(fn START END &optional URL CHARSET)" t) (autoload 'w3m-buffer "w3m" "Render the current buffer.
See `w3m-region' for the optional arguments.

(fn &optional URL CHARSET)" t) (register-definition-prefixes "w3m" '("emacs-w3m-version" "w3m-")) (autoload 'w3m-about-antenna "w3m-antenna" "

(fn URL &optional NO-DECODE NO-CACHE POST-DATA REFERER HANDLER)") (autoload 'w3m-antenna "w3m-antenna" "Report changes of WEB sites, which is specified in `w3m-antenna-sites'.

(fn &optional NO-CACHE)" t) (register-definition-prefixes "w3m-antenna" '("w3m-antenna-")) (autoload 'w3m-bookmark-add-this-url "w3m-bookmark" "Add link under cursor to bookmark." t) (autoload 'w3m-bookmark-add-current-url "w3m-bookmark" "Add a url of the current page to the bookmark.
With prefix, ask for a new url instead of the present one.

(fn &optional ARG)" t) (autoload 'w3m-bookmark-add-all-urls "w3m-bookmark" "Add urls of all pages being visited to the bookmark." t) (autoload 'w3m-bookmark-add-current-url-group "w3m-bookmark" "Add link of the group of current urls to the bookmark." t) (autoload 'w3m-bookmark-view "w3m-bookmark" "Display the bookmark list in the current buffer.

(fn &optional RELOAD)" t) (autoload 'w3m-bookmark-view-new-session "w3m-bookmark" "Display the bookmark list in a new buffer.

(fn &optional RELOAD)" t) (autoload 'w3m-about-bookmark "w3m-bookmark" "

(fn &rest ARGS)") (autoload 'w3m-setup-bookmark-menu "w3m-bookmark" "Setup w3m bookmark items in menubar.") (register-definition-prefixes "w3m-bookmark" '("w3m-bookmark-")) (autoload 'report-emacs-w3m-bug "w3m-bug" "Report a bug in emacs-w3m.
Prompts for bug subject.  Leaves you in a mail buffer.

(fn TOPIC &optional BUFFER)" t) (register-definition-prefixes "w3m-bug" '("report-emacs-w3m-bug-")) (autoload 'w3m-cookie-shutdown "w3m-cookie" "Save cookies, and reset cookies' data.

(fn &optional INTERACTIVE-P)" t) (autoload 'w3m-cookie-set "w3m-cookie" "Register cookies which correspond to URL.
BEG and END should be an HTTP response header region on current buffer.

(fn URL BEG END)") (autoload 'w3m-cookie-get "w3m-cookie" "Get a cookie field string which corresponds to the URL.

(fn URL)") (autoload 'w3m-cookie "w3m-cookie" "Display cookies and enable you to manage them.

(fn &optional NO-CACHE)" t) (autoload 'w3m-about-cookie "w3m-cookie" "Make the html contents to display and to enable you to manage cookies.
To delete all cookies associated with amazon.com for example, do it in
the following two steps:

1. Mark them `unused' (type `C-c C-c' or press any OK button).

Limit to [amazon.com          ] <= [ ]regexp  [OK]
( )Noop  ( )Use all  (*)Unuse all  ( )Delete unused all  [OK]

2. Delete cookies that are marked `unused'.

Limit to [amazon.com          ] <= [ ]regexp  [OK]
( )Noop  ( )Use all  ( )Unuse all  (*)Delete unused all  [OK]

You can mark cookies on the one-by-one basis of course.  The `Limit-to'
string is case insensitive and allows a regular expression.

(fn URL &optional NO-DECODE NO-CACHE POST-DATA &rest ARGS)") (register-definition-prefixes "w3m-cookie" '("w3m-")) (autoload 'w3m-about-dtree "w3m-dtree" "

(fn URL &optional NODECODE ALLFILES &rest ARGS)") (autoload 'w3m-dtree "w3m-dtree" "Display directory tree on local file system.
If called with prefix argument, display all directories and files.

(fn ALLFILES PATH)" t) (register-definition-prefixes "w3m-dtree" '("w3m-dtree-")) (register-definition-prefixes "w3m-ems" '("w3m-")) (register-definition-prefixes "w3m-favicon" '("w3m-")) (defvar w3m-fb-mode nil "Non-nil if W3m-Fb mode is enabled.
See the `w3m-fb-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `w3m-fb-mode'.") (custom-autoload 'w3m-fb-mode "w3m-fb" nil) (autoload 'w3m-fb-mode "w3m-fb" "Toggle W3M Frame Buffer mode.

This allows frame-local lists of buffers (tabs).

This is a global minor mode.  If called interactively, toggle the
`W3m-Fb mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='w3m-fb-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "w3m-fb" '("w3m-fb-")) (autoload 'w3m-filter "w3m-filter" "Apply filtering rule of URL against a content in this buffer.

(fn URL)") (autoload 'w3m-toggle-filtering "w3m-filter" "Toggle whether to modify html source by the filters before rendering.
With prefix arg, prompt for a single filter to toggle (a function
toggled last will first appear) with completion.

(fn ARG)" t) (register-definition-prefixes "w3m-filter" '("w3m-filter-")) (autoload 'w3m-fontify-forms "w3m-form" "Process half-dumped data and fontify forms in this buffer.") (register-definition-prefixes "w3m-form" '("w3m-fo")) (register-definition-prefixes "w3m-hist" '("w3m-history")) (register-definition-prefixes "w3m-image" '("w3m-")) (autoload 'w3m-lnum-mode "w3m-lnum" "Minor mode to extend point commands by using Conkeror style number selection.
With prefix ARG 0 disable battery included point functions, otherwise
enable them.  With no prefix ARG - toggle.

(fn &optional ARG)" t) (autoload 'w3m-lnum-goto "w3m-lnum" "Turn on link, image and form numbers and ask for one to go to.
0 corresponds to location url." t) (autoload 'w3m-lnum-follow "w3m-lnum" "Turn on link numbers, ask for one and execute appropriate action on it.
If link - visit it, when button - press, when input - activate it,
If image - toggle it.
With prefix ARG visit link in new session or don't move over
field/button/image on activation/push/toggle.
With `-' ARG, for link image - go to it and toggle it, if link,
visit in background.  With -4 ARG, for link image - toggle it.
With double prefix ARG, prompt for url to visit.
With triple prefix ARG, prompt for url to visit in new session.

(fn ARG)" t) (autoload 'w3m-lnum-universal "w3m-lnum" "Turn on link numbers, ask for one and offer actions over it.
It will be done depending on the selection type.  Actions may be
selected either by hitting corresponding key, pressing <return> over
the action line or left clicking." t) (autoload 'w3m-lnum-toggle-inline-image "w3m-lnum" "If image at point, toggle it.
Otherwise turn on link numbers and toggle selected image.
With prefix ARG open url under image in new session.
If no such url, move over image and toggle it.

(fn &optional ARG)" t) (autoload 'w3m-lnum-view-image "w3m-lnum" "Display the image under point in the external viewer.
If no image at poing, turn on image numbers and display selected.
The viewer is defined in `w3m-content-type-alist' for every type of an
image." t) (autoload 'w3m-lnum-save-image "w3m-lnum" "Save the image under point to a file.
If no image at point, turn on image numbers and save selected.
The default name will be the original name of the image." t) (autoload 'w3m-lnum-external-view-this-url "w3m-lnum" "Launch the external browser and display the link at point.
If no link at point, turn on link numbers and open selected externally." t) (autoload 'w3m-lnum-edit-this-url "w3m-lnum" "Edit the page linked from the anchor under the cursor.
If no such, turn on link numbers and edit selected." t) (autoload 'w3m-lnum-print-this-url "w3m-lnum" "Display the url under point in the echo area and put it into `kill-ring'.
If no url under point, activate numbering and select one." t) (autoload 'w3m-lnum-download-this-url "w3m-lnum" "Download the file or the page pointed to by the link under point.
If no point, activate numbering and select andchor to download." t) (autoload 'w3m-lnum-bookmark-add-this-url "w3m-lnum" "Add link under cursor to bookmarks.
If no link under point, activate numbering and ask for one." t) (register-definition-prefixes "w3m-lnum" '("w3m-")) (register-definition-prefixes "w3m-mail" '("w3m-mail")) (autoload 'w3m-about-namazu "w3m-namazu" "

(fn URL &optional NO-DECODE NO-CACHE &rest ARGS)") (autoload 'w3m-namazu "w3m-namazu" "Search indexed files with Namazu.

(fn INDEX QUERY &optional RELOAD)" t) (register-definition-prefixes "w3m-namazu" '("w3m-namazu-")) (autoload 'w3m-about-perldoc "w3m-perldoc" "

(fn URL &optional NO-DECODE NO-CACHE &rest ARGS)") (autoload 'w3m-perldoc "w3m-perldoc" "View Perl documents.

(fn DOCNAME)" t) (register-definition-prefixes "w3m-perldoc" '("w3m-perldoc-")) (register-definition-prefixes "w3m-proc" '("w3m-process-")) (register-definition-prefixes "w3m-rss" '("w3m-rss-")) (register-definition-prefixes "w3m-save" '("w3m-save-buffer")) (autoload 'w3m-search "w3m-search" "Search QUERY using SEARCH-ENGINE.

Search results will appear in the current buffer.

When called interactively with a prefix argument, you can choose one of
the search engines defined in `w3m-search-engine-alist'.  Otherwise use
`w3m-search-default-engine'.
If Transient Mark mode, use the region as an initial string of query
and deactivate the mark.

(fn SEARCH-ENGINE QUERY)" t) (autoload 'w3m-search-new-session "w3m-search" "Like `w3m-search', but do the search in a new buffer.

(fn SEARCH-ENGINE QUERY)" t) (autoload 'w3m-search-uri-replace "w3m-search" "Generate query string for ENGINE from URI matched by last search.

(fn URI ENGINE)") (register-definition-prefixes "w3m-search" '("w3m-search-")) (autoload 'w3m-session-save "w3m-session" "Save the current session (all currently open emacs-w3m buffers).

The user will be prompted for a name for the saved session.  The
saved session information will include, for each currently open
emacs-w3m buffer: the current url and page title, and the
buffer's url history." t) (autoload 'w3m-session-crash-recovery-remove "w3m-session" "Remove crash recovery session set.") (autoload 'w3m-session-select "w3m-session" "Select session from session list.
Position point at N-th session if N is given.  With the
prefix-argument, toggles the position of the popup window between
being below or beside the main window.

(fn &optional N TOGGLE NOMSG)" t) (autoload 'w3m-setup-session-menu "w3m-session" "Setup w3m session items in menubar.") (autoload 'w3m-session-last-autosave-session "w3m-session") (autoload 'w3m-session-last-crashed-session "w3m-session") (register-definition-prefixes "w3m-session" '("w3m-")) (autoload 'w3m-replace-symbol "w3m-symbol") (register-definition-prefixes "w3m-symbol" '("w3m-")) (register-definition-prefixes "w3m-tabmenu" '("w3m-")) (register-definition-prefixes "w3m-util" '("w3m-")) (autoload 'w3m-weather "w3m-weather" "Display weather report.

(fn AREA)" t) (autoload 'w3m-about-weather "w3m-weather" "

(fn URL NO-DECODE NO-CACHE POST-DATA REFERER HANDLER)") (register-definition-prefixes "w3m-weather" '("w3m-weather-")) (provide 'w3m-autoloads)) "markdown-mode" ((markdown-mode markdown-mode-autoloads) (autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files.

(fn)" t) (add-to-list 'auto-mode-alist '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode)) (autoload 'gfm-mode "markdown-mode" "Major mode for editing GitHub Flavored Markdown files.

(fn)" t) (autoload 'markdown-view-mode "markdown-mode" "Major mode for viewing Markdown content.

(fn)" t) (autoload 'gfm-view-mode "markdown-mode" "Major mode for viewing GitHub Flavored Markdown content.

(fn)" t) (autoload 'markdown-live-preview-mode "markdown-mode" "Toggle native previewing on save for a specific markdown file.

This is a minor mode.  If called interactively, toggle the
`Markdown-Live-Preview mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `markdown-live-preview-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "markdown-mode" '("defun-markdown-" "gfm-" "markdown")) (provide 'markdown-mode-autoloads)) "tsc" ((tsc-obsolete tsc-dyn-get tsc tsc-autoloads) (register-definition-prefixes "tsc" '("tsc-")) (register-definition-prefixes "tsc-dyn-get" '("tsc-")) (provide 'tsc-autoloads)) "tree-sitter" ((tree-sitter-cli tree-sitter-hl tree-sitter-extras tree-sitter-debug tree-sitter-autoloads tree-sitter tree-sitter-load tree-sitter-query) (autoload 'tree-sitter-mode "tree-sitter" "Minor mode that keeps an up-to-date syntax tree using incremental parsing.

This is a minor mode.  If called interactively, toggle the
`tree-sitter mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `tree-sitter-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'turn-on-tree-sitter-mode "tree-sitter" "Turn on `tree-sitter-mode' in a buffer, if possible.") (put 'global-tree-sitter-mode 'globalized-minor-mode t) (defvar global-tree-sitter-mode nil "Non-nil if Global Tree-Sitter mode is enabled.
See the `global-tree-sitter-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-tree-sitter-mode'.") (custom-autoload 'global-tree-sitter-mode "tree-sitter" nil) (autoload 'global-tree-sitter-mode "tree-sitter" "Toggle Tree-Sitter mode in all buffers.
With prefix ARG, enable Global Tree-Sitter mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Tree-Sitter mode is enabled in all buffers where
`turn-on-tree-sitter-mode' would do it.

See `tree-sitter-mode' for more information on Tree-Sitter mode.

(fn &optional ARG)" t) (define-obsolete-function-alias 'tree-sitter-node-at-point 'tree-sitter-node-at-pos "2021-08-30") (autoload 'tree-sitter-node-at-pos "tree-sitter" "Return the smallest syntax node of type NODE-TYPE at POS.
NODE-TYPE may be a symbol, corresponding to a named syntax node; a string,
corresponding to an anonymous node, or a keyword, holding a special value. For
the special value `:named', return the smallest named node at POS. For the
special value `:anonymous', return the smallest anonymous node at POS. IF POS is
nil, POS defaults to the point. Unless IGNORE-INVALID-TYPE is non-nil, signal an
error when a specified named NODE-TYPE does not exist in the current grammar.
Whenever NODE-TYPE is non-nil (other than `:named'), it is possible for the
function to return nil.

(fn &optional NODE-TYPE POS IGNORE-INVALID-TYPE)") (register-definition-prefixes "tree-sitter" '("tree-sitter-")) (register-definition-prefixes "tree-sitter-cli" '("tree-sitter-cli-")) (autoload 'tree-sitter-debug-mode "tree-sitter-debug" "Toggle syntax tree debugging for the current buffer.

This mode displays the syntax tree in another buffer, and keeps it up-to-date.

This is a minor mode.  If called interactively, toggle the
`Tree-Sitter-Debug mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `tree-sitter-debug-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'tree-sitter-debug-query "tree-sitter-debug" "Execute query PATTERNS against the current syntax tree and return captures.

If the optional arg MATCHES is non-nil, matches (from `tsc-query-matches') are
returned instead of captures (from `tsc-query-captures').

If the optional arg TAG-ASSIGNER is non-nil, it is passed to `tsc-make-query' to
assign custom tags to capture names.

This function is primarily useful for debugging purpose. Other packages should
build queries and cursors once, then reuse them.

(fn PATTERNS &optional MATCHES TAG-ASSIGNER)") (register-definition-prefixes "tree-sitter-debug" '("tree-sitter-debug-")) (autoload 'tree-sitter-save-excursion "tree-sitter-extras" "Save the current location within the syntax tree; execute BODY; restore it.

If the original location cannot be restored due to the syntax tree changing too
much, this macro behaves like `save-excursion', unless
`tree-sitter-save-excursion-try-hard' is non-nil, in which case it tries to get
as close as possible to the original location.

After the location is restored, the buffer text is scrolled so that point stays
at roughly the same vertical screen position. If `pixel-scroll' is available and
`tree-sitter-save-excursion-pixelwise' is non-nil, pixelwise scrolling is used
instead, to make this restoration exact.

(fn &rest BODY)" nil t) (function-put 'tree-sitter-save-excursion 'lisp-indent-function 0) (register-definition-prefixes "tree-sitter-extras" '("tree-sitter-")) (autoload 'tree-sitter-hl-add-patterns "tree-sitter-hl" "Add custom syntax highlighting PATTERNS.
If LANG-SYMBOL is non-nil, it identifies the language that PATTERNS should be
applied to. If LANG-SYMBOL is nil, PATTERNS are applied to the current buffer,
and are prioritized over language-specific patterns. Either way, PATTERNS are
prioritized over `tree-sitter-hl-default-patterns'.

This function should be used by minor modes and configuration code. Major modes
should set `tree-sitter-hl-default-patterns' instead.

(fn LANG-SYMBOL PATTERNS)") (function-put 'tree-sitter-hl-add-patterns 'lisp-indent-function 1) (autoload 'tree-sitter-hl-mode "tree-sitter-hl" "Toggle syntax highlighting based on Tree-sitter's syntax tree.

If `tree-sitter-hl-default-patterns' is nil, turning on this mode does nothing,
and does not interfere with `font-lock-mode'.

Enabling this automatically enables `tree-sitter-mode' in the buffer.

To enable this automatically whenever `tree-sitter-mode' is enabled:

 (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

This is a minor mode.  If called interactively, toggle the
`Tree-Sitter-Hl mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `tree-sitter-hl-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "tree-sitter-hl" '("tree-sitter-hl-")) (autoload 'tree-sitter-require "tree-sitter-load" "Return the language object loaded and registered under the name LANG-SYMBOL.
If the language has not been loaded yet, load it with `tree-sitter-load'.

FILE should be the base name (without extension) of the native shared library
that exports the language as the native symbol NATIVE-SYMBOL-NAME.

If FILE is nil, the base name is assumed to be LANG-SYMBOL's name.

If NATIVE-SYMBOL-NAME is nil, the name of the exported native symbol is assumed
to be LANG-SYMBOL's name, prefixed with \"tree_sitter_\".

(fn LANG-SYMBOL &optional FILE NATIVE-SYMBOL-NAME)") (register-definition-prefixes "tree-sitter-load" '("tree-sitter-l")) (autoload 'tree-sitter-query-builder "tree-sitter-query" "Provide means for developers to write and test tree-sitter queries.

The buffer on focus when the command is called is set as the target buffer." t) (register-definition-prefixes "tree-sitter-query" '("tree-sitter-")) (provide 'tree-sitter-autoloads)) "tree-sitter-langs" ((tree-sitter-langs-autoloads tree-sitter-langs tree-sitter-langs-build) (autoload 'tree-sitter-langs--init-load-path "tree-sitter-langs" "Add the directory containing compiled grammars to `tree-sitter-load-path'.

(fn &rest ARGS)") (advice-add 'tree-sitter-load :before #'tree-sitter-langs--init-load-path) (autoload 'tree-sitter-langs--init-major-mode-alist "tree-sitter-langs" "Link known major modes to languages provided by the bundle.

(fn &rest ARGS)") (advice-add 'tree-sitter--setup :before #'tree-sitter-langs--init-major-mode-alist) (autoload 'tree-sitter-langs--set-hl-default-patterns "tree-sitter-langs" "Use syntax highlighting patterns provided by `tree-sitter-langs'.

(fn &rest ARGS)") (advice-add 'tree-sitter-hl--setup :before #'tree-sitter-langs--set-hl-default-patterns) (register-definition-prefixes "tree-sitter-langs" '("tree-sitter-langs-")) (autoload 'tree-sitter-langs-install-grammars "tree-sitter-langs-build" "Download and install the specified VERSION of the language grammar bundle.
If VERSION or OS is not specified, use the default of
`tree-sitter-langs--bundle-version' and `tree-sitter-langs--os'.

This installs the grammar bundle even if the same version was already installed,
unless SKIP-IF-INSTALLED is non-nil.

The download bundle file is deleted after installation, unless KEEP-BUNDLE is
non-nil.

(fn &optional SKIP-IF-INSTALLED VERSION OS KEEP-BUNDLE)" t) (register-definition-prefixes "tree-sitter-langs-build" '("tree-sitter-langs-")) (provide 'tree-sitter-langs-autoloads)) "aggressive-indent" ((aggressive-indent-autoloads aggressive-indent) (autoload 'aggressive-indent-indent-defun "aggressive-indent" "Indent current defun.
Throw an error if parentheses are unbalanced.
If L and R are provided, use them for finding the start and end of defun.

(fn &optional L R)" t) (autoload 'aggressive-indent-indent-region-and-on "aggressive-indent" "Indent region between L and R, and then some.
Call `aggressive-indent-region-function' between L and R, and
then keep indenting until nothing more happens.

(fn L R)" t) (autoload 'aggressive-indent-mode "aggressive-indent" "Minor mode to keep your code always indented.

This is a minor mode.  If called interactively, toggle the
`Aggressive-Indent mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `aggressive-indent-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'global-aggressive-indent-mode 'globalized-minor-mode t) (defvar global-aggressive-indent-mode nil "Non-nil if Global Aggressive-Indent mode is enabled.
See the `global-aggressive-indent-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-aggressive-indent-mode'.") (custom-autoload 'global-aggressive-indent-mode "aggressive-indent" nil) (autoload 'global-aggressive-indent-mode "aggressive-indent" "Toggle Aggressive-Indent mode in all buffers.
With prefix ARG, enable Global Aggressive-Indent mode if ARG is
positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Aggressive-Indent mode is enabled in all buffers where
`aggressive-indent-mode' would do it.

See `aggressive-indent-mode' for more information on Aggressive-Indent
mode.

(fn &optional ARG)" t) (defalias 'aggressive-indent-global-mode #'global-aggressive-indent-mode) (register-definition-prefixes "aggressive-indent" '("aggressive-indent-")) (provide 'aggressive-indent-autoloads)) "queue" ((queue-autoloads queue) (register-definition-prefixes "queue" '("make-queue" "queue-")) (provide 'queue-autoloads)) "undo-tree" ((undo-tree undo-tree-autoloads) (autoload 'undo-tree-mode "undo-tree" "Toggle undo-tree mode.

With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.

Undo-tree-mode replaces Emacs' standard undo feature with a more
powerful yet easier to use version, that treats the undo history
as what it is: a tree.

The following keys are available in `undo-tree-mode':

  \\{undo-tree-mode-map}

Within the undo-tree visualizer, the following keys are available:

  \\{undo-tree-visualizer-mode-map}

This is a minor mode.  If called interactively, toggle the
`Undo-Tree mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `undo-tree-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'global-undo-tree-mode 'globalized-minor-mode t) (defvar global-undo-tree-mode nil "Non-nil if Global Undo-Tree mode is enabled.
See the `global-undo-tree-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-undo-tree-mode'.") (custom-autoload 'global-undo-tree-mode "undo-tree" nil) (autoload 'global-undo-tree-mode "undo-tree" "Toggle Undo-Tree mode in all buffers.
With prefix ARG, enable Global Undo-Tree mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Undo-Tree mode is enabled in all buffers where
`turn-on-undo-tree-mode' would do it.

See `undo-tree-mode' for more information on Undo-Tree mode.

(fn &optional ARG)" t) (register-definition-prefixes "undo-tree" '("*undo-tree-id-counter*" "buffer-undo-tree" "turn-on-undo-tree-mode" "undo-")) (provide 'undo-tree-autoloads)) "which-key" ((which-key which-key-autoloads) (defvar which-key-mode nil "Non-nil if Which-Key mode is enabled.
See the `which-key-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `which-key-mode'.") (custom-autoload 'which-key-mode "which-key" nil) (autoload 'which-key-mode "which-key" "Toggle which-key-mode.

This is a global minor mode.  If called interactively, toggle the
`Which-Key mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='which-key-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'which-key-setup-side-window-right "which-key" "Apply suggested settings for side-window that opens on right." t) (autoload 'which-key-setup-side-window-right-bottom "which-key" "Apply suggested settings for side-window that opens on right
if there is space and the bottom otherwise." t) (autoload 'which-key-setup-side-window-bottom "which-key" "Apply suggested settings for side-window that opens on bottom." t) (autoload 'which-key-setup-minibuffer "which-key" "Apply suggested settings for minibuffer.
Do not use this setup if you use the paging commands. Instead use
`which-key-setup-side-window-bottom', which is nearly identical
but more functional." t) (autoload 'which-key-add-keymap-based-replacements "which-key" "Replace the description of KEY using REPLACEMENT in KEYMAP.
KEY should take a format suitable for use in `kbd'. REPLACEMENT
should be a cons cell of the form (STRING . COMMAND) for each
REPLACEMENT, where STRING is the replacement string and COMMAND
is a symbol corresponding to the intended command to be
replaced. COMMAND can be nil if the binding corresponds to a key
prefix. An example is

(which-key-add-keymap-based-replacements global-map
  \"C-x w\" \\='(\"Save as\" . write-file)).

For backwards compatibility, REPLACEMENT can also be a string,
but the above format is preferred, and the option to use a string
for REPLACEMENT will eventually be removed.

(fn KEYMAP KEY REPLACEMENT &rest MORE)") (autoload 'which-key-add-key-based-replacements "which-key" "Replace the description of KEY-SEQUENCE with REPLACEMENT.
KEY-SEQUENCE is a string suitable for use in `kbd'. REPLACEMENT
may either be a string, as in

(which-key-add-key-based-replacements \"C-x 1\" \"maximize\")

a cons of two strings as in

(which-key-add-key-based-replacements \"C-x 8\"
                                        \\='(\"unicode\" . \"Unicode keys\"))

or a function that takes a (KEY . BINDING) cons and returns a
replacement.

In the second case, the second string is used to provide a longer
name for the keys under a prefix.

MORE allows you to specifcy additional KEY REPLACEMENT pairs.  All
replacements are added to `which-key-replacement-alist'.

(fn KEY-SEQUENCE REPLACEMENT &rest MORE)") (autoload 'which-key-add-major-mode-key-based-replacements "which-key" "Functions like `which-key-add-key-based-replacements'.
The difference is that MODE specifies the `major-mode' that must
be active for KEY-SEQUENCE and REPLACEMENT (MORE contains
addition KEY-SEQUENCE REPLACEMENT pairs) to apply.

(fn MODE KEY-SEQUENCE REPLACEMENT &rest MORE)") (autoload 'which-key-reload-key-sequence "which-key" "Simulate entering the key sequence KEY-SEQ.
KEY-SEQ should be a list of events as produced by
`listify-key-sequence'. If nil, KEY-SEQ defaults to
`which-key--current-key-list'. Any prefix arguments that were
used are reapplied to the new key sequence.

(fn &optional KEY-SEQ)") (autoload 'which-key-show-standard-help "which-key" "Call the command in `which-key--prefix-help-cmd-backup'.
Usually this is `describe-prefix-bindings'.

(fn &optional _)" t) (autoload 'which-key-show-next-page-no-cycle "which-key" "Show next page of keys unless on the last page, in which case
call `which-key-show-standard-help'." t) (autoload 'which-key-show-previous-page-no-cycle "which-key" "Show previous page of keys unless on the first page, in which
case do nothing." t) (autoload 'which-key-show-next-page-cycle "which-key" "Show the next page of keys, cycling from end to beginning
after last page.

(fn &optional _)" t) (autoload 'which-key-show-previous-page-cycle "which-key" "Show the previous page of keys, cycling from beginning to end
after first page.

(fn &optional _)" t) (autoload 'which-key-show-top-level "which-key" "Show top-level bindings.

(fn &optional _)" t) (autoload 'which-key-show-major-mode "which-key" "Show top-level bindings in the map of the current major mode.

This function will also detect evil bindings made using
`evil-define-key' in this map. These bindings will depend on the
current evil state. 

(fn &optional ALL)" t) (autoload 'which-key-show-full-major-mode "which-key" "Show all bindings in the map of the current major mode.

This function will also detect evil bindings made using
`evil-define-key' in this map. These bindings will depend on the
current evil state. " t) (autoload 'which-key-dump-bindings "which-key" "Dump bindings from PREFIX into buffer named BUFFER-NAME.

PREFIX should be a string suitable for `kbd'.

(fn PREFIX BUFFER-NAME)" t) (autoload 'which-key-undo-key "which-key" "Undo last keypress and force which-key update.

(fn &optional _)" t) (autoload 'which-key-C-h-dispatch "which-key" "Dispatch C-h commands by looking up key in
`which-key-C-h-map'. This command is always accessible (from any
prefix) if `which-key-use-C-h-commands' is non nil." t) (autoload 'which-key-show-keymap "which-key" "Show the top-level bindings in KEYMAP using which-key.
KEYMAP is selected interactively from all available keymaps.

If NO-PAGING is non-nil, which-key will not intercept subsequent
keypresses for the paging functionality.

(fn KEYMAP &optional NO-PAGING)" t) (autoload 'which-key-show-full-keymap "which-key" "Show all bindings in KEYMAP using which-key.
KEYMAP is selected interactively from all available keymaps.

(fn KEYMAP)" t) (autoload 'which-key-show-minor-mode-keymap "which-key" "Show the top-level bindings in KEYMAP using which-key.
KEYMAP is selected interactively by mode in
`minor-mode-map-alist'.

(fn &optional ALL)" t) (autoload 'which-key-show-full-minor-mode-keymap "which-key" "Show all bindings in KEYMAP using which-key.
KEYMAP is selected interactively by mode in
`minor-mode-map-alist'." t) (register-definition-prefixes "which-key" '("evil-state" "which-key-")) (provide 'which-key-autoloads)) "avy" ((avy-autoloads avy) (autoload 'avy-process "avy" "Select one of CANDIDATES using `avy-read'.
Use OVERLAY-FN to visualize the decision overlay.
CLEANUP-FN should take no arguments and remove the effects of
multiple OVERLAY-FN invocations.

(fn CANDIDATES &optional OVERLAY-FN CLEANUP-FN)") (autoload 'avy-goto-char "avy" "Jump to the currently visible CHAR.
The window scope is determined by `avy-all-windows' (ARG negates it).

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-char-in-line "avy" "Jump to the currently visible CHAR in the current line.

(fn CHAR)" t) (autoload 'avy-goto-char-2 "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn CHAR1 CHAR2 &optional ARG BEG END)" t) (autoload 'avy-goto-char-2-above "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
This is a scoped version of `avy-goto-char-2', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR1 CHAR2 &optional ARG)" t) (autoload 'avy-goto-char-2-below "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
This is a scoped version of `avy-goto-char-2', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR1 CHAR2 &optional ARG)" t) (autoload 'avy-isearch "avy" "Jump to one of the current isearch candidates." t) (autoload 'avy-goto-word-0 "avy" "Jump to a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn ARG &optional BEG END)" t) (autoload 'avy-goto-whitespace-end "avy" "Jump to the end of a whitespace sequence.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn ARG &optional BEG END)" t) (autoload 'avy-goto-word-1 "avy" "Jump to the currently visible CHAR at a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.
When SYMBOL is non-nil, jump to symbol start instead of word start.

(fn CHAR &optional ARG BEG END SYMBOL)" t) (autoload 'avy-goto-word-1-above "avy" "Jump to the currently visible CHAR at a word start.
This is a scoped version of `avy-goto-word-1', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-word-1-below "avy" "Jump to the currently visible CHAR at a word start.
This is a scoped version of `avy-goto-word-1', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-symbol-1 "avy" "Jump to the currently visible CHAR at a symbol start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-symbol-1-above "avy" "Jump to the currently visible CHAR at a symbol start.
This is a scoped version of `avy-goto-symbol-1', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-symbol-1-below "avy" "Jump to the currently visible CHAR at a symbol start.
This is a scoped version of `avy-goto-symbol-1', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-subword-0 "avy" "Jump to a word or subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).

When PREDICATE is non-nil it's a function of zero parameters that
should return true.

BEG and END narrow the scope where candidates are searched.

(fn &optional ARG PREDICATE BEG END)" t) (autoload 'avy-goto-subword-1 "avy" "Jump to the currently visible CHAR at a subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).
The case of CHAR is ignored.

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-word-or-subword-1 "avy" "Forward to `avy-goto-subword-1' or `avy-goto-word-1'.
Which one depends on variable `subword-mode'." t) (autoload 'avy-goto-line "avy" "Jump to a line start in current buffer.

When ARG is 1, jump to lines currently visible, with the option
to cancel to `goto-line' by entering a number.

When ARG is 4, negate the window scope determined by
`avy-all-windows'.

Otherwise, forward to `goto-line' with ARG.

(fn &optional ARG)" t) (autoload 'avy-goto-line-above "avy" "Goto visible line above the cursor.
OFFSET changes the distance between the closest key to the cursor and
the cursor
When BOTTOM-UP is non-nil, display avy candidates from top to bottom

(fn &optional OFFSET BOTTOM-UP)" t) (autoload 'avy-goto-line-below "avy" "Goto visible line below the cursor.
OFFSET changes the distance between the closest key to the cursor and
the cursor
When BOTTOM-UP is non-nil, display avy candidates from top to bottom

(fn &optional OFFSET BOTTOM-UP)" t) (autoload 'avy-goto-end-of-line "avy" "Call `avy-goto-line' and move to the end of the line.

(fn &optional ARG)" t) (autoload 'avy-copy-line "avy" "Copy a selected line above the current line.
ARG lines can be used.

(fn ARG)" t) (autoload 'avy-move-line "avy" "Move a selected line above the current line.
ARG lines can be used.

(fn ARG)" t) (autoload 'avy-copy-region "avy" "Select two lines and copy the text between them to point.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil.

(fn ARG)" t) (autoload 'avy-move-region "avy" "Select two lines and move the text between them above the current line." t) (autoload 'avy-kill-region "avy" "Select two lines and kill the region between them.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil.

(fn ARG)" t) (autoload 'avy-kill-ring-save-region "avy" "Select two lines and save the region between them to the kill ring.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn ARG)" t) (autoload 'avy-kill-whole-line "avy" "Select line and kill the whole selected line.

With a numerical prefix ARG, kill ARG line(s) starting from the
selected line.  If ARG is negative, kill backward.

If ARG is zero, kill the selected line but exclude the trailing
newline.

\\[universal-argument] 3 \\[avy-kil-whole-line] kill three lines
starting from the selected line.  \\[universal-argument] -3

\\[avy-kill-whole-line] kill three lines backward including the
selected line.

(fn ARG)" t) (autoload 'avy-kill-ring-save-whole-line "avy" "Select line and save the whole selected line as if killed, but don’t kill it.

This command is similar to `avy-kill-whole-line', except that it
saves the line(s) as if killed, but does not kill it(them).

With a numerical prefix ARG, kill ARG line(s) starting from the
selected line.  If ARG is negative, kill backward.

If ARG is zero, kill the selected line but exclude the trailing
newline.

(fn ARG)" t) (autoload 'avy-setup-default "avy" "Setup the default shortcuts.") (autoload 'avy-goto-char-timer "avy" "Read one or many consecutive chars and jump to the first one.
The window scope is determined by `avy-all-windows' (ARG negates it).

(fn &optional ARG)" t) (autoload 'avy-transpose-lines-in-region "avy" "Transpose lines in the active region." t) (register-definition-prefixes "avy" '("avy-")) (provide 'avy-autoloads)) "crux" ((crux crux-autoloads) (autoload 'crux-open-with "crux" "Open visited file in default external program.
When in dired mode, open file under the cursor.

With a prefix ARG always prompt for command to use.

(fn ARG)" t) (autoload 'crux-visit-term-buffer "crux" "Create or visit a terminal buffer.
If the process in that buffer died, ask to restart." t) (autoload 'crux-visit-shell-buffer "crux" "Create or visit a shell buffer.
If the process in that buffer died, ask to restart." t) (autoload 'crux-indent-rigidly-and-copy-to-clipboard "crux" "Indent region between BEGIN and END by ARG columns and copy to clipboard.

(fn BEGIN END ARG)" t) (autoload 'crux-smart-open-line-above "crux" "Insert an empty line above the current line.
Position the cursor at its beginning, according to the current mode." t) (autoload 'crux-smart-open-line "crux" "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.

With a prefix ARG open line above the current line.

(fn ARG)" t) (autoload 'crux-smart-kill-line "crux" "Kill to the end of the line and kill whole line on the next call." t) (autoload 'crux-top-join-line "crux" "Join the current line with the line beneath it." t) (autoload 'crux-kill-whole-line "crux" "A simple wrapper around command `kill-whole-line' that respects indentation.
Passes ARG to command `kill-whole-line' when provided.

(fn &optional ARG)" t) (autoload 'crux-kill-line-backwards "crux" "Kill line backwards and adjust the indentation." t) (autoload 'crux-kill-and-join-forward "crux" "If at end of line, join with following; otherwise kill line.
Passes ARG to command `kill-line' when provided.
Deletes whitespace at join.

(fn &optional ARG)" t) (autoload 'crux-move-to-mode-line-start "crux" "Move to the beginning, skipping mode specific line start regex." t) (autoload 'crux-move-beginning-of-line "crux" "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there.

(fn ARG)" t) (autoload 'crux-indent-defun "crux" "Indent the current defun." t) (autoload 'crux-duplicate-current-line-or-region "crux" "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated.

(fn ARG)" t) (autoload 'crux-duplicate-and-comment-current-line-or-region "crux" "Duplicates and comments the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated.

(fn ARG)" t) (autoload 'crux-rename-file-and-buffer "crux" "Rename current buffer and if the buffer is visiting a file, rename it too." t) (autoload 'crux-delete-file-and-buffer "crux" "Kill the current buffer and deletes the file it is visiting." t) (autoload 'crux-copy-file-preserve-attributes "crux" "Copy the current file-visiting buffer's file to a destination.

This function prompts for the new file's location and copies it
similar to cp -p. If the new location is a directory, and the
directory does not exist, this function confirms with the user
whether it should be created. A directory must end in a slash
like `copy-file' expects. If the destination is a directory and
already has a file named as the origin file, offers to
overwrite.

If the current buffer is not a file-visiting file or the
destination is a non-existent directory but the user has elected
to not created it, nothing will be done.

When invoke with C-u, the newly created file will be visited.

(fn VISIT)" t) (autoload 'crux-view-url "crux" "Open a new buffer containing the contents of URL." t) (autoload 'crux-cleanup-buffer-or-region "crux" "Cleanup a region if selected, otherwise the whole buffer." t) (autoload 'crux-eval-and-replace "crux" "Replace the preceding sexp with its value." t) (autoload 'crux-recompile-init "crux" "Byte-compile all your dotfiles again." t) (autoload 'crux-sudo-edit "crux" "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file.

(fn &optional ARG)" t) (autoload 'crux-reopen-as-root "crux" "Find file as root if necessary.

Meant to be used as `find-file-hook'.
See also `crux-reopen-as-root-mode'.") (defvar crux-reopen-as-root-mode nil "Non-nil if Crux-Reopen-As-Root mode is enabled.
See the `crux-reopen-as-root-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `crux-reopen-as-root-mode'.") (custom-autoload 'crux-reopen-as-root-mode "crux" nil) (autoload 'crux-reopen-as-root-mode "crux" "Automatically reopen files as root if we can't write to them

as the current user.

This is a global minor mode.  If called interactively, toggle the
`Crux-Reopen-As-Root mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='crux-reopen-as-root-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'crux-insert-date "crux" "Insert a timestamp according to locale's date and time format." t) (autoload 'crux-recentf-find-file "crux" "Find a recent file using `completing-read'.
When optional argument FILTER is a function, it is used to
transform recent files before completion.

(fn &optional FILTER)" t) (autoload 'crux-recentf-find-directory "crux" "Find a recent directory using `completing-read'." t) (autoload 'crux-transpose-windows "crux" "Transpose the buffers shown in two windows.
Prefix ARG determines if the current windows buffer is swapped
with the next or previous window, and the number of
transpositions to execute in sequence.

(fn ARG)" t) (autoload 'crux-switch-to-previous-buffer "crux" "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers." t) (autoload 'crux-other-window-or-switch-buffer "crux" "Call `other-window' if more than one window is visible.
Switch to most recent buffer otherwise." t) (autoload 'crux-kill-other-buffers "crux" "Kill all buffers but the current one.
Doesn't mess with special buffers." t) (autoload 'crux-kill-buffer-truename "crux" "Kill absolute path of file visited in current buffer." t) (autoload 'crux-create-scratch-buffer "crux" "Create a new scratch buffer." t) (autoload 'crux-find-user-init-file "crux" "Edit the `user-init-file', in another window." t) (autoload 'crux-find-user-custom-file "crux" "Edit the `custom-file', in another window." t) (autoload 'crux-find-shell-init-file "crux" "Edit the shell init file in another window." t) (autoload 'crux-upcase-region "crux" "`upcase-region' when `transient-mark-mode' is on and region is active.

(fn BEG END)" t) (autoload 'crux-downcase-region "crux" "`downcase-region' when `transient-mark-mode' is on and region is active.

(fn BEG END)" t) (autoload 'crux-capitalize-region "crux" "`capitalize-region' when `transient-mark-mode' is on and region is active.

(fn BEG END)" t) (autoload 'crux-ispell-word-then-abbrev "crux" "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev.  Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer.  You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'.

(fn P)" t) (register-definition-prefixes "crux" '("crux-")) (provide 'crux-autoloads)) "expand-region" ((expand-region-pkg web-mode-expansions feature-mode-expansions python-mode-expansions sml-mode-expansions expand-region-core nxml-mode-expansions clojure-mode-expansions jsp-expansions js2-mode-expansions css-mode-expansions cc-mode-expansions python-el-expansions expand-region-autoloads ruby-mode-expansions erlang-mode-expansions js-mode-expansions octave-expansions python-el-fgallina-expansions yaml-mode-expansions cperl-mode-expansions enh-ruby-mode-expansions latex-mode-expansions er-basic-expansions text-mode-expansions html-mode-expansions the-org-mode-expansions expand-region subword-mode-expansions expand-region-custom) (register-definition-prefixes "cc-mode-expansions" '("er/")) (register-definition-prefixes "clojure-mode-expansions" '("er/")) (register-definition-prefixes "cperl-mode-expansions" '("er/")) (register-definition-prefixes "css-mode-expansions" '("er/")) (register-definition-prefixes "enh-ruby-mode-expansions" '("er/add-enh-ruby-mode-expansions")) (register-definition-prefixes "er-basic-expansions" '("er--" "er/mark-")) (register-definition-prefixes "erlang-mode-expansions" '("er/add-erlang-mode-expansions")) (autoload 'er/expand-region "expand-region" "Increase selected region by semantic units.

With prefix argument expands the region that many times.
If prefix argument is negative calls `er/contract-region'.
If prefix argument is 0 it resets point and mark to their state
before calling `er/expand-region' for the first time.

(fn ARG)" t) (autoload 'er/contract-region "expand-region-core" "Contract the selected region to its previous size.
With prefix argument contracts that many times.
If prefix argument is negative calls `er/expand-region'.
If prefix argument is 0 it resets point and mark to their state
before calling `er/expand-region' for the first time.

(fn ARG)" t) (register-definition-prefixes "expand-region-core" '("er--" "er/")) (let ((loads (get 'expand-region 'custom-loads))) (if (member '"expand-region-custom" loads) nil (put 'expand-region 'custom-loads (cons '"expand-region-custom" loads)) (put 'tools 'custom-loads (cons 'expand-region (get 'tools 'custom-loads))))) (defvar expand-region-preferred-python-mode 'python "The name of your preferred python mode") (custom-autoload 'expand-region-preferred-python-mode "expand-region-custom" t) (defvar expand-region-guess-python-mode t "If expand-region should attempt to guess your preferred python mode") (custom-autoload 'expand-region-guess-python-mode "expand-region-custom" t) (defvar expand-region-autocopy-register "" "If set to a string of a single character (try \"e\"), then the
contents of the most recent expand or contract command will
always be copied to the register named after that character.") (custom-autoload 'expand-region-autocopy-register "expand-region-custom" t) (defvar expand-region-skip-whitespace t "If expand-region should skip past whitespace on initial expansion") (custom-autoload 'expand-region-skip-whitespace "expand-region-custom" t) (defvar expand-region-fast-keys-enabled t "If expand-region should bind fast keys after initial expand/contract") (custom-autoload 'expand-region-fast-keys-enabled "expand-region-custom" t) (defvar expand-region-contract-fast-key "-" "Key to use after an initial expand/contract to contract once more.") (custom-autoload 'expand-region-contract-fast-key "expand-region-custom" t) (defvar expand-region-reset-fast-key "0" "Key to use after an initial expand/contract to undo.") (custom-autoload 'expand-region-reset-fast-key "expand-region-custom" t) (defvar expand-region-exclude-text-mode-expansions '(html-mode nxml-mode) "List of modes which derive from `text-mode' for which text mode expansions are not appropriate.") (custom-autoload 'expand-region-exclude-text-mode-expansions "expand-region-custom" t) (defvar expand-region-smart-cursor nil "Defines whether the cursor should be placed intelligently after expansion.

If set to t, and the cursor is already at the beginning of the new region,
keep it there; otherwise, put it at the end of the region.

If set to nil, always place the cursor at the beginning of the region.") (custom-autoload 'expand-region-smart-cursor "expand-region-custom" t) (define-obsolete-variable-alias 'er/enable-subword-mode? 'expand-region-subword-enabled "2019-03-23") (defvar expand-region-subword-enabled nil "Whether expand-region should use subword expansions.") (custom-autoload 'expand-region-subword-enabled "expand-region-custom" t) (register-definition-prefixes "expand-region-custom" '("expand-region-")) (register-definition-prefixes "feature-mode-expansions" '("er--block-between-keywords" "er/")) (register-definition-prefixes "html-mode-expansions" '("er--" "er/")) (register-definition-prefixes "js-mode-expansions" '("er/")) (register-definition-prefixes "js2-mode-expansions" '("er/add-js2-mode-expansions" "js2-mark-parent-statement")) (register-definition-prefixes "jsp-expansions" '("er/")) (register-definition-prefixes "latex-mode-expansions" '("er/")) (register-definition-prefixes "nxml-mode-expansions" '("er/")) (register-definition-prefixes "octave-expansions" '("er/")) (register-definition-prefixes "python-el-expansions" '("er--python-string-delimiter" "er/")) (register-definition-prefixes "python-el-fgallina-expansions" '("er--python-" "er/")) (register-definition-prefixes "python-mode-expansions" '("er--" "er/" "py-goto-beyond-clause")) (register-definition-prefixes "ruby-mode-expansions" '("er/")) (register-definition-prefixes "sml-mode-expansions" '("er/")) (register-definition-prefixes "subword-mode-expansions" '("er/")) (register-definition-prefixes "text-mode-expansions" '("er/")) (register-definition-prefixes "the-org-mode-expansions" '("er/")) (register-definition-prefixes "web-mode-expansions" '("er/add-web-mode-expansions")) (register-definition-prefixes "yaml-mode-expansions" '("er--" "er/" "yaml-indent")) (provide 'expand-region-autoloads))))

#s(hash-table size 65 test eq rehash-size 1.5 rehash-threshold 0.8125 data (org-elpa #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 15 "melpa" nil "gnu-elpa-mirror" nil "nongnu-elpa" nil "el-get" nil "emacsmirror-mirror" nil "straight" nil "use-package" nil "bind-key" nil "org" (org :type git :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git" :local-repo "org" :depth full :pre-build (straight-recipes-org-elpa--build) :build (:not autoloads) :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*"))) "org-roam" nil "dash" nil "emacsql" nil "magit-section" nil "compat" nil "seq" nil "deft" nil "vertico" nil "orderless" nil "embark" nil "embark-consult" nil "consult" nil "marginalia" nil "corfu" nil "magit" nil "git-commit" nil "transient" nil "with-editor" nil "vterm" nil "exec-path-from-shell" nil "cl-lib" nil "osx-lib" nil "projectile" nil "racket-mode" nil "sly" nil "auto-complete" nil "popup" nil "ac-sly" nil "smartparens" nil "rainbow-delimiters" nil "w3m" nil "markdown-mode" nil "tree-sitter" nil "tsc" nil "tree-sitter-langs" nil "aggressive-indent" nil "undo-tree" nil "queue" nil "which-key" nil "avy" nil "crux" nil "expand-region" nil)) melpa #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 2 "gnu-elpa-mirror" nil "nongnu-elpa" nil "el-get" (el-get :type git :flavor melpa :files ("*.el" ("recipes" "recipes/el-get.rcp") "methods" "el-get-pkg.el") :host github :repo "dimitri/el-get") "emacsmirror-mirror" nil "straight" nil "use-package" (use-package :type git :flavor melpa :files (:defaults (:exclude "bind-key.el" "bind-chord.el" "use-package-chords.el" "use-package-ensure-system-package.el") "use-package-pkg.el") :host github :repo "jwiegley/use-package") "bind-key" (bind-key :type git :flavor melpa :files ("bind-key.el" "bind-key-pkg.el") :host github :repo "jwiegley/use-package") "org-roam" (org-roam :type git :flavor melpa :files (:defaults "extensions/*" "org-roam-pkg.el") :host github :repo "org-roam/org-roam") "dash" (dash :type git :flavor melpa :files ("dash.el" "dash.texi" "dash-pkg.el") :host github :repo "magnars/dash.el") "emacsql" (emacsql :type git :flavor melpa :files (:defaults "sqlite" "emacsql-pkg.el") :host github :repo "magit/emacsql") "magit-section" (magit-section :type git :flavor melpa :files ("lisp/magit-section.el" "lisp/magit-section-pkg.el" "docs/magit-section.texi" "Documentation/magit-section.texi" "magit-section-pkg.el") :host github :repo "magit/magit") "compat" nil "seq" nil "deft" (deft :type git :flavor melpa :host github :repo "jrblevin/deft") "vertico" (vertico :type git :flavor melpa :files (:defaults "extensions/vertico-*.el" "vertico-pkg.el") :host github :repo "minad/vertico") "orderless" (orderless :type git :flavor melpa :host github :repo "oantolin/orderless") "embark" (embark :type git :flavor melpa :files ("embark.el" "embark-org.el" "embark.texi" "embark-pkg.el") :host github :repo "oantolin/embark") "embark-consult" (embark-consult :type git :flavor melpa :files ("embark-consult.el" "embark-consult-pkg.el") :host github :repo "oantolin/embark") "consult" (consult :type git :flavor melpa :host github :repo "minad/consult") "marginalia" (marginalia :type git :flavor melpa :host github :repo "minad/marginalia") "corfu" (corfu :type git :flavor melpa :files (:defaults "extensions/corfu-*.el" "corfu-pkg.el") :host github :repo "minad/corfu") "magit" (magit :type git :flavor melpa :files ("lisp/magit*.el" "lisp/git-rebase.el" "docs/magit.texi" "docs/AUTHORS.md" "LICENSE" "Documentation/magit.texi" "Documentation/AUTHORS.md" (:exclude "lisp/magit-libgit.el" "lisp/magit-libgit-pkg.el" "lisp/magit-section.el" "lisp/magit-section-pkg.el") "magit-pkg.el") :host github :repo "magit/magit") "git-commit" (git-commit :type git :flavor melpa :files ("lisp/git-commit.el" "lisp/git-commit-pkg.el" "git-commit-pkg.el") :host github :repo "magit/magit") "transient" (transient :type git :flavor melpa :host github :repo "magit/transient") "with-editor" (with-editor :type git :flavor melpa :host github :repo "magit/with-editor") "vterm" (vterm :type git :flavor melpa :files ("CMakeLists.txt" "elisp.c" "elisp.h" "emacs-module.h" "etc" "utf8.c" "utf8.h" "vterm.el" "vterm-module.c" "vterm-module.h" "vterm-pkg.el") :host github :repo "akermu/emacs-libvterm") "exec-path-from-shell" (exec-path-from-shell :type git :flavor melpa :host github :repo "purcell/exec-path-from-shell") "cl-lib" nil "osx-lib" (osx-lib :type git :flavor melpa :host github :repo "raghavgautam/osx-lib") "projectile" (projectile :type git :flavor melpa :host github :repo "bbatsov/projectile") "racket-mode" (racket-mode :type git :flavor melpa :files (:defaults "*.rkt" ("racket" "racket/*") (:exclude "racket/example/*" "racket/test/*") "racket-mode-pkg.el") :host github :repo "greghendershott/racket-mode") "sly" (sly :type git :flavor melpa :files (:defaults "lib" "slynk" "contrib" "doc/images" (:exclude "sly-autoloads.el") "sly-pkg.el") :host github :repo "joaotavora/sly") "auto-complete" (auto-complete :type git :flavor melpa :files ("*.el" "dict" "auto-complete-pkg.el") :host github :repo "auto-complete/auto-complete") "popup" (popup :type git :flavor melpa :host github :repo "auto-complete/popup-el") "ac-sly" (ac-sly :type git :flavor melpa :host github :repo "qoocku/ac-sly") "smartparens" (smartparens :type git :flavor melpa :host github :repo "Fuco1/smartparens") "rainbow-delimiters" (rainbow-delimiters :type git :flavor melpa :host github :repo "Fanael/rainbow-delimiters") "w3m" (w3m :type git :flavor melpa :files (:defaults "icons" (:exclude "octet.el" "mew-w3m.el" "w3m-xmas.el" "doc/*.texi") "w3m-pkg.el") :host github :repo "emacs-w3m/emacs-w3m") "markdown-mode" (markdown-mode :type git :flavor melpa :host github :repo "jrblevin/markdown-mode") "tree-sitter" (tree-sitter :type git :flavor melpa :files (:defaults (:exclude "lisp/tree-sitter-tests.el") "tree-sitter-pkg.el") :branch "release" :host github :repo "emacs-tree-sitter/elisp-tree-sitter") "tsc" (tsc :type git :flavor melpa :files ("core/*.el" "core/Cargo.toml" "core/Cargo.lock" "core/src" "tsc-pkg.el") :branch "release" :host github :repo "emacs-tree-sitter/elisp-tree-sitter") "tree-sitter-langs" (tree-sitter-langs :type git :flavor melpa :files (:defaults "queries" "tree-sitter-langs-pkg.el") :branch "release" :host github :repo "emacs-tree-sitter/tree-sitter-langs") "aggressive-indent" (aggressive-indent :type git :flavor melpa :host github :repo "Malabarba/aggressive-indent-mode") "undo-tree" nil "queue" nil "which-key" (which-key :type git :flavor melpa :host github :repo "justbur/emacs-which-key") "avy" (avy :type git :flavor melpa :host github :repo "abo-abo/avy") "crux" (crux :type git :flavor melpa :host github :repo "bbatsov/crux") "expand-region" (expand-region :type git :flavor melpa :host github :repo "magnars/expand-region.el"))) gnu-elpa-mirror #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 3 "nongnu-elpa" nil "emacsmirror-mirror" nil "straight" nil "compat" (compat :type git :host github :repo "emacs-straight/compat" :files ("*" (:exclude ".git"))) "seq" nil "cl-lib" nil "undo-tree" (undo-tree :type git :host github :repo "emacs-straight/undo-tree" :files ("*" (:exclude ".git"))) "queue" (queue :type git :host github :repo "emacs-straight/queue" :files ("*" (:exclude ".git"))))) nongnu-elpa #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 4 "emacsmirror-mirror" nil "straight" nil "seq" nil "cl-lib" nil)) el-get #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 2 "emacsmirror-mirror" nil "straight" nil "seq" nil "cl-lib" nil)) emacsmirror-mirror #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 2 "straight" (straight :type git :host github :repo "emacsmirror/straight") "seq" nil "cl-lib" nil))))

("expand-region" "crux" "avy" "which-key" "queue" "undo-tree" "aggressive-indent" "tree-sitter-langs" "tsc" "tree-sitter" "markdown-mode" "w3m" "rainbow-delimiters" "smartparens" "ac-sly" "popup" "auto-complete" "sly" "racket-mode" "projectile" "osx-lib" "cl-lib" "exec-path-from-shell" "vterm" "with-editor" "transient" "git-commit" "magit" "corfu" "marginalia" "consult" "embark-consult" "embark" "orderless" "vertico" "deft" "seq" "compat" "magit-section" "emacsql" "dash" "org-roam" "org" "bind-key" "use-package" "emacs" "straight" "emacsmirror-mirror" "el-get" "nongnu-elpa" "gnu-elpa-mirror" "melpa" "org-elpa")

t
