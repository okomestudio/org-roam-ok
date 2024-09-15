# org-roam-plugin-ok

An [org-roam](https://github.com/org-roam/org-roam) plug-in minor mode
for better PKM in Emacs.

The enhancements include:

- Informative minibuffer lines
  - Title-alias distinction
  - Parent-child connection between org-roam file nodes
- Japanese-aware unlinked references parsing
- Use of hyphen for title slug
- Automatic creation of missing parent directories
- In-memory caching for faster node lookup
- etc.

This is a packaged version of my `org-roam` customization. As the
related `init.el` section has grown large, I turned it into a package
to tidy up the initialization.

## Installation

To install via `use-package`, have the following lines in your `init.el`:

``` emacs-lisp
(use-package org-roam-plugin-ok
  :straight (:host github :repo "okomestudio/org-roam-plugin-ok")
  :init
  (use-package ok-plural
    :straight (:host github :repo "okomestudio/ok-plural.el")
    :demand t)
  (org-roam-plugin-ok-on-idle-init-setup))
```

Running the `org-roam-plugin-ok-on-idle-init-setup` function is
optional; it simply load the minor mode and fill the in-memory cache
to speed up the very first node query. Otherwise, use the
`org-roam-plugin-ok-mode` function explicitly to enable the minor
mode.

## Usage

To use the enhanced unlinked references section in the org-roam
buffer, add `orp-ok-ja-unlinked-refrences-section` to the
`org-roam-mode-sections` variable, e.g.:

``` emacs-lisp
(setopt org-roam-mode-sections
        (list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              #'orp-ok-ja-unlinked-references-section))
```

## Development

The feature is named `org-roam-plugin-ok`, but the shorter prefix
`orp-ok` is used within the code base.
