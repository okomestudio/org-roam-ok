# org-roam-plugin-ok

An [org-roam](https://github.com/org-roam/org-roam) plug-in for better
PKM in Emacs.

The enhancements include:

- Informative minibuffer lines
  - Title-alias distinction
  - Parent-child connection between org-roam file nodes
- Japanese-aware unlinked references parsing
- Use of hyphen for title slug
- Automatic creation of missing parent directories
- etc.

This is a packaged version of my customization of org-roam. As the
related `init.el` section has grown large, I turned it into a package
to tidy up the initialization.

## Installation

To install via `use-package`, have the following lines in your `init.el`:

``` emacs-lisp
(use-package org-roam-plugin-ja
    :straight (:host github :repo "okomestudio/org-roam-plugin-ok")
    :demand t)
```

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

## TODOs

- [ ] Make the module a minor mode
