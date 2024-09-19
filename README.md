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
(use-package org-roam-timestamps
  :after org-roam)

(use-package ok-plural
  :straight (ok-plural :host github :repo "okomestudio/ok-plural.el"))

(use-package org-roam-plugin-ok
  :straight (:host github :repo "okomestudio/org-roam-plugin-ok")
  :init (org-roam-plugin-ok-on-idle-init-setup))
```

Running the `org-roam-plugin-ok-on-idle-init-setup` function is
optional; it simply loads the minor mode and fill the in-memory cache
to speed up the very first node query. Otherwise, use the
`org-roam-plugin-ok-mode` function explicitly to enable the minor
mode, e.g.,

``` emacs-lisp
(use-package org-roam
  ...
  :config
  ...
  (org-roam-plugin-ok-mode 1))
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

## Customization

- `org-roam-plugin-ok-on-idle-delay` (default: 60): Idle time delay in
  second before the on-idle initializer runs when
  `org-roam-plugin-ok-on-idle-init-setup` has been run

## Development

The feature is named `org-roam-plugin-ok`, but the shorter prefix
`orp-ok` is used within the code base.

## TODOs

- [ ] Document enhancements in more detail
- [ ] Make `org-roam-timestamps` dependency optional
