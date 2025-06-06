# org-roam-ok

An [org-roam](https://github.com/org-roam/org-roam) plug-in minor mode for better PKM in
Emacs.

The enhancements include:

- Informative minibuffer lines
  - Title-alias distinction
  - Parent-child connection between org-roam file nodes
- Use of hyphen for title slug
- Automatic creation of missing parent directories
- In-memory caching for faster node lookup
- etc.

This is a packaged version of my `org-roam` customization. As the related `init.el`
section has grown large, I turned it into a package to tidy up the initialization.

## Installation

To install via `use-package`, have the following lines in your `init.el`:

``` emacs-lisp
(use-package org-roam-timestamps
  :after org-roam)

(use-package org-roam-ok
  :straight (:host github :repo "okomestudio/org-roam-ok")
  :init (org-roam-ok-on-idle-init-setup))
```

Running the `org-roam-ok-on-idle-init-setup` function is optional; it simply loads the
minor mode and fill the in-memory cache to speed up the very first node query. Otherwise,
use the `org-roam-ok-mode` function explicitly to enable the minor mode, e.g.,

``` emacs-lisp
(use-package org-roam
  ...
  :config
  ...
  (org-roam-ok-mode 1))
```

## Customization

- `org-roam-ok-on-idle-delay` (default: 60): Idle time delay in second before the on-idle
  initializer runs when `org-roam-ok-on-idle-init-setup` has been run

## Development

TBD.

## TODOs

- [ ] Document enhancements in more detail
- [ ] Make `org-roam-timestamps` dependency optional
