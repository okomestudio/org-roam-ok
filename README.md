# org-roam-plugin-ja

This package supplies the plug-ins for those using Japanese with Org
Roam.

To install this package via `use-package`, have the following lines in
your `init.el`:

``` emacs-lisp
(use-package org-roam-plugin-ja
    :straight (:host github :repo "okomestudio/org-roam-plugin-ja")
    :demand t)
```

To use the enhanced unlinked references section in the Org Roam
buffer, add `org-roam-plugin-ja-unlinked-refrences-section` to the
`org-roam-mode-sections` variable, e.g.:

``` emacs-lisp
(setopt org-roam-mode-sections
        (list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              #'org-roam-plugin-ja-unlinked-references-section))
```
