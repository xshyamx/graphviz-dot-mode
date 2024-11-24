# Grapvhiz Dot Mode #

Major mode for [Graphviz](https://graphviz.org/) files.

Supports

- Xref navigation of component definitions & references
- Generating png preview files using the `dot` executable

Adds the following keybindings

| Keybinding | Feature |
|------------|---------|
| <kbd>C-c C-c</kbd> | Generate preview file |
| <kbd>C-c C-o</kbd> | Generate preview file and open it |
| <kbd>C-c !</kbd> | Inserts the bootstrap template |
| <kbd>C-c i r</kbd> | Inserts a record |
| <kbd>C-c i c</kbd> | Inserts a cylinder |
| <kbd>C-c i n</kbd> | Inserts a node |

## Installation ##

### Pre-requisites ###

1. Install the latest Grapvhiz executable package with the `dot` executable

### For Emacs ###

1. Clone the repository into site-lisp in the emacs user directory

	``` sh
	git clone https://github.com/xshyamx/graphviz-dot-mode \
	    $HOME/.emacs.d/site-lisp/graphviz-dot-mode
	```

2. Add to `load-path`

    ```emacs-lisp
    (add-to-list
    	'load-path
    	(expand-file-name "site-lisp/graphviz-dot-mode" user-emacs-directory))
    ```

3. Load & configure

	```emacs-lisp
	(require 'graphviz-dot-mode)
	(setq graphviz-dot-cmd
		   "c:/software/graphviz/bin/dot.exe"))
	```
