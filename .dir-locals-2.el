((latex-mode . ((eval . (progn
                          (defvar latexrun-mode-map (make-sparse-keymap)
                            "Keymap while context-mode is active.")
                          (define-minor-mode latexrun-mode
                            "Remap "
                            nil
                            :lighter "latexrun"
                            latexrun-mode-map)
                          (latexrun-mode 1)
                          (defun latexrun ()
                            (interactive)
                            (save-buffer)
                            (evil-make nil))
                          (spacemacs/set-leader-keys-for-minor-mode 'latexrun-mode
                            "b" 'latexrun)
                          ))))
 (latex-mode . ((fill-column . 60)))
 (latex-mode . ((TeX-master . "paper"))))
