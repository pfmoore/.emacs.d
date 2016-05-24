;; Find an available font, from https://www.emacswiki.org/emacs/SetFonts
(require 'cl)
(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (find-if (lambda (f) (find-font (font-spec :name f))) fonts))

(setq pfm-default-font
      (font-candidate
       "DejaVu Sans Mono-12"
       "Consolas-12"
       "Courier-12"))

(add-to-list 'default-frame-alist (cons 'font pfm-default-font))
(set-default-font pfm-default-font)

(provide 'pfm-font)
