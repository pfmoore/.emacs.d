;; Find an available font, from https://www.emacswiki.org/emacs/SetFonts
(require 'cl)
(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (find-if (lambda (f) (find-font (font-spec :name f))) fonts))

(setq pfm-default-font
      (font-candidate
       "Wibble-12"
       "DejaVu Sans Mono-12"
       "Consolas-12"
       "Courier-12"))


(provide 'pfm-font)
