(require 'ox-latex)

(setq org-latex-default-packages-alist
      '(("AUTO" "inputenc" t ("pdflatex"))
	("T1" "fontenc" t ("pdflatex"))
	("" "fontspec" t ("lualatex" "xelatex"))
	("" "graphicx" nil)
	("" "hyperref" nil)))

(provide 'init-pdf-ox)
