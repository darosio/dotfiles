#!/usr/bin/env sh
#
yay -S --noconfirm texlive-basic
yay -S --noconfirm texlive-latex
yay -S --noconfirm texlive-latexrecommended
yay -S --noconfirm texlive-latexextra
yay -S --noconfirm texlive-fontsrecommended
yay -S --noconfirm texlive-fontsextra
yay -S --noconfirm texlive-xetex
yay -S --noconfirm # texlive-bibtexextra contains the BibLaTeX package and additional BibTeX styles and bibliography databases.
yay -S --noconfirm texlive-mathscience
yay -S --noconfirm biber
# ln -s /usr/share/texmf-dist/fonts/opentype/public/<some_fonts_you_want> ~/.fonts/OTF/ (or TTF or Type1)
# fc-cache ~/.fonts
# mkfontscale ~/.fonts/OTF (or TTF or Type1)
# mkfontdir ~/.fonts/OTF (or TTF or Type1)
#   - java-environment: to use pdfannotextractor [installed]
# latex-mk	<http://latex-mk.sourceforge.net/examples.html>
# lacheck
# lgrind (progs format in latex)
