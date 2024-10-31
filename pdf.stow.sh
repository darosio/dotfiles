#!/usr/bin/env sh
#

# mkdir -p "$HOME"/.config/gh
stow -t "$HOME" pdf
yay -S --noconfirm diffpdf
yay -S --noconfirm pdfcrack
# Conversion: img2pdf (for converting images to PDF), pdf2svg (for PDF to SVG).
yay -S --noconfirm img2pdf
yay -S --noconfirm pdf2svg
# Search: pdfgrep for searching within PDFs.
yay -S --noconfirm pdfgrep
# OCR and Scanning: gscan2pdf (adds OCR to scanned documents).
yay -S --noconfirm tesseract-data-eng
yay -S --noconfirm tesseract-data-ita
yay -S --noconfirm gscan2pdf
yay -S --noconfirm simple-scan
yay -S --noconfirm paperwork
# PDF Arrangement: pdfarranger, pdfmixtool, pdfslicer, and pdftricks (merging, splitting, and rearranging).
yay -S --noconfirm pdfarranger
yay -S --noconfirm pdfmixtool
yay -S --noconfirm pdfslicer
yay -S --noconfirm pdftricks
#   http://www.pdflabs.com/docs/pdftk-cli-examples/
yay -S --noconfirm pdftk        # - pdftk A=HP0092.pdf B=HP0093.pdf shuffle A Bend-1 output 730-anna-2012.pdf
yay -S --noconfirm qpdf
# Annotation
yay -S --noconfirm xournalpp
# Advanced Search and Data Extraction: pdfminer.six is another Python tool, particularly strong in text extraction for structured documents.
# [poppler] pdfimages *.pdf ./dirIMAGES [-j (jpg) -opw -upw (passwd) -f 10 -l 11 (pages #)
yay -S --noconfirm python-pdfminer
# Print
yay -S --noconfirm python-weasyprint
yay -S --noconfirm wkhtmltopdf-static  # AUR
# Ppresenter
yay -S --noconfirm pdfpc
