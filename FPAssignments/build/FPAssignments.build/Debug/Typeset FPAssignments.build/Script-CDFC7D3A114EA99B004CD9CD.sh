#!/bin/sh
/usr/local/texlive/2010/bin/universal-darwin/pdflatex $(SRCROOT)FPAssignments.tex && open $(SRCROOT)FPAssignments.pdf

# or if the pdf file is already open in preview

# /usr/local/texlive/2010/bin/universal-darwin/pdflatex $(SRCROOT)FPAssignments.tex && open "/Application/Preview.app"
