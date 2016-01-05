#!/bin/sh
/usr/texbin/xelatex $(SRCROOT)FPSummary.tex && open $(SRCROOT)FPSummary.pdf

# or if the pdf file is already open in preview

# /usr/texbin/xelatex $(SRCROOT)FPSummary.tex && open "/Application/Preview.app"
