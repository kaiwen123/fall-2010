# This is the makefile for artifitial intelligence fall 2011. 
# Author Simon Guo. 
# Instructor Shaojun Wang. 

TMPS=*.aux *.log *~ *.dvi homework1.pdf homework2.pdf homework3.pdf final.pdf
SRC_HWK1=homework1.tex AI-HWK-1_11.pdf AI-HWK-1_1.pdf \
	AI-HWK-1_3c.pdf AI-HWK-1_3.pdf AI-HWK-1_4b.pdf \
	AI-HWK-1_4c.pdf AI-HWK-1_8a.pdf 

SRC_HWK2=homework2.tex AI-HWK-2_14_11.pdf \
	 AI-HWK-2_14_14.pdf AI-HWK-2_14_1.pdf

SRC_HWK3=homework3.tex AI-HWK3_1c.pdf

SRC_FINAL=final.tex AI-FINAL-2_1.pdf AI-FINAL-3_1.pdf

hwk1 : $(SRC_HWK1)
	pdflatex $< #-o homework1.pdf

hwk2 : $(SRC_HWK2)
	pdflatex $< #-o homework2.pdf

hwk3 : $(SRC_HWK3)
	pdflatex $< # -o $@.pdf

final: $(SRC_FINAL)
	pdflatex $< -o final.pdf

clean: 
	@rm -rf $(TMPS)