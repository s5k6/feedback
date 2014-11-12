outputdir = out
targets = feedback grades

.PHONY : all clean distclean test

all : $(targets)

feedback : Feedback.lhs Tabular.lhs out/Help.hs
	ghc --make -outputdir $(outputdir) -o feedback Feedback.lhs out/Help.hs
	strip feedback

out/Help.hs : help.txt help.sed
	mkdir out
	sed -r -f help.sed help.txt >$@

grades : Grades.lhs Tabular.lhs
	ghc --make -outputdir $(outputdir) -o grades Grades.lhs
	strip grades

install : $(targets)
	cp feedback ~/opt/bin/
	cp grades ~/opt/bin/

clean :
	rm -rf $(outputdir)

distclean : clean
	rm -f $(targets)

test : feedback
	demo/feedback
	x less -Sx30,38 demo/total_punkte
