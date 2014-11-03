outputdir = out
targets = feedback grades

.PHONY : all clean distclean

all : $(targets)

feedback : Feedback.lhs Tabular.lhs
	ghc --make -outputdir $(outputdir) -o feedback Feedback.lhs
	strip feedback

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
