outputdir = out
targets = feedback grades

.PHONY : all clean distclean test

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

test : feedback
	./feedback \
	  overview=demo/overview feedback=demo/group/%/punkte \
	  reqdTotal=50 reqdEach=10 maxLow=3 \
	  maxPoints=demo/maxPoints \
	  groups=demo/groups \
	  demo/tut{1,2}
	less -x30,38 demo/overview
