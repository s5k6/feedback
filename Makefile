outputdir = out

.PHONY : all clean distclean

all : feedback

feedback : Feedback.lhs Tabular.lhs
	ghc --make -outputdir $(outputdir) -o feedback Feedback.lhs
	strip feedback

install : feedback
	cp feedback ~/opt/bin/

clean :
	rm -rf $(outputdir)

distclean : clean
	rm -f feedback
