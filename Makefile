OWLVER=0.1.15
OWLURL=https://github.com/aoh/owl-lisp/releases/download/v$(OWLVER)

everything: ed
	test/run ./ed

ed: ed.c
	cc -O2 -o ed ed.c

ed.c: ed.scm ol
	./ol -o ed.c ed.scm

ol:
	test -f ol-$(OWLVER).c.gz || wget $(OWLURL)/ol-$(OWLVER).c.gz
	gzip -d < ol-$(OWLVER).c.gz > ol-$(OWLVER).c
	cc -O2 -o ol ol-$(OWLVER).c

clean:
	-rm ed.c ed
	-rm ol-$(OWLVER).c ol

mrproper: clean
	-rm ol-$(OWLVER).c.gz
