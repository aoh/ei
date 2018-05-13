OWLVER=0.1.15
OWLURL=https://github.com/aoh/owl-lisp/releases/download/v$(OWLVER)

everything: ei
	test/run ./ei

ei: ei.c
	cc -O2 -o ei ei.c

ei.c: ei.scm ol
	./ol -o ei.c ei.scm

ol:
	test -f ol-$(OWLVER).c.gz || wget $(OWLURL)/ol-$(OWLVER).c.gz
	gzip -d < ol-$(OWLVER).c.gz > ol-$(OWLVER).c
	cc -O2 -o ol ol-$(OWLVER).c

clean:
	-rm ei.c ei
	-rm ol-$(OWLVER).c ol

mrproper: clean
	-rm ol-$(OWLVER).c.gz
