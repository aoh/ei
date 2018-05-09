OWLVER=0.1.15
OWLURL=https://github.com/aoh/owl-lisp/releases/download/v$(OWLVER)

everything: ed
	# compare against reference ed if available
	-test -x /bin/ed && test/run ed
	# test this ed
	test/run ./ed

ed: ed.c
	cc -O2 -o ed ed.c

ed.c: ed.scm ol
	./ol -O1 -o ed.c ed.scm

ol:
	echo $(OWLURL)/ol-$(OWLVER).c.gz
	test -f ol-$(OWLVER).c.gz || wget $(OWLURL)/ol-$(OWLVER).c.gz
	gzip -d < ol-$(OWLVER).c.gz > ol-$(OWLVER).c
	cc -O2 -o ol ol-$(OWLVER).c
