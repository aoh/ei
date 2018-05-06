ed: ed.c
	cc -O2 -o ed ed.c

ed.c: ed.scm tmp/owl-lisp/bin/vm
	tmp/owl-lisp/bin/ol -o ed.c ed.scm

tmp/owl-lisp:
	mkdir -p tmp
	cd tmp; git clone https://github.com/aoh/owl-lisp.git

tmp/owl-lisp/bin/ol: tmp/owl-lisp
	cd tmp/owl-lisp && make

