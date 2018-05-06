ed: ed.c
	cc -O1 -o ed ed.c

ed.c: ed.scm
	ol -o ed.c ed.scm

clean:
	-rm ed ed.c

