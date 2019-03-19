
all: al.c al

al.c: al.l
	flex --outfile=al.c al.l

al:
	gcc -o al al.c

clean: 
	touch *
	rm al
	rm al.c
