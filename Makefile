
all: parser.c al.c al 

al.c: al.l
	flex --outfile=al.c al.l

parser.c: parser.y	
	bison --yacc -v --defines --output=parser.c parser.y

al:
	gcc -o al al.c parser.c

clean: 
	touch *
	rm parser.h
	rm parser.c
	rm al.c
	rm al
	
