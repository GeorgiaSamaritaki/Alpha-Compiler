
all: clean parser.cpp al.cpp al 

al.cpp: al.l
	flex --outfile=al.cpp al.l

parser.cpp: parser.y	
	bison --yacc -v --defines --output=parser.cpp parser.y

al:
	g++ -Wno-write-strings -o al parser.cpp al.cpp symtable.cpp

clean: 	
	touch *
	-rm parser.hpp
	-rm parser.cpp
	-rm al.cpp
	-rm al
	-rm parser.output
	clear
