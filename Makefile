
all: clean parser.cpp al.cpp al 

al.cpp: al.l
	flex --outfile=al.cpp al.l

parser.cpp: parser.y	
	bison --yacc --debug -v -d -t --defines --output=parser.cpp parser.y

al:
	g++ -Wno-write-strings -o al parser.cpp al.cpp 
	
test:
	al tests/Working/Anonymous.asc                
	al tests/Working/Circle.asc                   
	al tests/Working/Grammar.asc                  
	al tests/Working/ShadowedNameOffunctions.asc  
	al tests/Working/Tree.asc
	al tests/Working/Block.asc                    
	al tests/Working/GlobalAndLocal.asc           
	al tests/Working/Random.asc                   
	al tests/Working/Simple.asc
	al tests/Errors/Error0.asc
	al tests/Errors/Error1.asc
	al tests/Errors/Error2.asc
	al tests/Errors/Error3.asc
	al tests/Errors/Error4.asc
	al tests/Errors/Error5.asc
	al tests/Errors/Error6.asc
	al tests/Errors/Error7.asc
	al tests/Errors/Error8.asc
	al tests/Errors/Error9.asc
	al tests/Errors/Error10.asc
	al tests/Errors/Error11.asc
	al tests/Errors/Error12.asc


clean: 	
	touch *
	-rm parser.hpp
	-rm parser.cpp
	-rm al.cpp
	-rm al
	-rm parser.output
	-rm quads.txt
	clear
