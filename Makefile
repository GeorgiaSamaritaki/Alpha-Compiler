
all: clean parser.cpp al.cpp al 

al.cpp: al.l
	flex --outfile=al.cpp al.l

parser.cpp: parser.y	
	bison --yacc -v -d -t --defines --output=parser.cpp parser.y

al:
	g++ -Wno-write-strings -o al parser.cpp al.cpp 
tests:
	al Tests/Errors/Error0.asc
	al Tests/Errors/Error1.asc
	al Tests/Errors/Error2.asc
	al Tests/Errors/Error3.asc
	al Tests/Errors/Error4.asc
	al Tests/Errors/Error5.asc
	al Tests/Errors/Error6.asc
	al Tests/Errors/Error7.asc
	al Tests/Errors/Error8.asc
	al Tests/Errors/Error9.asc
	al Tests/Errors/Error10.asc
	al Tests/Errors/Error11.asc
	al Tests/Errors/Error12.asc
	al Tests/Working/Anonymous.asc                
	al Tests/Working/Circle.asc                   
	al Tests/Working/Grammar.asc                  
	al Tests/Working/ShadowedNameOffunctions.asc  
	al Tests/Working/Tree.asc
	al Tests/Working/Block.asc                    
	al Tests/Working/GlobalAndLocal.asc           
	al Tests/Working/Random.asc                   
	al Tests/Working/Simple.asc


clean: 	
	touch *
	-rm parser.hpp
	-rm parser.cpp
	-rm al.cpp
	-rm al
	-rm parser.output
	clear
