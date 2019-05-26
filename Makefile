
all: clean parser.cpp al.cpp al avm

al.cpp: al.l
	flex --outfile=al.cpp al.l

parser.cpp: parser.y	
	bison --yacc --debug -v -d -t --defines --output=parser.cpp parser.y

al:
	g++ -Wno-write-strings -o al parser.cpp al.cpp 
	
avm: main.cpp
	g++ -Wno-write-strings -o avm main.cpp 

test:
	al tests/phase2/Working/Anonymous.asc                
	al tests/phase2/Working/Circle.asc                   
	al tests/phase2/Working/Grammar.asc        	          
	al tests/phase2/Working/ShadowedNameOffunctions.asc  
	al tests/phase2/Working/Block.asc                    
	al tests/phase2/Working/GlobalAndLocal.asc           
	al tests/phase2/Working/Random.asc                   
	al tests/phase2/Working/Simple.asc
	al tests/phase2/Errors/Error0.asc
	al tests/phase2/Errors/Error1.asc
	al tests/phase2/Errors/Error2.asc
	al tests/phase2/Errors/Error3.asc
	al tests/phase2/Errors/Error4.asc
	al tests/phase2/Errors/Error5.asc
	al tests/phase2/Errors/Error6.asc
	al tests/phase2/Errors/Error7.asc
	al tests/phase2/Errors/Error8.asc
	al tests/phase2/Errors/Error9.asc
	al tests/phase2/Errors/Error10.asc
	al tests/phase2/Errors/Error11.asc
	al tests/phase2/Errors/Error12.asc
	al tests/phase3/backpatch0.asc
	al tests/phase3/backpatch1.asc
	al tests/phase3/backpatch2.asc
	al tests/phase3/backpatch3.asc
	al tests/phase3/backpatch.asc
	al tests/phase3/p3t_assignments_complex.asc
	al tests/phase3/p3t_assignments_objects.asc
	al tests/phase3/p3t_assignments_simple.asc
	al tests/phase3/p3t_basic_expr.asc
	al tests/phase3/p3t_calls.asc
	al tests/phase3/p3t_const_maths.asc
	al tests/phase3/p3t_flow_control.asc
	al tests/phase3/p3t_flow_control_error.asc
	al tests/phase3/p3t_funcdecl.asc
	al tests/phase3/p3t_if_else.asc
	al tests/phase3/p3t_object_creation_expr.asc
	al tests/phase3/p3t_relational.asc
	al tests/phase3/p3t_var_maths.asc
	al tests/phase3/t.asc
	al tests/phase3/vavouris1.asc
	al tests/phase3/vavouris.asc

tests:	al tests/phase4-5/basic_complex.asc
		al tests/phase4-5/basic_simple.asc       
		al tests/phase4-5/queens.asc            
		al tests/phase4-5/tables3.asc
		al tests/phase4-5/funcs.asc              
		al tests/phase4-5/Random.asc             
		al tests/phase4-5/tables_bonus.asc
		al tests/phase4-5/calc.asc               
		al tests/phase4-5/hercules.asc           
		al tests/phase4-5/ShadowedFunctions.asc  
		al tests/phase4-5/Tree1.asc
		al tests/phase4-5/Circle.asc             
		al tests/phase4-5/libfuncs.asc           
		al tests/phase4-5/tables1.asc            
		al tests/phase4-5/Tree2.asc
		al tests/phase4-5/delegation.asc         
		al tests/phase4-5/line_point.asc         
		al tests/phase4-5/tables2.asc            
		al tests/phase4-5/visitor.asc
		al tests/phase4-5/err1.asc               
		al tests/phase4-5/err2.asc              
		al tests/phase4-5/err3.asc               
		al tests/phase4-5/err4.asc               
		al tests/phase4-5/err5.asc               
		al tests/phase4-5/err6.asc              
		al tests/phase2/Working/Tree.asc

clean: 	
	touch *
	-rm parser.hpp
	-rm parser.cpp
	-rm al.cpp
	-rm al
	-rm parser.output
	-rm quads.txt
	-rm tcode.txt
	-rm *.abc
	-rm avm
	clear
