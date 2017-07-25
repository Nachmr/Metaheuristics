########################################################################
##
## Prácticas IG, curso 2014-15
## Carlos Ureña, Oct-2014
## 
## archivo 'make' para compilar, enlazar y ejecutar
## invocar von 'make'
## genera archivo ejecutable 'prac', usando todos los .cpp/.cc/.c presentes
##
########################################################################

.SUFFIXES:
.PHONY: start, exec, all, compile, clean, tar

target_name         := prac
opt_dbg_flag        := -g
exit_first          := -Wfatal-errors
warn_all            := -Wall

units_ext           := $(wildcard *.cpp *.cc *.c)
headers             := $(wildcard *.hpp *.hh *.h)



units               := $(basename $(units_ext))
objs                := $(addsuffix .o, $(notdir $(units)))
c_flags             := -I $(src_dir) $(opt_dbg_flag) $(exit_first) $(warn_all)
ld_libs             := $(gtk_ld_libs) 

start:
	@make --no-print-directory exec

exec: $(target_name)
	@echo "ejecutando " $(target_name) " ...."
	./$(target_name)

all:
	make clean
	make compile

compile: $(target_name)
	@echo "compilando fuentes: " $(units_ext)
	@make --no-print-directory $(target_name)

$(target_name) : $(objs)
	@echo `tput bold`---------------------------------------------------------------
	@echo "Enlazando      :" $(target_name) 
	@echo "Unidades(ext)  :" $(units_ext) 
	@echo "Objetos        :" $(objs) 
	@tput sgr0
	g++ -o $(target_name) $(objs) $(ld_libs) 
	@echo ---------------------------------------------------------------
	
	
%.o: %.cpp $(headers)
	@echo `tput bold`---------------------------------------------------------------
	@echo Compilando: $(notdir $<) 
	@tput sgr0
	@g++ $(c_flags) -c $<

%.o: %.cc $(headers)
	@echo `tput bold`---------------------------------------------------------------
	@echo Compilando: $(notdir $<) 
	@tput sgr0
	@g++ $(c_flags) -c $<
	
%.o: %.c $(headers)
	@echo `tput bold`---------------------------------------------------------------
	@echo Compilando: $(notdir $<) 
	@tput sgr0
	@g++ $(c_flags) -c $<

clean:
	rm -f *.o $(target_name)
	
tar:
	tar czvf archivos_prac_1.tgz *.c* *.h* *.ply makefile




