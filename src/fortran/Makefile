#compiler
FC = gfortran

#compiler flags
#standard
CFLAGS = -std=f2008ts
#warning flags
CFLAGS += -Wall
#debugging options
CFLAGS += -fPIC -fmax-errors=3 -fcheck=all -fbacktrace
#GDB flag
CFLAGS += -g

#source files
SRCS = mod_misc mod_grid mod_initial mod_diff
OBJS = $(SRCS:=.o)

#executable
MAIN = ../../bin/Adv.out
COMPI = ../compile_files/

#compile project
all : $(MAIN)
	@echo Model compiled

$(MAIN) : $(OBJS)
	$(FC) $(CFLAGS) -o $(MAIN) $(OBJS) Adv.f95 -I $(COMPI)

.SUFFIXES : .o .f95

.f95.o :
	mkdir -p ../compile_files
	$(FC) $(CFLAGS) -c $< -I $(COMPI) -J $(COMPI)

clean :
	$(RM) *.o $(MAIN) ../../bin/test.csv
	rm -r $(COMPI)