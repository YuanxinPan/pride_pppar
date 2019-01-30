# Makefile created by Yuanxin Pan (yxpan.im@gmail.com) on Jan 7 2018

all: redig

DIR_GUARD = @mkdir -p $(@D)
OBJ_PATH = .obj/

# ASSIGMENTS
FTN = gfortran 
FFLAGS = -g -O3

LIBCOM = ../lib/libcom.a  

SRC := $(wildcard *.f90)
OBJ := $(SRC:.f90=.o)
OBJ := $(addprefix $(OBJ_PATH), $(OBJ))

redig : $(OBJ) $(LIBCOM)
	$(FTN) $(FFLAGS) $(OBJ) $(LIBCOM) -o $@

$(OBJ_PATH)%.o : %.f90
	$(DIR_GUARD)
	$(FTN) $(FFLAGS) -c $< -o $@
	@echo -n "$(OBJ_PATH)$*.o : " > $(@:.o=.d)
	@sed -n "/include/s/include '\(.*\.h\)'/\1/p" $< | xargs >> $(@:.o=.d)

.PHONY : clean
clean :
	-rm redig $(OBJ_PATH)*

-include $(OBJ:.o=.d)

