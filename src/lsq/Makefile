# Makefile created by Yuanxin Pan (yxpan.im@gmail.com), on Mar 9, 2018

all: lsq

DIR_GUARD = @mkdir -p $(@D)
BIN_PATH = ./
OBJ_PATH = .obj/

FFLAGS = -g -O3
FTN = gfortran

LIBCOM = ../lib/libcom.a
LIBEPH = ../de405/libde405.a

SRC := $(wildcard *.f90)
OBJ := $(SRC:.f90=.o)
OBJ := $(addprefix $(OBJ_PATH), $(OBJ))

lsq : $(OBJ) $(LIBEPH) $(LIBCOM)
	$(FTN) $(FFLAGS) $(OBJ) $(LIBEPH) $(LIBCOM) -o $@

$(OBJ_PATH)%.o : %.f90
	$(DIR_GUARD)
	$(FTN) $(FFLAGS) -c $< -o $@
	@echo -n "$(OBJ_PATH)$*.o : " > $(@:.o=.d)
	@sed -n "/include/s/include '\(.*\.h\)'/\1/p" $< | xargs >> $(@:.o=.d)

.PHONY : clean
clean :
	-rm lsq $(OBJ_PATH)*

-include $(OBJ:.o=.d)

