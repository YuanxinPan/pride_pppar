# Makefile created by Yuanxin Pan (yxpan.im@gmail.com), on Mar 9, 2018

all: arsig

DIR_GUARD = @mkdir -p $(@D)
BIN_PATH = ./
OBJ_PATH = .obj/

# ASSIGMENTS
FTN = gfortran
FFLAGS = -g

LIBCOM = ../lib/libcom.a
LIBLAMBDA = ../lambda/liblambda.a

SRC := $(wildcard *.f90)
OBJ := $(SRC:.f90=.o)
OBJ := $(addprefix $(OBJ_PATH), $(OBJ))

arsig : $(OBJ) $(LIBCOM) $(LIBLAMBDA)
	$(FTN) $(FFLAGS) $(OBJ) $(LIBCOM) $(LIBLAMBDA) -o $@

$(LIB) : $(OBJ)
	ar -rv $(LIB) $(OBJ)
 
$(OBJ_PATH)%.o : %.f90
	$(DIR_GUARD)
	$(FTN) $(FFLAGS) -c $< -o $@
	@echo -n "$(OBJ_PATH)$*.o : " > $(@:.o=.d)
	@sed -n "/include/s/include '\(.*\.h\)'/\1/p" $< | xargs >> $(@:.o=.d)
 
.PHONY : clean
clean :
	-rm arsig $(OBJ_PATH)*
 
 -include $(OBJ:.o=.d)

