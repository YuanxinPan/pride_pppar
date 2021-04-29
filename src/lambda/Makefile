# Makefile created by Yuanxin Pan (yxpan@whu.edu.cn), on Jan 29, 2018

LIB = liblambda.a
all : $(LIB)

DIR_GUARD = @mkdir -p $(@D)
BIN_PATH = ./
OBJ_PATH = .obj/

# Flags
FFLAGS = -g -O3
FTN = gfortran

SRC := $(wildcard *.f)
OBJ := $(SRC:.f=.o)
OBJ := $(addprefix $(OBJ_PATH), $(OBJ))

$(LIB) : $(OBJ)
	ar -rv $(LIB) $(OBJ)

$(OBJ_PATH)%.o : %.f
	$(DIR_GUARD)
	$(FTN) $(FFLAGS) -c $< -o $@
	@echo -n "$(OBJ_PATH)$*.o : " > $(@:.o=.d)
	@sed -n "/include/s/include '\(.*\.h\)'/\1/p" $< | xargs >> $(@:.o=.d)

.PHONY : clean
clean :
	-rm $(LIB) $(OBJ_PATH)*

-include $(OBJ:.o=.d)

