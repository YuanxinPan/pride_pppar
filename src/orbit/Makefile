# Makefile modified by Yuanxin Pan (yxpan.im@gmail.com), on Dec 23, 2017

all: sp3orb

DIR_GUARD = @mkdir -p $(@D)
BIN_PATH = ./
OBJ_PATH = .obj/

# Flags
FFLAGS = -g -O3
FTN = gfortran

LIBCOM = ../lib/libcom.a

OBJ = sp3orb.o \
      get_sp3orb_args.o
OBJ := $(addprefix $(OBJ_PATH), $(OBJ))

sp3orb : $(OBJ) $(LIBCOM)
	$(FTN) $(FFLAGS) $(OBJ) $(LIBCOM) -o $@

$(OBJ_PATH)%.o : %.f90
	$(DIR_GUARD)
	$(FTN) $(FFLAGS) -c $< -o $@
	@echo -n "$(OBJ_PATH)$*.o : " > $(@:.o=.d)
	@sed -n "/include/s/include '\(.*\.h\)'/\1/p" $< | xargs >> $(@:.o=.d)

.PHONY : clean
clean :
	-rm sp3orb $(OBJ_PATH)*

-include $(OBJ:.o=.d)

