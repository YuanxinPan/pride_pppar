# Makefile for oi on SunOS 5.5.1 by unimake at Fri Nov 20 13:09:26 MET 1998

all: mergesp3

# ASSIGMENTS

FFLAGS = -g
FTN = gfortran

LIB = ../lib/libcom.a  

mergesp3 : mergesp3.f90 get_sp3orb_args.f90 $(LIB)
	$(FTN) $(FFLAGS) mergesp3.f90 get_sp3orb_args.f90 $(LIB) -o mergesp3
