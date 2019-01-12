# Makefile for oi on SunOS 5.5.1 by unimake at Fri Nov 20 13:09:26 MET 1998

all: get_ctrl

# ASSIGMENTS

FFLAGS = -g
FTN = gfortran

LIB = ../lib/libcom.a  

get_ctrl : get_ctrl.f90 $(LIB)
	$(FTN) $(FFLAGS) get_ctrl.f90 $(LIB) -o get_ctrl
