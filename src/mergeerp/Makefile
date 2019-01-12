# Makefile for oi on SunOS 5.5.1 by unimake at Fri Nov 20 13:09:26 MET 1998

all: mergeerp

# ASSIGMENTS

FFLAGS = -g
FTN = gfortran

LIB = ../lib/libcom.a  

mergeerp : mergeerp.f90 get_erp_args.f90 $(LIB)
	$(FTN) $(FFLAGS) mergeerp.f90 get_erp_args.f90 $(LIB) -o mergeerp
