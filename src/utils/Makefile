# Makefile created by Yuanxin Pan (yxpan@whu.edu.cn), on Jun 1, 2019

all : xyz2enu enucov

CC = g++
CPPFALGS = -Wall -O3

xyz2enu : xyz2enu.cpp
	$(CC) $(CPPFALGS) $< -o $@

enucov : enucov.f90 ../lib/libcom.a
	gfortran $(CPPFALGS) $< ../lib/libcom.a -o $@

.PHONY : clean
clean :
	-rm xyz2enu enucov

