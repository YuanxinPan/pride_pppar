!
!! chi2.f90
!!
!!    Copyright (C) 2018 by Wuhan University
!!
!!    This program is free software: you can redistribute it and/or modify
!!    it under the terms of the GNU General Public License (version 3) as
!!    published by the Free Software Foundation.
!!
!!    This program is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public License (version 3) for more details.
!!
!!    You should have received a copy of the GNU General Public License
!!    along with this program.  If not, see <https://www.gnu.org/licenses/>.
!!
!!  PURPOSE: Compute the normal function
!!
!!  AUTHOR : Shaoming Xin    jsx_miracle@whu.edu.cn
!!
!!  VERSION: ver 1.00        jan-25-2019
!!
!!  DATE   : jan-25, 2019
!!
!!  INPUT  :
!!           N : DERGEE OF FREEDOM                                   I*4
!!           CH: CHI2 VALUE                                          R*8
!!           P : DOWN SATAT. F(CHI2,N)                               R*8
!! OUTPUT :
!!           D : VALUE OF DENCITY FUNCTION                           R*8

subroutine chi2(n, chv, p, valu)
implicit none
integer*4 n
real*8 chv,p,valu
! local
integer*4 n2,i
real*8 lul,pi,pis,x,pp,chs,iai,u

pi = datan(1.d0)*4.d0
pis = dsqrt(pi)
x = chv/2.d0
chs = dsqrt(chv)

if (int(real(n*0.5d0)) .eq. real(n*0.5d0)) goto 100

lul = dlog(dsqrt(x)) - x - dlog(pis)

call normal(chs, pp)
p = 2.d0*(pp - 0.5d0)
iai = 1
goto 110

100 lul = dlog(x) - x
p = 1.d0 - dexp(-x)
iai = 2.d0

110 u = dexp(lul)
if (iai .eq. n) goto 120

n2 = n - 2
do i = iai, n2, 2
  p = p - 2.d0*u/dble(i)
  lul = dlog(chv/dble(i)) + lul
  u = dexp(lul)
enddo

120 valu = u/chv

return
end
