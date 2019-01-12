!
!! CHI2.f90
!! 
!!    Copyright (C) 2018 by J.Geng
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
SUBROUTINE CHI2(N,CH,P,D)
!!
!! PURPOSE    :  COMPUTE NORMAL FUNCTION
!!
!! PARAMETERS :
!!         IN :  N : DERGEE OF FREEDOM                                   I*4
!!               CH: CHI2 VALUE                                          R*8
!!               P : DOWN SATAT. F(CHI2,N)                               R*8
!!        OUT :  D : VALUE OF DENCITY FUNCTION                           R*8
!!
IMPLICIT REAL*8 (A-H,O-Z)
REAL*8 LU

PI=DATAN(1.D0)*4.D0
PIS=DSQRT(PI)
X=CH/2.D0
CHS=DSQRT(CH)

IF(INT(REAL(N*.5)).EQ.REAL(N*.5)) GOTO 100

LU=DLOG(DSQRT(X))-X-DLOG(PIS)

CALL NORMAL(CHS,PP)
P=2.D0*(PP-.5D0)
IAI=1
GOTO 110

100 LU=DLOG(X)-X
P=1.D0-DEXP(-X)
IAI=2.D0

110 U=DEXP(LU)
IF (IAI.EQ.N) GOTO 130

N2=N-2
DO I=IAI,N2,2
  P=P-2.D0*U/DBLE(I)
  LU=DLOG(CH/DBLE(I))+LU
  U=DEXP(LU)
ENDDO

130 D=U/CH

RETURN
END
