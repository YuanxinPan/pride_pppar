!
!! NORMAL.f90
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
!
SUBROUTINE NORMAL(U,P)
!!
!! PARAMETERS :
!!         IN :  U : NORMAL OFFSET POINT                                 R*8
!!
!!        OUT :  P : DOWN SATAT.                                         R*8
!!
IMPLICIT REAL*8 (A-H,O-Z)

IF (U.LT.-5.D0) THEN
  P=0.D0
  RETURN
ENDIF

IF (U.GT.5.D0) THEN
  P=1.D0
  RETURN
ENDIF

Y=DABS(U)/DSQRT(2.D0)

A1=.0705230784D0
A2=.0422820123D0
A3=.0092705272D0
A4=.0001520143D0
A5=.0002765672D0
A6=.0000430638D0

ER=1.D0-(1.D0+Y*(A1+Y*(A2+Y*(A3+Y*(A4+Y*(A5+Y*A6))))))**(-16)
Q=.5D0*ER

IF (U.LT.0.D0) THEN
  P=.5D0-Q
ELSE
  P=.5D0+Q
ENDIF

RETURN
END
