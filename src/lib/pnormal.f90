!
!! PNORMAL.f90
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
SUBROUTINE PNORMAL(Q,U)
!!
!! PURPOSE    :  COMPUTE NORMAL FUNCTION DEVIATESS
!!
!! PARAMETERS :
!!         IN :  Q : UP SATA. (RFA)                                      R*8
!!
!!        OUT :  U : DEVIATESS (U_RFA)                                   R*8
!!
IMPLICIT REAL*8 (A-H,O-Z)

IF(Q.LE.0.D0.OR.Q.GE.1.D0) THEN
  write(*,'(a)') '***ERROR(pnormal): INPUT RFA CAN NOT >=1 OR <=0'
  call exit(1)
ENDIF

! IF RFA =.5 THEN U=0
IF(Q.EQ..5D0) THEN
  U=0.D0
  RETURN
ENDIF

IF(Q.LT..5D0.AND.Q.GT.0.D0) THEN
  P=Q
ELSE IF(Q.GT.0.5) THEN
  P=1.D0-Q
ENDIF

Y=-1.D0*DLOG(4.D0*P*(1.D0-P))
B0=1.570796288D0
B1=.3706987906D-1
B2=-.8364353598D-3
B3=-.2250947176D-3
B4=.6841218299D-5
B5=.5824238515D-5
B6=-.1045274970D-5
B7=.8360937017D-7
B8=-.3231081277D-8
B9=.3657763036D-10
B10=.6936233982D-12

Y=Y*(B0+Y*(B1+Y*(B2+Y*(B3+Y*(B4+Y*(B5+Y*(B6+Y*(B7+Y*(B8+Y*(B9+Y*B10))))))))))

U=DSQRT(Y)
IF(Q.GT..5D0) U=-1.D0*U

RETURN
END
