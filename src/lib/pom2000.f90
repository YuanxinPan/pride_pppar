!
!! POM2000.f90
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
SUBROUTINE POM2000 ( XP, YP, SP, RPOM )
!+
!
!  Form the matrix of polar motion, IAU 2000.
!
!  Annex to IERS Conventions 2000, Chapter 5
!
!  Given:
!     XP,YP      d      coordinates of the pole (radians)
!     SP         d      the quantity s' (radians)
!
!  Returned:
!     RPOM     d(3,3)   polar-motion matrix
!
!  The returned rotation matrix is the first to be applied when
!  transforming a TRS vector into a CRS vector.
!
!  This revision:  2002 November 25
!
!-----------------------------------------------------------------------

IMPLICIT NONE

REAL*8 XP, YP, SP, RPOM(3,3)
!
!! LOCAL
INTEGER*4 I

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RPOM = 0.D0
DO I=1,3
  RPOM(I,I)=1.D0
ENDDO
!  Construct the matrix.
CALL ROT_X( YP,RPOM)
CALL ROT_Y( XP,RPOM)
CALL ROT_Z(-SP,RPOM)

RETURN
END
