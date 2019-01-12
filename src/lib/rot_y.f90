!
!! ROT_Y.f90
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
SUBROUTINE ROT_Y ( THETA, R )
!+
!
!  Rotate an r-matrix about the y-axis.
!
!  Status:  vector/matrix support routine.
!
!  Given:
!     THETA    d         angle (radians)
!
!  Given and returned:
!     R        d(3,3)    r-matrix
!
!  Sign convention:  The matrix can be used to rotate the
!  reference frame of a vector.  Calling this routine with
!  positive THETA incorporates in the matrix an additional
!  rotation, about the y-axis, anticlockwise as seen looking
!  towards the origin from positive y.
!
!  This revision:  2007 November 7
!
!-----------------------------------------------------------------------

IMPLICIT NONE

REAL*8 THETA, R(3,3)
!
!! LOCAL
INTEGER*4 I
REAL*8 S, C, A(3,3)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!  Matrix representing new rotation.
S = SIN(THETA)
C = COS(THETA)
A = 0.D0
DO I=1,3
  A(I,I)=1.D0
ENDDO
A(1,1) = C
A(3,1) = S
A(1,3) = -S
A(3,3) = C

!  Rotate.
CALL MATMPY(A,R,R,3,3,3)

!  Finished.
RETURN
END
