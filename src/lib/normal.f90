!
!! normal.f90
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
!!   PURPOSE: Compute the normal function
!!
!!   AUTHOR : Shaoming Xin    jsx_miracle@whu.edu.cn
!!
!!   VERSION: ver 1.00        jan-25-2019
!!
!!   DATE   : jan-25, 2019
!!
!!   INPUT  : u
!!
!!   OUTPUT : p

subroutine normal(u, p)
implicit none
real*8 u,p
!
real*8 y,an(6),er

an(1) = 0.0705230784d0
an(2) = 0.0422820123d0
an(3) = 0.0092705272d0
an(4) = 0.0001520143d0
an(5) = 0.0002765672d0
an(6) = 0.0000430638d0

if (u .lt. -5.d0) then
  p = 0.d0
  return
else if (u .gt. 5.d0) then
  p = 1.d0
  return
endif

y = dabs(u)/dsqrt(2.d0)

er = 1.d0 - (1.d0 + y*(an(1) + y*(an(2) + y*(an(3) + y*(an(4) + y*(an(5) + y*an(6)))))))**(-16)

if (u .lt. 0.d0) then
  p = 0.5d0 - 0.5d0*er
else
  p = 0.5d0 + 0.5d0*er
endif

return
end
