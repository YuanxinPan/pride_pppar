!
!! yr2year.f90
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
!! author: J.Geng X.Chen
!! tester: X.Chen Y.Pan S.Mao J.Zhou C.Li S.Yang
!!
!
subroutine yr2year(iyear)
  implicit none
  integer*4 iyear

  if (iyear .gt. 1900) return

  if (iyear .le. 20) then
    iyear = iyear + 2000
  else
    iyear = iyear + 1900
  endif

  return
end
