!
!! wpress.f90
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
!!   PURPOSE: Compute the partial pressure of water vapor(mbar)
!!
!!   AUTHOR : Shaoming Xin    jsx_miracle@whu.edu.cn
!!
!!   VERSION: ver 1.00        Jan-25-2019
!!
!!   DATE   : Jan-25, 2019
!!
!!   INPUT  : Rhum (Relative humidity)
!!            Temp (Temperature)

real*8 function wpress(Rhum, Temp)
implicit none
!
real*8 Rhum, Temp
wpress = Rhum*6.11d0*10.d0**(7.5d0*Temp/(2.373d2+Temp))
return
end
