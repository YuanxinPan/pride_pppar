!
!! get_erp_args.f90
!!
!!    Copyright (C) 2018 by Wuhan University
!!
!!    This program is an open source software: you can redistribute it and/or modify
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
!! author: J. Geng, M. Ge
!! tester: Y. Pan, X. Chen, J. Zhou, S. Mao
!!
!
subroutine get_erp_args(erpfil1, erpfil2, erpfil3, mererpfil)
  implicit none
  include '../header/const.h'
  include '../header/orbit.h'

  character*256 erpfil1      ! last day
  character*256 erpfil2      ! processing day
  character*256 erpfil3      ! next day
  character*256 mererpfil

!
!! functions called
  integer*4 get_valid_unit
!
  erpfil1 = ' '
  erpfil2 = ' '
  erpfil3 = ' '
  mererpfil = ' '
  call getarg(1, erpfil1)        ! last day
  call getarg(2, erpfil2)        ! processing day
  call getarg(3, erpfil3)        ! next day
  call getarg(4, mererpfil)
  return
end
