!
!! read_kinpos.f90
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
!!
!! purpose  : read kinematic position file
!! parameter:
!!    input : SITE%  -- station struct
!!            jd,sod -- requested time
!!    output: x,y,z  -- kinematic position in m
!
subroutine read_kinpos(SITE, jd, sod, x, y, z)
  implicit none
  include '../header/const.h'
  include '../header/station.h'

  integer*4 jd
  real*8 sod, x, y, z
  type(station) SITE
!
!! local
  logical*1 lexist
  integer*4 i, jdx, ierr
  real*8 dt, sodx, xt(3)
  character*256 line
!
!! function called
  integer*4 get_valid_unit
  real*8 timdif
!
!! check and open kin file
  if (SITE%ikin .eq. 0) then
    inquire (file=SITE%kinfil, exist=lexist)
    if (.not. lexist) then
      if (SITE%ixyz .eq. 0) SITE%ixyz = 1
      return
    endif
    SITE%ikin = get_valid_unit(10)
    open (SITE%ikin, file=SITE%kinfil, status='old', iostat=ierr)
    if (ierr .eq. 0) then
      write (*, '(2a)') '%%%MESSAGE(read_kinpos): kinematic read ', trim(SITE%kinfil)
    else
      write (*, '(2a)') '***ERROR(read_kinpos): open file ', trim(SITE%kinfil)
      call exit(1)
    endif
!
!! read header
    do while (.true.)
      read (SITE%ikin, '(a)', end=100) line
      if (index(line, 'END OF HEADER') .ne. 0) exit
    enddo
  endif
!
!! read kinematic position
  line = ' '
  do while (.true.)
    read (SITE%ikin, '(a)', end=100) line
    read (line, '(i5,f9.2,2x,3f13.3)', err=200) jdx, sodx, (xt(i), i=1, 3)
    dt = timdif(jdx, sodx, jd, sod)
    if (dabs(dt) .lt. MAXWND) then
      x = xt(1); y = xt(2); z = xt(3)
      SITE%ixyz = 0
      return
    else if (dt .gt. MAXWND) then
      backspace SITE%ikin
      if (SITE%ixyz .eq. 0) SITE%ixyz = 1
      return
    endif
  enddo

100 write (*, '(2a)') '###WARNING(read_kinpos): end of file ', trim(SITE%kinfil)
  if (SITE%ixyz .eq. 0) SITE%ixyz = 1
  return
200 write (*, '(2a)') '***ERROR(read_kinpos): read file ', trim(SITE%kinfil)
  call exit(1)
end
