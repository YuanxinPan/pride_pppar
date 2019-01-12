!
!! mergeerp.f90
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
!!
!  FUNCTIONS:
!  mergeerp - mrege erp.
!
program mergeerp
  implicit none

  integer*4 nargs, lfn1, lfn2, lfn3, merlfn, i
  integer*4 ierr1, ierr2, ierr3
  character*256 erpfil1      ! last day
  character*256 erpfil2      ! processing day
  character*256 erpfil3      ! next day
  character*256 mererpfil
  character*256 line1, line2, line3

!! functions called
  integer*4 get_valid_unit
!! read command arguments
  nargs = iargc()
  if (nargs .eq. 0) then
    write (*, '(a)') 'mergeerp command arguments error!'
    call exit(4)
  endif

  erpfil1 = ' '
  erpfil2 = ' '
  erpfil3 = ' '
  mererpfil = ' '

!! get arguements
  call get_erp_args(erpfil1, erpfil2, erpfil3, mererpfil)

  lfn1 = get_valid_unit(10)
  lfn2 = get_valid_unit(20)
  lfn3 = get_valid_unit(30)
  merlfn = get_valid_unit(40)
  open (lfn1, file=erpfil1, status='old', iostat=ierr1)
  open (lfn2, file=erpfil2, status='old', iostat=ierr2)
  open (lfn3, file=erpfil3, status='old', iostat=ierr3)
  open (merlfn, file=mererpfil)

  if (ierr1 .ne. 0) then
    write (*, '(2a)') '***ERROR(mergeerp): open file ', trim(erpfil1)
    call exit(1)
  endif

  if (ierr2 .ne. 0) then
    write (*, '(2a)') '***ERROR(mergeerp): open file ', trim(erpfil2)
    call exit(1)
  endif

  if (ierr3 .ne. 0) then
    write (*, '(2a)') '***ERROR(mergeerp): open file ', trim(erpfil3)
    call exit(1)
  endif

!  inquire(file=merlfn,opened=lopen,number=openid) !add by zwx 20141031
  line2 = ' '
  read (lfn2, '(a)') line2
  if (line2(1:9) .ne. 'version 2' .and. line2(1:9) .ne. 'VERSION 2') then
    write (*, '(2a)') '***ERROR(read_igserp): unknown version ', line2(1:9)
    call exit(1)
  endif
  write (merlfn, '(a130)') line2
  line2 = ' '
  do while(index(line2,'MJD').eq.0.or.index(line2,'UT1').eq.0.or.index(line2,'UTC').eq.0.or.index(line2,'LOD').eq.0)
    read (lfn2, '(a)') line2
    write (merlfn, '(a130)') line2
  enddo
  read (lfn2, '(a)') line2
  write (merlfn, '(a130)') line2
  line1 = ' '
  line3 = ' '
  !! add last week erp data
  do while(index(line1,'MJD').eq.0.or.index(line1,'UT1').eq.0.or.index(line1,'UTC').eq.0.or.index(line1,'LOD').eq.0)
    read (lfn1, '(a)') line1
  enddo
  read (lfn1, '(a)') line1
  do i = 1, 7
    read (lfn1, '(a)') line1
    write (merlfn, '(a130)') line1
  enddo
  !! add processing week erp data
  do i = 1, 7
    read (lfn2, '(a)') line1
    write (merlfn, '(a130)') line1
  enddo
  !! add nex week erp data
  do while(index(line3,'MJD').eq.0.or.index(line3,'UT1').eq.0.or.index(line3,'UTC').eq.0.or.index(line3,'LOD').eq.0)
    read (lfn3, '(a)') line3
  enddo
  read (lfn3, '(a)') line3
  do i = 1, 7
    read (lfn3, '(a)') line3
    write (merlfn, '(a130)') line3
  enddo

  close (merlfn)
  close (lfn1)
  close (lfn2)
  close (lfn3)
end program mergeerp
