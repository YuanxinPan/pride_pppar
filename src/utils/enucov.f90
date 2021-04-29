program main
  implicit none
!! local
  integer*4 lfnpos, lfncov, ierr, nargs, nline
  real*8 xyz(3), cor(3), blh(3)
  real*8 xyzcov(3,3), enucov(3,3), rot(3,3), F(3,3)
  character*4   site
  character*256 posfile, buf
!! function called
  integer*4 get_valid_unit

  nargs = iargc()
  if (nargs .lt. 1) then
    write (*, '(a)') 'Usage: enucov pos_file'
    call exit(1)
  endif
  call getarg(1, posfile)

  lfnpos = get_valid_unit(10)
  open(lfnpos, file=posfile, status='old', iostat=ierr)
  if (ierr .ne. 0) then
    write(*,'(2a)') 'error: no such file: ', trim(posfile)
    call exit(1)
  endif

  !! read xyz & xyzcov
  nline = 0
  do while (.true.)
    read(lfnpos, '(a)', iostat=ierr) buf
    if (ierr .ne. 0) exit
    nline = nline + 1
    if (nline .eq. 3) then
      read(buf, '(a4,3f15.4)') site, xyz
      ! write(*, '(3f14.4)') xyz(1), xyz(2), xyz(3)
    else if (nline .eq. 4) then
      read(buf(5:), '(3f15.4)') cor
      ! write(*, '(3f14.4)') cor(1), cor(2), cor(3)
    else if (nline .eq. 7) then
      read(buf(5:), '(3e12.3)') xyzcov(1,1), xyzcov(2,2), xyzcov(3,3)
      ! write(*, '(3e12.3)') xyzcov(1,1), xyzcov(2,2), xyzcov(3,3)
    else if (nline .eq. 8) then
      read(buf(5:), '(3e12.3)') xyzcov(1,2), xyzcov(1,3), xyzcov(2,3)
      ! write(*, '(3e12.3)') xyzcov(1,2), xyzcov(1,3), xyzcov(2,3)
    endif
  enddo
  xyz = xyz + cor
  xyzcov(2,1) = xyzcov(1,2)
  xyzcov(3,1) = xyzcov(1,3)
  xyzcov(3,2) = xyzcov(2,3)

  !! compute blh & enucov
  call xyzblh(xyz, 1.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, blh)
  call rot_enu2xyz(blh(1),blh(2),rot)

  F = transpose(rot)
  call matmpy(F, xyzcov, enucov, 3, 3, 3)
  call matmpy(enucov, rot, enucov, 3, 3, 3)

  !! output xyz & enucov
  lfncov = get_valid_unit(10)
  open(lfncov, file='enucov.out', status='replace', iostat=ierr)
  write(lfncov, '(a4, 3f15.4, 6e12.3)') site, xyz, enucov(1,1), enucov(2,2), &
                         enucov(3,3), enucov(1,2), enucov(1,3), enucov(2,3)

  close(lfnpos)
  close(lfncov)
end program
