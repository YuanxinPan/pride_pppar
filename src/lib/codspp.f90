!
!! codspp.f90
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
!! purpose  : single point positioning using range observations to check
!!            and/or improving station position
!! parameter:
!!    input : lfncid,lfnrem -- tmp files
!!            jd,sod        -- checkint time
!!            OB       -- site information
!!            ite           -- # of iterations
!!            flag          -- flag for position initializtion
!!    output: deltax        -- position correction
!! author   : Geng J
!! created  : Nov. 3, 2007
!
subroutine codspp(lfncid, lfnrem, jd, sod, OB, ite, flag, deltax)
  implicit none
  include '../header/const.h'
  include '../header/rnxobs.h'

  integer*4 lfncid, lfnrem, jd, ite, flag
  real*8 sod, deltax(1:*)
  type(rnxobr) OB
!
!! local
  integer*4 nobs, i, j, k, iobs, iptx, ipt(MAXSAT)
  real*8 g, g1, det, sig0, sig1
  real*8 amat(MAXSAT, 4), omc(MAXSAT)
  real*8 c(4, 4), w(4), v(MAXSAT), solu(4)
!
!! function called
  logical*1 chitst
  real*8 dot

  g = 7.7d0/6.0d0
  g1 = g/(g*g - 1.d0)
!
!! range omc and partial derivatives
  nobs = 0
  do i = 1, OB%nprn
    if (OB%omc(i, 3) .ne. 0.d0) then
      nobs = nobs + 1
      do j = 1, 3
        amat(nobs, j) = OB%amat(j, i)
      enddo
!! receiver clock unit (m)
      amat(nobs, 4) = FREQ1/VLIGHT
      omc(nobs) = OB%omc(i, 3) - g1*(OB%omc(i, 4) - OB%omc(i, 3)/g)
      ipt(nobs) = i
    endif
  enddo
!
!! at least 5 satellites
  iptx = 0
  deltax(1:5) = 0.d0
5 iobs = nobs
  if (iptx .ne. 0) iobs = nobs - 1
  if (iobs .gt. 4) then
    c = 0.d0
    w = 0.d0
    solu = 0.d0
    sig0 = 0.d0
    do j = 1, nobs
      sig0 = sig0 + omc(j)*omc(j)
    enddo
!
!! add to normal equation
    do i = 1, 4
      do j = 1, nobs
        w(i) = w(i) + amat(j, i)*omc(j)
      enddo
      do j = 1, i
        do k = 1, nobs
          c(i, j) = c(i, j) + amat(k, i)*amat(k, j)
        enddo
        c(j, i) = c(i, j)
      enddo
    enddo
!
!! solve normal equation
    call matinv(c, 4, 4, det)
    if (det .eq. 0.d0) then
      write (oscr, '(a)') '***ERROR(codspp): matrix singularity '
      call exit(1)
    endif
!
!! solution
    do i = 1, 4
      do j = 1, 4
        solu(i) = solu(i) + c(i, j)*w(j)
      enddo
    enddo
!
!! sigma0
    do i = 1, 4
      sig0 = sig0 - solu(i)*w(i)
    enddo
    sig0 = dsqrt(sig0/(iobs - 4))
    if (iptx .ne. 0 .and. chitst(-1, iobs - 1, sig1, sig0, 0.99d0)) then
!! in this condition, save the epoch for it may be related to pseudorange quality
      iptx = 0
      deltax(1:5) = 0.d0
      return
    endif
    sig1 = sig0
    deltax(1:4) = solu(1:4)
    deltax(5) = 0.d0
    do i = 1, 3
      deltax(5) = deltax(5) + deltax(i)*deltax(i)
    enddo
    deltax(5) = dsqrt(deltax(5))
!
!! set OB
    if (iptx .ne. 0) then
      ite = 0
      deltax(5) = 10.d0
      OB%obs(ipt(iptx), 1:4) = 0.d0
      write (oscr, '(a,i5,f8.1,a,i2)') '###WARNING(codspp): bad range in SIT at ', jd, sod, ' for SAT', OB%prn(ipt(iptx))
      if (lfncid .ne. 0 .and. lfnrem .ne. 0) then
        write (lfncid) 'de'
        write (lfnrem) 1, jd, sod, 1, ipt(iptx)
      endif
      return
    endif
!
!! reliability
    if (ite .eq. 10 .or. sig0*0.19d0 .gt. 10.d0 .and. deltax(5) .lt. 1.d0 .and. dabs(deltax(4)/VLIGHT) .lt. 1.d-6) then
!
!! residual vector
      do i = 1, nobs
        v(i) = -omc(i)
        do j = 1, 4
          v(i) = v(i) + amat(i, j)*deltax(j)
        enddo
      enddo
!
!! correlation analysis
      call cltasy(nobs, 4, amat, c, v, iptx)
      if (iptx .ne. 0) then
        omc(iptx) = 0.d0
        amat(iptx, 1:4) = 0.d0
        goto 5
      else
        deltax(1:5) = 0.d0
      endif
    endif
  else
    deltax(1:4) = 0.d0
    if (flag .eq. 2) then
      deltax(5) = 10.d0
      write (oscr, '(a,i5,f8.1)') '###WARNING(codspp): initialization fails in SIT at ', jd, sod
      do i = 1, nobs
        OB%obs(ipt(i), 1:4) = 0.d0
      enddo
      if (lfncid .ne. 0 .and. lfnrem .ne. 0) then
        write (lfncid) 'de'
        write (lfnrem) 1, jd, sod, nobs, (ipt(i), i=1, nobs)
      endif
    else
      deltax(5) = 0.d0
    endif
  endif

  return
end
