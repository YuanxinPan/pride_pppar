!
!! bdeci.f90
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
subroutine bdeci(est, sigma, ih, cutdev, cutsig, prob, deci)
!
!!    Calculate the decision-function value as per Appendix A
!!    of Dong and Bock [1989].
!!       Da-nan Dong  880403
!!    Add damping factor. DND  880602
!!    Change definition of decision function based on hypothesis
!!       test theory.      DND 880929
!!    Remove arbitrary factor of 3., scale wide-lane but not narrow-lane
!!       sigmas.    King 930322
!!
!!    Input:
!!      est:     estimated (real) bias value
!!      sigma:   estimated uncertainty of bias value, scaled by nrms for
!!                 widelane, unscaled for narrow lane
!!      ih :     control for receiver ambiguity
!!                 = 1  unit=one cycle
!!                 = 2  unit=half cycle
!!      cutdev:  threshold deviation for taper function (= 0.4 in Dong and Bock;
!!                 default still 0.4 here and in FIXDRV)
!!      cutsig:  threshold sigma for taper function (=0.33 in Dong and Bock;
!!                 default still 0.4 here and in FIXDRV)
!!
!!    Output:
!!      prob : area of decision-function region, set = 1.0 for
!!             each bias on input to BDECI, reduced by the probability
!!             of an error in rounding each bias. (Since we now search
!!             only one bias at a time, the cumulative probability is
!!             not calculated by NBIASR or NBIASP.)
!!      deci : decision function d(x,sigma) = FT/Q, inverse of
!!             1. - allowable rate for a type 1 error (alpha),
!!             compared with input wlcut or nlcut in NBIASR.
!!             (But F is no longer used because we search one bias at
!!             a time.)
!
  implicit none
  include '../header/const.h'

  real*8 est, sigma, cutdev, cutsig, prob, dev, deci, term1, term2, c, d1, a1, bint, taper, add, cdev, csig, trun, s1, s2
  real*8 b1, b2, erfc, erfcb1, erfcb2
  integer*4 ih, j
  external erfc

  data s2/1.414213562373095d0/

  prob = 1.d0
!
!! compute the deviation of the estimated value from an integer or half-integer
  add = est*dble(ih)
  bint = dint(add + 0.5d0*dsign(1.d0, add))/dble(ih)
  dev = dabs(bint - est)
!
!! set the cutoff deviation from the input or default value
  cdev = cutdev
!! default was 0.4 prior to 930315, now 0.15
  if (cutdev .lt. 1.d-3) cdev = 0.15d0
!! this was 0.6d0*cdev by mistake, prior to 930319
  if (ih .eq. 2) cdev = 0.5d0*cdev

!! if the deviation is greater than the cutoff, set prob and deci and exit
  if (dev .ge. cdev) then
    prob = 0.d0
    cdev = 0.5d0
  endif

!**old code:
!  scale the estimated sigma by the nrms (sclerr) for both widelane and narrowlane
!  (these should be treated differently)
!  sigtemp = sclerr*sigma
!  s1 = 1.0d0/(sigtemp*s2)
!**new code
  s1 = 1.d0/(sigma*s2)
!  numerical truncation tolerance
  trun = 1.d-9

!  compute the taper (T)
!  this term is (1 - dev/0.4) in Dong and Bock;
  term1 = 1.d0 - dev/cdev
!  this term is (1. - 3*scaled_sigma) in Dong and Bock; since cutsig is
!  0.4 now by default, term2 = 1.3 - 3*scaled_sigma
!  Dong and Bock:  term2 = (.333 - sigtemp)*3.
!  New:
  csig = cutsig
!  default changed from 0.4 to 0.15 930319
  if (cutsig .lt. 1.d-3) csig = 0.15d0
  if (sigma .gt. csig) then
    prob = 0.d0
    csig = 0.5d0
  endif
!**old code:
!  term2 = (csig-sigtemp)*3.d0
!**new code
  term2 = (csig - sigma)*3.d0
!**old code      if (bcigma.lt.1.0d-3) term1 = (0.4d0-c)*3.0d0
  if (term2 .lt. 0.d0) term2 = 0.d0
!  we now square the first term (linear in Dong and Bock) to
!  achieve a greater taper
  taper = term1**2*term2

!  compute Q according to equation A-12 in Dong and Bock
  c = 0.d0
  do j = 1, 50
    a1 = dble(j)
!  b1 = sngl((a1-dev)*s1)
!  b2 = sngl((a1+dev)*s1)
!  d1 = dble(erfc(b1)-erfc(b2))
    b1 = (a1 - dev)*s1
    b2 = (a1 + dev)*s1
!  limit the range of erf to  avoid underflows
    if (b1 .lt. 0.d0 .or. b1 .gt. 15.d0) then
      erfcb1 = 0.d0
    else
      erfcb1 = erfc(b1)
    endif
    if (b2 .lt. 0.d0 .or. b2 .gt. 15.d0) then
      erfcb2 = 0.d0
    else
      erfcb2 = erfc(b2)
    endif
!* d1 = erfc(b1)-erfc(b2)
    d1 = erfcb1 - erfcb2
    c = c + d1
    if (d1 .lt. trun) goto 440
  enddo

! return the decision function and reduced probability
440 continue
  if (prob .gt. 0.d0) prob = 1.d0 - c
  if (c .lt. 1.d-9) c = 1.d-9
  deci = taper/c
  continue
  return
end

!--------------------------------------------------------------
function erf(x)
  real*8 erf, gammp, x, half
  half = 0.5d0
  if (x .lt. 0.d0) then
    erf = -gammp(half, x**2)
  else
    erf = gammp(half, x**2)
  endif
  return
end
!--------------------------------------------------------------
function erfc(x)
  real*8 erfc, x, gammp, gammq, half
  half = 0.5d0
  if (x .lt. 0.d0) then
    erfc = 1.d0 + gammp(half, x**2)
  else
    erfc = gammq(half, x**2)
  endif
  return
end
!--------------------------------------------------------------
function gammp(a, x)
  real*8 gammp, a, x, gln, gammcf
  if (x .lt. 0.d0 .or. a .le. 0.d0) read(*,*) !pause
  if (x .lt. a + 1.d0) then
    call gser(gammp, a, x, gln)
  else
    call gcf(gammcf, a, x, gln)
    gammp = 1.d0 - gammcf
  endif
  return
end
!--------------------------------------------------------------
function gammq(a, x)
  real*8 gammq, a, x, gamser, gln
  if (x .lt. 0.d0 .or. a .le. 0.d0) read(*,*) !pause
  if (x .lt. a + 1.d0) then
    call gser(gamser, a, x, gln)
    gammq = 1.d0 - gamser
  else
    call gcf(gammq, a, x, gln)
  endif
  return
end
!--------------------------------------------------------------
function gammln(xx)
  real*8 cof(6), stp, half, one, fpf, x, xx, tmp, ser, gammln
  integer*4 j
  data cof, stp/76.18009173d0, -86.50532033d0, 24.01409822d0, -1.231739516d0, .120858003d-2, -.536382d-5, 2.50662827465d0/
  data half, one, fpf/0.5d0, 1.0d0, 5.5d0/
  x = xx - one
  tmp = x + fpf
  tmp = (x + half)*log(tmp) - tmp
  ser = one
  do j = 1, 6
    x = x + one
    ser = ser + cof(j)/x
  enddo
  gammln = tmp + log(stp*ser)
  return
end
!--------------------------------------------------------------
subroutine gcf(gammcf, a, x, gln)
  real*8 gammcf, gammln, a, anf, x, gln, gold, a0, a1, b0, b1, fac, an, ana, g, eps
  integer*4 n, itmax
  parameter(itmax=100, eps=3.d-7)
  gln = gammln(a)
  g = 0.d0
  gold = 0.d0
  a0 = 1.d0
  a1 = x
  b0 = 0.d0
  b1 = 1.d0
  fac = 1.d0
  do n = 1, itmax
    an = float(n)
    ana = an - a
    a0 = (a1 + a0*ana)*fac
    b0 = (b1 + b0*ana)*fac
    anf = an*fac
    a1 = x*a0 + anf*a1
    b1 = x*b0 + anf*b1
    if (a1 .ne. 0.) then
      fac = 1.d0/a1
      g = b1*fac
      if (abs((g - gold)/g) .lt. eps) then
        gammcf = dexp(-x + a*dlog(x) - gln)*g
        return
      endif
      gold = g
    endif
  enddo

  write (*, '(a)') '***ERROR(bdeci/gcf): a too large, itmax to small '
  call exit(1)
end
!--------------------------------------------------------------
subroutine gser(gamser, a, x, gln)
  real*8 gamser, a, x, gln, gammln, ap, sum, del, eps
  integer*4 n, itmax
  parameter(itmax=100, eps=3.d-7)
  gln = gammln(a)
  if (x .le. 0.d0) then
    if (x .lt. 0.d0) then
      write (*, '(a)') '***ERROR(bdeci/gser): x < 0 '
      call exit(1)
    endif
    gamser = 0.d0
    return
  endif
  ap = a
  sum = 1.d0/a
  del = sum
  do n = 1, itmax
    ap = ap + 1.d0
    del = del*x/ap
    sum = sum + del
    if (abs(del) .lt. abs(sum)*eps) then
      gamser = sum*exp(-x + a*log(x) - gln)
      return
    endif
  enddo

  write (*, '(a)') '***ERROR(bdeci/gser): a too large, itmax too small'
  call exit(1)
!gamser = sum*exp(-x+a*log(x)-gln)
end
