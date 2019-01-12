!
!! proc_xl_dirc.f90
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
!! purpose  : process wide-lane directional data
!! parameter:
!!    input : nxl -- number of wide-lane fractional parts
!!            rxl -- fractional parts
!!            wgt -- weight
!!            ifg -- flag
!!    output: ndl -- number of deleted fractional parts
!!            fxl -- estimate of fractional parts
!!            vxl -- rms
!!            sxl -- sigma
!! author   : Geng J
!! created  : Apr 4, 2009
!
subroutine proc_xl_dirc(nxl,rxl,wgt,ifg,ndl,fxl,vxl,sxl)
implicit none

integer*4 nxl,ndl,ifg(1:*)
real*8 rxl(1:*),wgt(1:*),fxl,vxl,sxl
!
!! local
integer*4 i,j,ndel,flg(nxl),loc
real*8 gap(3),bwl(nxl),sav(nxl),mean,rms,sig
!
!! compute fractional parts
do i=1,nxl
  bwl(i)=rxl(i)-nint(rxl(i))
enddo
!
!! loop for each array elements
ndl=0
vxl=1000.d0
sxl=0.d0
if(fxl.eq.10.d0) then
  do i=1,nxl
    rxl(i)=bwl(i)
    do j=1,nxl
      if(j.eq.i) cycle
      gap(1)=bwl(j)-rxl(i)
      gap(2)=bwl(j)+1.d0-rxl(i)
      gap(3)=bwl(j)-1.d0-rxl(i)
      loc=minloc(dabs(gap),1)    ! minimum difference
      rxl(j)=gap(loc)+rxl(i)
    enddo
!! get weighted mean
    flg(1:nxl)=0
    call get_wgt_mean(.true.,rxl,flg,wgt,nxl,ndel,mean,rms,sig)
    if(rms.lt.vxl) then
      sav(1:nxl)=rxl(1:nxl)
      ifg(1:nxl)=flg(1:nxl)
      ndl=ndel
      fxl=mean
      vxl=rms
      sxl=sig
    endif
  enddo
  rxl(1:nxl)=sav(1:nxl)
else
  do j=1,nxl
    gap(1)=bwl(j)-fxl
    gap(2)=bwl(j)+1.d0-fxl
    gap(3)=bwl(j)-1.d0-fxl
    loc=minloc(dabs(gap),1)  ! minimum difference
    rxl(j)=gap(loc)+fxl
  enddo
  ifg(1:nxl)=0
  call get_wgt_mean(.true.,rxl,ifg,wgt,nxl,ndl,fxl,vxl,sxl)
endif

return
end
