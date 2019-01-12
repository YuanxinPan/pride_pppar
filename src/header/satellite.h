type satellite
  integer*4 prn,iptatx
  character*1 sys
  character*20 typ
!! antenna
  real*8 xyz(3,2)
!! satellite attitude
  real*8 xscf(3),yscf(3),zscf(3)
!! clock correction
  real*8 sclock,dclk0
!! epoch difference clock
  integer*4 lepo
  real*8 epclk
end type
