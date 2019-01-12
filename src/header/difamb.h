!
!! DIFferential AMBiguity
type difamb
  integer*4 id,ig,iv,iepc(2),ipt(4)
  real*8 elv,rwl,swl,rnl,snl,dec,rsv
end type
!
!! Saved AMBiguity
type vsdamb
  integer*4 ibeg,iend,ipt(2)
  real*8 rwl,rnl
end type
