      type jpleph_header
         integer*4 numde,ncon,ipt(3,13)
         character*6 cnam(400),TTL(14,3)
         real*8 cval(400),ss(3),au,emrat
      end type
