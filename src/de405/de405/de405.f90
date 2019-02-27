!
!! de405.f90
!!
!!    Copyright (C) 2018 by Wuhan University
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
!
!!
!! pupose   : Generate jpleph_de405
!!
!! parameter: lfn -- file unit
!!            HD  -- rinex head structure, see `rinex_observation.h`
!!            ierr -- error code
!!
!! author   : X. Chen
!!

       program de405
       implicit none
       INCLUDE 'jpleph.h'
       TYPE(JPLEPH_HEADER) HD
!
!! local variables
      LOGICAL KM,BARY,FIRST
      INTEGER*4 NRECL,KSIZE,NRL,NR,I,J,K,LFN,IRECSZ,lfn1
      integer*4 ioerr,flag,ii,kk,jj,jjj
      character*80 line,msg
      real*8 BUF(1500)
!
!! read jpleph_405 header
        lfn=10
        OPEN(lfn,FILE='jpleph',STATUS='OLD',err=200)
        do while(.true.)
          msg=''
          read(lfn,'(a)',end=300) line
          if(index(line(1:5),'GROUP').ne.0) then
             read(line,'(6x,i7)',iostat=ioerr) flag
             if(ioerr.ne.0) msg='read jpl error'
             if(flag.eq.1010) then
             elseif(flag.eq.1030) then
                read(lfn,'(a)',end=200) line
                read(line,'(3f12.2)',iostat=ioerr) (HD%SS(i),i=1,3)
                if(ioerr.ne.0) msg='read jpl header error: SS'
             elseif(flag.eq.1040) then
                line=''
                read(lfn,'(a)',end=200) line
                read(line,'(i6)',end=200) HD%NCON
                if(mod(HD%NCON,10).eq.0) then
                   ii = HD%NCON/10
                else
                   ii=HD%NCON/10+1
                endif
                do i=1,ii
                   read(lfn,'(a)',end=200) line
                   read(line,'(10(2x,a6))',iostat=ioerr) (HD%CNAM(jj),jj=1+(i-1)*10,10+(i-1)*10)
                   if(ioerr.ne.0) msg='read jpl header error: CNAM'
                enddo
                HD%AU=149597870.691d0
                HD%EMRAT=81.30056d0
                HD%NUMDE=405
             elseif(flag.eq.1041) then
                read(lfn,'(a)',end=200) line
                read(line,'(i6)',end=200) HD%NCON
                if(mod(HD%NCON,3).eq.0) then
                   ii = HD%NCON/3
                else
                   ii=HD%NCON/3+1
                endif
                do i=1,ii
                   read(lfn,'(a)',end=200) line
                   read(line,'(3f26.18)',iostat=ioerr) (HD%CVAL(jj),jj=1+(i-1)*3,3+(i-1)*3)
                   if(ioerr.ne.0) msg='read jpl header error: CVAL'
                enddo
             elseif(flag.eq.1050) then
                read(lfn,'(a)',end=200) line
                read(line,'(13i6)',iostat=ioerr,end=200) (HD%IPT(1,jj),jj=1,13)
                if(ioerr.ne.0) msg='read jpl header error: IPT'
                read(lfn,'(a)',end=200) line
                read(line,'(13i6)',iostat=ioerr,end=200) (HD%IPT(2,jj),jj=1,13)
                if(ioerr.ne.0) msg='read jpl header error: IPT'
                read(lfn,'(a)',end=200) line
                read(line,'(13i6)',iostat=ioerr,end=200) (HD%IPT(3,jj),jj=1,13)
                if(ioerr.ne.0) msg='read jpl header error: IPT'
             endif
          elseif(index(line,'END OF HEADER').ne.0) then
            NRECL=4
            KSIZE=2036
            IRECSZ=NRECL*KSIZE
            lfn1 = 20
            OPEN(lfn1,FILE='jpleph_de405',ACCESS='DIRECT',RECL=IRECSZ,STATUS='NEW',err=200)
            write(lfn1,REC=1) HD%CNAM,HD%SS,HD%NCON,HD%AU,HD%EMRAT, &
                              ((HD%IPT(I,J),I=1,3),J=1,12),HD%NUMDE,(HD%IPT(I,13),I=1,3)
            write(LFN1,REC=2) HD%CVAL
              do while(.true.)
                read(lfn,'(a)',end=300) line
                read(line,'(i6,i6)') ii,jj
                if(mod(jj,3).eq.0) then
                  jjj = jj/3
                else
                  jjj=jj/3+1
                endif
                do i=1,jjj
                  read(lfn,'(a)',end=300) line
                  read(line,'(3f26.18)',err=100) (buf(kk),kk=1+(i-1)*3,3+(i-1)*3)
                enddo
                write(LFN1,REC=ii+2) (BUF(K),K=1,jj)
              enddo
          endif
          if(len_trim(msg).ne.0) then
               write(*,'(a)') '***ERROR: '//msg
               call exit(1)
          endif
        enddo

  100 WRITE(*,'(a)') '***ERROR(jpleph_state):read `jpleph_de405` '
      call exit(1)

  200 WRITE(*,'(a)') '***ERROR(jpleph_state): open `jpleph_de405` '
      call exit(1)

  300 close(10)
      close(20)

stop
end
