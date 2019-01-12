      subroutine LAMBDA4 (n, Q, a, disall)

c*ver version 1.0, dd. 13-10-96
c*aut Delft Geodetic Computing Centre/LGR, Paul de Jonge
c*aut copyright by Delft University of Technology, Faculty of Geodesy

c*rol integer estimation with the LAMBDA method. Instead of computing
c*rol matrix Z, we now compute Z^-* directly (using SRC1i).
c*rol For the UDL decomposition of the variance-covariance matrix,
c*rol FMFAC6 is used, since it checks on possible singularity.

c     name   i/o      parameter description
c     ------ -------- --------------------------------------------------
c*par n      input    dimension of matrix
c*par L      input    variance-covariance matrix. This matrix is symmetric,
c*                    and only the lower triangular part is stored and
c*                    accessed. Here it is stored column-wise in a
c*                    2-dimensional array, to avoid the necessity of a
c*                    dedicated storage scheme.
c*           work     | lower triangular matrix L:    *
c*                    | variance covariance matrix = L D L
c*par D      work     | diagonal matrix
c*par a      input    the vector with real valued estimates \hat{a} (float
c*                    solution)
c*           output   the vector with integer valued estimates \check{a}
c*                    (fixed solution)
c*par cands  work     | 2-dimensional array to store the candidates
c*par disall output   | according squared norms \hat{a}-\check{a}
c*                    | (sorted in increasing order)
c*par Zt     work     inverse transpose Z-matrix
c*par v1     work     double precision work array with length=n
c*par v2     work     double precision work array with length=n+1
c*par v3     work     double precision work array with length=n+1
c*par v4     work     double precision work array with length=n
c*par v5     work     double precision work array with length=n
c*par v6     work     double precision work array with length=n
c*par ak     work     contains the (integer) increments of the original
c*                    float ambiguities

      implicit double precision (a-h, o-z)
      parameter (MaxCan=2)

      double precision Q (1:*), a (n), disall (2)
      double precision, pointer :: D (:), Zt (:,:), cands (:,:),
     +  v1 (:), v2 (:) , v3(:), v4(:), v5(:), v6(:), ak(:), L (:,:)

c*    memory allocation
      allocate(L(n, n));
      allocate(cands(n, MaxCan))
      allocate(D(n));  allocate(ak(n))
      allocate(Zt(n, n))
      allocate(v1(n)); allocate(v2(n+1)); allocate(v3(n+1))
      allocate(v4(n)); allocate(v5(n));   allocate(v6(n))
c*    initialize Zt=unit matrix & fill in L matrix
      k=0
      do i=1,n
         do j=1,n
            L (j,i)=0.d0
            Zt(j,i)=0.d0
            if (j.ge.i) then
               k=k+1
               L(j,i)=Q(k)
            end if
         end do
         Zt(i,i)=1.d0
      end do

c*    make estimates in 'a' between -1 and +1 by subtracting an
c*    integer number, store the increments in ak (=shift the centre
c*    of the ellipsoid over the grid by an integer translation)
c*    (see section 4.6)
      do i=1,n
         v1(i)=mod(a(i),1d0)
         ak(i)=a(i)-v1(i)
         a(i)=v1(i)
      end do

c*                -*    -1    -1
c*    make the L_1   D_1   L_1    decomposition of the variance-covariance
c*    matrix Q_hat{a} (equation 3.8, case 3) (section 3.3)
      eps=1d-9
      call FMFAC6 (L, D, n, eps)

c*    compute the Z-transformation based on L and D of Q, ambiguities
c*    are transformed according to \hat{z}=Z^* \hat{a}
      call SRC1i (n, L, D, a, Zt)

c*    For the search we need L and D of Q^{-1}, see section 4.1, or
c*    L^{-1} and D^{-1} of Q here (in our case we use of course
c*    the LAMBDA-transformed L and D as they came from SRC1)
      call INVLT2d (n, L, L, v1)
c*    ... and D_1
      do i=1,n
         D(i)=1d0/D(i)
      end do

c*    find a suitable Chi^2 such that we have two candidates at minimum
c*    use an eps to make sure the two candidates are inside the ellipsoid
c*    (section 4.11)
      eps=1d-6
      call CHIstrt4 (n, D, L, v1, v2, a, v3, v4)
      Chi_1=v3(2)+eps

c*    find the two candidates with minimum norm (section 4.5)
      call FI71 (Chi_1, MaxCan, n, a, D, L, v1, v2, v3, v4, v5, v6,
     +  ncan, disall, cands, ipos)

c*    compute a = Z^-* z
      do i=1,n
         v1(i)=0d0
         do j=1,n
            v1(i)=v1(i)+Zt(i,j)*cands(j,ipos)
         end do
      end do

c*    ...and add the increment to them
      do i=1,n
         a(i)=v1(i)+ak(i)
      end do

c*    'sort' the vector of squared norms in increasing order
c*    (if ipos equals 2, the best candidate is in the second place:
c*    so reverse disall)
      if (ipos.eq.2) then
         h=disall(1)
         disall(1)=disall(2)
         disall(2)=h
      endif

c*    memory deallocation
      deallocate(L)
      deallocate(cands)
      deallocate(D);  deallocate(ak)
      deallocate(Zt)
      deallocate(v1); deallocate(v2); deallocate(v3)
      deallocate(v4); deallocate(v5); deallocate(v6)
      return
      end
