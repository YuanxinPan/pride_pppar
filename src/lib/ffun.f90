real*8 function ffun(phi, h)
!
!     Computes the ellipsoidal/elevation-dependent variation of the
!     average acceleration of gravity from Saastamoinen model.
!
!     INPUT:
!       PHI    Geocentric latitude, radians
!       H      Elevation of site above geoid, km
!
  real*8 phi, h
!
  ffun = 1.d0 - 0.266d-2*dcos(2.d0*phi) - 0.28d-3*h
  return
end
