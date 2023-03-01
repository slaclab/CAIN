      SUBROUTINE ROTMAT(ANGLE,AXIS,M0,M,N0,N,R)
C  3*3 rotation matrix around the AXIS by the angle ANGLE.
C  Rotation of the coordinate axis, not the points.
C  AXIS must be a unit vector. Not checked.
      IMPLICIT NONE
      INTEGER M0,M,N0,N
      REAL*8 ANGLE,AXIS(3),R(M0:M,N0:N),A0,A1,A2,A3,SI
      A0=COS(ANGLE/2)
      SI=SIN(ANGLE/2)
      A1=SI*AXIS(1)
      A2=SI*AXIS(2)
      A3=SI*AXIS(3)
      R(1,1)=1-2*(A2**2+A3**2)
      R(2,1)=2*(A1*A2-A3*A0)
      R(3,1)=2*(A1*A3+A2*A0)
      R(1,2)=2*(A1*A2+A3*A0)
      R(2,2)=1-2*(A3**2+A1**2)
      R(3,2)=2*(A2*A3-A1*A0)
      R(1,3)=2*(A1*A3-A2*A0)
      R(2,3)=2*(A2*A3+A1*A0)
      R(3,3)=1-2*(A1**2+A2**2)
      RETURN
      END
