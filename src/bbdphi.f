      SUBROUTINE BBDPHI
C  Compute derivatives of phi
C dPHI/dx=DPHI(1)/DX, dPHI/dy=DPHI(2)/DY, dPhi/dxdy=DPHI(3)/DXDY
C
      USE BBPKCM
      IMPLICIT NONE
C      INCLUDE 'include/bbpkcm.h'
      INTEGER I,J,L
C
      DO 400 L=1,2
C  dphi/dx
      DO 230 J=1,NXY(2)
*VOCL LOOP,NOVREC
        DO 220 I=2,NXY(1)-1
          DPHI(1,I,J,L)=(PHI(I+1,J,L)-PHI(I-1,J,L))/2D0
 220    CONTINUE
 230  CONTINUE
*VOCL LOOP,NOVREC
      DO 240 J=1,NXY(2)
        DPHI(1,1,J,L)=(-3*PHI(1,J,L)+4*PHI(2,J,L)-PHI(3,J,L))/2
        DPHI(1,NXY(1),J,L)=(3*PHI(NXY(1),J,L)-4*PHI(NXY(1)-1,J,L)
     %       +PHI(NXY(1)-2,J,L))/2
 240  CONTINUE
C  dphi/dy and d(dphi/dx)/dy
      DO 280 I=1,NXY(1)
*VOCL LOOP,NOVREC
        DO 260 J=2,NXY(2)-1
          DPHI(2,I,J,L)=(PHI(I,J+1,L)-PHI(I,J-1,L))/2D0
          DPHI(3,I,J,L)=(DPHI(1,I,J+1,L)-DPHI(1,I,J-1,L))/2D0
 260    CONTINUE
 280  CONTINUE
*VOCL LOOP,NOVREC
      DO 300 I=1,NXY(1)
        DPHI(2,I,1,L)=(-3*PHI(I,1,L)+4*PHI(I,2,L)-PHI(I,3,L))/2
        DPHI(3,I,1,L)=(-3*DPHI(1,I,1,L)+4*DPHI(1,I,2,L)
     %       -DPHI(1,I,3,L))/2
        DPHI(2,I,NXY(2),L)=(3*PHI(I,NXY(2),L)-4*PHI(I,NXY(2)-1,L)
     %       +PHI(I,NXY(2)-2,L))/2
        DPHI(3,I,NXY(2),L)=(3*DPHI(1,I,NXY(2),L)
     %       -4*DPHI(1,I,NXY(2)-1,L)+DPHI(1,I,NXY(2)-2,L))/2
 300  CONTINUE
 400  CONTINUE
      RETURN
      END
