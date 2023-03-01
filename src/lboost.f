      SUBROUTINE LBOOST(BG,EV,T)
C  Lorentz boost matrix
      IMPLICIT NONE
      REAL*8 BG,EV(3),T(0:3,0:3)
      INTEGER I,J
      REAL*8 GAM,GAMX,BET
C
      GAM=SQRT(1+BG**2)
      BET=BG/GAM
      GAMX=BG**2/(GAM+1)
      T(0,0)=GAM
      DO 420 I=1,3
        T(0,I)=-BG*EV(I)
        T(I,0)=T(0,I)
        DO 410 J=1,3
          T(I,J)=GAMX*EV(I)*EV(J)
          IF(I.EQ.J) T(I,J)=T(I,J)+1
 410    CONTINUE
 420  CONTINUE
      RETURN
      END
