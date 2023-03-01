      SUBROUTINE UNIT8(X,M,N)
      IMPLICIT NONE
      INTEGER M,N
      REAL*8 X(M,N)
      INTEGER I,J
      DO 200 I=1,N
        DO 180 J=1,N
          X(I,J)=0
 180    CONTINUE
 200  CONTINUE
      DO 220 I=1,N
        X(I,I)=1
 220  CONTINUE
      RETURN
      END
