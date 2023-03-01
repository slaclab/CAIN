      SUBROUTINE BESJDJ(N,X,J,DJ)
C  Bessel function of integer order. Temporary routine
C    J= J(N,X),  DJ= dJ(N,X)/dX
      IMPLICIT NONE
      INTEGER M
      PARAMETER (M=100)
      INTEGER N
      REAL*8 X,J,DJ
	INCLUDE 'include/ctrlcm.h'
      INTEGER NA
      REAL*8 BJ(0:M+1)
C
      NA=ABS(N)
      IF(NA.GT.M) GOTO 900
      IF(X.EQ.0) THEN
        J=0
        DJ=0
        IF(NA.EQ.0) J=1
        IF(NA.EQ.1) DJ=0.5D0
      ELSE
        CALL J0TON(X,NA+1,BJ)
        J=BJ(NA)
        DJ=NA*J/X-BJ(NA+1)
      ENDIF
      IF(N.LT.0) THEN
        IF(MOD(N,2).NE.0) THEN
          J=-J
          DJ=-DJ
        ENDIF
      ENDIF
      RETURN
 900  WRITE(MSGFL,910)
 910  FORMAT(' (BESJDJ) Order too large.')
      CALL STOPCAIN(100)
      END
