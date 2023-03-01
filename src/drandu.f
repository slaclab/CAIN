C******************** DRANDU *******************************************
      SUBROUTINE DRANDU(X,N)
C  GENERATE UNIFORM RANDUM DEVIATES IN (0,1).
C   OUTPUT: (X(I),I=1,N)  RANDUM DEVIATES. R*8.
C  Called routines
C   RANDCAIN    real*8 uniform random number in (0,1)
      IMPLICIT NONE
      INTEGER N,I
      REAL*8 X(N)
      REAL*8 RANDCAIN
	EXTERNAL RANDCAIN
      IF(N.LE.3) THEN
        DO 200 I=1,N
          X(I)=RANDCAIN()
 200    CONTINUE
      ELSE
        CALL RANDN(X,N)
      ENDIF
      RETURN
      END
