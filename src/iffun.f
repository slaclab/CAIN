C----------------------- IFFUN ------------------------------------
      SUBROUTINE IFFUN(T,IFUN)
      IMPLICIT NONE
      CHARACTER*(*) T
      INTEGER IFUN
      INCLUDE 'include/nameleng.h'
      INCLUDE 'include/funlis.h'
      INTEGER N,I,J
      CHARACTER*(MCHAR) NAM
C
      IFUN=0
      N=LEN(T)
      J=0
      NAM=' '
      DO 200 I=1,N
        IF(T(I:I).NE.' ') THEN
          J=J+1
          IF(J.GT.MCHAR) RETURN
          NAM(J:J)=T(I:I)
        ENDIF
 200  CONTINUE
      DO 220 I=1,MFUN
        IF(NAM.EQ.NAMFUN(I)) THEN
          IFUN=I
          RETURN
        ENDIF
 220  CONTINUE
      RETURN
      END
