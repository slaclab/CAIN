C------------------ IFPAR -------------------------
      SUBROUTINE IFPAR(T,IPAR,X)
      IMPLICIT NONE
      INTEGER IPAR
      CHARACTER*(*) T
      REAL*8 X
      INCLUDE 'include/nameleng.h'
      INCLUDE 'include/evparc.h'
C
      INTEGER N,I,J
      CHARACTER*(MCHAR) NAM
C
      IPAR=0
      IF(NPAR.LE.0) RETURN
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
      DO 220 I=1,NPAR
        IF(NAM.EQ.NAMPAR(I)) THEN
          IPAR=I
          X=VPAR(I)
          RETURN
        ENDIF
 220  CONTINUE
      RETURN
      END
