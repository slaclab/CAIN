C----------------------- IFUFN ------------------------------------
      SUBROUTINE IFUFN(T,IUFN)
      IMPLICIT NONE
      CHARACTER*(*) T
      INTEGER IUFN
      INCLUDE 'include/nameleng.h'
      INCLUDE 'include/ufncm.h'
      INTEGER N,I,J
      CHARACTER*(MCHAR) NAM
C
      IUFN=0
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
      DO 220 I=1,MUFN00
        IF(NAM.EQ.NAMUFN(I)) THEN
          IUFN=I
          RETURN
        ENDIF
 220  CONTINUE
      RETURN
      END
